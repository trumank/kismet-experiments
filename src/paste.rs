use crate::bytecode::{
    address_index::AddressIndex,
    expr::{Expr, ExprKind},
    refs::FunctionRef,
    types::BytecodeOffset,
};
use paste_buffer::{BPGraphClipboardData, EdGraphNodeObject, GraphType, PasteBuffer, Serializer};
use paste_buffer::{
    K2NodeGetArrayItem,
    nodes::{
        EEdGraphPinDirection, ESelfContextInfo, ExportTextPath, FBPVariableDescription,
        FEdGraphTerminalType, FText, Guid, K2NodeCallArrayFunction, K2NodeCallFunction,
        K2NodeExecutionSequence, K2NodeFunctionEntry, K2NodeFunctionResult, K2NodeIfThenElse,
        K2NodeMakeArray, K2NodeVariableGet, K2NodeVariableSet, MemberReference, Pin, PinIdentifier,
        PinType, UserDefinedPin,
    },
};
use rand::Rng;
use std::collections::{HashMap, VecDeque};

fn guid_random() -> Guid {
    let mut bytes = [0; 16];
    rand::thread_rng().fill(&mut bytes);
    Guid::from_bytes(bytes)
}

// Constants for node positioning
const NODE_SPACING_X: i32 = 300;
const NODE_SPACING_Y: i32 = 0;
const VARIABLE_NODE_SPACING_X: i32 = 200;
const VARIABLE_NODE_SPACING_Y: i32 = 80;
const VARIABLE_SET_NODE_OFFSET_X: i32 = 200;

/// Helper functions to create common PinType values
fn pintype_exec() -> PinType {
    PinType {
        pin_category: "exec".to_string(),
        ..Default::default()
    }
}

fn pintype_int() -> PinType {
    PinType {
        pin_category: "int".to_string(),
        ..Default::default()
    }
}

fn pintype_float() -> PinType {
    PinType {
        pin_category: "float".to_string(),
        ..Default::default()
    }
}

fn pintype_bool() -> PinType {
    PinType {
        pin_category: "bool".to_string(),
        ..Default::default()
    }
}

fn pintype_string() -> PinType {
    PinType {
        pin_category: "string".to_string(),
        ..Default::default()
    }
}

fn pintype_object(class: &str) -> PinType {
    PinType {
        pin_category: "object".to_string(),
        pin_sub_category: "".to_string(),
        pin_sub_category_object: Some(ExportTextPath::class(class)),
        ..Default::default()
    }
}

/// Context passed around during node graph generation
struct PasteContext<'a> {
    /// The function being decompiled
    func: (&'a str, &'a jmap::Function),
    /// JMAP data for looking up function definitions
    jmap: &'a jmap::Jmap,
    /// Address index for resolving addresses to objects
    address_index: &'a AddressIndex<'a>,
    /// Counter for generating unique node names
    node_counter: i32,
}

impl<'a> PasteContext<'a> {
    fn new(
        func: (&'a str, &'a jmap::Function),
        jmap: &'a jmap::Jmap,
        address_index: &'a AddressIndex<'a>,
    ) -> Self {
        Self {
            func,
            jmap,
            address_index,
            node_counter: 0,
        }
    }

    fn next_node_id(&mut self) -> i32 {
        let id = self.node_counter;
        self.node_counter += 1;
        id
    }

    fn graph_name(&self) -> &str {
        get_object_name(self.func.0)
    }

    /// Get standard horizontal position for a node
    fn node_pos_x(&self) -> i32 {
        self.node_counter * NODE_SPACING_X
    }

    /// Get standard vertical position for a node
    fn node_pos_y(&self) -> i32 {
        NODE_SPACING_Y
    }

    /// Get compact horizontal position for variable nodes
    fn variable_node_pos_x(&self) -> i32 {
        self.node_counter * VARIABLE_NODE_SPACING_X
    }

    /// Get compact vertical position for variable nodes
    fn variable_node_pos_y(&self) -> i32 {
        self.node_counter * VARIABLE_NODE_SPACING_Y
    }
}

/// Represents where an exec output should connect to
#[derive(Debug, Clone)]
enum ExecTarget {
    /// Fall through to the next sequential expression
    FallThrough,
    /// Jump to a specific bytecode offset
    Offset(BytecodeOffset),
    /// Pop the execution flow stack and jump to the popped offset
    PopExecutionFlow,
}

/// Represents a compiled node graph for an expression
/// An expression may compile to multiple nodes (e.g., function call + variable gets for params)
#[derive(Debug, Clone)]
struct NodeGraph {
    /// All nodes generated for this expression
    nodes: Vec<EdGraphNodeObject>,
    /// The exec input pin (if this graph has execution flow)
    exec_input: Option<PinIdentifier>,
    /// Exec output pins mapped to their targets
    /// For linear flow, target is FallThrough
    /// For branches (if/else), multiple outputs with different targets
    exec_outputs: Vec<(PinIdentifier, ExecTarget)>,
    /// Data output pins by parameter name (for value expressions)
    data_outputs: Vec<(String, PinIdentifier)>, // (param_name, pin_connection)
    /// Internal data connections within this graph (from, to)
    internal_connections: Vec<(PinIdentifier, PinIdentifier)>,
}

/// Convert a sequence of bytecode expressions to a Blueprint paste graph
///
/// This is the main entry point for converting decompiled bytecode into a Blueprint node graph
/// that can be pasted into Unreal Editor.
///
/// # Algorithm Overview
/// 1. Creates a FunctionEntry node with parameters from the function signature
/// 2. Processes expressions using a queue-based approach to handle control flow
/// 3. Handles special cases like:
///    - Jump/JumpIfNot for conditional branches
///    - PushExecutionFlow/PopExecutionFlow for loop control (e.g., ForEach)
///    - Let/LetObj/etc for variable assignments
///    - Function calls (VirtualFunction, FinalFunction, CallMath)
/// 4. Connects execution pins and data pins between nodes
/// 5. Serializes the graph to Unreal's copy-paste format
///
/// # Parameters
/// - `expressions`: Decompiled bytecode expressions in execution order
/// - `address_index`: Index for resolving memory addresses to objects/properties
/// - `function_name`: Full path name of the function being decompiled
/// - `func`: The function metadata from JMAP
/// - `jmap`: The full JMAP data for looking up references
pub fn format_as_paste(
    expressions: &[Expr],
    address_index: &AddressIndex,
    function_name: &str,
    func: &jmap::Function,
    jmap: &jmap::Jmap,
) {
    // Extract just the object name from the full path (e.g., "MyFunction" from "/Script/Module.Class:MyFunction")
    let graph_name = function_name
        .rsplit_once(':')
        .map(|(_, name)| name)
        .unwrap_or(function_name);

    // Collect all nodes into a Vec<EdGraphNodeObject>
    let mut all_nodes: Vec<EdGraphNodeObject> = Vec::new();

    // Create function entry node with parameters
    let entry_node = create_function_entry_node(graph_name, func);
    let entry_node_name = match &entry_node {
        EdGraphNodeObject::K2NodeFunctionEntry(node) => node.name.clone(),
        _ => unreachable!(),
    };

    // Build a map of parameter names to pin IDs for the entry node
    let mut entry_param_pins = std::collections::HashMap::new();
    let mut entry_exec_pin = None;
    let entry_pins = match &entry_node {
        EdGraphNodeObject::K2NodeFunctionEntry(node) => &node.pins,
        _ => unreachable!(),
    };
    for pin in entry_pins {
        if pin.pin_name == "then" {
            entry_exec_pin = Some(pin.pin_id);
        } else {
            entry_param_pins.insert(pin.pin_name.clone(), pin.pin_id);
        }
    }

    all_nodes.push(entry_node);

    // Create context for node generation
    let mut ctx = PasteContext::new((function_name, func), jmap, address_index);

    // Build a map of BytecodeOffset -> Expr index for quick lookup
    let expr_map: HashMap<BytecodeOffset, usize> = expressions
        .iter()
        .enumerate()
        .map(|(idx, expr)| (expr.offset, idx))
        .collect();

    // Map of BytecodeOffset -> PinConnection for connecting control flow
    let mut offset_to_exec_input: HashMap<BytecodeOffset, PinIdentifier> = HashMap::new();

    // Map of BytecodeOffset -> NodeGraph for processed expressions
    let mut processed_graphs: HashMap<BytecodeOffset, NodeGraph> = HashMap::new();

    // Queue of (offset, prev_exec_output) to process
    let mut queue: VecDeque<(BytecodeOffset, Option<PinIdentifier>)> = VecDeque::new();

    // Track all connections to be applied after nodes are collected
    // Format: (from, to)
    let mut connections: Vec<(PinIdentifier, PinIdentifier)> = Vec::new();

    // Start with the first expression, connected to FunctionEntry
    if let Some(first_expr) = expressions.first() {
        let entry_exec = entry_exec_pin.map(|pin| PinIdentifier {
            node_name: entry_node_name.clone(),
            pin_guid: pin,
        });
        queue.push_back((first_expr.offset, entry_exec));
    }

    // Process expressions from the queue
    while let Some((offset, prev_exec)) = queue.pop_front() {
        // If already processed, just connect to it and continue
        if processed_graphs.contains_key(&offset) {
            // Connect the previous exec output to this node's exec input
            if let (Some(prev_conn), Some(curr_conn)) =
                (prev_exec, offset_to_exec_input.get(&offset))
            {
                connections.push((prev_conn, curr_conn.clone()));
            }
            continue;
        }

        // Get the expression by index
        let Some(&expr_idx) = expr_map.get(&offset) else {
            eprintln!(
                "WARNING: No expression found at offset 0x{:X} in expression map",
                offset.0
            );
            continue;
        };
        let expr = &expressions[expr_idx];

        // Handle Jump expressions specially - they transparently redirect to target
        if let ExprKind::Jump { target } = &expr.kind {
            // Queue the jump target with the current prev_exec, effectively jumping there
            queue.push_back((*target, prev_exec));
            // Mark this offset as processed to avoid reprocessing
            processed_graphs.insert(offset, create_empty_node_graph());
            continue;
        }

        // Handle PushExecutionFlow - create a sequence node with 2 outputs
        if let ExprKind::PushExecutionFlow { push_offset } = &expr.kind {
            // Create an ExecutionSequence node with 2 outputs
            // then_0 executes first (loop body), then_1 executes after (continuation)
            // The Blueprint runtime handles the "stack" semantics - no need for compile-time stack
            let node_name = format!("K2Node_ExecutionSequence_{}", ctx.next_node_id());
            let exec_in_pin = guid_random();
            let then_0_pin = guid_random();
            let then_1_pin = guid_random();

            let mut seq_node = K2NodeExecutionSequence::new(node_name.clone());

            // Set EdGraphNode properties (via Deref)
            seq_node.pins = vec![
                Pin {
                    pin_id: exec_in_pin,
                    pin_name: "execute".to_string(),
                    direction: EEdGraphPinDirection::Input,
                    pin_type: pintype_exec(),
                    ..Default::default()
                },
                Pin {
                    pin_id: then_0_pin,
                    pin_name: "then_0".to_string(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type: pintype_exec(),
                    ..Default::default()
                },
                Pin {
                    pin_id: then_1_pin,
                    pin_name: "then_1".to_string(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type: pintype_exec(),
                    ..Default::default()
                },
            ];
            seq_node.node_pos_x = ctx.node_pos_x();
            seq_node.node_pos_y = ctx.node_pos_y();
            seq_node.node_guid = guid_random();

            all_nodes.push(EdGraphNodeObject::K2NodeExecutionSequence(seq_node));

            // Connect to previous exec
            if let Some(prev_conn) = prev_exec {
                connections.push((
                    prev_conn,
                    PinIdentifier {
                        node_name: node_name.clone(),
                        pin_guid: exec_in_pin,
                    },
                ));
            }

            // Register exec input for this offset
            offset_to_exec_input.insert(
                offset,
                PinIdentifier {
                    node_name: node_name.clone(),
                    pin_guid: exec_in_pin,
                },
            );

            // Queue then_0 to fall through to next expression
            if let Some(next_expr) = expressions.get(expr_idx + 1) {
                queue.push_back((
                    next_expr.offset,
                    Some(PinIdentifier {
                        node_name: node_name.clone(),
                        pin_guid: then_0_pin,
                    }),
                ));
            }

            // Queue then_1 to jump to the pushed address
            queue.push_back((
                *push_offset,
                Some(PinIdentifier {
                    node_name: node_name.clone(),
                    pin_guid: then_1_pin,
                }),
            ));

            // Mark as processed
            processed_graphs.insert(
                offset,
                NodeGraph {
                    nodes: vec![],
                    exec_input: Some(PinIdentifier {
                        node_name,
                        pin_guid: exec_in_pin,
                    }),
                    exec_outputs: vec![],
                    data_outputs: vec![],
                    internal_connections: vec![],
                },
            );
            continue;
        }

        // Handle PopExecutionFlow - pop continuation and jump to it
        if let ExprKind::PopExecutionFlow = &expr.kind {
            // Mark as processed
            processed_graphs.insert(offset, create_empty_node_graph());
            continue;
        }

        // Convert to node graph
        let Some(node_graph) = expr_to_node(expr, &mut ctx, None) else {
            eprintln!(
                "WARNING: Failed to convert expression at offset 0x{:X} (kind: {:?}) to node graph",
                offset.0, expr.kind
            );
            continue;
        };

        // Destructure node_graph to avoid partial moves
        let NodeGraph {
            nodes,
            exec_input,
            exec_outputs,
            data_outputs,
            internal_connections,
        } = node_graph;

        // Register this node's exec input for control flow connections
        if let Some(pin_connection) = &exec_input {
            offset_to_exec_input.insert(offset, pin_connection.clone());
        }

        // Add all nodes from this graph
        all_nodes.extend(nodes);

        // Connect execution flow from previous node
        if let (Some(prev_conn), Some(curr_conn)) = (prev_exec, &exec_input) {
            connections.push((prev_conn, curr_conn.clone()));
        }

        // Connect internal data pins (e.g., VariableGet -> CallFunction params)
        connections.extend(internal_connections.clone());

        // Connect data_outputs from FunctionEntry to call nodes
        for (param_name, target_conn) in &data_outputs {
            if let Some(entry_pin_id) = entry_param_pins.get(param_name) {
                connections.push((
                    PinIdentifier {
                        node_name: entry_node_name.clone(),
                        pin_guid: *entry_pin_id,
                    },
                    target_conn.clone(),
                ));
            }
        }

        // Queue all exec output targets
        for (out_conn, exec_target) in &exec_outputs {
            let target = match exec_target {
                ExecTarget::FallThrough => {
                    // Fall-through to next expression
                    if let Some(next_expr) = expressions.get(expr_idx + 1) {
                        next_expr.offset
                    } else {
                        continue; // No next expression
                    }
                }
                ExecTarget::Offset(offset) => {
                    // Explicit jump target
                    *offset
                }
                ExecTarget::PopExecutionFlow => {
                    continue;
                }
            };

            queue.push_back((target, Some(out_conn.clone())));
        }

        // Reconstruct node_graph for storage
        processed_graphs.insert(
            offset,
            NodeGraph {
                nodes: vec![],
                exec_input,
                exec_outputs,
                data_outputs,
                internal_connections,
            },
        );
    }

    // Apply all tracked connections to the pins
    for (from_conn, to_conn) in connections {
        let from_node = all_nodes
            .iter_mut()
            .find_map(|n| {
                let node = n.as_ed_graph_node_mut();
                (node.name == from_conn.node_name).then_some(node)
            })
            .expect("bad 'from' node name");
        let from_pin = from_node
            .pins
            .iter_mut()
            .find(|p| p.pin_id == from_conn.pin_guid)
            .expect("bad 'from' pin id");
        from_pin.linked_to.push(to_conn.clone());

        let to_node = all_nodes
            .iter_mut()
            .find_map(|n| {
                let node = n.as_ed_graph_node_mut();
                (node.name == to_conn.node_name).then_some(node)
            })
            .expect("bad 'to' node name");
        let to_pin = to_node
            .pins
            .iter_mut()
            .find(|p| p.pin_id == to_conn.pin_guid)
            .expect("bad 'to' pin id");
        to_pin.linked_to.push(from_conn);
    }

    let all_nodes = all_nodes.into_iter().map(|n| n.into()).collect::<Vec<_>>();

    let paste_buffer = PasteBuffer::UObjects(all_nodes.clone());
    let mut serializer = Serializer::new();
    let output = serializer.serialize(&paste_buffer);
    println!("{}", output);

    // Serialize and print the graph
    let graph_data = BPGraphClipboardData {
        graph_name: graph_name.to_string(),
        graph_type: GraphType::Function,
        original_blueprint: Some(ExportTextPath::blueprint(
            "/Game/Decompiled/GeneratedFunction.GeneratedFunction",
        )),
        nodes: all_nodes,
    };
    let paste_buffer = PasteBuffer::BPGraph(graph_data);
    let mut serializer = Serializer::new();
    let output = serializer.serialize(&paste_buffer);
    println!("{}", output);
}

fn create_function_entry_node(function_name: &str, func: &jmap::Function) -> EdGraphNodeObject {
    // Create user-defined pins for function parameters
    let mut user_defined_pins = Vec::new();
    let mut pins = vec![Pin {
        pin_id: guid_random(),
        pin_name: "then".to_string(),
        direction: EEdGraphPinDirection::Output,
        pin_type: pintype_exec(),
        ..Default::default()
    }];

    // Collect local variables (non-parameter properties)
    let mut local_variables = Vec::new();

    // Extract parameters from the function - properties with CPF_Parm flag
    for prop in &func.r#struct.properties {
        if prop.flags.contains(jmap::EPropertyFlags::CPF_Parm) {
            let pin_type = property_to_pin_type(prop);

            // Return parameters and out parameters are inputs on the function entry node
            if prop.flags.contains(jmap::EPropertyFlags::CPF_ReturnParm)
                || prop.flags.contains(jmap::EPropertyFlags::CPF_OutParm)
            {
                user_defined_pins.push(UserDefinedPin {
                    pin_name: prop.name.clone(),
                    pin_type: pin_type.clone(),
                    desired_direction: EEdGraphPinDirection::Input,
                    default_value: None,
                });

                pins.push(Pin {
                    pin_id: guid_random(),
                    pin_name: prop.name.clone(),
                    direction: EEdGraphPinDirection::Input,
                    pin_type,
                    ..Default::default()
                });
            } else {
                // Regular input parameters are outputs on the function entry node
                user_defined_pins.push(UserDefinedPin {
                    pin_name: prop.name.clone(),
                    pin_type: pin_type.clone(),
                    desired_direction: EEdGraphPinDirection::Output,
                    default_value: None,
                });

                pins.push(Pin {
                    pin_id: guid_random(),
                    pin_name: prop.name.clone(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type,
                    ..Default::default()
                });
            }
        } else {
            // This is a local variable
            local_variables.push(FBPVariableDescription {
                var_name: prop.name.clone(),
                var_guid: guid_random().to_string(),
                var_type: property_to_pin_type(prop),
                friendly_name: Some(prop.name.clone()),
                category: None,
                property_flags: Some(
                    prop.flags
                        .union(
                            jmap::EPropertyFlags::CPF_Edit
                                | jmap::EPropertyFlags::CPF_BlueprintVisible,
                        )
                        .bits(),
                ),
            });
        }
    }

    // Create K2NodeFunctionEntry with proper hierarchy
    let mut entry_node = K2NodeFunctionEntry::new("K2Node_FunctionEntry_0".to_string());

    // Set EdGraphNode properties (via Deref)
    entry_node.pins = pins;
    entry_node.node_pos_x = 0;
    entry_node.node_pos_y = 0;
    entry_node.node_guid = guid_random();

    // Set EditablePinBase properties
    entry_node.is_editable = true;
    entry_node.user_defined_pins = user_defined_pins;

    // Set FunctionTerminator properties
    entry_node.function_reference = MemberReference {
        member_parent: None,
        member_scope: None,
        member_name: Some(function_name.to_string()),
        member_guid: None,
        self_context: false,
        was_deprecated: false,
    };

    // Set FunctionEntry specific properties
    entry_node.extra_flags = 201457664;
    entry_node.local_variables = local_variables;

    EdGraphNodeObject::K2NodeFunctionEntry(entry_node)
}

/// Look up a function by its reference (address or name)
///
/// If `context_expr` is None, the function should be resolved on the owning class (self).
/// If `context_expr` is Some, the function should be resolved on the context object's type.
///
/// Returns the function and its full path name from the JMAP
fn resolve_function<'a>(
    ctx: &PasteContext<'a>,
    func_ref: &FunctionRef,
    context_expr: Option<&Expr>,
) -> Option<(&'a str, &'a jmap::Function)> {
    match func_ref {
        FunctionRef::ByAddress(addr) => {
            // Look up the object by address
            if let Some(obj_name) = ctx.address_index.object_index.get(&addr.0)
                && let Some(jmap::ObjectType::Function(func)) = ctx.jmap.objects.get(*obj_name)
            {
                return Some((*obj_name, func));
            }
            eprintln!(
                "WARNING: Failed to resolve function by address 0x{:X}",
                addr.0
            );
            None
        }
        FunctionRef::ByName(name) => {
            // Virtual function by name - need to resolve on the target class
            let target_class = if let Some(ctx_expr) = context_expr {
                // Try to extract the class from the context expression
                match &ctx_expr.kind {
                    ExprKind::ObjectConst(obj_ref) => {
                        // Look up the object by address using AddressIndex
                        if let Some(obj_path) =
                            ctx.address_index.object_index.get(&obj_ref.address.0)
                        {
                            if let Some(obj) = ctx.jmap.objects.get(*obj_path) {
                                &obj.get_object().class
                            } else {
                                // Object path found but not in JMAP
                                eprintln!(
                                    "WARNING: Object path '{}' found in AddressIndex but not in JMAP for function '{}'",
                                    obj_path,
                                    name.as_str()
                                );
                                return None;
                            }
                        } else {
                            // Object address not found in AddressIndex
                            eprintln!(
                                "WARNING: Object address 0x{:X} not found in AddressIndex for function '{}'",
                                obj_ref.address.0,
                                name.as_str()
                            );
                            return None;
                        }
                    }
                    ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) => {
                        // FIXME need to handle EX_Context switching
                        // Look up the property type to determine the object class
                        if let Some(prop_info) =
                            ctx.address_index.resolve_property(prop_ref.address)
                        {
                            let prop = prop_info.property;
                            // Extract the class from the property type
                            match &prop.r#type {
                                jmap::PropertyType::Object { property_class } => {
                                    property_class.as_str()
                                }
                                jmap::PropertyType::Interface { interface_class } => {
                                    interface_class.as_str()
                                }
                                other => {
                                    eprintln!(
                                        "WARNING: Variable '{}' has non-object property type {:?} for function '{}' (offset: 0x{:X})",
                                        prop.name,
                                        other,
                                        name.as_str(),
                                        ctx_expr.offset.0
                                    );
                                    return None;
                                }
                            }
                        } else {
                            eprintln!(
                                "WARNING: Failed to resolve property at address 0x{:X} for function '{}' (offset: 0x{:X})",
                                prop_ref.address.0,
                                name.as_str(),
                                ctx_expr.offset.0
                            );
                            return None;
                        }
                    }
                    _ => {
                        // Can't extract type from other context expressions yet
                        eprintln!(
                            "WARNING: Cannot extract type from context expression kind {:?} for function '{}' (offset: 0x{:X})",
                            ctx_expr.kind,
                            name.as_str(),
                            ctx_expr.offset.0
                        );
                        return None;
                    }
                }
            } else {
                // No context means call on self - use the owning class
                // Get the class that owns this function
                if let Some(parent_class) = &ctx.func.1.r#struct.object.outer {
                    parent_class.as_str()
                } else {
                    eprintln!(
                        "WARNING: Owning class has no outer for self-context function '{}' in '{}'",
                        name.as_str(),
                        ctx.func.0,
                    );
                    return None;
                }
            };

            // Search for a function with this name in the target class and its super classes
            let name_str = name.as_str();

            // Walk up the inheritance hierarchy
            let mut current_class = Some(target_class);
            while let Some(class_path) = current_class {
                // Look for a function object that belongs to this class
                for (obj_path, obj) in &ctx.jmap.objects {
                    if let jmap::ObjectType::Function(func) = obj {
                        // Check if this function's outer matches the current class
                        if let Some(outer) = &func.r#struct.object.outer
                            && outer == class_path
                        {
                            let obj_name = get_object_name(obj_path);
                            // Case-insensitive exact match
                            if obj_name.eq_ignore_ascii_case(name_str) {
                                return Some((obj_path.as_str(), func));
                            }
                        }
                    }
                }

                // Move to the super class
                current_class = ctx
                    .jmap
                    .objects
                    .get(class_path)
                    .and_then(jmap::ObjectType::get_struct)
                    .and_then(|s| s.super_struct.as_deref());
            }

            eprintln!(
                "WARNING: Function '{}' not found in target class '{}' or its super classes",
                name_str, target_class
            );
            None
        }
    }
}

/// Extract just the function name from a full object path
/// E.g., "/Script/Module.Class:FunctionName" -> "FunctionName"
fn get_object_name(full_path: &str) -> &str {
    full_path
        .rsplit_once([',', ':'])
        .map(|(_, name)| name)
        .unwrap_or(full_path)
}

/// Check if a property is a function parameter (not a local variable)
fn is_function_parameter(prop: &jmap::Property) -> bool {
    prop.flags.contains(jmap::EPropertyFlags::CPF_Parm)
}

/// Extract parameter name from a LocalVariable expression, if it refers to a function parameter
fn get_parameter_name(expr: &Expr, ctx: &PasteContext) -> Option<String> {
    match &expr.kind {
        ExprKind::LocalVariable(prop_ref)
        | ExprKind::InstanceVariable(prop_ref)
        | ExprKind::LocalOutVariable(prop_ref) => {
            let prop_info = ctx.address_index.resolve_property(prop_ref.address)?;
            let prop = prop_info.property;
            if is_function_parameter(prop) {
                return Some(prop.name.clone());
            }
            None
        }
        _ => None,
    }
}

/// Helper to emit a warning for property resolution failure
fn warn_property_resolution_failed(context: &str, address: u64, offset: usize) {
    eprintln!(
        "WARNING: {} - Failed to resolve property at address 0x{:X} (offset: 0x{:X})",
        context, address, offset
    );
}

/// Helper to create an empty NodeGraph (for skipped expressions like debug markers)
fn create_empty_node_graph() -> NodeGraph {
    NodeGraph {
        nodes: vec![],
        exec_input: None,
        exec_outputs: vec![],
        data_outputs: vec![],
        internal_connections: vec![],
    }
}

/// Create a VariableGet node for a parameter expression
/// Returns the node and the pin that outputs the variable value
/// Returns None if the expression refers to a function parameter (should connect directly to FunctionEntry)
fn create_variable_get_node(
    expr: &Expr,
    ctx: &mut PasteContext,
) -> Option<(EdGraphNodeObject, Guid, PinType)> {
    match &expr.kind {
        ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) => {
            // Look up the property using AddressIndex
            let prop_info = ctx.address_index.resolve_property(prop_ref.address);
            if prop_info.is_none() {
                warn_property_resolution_failed(
                    "create_variable_get_node",
                    prop_ref.address.0,
                    expr.offset.0,
                );
                return None;
            }
            let prop_info = prop_info?;
            let prop = prop_info.property;

            // Skip function parameters - they should connect directly to FunctionEntry/Result
            if is_function_parameter(prop) {
                return None;
            }

            let var_name = &prop.name;
            let pin_type = property_to_pin_type(prop);
            let is_instance_var = matches!(expr.kind, ExprKind::InstanceVariable(_));

            let node_name = format!("K2Node_VariableGet_{}", ctx.next_node_id());

            let output_pin_id = guid_random();

            let mut node = K2NodeVariableGet::new(node_name);

            // Set EdGraphNode properties (via Deref)
            node.pins = vec![Pin {
                pin_id: output_pin_id,
                pin_name: var_name.to_string(),
                direction: EEdGraphPinDirection::Output,
                pin_type: pin_type.clone(),
                ..Default::default()
            }];
            node.node_pos_x = ctx.variable_node_pos_x();
            node.node_pos_y = ctx.variable_node_pos_y();
            node.node_guid = guid_random();

            // Set K2NodeVariableGet specific properties
            node.variable_reference = MemberReference {
                member_parent: None,
                member_scope: if is_instance_var {
                    None // Instance variables don't have a scope
                } else {
                    Some(ctx.graph_name().to_string()) // Local variables are scoped to the function
                },
                member_name: Some(var_name.to_string()),
                member_guid: None,
                self_context: is_instance_var,
                was_deprecated: false,
            };
            node.self_context_info = ESelfContextInfo::Unspecified;

            Some((
                EdGraphNodeObject::K2NodeVariableGet(node),
                output_pin_id,
                pin_type,
            ))
        }
        _ => {
            eprintln!(
                "WARNING: create_variable_get_node - unsupported expression kind: {:?} (offset: 0x{:X})",
                expr.kind, expr.offset.0
            );
            None
        }
    }
}

/// Create a VariableSet node for an out parameter expression
/// Returns the node and the pin that receives the variable value
/// Returns None if the expression refers to a function parameter (should connect directly to FunctionResult)
fn create_variable_set_node(
    expr: &Expr,
    ctx: &mut PasteContext,
) -> Option<(EdGraphNodeObject, Guid, PinType)> {
    match &expr.kind {
        ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) => {
            // Look up the property using AddressIndex
            let prop_info = ctx.address_index.resolve_property(prop_ref.address);
            if prop_info.is_none() {
                warn_property_resolution_failed(
                    "create_variable_set_node",
                    prop_ref.address.0,
                    expr.offset.0,
                );
                return None;
            }
            let prop_info = prop_info?;
            let prop = prop_info.property;

            // Skip function parameters - they should connect directly to FunctionEntry/Result
            if is_function_parameter(prop) {
                return None;
            }

            let var_name = &prop.name;
            let pin_type = property_to_pin_type(prop);
            let is_instance_var = matches!(expr.kind, ExprKind::InstanceVariable(_));

            let node_name = format!("K2Node_VariableSet_{}", ctx.next_node_id());

            let exec_in_pin = guid_random();
            let exec_out_pin = guid_random();
            let value_input_pin = guid_random();
            let output_get_pin = guid_random();

            let mut node = K2NodeVariableSet::new(node_name);

            // Set EdGraphNode properties (via Deref)
            node.pins = vec![
                Pin {
                    pin_id: exec_in_pin,
                    pin_name: "execute".to_string(),
                    direction: EEdGraphPinDirection::Input,
                    pin_type: pintype_exec(),
                    ..Default::default()
                },
                Pin {
                    pin_id: exec_out_pin,
                    pin_name: "then".to_string(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type: pintype_exec(),
                    ..Default::default()
                },
                Pin {
                    pin_id: value_input_pin,
                    pin_name: var_name.to_string(),
                    direction: EEdGraphPinDirection::Input,
                    pin_type: pin_type.clone(),
                    ..Default::default()
                },
                Pin {
                    pin_id: output_get_pin,
                    pin_name: "Output_Get".to_string(),
                    pin_tool_tip: "Retrieves the value of the variable, can use instead of a separate Get node".to_string(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type: pin_type.clone(),
                    ..Default::default()
                },
            ];
            node.node_pos_x = ctx.variable_node_pos_x();
            node.node_pos_y = ctx.variable_node_pos_y();
            node.node_guid = guid_random();

            // Set K2NodeVariableSet specific properties
            node.variable_reference = MemberReference {
                member_parent: None,
                member_scope: if is_instance_var {
                    None // Instance variables don't have a scope
                } else {
                    Some(ctx.graph_name().to_string()) // Local variables are scoped to the function
                },
                member_name: Some(var_name.to_string()),
                member_guid: None,
                self_context: is_instance_var,
                was_deprecated: false,
            };
            node.self_context_info = ESelfContextInfo::Unspecified;

            Some((
                EdGraphNodeObject::K2NodeVariableSet(node),
                value_input_pin,
                pin_type,
            ))
        }
        _ => {
            eprintln!(
                "WARNING: create_variable_set_node - unsupported expression kind: {:?} (offset: 0x{:X})",
                expr.kind, expr.offset.0
            );
            None
        }
    }
}

/// Add parameter pins to a function call node based on the function definition
/// Returns (param_name, pin_id, is_input) for each parameter to help with connections
fn add_function_parameter_pins(
    pins: &mut Vec<Pin>,
    called_func: Option<(&str, &jmap::Function)>,
    _is_pure: bool,
) -> Vec<(String, Guid, bool)> {
    let mut param_info = Vec::new();

    if let Some((_path, func)) = called_func {
        for prop in &func.r#struct.properties {
            if prop.flags.contains(jmap::EPropertyFlags::CPF_Parm) {
                let pin_id = guid_random();
                let pin_type = property_to_pin_type(prop);

                // Return parameters are always outputs
                if prop.flags.contains(jmap::EPropertyFlags::CPF_ReturnParm) {
                    pins.push(Pin {
                        pin_id,
                        pin_name: prop.name.clone(),
                        direction: EEdGraphPinDirection::Output,
                        pin_type,
                        ..Default::default()
                    });
                    param_info.push((prop.name.clone(), pin_id, false));
                } else if prop.flags.contains(jmap::EPropertyFlags::CPF_OutParm) {
                    // Check if this is a const reference (const T& parameter)
                    // These are inputs, not outputs, especially for pure functions
                    let is_const_ref = prop.flags.contains(jmap::EPropertyFlags::CPF_ConstParm);

                    if is_const_ref {
                        // Const reference input for pure functions (e.g., const TSet<T>& TargetSet)
                        pins.push(Pin {
                            pin_id,
                            pin_name: prop.name.clone(),
                            direction: EEdGraphPinDirection::Input,
                            pin_type,
                            ..Default::default()
                        });
                        param_info.push((prop.name.clone(), pin_id, true));
                    } else {
                        // True output parameter (T& OutParam)
                        pins.push(Pin {
                            pin_id,
                            pin_name: prop.name.clone(),
                            direction: EEdGraphPinDirection::Output,
                            pin_type,
                            ..Default::default()
                        });
                        param_info.push((prop.name.clone(), pin_id, false));
                    }
                } else {
                    // Regular input parameters
                    pins.push(Pin {
                        pin_id,
                        pin_name: prop.name.clone(),
                        direction: EEdGraphPinDirection::Input,
                        pin_type,
                        ..Default::default()
                    });
                    param_info.push((prop.name.clone(), pin_id, true));
                }
            }
        }
    }

    param_info
}

/// Convert a PinType to FEdGraphTerminalType (used for map value types)
fn pin_type_to_terminal_type(pin_type: &PinType) -> FEdGraphTerminalType {
    FEdGraphTerminalType {
        terminal_category: pin_type.pin_category.clone(),
        terminal_sub_category: pin_type.pin_sub_category.clone(),
        terminal_sub_category_object: pin_type.pin_sub_category_object.clone(),
        terminal_is_const: pin_type.is_const,
        terminal_is_weak_pointer: pin_type.is_weak_pointer,
        terminal_is_uobject_wrapper: pin_type.is_uobject_wrapper,
    }
}

fn property_to_pin_type(prop: &jmap::Property) -> PinType {
    // Convert JMAP property type to Blueprint PinType
    match &prop.r#type {
        jmap::PropertyType::Int => pintype_int(),
        jmap::PropertyType::Int8 | jmap::PropertyType::Byte { r#enum: None } => PinType {
            pin_category: "byte".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Int16 | jmap::PropertyType::UInt16 | jmap::PropertyType::UInt32 => {
            pintype_int()
        }
        jmap::PropertyType::Int64 | jmap::PropertyType::UInt64 => PinType {
            pin_category: "int64".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Float | jmap::PropertyType::Double => pintype_float(),
        jmap::PropertyType::Bool { .. } => pintype_bool(),
        jmap::PropertyType::Str => pintype_string(),
        jmap::PropertyType::Name => PinType {
            pin_category: "name".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Text => PinType {
            pin_category: "text".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Byte { r#enum: Some(_) } => {
            todo!("property_to_pin_type: enum byte properties not yet supported");
        }
        jmap::PropertyType::Enum { r#enum, .. } => {
            todo!(
                "property_to_pin_type: enum properties not yet supported: {:?}",
                r#enum
            );
        }
        jmap::PropertyType::Struct { r#struct } => {
            todo!(
                "property_to_pin_type: struct properties not yet supported: {}",
                r#struct
            );
        }
        jmap::PropertyType::Object { property_class } => pintype_object(property_class),
        jmap::PropertyType::Class {
            property_class,
            meta_class,
        } => PinType {
            pin_category: "class".to_string(),
            pin_sub_category: meta_class.clone(),
            pin_sub_category_object: Some(ExportTextPath::class(property_class)),
            ..Default::default()
        },
        jmap::PropertyType::Interface { interface_class } => PinType {
            pin_category: "interface".to_string(),
            pin_sub_category_object: Some(ExportTextPath::class(interface_class)),
            container_type: Some("None".to_string()),
            ..Default::default()
        },
        jmap::PropertyType::Array { inner } => {
            // Get the inner type and modify it to be an array
            let mut inner_pin_type = property_to_pin_type(inner);
            inner_pin_type.container_type = Some("Array".to_string());
            inner_pin_type
        }
        jmap::PropertyType::Set { key_prop } => {
            // Get the key type and modify it to be a set
            let mut key_pin_type = property_to_pin_type(key_prop);
            key_pin_type.container_type = Some("Set".to_string());
            key_pin_type
        }
        jmap::PropertyType::Map {
            key_prop,
            value_prop,
        } => {
            // For maps, the base pin category is the KEY type
            // The VALUE type is stored in pin_value_type as FEdGraphTerminalType
            let key_pin_type = property_to_pin_type(key_prop);
            let value_pin_type = property_to_pin_type(value_prop);
            let value_terminal_type = pin_type_to_terminal_type(&value_pin_type);

            PinType {
                pin_category: key_pin_type.pin_category,
                pin_sub_category: String::new(),
                pin_sub_category_object: key_pin_type.pin_sub_category_object,
                container_type: Some("Map".to_string()),
                pin_value_type: Some(value_terminal_type),
                ..Default::default()
            }
        }
        other => panic!(
            "property_to_pin_type: unsupported property type: {:?}",
            other
        ),
    }
}

/// Create pins for Array_Get (K2NodeGetArrayItem)
/// Returns (pins, param_pins) where param_pins maps function params to pin IDs
fn create_array_get_pins(
    params: &[Expr],
    ctx: &PasteContext,
) -> (Vec<Pin>, Vec<(String, Guid, bool)>) {
    // Get the element type from the third parameter (the out param)
    let element_pin_type = if params.len() >= 3 {
        // Try to infer element type from the out parameter
        if let ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) =
            &params[2].kind
        {
            if let Some(prop_info) = ctx.address_index.resolve_property(prop_ref.address) {
                property_to_pin_type(prop_info.property)
            } else {
                pintype_int() // fallback
            }
        } else {
            pintype_int() // fallback
        }
    } else {
        pintype_int() // fallback
    };

    let array_pin_id = guid_random();
    let index_pin_id = guid_random();
    let output_pin_id = guid_random();

    let pins = vec![
        Pin {
            pin_id: array_pin_id,
            pin_name: "Array".to_string(),
            direction: EEdGraphPinDirection::Input,
            pin_type: PinType {
                container_type: Some("Array".to_string()),
                ..element_pin_type.clone()
            },
            ..Default::default()
        },
        Pin {
            pin_id: index_pin_id,
            pin_name: "Dimension 1".to_string(),
            direction: EEdGraphPinDirection::Input,
            pin_type: pintype_int(),
            default_value: "0".to_string(),
            autogenerated_default_value: "0".to_string(),
            ..Default::default()
        },
        Pin {
            pin_id: output_pin_id,
            pin_name: "Output".to_string(),
            direction: EEdGraphPinDirection::Output,
            pin_type: PinType {
                is_reference: true,
                ..element_pin_type
            },
            ..Default::default()
        },
    ];

    // Map function params to the custom pins: (param_name, pin_id, is_input)
    let param_pins = vec![
        ("Array".to_string(), array_pin_id, true),
        ("Dimension 1".to_string(), index_pin_id, true),
        ("Output".to_string(), output_pin_id, false),
    ];

    (pins, param_pins)
}

/// Create pins for a normal function call node
/// Returns (pins, param_pins, exec_in_pin, exec_out_pin)
fn create_normal_function_pins(
    called_func: Option<(&str, &jmap::Function)>,
    context_expr: Option<&Expr>,
    is_pure: bool,
) -> (
    Vec<Pin>,
    Vec<(String, Guid, bool)>,
    Option<Guid>,
    Option<Guid>,
) {
    let mut pins = vec![];

    // Only add exec pins if not pure
    let (exec_in_pin, exec_out_pin) = if !is_pure {
        let exec_in = guid_random();
        let exec_out = guid_random();
        pins.push(Pin {
            pin_id: exec_in,
            pin_name: "execute".to_string(),
            direction: EEdGraphPinDirection::Input,
            pin_type: pintype_exec(),
            ..Default::default()
        });
        pins.push(Pin {
            pin_id: exec_out,
            pin_name: "then".to_string(),
            direction: EEdGraphPinDirection::Output,
            pin_type: pintype_exec(),
            ..Default::default()
        });
        (Some(exec_in), Some(exec_out))
    } else {
        (None, None)
    };

    // If we have a context expression, add a self/target pin
    if context_expr.is_some() {
        pins.push(Pin {
            pin_id: guid_random(),
            pin_name: "self".to_string(),
            direction: EEdGraphPinDirection::Input,
            pin_type: pintype_object("/Script/CoreUObject.Object"),
            ..Default::default()
        });
    }

    // Add parameter pins based on function signature
    let param_pins = add_function_parameter_pins(&mut pins, called_func, is_pure);

    (pins, param_pins, exec_in_pin, exec_out_pin)
}

/// Helper function to create a function call node graph (used by VirtualFunction, LocalVirtualFunction, FinalFunction)
/// If is_pure is true, creates a pure function node without exec pins
fn create_function_call_node(
    func_ref: &FunctionRef,
    params: &[Expr],
    ctx: &mut PasteContext,
    context_expr: Option<&Expr>,
    pure_context: bool,
) -> Option<NodeGraph> {
    // Try to resolve the function to get parameter information
    let resolved = resolve_function(ctx, func_ref, context_expr);

    let (called_func, func_name, member_parent, is_array_get) = if let Some((path, f)) = resolved {
        let is_array_get = path == "/Script/Engine.KismetArrayLibrary:Array_Get";

        // Extract the parent class from the function's outer
        let parent = f.r#struct.object.outer.as_deref();
        (
            Some((path, f)),
            get_object_name(path).to_string(),
            parent,
            is_array_get,
        )
    } else {
        // Failed to resolve - extract name from the FunctionRef itself
        let name = match func_ref {
            FunctionRef::ByName(name) => {
                eprintln!(
                    "WARNING: create_function_call_node - Could not resolve function '{}', creating node with unknown signature",
                    name.as_str()
                );
                name.as_str().to_string()
            }
            FunctionRef::ByAddress(addr) => {
                eprintln!(
                    "WARNING: create_function_call_node - Could not resolve function at address 0x{:X}, creating placeholder node",
                    addr.0
                );
                format!("UnknownFunc_{:X}", addr.0)
            }
        };
        (None, name, None, false)
    };

    // Check if the function is actually pure by looking at its flags
    let function_is_pure = called_func
        .map(|(_path, func)| {
            func.function_flags
                .contains(jmap::EFunctionFlags::FUNC_BlueprintPure)
        })
        .unwrap_or(false);

    // Override is_pure if the function itself is marked as pure (Array_Get is always pure)
    let is_pure = pure_context || function_is_pure || is_array_get;

    // Create pins based on whether this is Array_Get or a normal function
    let (pins, param_pins, exec_in_pin, exec_out_pin) = if is_array_get {
        let (pins, param_pins) = create_array_get_pins(params, ctx);
        (pins, param_pins, None, None) // Array_Get is pure, no exec pins
    } else {
        create_normal_function_pins(called_func, context_expr, is_pure)
    };

    // Create the appropriate node type based on the function
    let call_node = if is_array_get {
        let node_name = format!("K2Node_GetArrayItem_{}", ctx.next_node_id());
        let mut array_get_node = K2NodeGetArrayItem::new(node_name);
        array_get_node.pins = pins;
        array_get_node.node_pos_x = ctx.node_pos_x();
        array_get_node.node_pos_y = ctx.node_pos_y();
        array_get_node.node_guid = guid_random();
        array_get_node.return_by_ref_desired = true;
        EdGraphNodeObject::K2NodeGetArrayItem(array_get_node)
    } else {
        let node_name = format!("K2Node_CallFunction_{}", ctx.next_node_id());
        let mut call_node = K2NodeCallFunction::new(node_name);

        // Set EdGraphNode properties (via Deref)
        call_node.pins = pins;
        call_node.node_pos_x = ctx.node_pos_x();
        call_node.node_pos_y = ctx.node_pos_y();
        call_node.node_guid = guid_random();

        // Set K2NodeCallFunction specific properties
        call_node.function_reference = MemberReference {
            member_parent: member_parent.map(ExportTextPath::class),
            member_scope: None,
            member_name: Some(func_name),
            member_guid: None,
            self_context: context_expr.is_none(),
            was_deprecated: false,
        };
        call_node.is_pure_func = pure_context || function_is_pure || is_array_get;
        call_node.is_const_func = false;
        call_node.is_interface_call = false;
        call_node.is_final_function = false;

        // Check if this needs to be wrapped in K2NodeCallArrayFunction
        let is_array_function = called_func.is_some_and(|(path, _func)| match path {
            // TODO potentially more
            "/Script/Engine.KismetArrayLibrary:Array_Length" => true,
            _ => false,
        });

        // Wrap in K2NodeCallArrayFunction if needed, otherwise use regular K2NodeCallFunction
        if is_array_function {
            EdGraphNodeObject::K2NodeCallArrayFunction(K2NodeCallArrayFunction { base: call_node })
        } else {
            EdGraphNodeObject::K2NodeCallFunction(call_node)
        }
    };

    let node_name = call_node.name().to_string();

    // Process parameters
    let mut all_nodes = vec![call_node];
    let mut param_connections = Vec::new();
    let mut out_param_set_nodes = Vec::new(); // Track VariableSet nodes for execution flow
    let mut data_outputs = Vec::new(); // Track (param_name, PinConnection) for FunctionEntry connections

    // Process parameters the same way for both pure and impure functions
    for (param_expr, (_param_name, param_pin_id, is_input)) in params.iter().zip(param_pins.iter())
    {
        if *is_input {
            // Process input parameter as data output
            if let Some(param_data) = expr_to_data_output(param_expr, ctx) {
                match param_data {
                    DataOutput::Constant(_const_val) => {
                        // Constant will be inlined on the pin, no connection needed
                        // The pin already has the default value set
                    }
                    DataOutput::Computed {
                        nodes: param_nodes,
                        output_pin,
                        internal_connections: param_conns,
                        function_param_connections: param_func_params,
                    } => {
                        // Add the nodes that compute this parameter
                        all_nodes.extend(param_nodes);
                        param_connections.extend(param_conns);
                        data_outputs.extend(param_func_params);

                        // Connect the parameter output to the function's input pin
                        param_connections.push((
                            output_pin,
                            PinIdentifier {
                                node_name: node_name.clone(),
                                pin_guid: *param_pin_id,
                            },
                        ));
                    }
                    DataOutput::FunctionParameter { param_name } => {
                        // This is a direct function parameter reference
                        // Track it for connection to FunctionEntry at the top level
                        data_outputs.push((
                            param_name,
                            PinIdentifier {
                                node_name: node_name.clone(),
                                pin_guid: *param_pin_id,
                            },
                        ));
                    }
                }
            }
        } else {
            // Output parameters only exist for impure functions
            // Try to create VariableSet node
            if let Some((var_set_node, var_input_pin, _pin_type)) =
                create_variable_set_node(param_expr, ctx)
            {
                let var_node_name = var_set_node.name().to_string();

                // Get exec pins for chaining
                let (var_set_exec_in, var_set_exec_out) = match &var_set_node {
                    EdGraphNodeObject::K2NodeVariableSet(node) => {
                        let exec_in = node
                            .pins
                            .iter()
                            .find(|p| p.pin_name == "execute")
                            .map(|p| p.pin_id)
                            .unwrap();
                        let exec_out = node
                            .pins
                            .iter()
                            .find(|p| p.pin_name == "then")
                            .map(|p| p.pin_id)
                            .unwrap();
                        (exec_in, exec_out)
                    }
                    _ => unreachable!("create_variable_set_node should return K2NodeVariableSet"),
                };

                all_nodes.push(var_set_node);
                out_param_set_nodes.push((
                    var_node_name.clone(),
                    var_set_exec_in,
                    var_set_exec_out,
                ));

                // Track connection: call_node param output -> var_set input
                param_connections.push((
                    PinIdentifier {
                        node_name: node_name.clone(),
                        pin_guid: *param_pin_id,
                    },
                    PinIdentifier {
                        node_name: var_node_name,
                        pin_guid: var_input_pin,
                    },
                ));
            }
            // Note: Direct function parameter out params would be handled by FunctionResult node
        }
    }

    // Determine exec input and output
    // For pure functions with out params, the first VariableSet becomes the exec input
    // For impure functions, use the function's exec pins
    let (exec_input, final_exec_output) = if !out_param_set_nodes.is_empty() {
        // We have VariableSet nodes for out parameters
        let (first_set_name, first_set_exec_in, _) = out_param_set_nodes.first().unwrap();

        // Connect call_node's exec output to first VariableSet's exec input (if function has exec pins)
        if let Some(exec_out) = exec_out_pin {
            param_connections.push((
                PinIdentifier {
                    node_name: node_name.clone(),
                    pin_guid: exec_out,
                },
                PinIdentifier {
                    node_name: first_set_name.clone(),
                    pin_guid: *first_set_exec_in,
                },
            ));
        }

        // Chain VariableSet nodes together if there are multiple
        for i in 0..out_param_set_nodes.len() - 1 {
            let (curr_name, _, curr_exec_out) = &out_param_set_nodes[i];
            let (next_name, next_exec_in, _) = &out_param_set_nodes[i + 1];
            param_connections.push((
                PinIdentifier {
                    node_name: curr_name.clone(),
                    pin_guid: *curr_exec_out,
                },
                PinIdentifier {
                    node_name: next_name.clone(),
                    pin_guid: *next_exec_in,
                },
            ));
        }

        // Exec input is the first VariableSet or func exec if exists
        let exec_input = Some(if let Some(in_pin) = exec_in_pin {
            PinIdentifier {
                node_name: node_name.clone(),
                pin_guid: in_pin,
            }
        } else {
            PinIdentifier {
                node_name: first_set_name.clone(),
                pin_guid: *first_set_exec_in,
            }
        });

        // Exec output is the last VariableSet
        let (last_name, _, last_exec_out) = out_param_set_nodes.last().unwrap();
        let exec_output = Some(PinIdentifier {
            node_name: last_name.clone(),
            pin_guid: *last_exec_out,
        });

        (exec_input, exec_output)
    } else if !is_pure {
        // No out params, but function is impure - use function's exec pins
        let exec_input = exec_in_pin.map(|pin| PinIdentifier {
            node_name: node_name.clone(),
            pin_guid: pin,
        });
        let exec_output = exec_out_pin.map(|pin| PinIdentifier {
            node_name: node_name.clone(),
            pin_guid: pin,
        });
        (exec_input, exec_output)
    } else {
        // Pure function with no out params - no exec flow
        (None, None)
    };

    Some(NodeGraph {
        nodes: all_nodes,
        exec_input,
        exec_outputs: if let Some(out_conn) = final_exec_output {
            vec![(out_conn, ExecTarget::FallThrough)]
        } else {
            vec![]
        },
        data_outputs,
        internal_connections: param_connections,
    })
}

/// Represents a data output from a sub-expression (pure data, no exec flow)
#[derive(Debug, Clone)]
enum DataOutput {
    /// A constant value that should be inlined as a default value on the consuming pin
    Constant(String),
    /// Nodes that compute a value with a pin connection to get the result
    Computed {
        /// All nodes needed to compute this value (e.g., pure function call nodes, variable gets)
        nodes: Vec<EdGraphNodeObject>,
        /// The pin that outputs the computed value
        output_pin: PinIdentifier,
        /// Internal connections within the data expression
        internal_connections: Vec<(PinIdentifier, PinIdentifier)>,
        /// Function parameter connections from nested function calls
        function_param_connections: Vec<(String, PinIdentifier)>,
    },
    /// A reference to a function parameter (should be connected to FunctionEntry output)
    FunctionParameter {
        /// Name of the function parameter
        param_name: String,
    },
}

/// Information about a variable extracted from an expression
#[derive(Debug, Clone)]
struct VariableInfo {
    name: String,
    scope: Option<String>,
    is_self_context: bool,
    pin_type: PinType,
}

impl VariableInfo {
    /// Convert to a MemberReference
    fn to_member_reference(&self) -> MemberReference {
        MemberReference {
            member_parent: None,
            member_scope: self.scope.clone(),
            member_name: Some(self.name.clone()),
            member_guid: None,
            self_context: self.is_self_context,
            was_deprecated: false,
        }
    }
}

/// Helper functions for common pin creation patterns
mod pin_helpers {
    use super::*;

    pub fn create_exec_input_pin(pin_id: Guid) -> Pin {
        Pin {
            pin_id,
            pin_name: "execute".to_string(),
            direction: EEdGraphPinDirection::Input,
            pin_type: pintype_exec(),
            ..Default::default()
        }
    }

    pub fn create_exec_output_pin(pin_id: Guid, name: &str) -> Pin {
        Pin {
            pin_id,
            pin_name: name.to_string(),
            direction: EEdGraphPinDirection::Output,
            pin_type: pintype_exec(),
            ..Default::default()
        }
    }

    pub fn create_data_input_pin(pin_id: Guid, name: String, pin_type: PinType) -> Pin {
        Pin {
            pin_id,
            pin_name: name,
            direction: EEdGraphPinDirection::Input,
            pin_type,
            ..Default::default()
        }
    }

    pub fn create_data_output_pin(pin_id: Guid, name: String, pin_type: PinType) -> Pin {
        Pin {
            pin_id,
            pin_name: name,
            direction: EEdGraphPinDirection::Output,
            pin_type,
            ..Default::default()
        }
    }
}

/// Extract variable information from an expression (LocalVariable or InstanceVariable)
fn extract_variable_info(expr: &Expr, ctx: &PasteContext) -> Option<VariableInfo> {
    match &expr.kind {
        ExprKind::LocalVariable(prop_ref)
        | ExprKind::InstanceVariable(prop_ref)
        | ExprKind::LocalOutVariable(prop_ref) => {
            let prop_info = ctx.address_index.resolve_property(prop_ref.address);
            if prop_info.is_none() {
                warn_property_resolution_failed(
                    "extract_variable_info",
                    prop_ref.address.0,
                    expr.offset.0,
                );
                return None;
            }
            let prop_info = prop_info?;
            let prop = prop_info.property;
            let is_self = matches!(expr.kind, ExprKind::InstanceVariable(_));
            let scope = if is_self {
                None // Instance variables don't have a scope
            } else if is_function_parameter(prop) {
                None // Function parameters don't have a scope
            } else {
                Some(ctx.graph_name().to_string()) // Local variables are scoped to the function
            };

            Some(VariableInfo {
                name: prop.name.clone(),
                scope,
                is_self_context: is_self,
                pin_type: property_to_pin_type(prop),
            })
        }
        _ => {
            eprintln!(
                "WARNING: extract_variable_info called with unsupported expression kind: {:?} (offset: 0x{:X})",
                expr.kind, expr.offset.0
            );
            None
        }
    }
}

/// Apply a DataOutput to a target pin, handling constants, computed values, and function parameters
/// Returns (nodes_to_add, connections_to_add, function_param_connections)
fn apply_data_output(
    data_output: DataOutput,
    target_pin: PinIdentifier,
) -> (
    Vec<EdGraphNodeObject>,
    Vec<(PinIdentifier, PinIdentifier)>,
    Vec<(String, PinIdentifier)>,
) {
    match data_output {
        DataOutput::Constant(_) => {
            // Constant is inlined on the pin, no nodes or connections needed
            (vec![], vec![], vec![])
        }
        DataOutput::Computed {
            nodes,
            output_pin,
            internal_connections,
            function_param_connections,
        } => {
            // Add the computing nodes and connect the output to the target
            let mut connections = internal_connections;
            connections.push((output_pin, target_pin));
            (nodes, connections, function_param_connections)
        }
        DataOutput::FunctionParameter { param_name } => {
            // Track this for connection to FunctionEntry
            (vec![], vec![], vec![(param_name, target_pin)])
        }
    }
}

/// Create an IfThenElse branch node with a condition
/// Returns NodeGraph with the branch node and optional variable get for the condition
fn create_branch_node(
    condition_expr: &Expr,
    then_target: ExecTarget,
    else_target: ExecTarget,
    ctx: &mut PasteContext,
) -> Option<NodeGraph> {
    let node_name = format!("K2Node_IfThenElse_{}", ctx.next_node_id());

    let exec_in_pin = guid_random();
    let condition_pin = guid_random();
    let then_pin = guid_random();
    let else_pin = guid_random();

    // Create VariableGet for condition if needed
    let mut all_nodes = Vec::new();
    let mut internal_connections = Vec::new();

    if let Some((var_get_node, var_output_pin, _pin_type)) =
        create_variable_get_node(condition_expr, ctx)
    {
        let var_node_name = var_get_node.name().to_string();
        all_nodes.push(var_get_node);

        // Connect condition variable to the Condition pin
        internal_connections.push((
            PinIdentifier {
                node_name: var_node_name,
                pin_guid: var_output_pin,
            },
            PinIdentifier {
                node_name: node_name.clone(),
                pin_guid: condition_pin,
            },
        ));
    }

    let mut if_node = K2NodeIfThenElse::new(node_name.clone());

    // Set EdGraphNode properties (via Deref)
    if_node.pins = vec![
        pin_helpers::create_exec_input_pin(exec_in_pin),
        Pin {
            pin_id: condition_pin,
            pin_name: "Condition".to_string(),
            direction: EEdGraphPinDirection::Input,
            pin_type: pintype_bool(),
            default_value: "true".to_string(),
            autogenerated_default_value: "true".to_string(),
            ..Default::default()
        },
        Pin {
            pin_id: then_pin,
            pin_name: "then".to_string(),
            pin_friendly_name: Some(FText::literal("true".to_string())),
            direction: EEdGraphPinDirection::Output,
            pin_type: pintype_exec(),
            ..Default::default()
        },
        Pin {
            pin_id: else_pin,
            pin_name: "else".to_string(),
            pin_friendly_name: Some(FText::literal("false".to_string())),
            direction: EEdGraphPinDirection::Output,
            pin_type: pintype_exec(),
            ..Default::default()
        },
    ];
    if_node.node_pos_x = ctx.node_pos_x();
    if_node.node_pos_y = ctx.node_pos_y();
    if_node.node_guid = guid_random();

    all_nodes.push(EdGraphNodeObject::K2NodeIfThenElse(if_node));

    Some(NodeGraph {
        nodes: all_nodes,
        exec_input: Some(PinIdentifier {
            node_name: node_name.clone(),
            pin_guid: exec_in_pin,
        }),
        exec_outputs: vec![
            (
                PinIdentifier {
                    node_name: node_name.clone(),
                    pin_guid: then_pin,
                },
                then_target,
            ),
            (
                PinIdentifier {
                    node_name,
                    pin_guid: else_pin,
                },
                else_target,
            ),
        ],
        data_outputs: vec![],
        internal_connections,
    })
}

/// Convert a sub-expression to a data output (for pure data expressions like function parameters)
/// This is for expressions used in data context (not top-level statements)
fn expr_to_data_output(expr: &Expr, ctx: &mut PasteContext) -> Option<DataOutput> {
    match &expr.kind {
        // Boolean constants
        ExprKind::True => Some(DataOutput::Constant("true".to_string())),
        ExprKind::False => Some(DataOutput::Constant("false".to_string())),

        // Integer constants
        ExprKind::IntConst(val) => Some(DataOutput::Constant(val.to_string())),
        ExprKind::Int64Const(val) => Some(DataOutput::Constant(val.to_string())),
        ExprKind::UInt64Const(val) => Some(DataOutput::Constant(val.to_string())),
        ExprKind::IntZero => Some(DataOutput::Constant("0".to_string())),
        ExprKind::IntOne => Some(DataOutput::Constant("1".to_string())),
        ExprKind::ByteConst(val) | ExprKind::IntConstByte(val) => {
            Some(DataOutput::Constant(val.to_string()))
        }
        ExprKind::FloatConst(val) => Some(DataOutput::Constant(val.to_string())),
        ExprKind::StringConst(val) | ExprKind::UnicodeStringConst(val) => {
            Some(DataOutput::Constant(val.clone()))
        }
        ExprKind::NameConst(val) => Some(DataOutput::Constant(val.as_str().to_string())),
        ExprKind::NoObject | ExprKind::NoInterface => {
            Some(DataOutput::Constant("None".to_string()))
        }
        ExprKind::Nothing | ExprKind::NothingInt32 => Some(DataOutput::Constant("".to_string())),

        // Variable reads: check if function parameter, otherwise create VariableGet nodes
        ExprKind::LocalVariable(_) | ExprKind::InstanceVariable(_) => {
            // First check if this is a function parameter
            if let Some(param_name) = get_parameter_name(expr, ctx) {
                return Some(DataOutput::FunctionParameter { param_name });
            }

            // Otherwise create a VariableGet node
            if let Some((var_get_node, output_pin, _pin_type)) = create_variable_get_node(expr, ctx)
            {
                let node_name = var_get_node.name().to_string();
                Some(DataOutput::Computed {
                    nodes: vec![var_get_node],
                    output_pin: PinIdentifier {
                        node_name,
                        pin_guid: output_pin,
                    },
                    internal_connections: vec![],
                    function_param_connections: vec![],
                })
            } else {
                None
            }
        }

        // Context wraps another expression
        ExprKind::Context { context, .. } => {
            // Recursively process the context expression
            expr_to_data_output(context, ctx)
        }

        // Pure function calls
        ExprKind::VirtualFunction { func, params }
        | ExprKind::LocalVirtualFunction { func, params }
        | ExprKind::FinalFunction { func, params }
        | ExprKind::LocalFinalFunction { func, params }
        | ExprKind::CallMath { func, params } => {
            // Create as a pure function call (CallMath is always pure)
            let node_graph = create_function_call_node(func, params, ctx, None, true)?;

            // Find the return value pin
            // Try to find "ReturnValue" first (standard UE convention), otherwise first non-exec output
            let call_node = node_graph.nodes.first()?;

            // Get pins based on node type
            let pins = &call_node.as_ed_graph_node().pins;

            let return_pin = pins
                .iter()
                .find(|p| {
                    p.pin_name == "ReturnValue" && p.direction == EEdGraphPinDirection::Output
                })
                .or_else(|| {
                    pins.iter().find(|p| {
                        p.direction == EEdGraphPinDirection::Output
                            && p.pin_type.pin_category != "exec"
                    })
                })?;

            // Clone the data we need before moving node_graph
            let node_name = call_node.name().to_string();
            let return_pin_id = return_pin.pin_id;

            Some(DataOutput::Computed {
                nodes: node_graph.nodes,
                output_pin: PinIdentifier {
                    node_name,
                    pin_guid: return_pin_id,
                },
                internal_connections: node_graph.internal_connections,
                function_param_connections: node_graph.data_outputs,
            })
        }

        other => {
            eprintln!(
                "WARNING: expr_to_data_output - unhandled expression kind: {:?} (offset: 0x{:X})",
                other, expr.offset.0
            );
            None
        }
    }
}

/// Convert an expression to a Blueprint node graph
///
/// Returns a NodeGraph containing all nodes needed for this expression, along with
/// connection points for linking to other expressions.
///
/// `context_expr` is an optional expression that provides the target/self object for this operation
fn expr_to_node(
    expr: &Expr,
    ctx: &mut PasteContext,
    context_expr: Option<&Expr>,
) -> Option<NodeGraph> {
    match &expr.kind {
        // Context expression unwraps the inner operation with a target object
        ExprKind::Context {
            object, context, ..
        } => {
            // Recursively process the context expression with the object as the target
            expr_to_node(context, ctx, Some(object))
        }

        ExprKind::VirtualFunction {
            func: func_ref,
            params,
        }
        | ExprKind::LocalVirtualFunction {
            func: func_ref,
            params,
        }
        | ExprKind::FinalFunction {
            func: func_ref,
            params,
        }
        | ExprKind::LocalFinalFunction {
            func: func_ref,
            params,
        } => create_function_call_node(func_ref, params, ctx, context_expr, false),

        ExprKind::Let {
            variable, value, ..
        }
        | ExprKind::LetBool { variable, value }
        | ExprKind::LetObj { variable, value }
        | ExprKind::LetWeakObjPtr { variable, value }
        | ExprKind::LetDelegate { variable, value }
        | ExprKind::LetMulticastDelegate { variable, value } => {
            // Extract variable information from the variable expression
            let var_info = extract_variable_info(variable, ctx).unwrap_or_else(|| {
                eprintln!(
                    "WARNING: Let expression at offset 0x{:X} - using fallback UNKNOWN_VAR for variable",
                    expr.offset.0
                );
                VariableInfo {
                    name: "UNKNOWN_VAR".to_string(),
                    scope: None,
                    is_self_context: true,
                    pin_type: pintype_object("/Script/CoreUObject.Object"),
                }
            });

            // Process the value expression as a data output
            let value_data = expr_to_data_output(value, ctx)?;

            let node_name = format!("K2Node_VariableSet_{}", ctx.next_node_id());

            let exec_in_pin = guid_random();
            let exec_out_pin = guid_random();
            let value_input_pin = guid_random();
            let output_get_pin = guid_random();

            // Build the value input pin with optional default value
            let value_pin = match &value_data {
                DataOutput::Constant(const_val) => Pin {
                    pin_id: value_input_pin,
                    pin_name: var_info.name.clone(),
                    direction: EEdGraphPinDirection::Input,
                    pin_type: var_info.pin_type.clone(),
                    default_value: const_val.clone(),
                    autogenerated_default_value: const_val.clone(),
                    ..Default::default()
                },
                DataOutput::Computed { .. } | DataOutput::FunctionParameter { .. } => Pin {
                    pin_id: value_input_pin,
                    pin_name: var_info.name.clone(),
                    direction: EEdGraphPinDirection::Input,
                    pin_type: var_info.pin_type.clone(),
                    ..Default::default()
                },
            };

            let mut var_set_node = K2NodeVariableSet::new(node_name.clone());

            // Set EdGraphNode properties (via Deref)
            var_set_node.pins = vec![
                pin_helpers::create_exec_input_pin(exec_in_pin),
                pin_helpers::create_exec_output_pin(exec_out_pin, "then"),
                value_pin,
                Pin {
                    pin_id: output_get_pin,
                    pin_name: "Output_Get".to_string(),
                    pin_tool_tip: "Retrieves the value of the variable, can use instead of a separate Get node".to_string(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type: var_info.pin_type.clone(),
                    ..Default::default()
                },
            ];
            var_set_node.node_pos_x = ctx.node_pos_x();
            var_set_node.node_pos_y = ctx.node_pos_y();
            var_set_node.node_guid = guid_random();

            // Set K2NodeVariableSet specific properties
            var_set_node.variable_reference = var_info.to_member_reference();
            var_set_node.self_context_info = ESelfContextInfo::Unspecified;

            // Handle nodes and connections based on whether value is constant, computed, or a function parameter
            let (value_nodes, value_connections, data_outputs_local) = apply_data_output(
                value_data,
                PinIdentifier {
                    node_name: node_name.clone(),
                    pin_guid: value_input_pin,
                },
            );

            let mut all_nodes = value_nodes;
            all_nodes.push(EdGraphNodeObject::K2NodeVariableSet(var_set_node));
            let internal_connections = value_connections;

            Some(NodeGraph {
                nodes: all_nodes,
                exec_input: Some(PinIdentifier {
                    node_name: node_name.clone(),
                    pin_guid: exec_in_pin,
                }),
                exec_outputs: vec![(
                    PinIdentifier {
                        node_name,
                        pin_guid: exec_out_pin,
                    },
                    ExecTarget::FallThrough,
                )],
                data_outputs: data_outputs_local,
                internal_connections,
            })
        }

        ExprKind::Return(_return_expr) => {
            let node_name = format!("K2Node_FunctionResult_{}", ctx.next_node_id());

            let exec_in_pin = guid_random();

            let mut pins = vec![pin_helpers::create_exec_input_pin(exec_in_pin)];

            // Add pins for return parameters and out parameters
            for prop in &ctx.func.1.r#struct.properties {
                if prop.flags.contains(jmap::EPropertyFlags::CPF_Parm)
                    && (prop.flags.contains(jmap::EPropertyFlags::CPF_ReturnParm)
                        || prop.flags.contains(jmap::EPropertyFlags::CPF_OutParm))
                {
                    let pin_type = property_to_pin_type(prop);
                    pins.push(Pin {
                        pin_id: guid_random(),
                        pin_name: prop.name.clone(),
                        direction: EEdGraphPinDirection::Input,
                        pin_type,
                        ..Default::default()
                    });
                }
            }

            let mut node = K2NodeFunctionResult::new(node_name.clone());

            // Set EdGraphNode properties (via Deref)
            node.pins = pins;
            node.node_pos_x = ctx.node_pos_x();
            node.node_pos_y = ctx.node_pos_y();
            node.node_guid = guid_random();

            // Set K2NodeFunctionResult specific properties
            node.function_reference = MemberReference {
                member_parent: None,
                member_scope: None,
                member_name: Some(ctx.graph_name().to_string()),
                member_guid: None,
                self_context: false,
                was_deprecated: false,
            };
            node.is_editable = true;

            Some(NodeGraph {
                nodes: vec![EdGraphNodeObject::K2NodeFunctionResult(node)],
                exec_input: Some(PinIdentifier {
                    node_name,
                    pin_guid: exec_in_pin,
                }),
                exec_outputs: vec![], // Return has no exec output
                data_outputs: vec![],
                internal_connections: vec![],
            })
        }

        ExprKind::JumpIfNot { condition, target } => {
            // Use helper to create branch node
            // "then" pin (condition true) continues to next expression
            // "else" pin (condition false) jumps to target
            create_branch_node(
                condition,
                ExecTarget::FallThrough,
                ExecTarget::Offset(*target),
                ctx,
            )
        }

        ExprKind::SetArray {
            array_expr,
            elements,
        } => {
            // SetArray is a top-level statement that creates:
            // 1. MakeArray node (pure data)
            // 2. VariableSet node (exec flow) connected to the MakeArray output

            // Extract variable information from array_expr
            let var_info = extract_variable_info(array_expr, ctx).unwrap_or_else(|| {
                eprintln!(
                    "WARNING: SetArray expression at offset 0x{:X} - using fallback UNKNOWN_VAR for array variable",
                    expr.offset.0
                );
                VariableInfo {
                    name: "UNKNOWN_VAR".to_string(),
                    scope: None,
                    is_self_context: false,
                    pin_type: PinType {
                        pin_category: "string".to_string(),
                        container_type: Some("Array".to_string()),
                        ..Default::default()
                    },
                }
            });

            // Create MakeArray node
            let make_array_name = format!("K2Node_MakeArray_{}", ctx.next_node_id());
            let array_output_pin = guid_random();

            // Get element type (without container)
            let element_pin_type = PinType {
                pin_category: var_info.pin_type.pin_category.clone(),
                pin_sub_category: var_info.pin_type.pin_sub_category.clone(),
                pin_sub_category_object: var_info.pin_type.pin_sub_category_object.clone(),
                container_type: None,
                ..var_info.pin_type.clone()
            };

            let mut make_array_pins = vec![pin_helpers::create_data_output_pin(
                array_output_pin,
                "Array".to_string(),
                var_info.pin_type.clone(),
            )];

            let mut all_nodes = Vec::new();
            let mut internal_connections = Vec::new();
            let mut data_outputs = Vec::new();

            // Process each element
            for (idx, element_expr) in elements.iter().enumerate() {
                let element_pin_id = guid_random();
                let mut element_pin = Pin {
                    pin_id: element_pin_id,
                    pin_name: format!("[{}]", idx),
                    direction: EEdGraphPinDirection::Input,
                    pin_type: element_pin_type.clone(),
                    ..Default::default()
                };

                // Process element as data output
                if let Some(element_data) = expr_to_data_output(element_expr, ctx) {
                    match element_data {
                        DataOutput::Constant(const_val) => {
                            element_pin.default_value = const_val.clone();
                            element_pin.autogenerated_default_value = const_val;
                        }
                        DataOutput::Computed {
                            nodes,
                            output_pin,
                            internal_connections: elem_conns,
                            function_param_connections: elem_func_params,
                        } => {
                            all_nodes.extend(nodes);
                            internal_connections.extend(elem_conns);
                            internal_connections.push((
                                output_pin,
                                PinIdentifier {
                                    node_name: make_array_name.clone(),
                                    pin_guid: element_pin_id,
                                },
                            ));
                            data_outputs.extend(elem_func_params);
                        }
                        DataOutput::FunctionParameter { .. } => {
                            // Leave unconnected for now
                        }
                    }
                }

                make_array_pins.push(element_pin);
            }

            let mut make_array_node = K2NodeMakeArray::new(make_array_name.clone());

            // Set EdGraphNode properties (via Deref)
            make_array_node.pins = make_array_pins;
            make_array_node.node_pos_x = ctx.node_pos_x();
            make_array_node.node_pos_y = ctx.node_pos_y();
            make_array_node.node_guid = guid_random();

            // Set K2NodeMakeArray specific properties
            make_array_node.num_inputs = elements.len() as i32;

            all_nodes.push(EdGraphNodeObject::K2NodeMakeArray(make_array_node));

            // Create VariableSet node
            let var_set_name = format!("K2Node_VariableSet_{}", ctx.next_node_id());
            let exec_in_pin = guid_random();
            let exec_out_pin = guid_random();
            let value_input_pin = guid_random();
            let output_get_pin = guid_random();

            let mut var_set_node = K2NodeVariableSet::new(var_set_name.clone());

            // Set EdGraphNode properties (via Deref)
            var_set_node.pins = vec![
                pin_helpers::create_exec_input_pin(exec_in_pin),
                pin_helpers::create_exec_output_pin(exec_out_pin, "then"),
                pin_helpers::create_data_input_pin(
                    value_input_pin,
                    var_info.name.clone(),
                    var_info.pin_type.clone(),
                ),
                Pin {
                    pin_id: output_get_pin,
                    pin_name: "Output_Get".to_string(),
                    pin_tool_tip: "Retrieves the value of the variable, can use instead of a separate Get node".to_string(),
                    direction: EEdGraphPinDirection::Output,
                    pin_type: var_info.pin_type.clone(),
                    ..Default::default()
                },
            ];
            var_set_node.node_pos_x = ctx.node_pos_x() + VARIABLE_SET_NODE_OFFSET_X;
            var_set_node.node_pos_y = ctx.node_pos_y();
            var_set_node.node_guid = guid_random();

            // Set K2NodeVariableSet specific properties
            var_set_node.variable_reference = var_info.to_member_reference();
            var_set_node.self_context_info = ESelfContextInfo::Unspecified;

            all_nodes.push(EdGraphNodeObject::K2NodeVariableSet(var_set_node));

            // Connect MakeArray output to VariableSet input
            internal_connections.push((
                PinIdentifier {
                    node_name: make_array_name,
                    pin_guid: array_output_pin,
                },
                PinIdentifier {
                    node_name: var_set_name.clone(),
                    pin_guid: value_input_pin,
                },
            ));

            Some(NodeGraph {
                nodes: all_nodes,
                exec_input: Some(PinIdentifier {
                    node_name: var_set_name.clone(),
                    pin_guid: exec_in_pin,
                }),
                exec_outputs: vec![(
                    PinIdentifier {
                        node_name: var_set_name,
                        pin_guid: exec_out_pin,
                    },
                    ExecTarget::FallThrough,
                )],
                data_outputs: vec![],
                internal_connections,
            })
        }

        ExprKind::PopExecutionFlowIfNot { condition } => {
            // PopExecutionFlowIfNot creates a branch:
            // - If condition is TRUE: fall through to next expression (don't pop)
            // - If condition is FALSE: pop execution flow stack and jump to continuation
            create_branch_node(
                condition,
                ExecTarget::FallThrough,
                ExecTarget::PopExecutionFlow,
                ctx,
            )
        }

        other => {
            panic!("expr_to_node: unsupported expression kind: {:?}", other);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{
        address_index::AddressIndex, parser::ScriptParser, reader::ScriptReader,
    };
    use crate::dot::{Attributes, Edge, Graph, Node, XmlTag};
    use std::fs;

    /// Render a NodeGraph to DOT format for visualization
    fn render_node_graph_to_dot(node_graph: &NodeGraph) -> String {
        let mut graph = Graph::new("digraph");

        // Set graph attributes for better layout
        graph.base.graph_attributes.add("rankdir", "LR");
        graph.base.node_attributes.add("shape", "plaintext");
        graph.base.node_attributes.add("fontname", "monospace");
        graph.base.node_attributes.add("fontsize", "10");

        // Process each node in the graph
        for ed_node in &node_graph.nodes {
            let node = ed_node.as_ed_graph_node();
            let node_name = &node.name;

            let header_color = match ed_node {
                EdGraphNodeObject::K2NodeFunctionEntry(_) => "lightgreen",
                EdGraphNodeObject::K2NodeFunctionResult(_) => "lightcoral",
                EdGraphNodeObject::K2NodeCallFunction(_)
                | EdGraphNodeObject::K2NodeCallArrayFunction(_) => "lightblue",
                EdGraphNodeObject::K2NodeVariableGet(_) => "lightyellow",
                EdGraphNodeObject::K2NodeVariableSet(_) => "lightgoldenrodyellow",
                EdGraphNodeObject::K2NodeIfThenElse(_) => "plum",
                EdGraphNodeObject::K2NodeExecutionSequence(_) => "lightcyan",
                EdGraphNodeObject::K2NodeMakeArray(_) => "peachpuff",
                EdGraphNodeObject::K2NodeGetArrayItem(_) => "wheat",
                _ => "white",
            };

            // Build HTML table for the node
            let mut rows = vec![
                // Header row with node type
                XmlTag::new("TR").child(
                    XmlTag::new("TD")
                        .attr("COLSPAN", "2")
                        .attr("BGCOLOR", header_color)
                        .attr("ALIGN", "center")
                        .child(node_name),
                ),
            ];

            // Separate input and output pins
            let mut input_pins = Vec::new();
            let mut output_pins = Vec::new();

            for pin in &node.pins {
                match pin.direction {
                    EEdGraphPinDirection::Input => input_pins.push(pin),
                    EEdGraphPinDirection::Output => output_pins.push(pin),
                }
            }

            // Determine the maximum number of rows needed
            let max_pins = input_pins.len().max(output_pins.len());

            // Create rows with inputs on the left and outputs on the right
            for i in 0..max_pins {
                let mut row = XmlTag::new("TR");

                // Left cell (input pin)
                if let Some(pin) = input_pins.get(i) {
                    let pin_color = if pin.pin_type.pin_category == "exec" {
                        "white"
                    } else {
                        "lightyellow"
                    };
                    let pin_info = format!("{} ({})", pin.pin_name, pin.pin_type.pin_category);

                    row = row.child(
                        XmlTag::new("TD")
                            .attr("BGCOLOR", pin_color)
                            .attr("ALIGN", "left")
                            .attr("PORT", format!("pin_{}", pin.pin_id))
                            .child(pin_info),
                    );
                } else {
                    // Empty cell if no input pin at this position
                    row = row.child(XmlTag::new("TD"));
                }

                // Right cell (output pin)
                if let Some(pin) = output_pins.get(i) {
                    let pin_color = if pin.pin_type.pin_category == "exec" {
                        "white"
                    } else {
                        "lightyellow"
                    };
                    let pin_info = format!("{} ({})", pin.pin_name, pin.pin_type.pin_category);

                    row = row.child(
                        XmlTag::new("TD")
                            .attr("BGCOLOR", pin_color)
                            .attr("ALIGN", "right")
                            .attr("PORT", format!("pin_{}", pin.pin_id))
                            .child(pin_info),
                    );
                } else {
                    // Empty cell if no output pin at this position
                    row = row.child(XmlTag::new("TD"));
                }

                rows.push(row);
            }

            let table = XmlTag::new("TABLE")
                .attr("BORDER", "1")
                .attr("CELLBORDER", "1")
                .attr("CELLSPACING", "0")
                .attr("CELLPADDING", "4")
                .body(rows);

            let mut attrs = Attributes::default();
            attrs.add("label", crate::dot::Id::Html(table.into()));

            graph
                .base
                .nodes
                .push(Node::new_attr(node_name.as_str(), attrs));
        }

        // Add edges for internal connections
        for (from_pin, to_pin) in &node_graph.internal_connections {
            let from_port = format!("pin_{}:e", from_pin.pin_guid);
            let to_port = format!("pin_{}:w", to_pin.pin_guid);

            let mut edge = Edge::new_compass(
                from_pin.node_name.as_str(),
                Some(from_port.as_str()),
                to_pin.node_name.as_str(),
                Some(to_port.as_str()),
                std::iter::empty::<(String, String)>(),
            );

            edge.attributes.add("color", "blue");
            graph.base.edges.push(edge);
        }

        // Render to string
        let mut output = String::new();
        graph.write(&mut output).unwrap();
        output
    }

    #[test]
    fn test_paste_expr() {
        // Load the JMAP file
        let jmap_data = std::io::BufReader::new(fs::File::open("fsd_cd2.jmap").unwrap());
        let jmap: jmap::Jmap = serde_json::from_reader(jmap_data).expect("Failed to parse JMAP");

        // Find the function
        let function_path =
            "/Game/_AssemblyStorm/CustomDifficulty2/Modules/Darkness.Darkness_C:ValueOrDefault";
        let func = jmap
            .objects
            .get(function_path)
            .and_then(|obj| {
                if let jmap::ObjectType::Function(f) = obj {
                    Some(f)
                } else {
                    None
                }
            })
            .expect("Failed to find function in JMAP");

        // Build address index
        let address_index = AddressIndex::new(&jmap);

        // Parse the bytecode
        let script = &func.r#struct.script;
        let reader = ScriptReader::new(
            script,
            jmap.names.as_ref().expect("name map is required"),
            &address_index,
        );
        let mut parser = ScriptParser::new(reader);
        let exprs = parser.parse_all();

        for (i, e) in exprs.iter().enumerate() {
            println!("{i}: {e:?}");
        }

        let expr = &exprs[2];

        // Create context
        let mut ctx = PasteContext::new((function_path, func), &jmap, &address_index);

        // Convert the first expression to a node graph
        let node_graph =
            expr_to_node(expr, &mut ctx, None).expect("Failed to convert expression to node graph");

        // Verify we got a node graph with nodes
        assert!(!node_graph.nodes.is_empty(), "No nodes generated");

        // Print debug info
        println!("First expression: {:?}", expr.kind);
        println!("Generated {} nodes", node_graph.nodes.len());
        println!("Exec input: {:?}", node_graph.exec_input);
        println!("Exec outputs: {:?}", node_graph.exec_outputs);
        println!("Data outputs: {:?}", node_graph.data_outputs);

        // Render to DOT format
        let dot_output = render_node_graph_to_dot(&node_graph);
        println!("\n=== DOT Graph ===\n{}", dot_output);

        // Write to file
        fs::write("output.dot", &dot_output).expect("Failed to write output.dot");
        println!("\nDOT graph written to output.dot");
        println!("Render with: dot -Tpng output.dot -o output.png");
    }
}
