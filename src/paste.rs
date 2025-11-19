use crate::bytecode::{
    address_index::AddressIndex,
    expr::{Expr, ExprKind},
    refs::FunctionRef,
    types::BytecodeOffset,
};
use paste_buffer::{
    BPGraph, ExportTextPath, FunctionReference, Guid, Node, NodeData, Pin, PinConnection,
    PinDirection, PinFlags, PinType, UserDefinedPin,
};
use std::collections::{HashMap, VecDeque};

// Constants for node positioning
const NODE_SPACING_X: i32 = 300;
const NODE_SPACING_Y: i32 = 0;
const VARIABLE_NODE_SPACING_X: i32 = 200;
const VARIABLE_NODE_SPACING_Y: i32 = 80;
const VARIABLE_SET_NODE_OFFSET_X: i32 = 200;

// Default property flags for local variables
const DEFAULT_LOCAL_VAR_FLAGS: u64 = 5;

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
    nodes: Vec<Node>,
    /// The exec input pin (if this graph has execution flow)
    exec_input: Option<PinConnection>,
    /// Exec output pins mapped to their targets
    /// For linear flow, target is FallThrough
    /// For branches (if/else), multiple outputs with different targets
    exec_outputs: Vec<(PinConnection, ExecTarget)>,
    /// Data output pins by parameter name (for value expressions)
    data_outputs: Vec<(String, PinConnection)>, // (param_name, pin_connection)
    /// Internal data connections within this graph (from, to)
    internal_connections: Vec<(PinConnection, PinConnection)>,
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

    let mut graph = BPGraph {
        graph_name: graph_name.to_string(),
        graph_type: "GT_Function".to_string(),
        original_blueprint: ExportTextPath::blueprint(
            "/Game/Decompiled/GeneratedFunction.GeneratedFunction",
        ),
        nodes: vec![],
    };

    // Create function entry node with parameters
    let entry_node = create_function_entry_node(graph_name, func);
    let entry_node_name = entry_node.name.clone();

    // Build a map of parameter names to pin IDs for the entry node
    let mut entry_param_pins = std::collections::HashMap::new();
    let mut entry_exec_pin = None;
    for pin in &entry_node.pins {
        if pin.pin_name == "then" {
            entry_exec_pin = Some(pin.pin_id);
        } else {
            entry_param_pins.insert(pin.pin_name.clone(), pin.pin_id);
        }
    }

    graph.add_node(entry_node);

    // Create context for node generation
    let mut ctx = PasteContext::new((function_name, func), jmap, address_index);

    // Build a map of BytecodeOffset -> Expr index for quick lookup
    let expr_map: HashMap<BytecodeOffset, usize> = expressions
        .iter()
        .enumerate()
        .map(|(idx, expr)| (expr.offset, idx))
        .collect();

    // Map of BytecodeOffset -> PinConnection for connecting control flow
    let mut offset_to_exec_input: HashMap<BytecodeOffset, PinConnection> = HashMap::new();

    // Map of BytecodeOffset -> NodeGraph for processed expressions
    let mut processed_graphs: HashMap<BytecodeOffset, NodeGraph> = HashMap::new();

    // Queue of (offset, prev_exec_output) to process
    let mut queue: VecDeque<(BytecodeOffset, Option<PinConnection>)> = VecDeque::new();

    // Start with the first expression, connected to FunctionEntry
    if let Some(first_expr) = expressions.first() {
        let entry_exec = entry_exec_pin.map(|pin| PinConnection {
            node_name: entry_node_name.clone(),
            pin_id: pin,
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
                graph.connect_pins(
                    &prev_conn.node_name,
                    &prev_conn.pin_id,
                    &curr_conn.node_name,
                    &curr_conn.pin_id,
                );
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
            let exec_in_pin = Guid::random();
            let then_0_pin = Guid::random();
            let then_1_pin = Guid::random();

            let seq_node = Node {
                name: node_name.clone(),
                guid: Guid::random(),
                pos_x: ctx.node_pos_x(),
                pos_y: ctx.node_pos_y(),
                advanced_pin_display: None,
                node_data: NodeData::ExecutionSequence,
                pins: vec![
                    Pin {
                        pin_id: exec_in_pin,
                        pin_name: "execute".to_string(),
                        direction: Some(PinDirection::Input),
                        pin_type: PinType::exec(),
                        ..Default::default()
                    },
                    Pin {
                        pin_id: then_0_pin,
                        pin_name: "then_0".to_string(),
                        direction: Some(PinDirection::Output),
                        pin_type: PinType::exec(),
                        ..Default::default()
                    },
                    Pin {
                        pin_id: then_1_pin,
                        pin_name: "then_1".to_string(),
                        direction: Some(PinDirection::Output),
                        pin_type: PinType::exec(),
                        ..Default::default()
                    },
                ],
                user_defined_pins: vec![],
            };

            graph.add_node(seq_node);

            // Connect to previous exec
            if let Some(prev_conn) = prev_exec {
                graph.connect_pins(
                    &prev_conn.node_name,
                    &prev_conn.pin_id,
                    &node_name,
                    &exec_in_pin,
                );
            }

            // Register exec input for this offset
            offset_to_exec_input.insert(
                offset,
                PinConnection {
                    node_name: node_name.clone(),
                    pin_id: exec_in_pin,
                },
            );

            // Queue then_0 to fall through to next expression
            if let Some(next_expr) = expressions.get(expr_idx + 1) {
                queue.push_back((
                    next_expr.offset,
                    Some(PinConnection {
                        node_name: node_name.clone(),
                        pin_id: then_0_pin,
                    }),
                ));
            }

            // Queue then_1 to jump to the pushed address
            queue.push_back((
                *push_offset,
                Some(PinConnection {
                    node_name: node_name.clone(),
                    pin_id: then_1_pin,
                }),
            ));

            // Mark as processed
            processed_graphs.insert(
                offset,
                NodeGraph {
                    nodes: vec![],
                    exec_input: Some(PinConnection {
                        node_name,
                        pin_id: exec_in_pin,
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

        // Register this node's exec input for control flow connections
        if let Some(pin_connection) = &node_graph.exec_input {
            offset_to_exec_input.insert(offset, pin_connection.clone());
        }

        // Add all nodes from this graph
        for node in &node_graph.nodes {
            graph.add_node(node.clone());
        }

        // Connect execution flow from previous node
        if let (Some(prev_conn), Some(curr_conn)) = (prev_exec, &node_graph.exec_input) {
            graph.connect_pins(
                &prev_conn.node_name,
                &prev_conn.pin_id,
                &curr_conn.node_name,
                &curr_conn.pin_id,
            );
        }

        // Connect internal data pins (e.g., VariableGet -> CallFunction params)
        for (from, to) in &node_graph.internal_connections {
            graph.connect_pins(&from.node_name, &from.pin_id, &to.node_name, &to.pin_id);
        }

        // Connect data_outputs from FunctionEntry to call nodes
        for (param_name, target_conn) in &node_graph.data_outputs {
            if let Some(entry_pin_id) = entry_param_pins.get(param_name) {
                graph.connect_pins(
                    &entry_node_name,
                    entry_pin_id,
                    &target_conn.node_name,
                    &target_conn.pin_id,
                );
            }
        }

        // Queue all exec output targets
        for (out_conn, exec_target) in &node_graph.exec_outputs {
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

        processed_graphs.insert(offset, node_graph);
    }

    // Serialize and print the graph
    let output = graph.serialize();
    println!("{}", output);
}

fn create_function_entry_node(function_name: &str, func: &jmap::Function) -> Node {
    use paste_buffer::LocalVariable;

    // Create user-defined pins for function parameters
    let mut user_defined_pins = Vec::new();
    let mut pins = vec![Pin {
        pin_id: Guid::random(),
        pin_name: "then".to_string(),
        pin_tooltip: None,
        pin_friendly_name: None,
        direction: Some(PinDirection::Output),
        pin_type: PinType::exec(),
        default_value: None,
        autogenerated_default_value: None,
        default_text_value: None,
        default_object: None,
        linked_to: vec![],
        persistent_guid: Guid::zero(),
        flags: PinFlags::default(),
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
                    desired_pin_direction: PinDirection::Input,
                    pin_default_value: None,
                });

                pins.push(Pin {
                    pin_id: Guid::random(),
                    pin_name: prop.name.clone(),
                    pin_tooltip: None,
                    pin_friendly_name: None,
                    direction: Some(PinDirection::Input),
                    pin_type,
                    default_value: None,
                    autogenerated_default_value: None,
                    default_text_value: None,
                    default_object: None,
                    linked_to: vec![],
                    persistent_guid: Guid::zero(),
                    flags: PinFlags::default(),
                });
            } else {
                // Regular input parameters are outputs on the function entry node
                user_defined_pins.push(UserDefinedPin {
                    pin_name: prop.name.clone(),
                    pin_type: pin_type.clone(),
                    desired_pin_direction: PinDirection::Output,
                    pin_default_value: None,
                });

                pins.push(Pin {
                    pin_id: Guid::random(),
                    pin_name: prop.name.clone(),
                    pin_tooltip: None,
                    pin_friendly_name: None,
                    direction: Some(PinDirection::Output),
                    pin_type,
                    default_value: None,
                    autogenerated_default_value: None,
                    default_text_value: None,
                    default_object: None,
                    linked_to: vec![],
                    persistent_guid: Guid::zero(),
                    flags: PinFlags::default(),
                });
            }
        } else {
            // This is a local variable
            local_variables.push(LocalVariable {
                var_name: prop.name.clone(),
                var_guid: Guid::random(),
                var_type: property_to_pin_type(prop),
                friendly_name: Some(prop.name.clone()),
                category: None,
                property_flags: Some(DEFAULT_LOCAL_VAR_FLAGS),
            });
        }
    }

    Node {
        name: "K2Node_FunctionEntry_0".to_string(),
        guid: Guid::random(),
        pos_x: 0,
        pos_y: 0,
        advanced_pin_display: None,
        node_data: NodeData::FunctionEntry {
            function_reference: FunctionReference {
                member_parent: None,
                member_name: function_name.to_string(),
                member_guid: None,
                self_context: false,
            },
            extra_flags: Some(201457664),
            is_editable: Some(true),
            local_variables,
        },
        pins,
        user_defined_pins,
    }
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
) -> Option<(&'a jmap::Function, &'a str)> {
    match func_ref {
        FunctionRef::ByAddress(addr) => {
            // Look up the object by address
            if let Some(obj_name) = ctx.address_index.object_index.get(&addr.0)
                && let Some(jmap::ObjectType::Function(func)) = ctx.jmap.objects.get(*obj_name)
            {
                return Some((func, *obj_name));
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
                                return Some((func, obj_path.as_str()));
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
fn create_variable_get_node(expr: &Expr, ctx: &mut PasteContext) -> Option<(Node, Guid, PinType)> {
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

            let output_pin_id = Guid::random();

            let node = Node {
                name: node_name,
                guid: Guid::random(),
                pos_x: ctx.variable_node_pos_x(),
                pos_y: ctx.variable_node_pos_y(),
                advanced_pin_display: None,
                node_data: NodeData::VariableGet {
                    variable_reference: paste_buffer::VariableReference {
                        member_parent: None,
                        member_scope: if is_instance_var {
                            None // Instance variables don't have a scope
                        } else {
                            Some(ctx.graph_name().to_string()) // Local variables are scoped to the function
                        },
                        member_name: var_name.to_string(),
                        member_guid: None,
                        self_context: is_instance_var,
                    },
                    self_context_info: None,
                    error_type: None,
                },
                pins: vec![Pin {
                    pin_id: output_pin_id,
                    pin_name: var_name.to_string(),
                    pin_tooltip: None,
                    pin_friendly_name: None,
                    direction: Some(PinDirection::Output),
                    pin_type: pin_type.clone(),
                    default_value: None,
                    autogenerated_default_value: None,
                    default_text_value: None,
                    default_object: None,
                    linked_to: vec![],
                    persistent_guid: Guid::zero(),
                    flags: PinFlags::default(),
                }],
                user_defined_pins: vec![],
            };

            Some((node, output_pin_id, pin_type))
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
fn create_variable_set_node(expr: &Expr, ctx: &mut PasteContext) -> Option<(Node, Guid, PinType)> {
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

            let exec_in_pin = Guid::random();
            let exec_out_pin = Guid::random();
            let value_input_pin = Guid::random();
            let output_get_pin = Guid::random();

            let node = Node {
                name: node_name,
                guid: Guid::random(),
                pos_x: ctx.variable_node_pos_x(),
                pos_y: ctx.variable_node_pos_y(),
                advanced_pin_display: None,
                node_data: NodeData::VariableSet {
                    variable_reference: paste_buffer::VariableReference {
                        member_parent: None,
                        member_scope: if is_instance_var {
                            None // Instance variables don't have a scope
                        } else {
                            Some(ctx.graph_name().to_string()) // Local variables are scoped to the function
                        },
                        member_name: var_name.to_string(),
                        member_guid: None,
                        self_context: is_instance_var,
                    },
                },
                pins: vec![
                    Pin {
                        pin_id: exec_in_pin,
                        pin_name: "execute".to_string(),
                        direction: Some(PinDirection::Input),
                        pin_type: PinType::exec(),
                        ..Default::default()
                    },
                    Pin {
                        pin_id: exec_out_pin,
                        pin_name: "then".to_string(),
                        direction: Some(PinDirection::Output),
                        pin_type: PinType::exec(),
                        ..Default::default()
                    },
                    Pin {
                        pin_id: value_input_pin,
                        pin_name: var_name.to_string(),
                        direction: Some(PinDirection::Input),
                        pin_type: pin_type.clone(),
                        ..Default::default()
                    },
                    Pin {
                        pin_id: output_get_pin,
                        pin_name: "Output_Get".to_string(),
                        pin_tooltip: Some("Retrieves the value of the variable, can use instead of a separate Get node".to_string()),
                        direction: Some(PinDirection::Output),
                        pin_type: pin_type.clone(),
                        ..Default::default()
                    },
                ],
                user_defined_pins: vec![],
            };

            Some((node, value_input_pin, pin_type))
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
    called_func: Option<&jmap::Function>,
    is_pure: bool,
) -> Vec<(String, Guid, bool)> {
    let mut param_info = Vec::new();

    if let Some(func) = called_func {
        for prop in &func.r#struct.properties {
            if prop.flags.contains(jmap::EPropertyFlags::CPF_Parm) {
                let pin_id = Guid::random();
                let pin_type = property_to_pin_type(prop);

                // Return parameters are always outputs
                if prop.flags.contains(jmap::EPropertyFlags::CPF_ReturnParm) {
                    pins.push(Pin {
                        pin_id,
                        pin_name: prop.name.clone(),
                        direction: Some(PinDirection::Output),
                        pin_type,
                        ..Default::default()
                    });
                    param_info.push((prop.name.clone(), pin_id, false));
                } else if prop.flags.contains(jmap::EPropertyFlags::CPF_OutParm) {
                    // Check if this is a const reference (const T& parameter)
                    // These are inputs, not outputs, especially for pure functions
                    let is_const_ref = prop.flags.contains(jmap::EPropertyFlags::CPF_ConstParm);

                    if is_pure && is_const_ref {
                        // Const reference input for pure functions (e.g., const TSet<T>& TargetSet)
                        pins.push(Pin {
                            pin_id,
                            pin_name: prop.name.clone(),
                            direction: Some(PinDirection::Input),
                            pin_type,
                            ..Default::default()
                        });
                        param_info.push((prop.name.clone(), pin_id, true));
                    } else {
                        // True output parameter (T& OutParam)
                        pins.push(Pin {
                            pin_id,
                            pin_name: prop.name.clone(),
                            direction: Some(PinDirection::Output),
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
                        direction: Some(PinDirection::Input),
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

fn property_to_pin_type(prop: &jmap::Property) -> PinType {
    // Convert JMAP property type to Blueprint PinType
    match &prop.r#type {
        jmap::PropertyType::Int => PinType::int(),
        jmap::PropertyType::Int8 | jmap::PropertyType::Byte { r#enum: None } => PinType {
            category: "byte".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Int16 | jmap::PropertyType::UInt16 | jmap::PropertyType::UInt32 => {
            PinType::int()
        }
        jmap::PropertyType::Int64 | jmap::PropertyType::UInt64 => PinType {
            category: "int64".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Float | jmap::PropertyType::Double => PinType::float(),
        jmap::PropertyType::Bool { .. } => PinType::bool(),
        jmap::PropertyType::Str => PinType::string(),
        jmap::PropertyType::Name => PinType {
            category: "name".to_string(),
            ..Default::default()
        },
        jmap::PropertyType::Text => PinType {
            category: "text".to_string(),
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
        jmap::PropertyType::Object { property_class } => PinType::object(property_class),
        jmap::PropertyType::Class {
            property_class,
            meta_class,
        } => PinType {
            category: "class".to_string(),
            sub_category: meta_class.clone(),
            sub_category_object: Some(ExportTextPath::class(property_class)),
            ..Default::default()
        },
        jmap::PropertyType::Interface { interface_class } => PinType {
            category: "interface".to_string(),
            sub_category_object: Some(ExportTextPath::class(interface_class)),
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
            // For maps, the pin type is based on the value type with Map container
            // The key type is stored in sub_category
            let mut value_pin_type = property_to_pin_type(value_prop);
            value_pin_type.container_type = Some("Map".to_string());

            // Set the key type as the sub-category
            let key_pin_type = property_to_pin_type(key_prop);
            value_pin_type.sub_category = key_pin_type.category.clone();

            value_pin_type
        }
        other => panic!(
            "property_to_pin_type: unsupported property type: {:?}",
            other
        ),
    }
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
    let node_name = format!("K2Node_CallFunction_{}", ctx.next_node_id());

    // Try to resolve the function to get parameter information
    let resolved = resolve_function(ctx, func_ref, context_expr);
    let (called_func, func_name, member_parent) = if let Some((f, path)) = resolved {
        // Extract the parent class from the function's outer
        let parent = f.r#struct.object.outer.as_deref();
        (Some(f), get_object_name(path).to_string(), parent)
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
        (None, name, None)
    };

    // Check if the function is actually pure by looking at its flags
    let function_is_pure = called_func
        .map(|f| {
            f.function_flags
                .contains(jmap::EFunctionFlags::FUNC_BlueprintPure)
        })
        .unwrap_or(false);

    // Override is_pure if the function itself is marked as pure
    let is_pure = pure_context || function_is_pure;

    let mut pins = vec![];

    // Only add exec pins if not pure
    let (exec_in_pin, exec_out_pin) = if !is_pure {
        let exec_in = Guid::random();
        let exec_out = Guid::random();
        pins.push(Pin {
            pin_id: exec_in,
            pin_name: "execute".to_string(),
            direction: Some(PinDirection::Input),
            pin_type: PinType::exec(),
            ..Default::default()
        });
        pins.push(Pin {
            pin_id: exec_out,
            pin_name: "then".to_string(),
            direction: Some(PinDirection::Output),
            pin_type: PinType::exec(),
            ..Default::default()
        });
        (Some(exec_in), Some(exec_out))
    } else {
        (None, None)
    };

    // If we have a context expression, add a self/target pin
    if context_expr.is_some() {
        pins.push(Pin {
            pin_id: Guid::random(),
            pin_name: "self".to_string(),
            direction: Some(PinDirection::Input),
            pin_type: PinType::object("/Script/CoreUObject.Object"),
            ..Default::default()
        });
    }

    // Add parameter pins and get their info for connecting
    let param_pins = add_function_parameter_pins(&mut pins, called_func, is_pure);

    let call_node = Node {
        name: node_name.clone(),
        guid: Guid::random(),
        pos_x: ctx.node_pos_x(),
        pos_y: ctx.node_pos_y(),
        node_data: NodeData::CallFunction {
            function_reference: FunctionReference {
                member_parent: member_parent.map(ExportTextPath::class),
                member_name: func_name,
                member_guid: None,
                self_context: context_expr.is_none(),
            },
            is_pure,
            is_const: None,
            is_interface_call: None,
            error_type: None,
        },
        pins,
        ..Default::default()
    };

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
                    } => {
                        // Add the nodes that compute this parameter
                        all_nodes.extend(param_nodes);
                        param_connections.extend(param_conns);

                        // Connect the parameter output to the function's input pin
                        param_connections.push((
                            output_pin,
                            PinConnection {
                                node_name: node_name.clone(),
                                pin_id: *param_pin_id,
                            },
                        ));
                    }
                    DataOutput::FunctionParameter { param_name } => {
                        // This is a direct function parameter reference
                        // Track it for connection to FunctionEntry at the top level
                        data_outputs.push((
                            param_name,
                            PinConnection {
                                node_name: node_name.clone(),
                                pin_id: *param_pin_id,
                            },
                        ));
                    }
                }
            }
        } else if !is_pure {
            // Output parameters only exist for impure functions
            // Try to create VariableSet node
            if let Some((var_set_node, var_input_pin, _pin_type)) =
                create_variable_set_node(param_expr, ctx)
            {
                let var_node_name = var_set_node.name.clone();

                // Get exec pins for chaining
                let var_set_exec_in = var_set_node
                    .pins
                    .iter()
                    .find(|p| p.pin_name == "execute")
                    .map(|p| p.pin_id)
                    .unwrap();
                let var_set_exec_out = var_set_node
                    .pins
                    .iter()
                    .find(|p| p.pin_name == "then")
                    .map(|p| p.pin_id)
                    .unwrap();

                all_nodes.push(var_set_node);
                out_param_set_nodes.push((
                    var_node_name.clone(),
                    var_set_exec_in,
                    var_set_exec_out,
                ));

                // Track connection: call_node param output -> var_set input
                param_connections.push((
                    PinConnection {
                        node_name: node_name.clone(),
                        pin_id: *param_pin_id,
                    },
                    PinConnection {
                        node_name: var_node_name,
                        pin_id: var_input_pin,
                    },
                ));
            }
            // Note: Direct function parameter out params would be handled by FunctionResult node
        }
    }

    // Determine final execution output (only for impure functions)
    // Pure functions don't have exec flow
    let final_exec_output = if !is_pure {
        // If there are out parameter VariableSet nodes, chain them and use the last one's output
        if let Some((first_set_name, first_set_exec_in, _)) = out_param_set_nodes.first() {
            // Connect call_node's exec output to first VariableSet's exec input
            if let Some(exec_out) = exec_out_pin {
                param_connections.push((
                    PinConnection {
                        node_name: node_name.clone(),
                        pin_id: exec_out,
                    },
                    PinConnection {
                        node_name: first_set_name.clone(),
                        pin_id: *first_set_exec_in,
                    },
                ));
            }

            // Chain VariableSet nodes together if there are multiple
            for i in 0..out_param_set_nodes.len() - 1 {
                let (curr_name, _, curr_exec_out) = &out_param_set_nodes[i];
                let (next_name, next_exec_in, _) = &out_param_set_nodes[i + 1];
                param_connections.push((
                    PinConnection {
                        node_name: curr_name.clone(),
                        pin_id: *curr_exec_out,
                    },
                    PinConnection {
                        node_name: next_name.clone(),
                        pin_id: *next_exec_in,
                    },
                ));
            }

            // Final output is the last VariableSet's exec output
            let (last_name, _, last_exec_out) = out_param_set_nodes.last().unwrap();
            Some(PinConnection {
                node_name: last_name.clone(),
                pin_id: *last_exec_out,
            })
        } else {
            // No out params, use the call_node's exec output directly
            exec_out_pin.map(|pin| PinConnection {
                node_name: node_name.clone(),
                pin_id: pin,
            })
        }
    } else {
        None // Pure functions have no exec output
    };

    Some(NodeGraph {
        nodes: all_nodes,
        exec_input: exec_in_pin.map(|pin| PinConnection {
            node_name: node_name.clone(),
            pin_id: pin,
        }),
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
        nodes: Vec<Node>,
        /// The pin that outputs the computed value
        output_pin: PinConnection,
        /// Internal connections within the data expression
        internal_connections: Vec<(PinConnection, PinConnection)>,
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
    /// Convert to a paste_buffer::VariableReference
    fn to_variable_reference(&self) -> paste_buffer::VariableReference {
        paste_buffer::VariableReference {
            member_parent: None,
            member_scope: self.scope.clone(),
            member_name: self.name.clone(),
            member_guid: None,
            self_context: self.is_self_context,
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
            direction: Some(PinDirection::Input),
            pin_type: PinType::exec(),
            ..Default::default()
        }
    }

    pub fn create_exec_output_pin(pin_id: Guid, name: &str) -> Pin {
        Pin {
            pin_id,
            pin_name: name.to_string(),
            direction: Some(PinDirection::Output),
            pin_type: PinType::exec(),
            ..Default::default()
        }
    }

    pub fn create_data_input_pin(pin_id: Guid, name: String, pin_type: PinType) -> Pin {
        Pin {
            pin_id,
            pin_name: name,
            direction: Some(PinDirection::Input),
            pin_type,
            ..Default::default()
        }
    }

    pub fn create_data_output_pin(pin_id: Guid, name: String, pin_type: PinType) -> Pin {
        Pin {
            pin_id,
            pin_name: name,
            direction: Some(PinDirection::Output),
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
    target_pin: PinConnection,
) -> (
    Vec<Node>,
    Vec<(PinConnection, PinConnection)>,
    Vec<(String, PinConnection)>,
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
        } => {
            // Add the computing nodes and connect the output to the target
            let mut connections = internal_connections;
            connections.push((output_pin, target_pin));
            (nodes, connections, vec![])
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

    let exec_in_pin = Guid::random();
    let condition_pin = Guid::random();
    let then_pin = Guid::random();
    let else_pin = Guid::random();

    // Create VariableGet for condition if needed
    let mut all_nodes = Vec::new();
    let mut internal_connections = Vec::new();

    if let Some((var_get_node, var_output_pin, _pin_type)) =
        create_variable_get_node(condition_expr, ctx)
    {
        let var_node_name = var_get_node.name.clone();
        all_nodes.push(var_get_node);

        // Connect condition variable to the Condition pin
        internal_connections.push((
            PinConnection {
                node_name: var_node_name,
                pin_id: var_output_pin,
            },
            PinConnection {
                node_name: node_name.clone(),
                pin_id: condition_pin,
            },
        ));
    }

    let if_node = Node {
        name: node_name.clone(),
        guid: Guid::random(),
        pos_x: ctx.node_pos_x(),
        pos_y: ctx.node_pos_y(),
        advanced_pin_display: None,
        node_data: NodeData::IfThenElse,
        pins: vec![
            pin_helpers::create_exec_input_pin(exec_in_pin),
            Pin {
                pin_id: condition_pin,
                pin_name: "Condition".to_string(),
                direction: Some(PinDirection::Input),
                pin_type: PinType::bool(),
                default_value: Some("true".to_string()),
                autogenerated_default_value: Some("true".to_string()),
                ..Default::default()
            },
            Pin {
                pin_id: then_pin,
                pin_name: "then".to_string(),
                pin_friendly_name: Some("true".to_string()),
                direction: Some(PinDirection::Output),
                pin_type: PinType::exec(),
                ..Default::default()
            },
            Pin {
                pin_id: else_pin,
                pin_name: "else".to_string(),
                pin_friendly_name: Some("false".to_string()),
                direction: Some(PinDirection::Output),
                pin_type: PinType::exec(),
                ..Default::default()
            },
        ],
        user_defined_pins: vec![],
    };

    all_nodes.push(if_node);

    Some(NodeGraph {
        nodes: all_nodes,
        exec_input: Some(PinConnection {
            node_name: node_name.clone(),
            pin_id: exec_in_pin,
        }),
        exec_outputs: vec![
            (
                PinConnection {
                    node_name: node_name.clone(),
                    pin_id: then_pin,
                },
                then_target,
            ),
            (
                PinConnection {
                    node_name,
                    pin_id: else_pin,
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
                Some(DataOutput::Computed {
                    nodes: vec![var_get_node.clone()],
                    output_pin: PinConnection {
                        node_name: var_get_node.name,
                        pin_id: output_pin,
                    },
                    internal_connections: vec![],
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
            let return_pin = call_node
                .pins
                .iter()
                .find(|p| p.pin_name == "ReturnValue" && p.direction == Some(PinDirection::Output))
                .or_else(|| {
                    call_node.pins.iter().find(|p| {
                        p.direction == Some(PinDirection::Output) && p.pin_type.category != "exec"
                    })
                })?;

            // Clone the data we need before moving node_graph
            let node_name = call_node.name.clone();
            let return_pin_id = return_pin.pin_id;

            Some(DataOutput::Computed {
                nodes: node_graph.nodes,
                output_pin: PinConnection {
                    node_name,
                    pin_id: return_pin_id,
                },
                internal_connections: node_graph.internal_connections,
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
                    pin_type: PinType::object("/Script/CoreUObject.Object"),
                }
            });

            // Process the value expression as a data output
            let value_data = expr_to_data_output(value, ctx)?;

            let node_name = format!("K2Node_VariableSet_{}", ctx.next_node_id());

            let exec_in_pin = Guid::random();
            let exec_out_pin = Guid::random();
            let value_input_pin = Guid::random();
            let output_get_pin = Guid::random();

            // Build the value input pin with optional default value
            let value_pin = match &value_data {
                DataOutput::Constant(const_val) => Pin {
                    pin_id: value_input_pin,
                    pin_name: var_info.name.clone(),
                    direction: Some(PinDirection::Input),
                    pin_type: var_info.pin_type.clone(),
                    default_value: Some(const_val.clone()),
                    autogenerated_default_value: Some(const_val.clone()),
                    ..Default::default()
                },
                DataOutput::Computed { .. } | DataOutput::FunctionParameter { .. } => Pin {
                    pin_id: value_input_pin,
                    pin_name: var_info.name.clone(),
                    direction: Some(PinDirection::Input),
                    pin_type: var_info.pin_type.clone(),
                    ..Default::default()
                },
            };

            let var_set_node = Node {
                name: node_name.clone(),
                guid: Guid::random(),
                pos_x: ctx.node_pos_x(),
                pos_y: ctx.node_pos_y(),
                node_data: NodeData::VariableSet {
                    variable_reference: var_info.to_variable_reference(),
                },
                pins: vec![
                    pin_helpers::create_exec_input_pin(exec_in_pin),
                    pin_helpers::create_exec_output_pin(exec_out_pin, "then"),
                    value_pin,
                    Pin {
                        pin_id: output_get_pin,
                        pin_name: "Output_Get".to_string(),
                        pin_tooltip: Some("Retrieves the value of the variable, can use instead of a separate Get node".to_string()),
                        direction: Some(PinDirection::Output),
                        pin_type: var_info.pin_type.clone(),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            };

            // Handle nodes and connections based on whether value is constant, computed, or a function parameter
            let (value_nodes, value_connections, data_outputs_local) = apply_data_output(
                value_data,
                PinConnection {
                    node_name: node_name.clone(),
                    pin_id: value_input_pin,
                },
            );

            let mut all_nodes = value_nodes;
            all_nodes.push(var_set_node);
            let internal_connections = value_connections;

            Some(NodeGraph {
                nodes: all_nodes,
                exec_input: Some(PinConnection {
                    node_name: node_name.clone(),
                    pin_id: exec_in_pin,
                }),
                exec_outputs: vec![(
                    PinConnection {
                        node_name,
                        pin_id: exec_out_pin,
                    },
                    ExecTarget::FallThrough,
                )],
                data_outputs: data_outputs_local,
                internal_connections,
            })
        }

        ExprKind::Return(_return_expr) => {
            let node_name = format!("K2Node_FunctionResult_{}", ctx.next_node_id());

            let exec_in_pin = Guid::random();

            let mut pins = vec![pin_helpers::create_exec_input_pin(exec_in_pin)];

            // Add pins for return parameters and out parameters
            for prop in &ctx.func.1.r#struct.properties {
                if prop.flags.contains(jmap::EPropertyFlags::CPF_Parm)
                    && (prop.flags.contains(jmap::EPropertyFlags::CPF_ReturnParm)
                        || prop.flags.contains(jmap::EPropertyFlags::CPF_OutParm))
                {
                    let pin_type = property_to_pin_type(prop);
                    pins.push(Pin {
                        pin_id: Guid::random(),
                        pin_name: prop.name.clone(),
                        pin_tooltip: None,
                        pin_friendly_name: None,
                        direction: Some(PinDirection::Input),
                        pin_type,
                        default_value: None,
                        autogenerated_default_value: None,
                        default_text_value: None,
                        default_object: None,
                        linked_to: vec![],
                        persistent_guid: Guid::zero(),
                        flags: PinFlags::default(),
                    });
                }
            }

            let node = Node {
                name: node_name.clone(),
                guid: Guid::random(),
                pos_x: ctx.node_pos_x(),
                pos_y: ctx.node_pos_y(),
                node_data: NodeData::FunctionResult {
                    function_reference: FunctionReference {
                        member_parent: None,
                        member_name: "UNKNOWN".to_string(),
                        member_guid: None,
                        self_context: false,
                    },
                    is_editable: Some(true),
                },
                pins,
                ..Default::default()
            };

            Some(NodeGraph {
                nodes: vec![node],
                exec_input: Some(PinConnection {
                    node_name,
                    pin_id: exec_in_pin,
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
                        category: "string".to_string(),
                        container_type: Some("Array".to_string()),
                        ..Default::default()
                    },
                }
            });

            // Create MakeArray node
            let make_array_name = format!("K2Node_MakeArray_{}", ctx.next_node_id());
            let array_output_pin = Guid::random();

            // Get element type (without container)
            let element_pin_type = PinType {
                category: var_info.pin_type.category.clone(),
                sub_category: var_info.pin_type.sub_category.clone(),
                sub_category_object: var_info.pin_type.sub_category_object.clone(),
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

            // Process each element
            for (idx, element_expr) in elements.iter().enumerate() {
                let element_pin_id = Guid::random();
                let mut element_pin = Pin {
                    pin_id: element_pin_id,
                    pin_name: format!("[{}]", idx),
                    direction: Some(PinDirection::Input),
                    pin_type: element_pin_type.clone(),
                    ..Default::default()
                };

                // Process element as data output
                if let Some(element_data) = expr_to_data_output(element_expr, ctx) {
                    match element_data {
                        DataOutput::Constant(const_val) => {
                            element_pin.default_value = Some(const_val.clone());
                            element_pin.autogenerated_default_value = Some(const_val);
                        }
                        DataOutput::Computed {
                            nodes,
                            output_pin,
                            internal_connections: elem_conns,
                        } => {
                            all_nodes.extend(nodes);
                            internal_connections.extend(elem_conns);
                            internal_connections.push((
                                output_pin,
                                PinConnection {
                                    node_name: make_array_name.clone(),
                                    pin_id: element_pin_id,
                                },
                            ));
                        }
                        DataOutput::FunctionParameter { .. } => {
                            // Leave unconnected for now
                        }
                    }
                }

                make_array_pins.push(element_pin);
            }

            let make_array_node = Node {
                name: make_array_name.clone(),
                guid: Guid::random(),
                pos_x: ctx.node_pos_x(),
                pos_y: ctx.node_pos_y(),
                node_data: NodeData::MakeArray {
                    num_inputs: Some(elements.len() as i32),
                },
                pins: make_array_pins,
                ..Default::default()
            };

            all_nodes.push(make_array_node);

            // Create VariableSet node
            let var_set_name = format!("K2Node_VariableSet_{}", ctx.next_node_id());
            let exec_in_pin = Guid::random();
            let exec_out_pin = Guid::random();
            let value_input_pin = Guid::random();
            let output_get_pin = Guid::random();

            let var_set_node = Node {
                name: var_set_name.clone(),
                guid: Guid::random(),
                pos_x: ctx.node_pos_x() + VARIABLE_SET_NODE_OFFSET_X,
                pos_y: ctx.node_pos_y(),
                node_data: NodeData::VariableSet {
                    variable_reference: var_info.to_variable_reference(),
                },
                pins: vec![
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
                        pin_tooltip: Some(
                            "Retrieves the value of the variable, can use instead of a separate Get node"
                                .to_string(),
                        ),
                        direction: Some(PinDirection::Output),
                        pin_type: var_info.pin_type.clone(),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            };

            all_nodes.push(var_set_node);

            // Connect MakeArray output to VariableSet input
            internal_connections.push((
                PinConnection {
                    node_name: make_array_name,
                    pin_id: array_output_pin,
                },
                PinConnection {
                    node_name: var_set_name.clone(),
                    pin_id: value_input_pin,
                },
            ));

            Some(NodeGraph {
                nodes: all_nodes,
                exec_input: Some(PinConnection {
                    node_name: var_set_name.clone(),
                    pin_id: exec_in_pin,
                }),
                exec_outputs: vec![(
                    PinConnection {
                        node_name: var_set_name,
                        pin_id: exec_out_pin,
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
