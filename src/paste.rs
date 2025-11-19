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

/// Context passed around during node graph generation
struct PasteContext<'a> {
    /// The function being decompiled
    func: &'a jmap::Function,
    /// JMAP data for looking up function definitions
    jmap: &'a jmap::Jmap,
    /// Address index for resolving addresses to objects
    address_index: &'a AddressIndex<'a>,
    /// The name of the blueprint graph (used for variable scope)
    graph_name: &'a str,
    /// Counter for generating unique node names
    node_counter: i32,
}

impl<'a> PasteContext<'a> {
    fn new(
        func: &'a jmap::Function,
        jmap: &'a jmap::Jmap,
        address_index: &'a AddressIndex<'a>,
        graph_name: &'a str,
    ) -> Self {
        Self {
            func,
            jmap,
            address_index,
            graph_name,
            node_counter: 0,
        }
    }

    fn next_node_id(&mut self) -> i32 {
        let id = self.node_counter;
        self.node_counter += 1;
        id
    }
}

/// Represents a compiled node graph for an expression
/// An expression may compile to multiple nodes (e.g., function call + variable gets for params)
#[derive(Debug, Clone)]
struct NodeGraph {
    /// All nodes generated for this expression
    nodes: Vec<Node>,
    /// The exec input pin (if this graph has execution flow)
    exec_input: Option<PinConnection>,
    /// Exec output pins mapped to their target bytecode offsets
    /// For linear flow, target is the next expression
    /// For branches (if/else), multiple outputs with different targets
    exec_outputs: Vec<(PinConnection, Option<BytecodeOffset>)>,
    /// Data output pins by parameter name (for value expressions)
    data_outputs: Vec<(String, PinConnection)>, // (param_name, pin_connection)
    /// Internal data connections within this graph (from, to)
    internal_connections: Vec<(PinConnection, PinConnection)>,
}

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
    let mut ctx = PasteContext::new(func, jmap, address_index, graph_name);

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
        // Skip if already processed
        if processed_graphs.contains_key(&offset) {
            continue;
        }

        // Get the expression by index
        let Some(&expr_idx) = expr_map.get(&offset) else {
            continue;
        };
        let expr = &expressions[expr_idx];

        // Handle Jump expressions specially - they transparently redirect to target
        if let ExprKind::Jump { target } = &expr.kind {
            // Queue the jump target with the current prev_exec, effectively jumping there
            queue.push_back((*target, prev_exec));
            // Mark this offset as processed to avoid reprocessing
            processed_graphs.insert(
                offset,
                NodeGraph {
                    nodes: vec![],
                    exec_input: None,
                    exec_outputs: vec![],
                    data_outputs: vec![],
                    internal_connections: vec![],
                },
            );
            continue;
        }

        // Convert to node graph
        let Some(node_graph) = expr_to_node(expr, &mut ctx, None) else {
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
        for (out_conn, target_offset) in &node_graph.exec_outputs {
            let target = if let Some(target_off) = target_offset {
                // Explicit jump target
                *target_off
            } else {
                // Fall-through to next expression
                if let Some(next_expr) = expressions.get(expr_idx + 1) {
                    next_expr.offset
                } else {
                    continue; // No next expression
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
                property_flags: Some(5), // Default property flags for local vars
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
    func_ref: &FunctionRef,
    context_expr: Option<&Expr>,
    owning_class: &jmap::Function,
    jmap: &'a jmap::Jmap,
    address_index: &AddressIndex<'a>,
) -> Option<(&'a jmap::Function, &'a str)> {
    match func_ref {
        FunctionRef::ByAddress(addr) => {
            // Look up the object by address
            if let Some(obj_name) = address_index.object_index.get(&addr.0)
                && let Some(jmap::ObjectType::Function(func)) = jmap.objects.get(*obj_name)
            {
                return Some((func, *obj_name));
            }
            None
        }
        FunctionRef::ByName(name) => {
            // Virtual function by name - need to resolve on the target class
            let target_class = if let Some(_ctx) = context_expr {
                // TODO: Extract the type from the context expression
                // For now, we can't resolve this without type information
                return None;
            } else {
                // No context means call on self - use the owning class
                // Get the class that owns this function
                if let Some(parent_class) = &owning_class.r#struct.object.outer {
                    parent_class
                } else {
                    return None;
                }
            };

            // Search for a function with this name in the target class
            let name_str = name.as_str();

            // Look for a function object that belongs to this class
            for (obj_path, obj) in &jmap.objects {
                if let jmap::ObjectType::Function(func) = obj {
                    // Check if this function's outer matches the target class
                    if let Some(outer) = &func.r#struct.object.outer
                        && outer == target_class {
                            // Extract the object name (last component after ':')
                            let obj_name = extract_function_name(obj_path);
                            // Case-insensitive exact match
                            if obj_name.eq_ignore_ascii_case(name_str) {
                                return Some((func, obj_path.as_str()));
                            }
                        }
                }
            }

            None
        }
    }
}

/// Extract just the function name from a full object path
/// E.g., "/Script/Module.Class:FunctionName" -> "FunctionName"
fn extract_function_name(full_path: &str) -> &str {
    full_path
        .rsplit_once(':')
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
        ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) => {
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

/// Create a VariableGet node for a parameter expression
/// Returns the node and the pin that outputs the variable value
/// Returns None if the expression refers to a function parameter (should connect directly to FunctionEntry)
fn create_variable_get_node(expr: &Expr, ctx: &mut PasteContext) -> Option<(Node, Guid, PinType)> {
    match &expr.kind {
        ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) => {
            // Look up the property using AddressIndex
            let prop_info = ctx.address_index.resolve_property(prop_ref.address)?;
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
                pos_x: (ctx.node_counter * 200),
                pos_y: (ctx.node_counter * 80),
                advanced_pin_display: None,
                node_data: NodeData::VariableGet {
                    variable_reference: paste_buffer::VariableReference {
                        member_parent: None,
                        member_scope: if is_instance_var {
                            None // Instance variables don't have a scope
                        } else {
                            Some(ctx.graph_name.to_string()) // Local variables are scoped to the function
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
        _ => None, // TODO: Handle other parameter expression types
    }
}

/// Create a VariableSet node for an out parameter expression
/// Returns the node and the pin that receives the variable value
/// Returns None if the expression refers to a function parameter (should connect directly to FunctionResult)
fn create_variable_set_node(expr: &Expr, ctx: &mut PasteContext) -> Option<(Node, Guid, PinType)> {
    match &expr.kind {
        ExprKind::LocalVariable(prop_ref) | ExprKind::InstanceVariable(prop_ref) => {
            // Look up the property using AddressIndex
            let prop_info = ctx.address_index.resolve_property(prop_ref.address)?;
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
                pos_x: (ctx.node_counter * 200),
                pos_y: (ctx.node_counter * 80),
                advanced_pin_display: None,
                node_data: NodeData::VariableSet {
                    variable_reference: paste_buffer::VariableReference {
                        member_parent: None,
                        member_scope: if is_instance_var {
                            None // Instance variables don't have a scope
                        } else {
                            Some(ctx.graph_name.to_string()) // Local variables are scoped to the function
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
        _ => None,
    }
}

/// Add parameter pins to a function call node based on the function definition
/// Returns (param_name, pin_id, is_input) for each parameter to help with connections
fn add_function_parameter_pins(
    pins: &mut Vec<Pin>,
    called_func: Option<&jmap::Function>,
) -> Vec<(String, Guid, bool)> {
    let mut param_info = Vec::new();

    if let Some(func) = called_func {
        for prop in &func.r#struct.properties {
            if prop.flags.contains(jmap::EPropertyFlags::CPF_Parm) {
                let pin_id = Guid::random();
                let pin_type = property_to_pin_type(prop);

                // Skip return parameters - they're outputs
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
                    // Out parameters are outputs
                    pins.push(Pin {
                        pin_id,
                        pin_name: prop.name.clone(),
                        direction: Some(PinDirection::Output),
                        pin_type,
                        ..Default::default()
                    });
                    param_info.push((prop.name.clone(), pin_id, false));
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
        other => panic!(
            "property_to_pin_type: unsupported property type: {:?}",
            other
        ),
    }
}

/// Helper function to create a function call node graph (used by VirtualFunction, LocalVirtualFunction, FinalFunction)
fn create_function_call_node(
    func_ref: &FunctionRef,
    params: &[Expr],
    ctx: &mut PasteContext,
    context_expr: Option<&Expr>,
) -> Option<NodeGraph> {
    let node_name = format!("K2Node_CallFunction_{}", ctx.next_node_id());

    // Try to resolve the function to get parameter information
    let resolved = resolve_function(
        func_ref,
        context_expr,
        ctx.func,
        ctx.jmap,
        ctx.address_index,
    );
    let (called_func, func_name) = if let Some((f, path)) = resolved {
        (Some(f), extract_function_name(path).to_string())
    } else {
        // Failed to resolve - extract name from the FunctionRef itself
        let name = match func_ref {
            FunctionRef::ByName(name) => name.as_str().to_string(),
            FunctionRef::ByAddress(addr) => format!("UnknownFunc_{:X}", addr.0),
        };
        (None, name)
    };

    let exec_in_pin = Guid::random();
    let exec_out_pin = Guid::random();

    let mut pins = vec![
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
    ];

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
    let param_pins = add_function_parameter_pins(&mut pins, called_func);

    let call_node = Node {
        name: node_name.clone(),
        guid: Guid::random(),
        pos_x: (ctx.node_counter * 300),
        pos_y: 0,
        node_data: NodeData::CallFunction {
            function_reference: FunctionReference {
                member_parent: None,
                member_name: func_name,
                member_guid: None,
                self_context: context_expr.is_none(),
            },
            is_pure: false,
            is_const: None,
            is_interface_call: None,
            error_type: None,
        },
        pins,
        ..Default::default()
    };

    // Process parameters to create VariableGet/VariableSet nodes and connect them
    let mut all_nodes = vec![call_node];
    let mut param_connections = Vec::new();
    let mut out_param_set_nodes = Vec::new(); // Track VariableSet nodes for execution flow
    let mut data_outputs = Vec::new(); // Track (param_name, PinConnection) for FunctionEntry connections

    for (param_expr, (_param_name, param_pin_id, is_input)) in params.iter().zip(param_pins.iter())
    {
        if *is_input {
            // Input parameter: Try to create VariableGet node
            if let Some((var_get_node, var_output_pin, _pin_type)) =
                create_variable_get_node(param_expr, ctx)
            {
                let var_node_name = var_get_node.name.clone();
                all_nodes.push(var_get_node);

                // Track connection: var_get output -> call_node param input
                param_connections.push((
                    PinConnection {
                        node_name: var_node_name,
                        pin_id: var_output_pin,
                    },
                    PinConnection {
                        node_name: node_name.clone(),
                        pin_id: *param_pin_id,
                    },
                ));
            } else if let Some(func_param_name) = get_parameter_name(param_expr, ctx) {
                // This is a direct function parameter reference
                // Track it for connection to FunctionEntry at the top level
                data_outputs.push((
                    func_param_name,
                    PinConnection {
                        node_name: node_name.clone(),
                        pin_id: *param_pin_id,
                    },
                ));
            }
        } else {
            // Output parameter: Try to create VariableSet node
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

    // Determine final execution output
    // If there are out parameter VariableSet nodes, chain them and use the last one's output
    let final_exec_output =
        if let Some((first_set_name, first_set_exec_in, _)) = out_param_set_nodes.first() {
            // Connect call_node's exec output to first VariableSet's exec input
            param_connections.push((
                PinConnection {
                    node_name: node_name.clone(),
                    pin_id: exec_out_pin,
                },
                PinConnection {
                    node_name: first_set_name.clone(),
                    pin_id: *first_set_exec_in,
                },
            ));

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
            Some(PinConnection {
                node_name: node_name.clone(),
                pin_id: exec_out_pin,
            })
        };

    Some(NodeGraph {
        nodes: all_nodes,
        exec_input: Some(PinConnection {
            node_name: node_name.clone(),
            pin_id: exec_in_pin,
        }),
        exec_outputs: if let Some(out_conn) = final_exec_output {
            vec![(out_conn, None)] // None means fall-through to next expression
        } else {
            vec![]
        },
        data_outputs,
        internal_connections: param_connections,
    })
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
    let graph = match &expr.kind {
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
        } => create_function_call_node(func_ref, params, ctx, context_expr),

        ExprKind::Let {
            variable, value: _, ..
        }
        | ExprKind::LetBool { variable, value: _ }
        | ExprKind::LetObj { variable, value: _ }
        | ExprKind::LetWeakObjPtr { variable, value: _ }
        | ExprKind::LetDelegate { variable, value: _ }
        | ExprKind::LetMulticastDelegate { variable, value: _ } => {
            // Extract variable information from the variable expression
            let (var_name, var_scope, is_self_context, pin_type) = match dbg!(&variable.kind) {
                ExprKind::LocalVariable(prop_ref)
                | ExprKind::InstanceVariable(prop_ref)
                | ExprKind::LocalOutVariable(prop_ref) => {
                    if let Some(prop_info) = ctx.address_index.resolve_property(prop_ref.address) {
                        let prop = prop_info.property;
                        let name = prop.name.clone();
                        let is_self = matches!(variable.kind, ExprKind::InstanceVariable(_));
                        let scope = if is_self {
                            None // Instance variables don't have a scope
                        } else if is_function_parameter(prop) {
                            None // Function parameters don't have a scope
                        } else {
                            Some(ctx.graph_name.to_string()) // Local variables are scoped to the function
                        };
                        let ptype = property_to_pin_type(prop);
                        (name, scope, is_self, ptype)
                    } else {
                        (
                            "UNKNOWN_VAR".to_string(),
                            None,
                            true,
                            PinType::object("/Script/CoreUObject.Object"),
                        )
                    }
                }
                _ => (
                    "UNKNOWN_VAR".to_string(),
                    None,
                    true,
                    PinType::object("/Script/CoreUObject.Object"),
                ),
            };

            let node_name = format!("K2Node_VariableSet_{}", ctx.next_node_id());

            let exec_in_pin = Guid::random();
            let exec_out_pin = Guid::random();
            let value_input_pin = Guid::random();
            let output_get_pin = Guid::random();

            let node = Node {
                name: node_name.clone(),
                guid: Guid::random(),
                pos_x: (ctx.node_counter * 300),
                pos_y: 0,
                node_data: NodeData::VariableSet {
                    variable_reference: paste_buffer::VariableReference {
                        member_parent: None,
                        member_scope: var_scope,
                        member_name: var_name.clone(),
                        member_guid: None,
                        self_context: is_self_context,
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
                        pin_name: var_name.clone(),
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
                ..Default::default()
            };

            Some(NodeGraph {
                nodes: vec![node],
                exec_input: Some(PinConnection {
                    node_name: node_name.clone(),
                    pin_id: exec_in_pin,
                }),
                exec_outputs: vec![(
                    PinConnection {
                        node_name,
                        pin_id: exec_out_pin,
                    },
                    None,
                )],
                data_outputs: vec![],
                internal_connections: vec![],
            })
        }

        ExprKind::Return(_return_expr) => {
            let node_name = format!("K2Node_FunctionResult_{}", ctx.next_node_id());

            let exec_in_pin = Guid::random();

            let mut pins = vec![Pin {
                pin_id: exec_in_pin,
                pin_name: "execute".to_string(),
                direction: Some(PinDirection::Input),
                pin_type: PinType::exec(),
                ..Default::default()
            }];

            // Add pins for return parameters and out parameters
            for prop in &ctx.func.r#struct.properties {
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
                pos_x: (ctx.node_counter * 300),
                pos_y: 0,
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
            let node_name = format!("K2Node_IfThenElse_{}", ctx.next_node_id());

            let exec_in_pin = Guid::random();
            let condition_pin = Guid::random();
            let then_pin = Guid::random();
            let else_pin = Guid::random();

            // Create VariableGet for condition if needed
            let mut all_nodes = Vec::new();
            let mut internal_connections = Vec::new();

            if let Some((var_get_node, var_output_pin, _pin_type)) =
                create_variable_get_node(condition, ctx)
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
                pos_x: (ctx.node_counter * 300),
                pos_y: 0,
                advanced_pin_display: None,
                node_data: NodeData::IfThenElse,
                pins: vec![
                    Pin {
                        pin_id: exec_in_pin,
                        pin_name: "execute".to_string(),
                        direction: Some(PinDirection::Input),
                        pin_type: PinType::exec(),
                        ..Default::default()
                    },
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
                    // "then" pin continues to next expression (None means fall-through to next)
                    (
                        PinConnection {
                            node_name: node_name.clone(),
                            pin_id: then_pin,
                        },
                        None,
                    ),
                    // "else" pin jumps to target
                    (
                        PinConnection {
                            node_name,
                            pin_id: else_pin,
                        },
                        Some(*target),
                    ),
                ],
                data_outputs: vec![],
                internal_connections,
            })
        }

        other => {
            panic!("expr_to_node: unsupported expression kind: {:?}", other);
        }
    };
    dbg!(&graph);
    graph
}
