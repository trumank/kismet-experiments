use std::collections::HashSet;

use crate::{
    bytecode::{
        address_index::AddressIndex,
        expr::{Expr, ExprKind, TextLiteral},
        refs::{ClassRef, FunctionRef, ObjectRef, PropertyRef, StructRef},
        types::BytecodeOffset,
    },
    formatters::theme::Theme,
};

pub struct AsmFormatter<'a> {
    indent_level: usize,
    address_index: &'a AddressIndex<'a>,
    referenced_offsets: HashSet<BytecodeOffset>,
}

impl<'a> AsmFormatter<'a> {
    pub fn new(
        address_index: &'a AddressIndex<'a>,
        referenced_offsets: HashSet<BytecodeOffset>,
    ) -> Self {
        Self {
            indent_level: 0,
            address_index,
            referenced_offsets,
        }
    }

    fn resolve_property(&self, prop: &PropertyRef) -> String {
        let prop_info = self.address_index.resolve_property(prop.address).unwrap();
        format!("{}::{}", prop_info.owner.path, prop_info.property.name)
    }

    fn resolve_class(&self, class: &ClassRef) -> String {
        self.address_index
            .resolve_object(class.address)
            .unwrap()
            .path
            .to_string()
    }

    fn resolve_struct(&self, s: &StructRef) -> String {
        self.address_index
            .resolve_object(s.address)
            .unwrap()
            .path
            .to_string()
    }

    fn resolve_object(&self, obj: &ObjectRef) -> String {
        self.address_index
            .resolve_object(obj.address)
            .unwrap()
            .path
            .to_string()
    }

    fn resolve_function(&self, func: &FunctionRef) -> String {
        match func {
            FunctionRef::ByName(n) => n.as_str().to_string(),
            FunctionRef::ByAddress(addr) => self
                .address_index
                .resolve_object(*addr)
                .unwrap()
                .path
                .to_string(),
        }
    }

    pub fn format(&mut self, expressions: &[Expr]) {
        for expr in expressions {
            // Only print label if this offset is referenced
            if self.referenced_offsets.contains(&expr.offset) {
                println!("{}:", self.print_label(expr.offset));
            }
            self.format_expr(expr);
        }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }

    fn add_indent(&mut self) {
        self.indent_level += 1;
    }

    fn drop_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    fn format_label(&self, offset: BytecodeOffset) -> String {
        Theme::label(format!("Label_0x{:X}", offset.0)).to_string()
    }

    fn print_label(&self, offset: BytecodeOffset) -> String {
        self.format_label(offset)
    }

    fn print_tag(&self, label: &str) {
        println!("{}   {}:", self.indent(), Theme::tag(label));
    }

    fn format_tagged_expr(&mut self, label: &str, expr: &Expr) {
        self.print_tag(label);
        self.format_expr(expr);
    }

    fn format_params(&mut self, params: &[Expr]) {
        if !params.is_empty() {
            for param in params {
                self.format_expr(param);
            }
        }
    }

    fn print_operation(&self, opcode: u8, expr: &Expr, description: impl std::fmt::Display) {
        println!(
            "{} {} [{}] {}",
            self.indent(),
            Theme::opcode(format!("${:02X}:", opcode)),
            Theme::offset(format!("0x{:04X}", expr.offset.0)),
            description
        );
    }

    fn format_expr(&mut self, expr: &Expr) {
        self.add_indent();

        match &expr.kind {
            // Variables
            ExprKind::LocalVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x00,
                    expr,
                    format!("Local variable {}", Theme::variable(name)),
                );
            }
            ExprKind::InstanceVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x01,
                    expr,
                    format!("Instance variable {}", Theme::variable(name)),
                );
            }
            ExprKind::DefaultVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x02,
                    expr,
                    format!("Default variable {}", Theme::variable(name)),
                );
            }
            ExprKind::LocalOutVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x48,
                    expr,
                    format!("Local out variable {}", Theme::variable(name)),
                );
            }
            ExprKind::ClassSparseDataVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x6C,
                    expr,
                    format!("Class sparse data variable {}", Theme::variable(name)),
                );
            }

            // Integer constants
            ExprKind::IntConst(val) => {
                self.print_operation(
                    0x1D,
                    expr,
                    format!("literal int32 {}", Theme::numeric_bold(val)),
                );
            }
            ExprKind::Int64Const(val) => {
                self.print_operation(
                    0x35,
                    expr,
                    format!(
                        "literal int64 {}",
                        Theme::numeric_bold(format!("0x{:X}", val))
                    ),
                );
            }
            ExprKind::UInt64Const(val) => {
                self.print_operation(
                    0x36,
                    expr,
                    format!(
                        "literal uint64 {}",
                        Theme::numeric_bold(format!("0x{:X}", val))
                    ),
                );
            }
            ExprKind::IntZero => {
                self.print_operation(0x25, expr, "EX_IntZero");
            }
            ExprKind::IntOne => {
                self.print_operation(0x26, expr, "EX_IntOne");
            }
            ExprKind::ByteConst(val) => {
                self.print_operation(0x24, expr, format!("literal byte {}", val));
            }
            ExprKind::IntConstByte(val) => {
                self.print_operation(0x2C, expr, format!("literal int {}", val));
            }

            // Float constants
            ExprKind::FloatConst(val) => {
                self.print_operation(
                    0x1E,
                    expr,
                    format!("literal float {}", Theme::numeric_bold(val)),
                );
            }

            // String constants
            ExprKind::StringConst(val) => {
                self.print_operation(
                    0x1F,
                    expr,
                    format!(
                        "literal ansi string {}",
                        crate::formatters::theme::quoted_string(val)
                    ),
                );
            }
            ExprKind::UnicodeStringConst(val) => {
                self.print_operation(
                    0x34,
                    expr,
                    format!(
                        "literal unicode string {}",
                        crate::formatters::theme::quoted_string(val)
                    ),
                );
            }
            ExprKind::NameConst(name) => {
                self.print_operation(0x21, expr, format!("literal name {}", name.as_str()));
            }

            // Vector/rotation/transform
            ExprKind::VectorConst { x, y, z } => {
                self.print_operation(0x23, expr, format!("literal vector ({}, {}, {})", x, y, z));
            }
            ExprKind::RotationConst { pitch, yaw, roll } => {
                self.print_operation(
                    0x22,
                    expr,
                    format!("literal rotation ({}, {}, {})", pitch, yaw, roll),
                );
            }
            ExprKind::TransformConst {
                rot_x,
                rot_y,
                rot_z,
                rot_w,
                trans_x,
                trans_y,
                trans_z,
                scale_x,
                scale_y,
                scale_z,
            } => {
                self.print_operation(
                    0x2B,
                    expr,
                    format!(
                        "literal transform R({},{},{},{}) T({},{},{}) S({},{},{})",
                        rot_x,
                        rot_y,
                        rot_z,
                        rot_w,
                        trans_x,
                        trans_y,
                        trans_z,
                        scale_x,
                        scale_y,
                        scale_z
                    ),
                );
            }

            // Special constants
            ExprKind::True => {
                self.print_operation(0x27, expr, "EX_True");
            }
            ExprKind::False => {
                self.print_operation(0x28, expr, "EX_False");
            }
            ExprKind::NoObject => {
                self.print_operation(0x2A, expr, "EX_NoObject");
            }
            ExprKind::NoInterface => {
                self.print_operation(0x2D, expr, "EX_NoInterface");
            }
            ExprKind::Self_ => {
                self.print_operation(0x17, expr, "EX_Self");
            }
            ExprKind::Nothing => {
                self.print_operation(0x0B, expr, "EX_Nothing");
            }
            ExprKind::NothingInt32 => {
                self.print_operation(0x0C, expr, "EX_NothingInt32");
            }

            // Object references
            ExprKind::ObjectConst(obj) => {
                let name = self.resolve_object(obj);
                self.print_operation(
                    0x20,
                    expr,
                    format!("EX_ObjectConst {}", Theme::object_ref(name)),
                );
            }
            ExprKind::PropertyConst(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x33,
                    expr,
                    format!("EX_PropertyConst {}", Theme::variable(name)),
                );
            }
            ExprKind::SkipOffsetConst(val) => {
                self.print_operation(
                    0x5B,
                    expr,
                    format!("literal CodeSkipSizeType -> {}", self.print_label(*val)),
                );
            }

            // Text constants
            ExprKind::TextConst(text_lit) => match text_lit {
                TextLiteral::Empty => {
                    self.print_operation(0x29, expr, "literal text - empty");
                }
                TextLiteral::LocalizedText {
                    source,
                    key,
                    namespace,
                } => {
                    self.print_operation(0x29, expr, "literal text - localized text");
                    self.format_tagged_expr("Source string", source);
                    self.format_tagged_expr("Key string", key);
                    self.format_tagged_expr("Namespace string", namespace);
                }
                TextLiteral::InvariantText { source } => {
                    self.print_operation(0x29, expr, "literal text - invariant text");
                    self.format_tagged_expr("Source string", source);
                }
                TextLiteral::LiteralString { source } => {
                    self.print_operation(0x29, expr, "literal text - literal string");
                    self.format_tagged_expr("Source string", source);
                }
                TextLiteral::StringTableEntry { table_id, key } => {
                    self.print_operation(0x29, expr, "literal text - string table entry");
                    self.format_tagged_expr("Table ID string", table_id);
                    self.format_tagged_expr("Key string", key);
                }
            },

            // Function calls
            ExprKind::VirtualFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(
                    0x1B,
                    expr,
                    format!("Virtual Function named {}", Theme::function(name)),
                );
                self.format_params(params);
            }
            ExprKind::FinalFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(
                    0x1C,
                    expr,
                    format!("Final Function {}", Theme::function(name)),
                );
                self.format_params(params);
            }
            ExprKind::LocalVirtualFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(
                    0x45,
                    expr,
                    format!(
                        "Local Virtual Script Function named {}",
                        Theme::function(name)
                    ),
                );
                self.format_params(params);
            }
            ExprKind::LocalFinalFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(
                    0x46,
                    expr,
                    format!("Local Final Script Function {}", Theme::function(name)),
                );
                self.format_params(params);
            }
            ExprKind::CallMath { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(0x68, expr, format!("Call Math {}", Theme::function(name)));
                self.format_params(params);
            }
            ExprKind::CallMulticastDelegate {
                stack_node,
                delegate_expr,
                params,
            } => {
                let name = self.resolve_function(stack_node);
                self.print_operation(
                    0x63,
                    expr,
                    format!("CallMulticastDelegate {}", Theme::function(name)),
                );
                self.format_tagged_expr("Delegate", delegate_expr);
                if !params.is_empty() {
                    self.print_tag("Params");
                    self.format_params(params);
                }
            }

            // Context/member access
            ExprKind::Context {
                object,
                field,
                context,
                skip_offset,
                fail_silent,
            } => {
                let opcode = if *fail_silent { 0x1A } else { 0x19 };
                let desc = if *fail_silent {
                    "Context (can fail silently on access none)"
                } else {
                    "Context"
                };
                self.print_operation(opcode, expr, desc);
                let field_str = field
                    .as_ref()
                    .map(|f| self.resolve_property(f))
                    .unwrap_or_else(|| "<none>".to_string());
                println!(
                    "{}   Skip: {} | Field: {}",
                    self.indent(),
                    Theme::offset(format!("0x{:X}", skip_offset)),
                    Theme::variable(field_str)
                );
                self.format_tagged_expr("Object", object);
                self.format_tagged_expr("Context", context);
            }
            ExprKind::ClassContext {
                object,
                field,
                context,
                skip_offset,
            } => {
                self.print_operation(0x12, expr, "Class Context");
                let field_str = field
                    .as_ref()
                    .map(|f| self.resolve_property(f))
                    .unwrap_or_else(|| "<none>".to_string());
                println!(
                    "{}   Skip: {} | Field: {}",
                    self.indent(),
                    Theme::offset(format!("0x{:X}", skip_offset)),
                    Theme::variable(field_str)
                );
                self.format_tagged_expr("Object", object);
                self.format_tagged_expr("Context", context);
            }
            ExprKind::StructMemberContext {
                struct_expr,
                member,
            } => {
                self.print_operation(
                    0x42,
                    expr,
                    format!(
                        "Struct member context - {}",
                        Theme::variable(self.resolve_property(member))
                    ),
                );
                self.format_tagged_expr("Struct", struct_expr);
            }
            ExprKind::InterfaceContext(inner_expr) => {
                self.print_operation(0x51, expr, "EX_InterfaceContext");
                self.format_expr(inner_expr);
            }

            // Casts
            ExprKind::DynamicCast {
                target_class,
                expr: cast_expr,
            } => {
                self.print_operation(
                    0x2E,
                    expr,
                    format!(
                        "DynamicCast to {}",
                        Theme::type_name(self.resolve_class(target_class))
                    ),
                );
                self.format_expr(cast_expr);
            }
            ExprKind::MetaCast {
                target_class,
                expr: cast_expr,
            } => {
                self.print_operation(
                    0x13,
                    expr,
                    format!(
                        "MetaCast to {}",
                        Theme::type_name(self.resolve_class(target_class))
                    ),
                );
                self.format_expr(cast_expr);
            }
            ExprKind::PrimitiveCast {
                conversion_type,
                expr: cast_expr,
            } => {
                self.print_operation(
                    0x38,
                    expr,
                    format!("PrimitiveCast of type {}", Theme::numeric(conversion_type)),
                );
                self.format_tagged_expr("Argument", cast_expr);
            }
            ExprKind::ObjToInterfaceCast {
                target_interface,
                expr: cast_expr,
            } => {
                self.print_operation(
                    0x52,
                    expr,
                    format!(
                        "ObjToInterfaceCast to {}",
                        Theme::type_name(self.resolve_class(target_interface))
                    ),
                );
                self.format_expr(cast_expr);
            }
            ExprKind::InterfaceToObjCast {
                target_class,
                expr: cast_expr,
            } => {
                self.print_operation(
                    0x55,
                    expr,
                    format!(
                        "InterfaceToObjCast to {}",
                        Theme::type_name(self.resolve_class(target_class))
                    ),
                );
                self.format_expr(cast_expr);
            }
            ExprKind::CrossInterfaceCast {
                target_interface,
                expr: cast_expr,
            } => {
                self.print_operation(
                    0x54,
                    expr,
                    format!(
                        "InterfaceToInterfaceCast to {}",
                        Theme::type_name(self.resolve_class(target_interface))
                    ),
                );
                self.format_expr(cast_expr);
            }

            // Collections
            ExprKind::ArrayConst {
                element_type,
                num_elements,
                elements,
            } => {
                self.print_operation(
                    0x65,
                    expr,
                    format!(
                        "array const<{}> - elements number: {}",
                        Theme::variable(self.resolve_property(element_type)),
                        Theme::numeric_bold(num_elements)
                    ),
                );
                self.format_params(elements);
            }
            ExprKind::StructConst {
                struct_type,
                serialized_size,
                elements,
            } => {
                self.print_operation(
                    0x2F,
                    expr,
                    format!(
                        "literal struct {} (serialized size: {})",
                        Theme::type_name(self.resolve_struct(struct_type)),
                        serialized_size
                    ),
                );
                self.format_params(elements);
            }
            ExprKind::SetConst {
                element_type,
                num_elements,
                elements,
            } => {
                self.print_operation(
                    0x3D,
                    expr,
                    format!(
                        "set const<{}> - elements number: {}",
                        Theme::variable(self.resolve_property(element_type)),
                        Theme::numeric_bold(num_elements)
                    ),
                );
                self.format_params(elements);
            }
            ExprKind::MapConst {
                key_type,
                value_type,
                num_elements,
                elements,
            } => {
                self.print_operation(
                    0x3F,
                    expr,
                    format!(
                        "map const<{}, {}> - elements number: {}",
                        Theme::variable(self.resolve_property(key_type)),
                        Theme::variable(self.resolve_property(value_type)),
                        Theme::numeric_bold(num_elements)
                    ),
                );
                self.format_params(elements);
            }

            // Array/set/map operations
            ExprKind::SetArray {
                array_expr,
                elements,
            } => {
                self.print_operation(0x31, expr, "set array");
                self.format_tagged_expr("Array", array_expr);
                if !elements.is_empty() {
                    self.print_tag("Elements");
                    self.format_params(elements);
                }
            }
            ExprKind::SetSet {
                set_expr,
                num: _,
                elements,
            } => {
                self.print_operation(0x39, expr, "set set");
                self.format_tagged_expr("Set", set_expr);
                if !elements.is_empty() {
                    self.print_tag("Elements");
                    self.format_params(elements);
                }
            }
            ExprKind::SetMap {
                map_expr,
                num: _,
                elements,
            } => {
                self.print_operation(0x3B, expr, "set map");
                self.format_tagged_expr("Map", map_expr);
                if !elements.is_empty() {
                    self.print_tag("Elements");
                    self.format_params(elements);
                }
            }
            ExprKind::ArrayGetByRef {
                array_expr,
                index_expr,
            } => {
                self.print_operation(0x6B, expr, "Array Get-by-Ref Index");
                self.format_tagged_expr("Array", array_expr);
                self.format_tagged_expr("Index", index_expr);
            }

            // Assignments
            ExprKind::Let {
                property,
                variable,
                value,
            } => {
                let property_str = property
                    .as_ref()
                    .map(|p| self.resolve_property(p))
                    .unwrap_or_else(|| "<none>".to_string());
                self.print_operation(
                    0x0F,
                    expr,
                    format!(
                        "Let (Variable = Expression) - {}",
                        Theme::variable(property_str)
                    ),
                );
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetObj { variable, value } => {
                self.print_operation(0x5F, expr, "Let Obj (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetWeakObjPtr { variable, value } => {
                self.print_operation(0x60, expr, "Let WeakObjPtr (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetBool { variable, value } => {
                self.print_operation(0x14, expr, "LetBool (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetDelegate { variable, value } => {
                self.print_operation(0x44, expr, "LetDelegate (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetMulticastDelegate { variable, value } => {
                self.print_operation(0x43, expr, "LetMulticastDelegate (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetValueOnPersistentFrame { property, value } => {
                self.print_operation(
                    0x64,
                    expr,
                    format!(
                        "LetValueOnPersistentFrame - {}",
                        Theme::variable(self.resolve_property(property))
                    ),
                );
                self.format_tagged_expr("Expression", value);
            }

            // Delegates
            ExprKind::InstanceDelegate(name) => {
                self.print_operation(
                    0x4B,
                    expr,
                    format!("instance delegate function named {}", name.as_str()),
                );
            }
            ExprKind::BindDelegate {
                func_name,
                delegate_expr,
                object_expr,
            } => {
                self.print_operation(0x61, expr, format!("BindDelegate '{}'", func_name.as_str()));
                self.format_tagged_expr("Delegate", delegate_expr);
                self.format_tagged_expr("Object", object_expr);
            }
            ExprKind::AddMulticastDelegate {
                delegate_expr,
                to_add_expr,
            } => {
                self.print_operation(0x5C, expr, "Add MC delegate");
                self.format_tagged_expr("Delegate", delegate_expr);
                self.format_tagged_expr("To Add", to_add_expr);
            }
            ExprKind::RemoveMulticastDelegate {
                delegate_expr,
                to_remove_expr,
            } => {
                self.print_operation(0x62, expr, "Remove MC delegate");
                self.format_tagged_expr("Delegate", delegate_expr);
                self.format_tagged_expr("To Remove", to_remove_expr);
            }
            ExprKind::ClearMulticastDelegate(clear_expr) => {
                self.print_operation(0x5D, expr, "Clear MC delegate");
                self.format_expr(clear_expr);
            }

            // Control flow
            ExprKind::Return(ret_expr) => {
                self.print_operation(0x04, expr, "Return expression");
                self.format_expr(ret_expr);
            }
            ExprKind::Jump { target } => {
                self.print_operation(
                    0x06,
                    expr,
                    format!("Jump to offset {}", self.print_label(*target)),
                );
            }
            ExprKind::JumpIfNot { condition, target } => {
                self.print_operation(
                    0x07,
                    expr,
                    format!("Jump to {} if not:", self.print_label(*target)),
                );
                self.format_expr(condition);
            }
            ExprKind::ComputedJump { offset_expr } => {
                self.print_operation(0x4E, expr, "Computed Jump, offset specified by expression:");
                self.format_expr(offset_expr);
            }
            ExprKind::SwitchValue {
                index,
                cases,
                default,
                end_offset,
            } => {
                self.print_operation(
                    0x69,
                    expr,
                    format!(
                        "Switch Value {} cases, end in {}",
                        cases.len(),
                        self.print_label(*end_offset)
                    ),
                );
                self.format_tagged_expr("Index", index);

                for (i, case) in cases.iter().enumerate() {
                    self.print_tag(&format!(
                        "Case [{}] ({})",
                        i,
                        self.print_label(case.case_offset)
                    ));
                    self.format_tagged_expr("Match Value", &case.case_value);
                    println!(
                        "{}   Next case offset: {}",
                        self.indent(),
                        Theme::offset(format!("0x{:X}", case.next_offset.as_usize()))
                    );
                    self.format_tagged_expr("Result", &case.result);
                }

                self.format_tagged_expr("Default result", default);
            }

            // Execution flow
            ExprKind::PushExecutionFlow { push_offset } => {
                self.print_operation(
                    0x4C,
                    expr,
                    format!("FlowStack.Push({})", self.print_label(*push_offset)),
                );
            }
            ExprKind::PopExecutionFlow => {
                self.print_operation(
                    0x4D,
                    expr,
                    "if (FlowStack.Num()) { jump to FlowStack.Pop(); } else { ERROR!!! }",
                );
            }
            ExprKind::PopExecutionFlowIfNot { condition } => {
                self.print_operation(
                    0x4F,
                    expr,
                    "if (!condition) { if (FlowStack.Num()) { jump to FlowStack.Pop(); } else { ERROR!!! } }"
                );
                self.format_expr(condition);
            }

            // Debug/instrumentation
            ExprKind::Assert {
                line,
                in_debug,
                condition,
            } => {
                self.print_operation(
                    0x09,
                    expr,
                    format!("assert at line {}, in debug mode = {}", line, in_debug),
                );
                self.format_expr(condition);
            }
            ExprKind::Skip {
                skip_count,
                expr: skip_expr,
            } => {
                self.print_operation(
                    0x18,
                    expr,
                    format!(
                        "possibly skip {} bytes of expr:",
                        Theme::offset(format!("0x{:X}", skip_count))
                    ),
                );
                self.format_expr(skip_expr);
            }
            ExprKind::Breakpoint => {
                self.print_operation(0x50, expr, "<<< BREAKPOINT >>>");
            }
            ExprKind::Tracepoint => {
                self.print_operation(0x5E, expr, ".. debug site ..");
            }
            ExprKind::WireTracepoint => {
                self.print_operation(0x5A, expr, ".. wire debug site ..");
            }
            ExprKind::InstrumentationEvent { event_type } => {
                self.print_operation(
                    0x6A,
                    expr,
                    format!(".. instrumented event type {} ..", event_type),
                );
            }

            // Special
            ExprKind::BitFieldConst => {
                self.print_operation(0x11, expr, "EX_BitFieldConst (unimplemented)");
            }
            ExprKind::DeprecatedOp4A => {
                self.print_operation(0x4A, expr, "This opcode has been removed and does nothing.");
            }
            ExprKind::EndOfScript => {
                self.print_operation(0x53, expr, "EX_EndOfScript");
            }
            ExprKind::EndParmValue => {
                self.print_operation(0x15, expr, "EX_EndParmValue");
            }

            ExprKind::SoftObjectConst(soft_expr) => {
                self.print_operation(0x67, expr, "EX_SoftObjectConst");
                self.format_expr(soft_expr);
            }
            ExprKind::FieldPathConst(field_expr) => {
                self.print_operation(0x6D, expr, "EX_FieldPathConst");
                self.format_expr(field_expr);
            }
        }

        self.drop_indent();
    }
}
