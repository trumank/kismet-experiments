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

    fn print_operation(&self, opcode: u8, description: impl std::fmt::Display) {
        println!(
            "{} {} {}",
            self.indent(),
            Theme::opcode(format!("${:02X}:", opcode)),
            description
        );
    }

    fn format_expr(&mut self, expr: &Expr) {
        self.add_indent();

        match &expr.kind {
            // Variables
            ExprKind::LocalVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(0x00, format!("Local variable {}", Theme::variable(name)));
            }
            ExprKind::InstanceVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(0x01, format!("Instance variable {}", Theme::variable(name)));
            }
            ExprKind::DefaultVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(0x02, format!("Default variable {}", Theme::variable(name)));
            }
            ExprKind::LocalOutVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x48,
                    format!("Local out variable {}", Theme::variable(name)),
                );
            }
            ExprKind::ClassSparseDataVariable(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(
                    0x6C,
                    format!("Class sparse data variable {}", Theme::variable(name)),
                );
            }

            // Integer constants
            ExprKind::IntConst(val) => {
                self.print_operation(0x1D, format!("literal int32 {}", Theme::numeric_bold(val)));
            }
            ExprKind::Int64Const(val) => {
                self.print_operation(
                    0x35,
                    format!(
                        "literal int64 {}",
                        Theme::numeric_bold(format!("0x{:X}", val))
                    ),
                );
            }
            ExprKind::UInt64Const(val) => {
                self.print_operation(
                    0x36,
                    format!(
                        "literal uint64 {}",
                        Theme::numeric_bold(format!("0x{:X}", val))
                    ),
                );
            }
            ExprKind::IntZero => {
                self.print_operation(0x25, "EX_IntZero");
            }
            ExprKind::IntOne => {
                self.print_operation(0x26, "EX_IntOne");
            }
            ExprKind::ByteConst(val) => {
                self.print_operation(0x24, format!("literal byte {}", val));
            }
            ExprKind::IntConstByte(val) => {
                self.print_operation(0x2C, format!("literal int {}", val));
            }

            // Float constants
            ExprKind::FloatConst(val) => {
                self.print_operation(0x1E, format!("literal float {}", Theme::numeric_bold(val)));
            }

            // String constants
            ExprKind::StringConst(val) => {
                self.print_operation(
                    0x1F,
                    format!(
                        "literal ansi string {}",
                        crate::formatters::theme::quoted_string(val)
                    ),
                );
            }
            ExprKind::UnicodeStringConst(val) => {
                self.print_operation(
                    0x34,
                    format!(
                        "literal unicode string {}",
                        crate::formatters::theme::quoted_string(val)
                    ),
                );
            }
            ExprKind::NameConst(name) => {
                self.print_operation(0x21, format!("literal name {}", name.as_str()));
            }

            // Vector/rotation/transform
            ExprKind::VectorConst { x, y, z } => {
                self.print_operation(0x23, format!("literal vector ({}, {}, {})", x, y, z));
            }
            ExprKind::RotationConst { pitch, yaw, roll } => {
                self.print_operation(
                    0x22,
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
                self.print_operation(0x27, "EX_True");
            }
            ExprKind::False => {
                self.print_operation(0x28, "EX_False");
            }
            ExprKind::NoObject => {
                self.print_operation(0x2A, "EX_NoObject");
            }
            ExprKind::NoInterface => {
                self.print_operation(0x2D, "EX_NoInterface");
            }
            ExprKind::Self_ => {
                self.print_operation(0x17, "EX_Self");
            }
            ExprKind::Nothing => {
                self.print_operation(0x0B, "EX_Nothing");
            }
            ExprKind::NothingInt32 => {
                self.print_operation(0x0C, "EX_NothingInt32");
            }

            // Object references
            ExprKind::ObjectConst(obj) => {
                let name = self.resolve_object(obj);
                self.print_operation(0x20, format!("EX_ObjectConst {}", Theme::object_ref(name)));
            }
            ExprKind::PropertyConst(prop) => {
                let name = self.resolve_property(prop);
                self.print_operation(0x33, format!("EX_PropertyConst {}", Theme::variable(name)));
            }
            ExprKind::SkipOffsetConst(val) => {
                self.print_operation(
                    0x5B,
                    format!("literal CodeSkipSizeType -> {}", self.print_label(*val)),
                );
            }

            // Text constants
            ExprKind::TextConst(text_lit) => match text_lit {
                TextLiteral::Empty => {
                    self.print_operation(0x29, "literal text - empty");
                }
                TextLiteral::LocalizedText {
                    source,
                    key,
                    namespace,
                } => {
                    self.print_operation(0x29, "literal text - localized text");
                    self.format_tagged_expr("Source string", source);
                    self.format_tagged_expr("Key string", key);
                    self.format_tagged_expr("Namespace string", namespace);
                }
                TextLiteral::InvariantText { source } => {
                    self.print_operation(0x29, "literal text - invariant text");
                    self.format_tagged_expr("Source string", source);
                }
                TextLiteral::LiteralString { source } => {
                    self.print_operation(0x29, "literal text - literal string");
                    self.format_tagged_expr("Source string", source);
                }
                TextLiteral::StringTableEntry { table_id, key } => {
                    self.print_operation(0x29, "literal text - string table entry");
                    self.format_tagged_expr("Table ID string", table_id);
                    self.format_tagged_expr("Key string", key);
                }
            },

            // Function calls
            ExprKind::VirtualFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(
                    0x1B,
                    format!("Virtual Function named {}", Theme::function(name)),
                );
                self.format_params(params);
            }
            ExprKind::FinalFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(0x1C, format!("Final Function {}", Theme::function(name)));
                self.format_params(params);
            }
            ExprKind::LocalVirtualFunction { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(
                    0x45,
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
                    format!("Local Final Script Function {}", Theme::function(name)),
                );
                self.format_params(params);
            }
            ExprKind::CallMath { func, params } => {
                let name = self.resolve_function(func);
                self.print_operation(0x68, format!("Call Math {}", Theme::function(name)));
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
                self.print_operation(opcode, desc);
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
                self.print_operation(0x12, "Class Context");
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
                    format!(
                        "Struct member context - {}",
                        Theme::variable(self.resolve_property(member))
                    ),
                );
                self.format_tagged_expr("Struct", struct_expr);
            }
            ExprKind::InterfaceContext(expr) => {
                self.print_operation(0x51, "EX_InterfaceContext");
                self.format_expr(expr);
            }

            // Casts
            ExprKind::DynamicCast { target_class, expr } => {
                self.print_operation(
                    0x2E,
                    format!(
                        "DynamicCast to {}",
                        Theme::type_name(self.resolve_class(target_class))
                    ),
                );
                self.format_expr(expr);
            }
            ExprKind::MetaCast { target_class, expr } => {
                self.print_operation(
                    0x13,
                    format!(
                        "MetaCast to {}",
                        Theme::type_name(self.resolve_class(target_class))
                    ),
                );
                self.format_expr(expr);
            }
            ExprKind::PrimitiveCast {
                conversion_type,
                expr,
            } => {
                self.print_operation(
                    0x38,
                    format!("PrimitiveCast of type {}", Theme::numeric(conversion_type)),
                );
                self.format_tagged_expr("Argument", expr);
            }
            ExprKind::ObjToInterfaceCast {
                target_interface,
                expr,
            } => {
                self.print_operation(
                    0x52,
                    format!(
                        "ObjToInterfaceCast to {}",
                        Theme::type_name(self.resolve_class(target_interface))
                    ),
                );
                self.format_expr(expr);
            }
            ExprKind::InterfaceToObjCast { target_class, expr } => {
                self.print_operation(
                    0x55,
                    format!(
                        "InterfaceToObjCast to {}",
                        Theme::type_name(self.resolve_class(target_class))
                    ),
                );
                self.format_expr(expr);
            }
            ExprKind::CrossInterfaceCast {
                target_interface,
                expr,
            } => {
                self.print_operation(
                    0x54,
                    format!(
                        "InterfaceToInterfaceCast to {}",
                        Theme::type_name(self.resolve_class(target_interface))
                    ),
                );
                self.format_expr(expr);
            }

            // Collections
            ExprKind::ArrayConst {
                element_type,
                num_elements,
                elements,
            } => {
                self.print_operation(
                    0x65,
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
                self.print_operation(0x31, "set array");
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
                self.print_operation(0x39, "set set");
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
                self.print_operation(0x3B, "set map");
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
                self.print_operation(0x6B, "Array Get-by-Ref Index");
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
                    format!(
                        "Let (Variable = Expression) - {}",
                        Theme::variable(property_str)
                    ),
                );
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetObj { variable, value } => {
                self.print_operation(0x5F, "Let Obj (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetWeakObjPtr { variable, value } => {
                self.print_operation(0x60, "Let WeakObjPtr (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetBool { variable, value } => {
                self.print_operation(0x14, "LetBool (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetDelegate { variable, value } => {
                self.print_operation(0x44, "LetDelegate (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetMulticastDelegate { variable, value } => {
                self.print_operation(0x43, "LetMulticastDelegate (Variable = Expression)");
                self.format_tagged_expr("Variable", variable);
                self.format_tagged_expr("Expression", value);
            }
            ExprKind::LetValueOnPersistentFrame { property, value } => {
                self.print_operation(
                    0x64,
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
                    format!("instance delegate function named {}", name.as_str()),
                );
            }
            ExprKind::BindDelegate {
                func_name,
                delegate_expr,
                object_expr,
            } => {
                self.print_operation(0x61, format!("BindDelegate '{}'", func_name.as_str()));
                self.format_tagged_expr("Delegate", delegate_expr);
                self.format_tagged_expr("Object", object_expr);
            }
            ExprKind::AddMulticastDelegate {
                delegate_expr,
                to_add_expr,
            } => {
                self.print_operation(0x5C, "Add MC delegate");
                self.format_tagged_expr("Delegate", delegate_expr);
                self.format_tagged_expr("To Add", to_add_expr);
            }
            ExprKind::RemoveMulticastDelegate {
                delegate_expr,
                to_remove_expr,
            } => {
                self.print_operation(0x62, "Remove MC delegate");
                self.format_tagged_expr("Delegate", delegate_expr);
                self.format_tagged_expr("To Remove", to_remove_expr);
            }
            ExprKind::ClearMulticastDelegate(expr) => {
                self.print_operation(0x5D, "Clear MC delegate");
                self.format_expr(expr);
            }

            // Control flow
            ExprKind::Return(expr) => {
                self.print_operation(0x04, "Return expression");
                self.format_expr(expr);
            }
            ExprKind::Jump { target } => {
                self.print_operation(
                    0x06,
                    format!("Jump to offset {}", self.print_label(*target)),
                );
            }
            ExprKind::JumpIfNot { condition, target } => {
                self.print_operation(
                    0x07,
                    format!("Jump to {} if not:", self.print_label(*target)),
                );
                self.format_expr(condition);
            }
            ExprKind::ComputedJump { offset_expr } => {
                self.print_operation(0x4E, "Computed Jump, offset specified by expression:");
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
                    format!("FlowStack.Push({})", self.print_label(*push_offset)),
                );
            }
            ExprKind::PopExecutionFlow => {
                self.print_operation(
                    0x4D,
                    "if (FlowStack.Num()) { jump to FlowStack.Pop(); } else { ERROR!!! }",
                );
            }
            ExprKind::PopExecutionFlowIfNot { condition } => {
                self.print_operation(
                    0x4F,
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
                    format!("assert at line {}, in debug mode = {}", line, in_debug),
                );
                self.format_expr(condition);
            }
            ExprKind::Skip { skip_count, expr } => {
                self.print_operation(
                    0x18,
                    format!(
                        "possibly skip {} bytes of expr:",
                        Theme::offset(format!("0x{:X}", skip_count))
                    ),
                );
                self.format_expr(expr);
            }
            ExprKind::Breakpoint => {
                self.print_operation(0x50, "<<< BREAKPOINT >>>");
            }
            ExprKind::Tracepoint => {
                self.print_operation(0x5E, ".. debug site ..");
            }
            ExprKind::WireTracepoint => {
                self.print_operation(0x5A, ".. wire debug site ..");
            }
            ExprKind::InstrumentationEvent { event_type } => {
                self.print_operation(
                    0x6A,
                    format!(".. instrumented event type {} ..", event_type),
                );
            }

            // Special
            ExprKind::BitFieldConst => {
                self.print_operation(0x11, "EX_BitFieldConst (unimplemented)");
            }
            ExprKind::DeprecatedOp4A => {
                self.print_operation(0x4A, "This opcode has been removed and does nothing.");
            }
            ExprKind::EndOfScript => {
                self.print_operation(0x53, "EX_EndOfScript");
            }
            ExprKind::EndParmValue => {
                self.print_operation(0x15, "EX_EndParmValue");
            }

            ExprKind::SoftObjectConst(expr) => {
                self.print_operation(0x67, "EX_SoftObjectConst");
                self.format_expr(expr);
            }
            ExprKind::FieldPathConst(expr) => {
                self.print_operation(0x6D, "EX_FieldPathConst");
                self.format_expr(expr);
            }
        }

        self.drop_indent();
    }
}
