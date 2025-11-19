/// Parser for Kismet bytecode to expression IR
use super::expr::{Expr, ExprKind, SwitchCase, TextLiteral};
use super::opcodes::{EBlueprintTextLiteralType, EExprToken};
use super::reader::ScriptReader;
use super::refs::{ClassRef, FunctionRef, ObjectRef, PropertyRef, StructRef};
use super::types::BytecodeOffset;

/// Parser that converts bytecode to expression IR
pub struct ScriptParser<'a> {
    reader: ScriptReader<'a>,
}

impl<'a> ScriptParser<'a> {
    pub fn new(reader: ScriptReader<'a>) -> Self {
        Self { reader }
    }

    /// Parse all expressions in the script
    pub fn parse_all(&mut self) -> Vec<Expr> {
        let mut expressions = Vec::new();
        let mut offset = 0;

        while offset < self.reader.script().len() {
            let expr = self.parse_expr(&mut offset);
            expressions.push(expr);
        }

        // Assert that bytecode properly terminates with EndOfScript
        assert!(
            !expressions.is_empty()
                && matches!(expressions.last().unwrap().kind, ExprKind::EndOfScript),
            "Bytecode must terminate with EndOfScript"
        );

        // Remove EndOfScript from the result
        expressions.pop();

        expressions
    }

    /// Parse a single expression starting at the given offset
    pub fn parse_expr(&mut self, offset: &mut usize) -> Expr {
        let expr_offset = BytecodeOffset::new(*offset);
        let opcode = EExprToken::from(self.reader.script()[*offset]);
        *offset += 1;

        let kind = self.parse_opcode(opcode, offset);

        Expr::new(expr_offset, kind)
    }

    fn parse_opcode(&mut self, opcode: EExprToken, offset: &mut usize) -> ExprKind {
        match opcode {
            // Variables
            EExprToken::LocalVariable => {
                let address = self.reader.read_address(offset);
                ExprKind::LocalVariable(PropertyRef::new(address))
            }
            EExprToken::InstanceVariable => {
                let address = self.reader.read_address(offset);
                ExprKind::InstanceVariable(PropertyRef::new(address))
            }
            EExprToken::DefaultVariable => {
                let address = self.reader.read_address(offset);
                ExprKind::DefaultVariable(PropertyRef::new(address))
            }
            EExprToken::LocalOutVariable => {
                let address = self.reader.read_address(offset);
                ExprKind::LocalOutVariable(PropertyRef::new(address))
            }
            EExprToken::ClassSparseDataVariable => {
                let address = self.reader.read_address(offset);
                ExprKind::ClassSparseDataVariable(PropertyRef::new(address))
            }

            // Integer constants
            EExprToken::IntConst => ExprKind::IntConst(self.reader.read_int(offset)),
            EExprToken::Int64Const => ExprKind::Int64Const(self.reader.read_qword(offset) as i64),
            EExprToken::UInt64Const => ExprKind::UInt64Const(self.reader.read_qword(offset)),
            EExprToken::IntZero => ExprKind::IntZero,
            EExprToken::IntOne => ExprKind::IntOne,
            EExprToken::ByteConst => ExprKind::ByteConst(self.reader.read_byte(offset)),
            EExprToken::IntConstByte => ExprKind::IntConstByte(self.reader.read_byte(offset)),

            // Floating point constants
            EExprToken::FloatConst => ExprKind::FloatConst(self.reader.read_float(offset)),

            // String constants
            EExprToken::StringConst => ExprKind::StringConst(self.reader.read_string8(offset)),
            EExprToken::UnicodeStringConst => {
                ExprKind::UnicodeStringConst(self.reader.read_string16(offset))
            }
            EExprToken::NameConst => ExprKind::NameConst(self.reader.read_name(offset)),

            // Vector/rotation/transform constants
            EExprToken::VectorConst => ExprKind::VectorConst {
                x: self.reader.read_float(offset),
                y: self.reader.read_float(offset),
                z: self.reader.read_float(offset),
            },
            EExprToken::RotationConst => ExprKind::RotationConst {
                pitch: self.reader.read_float(offset),
                yaw: self.reader.read_float(offset),
                roll: self.reader.read_float(offset),
            },
            EExprToken::TransformConst => ExprKind::TransformConst {
                rot_x: self.reader.read_float(offset),
                rot_y: self.reader.read_float(offset),
                rot_z: self.reader.read_float(offset),
                rot_w: self.reader.read_float(offset),
                trans_x: self.reader.read_float(offset),
                trans_y: self.reader.read_float(offset),
                trans_z: self.reader.read_float(offset),
                scale_x: self.reader.read_float(offset),
                scale_y: self.reader.read_float(offset),
                scale_z: self.reader.read_float(offset),
            },

            // Special constants
            EExprToken::True => ExprKind::True,
            EExprToken::False => ExprKind::False,
            EExprToken::NoObject => ExprKind::NoObject,
            EExprToken::NoInterface => ExprKind::NoInterface,
            EExprToken::Self_ => ExprKind::Self_,
            EExprToken::Nothing => ExprKind::Nothing,
            EExprToken::NothingInt32 => ExprKind::NothingInt32,

            // Object references
            EExprToken::ObjectConst => {
                let address = self.reader.read_address(offset);
                ExprKind::ObjectConst(ObjectRef::new(address))
            }
            EExprToken::SoftObjectConst => {
                let expr = self.parse_expr(offset);
                ExprKind::SoftObjectConst(Box::new(expr))
            }
            EExprToken::PropertyConst => {
                let address = self.reader.read_address(offset);
                ExprKind::PropertyConst(PropertyRef::new(address))
            }
            EExprToken::SkipOffsetConst => ExprKind::SkipOffsetConst(BytecodeOffset::new(
                self.reader.read_skip_count(offset) as usize,
            )),
            EExprToken::FieldPathConst => {
                let expr = self.parse_expr(offset);
                ExprKind::FieldPathConst(Box::new(expr))
            }

            // Text constants
            EExprToken::TextConst => {
                let text_type = EBlueprintTextLiteralType::from(self.reader.script()[*offset]);
                *offset += 1;

                let text_literal = match text_type {
                    EBlueprintTextLiteralType::Empty => TextLiteral::Empty,
                    EBlueprintTextLiteralType::LocalizedText => TextLiteral::LocalizedText {
                        source: Box::new(self.parse_expr(offset)),
                        key: Box::new(self.parse_expr(offset)),
                        namespace: Box::new(self.parse_expr(offset)),
                    },
                    EBlueprintTextLiteralType::InvariantText => TextLiteral::InvariantText {
                        source: Box::new(self.parse_expr(offset)),
                    },
                    EBlueprintTextLiteralType::LiteralString => TextLiteral::LiteralString {
                        source: Box::new(self.parse_expr(offset)),
                    },
                    EBlueprintTextLiteralType::StringTableEntry => {
                        let _table_object = self.reader.read_address(offset);
                        TextLiteral::StringTableEntry {
                            table_id: Box::new(self.parse_expr(offset)),
                            key: Box::new(self.parse_expr(offset)),
                        }
                    }
                };

                ExprKind::TextConst(text_literal)
            }

            // Function calls
            EExprToken::VirtualFunction => {
                let name = self.reader.read_name(offset);
                let func = FunctionRef::from_name(name);
                let params = self.parse_function_params(offset);
                ExprKind::VirtualFunction { func, params }
            }
            EExprToken::FinalFunction => {
                let address = self.reader.read_address(offset);
                let func = FunctionRef::from_address(address);
                let params = self.parse_function_params(offset);
                ExprKind::FinalFunction { func, params }
            }
            EExprToken::LocalVirtualFunction => {
                let name = self.reader.read_name(offset);
                let func = FunctionRef::from_name(name);
                let params = self.parse_function_params(offset);
                ExprKind::LocalVirtualFunction { func, params }
            }
            EExprToken::LocalFinalFunction => {
                let address = self.reader.read_address(offset);
                let func = FunctionRef::from_address(address);
                let params = self.parse_function_params(offset);
                ExprKind::LocalFinalFunction { func, params }
            }
            EExprToken::CallMath => {
                let address = self.reader.read_address(offset);
                let func = FunctionRef::from_address(address);
                let params = self.parse_function_params(offset);
                ExprKind::CallMath { func, params }
            }
            EExprToken::CallMulticastDelegate => {
                let address = self.reader.read_address(offset);
                let stack_node = FunctionRef::from_address(address);
                let delegate_expr = Box::new(self.parse_expr(offset));
                let params = self.parse_function_params(offset);
                ExprKind::CallMulticastDelegate {
                    stack_node,
                    delegate_expr,
                    params,
                }
            }

            // Context/member access
            EExprToken::Context | EExprToken::ContextFailSilent => {
                let fail_silent = opcode == EExprToken::ContextFailSilent;
                let object = Box::new(self.parse_expr(offset));
                let skip_offset = self.reader.read_skip_count(offset);
                let address = self.reader.read_address(offset);
                let field = if address.as_u64() == 0 {
                    None
                } else {
                    Some(PropertyRef::new(address))
                };
                let context = Box::new(self.parse_expr(offset));
                ExprKind::Context {
                    object,
                    field,
                    context,
                    skip_offset,
                    fail_silent,
                }
            }
            EExprToken::ClassContext => {
                let object = Box::new(self.parse_expr(offset));
                let skip_offset = self.reader.read_skip_count(offset);
                let address = self.reader.read_address(offset);
                let field = if address.as_u64() == 0 {
                    None
                } else {
                    Some(PropertyRef::new(address))
                };
                let context = Box::new(self.parse_expr(offset));
                ExprKind::ClassContext {
                    object,
                    field,
                    context,
                    skip_offset,
                }
            }
            EExprToken::StructMemberContext => {
                let address = self.reader.read_address(offset);
                let member = PropertyRef::new(address);
                let struct_expr = Box::new(self.parse_expr(offset));
                ExprKind::StructMemberContext {
                    struct_expr,
                    member,
                }
            }
            EExprToken::InterfaceContext => {
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::InterfaceContext(expr)
            }

            // Casts
            EExprToken::DynamicCast => {
                let address = self.reader.read_address(offset);
                let target_class = ClassRef::new(address);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::DynamicCast { target_class, expr }
            }
            EExprToken::MetaCast => {
                let address = self.reader.read_address(offset);
                let target_class = ClassRef::new(address);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::MetaCast { target_class, expr }
            }
            EExprToken::PrimitiveCast => {
                let conversion_type = self.reader.read_byte(offset);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::PrimitiveCast {
                    conversion_type,
                    expr,
                }
            }
            EExprToken::ObjToInterfaceCast => {
                let address = self.reader.read_address(offset);
                let target_interface = ClassRef::new(address);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::ObjToInterfaceCast {
                    target_interface,
                    expr,
                }
            }
            EExprToken::InterfaceToObjCast => {
                let address = self.reader.read_address(offset);
                let target_class = ClassRef::new(address);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::InterfaceToObjCast { target_class, expr }
            }
            EExprToken::CrossInterfaceCast => {
                let address = self.reader.read_address(offset);
                let target_interface = ClassRef::new(address);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::CrossInterfaceCast {
                    target_interface,
                    expr,
                }
            }

            // Collections
            EExprToken::ArrayConst => {
                let address = self.reader.read_address(offset);
                let element_type = PropertyRef::new(address);
                let num_elements = self.reader.read_int(offset);
                let elements = self.parse_until(offset, EExprToken::EndArrayConst);
                ExprKind::ArrayConst {
                    element_type,
                    num_elements,
                    elements,
                }
            }
            EExprToken::StructConst => {
                let address = self.reader.read_address(offset);
                let struct_type = StructRef::new(address);
                let serialized_size = self.reader.read_int(offset);
                let elements = self.parse_until(offset, EExprToken::EndStructConst);
                ExprKind::StructConst {
                    struct_type,
                    serialized_size,
                    elements,
                }
            }
            EExprToken::SetConst => {
                let address = self.reader.read_address(offset);
                let element_type = PropertyRef::new(address);
                let num_elements = self.reader.read_int(offset);
                let elements = self.parse_until(offset, EExprToken::EndSetConst);
                ExprKind::SetConst {
                    element_type,
                    num_elements,
                    elements,
                }
            }
            EExprToken::MapConst => {
                let key_address = self.reader.read_address(offset);
                let key_type = PropertyRef::new(key_address);
                let value_address = self.reader.read_address(offset);
                let value_type = PropertyRef::new(value_address);
                let num_elements = self.reader.read_int(offset);
                let elements = self.parse_until(offset, EExprToken::EndMapConst);
                ExprKind::MapConst {
                    key_type,
                    value_type,
                    num_elements,
                    elements,
                }
            }

            // Array/set/map operations
            EExprToken::SetArray => {
                let array_expr = Box::new(self.parse_expr(offset));
                let elements = self.parse_until(offset, EExprToken::EndArray);
                ExprKind::SetArray {
                    array_expr,
                    elements,
                }
            }
            EExprToken::SetSet => {
                let set_expr = Box::new(self.parse_expr(offset));
                let num = self.reader.read_int(offset);
                let elements = self.parse_until(offset, EExprToken::EndSet);
                ExprKind::SetSet {
                    set_expr,
                    num,
                    elements,
                }
            }
            EExprToken::SetMap => {
                let map_expr = Box::new(self.parse_expr(offset));
                let num = self.reader.read_int(offset);
                let elements = self.parse_until(offset, EExprToken::EndMap);
                ExprKind::SetMap {
                    map_expr,
                    num,
                    elements,
                }
            }
            EExprToken::ArrayGetByRef => {
                let array_expr = Box::new(self.parse_expr(offset));
                let index_expr = Box::new(self.parse_expr(offset));
                ExprKind::ArrayGetByRef {
                    array_expr,
                    index_expr,
                }
            }

            // Assignments
            EExprToken::Let => {
                let address = self.reader.read_address(offset);
                let property = if address.as_u64() == 0 {
                    None
                } else {
                    Some(PropertyRef::new(address))
                };
                let variable = Box::new(self.parse_expr(offset));
                let value = Box::new(self.parse_expr(offset));
                ExprKind::Let {
                    property,
                    variable,
                    value,
                }
            }
            EExprToken::LetObj => {
                let variable = Box::new(self.parse_expr(offset));
                let value = Box::new(self.parse_expr(offset));
                ExprKind::LetObj { variable, value }
            }
            EExprToken::LetWeakObjPtr => {
                let variable = Box::new(self.parse_expr(offset));
                let value = Box::new(self.parse_expr(offset));
                ExprKind::LetWeakObjPtr { variable, value }
            }
            EExprToken::LetBool => {
                let variable = Box::new(self.parse_expr(offset));
                let value = Box::new(self.parse_expr(offset));
                ExprKind::LetBool { variable, value }
            }
            EExprToken::LetDelegate => {
                let variable = Box::new(self.parse_expr(offset));
                let value = Box::new(self.parse_expr(offset));
                ExprKind::LetDelegate { variable, value }
            }
            EExprToken::LetMulticastDelegate => {
                let variable = Box::new(self.parse_expr(offset));
                let value = Box::new(self.parse_expr(offset));
                ExprKind::LetMulticastDelegate { variable, value }
            }
            EExprToken::LetValueOnPersistentFrame => {
                let address = self.reader.read_address(offset);
                let property = PropertyRef::new(address);
                let value = Box::new(self.parse_expr(offset));
                ExprKind::LetValueOnPersistentFrame { property, value }
            }

            // Delegates
            EExprToken::InstanceDelegate => {
                ExprKind::InstanceDelegate(self.reader.read_name(offset))
            }
            EExprToken::BindDelegate => {
                let func_name = self.reader.read_name(offset);
                let delegate_expr = Box::new(self.parse_expr(offset));
                let object_expr = Box::new(self.parse_expr(offset));
                ExprKind::BindDelegate {
                    func_name,
                    delegate_expr,
                    object_expr,
                }
            }
            EExprToken::AddMulticastDelegate => {
                let delegate_expr = Box::new(self.parse_expr(offset));
                let to_add_expr = Box::new(self.parse_expr(offset));
                ExprKind::AddMulticastDelegate {
                    delegate_expr,
                    to_add_expr,
                }
            }
            EExprToken::RemoveMulticastDelegate => {
                let delegate_expr = Box::new(self.parse_expr(offset));
                let to_remove_expr = Box::new(self.parse_expr(offset));
                ExprKind::RemoveMulticastDelegate {
                    delegate_expr,
                    to_remove_expr,
                }
            }
            EExprToken::ClearMulticastDelegate => {
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::ClearMulticastDelegate(expr)
            }

            // Control flow
            EExprToken::Return => {
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::Return(expr)
            }
            EExprToken::Jump => {
                let target = BytecodeOffset::new(self.reader.read_skip_count(offset) as usize);
                ExprKind::Jump { target }
            }
            EExprToken::JumpIfNot => {
                let target = BytecodeOffset::new(self.reader.read_skip_count(offset) as usize);
                let condition = Box::new(self.parse_expr(offset));
                ExprKind::JumpIfNot { condition, target }
            }
            EExprToken::ComputedJump => {
                let offset_expr = Box::new(self.parse_expr(offset));
                ExprKind::ComputedJump { offset_expr }
            }
            EExprToken::SwitchValue => {
                let num_cases = self.reader.read_word(offset);
                let end_offset = BytecodeOffset::new(self.reader.read_skip_count(offset) as usize);
                let index = Box::new(self.parse_expr(offset));

                let mut cases = Vec::new();
                for _ in 0..num_cases {
                    let case_offset = BytecodeOffset::new(*offset);
                    let case_value = self.parse_expr(offset);
                    let next_offset =
                        BytecodeOffset::new(self.reader.read_skip_count(offset) as usize);
                    let result = self.parse_expr(offset);

                    cases.push(SwitchCase {
                        case_offset,
                        case_value,
                        next_offset,
                        result,
                    });
                }

                let default = Box::new(self.parse_expr(offset));

                ExprKind::SwitchValue {
                    index,
                    cases,
                    default,
                    end_offset,
                }
            }

            // Execution flow
            EExprToken::PushExecutionFlow => {
                let push_offset = BytecodeOffset::new(self.reader.read_skip_count(offset) as usize);
                ExprKind::PushExecutionFlow { push_offset }
            }
            EExprToken::PopExecutionFlow => ExprKind::PopExecutionFlow,
            EExprToken::PopExecutionFlowIfNot => {
                let condition = Box::new(self.parse_expr(offset));
                ExprKind::PopExecutionFlowIfNot { condition }
            }

            // Debug/instrumentation
            EExprToken::Assert => {
                let line = self.reader.read_word(offset);
                let in_debug = self.reader.read_byte(offset) != 0;
                let condition = Box::new(self.parse_expr(offset));
                ExprKind::Assert {
                    line,
                    in_debug,
                    condition,
                }
            }
            EExprToken::Skip => {
                let skip_count = self.reader.read_skip_count(offset);
                let expr = Box::new(self.parse_expr(offset));
                ExprKind::Skip { skip_count, expr }
            }
            EExprToken::Breakpoint => ExprKind::Breakpoint,
            EExprToken::Tracepoint => ExprKind::Tracepoint,
            EExprToken::WireTracepoint => ExprKind::WireTracepoint,
            EExprToken::InstrumentationEvent => {
                let event_type = self.reader.script()[*offset];
                const INLINE_EVENT: u8 = 0;
                if event_type == INLINE_EVENT {
                    *offset += 12; // Skip FScriptName
                }
                *offset += 1; // Skip event type
                ExprKind::InstrumentationEvent { event_type }
            }

            // Special/terminator tokens
            EExprToken::BitFieldConst => ExprKind::BitFieldConst,
            EExprToken::DeprecatedOp4A => ExprKind::DeprecatedOp4A,
            EExprToken::EndOfScript => ExprKind::EndOfScript,
            EExprToken::EndParmValue => ExprKind::EndParmValue,

            // End markers - should be handled by their containing expressions
            EExprToken::EndFunctionParms
            | EExprToken::EndStructConst
            | EExprToken::EndArray
            | EExprToken::EndArrayConst
            | EExprToken::EndSetConst
            | EExprToken::EndMapConst
            | EExprToken::EndSet
            | EExprToken::EndMap => {
                // These should have been consumed by their container
                panic!(
                    "Unexpected end marker: {:?} at offset {}",
                    opcode,
                    *offset - 1
                );
            }

            EExprToken::Unknown(val) => {
                panic!("Unknown opcode 0x{:02X} at offset {}", val, *offset - 1);
            }
        }
    }

    /// Parse function parameters until EndFunctionParms
    fn parse_function_params(&mut self, offset: &mut usize) -> Vec<Expr> {
        let mut params = Vec::new();
        loop {
            let opcode = EExprToken::from(self.reader.script()[*offset]);
            if opcode == EExprToken::EndFunctionParms {
                *offset += 1;
                break;
            }
            params.push(self.parse_expr(offset));
        }
        params
    }

    /// Parse expressions until a specific end token
    fn parse_until(&mut self, offset: &mut usize, end_token: EExprToken) -> Vec<Expr> {
        let mut elements = Vec::new();
        loop {
            let opcode = EExprToken::from(self.reader.script()[*offset]);
            if opcode == end_token {
                *offset += 1;
                break;
            }
            elements.push(self.parse_expr(offset));
        }
        elements
    }
}
