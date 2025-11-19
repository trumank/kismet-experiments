/// Expression intermediate representation for Kismet bytecode
use super::refs::{ClassRef, FunctionRef, ObjectRef, PropertyRef, StructRef};
use super::types::{BytecodeOffset, Name};

/// An expression with its bytecode location
#[derive(Debug, Clone)]
pub struct Expr {
    pub offset: BytecodeOffset,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(offset: BytecodeOffset, kind: ExprKind) -> Self {
        Self { offset, kind }
    }

    /// Walk the expression tree, calling the visitor function on each expression
    pub fn walk<F>(&self, visitor: &mut F)
    where
        F: FnMut(&Expr),
    {
        visitor(self);

        match &self.kind {
            // Assignments with nested expressions
            ExprKind::Let {
                variable, value, ..
            } => {
                variable.walk(visitor);
                value.walk(visitor);
            }
            ExprKind::LetObj { variable, value }
            | ExprKind::LetWeakObjPtr { variable, value }
            | ExprKind::LetBool { variable, value }
            | ExprKind::LetDelegate { variable, value }
            | ExprKind::LetMulticastDelegate { variable, value } => {
                variable.walk(visitor);
                value.walk(visitor);
            }
            ExprKind::LetValueOnPersistentFrame { value, .. } => {
                value.walk(visitor);
            }

            // Control flow
            ExprKind::Return(expr) => {
                expr.walk(visitor);
            }
            ExprKind::JumpIfNot { condition, .. } => {
                condition.walk(visitor);
            }
            ExprKind::ComputedJump { offset_expr } => {
                offset_expr.walk(visitor);
            }
            ExprKind::SwitchValue {
                index,
                cases,
                default,
                ..
            } => {
                index.walk(visitor);
                for case in cases {
                    case.case_value.walk(visitor);
                    case.result.walk(visitor);
                }
                default.walk(visitor);
            }
            ExprKind::PopExecutionFlowIfNot { condition } => {
                condition.walk(visitor);
            }

            // Debug/instrumentation
            ExprKind::Assert { condition, .. } => {
                condition.walk(visitor);
            }
            ExprKind::Skip { expr, .. } => {
                expr.walk(visitor);
            }

            // Context/member access
            ExprKind::Context {
                object, context, ..
            } => {
                object.walk(visitor);
                context.walk(visitor);
            }
            ExprKind::ClassContext {
                object, context, ..
            } => {
                object.walk(visitor);
                context.walk(visitor);
            }
            ExprKind::StructMemberContext { struct_expr, .. } => {
                struct_expr.walk(visitor);
            }
            ExprKind::InterfaceContext(expr) => {
                expr.walk(visitor);
            }

            // Function calls
            ExprKind::VirtualFunction { params, .. }
            | ExprKind::FinalFunction { params, .. }
            | ExprKind::LocalVirtualFunction { params, .. }
            | ExprKind::LocalFinalFunction { params, .. }
            | ExprKind::CallMath { params, .. } => {
                for param in params {
                    param.walk(visitor);
                }
            }
            ExprKind::CallMulticastDelegate {
                delegate_expr,
                params,
                ..
            } => {
                delegate_expr.walk(visitor);
                for param in params {
                    param.walk(visitor);
                }
            }

            // Casts
            ExprKind::DynamicCast { expr, .. }
            | ExprKind::MetaCast { expr, .. }
            | ExprKind::PrimitiveCast { expr, .. }
            | ExprKind::ObjToInterfaceCast { expr, .. }
            | ExprKind::InterfaceToObjCast { expr, .. }
            | ExprKind::CrossInterfaceCast { expr, .. } => {
                expr.walk(visitor);
            }

            // Collections
            ExprKind::ArrayConst { elements, .. }
            | ExprKind::StructConst { elements, .. }
            | ExprKind::SetConst { elements, .. }
            | ExprKind::MapConst { elements, .. } => {
                for elem in elements {
                    elem.walk(visitor);
                }
            }
            ExprKind::SetArray {
                array_expr,
                elements,
            } => {
                array_expr.walk(visitor);
                for elem in elements {
                    elem.walk(visitor);
                }
            }
            ExprKind::SetSet {
                set_expr, elements, ..
            } => {
                set_expr.walk(visitor);
                for elem in elements {
                    elem.walk(visitor);
                }
            }
            ExprKind::SetMap {
                map_expr, elements, ..
            } => {
                map_expr.walk(visitor);
                for elem in elements {
                    elem.walk(visitor);
                }
            }
            ExprKind::ArrayGetByRef {
                array_expr,
                index_expr,
            } => {
                array_expr.walk(visitor);
                index_expr.walk(visitor);
            }

            // Delegates
            ExprKind::BindDelegate {
                delegate_expr,
                object_expr,
                ..
            } => {
                delegate_expr.walk(visitor);
                object_expr.walk(visitor);
            }
            ExprKind::AddMulticastDelegate {
                delegate_expr,
                to_add_expr,
            } => {
                delegate_expr.walk(visitor);
                to_add_expr.walk(visitor);
            }
            ExprKind::RemoveMulticastDelegate {
                delegate_expr,
                to_remove_expr,
            } => {
                delegate_expr.walk(visitor);
                to_remove_expr.walk(visitor);
            }
            ExprKind::ClearMulticastDelegate(expr) => {
                expr.walk(visitor);
            }

            // Object references with nested expressions
            ExprKind::SoftObjectConst(expr) | ExprKind::FieldPathConst(expr) => {
                expr.walk(visitor);
            }

            // Text constants
            ExprKind::TextConst(text_lit) => match text_lit {
                TextLiteral::LocalizedText {
                    source,
                    key,
                    namespace,
                } => {
                    source.walk(visitor);
                    key.walk(visitor);
                    namespace.walk(visitor);
                }
                TextLiteral::InvariantText { source } | TextLiteral::LiteralString { source } => {
                    source.walk(visitor);
                }
                TextLiteral::StringTableEntry { table_id, key } => {
                    table_id.walk(visitor);
                    key.walk(visitor);
                }
                TextLiteral::Empty => {}
            },

            // Leaf nodes - no nested expressions
            ExprKind::LocalVariable(_)
            | ExprKind::InstanceVariable(_)
            | ExprKind::DefaultVariable(_)
            | ExprKind::LocalOutVariable(_)
            | ExprKind::ClassSparseDataVariable(_)
            | ExprKind::IntConst(_)
            | ExprKind::Int64Const(_)
            | ExprKind::UInt64Const(_)
            | ExprKind::IntZero
            | ExprKind::IntOne
            | ExprKind::ByteConst(_)
            | ExprKind::IntConstByte(_)
            | ExprKind::FloatConst(_)
            | ExprKind::StringConst(_)
            | ExprKind::UnicodeStringConst(_)
            | ExprKind::NameConst(_)
            | ExprKind::VectorConst { .. }
            | ExprKind::RotationConst { .. }
            | ExprKind::TransformConst { .. }
            | ExprKind::True
            | ExprKind::False
            | ExprKind::NoObject
            | ExprKind::NoInterface
            | ExprKind::Self_
            | ExprKind::Nothing
            | ExprKind::NothingInt32
            | ExprKind::ObjectConst(_)
            | ExprKind::PropertyConst(_)
            | ExprKind::SkipOffsetConst(_)
            | ExprKind::InstanceDelegate(_)
            | ExprKind::Jump { .. }
            | ExprKind::PushExecutionFlow { .. }
            | ExprKind::PopExecutionFlow
            | ExprKind::Breakpoint
            | ExprKind::Tracepoint
            | ExprKind::WireTracepoint
            | ExprKind::InstrumentationEvent { .. }
            | ExprKind::BitFieldConst
            | ExprKind::DeprecatedOp4A
            | ExprKind::EndOfScript
            | ExprKind::EndParmValue => {
                // No nested expressions to visit
            }
        }
    }
}

/// Collect all bytecode offsets that are referenced by control flow instructions
pub fn collect_referenced_offsets(
    expressions: &[Expr],
) -> std::collections::HashSet<BytecodeOffset> {
    use std::collections::HashSet;

    let mut offsets = HashSet::new();

    for expr in expressions {
        expr.walk(&mut |e| match &e.kind {
            ExprKind::Jump { target } => {
                offsets.insert(*target);
            }
            ExprKind::JumpIfNot { target, .. } => {
                offsets.insert(*target);
            }
            ExprKind::SwitchValue {
                cases, end_offset, ..
            } => {
                offsets.insert(*end_offset);
                for case in cases {
                    offsets.insert(case.case_offset);
                    offsets.insert(case.next_offset);
                }
            }
            ExprKind::PushExecutionFlow { push_offset } => {
                offsets.insert(*push_offset);
            }
            ExprKind::SkipOffsetConst(offset) => {
                offsets.insert(*offset);
            }
            _ => {}
        });
    }

    offsets
}

/// All possible expression types in Kismet bytecode
#[derive(Debug, Clone)]
pub enum ExprKind {
    // Variables
    LocalVariable(PropertyRef),
    InstanceVariable(PropertyRef),
    DefaultVariable(PropertyRef),
    LocalOutVariable(PropertyRef),
    ClassSparseDataVariable(PropertyRef),

    // Constants - integers
    IntConst(i32),
    Int64Const(i64),
    UInt64Const(u64),
    IntZero,
    IntOne,
    ByteConst(u8),
    IntConstByte(u8),

    // Constants - floating point
    FloatConst(f32),

    // Constants - strings and names
    StringConst(String),
    UnicodeStringConst(String),
    NameConst(Name),

    // Constants - vectors and transforms
    VectorConst {
        x: f32,
        y: f32,
        z: f32,
    },
    RotationConst {
        pitch: f32,
        yaw: f32,
        roll: f32,
    },
    TransformConst {
        rot_x: f32,
        rot_y: f32,
        rot_z: f32,
        rot_w: f32,
        trans_x: f32,
        trans_y: f32,
        trans_z: f32,
        scale_x: f32,
        scale_y: f32,
        scale_z: f32,
    },

    // Constants - special values
    True,
    False,
    NoObject,
    NoInterface,
    Self_,
    Nothing,
    NothingInt32,

    // Constants - object references
    ObjectConst(ObjectRef),
    SoftObjectConst(Box<Expr>),
    PropertyConst(PropertyRef),
    SkipOffsetConst(BytecodeOffset),
    FieldPathConst(Box<Expr>),

    // Text constants
    TextConst(TextLiteral),

    // Function calls
    VirtualFunction {
        func: FunctionRef,
        params: Vec<Expr>,
    },
    FinalFunction {
        func: FunctionRef,
        params: Vec<Expr>,
    },
    LocalVirtualFunction {
        func: FunctionRef,
        params: Vec<Expr>,
    },
    LocalFinalFunction {
        func: FunctionRef,
        params: Vec<Expr>,
    },
    CallMath {
        func: FunctionRef,
        params: Vec<Expr>,
    },
    CallMulticastDelegate {
        stack_node: FunctionRef,
        delegate_expr: Box<Expr>,
        params: Vec<Expr>,
    },

    // Member access / context
    Context {
        object: Box<Expr>,
        field: Option<PropertyRef>,
        context: Box<Expr>,
        skip_offset: u32,
        fail_silent: bool,
    },
    ClassContext {
        object: Box<Expr>,
        field: Option<PropertyRef>,
        context: Box<Expr>,
        skip_offset: u32,
    },
    StructMemberContext {
        struct_expr: Box<Expr>,
        member: PropertyRef,
    },
    InterfaceContext(Box<Expr>),

    // Casts
    DynamicCast {
        target_class: ClassRef,
        expr: Box<Expr>,
    },
    MetaCast {
        target_class: ClassRef,
        expr: Box<Expr>,
    },
    PrimitiveCast {
        conversion_type: u8,
        expr: Box<Expr>,
    },
    ObjToInterfaceCast {
        target_interface: ClassRef,
        expr: Box<Expr>,
    },
    InterfaceToObjCast {
        target_class: ClassRef,
        expr: Box<Expr>,
    },
    CrossInterfaceCast {
        target_interface: ClassRef,
        expr: Box<Expr>,
    },

    // Collections
    ArrayConst {
        element_type: PropertyRef,
        num_elements: i32,
        elements: Vec<Expr>,
    },
    StructConst {
        struct_type: StructRef,
        serialized_size: i32,
        elements: Vec<Expr>,
    },
    SetConst {
        element_type: PropertyRef,
        num_elements: i32,
        elements: Vec<Expr>,
    },
    MapConst {
        key_type: PropertyRef,
        value_type: PropertyRef,
        num_elements: i32,
        elements: Vec<Expr>,
    },

    // Array/set/map operations
    SetArray {
        array_expr: Box<Expr>,
        elements: Vec<Expr>,
    },
    SetSet {
        set_expr: Box<Expr>,
        num: i32,
        elements: Vec<Expr>,
    },
    SetMap {
        map_expr: Box<Expr>,
        num: i32,
        elements: Vec<Expr>,
    },
    ArrayGetByRef {
        array_expr: Box<Expr>,
        index_expr: Box<Expr>,
    },

    // Assignment expressions (have side effects)
    Let {
        property: Option<PropertyRef>,
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetObj {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetWeakObjPtr {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetBool {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetDelegate {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetMulticastDelegate {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetValueOnPersistentFrame {
        property: PropertyRef,
        value: Box<Expr>,
    },

    // Delegate operations
    InstanceDelegate(Name),
    BindDelegate {
        func_name: Name,
        delegate_expr: Box<Expr>,
        object_expr: Box<Expr>,
    },
    AddMulticastDelegate {
        delegate_expr: Box<Expr>,
        to_add_expr: Box<Expr>,
    },
    RemoveMulticastDelegate {
        delegate_expr: Box<Expr>,
        to_remove_expr: Box<Expr>,
    },
    ClearMulticastDelegate(Box<Expr>),

    // Control flow
    Return(Box<Expr>),
    Jump {
        target: BytecodeOffset,
    },
    JumpIfNot {
        condition: Box<Expr>,
        target: BytecodeOffset,
    },
    ComputedJump {
        offset_expr: Box<Expr>,
    },
    SwitchValue {
        index: Box<Expr>,
        cases: Vec<SwitchCase>,
        default: Box<Expr>,
        end_offset: BytecodeOffset,
    },

    // Execution flow control
    PushExecutionFlow {
        push_offset: BytecodeOffset,
    },
    PopExecutionFlow,
    PopExecutionFlowIfNot {
        condition: Box<Expr>,
    },

    // Debug/instrumentation
    Assert {
        line: u16,
        in_debug: bool,
        condition: Box<Expr>,
    },
    Skip {
        skip_count: u32,
        expr: Box<Expr>,
    },
    Breakpoint,
    Tracepoint,
    WireTracepoint,
    InstrumentationEvent {
        event_type: u8,
    },

    // Special
    BitFieldConst,
    DeprecatedOp4A,
    EndOfScript,
    EndParmValue,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub case_offset: BytecodeOffset,
    pub case_value: Expr,
    pub next_offset: BytecodeOffset,
    pub result: Expr,
}

#[derive(Debug, Clone)]
pub enum TextLiteral {
    Empty,
    LocalizedText {
        source: Box<Expr>,
        key: Box<Expr>,
        namespace: Box<Expr>,
    },
    InvariantText {
        source: Box<Expr>,
    },
    LiteralString {
        source: Box<Expr>,
    },
    StringTableEntry {
        table_id: Box<Expr>,
        key: Box<Expr>,
    },
}
