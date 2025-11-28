pub mod nodes;

pub use nodes::{
    ESelfContextInfo, EdGraphNode, ExportTextPath, FGraphReference, FLinearColor,
    FOptionalPinFromProperty, Guid, K2Node, K2NodeAddComponentByClass, K2NodeAddDelegate,
    K2NodeBreakStruct, K2NodeCallArrayFunction, K2NodeCallFunction,
    K2NodeCommutativeAssociativeBinaryOperator, K2NodeComponentBoundEvent, K2NodeComposite,
    K2NodeCreateWidget, K2NodeCustomEvent, K2NodeDynamicCast, K2NodeEnumEquality,
    K2NodeEnumInequality, K2NodeEnumLiteral, K2NodeEvent, K2NodeExecutionSequence,
    K2NodeFormatText, K2NodeFunctionEntry, K2NodeFunctionResult, K2NodeGenericCreateObject,
    K2NodeGetArrayItem, K2NodeGetEnumeratorNameAsString, K2NodeIfThenElse, K2NodeInputAction,
    K2NodeKnot, K2NodeMacroInstance, K2NodeMakeArray, K2NodeMakeStruct, K2NodeSelect, K2NodeSelf,
    K2NodeSpawnActorFromClass, K2NodeStructMemberGet, K2NodeStructMemberSet, K2NodeStructOperation,
    K2NodeSwitchEnum, K2NodeTunnel, K2NodeVariableGet, K2NodeVariableSet, MemberReference,
    ObjectTrait, Pin, PinType, UEdGraph, UEdGraphNodeComment,
};

/// Parse error with line and column tracking
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub context: Option<String>,
}

impl ParseError {
    /// Create a new parse error with position information
    pub fn new(message: String, line: usize, column: usize) -> Self {
        Self {
            message,
            line,
            column,
            context: None,
        }
    }

    /// Create a new parse error with context
    pub fn with_context(message: String, line: usize, column: usize, context: String) -> Self {
        Self {
            message,
            line,
            column,
            context: Some(context),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parse error at {}:{}: {}",
            self.line, self.column, self.message
        )?;
        if let Some(ref ctx) = self.context {
            write!(f, "\n  {}", ctx)?;
            write!(f, "\n  {}^", " ".repeat(self.column.saturating_sub(1)))?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

/// Represents the graph type enum from UE4
#[derive(Debug, Clone, PartialEq)]
pub enum GraphType {
    Function,
    Ubergraph,
    Macro,
    Animation,
    Other(String),
}

impl GraphType {
    fn from_str(s: &str) -> Self {
        match s {
            "GT_Function" => GraphType::Function,
            "GT_Ubergraph" => GraphType::Ubergraph,
            "GT_Macro" => GraphType::Macro,
            "GT_Animation" => GraphType::Animation,
            _ => GraphType::Other(s.to_string()),
        }
    }

    fn to_str(&self) -> &str {
        match self {
            GraphType::Function => "GT_Function",
            GraphType::Ubergraph => "GT_Ubergraph",
            GraphType::Macro => "GT_Macro",
            GraphType::Animation => "GT_Animation",
            GraphType::Other(s) => s,
        }
    }
}

/// Represents the top-level Blueprint Graph clipboard data
#[derive(Debug, Clone, PartialEq)]
pub struct BPGraphClipboardData {
    pub graph_name: String,
    pub graph_type: GraphType,
    pub original_blueprint: Option<ExportTextPath>,
    pub nodes: Vec<UObject>,
}

/// Represents an EdGraphNode-derived object (all K2Node types and EdGraphNode_Comment)
#[derive(Debug, Clone, PartialEq)]
pub enum EdGraphNodeObject {
    K2NodeAddComponentByClass(K2NodeAddComponentByClass),
    K2NodeAddDelegate(K2NodeAddDelegate),
    K2NodeBreakStruct(K2NodeBreakStruct),
    K2NodeCallArrayFunction(K2NodeCallArrayFunction),
    K2NodeCallFunction(K2NodeCallFunction),
    K2NodeCommutativeAssociativeBinaryOperator(K2NodeCommutativeAssociativeBinaryOperator),
    K2NodeComponentBoundEvent(K2NodeComponentBoundEvent),
    K2NodeComposite(K2NodeComposite),
    K2NodeCreateWidget(K2NodeCreateWidget),
    K2NodeCustomEvent(K2NodeCustomEvent),
    K2NodeDynamicCast(K2NodeDynamicCast),
    K2NodeEnumEquality(K2NodeEnumEquality),
    K2NodeEnumInequality(K2NodeEnumInequality),
    K2NodeEnumLiteral(K2NodeEnumLiteral),
    K2NodeEvent(K2NodeEvent),
    K2NodeExecutionSequence(K2NodeExecutionSequence),
    K2NodeFormatText(K2NodeFormatText),
    K2NodeFunctionEntry(K2NodeFunctionEntry),
    K2NodeFunctionResult(K2NodeFunctionResult),
    K2NodeGenericCreateObject(K2NodeGenericCreateObject),
    K2NodeGetArrayItem(K2NodeGetArrayItem),
    K2NodeGetEnumeratorNameAsString(K2NodeGetEnumeratorNameAsString),
    K2NodeIfThenElse(K2NodeIfThenElse),
    K2NodeInputAction(K2NodeInputAction),
    K2NodeKnot(K2NodeKnot),
    K2NodeMacroInstance(K2NodeMacroInstance),
    K2NodeMakeArray(K2NodeMakeArray),
    K2NodeMakeStruct(K2NodeMakeStruct),
    K2NodeSelect(K2NodeSelect),
    K2NodeSelf(K2NodeSelf),
    K2NodeSpawnActorFromClass(K2NodeSpawnActorFromClass),
    K2NodeSwitchEnum(K2NodeSwitchEnum),
    K2NodeTunnel(K2NodeTunnel),
    K2NodeVariableGet(K2NodeVariableGet),
    K2NodeVariableSet(K2NodeVariableSet),
    UEdGraphNodeComment(UEdGraphNodeComment),
}

impl EdGraphNodeObject {
    /// Get the name of this EdGraphNode object
    pub fn name(&self) -> &str {
        match self {
            EdGraphNodeObject::K2NodeAddComponentByClass(node) => &node.name,
            EdGraphNodeObject::K2NodeAddDelegate(node) => &node.name,
            EdGraphNodeObject::K2NodeBreakStruct(node) => &node.name,
            EdGraphNodeObject::K2NodeCallArrayFunction(node) => &node.name,
            EdGraphNodeObject::K2NodeCallFunction(node) => &node.name,
            EdGraphNodeObject::K2NodeCommutativeAssociativeBinaryOperator(node) => &node.name,
            EdGraphNodeObject::K2NodeComponentBoundEvent(node) => &node.name,
            EdGraphNodeObject::K2NodeComposite(node) => &node.name,
            EdGraphNodeObject::K2NodeCreateWidget(node) => &node.name,
            EdGraphNodeObject::K2NodeCustomEvent(node) => &node.name,
            EdGraphNodeObject::K2NodeDynamicCast(node) => &node.name,
            EdGraphNodeObject::K2NodeEnumEquality(node) => &node.name,
            EdGraphNodeObject::K2NodeEnumInequality(node) => &node.name,
            EdGraphNodeObject::K2NodeEnumLiteral(node) => &node.name,
            EdGraphNodeObject::K2NodeEvent(node) => &node.name,
            EdGraphNodeObject::K2NodeExecutionSequence(node) => &node.name,
            EdGraphNodeObject::K2NodeFormatText(node) => &node.name,
            EdGraphNodeObject::K2NodeFunctionEntry(node) => &node.name,
            EdGraphNodeObject::K2NodeFunctionResult(node) => &node.name,
            EdGraphNodeObject::K2NodeGenericCreateObject(node) => &node.name,
            EdGraphNodeObject::K2NodeGetArrayItem(node) => &node.name,
            EdGraphNodeObject::K2NodeGetEnumeratorNameAsString(node) => &node.name,
            EdGraphNodeObject::K2NodeIfThenElse(node) => &node.name,
            EdGraphNodeObject::K2NodeInputAction(node) => &node.name,
            EdGraphNodeObject::K2NodeKnot(node) => &node.name,
            EdGraphNodeObject::K2NodeMacroInstance(node) => &node.name,
            EdGraphNodeObject::K2NodeMakeArray(node) => &node.name,
            EdGraphNodeObject::K2NodeMakeStruct(node) => &node.name,
            EdGraphNodeObject::K2NodeSelect(node) => &node.name,
            EdGraphNodeObject::K2NodeSelf(node) => &node.name,
            EdGraphNodeObject::K2NodeSpawnActorFromClass(node) => &node.name,
            EdGraphNodeObject::K2NodeSwitchEnum(node) => &node.name,
            EdGraphNodeObject::K2NodeTunnel(node) => &node.name,
            EdGraphNodeObject::K2NodeVariableGet(node) => &node.name,
            EdGraphNodeObject::K2NodeVariableSet(node) => &node.name,
            EdGraphNodeObject::UEdGraphNodeComment(node) => &node.name,
        }
    }

    /// fucking lmao
    pub fn as_ed_graph_node(&self) -> &EdGraphNode {
        match self {
            EdGraphNodeObject::K2NodeAddComponentByClass(node) => &node.base.base,
            EdGraphNodeObject::K2NodeAddDelegate(node) => &node.base.base,
            EdGraphNodeObject::K2NodeBreakStruct(node) => &node.base.base,
            EdGraphNodeObject::K2NodeCallArrayFunction(node) => &node.base.base,
            EdGraphNodeObject::K2NodeCallFunction(node) => &node.base.base,
            EdGraphNodeObject::K2NodeCommutativeAssociativeBinaryOperator(node) => &node.base.base,
            EdGraphNodeObject::K2NodeComponentBoundEvent(node) => &node.base.base,
            EdGraphNodeObject::K2NodeComposite(node) => &node.base.base,
            EdGraphNodeObject::K2NodeCreateWidget(node) => &node.base.base,
            EdGraphNodeObject::K2NodeCustomEvent(node) => &node.base.base.base,
            EdGraphNodeObject::K2NodeDynamicCast(node) => &node.base.base,
            EdGraphNodeObject::K2NodeEnumEquality(node) => &node.base.base,
            EdGraphNodeObject::K2NodeEnumInequality(node) => &node.base.base,
            EdGraphNodeObject::K2NodeEnumLiteral(node) => &node.base.base,
            EdGraphNodeObject::K2NodeEvent(node) => &node.base.base.base,
            EdGraphNodeObject::K2NodeExecutionSequence(node) => &node.base.base,
            EdGraphNodeObject::K2NodeFormatText(node) => &node.base.base,
            EdGraphNodeObject::K2NodeFunctionEntry(node) => &node.base.base.base,
            EdGraphNodeObject::K2NodeFunctionResult(node) => &node.base.base.base,
            EdGraphNodeObject::K2NodeGenericCreateObject(node) => &node.base.base,
            EdGraphNodeObject::K2NodeGetArrayItem(node) => &node.base.base,
            EdGraphNodeObject::K2NodeGetEnumeratorNameAsString(node) => &node.base.base,
            EdGraphNodeObject::K2NodeIfThenElse(node) => &node.base.base,
            EdGraphNodeObject::K2NodeInputAction(node) => &node.base.base,
            EdGraphNodeObject::K2NodeKnot(node) => &node.base.base,
            EdGraphNodeObject::K2NodeMacroInstance(node) => &node.base.base,
            EdGraphNodeObject::K2NodeMakeArray(node) => &node.base.base,
            EdGraphNodeObject::K2NodeMakeStruct(node) => &node.base.base,
            EdGraphNodeObject::K2NodeSelect(node) => &node.base.base,
            EdGraphNodeObject::K2NodeSelf(node) => &node.base.base,
            EdGraphNodeObject::K2NodeSpawnActorFromClass(node) => &node.base.base,
            EdGraphNodeObject::K2NodeSwitchEnum(node) => &node.base.base,
            EdGraphNodeObject::K2NodeTunnel(node) => &node.base.base,
            EdGraphNodeObject::K2NodeVariableGet(node) => &node.base.base,
            EdGraphNodeObject::K2NodeVariableSet(node) => &node.base.base,
            EdGraphNodeObject::UEdGraphNodeComment(node) => &node.base,
        }
    }

    /// Get a mutable reference to the base EdGraphNode
    pub fn as_ed_graph_node_mut(&mut self) -> &mut EdGraphNode {
        match self {
            EdGraphNodeObject::K2NodeAddComponentByClass(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeAddDelegate(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeBreakStruct(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeCallArrayFunction(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeCallFunction(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeCommutativeAssociativeBinaryOperator(node) => {
                &mut node.base.base
            }
            EdGraphNodeObject::K2NodeComponentBoundEvent(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeComposite(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeCreateWidget(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeCustomEvent(node) => &mut node.base.base.base,
            EdGraphNodeObject::K2NodeDynamicCast(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeEnumEquality(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeEnumInequality(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeEnumLiteral(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeEvent(node) => &mut node.base.base.base,
            EdGraphNodeObject::K2NodeExecutionSequence(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeFormatText(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeFunctionEntry(node) => &mut node.base.base.base,
            EdGraphNodeObject::K2NodeFunctionResult(node) => &mut node.base.base.base,
            EdGraphNodeObject::K2NodeGenericCreateObject(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeGetArrayItem(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeGetEnumeratorNameAsString(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeIfThenElse(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeInputAction(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeKnot(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeMacroInstance(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeMakeArray(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeMakeStruct(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeSelect(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeSelf(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeSpawnActorFromClass(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeSwitchEnum(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeTunnel(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeVariableGet(node) => &mut node.base.base,
            EdGraphNodeObject::K2NodeVariableSet(node) => &mut node.base.base,
            EdGraphNodeObject::UEdGraphNodeComment(node) => &mut node.base,
        }
    }

    /// Get the class path of this EdGraphNode object
    pub fn class(&self) -> &str {
        match self {
            EdGraphNodeObject::K2NodeAddComponentByClass(_) => {
                "/Script/BlueprintGraph.K2Node_AddComponentByClass"
            }
            EdGraphNodeObject::K2NodeAddDelegate(_) => "/Script/BlueprintGraph.K2Node_AddDelegate",
            EdGraphNodeObject::K2NodeBreakStruct(_) => "/Script/BlueprintGraph.K2Node_BreakStruct",
            EdGraphNodeObject::K2NodeCallArrayFunction(_) => {
                "/Script/BlueprintGraph.K2Node_CallArrayFunction"
            }
            EdGraphNodeObject::K2NodeCallFunction(_) => {
                "/Script/BlueprintGraph.K2Node_CallFunction"
            }
            EdGraphNodeObject::K2NodeCommutativeAssociativeBinaryOperator(_) => {
                "/Script/BlueprintGraph.K2Node_CommutativeAssociativeBinaryOperator"
            }
            EdGraphNodeObject::K2NodeComponentBoundEvent(_) => {
                "/Script/BlueprintGraph.K2Node_ComponentBoundEvent"
            }
            EdGraphNodeObject::K2NodeComposite(_) => "/Script/BlueprintGraph.K2Node_Composite",
            EdGraphNodeObject::K2NodeCreateWidget(_) => "/Script/UMGEditor.K2Node_CreateWidget",
            EdGraphNodeObject::K2NodeCustomEvent(_) => "/Script/BlueprintGraph.K2Node_CustomEvent",
            EdGraphNodeObject::K2NodeDynamicCast(_) => "/Script/BlueprintGraph.K2Node_DynamicCast",
            EdGraphNodeObject::K2NodeEnumEquality(_) => {
                "/Script/BlueprintGraph.K2Node_EnumEquality"
            }
            EdGraphNodeObject::K2NodeEnumInequality(_) => {
                "/Script/BlueprintGraph.K2Node_EnumInequality"
            }
            EdGraphNodeObject::K2NodeEnumLiteral(_) => "/Script/BlueprintGraph.K2Node_EnumLiteral",
            EdGraphNodeObject::K2NodeEvent(_) => "/Script/BlueprintGraph.K2Node_Event",
            EdGraphNodeObject::K2NodeExecutionSequence(_) => {
                "/Script/BlueprintGraph.K2Node_ExecutionSequence"
            }
            EdGraphNodeObject::K2NodeFormatText(_) => "/Script/BlueprintGraph.K2Node_FormatText",
            EdGraphNodeObject::K2NodeFunctionEntry(_) => {
                "/Script/BlueprintGraph.K2Node_FunctionEntry"
            }
            EdGraphNodeObject::K2NodeFunctionResult(_) => {
                "/Script/BlueprintGraph.K2Node_FunctionResult"
            }
            EdGraphNodeObject::K2NodeGenericCreateObject(_) => {
                "/Script/BlueprintGraph.K2Node_GenericCreateObject"
            }
            EdGraphNodeObject::K2NodeGetArrayItem(_) => {
                "/Script/BlueprintGraph.K2Node_GetArrayItem"
            }
            EdGraphNodeObject::K2NodeGetEnumeratorNameAsString(_) => {
                "/Script/BlueprintGraph.K2Node_GetEnumeratorNameAsString"
            }
            EdGraphNodeObject::K2NodeIfThenElse(_) => "/Script/BlueprintGraph.K2Node_IfThenElse",
            EdGraphNodeObject::K2NodeInputAction(_) => "/Script/BlueprintGraph.K2Node_InputAction",
            EdGraphNodeObject::K2NodeKnot(_) => "/Script/BlueprintGraph.K2Node_Knot",
            EdGraphNodeObject::K2NodeMacroInstance(_) => {
                "/Script/BlueprintGraph.K2Node_MacroInstance"
            }
            EdGraphNodeObject::K2NodeMakeArray(_) => "/Script/BlueprintGraph.K2Node_MakeArray",
            EdGraphNodeObject::K2NodeMakeStruct(_) => "/Script/BlueprintGraph.K2Node_MakeStruct",
            EdGraphNodeObject::K2NodeSelect(_) => "/Script/BlueprintGraph.K2Node_Select",
            EdGraphNodeObject::K2NodeSelf(_) => "/Script/BlueprintGraph.K2Node_Self",
            EdGraphNodeObject::K2NodeSpawnActorFromClass(_) => {
                "/Script/BlueprintGraph.K2Node_SpawnActorFromClass"
            }
            EdGraphNodeObject::K2NodeSwitchEnum(_) => "/Script/BlueprintGraph.K2Node_SwitchEnum",
            EdGraphNodeObject::K2NodeTunnel(_) => "/Script/BlueprintGraph.K2Node_Tunnel",
            EdGraphNodeObject::K2NodeVariableGet(_) => "/Script/BlueprintGraph.K2Node_VariableGet",
            EdGraphNodeObject::K2NodeVariableSet(_) => "/Script/BlueprintGraph.K2Node_VariableSet",
            EdGraphNodeObject::UEdGraphNodeComment(_) => "/Script/UnrealEd.EdGraphNode_Comment",
        }
    }
}

/// Represents a UObject - either an EdGraphNode or a UEdGraph
#[derive(Debug, Clone, PartialEq)]
pub enum UObject {
    EdGraphNode(EdGraphNodeObject),
    UEdGraph(UEdGraph),
}

impl UObject {
    /// Get the name of this object
    pub fn name(&self) -> &str {
        match self {
            UObject::EdGraphNode(node) => node.name(),
            UObject::UEdGraph(node) => &node.name,
        }
    }

    /// Get the class path of this object
    pub fn class(&self) -> &str {
        match self {
            UObject::EdGraphNode(node) => node.class(),
            UObject::UEdGraph(_) => "/Script/Engine.EdGraph",
        }
    }
}

impl From<EdGraphNodeObject> for UObject {
    fn from(node: EdGraphNodeObject) -> Self {
        UObject::EdGraphNode(node)
    }
}

/// The main paste buffer format which can be either a BPGraph or direct UObjects
#[derive(Debug, Clone, PartialEq)]
pub enum PasteBuffer {
    BPGraph(BPGraphClipboardData),
    UObjects(Vec<UObject>),
}

/// Parser for UE4 paste buffer format
pub struct Parser {
    input: String,
    pos: usize,
    line: usize,
    column: usize,
    current_object_class: Option<String>,
}

impl Parser {
    pub fn new(input: String) -> Self {
        Parser {
            input,
            pos: 0,
            line: 1,
            column: 1,
            current_object_class: None,
        }
    }

    /// Main entry point for parsing
    pub fn parse(&mut self) -> Result<PasteBuffer, ParseError> {
        self.skip_whitespace();

        if self.peek_str("BPGraph(") {
            Ok(PasteBuffer::BPGraph(self.parse_bpgraph()?))
        } else if self.peek_str("Begin Object") {
            Ok(PasteBuffer::UObjects(self.parse_objects()?))
        } else {
            Err(self.error("Unknown format: expected BPGraph or Begin Object".to_string()))
        }
    }

    fn parse_bpgraph(&mut self) -> Result<BPGraphClipboardData, ParseError> {
        self.expect_str("BPGraph(")?;

        let mut graph_name = None;
        let mut graph_type = None;
        let mut original_blueprint = None;
        let mut nodes = None;

        while !self.peek_str(")") && !self.is_eof() {
            self.skip_whitespace();

            if self.peek_str(")") {
                break;
            }

            let key = self.parse_identifier()?;
            self.skip_whitespace();
            self.expect_str("=")?;
            self.skip_whitespace();

            match key.as_str() {
                "GraphName" => {
                    graph_name = Some(self.parse_quoted_string()?);
                }
                "GraphType" => {
                    let gt = self.parse_identifier()?;
                    graph_type = Some(GraphType::from_str(&gt));
                }
                "OriginalBlueprint" => {
                    let bp_string = self.parse_blueprint_reference()?;
                    original_blueprint = Some(ExportTextPath::parse(&bp_string).map_err(|e| {
                        self.error(format!("Failed to parse OriginalBlueprint: {}", e))
                    })?);
                }
                "NodesString" => {
                    // Parse the quoted string containing serialized objects
                    let nodes_string = self.parse_quoted_string()?;
                    // Create a new parser for the nodes string content
                    let mut nodes_parser = Parser::new(nodes_string);
                    // Parse the objects within
                    nodes = Some(nodes_parser.parse_objects()?);
                }
                _ => {
                    return Err(self.error(format!("Unknown BPGraph key: {}", key)));
                }
            }

            self.skip_whitespace();
            if self.peek_str(",") {
                self.advance(1);
            }
        }

        self.expect_str(")")?;

        Ok(BPGraphClipboardData {
            graph_name: graph_name.ok_or_else(|| self.error("Missing GraphName".to_string()))?,
            graph_type: graph_type.ok_or_else(|| self.error("Missing GraphType".to_string()))?,
            original_blueprint,
            nodes: nodes.ok_or_else(|| self.error("Missing NodesString".to_string()))?,
        })
    }

    fn parse_objects(&mut self) -> Result<Vec<UObject>, ParseError> {
        let mut objects = Vec::new();

        while !self.is_eof() {
            self.skip_whitespace();
            if self.is_eof() {
                break;
            }
            if self.peek_str("Begin Object") {
                objects.push(self.parse_object()?);
            } else {
                break;
            }
        }

        Ok(objects)
    }

    fn parse_object(&mut self) -> Result<UObject, ParseError> {
        self.expect_str("Begin Object")?;
        self.skip_whitespace();

        let class = if self.peek_str("Class=") {
            self.expect_str("Class=")?;
            self.parse_class_path()?
        } else {
            String::new()
        };

        // Store current object class for error reporting
        self.current_object_class = Some(class.clone());

        self.skip_whitespace();

        let name = if self.peek_str("Name=") {
            self.expect_str("Name=")?;
            self.parse_quoted_string()?
        } else {
            String::new()
        };

        // Skip to end of line
        self.skip_to_newline();

        // Match on full class path for type dispatch
        let result = match class.as_str() {
            "/Script/BlueprintGraph.K2Node_AddComponentByClass" => {
                let node = self.parse_typed_object(K2NodeAddComponentByClass::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeAddComponentByClass(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_AddDelegate" => {
                let node = self.parse_typed_object(K2NodeAddDelegate::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeAddDelegate(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_BreakStruct" => {
                let node = self.parse_typed_object(K2NodeBreakStruct::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeBreakStruct(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_CallArrayFunction" => {
                let node = self.parse_typed_object(K2NodeCallArrayFunction::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeCallArrayFunction(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_CallFunction" => {
                let node = self.parse_typed_object(K2NodeCallFunction::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeCallFunction(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_CommutativeAssociativeBinaryOperator" => {
                let node = self.parse_typed_object(
                    K2NodeCommutativeAssociativeBinaryOperator::new(name.clone()),
                )?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeCommutativeAssociativeBinaryOperator(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_ComponentBoundEvent" => {
                let node = self.parse_typed_object(K2NodeComponentBoundEvent::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeComponentBoundEvent(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_Composite" => {
                let node = self.parse_typed_object(K2NodeComposite::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeComposite(
                    node,
                )))
            }
            "/Script/UMGEditor.K2Node_CreateWidget" => {
                let node = self.parse_typed_object(K2NodeCreateWidget::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeCreateWidget(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_CustomEvent" => {
                let node = self.parse_typed_object(K2NodeCustomEvent::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeCustomEvent(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_DynamicCast" => {
                let node = self.parse_typed_object(K2NodeDynamicCast::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeDynamicCast(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_EnumEquality" => {
                let node = self.parse_typed_object(K2NodeEnumEquality::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeEnumEquality(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_EnumInequality" => {
                let node = self.parse_typed_object(K2NodeEnumInequality::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeEnumInequality(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_EnumLiteral" => {
                let node = self.parse_typed_object(K2NodeEnumLiteral::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeEnumLiteral(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_Event" => {
                let node = self.parse_typed_object(K2NodeEvent::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeEvent(node)))
            }
            "/Script/BlueprintGraph.K2Node_ExecutionSequence" => {
                let node = self.parse_typed_object(K2NodeExecutionSequence::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeExecutionSequence(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_FormatText" => {
                let node = self.parse_typed_object(K2NodeFormatText::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeFormatText(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_FunctionEntry" => {
                let node = self.parse_typed_object(K2NodeFunctionEntry::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeFunctionEntry(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_FunctionResult" => {
                let node = self.parse_typed_object(K2NodeFunctionResult::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeFunctionResult(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_GenericCreateObject" => {
                let node = self.parse_typed_object(K2NodeGenericCreateObject::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeGenericCreateObject(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_GetArrayItem" => {
                let node = self.parse_typed_object(K2NodeGetArrayItem::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeGetArrayItem(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_GetEnumeratorNameAsString" => {
                let node =
                    self.parse_typed_object(K2NodeGetEnumeratorNameAsString::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeGetEnumeratorNameAsString(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_IfThenElse" => {
                let node = self.parse_typed_object(K2NodeIfThenElse::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeIfThenElse(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_InputAction" => {
                let node = self.parse_typed_object(K2NodeInputAction::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeInputAction(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_Knot" => {
                let node = self.parse_typed_object(K2NodeKnot::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeKnot(node)))
            }
            "/Script/BlueprintGraph.K2Node_MacroInstance" => {
                let node = self.parse_typed_object(K2NodeMacroInstance::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeMacroInstance(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_MakeArray" => {
                let node = self.parse_typed_object(K2NodeMakeArray::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeMakeArray(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_MakeStruct" => {
                let node = self.parse_typed_object(K2NodeMakeStruct::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeMakeStruct(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_Select" => {
                let node = self.parse_typed_object(K2NodeSelect::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeSelect(node)))
            }
            "/Script/BlueprintGraph.K2Node_Self" => {
                let node = self.parse_typed_object(K2NodeSelf::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeSelf(node)))
            }
            "/Script/BlueprintGraph.K2Node_SpawnActorFromClass" => {
                let node = self.parse_typed_object(K2NodeSpawnActorFromClass::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::K2NodeSpawnActorFromClass(node),
                ))
            }
            "/Script/BlueprintGraph.K2Node_SwitchEnum" => {
                let node = self.parse_typed_object(K2NodeSwitchEnum::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeSwitchEnum(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_Tunnel" => {
                let node = self.parse_typed_object(K2NodeTunnel::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeTunnel(node)))
            }
            "/Script/BlueprintGraph.K2Node_VariableGet" => {
                let node = self.parse_typed_object(K2NodeVariableGet::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeVariableGet(
                    node,
                )))
            }
            "/Script/BlueprintGraph.K2Node_VariableSet" => {
                let node = self.parse_typed_object(K2NodeVariableSet::new(name.clone()))?;
                Ok(UObject::EdGraphNode(EdGraphNodeObject::K2NodeVariableSet(
                    node,
                )))
            }
            "/Script/Engine.EdGraph" => {
                let node = self.parse_typed_object(UEdGraph::new(name.clone()))?;
                Ok(UObject::UEdGraph(node))
            }
            "/Script/UnrealEd.EdGraphNode_Comment" => {
                let node = self.parse_typed_object(UEdGraphNodeComment::new(name.clone()))?;
                Ok(UObject::EdGraphNode(
                    EdGraphNodeObject::UEdGraphNodeComment(node),
                ))
            }
            _ => Err(ParseError::new(
                format!("Unknown object class: {}", class),
                self.line,
                self.column,
            )),
        };

        // Clear current object class after parsing
        self.current_object_class = None;

        result
    }

    fn parse_typed_object<T: ObjectTrait>(&mut self, mut node: T) -> Result<T, ParseError> {
        loop {
            self.skip_whitespace();

            if self.peek_str("End Object") {
                self.expect_str("End Object")?;
                self.skip_to_newline();
                break;
            }

            if self.peek_str("CustomProperties") {
                self.expect_str("CustomProperties")?;
                self.skip_whitespace();

                // Track position to detect if custom property was handled
                let pos_before = self.pos;
                node.parse_custom_property(self)?;

                // If position hasn't changed, the custom property wasn't handled
                if self.pos == pos_before {
                    return Err(self.error(
                        "Unhandled CustomProperties - typed node must consume input or return error".to_string()
                    ));
                }
            } else {
                let (key, index) = self.parse_property_name()?;
                self.skip_whitespace();
                if self.peek_str("=") {
                    self.advance(1);
                    self.skip_whitespace();

                    node.parse_property((&key, index), self)?;
                    self.skip_to_newline();
                } else {
                    self.skip_to_newline();
                }
            }
        }

        Ok(node)
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let start = self.pos;
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch.is_alphanumeric() || ch == '_' {
                self.advance(1);
            } else {
                break;
            }
        }

        if self.pos == start {
            return Err(self.error("Expected identifier".to_string()));
        }

        Ok(self.input[start..self.pos].to_string())
    }

    fn parse_property_name(&mut self) -> Result<(String, Option<usize>), ParseError> {
        let start = self.pos;

        // Parse the base property name (alphanumeric, underscore, space, dot)
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch.is_alphanumeric() || ch == '_' || ch == ' ' || ch == '.' {
                self.advance(1);
            } else {
                break;
            }
        }

        let base_name = self.input[start..self.pos].trim().to_string();

        // Check for array index: PropertyName(0)
        if self.peek_str("(") {
            let paren_start = self.pos;
            self.advance(1); // consume '('

            // Try to parse integer index
            let index_start = self.pos;
            while !self.is_eof() && self.peek_char().is_ascii_digit() {
                self.advance(1);
            }

            if self.pos > index_start && self.peek_str(")") {
                // Successfully found array index
                let index_str = &self.input[index_start..self.pos];
                if let Ok(index) = index_str.parse::<usize>() {
                    self.advance(1); // consume ')'
                    return Ok((base_name, Some(index)));
                }
            }

            // Not an array index, reset and treat as part of name
            self.pos = paren_start;
            self.advance(1); // consume '('

            // Continue until we find the closing paren
            while !self.is_eof() && self.peek_char() != ')' {
                self.advance(1);
            }
            if !self.is_eof() {
                self.advance(1); // consume ')'
            }

            return Ok((self.input[start..self.pos].trim().to_string(), None));
        }

        Ok((base_name, None))
    }

    fn parse_simple_value(&mut self) -> Result<String, ParseError> {
        let start = self.pos;
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch == '\n' || ch == '\r' || ch == ',' || ch == ')' {
                break;
            }
            self.advance(1);
        }
        Ok(self.input[start..self.pos].trim().to_string())
    }

    fn parse_parenthesized_value(&mut self) -> Result<String, ParseError> {
        let start = self.pos;
        let mut depth = 0;

        while !self.is_eof() {
            let ch = self.peek_char();
            if ch == '(' {
                depth += 1;
                self.advance(1);
            } else if ch == ')' {
                self.advance(1);
                depth -= 1;
                if depth == 0 {
                    break;
                }
            } else {
                self.advance(1);
            }
        }

        Ok(self.input[start..self.pos].to_string())
    }

    fn parse_quoted_string(&mut self) -> Result<String, ParseError> {
        self.expect_str("\"")?;

        let mut result = String::new();
        let mut escaped = false;

        while !self.is_eof() {
            let ch = self.peek_char();

            if escaped {
                match ch {
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    '\'' => result.push('\''),
                    _ => {
                        result.push('\\');
                        result.push(ch);
                    }
                }
                escaped = false;
                self.advance(1);
            } else if ch == '\\' {
                escaped = true;
                self.advance(1);
            } else if ch == '"' {
                self.advance(1);
                break;
            } else {
                result.push(ch);
                self.advance(1);
            }
        }

        Ok(result)
    }

    fn parse_blueprint_reference(&mut self) -> Result<String, ParseError> {
        // Check if it starts with a blueprint type (Blueprint, WidgetBlueprint, etc.)
        let start = self.pos;

        // Parse the blueprint type identifier
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch.is_alphanumeric() || ch == '_' {
                self.advance(1);
            } else {
                break;
            }
        }

        if self.pos > start && self.peek_str("'") {
            let blueprint_type = self.input[start..self.pos].to_string();
            self.advance(1); // consume '

            // Read everything between the single quotes, preserving any inner quotes
            let path_start = self.pos;
            while !self.is_eof() && self.peek_char() != '\'' {
                self.advance(1);
            }
            let path = self.input[path_start..self.pos].to_string();

            if self.peek_str("'") {
                self.advance(1); // consume closing '
            }
            Ok(format!("{}'{}'", blueprint_type, path))
        } else {
            // Reset position if we didn't find a blueprint reference pattern
            self.pos = start;
            self.parse_quoted_string()
        }
    }

    fn parse_class_path(&mut self) -> Result<String, ParseError> {
        let start = self.pos;
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch == '\n' || ch == '\r' || (ch == ' ' && self.peek_str(" Name=")) {
                break;
            }
            self.advance(1);
        }
        Ok(self.input[start..self.pos].trim().to_string())
    }

    fn peek_str(&self, s: &str) -> bool {
        self.input[self.pos..].starts_with(s)
    }

    fn expect_str(&mut self, s: &str) -> Result<(), ParseError> {
        if self.peek_str(s) {
            self.advance(s.len());
            Ok(())
        } else {
            let preview = &self.input[self.pos..self.pos.min(self.pos + 50)];
            Err(self.error(format!("Expected '{}', found '{}'", s, preview)))
        }
    }

    fn peek_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap_or('\0')
    }

    fn advance(&mut self, count: usize) {
        let end_pos = (self.pos + count).min(self.input.len());

        // Update line and column based on characters we're advancing over
        for ch in self.input[self.pos..end_pos].chars() {
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }

        self.pos = end_pos;
    }

    fn skip_whitespace(&mut self) {
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                self.advance(1);
            } else {
                break;
            }
        }
    }

    fn skip_to_newline(&mut self) {
        while !self.is_eof() {
            let ch = self.peek_char();
            if ch == '\n' {
                self.advance(1);
                break;
            }
            self.advance(1);
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    /// Create a parse error at the current position with context
    pub fn error(&self, message: String) -> ParseError {
        let context = self.get_current_line_context();
        let full_message = if let Some(ref class) = self.current_object_class {
            format!("[{}] {}", class, message)
        } else {
            message
        };
        ParseError::with_context(full_message, self.line, self.column, context)
    }

    /// Get the current line as context for error messages
    fn get_current_line_context(&self) -> String {
        let line_start = self.input[..self.pos]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);

        let line_end = self.input[self.pos..]
            .find('\n')
            .map(|i| self.pos + i)
            .unwrap_or(self.input.len());

        self.input[line_start..line_end].to_string()
    }

    // Primitive parsing helpers for TypedNode implementations

    /// Parse an integer value from the current stream position
    pub fn parse_int<T: std::str::FromStr>(&mut self) -> Result<T, ParseError>
    where
        T::Err: std::fmt::Display,
    {
        self.skip_whitespace();
        let value_str = self.parse_simple_value()?;
        value_str
            .parse()
            .map_err(|e| self.error(format!("Failed to parse integer '{}': {}", value_str, e)))
    }

    /// Parse a boolean value from the current stream position (handles "True"/"False")
    pub fn parse_bool(&mut self) -> Result<bool, ParseError> {
        self.skip_whitespace();
        let value_str = self.parse_simple_value()?;
        match value_str.as_str() {
            "True" => Ok(true),
            "False" => Ok(false),
            _ => Err(self.error(format!(
                "Invalid boolean value: '{}' (expected 'True' or 'False')",
                value_str
            ))),
        }
    }

    /// Parse a string value from the current stream position
    pub fn parse_string(&mut self) -> Result<String, ParseError> {
        self.skip_whitespace();
        if self.peek_str("\"") {
            self.parse_quoted_string()
        } else {
            self.parse_simple_value()
        }
    }

    /// Parse an ExportTextPath (e.g., Blueprint'"/Game/MyBlueprint.MyBlueprint"')
    pub fn parse_export_text_path(&mut self) -> Result<nodes::ExportTextPath, ParseError> {
        let s = self.parse_string()?;
        nodes::ExportTextPath::parse(&s)
            .map_err(|e| self.error(format!("Failed to parse ExportTextPath: {}", e)))
    }

    /// Parse an FText value (can be quoted string or macro like NSLOCTEXT(...))
    pub fn parse_text(&mut self) -> Result<nodes::FText, ParseError> {
        self.skip_whitespace();

        let text_str = if self.peek_str("\"") {
            self.parse_quoted_string()?
        } else {
            // Check if this looks like a macro/function call (e.g., NSLOCTEXT(...))
            let start = self.pos;
            while !self.is_eof() {
                let ch = self.peek_char();
                if ch.is_alphanumeric() || ch == '_' {
                    self.advance(1);
                } else {
                    break;
                }
            }

            // If we found an identifier followed by '(', parse it as a function call
            if self.pos > start && self.peek_str("(") {
                let func_name = self.input[start..self.pos].to_string();
                let args = self.parse_parenthesized_value()?;
                format!("{}{}", func_name, args)
            } else {
                // Reset and parse as simple value (could be empty)
                self.pos = start;
                self.parse_simple_value()?
            }
        };

        Ok(nodes::FText::new(text_str))
    }

    /// Parse a GUID value from the current stream position
    /// Expected format: 32-character hex string like "5AC77268675F4782872317D596816038"
    pub fn parse_guid(&mut self) -> Result<nodes::Guid, ParseError> {
        let value_str = self.parse_string()?;
        nodes::Guid::parse_hex(&value_str).map_err(|e| self.error(e))
    }

    /// Parse a MemberReference from the current stream position
    /// Expected format: (MemberParent=...,MemberName="...",MemberGuid=...)
    pub fn parse_member_reference(&mut self) -> Result<nodes::MemberReference, ParseError> {
        // Handle empty member reference: ()
        if self.peek_str("()") {
            self.advance(2);
            return Ok(nodes::MemberReference::default());
        }

        self.expect_str("(")?;

        let mut member_ref = nodes::MemberReference::default();

        loop {
            self.skip_whitespace();

            if self.peek_str(")") {
                self.advance(1);
                break;
            }

            // Parse property name
            let (key, _index) = self.parse_property_name()?;
            self.skip_whitespace();
            self.expect_str("=")?;
            self.skip_whitespace();

            // Parse property value based on key
            match key.as_str() {
                "MemberParent" => {
                    member_ref.member_parent = Some(self.parse_export_text_path()?);
                }
                "MemberScope" => {
                    member_ref.member_scope = Some(self.parse_string()?);
                }
                "MemberName" => {
                    member_ref.member_name = Some(self.parse_string()?);
                }
                "MemberGuid" => {
                    member_ref.member_guid = Some(self.parse_string()?);
                }
                "bSelfContext" => {
                    member_ref.self_context = self.parse_bool()?;
                }
                "bWasDeprecated" => {
                    member_ref.was_deprecated = self.parse_bool()?;
                }
                _ => {
                    return Err(self.error(format!("Unknown MemberReference property: {}", key)));
                }
            }

            self.skip_whitespace();
            if self.peek_str(",") {
                self.advance(1);
            }
        }

        Ok(member_ref)
    }

    /// Parse a Pin from CustomProperties Pin (...) format
    pub fn parse_pin(&mut self) -> Result<nodes::Pin, ParseError> {
        nodes::parse_pin(self)
    }

    /// Parse a PinType from a parenthesized struct format
    /// Expected format: (PinCategory="...",PinSubCategoryObject=...,ContainerType=...)
    pub fn parse_pin_type(&mut self) -> Result<nodes::PinType, ParseError> {
        nodes::parse_pin_type(self)
    }

    /// Parse a FGraphReference from a parenthesized struct format
    /// Expected format: (MacroGraph=EdGraph'"Name"',GraphBlueprint=WidgetBlueprint'"/Path"',GraphGuid=GUID)
    pub fn parse_graph_reference(&mut self) -> Result<nodes::FGraphReference, ParseError> {
        // Handle empty graph reference: ()
        if self.peek_str("()") {
            self.advance(2);
            return Ok(nodes::FGraphReference::default());
        }

        self.expect_str("(")?;

        let mut graph_ref = nodes::FGraphReference::default();

        loop {
            self.skip_whitespace();

            if self.peek_str(")") {
                self.advance(1);
                break;
            }

            // Parse property name
            let (key, _index) = self.parse_property_name()?;
            self.skip_whitespace();
            self.expect_str("=")?;
            self.skip_whitespace();

            // Parse property value based on key
            match key.as_str() {
                "MacroGraph" => {
                    let val = self.parse_string()?;
                    graph_ref.macro_graph = if val == "None" || val.is_empty() {
                        None
                    } else {
                        Some(nodes::ExportTextPath::parse(&val).map_err(|e| self.error(e))?)
                    };
                }
                "GraphBlueprint" => {
                    let val = self.parse_blueprint_reference()?;
                    graph_ref.graph_blueprint = if val == "None" || val.is_empty() {
                        None
                    } else {
                        Some(nodes::ExportTextPath::parse(&val).map_err(|e| self.error(e))?)
                    };
                }
                "GraphGuid" => {
                    graph_ref.graph_guid = self.parse_guid()?;
                }
                _ => {
                    return Err(self.error(format!("Unknown FGraphReference property: {}", key)));
                }
            }

            self.skip_whitespace();
            if self.peek_str(",") {
                self.advance(1);
            }
        }

        Ok(graph_ref)
    }
}

/// Serializer for UE4 paste buffer format
pub struct Serializer {
    indent_level: usize,
}

impl Serializer {
    pub fn new() -> Self {
        Serializer { indent_level: 0 }
    }

    pub fn serialize(&mut self, buffer: &PasteBuffer) -> String {
        match buffer {
            PasteBuffer::BPGraph(data) => self.serialize_bpgraph(data),
            PasteBuffer::UObjects(objects) => self.serialize_objects(objects),
        }
    }

    fn serialize_bpgraph(&mut self, data: &BPGraphClipboardData) -> String {
        let mut result = String::from("BPGraph(");

        result.push_str(&format!(
            "GraphName=\"{}\"",
            nodes::escape_string(&data.graph_name)
        ));
        result.push_str(&format!(",GraphType={}", data.graph_type.to_str()));

        if let Some(ref bp) = data.original_blueprint {
            // Blueprint references are in format: Blueprint'"path"' or WidgetBlueprint'"path"'
            // They should not be escaped, just passed through as-is
            result.push_str(&format!(",OriginalBlueprint={}", bp.to_file_string()));
        }

        // Serialize the nodes objects to a string
        let nodes_string = self.serialize_objects(&data.nodes);
        result.push_str(&format!(
            ",NodesString=\"{}\"",
            nodes::escape_string(&nodes_string)
        ));
        result.push_str(")\r\n");

        result
    }

    fn serialize_objects(&mut self, objects: &[UObject]) -> String {
        let mut result = String::new();

        for obj in objects {
            result.push_str(&self.serialize_object(obj));
        }

        result
    }

    fn serialize_object(&mut self, obj: &UObject) -> String {
        match obj {
            UObject::EdGraphNode(node) => self.serialize_ed_graph_node_object(node),
            UObject::UEdGraph(node) => self.serialize_typed_node(obj.class(), obj.name(), node),
        }
    }

    fn serialize_ed_graph_node_object(&mut self, obj: &EdGraphNodeObject) -> String {
        match obj {
            EdGraphNodeObject::K2NodeAddComponentByClass(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeAddDelegate(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeBreakStruct(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeCallArrayFunction(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeCallFunction(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeCommutativeAssociativeBinaryOperator(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeComponentBoundEvent(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeComposite(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeCreateWidget(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeCustomEvent(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeDynamicCast(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeEnumEquality(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeEnumInequality(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeEnumLiteral(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeEvent(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeExecutionSequence(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeFormatText(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeFunctionEntry(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeFunctionResult(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeGenericCreateObject(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeGetArrayItem(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeGetEnumeratorNameAsString(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeIfThenElse(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeInputAction(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeKnot(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeMacroInstance(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeMakeArray(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeMakeStruct(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeSelect(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeSelf(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeSpawnActorFromClass(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeSwitchEnum(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeTunnel(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeVariableGet(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::K2NodeVariableSet(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
            EdGraphNodeObject::UEdGraphNodeComment(node) => {
                self.serialize_typed_node(obj.class(), obj.name(), node)
            }
        }
    }

    /// Serialize a typed node with Begin Object/End Object wrapper
    fn serialize_typed_node<T: ObjectTrait>(
        &mut self,
        class: &str,
        name: &str,
        node: &T,
    ) -> String {
        let mut result = String::new();
        let indent = "   ".repeat(self.indent_level);
        let prop_indent = "   ".repeat(self.indent_level + 1);

        result.push_str(&format!(
            "{}Begin Object Class={} Name=\"{}\"\r\n",
            indent, class, name
        ));

        node.serialize(&mut result, &prop_indent);
        node.serialize_custom_properties(&mut result, &prop_indent);

        result.push_str(&format!("{}End Object\r\n", indent));

        result
    }
}

impl Default for Serializer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_object() {
        let input = r#"Begin Object Class=/Script/UMG.VerticalBox Name="Panel"
   Slot=TestValue
End Object
"#;

        let mut parser = Parser::new(input.to_string());
        let result = parser.parse();

        // Should return an error for unknown class
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("Unknown object class"));
    }

    #[test]
    fn test_parse_error_line_and_column() {
        let input = "Invalid input here\nSecond line\nThird line";
        let mut parser = Parser::new(input.to_string());
        let result = parser.parse();

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 1);
        assert!(err.message.contains("Unknown format"));
    }

    #[test]
    fn test_parse_error_with_context() {
        let input = "BPGraph(InvalidKey=\"value\")";
        let mut parser = Parser::new(input.to_string());
        let result = parser.parse();

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.line > 0);
        assert!(err.column > 0);
    }

    #[test]
    fn test_parse_error_tracks_position() {
        // Test that errors on later lines have correct line numbers
        let input =
            "Begin Object Class=/Script/Test.Test Name=\"Test\"\nInvalidProperty\nEnd Object";
        let mut parser = Parser::new(input.to_string());
        let result = parser.parse();

        // This should parse successfully or fail at a specific position
        // The key is that any error should have valid line/column info
        if let Err(err) = result {
            assert!(err.line > 0);
            assert!(err.column > 0);
        }
    }

    #[test]
    fn test_error_display_format() {
        let err = ParseError::new("Test error".to_string(), 5, 10);
        let display = format!("{}", err);
        assert!(display.contains("5:10"));
        assert!(display.contains("Test error"));
    }

    #[test]
    fn test_error_with_context_display() {
        let err = ParseError::with_context(
            "Expected identifier".to_string(),
            2,
            15,
            "   SomeProperty = Value".to_string(),
        );
        let display = format!("{}", err);
        assert!(display.contains("2:15"));
        assert!(display.contains("Expected identifier"));
        assert!(display.contains("SomeProperty"));
        assert!(display.contains("^")); // Should show a caret pointing to the error
    }
}
