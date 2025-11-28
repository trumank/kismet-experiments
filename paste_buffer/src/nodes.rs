use crate::{ParseError, Parser};

macro_rules! impl_deref {
    ($type:ty => $target:ty) => {
        impl std::ops::Deref for $type {
            type Target = $target;

            fn deref(&self) -> &Self::Target {
                &self.base
            }
        }

        impl std::ops::DerefMut for $type {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.base
            }
        }
    };
}

pub trait ObjectTrait {
    /// Parse a property by key using the parser to read the value from the stream
    /// The key is a tuple of (property_name, optional_array_index)
    /// Properties not expecting arrays should match on (name, None)
    /// Properties expecting arrays should match on (name, Some(index))
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError>;

    /// Parse a custom property (e.g., "CustomProperties Pin (...)")
    /// The parser is positioned after "CustomProperties " when this is called
    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError>;

    /// Serialize this node's properties to UE4 paste buffer format
    /// Appends only the property lines (not Begin Object/End Object) to the buffer
    fn serialize(&self, buffer: &mut String, indent: &str);

    /// Serialize custom properties (e.g., "CustomProperties Pin (...)")
    /// Appends CustomProperties lines to the buffer
    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str);
}

/// FLinearColor representation for RGBA color values
/// Format: (R=0.318365,G=0.418298,B=1.000000,A=1.000000)
#[derive(Debug, Clone, PartialEq)]
pub struct FLinearColor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl FLinearColor {
    /// Parse from format: (R=0.318365,G=0.418298,B=1.000000,A=1.000000)
    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim();
        if !s.starts_with('(') || !s.ends_with(')') {
            return Err(format!("Invalid FLinearColor format: {}", s));
        }

        let inner = &s[1..s.len() - 1];
        let mut r = 0.0;
        let mut g = 0.0;
        let mut b = 0.0;
        let mut a = 1.0;

        for part in inner.split(',') {
            let part = part.trim();
            if let Some(eq_pos) = part.find('=') {
                let key = &part[..eq_pos];
                let value = &part[eq_pos + 1..];
                let parsed_value = value
                    .parse::<f32>()
                    .map_err(|e| format!("Failed to parse float '{}': {}", value, e))?;

                match key {
                    "R" => r = parsed_value,
                    "G" => g = parsed_value,
                    "B" => b = parsed_value,
                    "A" => a = parsed_value,
                    _ => return Err(format!("Unknown FLinearColor component: {}", key)),
                }
            }
        }

        Ok(FLinearColor { r, g, b, a })
    }

    /// Serialize to UE4 format with exactly 6 decimal places
    pub fn serialize(&self) -> String {
        format!(
            "(R={:.6},G={:.6},B={:.6},A={:.6})",
            self.r, self.g, self.b, self.a
        )
    }
}

impl std::fmt::Display for FLinearColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.serialize())
    }
}

/// FText representation for localized text
/// Unreal Engine FText can be a simple quoted string or a localization macro like NSLOCTEXT(...)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FText {
    /// A literal string value
    Literal(String),
    /// A macro call like NSLOCTEXT("namespace", "key", "value")
    Macro(String),
}

impl FText {
    /// Create a new FText from a string, auto-detecting if it's a macro or literal
    pub fn new(text: String) -> Self {
        // Check if it looks like a macro call (contains '(' and doesn't start with quote)
        if text.contains('(') && !text.starts_with('"') {
            FText::Macro(text)
        } else {
            FText::Literal(text)
        }
    }

    /// Create a literal FText
    pub fn literal(text: String) -> Self {
        FText::Literal(text)
    }

    /// Create a macro FText
    pub fn macro_call(text: String) -> Self {
        FText::Macro(text)
    }

    /// Get the string representation (returns the content regardless of variant)
    pub fn as_str(&self) -> &str {
        match self {
            FText::Literal(s) => s,
            FText::Macro(s) => s,
        }
    }

    /// Check if this FText is empty
    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    /// Convert to owned String
    pub fn into_string(self) -> String {
        match self {
            FText::Literal(s) => s,
            FText::Macro(s) => s,
        }
    }

    /// Serialize to UE4 format
    pub fn serialize(&self) -> String {
        match self {
            FText::Literal(s) => {
                if s.is_empty() {
                    String::new()
                } else {
                    format!("\"{}\"", escape_string(s))
                }
            }
            FText::Macro(s) => s.clone(),
        }
    }
}

impl std::fmt::Display for FText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Pin identifier referencing another pin (node name + GUID)
/// Format: "NodeName GUID"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PinIdentifier {
    pub node_name: String,
    pub pin_guid: Guid,
}

impl PinIdentifier {
    /// Parse from format: "NodeName GUID"
    pub fn parse(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.split_whitespace().collect();
        if parts.len() != 2 {
            return Err(format!("Invalid PinIdentifier format: {}", s));
        }

        let node_name = parts[0].to_string();
        let pin_guid = Guid::parse_hex(parts[1])?;

        Ok(PinIdentifier {
            node_name,
            pin_guid,
        })
    }
}

impl std::fmt::Display for PinIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.node_name, self.pin_guid)
    }
}

/// Pin direction enum
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EEdGraphPinDirection {
    Input,
    Output,
}

impl EEdGraphPinDirection {
    /// Parse from Unreal Engine string format (e.g., "EGPD_Output", "EGPD_Input")
    pub fn parse(s: &str) -> Result<Self, String> {
        match s {
            "EGPD_Input" => Ok(EEdGraphPinDirection::Input),
            "EGPD_Output" => Ok(EEdGraphPinDirection::Output),
            _ => Err(format!("Unknown pin direction: {}", s)),
        }
    }

    /// Convert to Unreal Engine string format
    pub fn as_str(&self) -> &'static str {
        match self {
            EEdGraphPinDirection::Input => "EGPD_Input",
            EEdGraphPinDirection::Output => "EGPD_Output",
        }
    }
}

impl std::fmt::Display for EEdGraphPinDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Advanced pin display mode for nodes
/// From ENodeAdvancedPins in UE4
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ENodeAdvancedPins {
    #[default]
    NoPins,
    Shown,
    Hidden,
}

impl std::str::FromStr for ENodeAdvancedPins {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "NoPins" => Ok(ENodeAdvancedPins::NoPins),
            "Shown" => Ok(ENodeAdvancedPins::Shown),
            "Hidden" => Ok(ENodeAdvancedPins::Hidden),
            _ => Err(format!("Unknown ENodeAdvancedPins: {}", s)),
        }
    }
}

impl ENodeAdvancedPins {
    pub fn to_str(&self) -> &'static str {
        match self {
            ENodeAdvancedPins::NoPins => "NoPins",
            ENodeAdvancedPins::Shown => "Shown",
            ENodeAdvancedPins::Hidden => "Hidden",
        }
    }
}

impl std::fmt::Display for ENodeAdvancedPins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Guid([u8; 16]);

impl Guid {
    pub const fn from_bytes(bytes: [u8; 16]) -> Self {
        Guid(bytes)
    }

    pub const fn zero() -> Self {
        Guid([0; 16])
    }

    pub fn is_zero(&self) -> bool {
        self.0 == [0; 16]
    }

    pub fn as_bytes(&self) -> &[u8; 16] {
        &self.0
    }

    /// Parse from hex string: "5AC77268675F4782872317D596816038"
    pub fn parse_hex(s: &str) -> Result<Self, String> {
        let s = s.trim();

        if s.len() != 32 {
            return Err(format!(
                "GUID must be exactly 32 hex characters, got {}",
                s.len()
            ));
        }

        let mut bytes = [0u8; 16];
        for i in 0..16 {
            let hex_pair = &s[i * 2..i * 2 + 2];
            bytes[i] = u8::from_str_radix(hex_pair, 16)
                .map_err(|e| format!("Failed to parse hex pair '{}': {}", hex_pair, e))?;
        }

        Ok(Guid(bytes))
    }

    /// Format as hex string: "5AC77268675F4782872317D596816038"
    pub fn to_hex_string(&self) -> String {
        self.0.iter().map(|b| format!("{:02X}", b)).collect()
    }
}

impl Default for Guid {
    fn default() -> Self {
        Self::zero()
    }
}

impl std::fmt::Debug for Guid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Guid(\"{}\")", self.to_hex_string())
    }
}

impl std::fmt::Display for Guid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_hex_string())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ExportTextPath {
    class_name: String,
    object_path: String,
}

impl ExportTextPath {
    pub fn new(class_name: impl Into<String>, object_path: impl Into<String>) -> Self {
        Self {
            class_name: class_name.into(),
            object_path: object_path.into(),
        }
    }

    /// Parse an ExportTextPath from either format:
    /// - `ClassName'ObjectPath'`
    /// - `ClassName'"ObjectPath"'`
    ///
    /// # Errors
    ///
    /// Returns an error if the input is malformed or missing quotes.
    pub fn parse(s: &str) -> Result<Self, String> {
        let (class_name, rest) = s
            .split_once('\'')
            .ok_or_else(|| format!("ExportTextPath missing opening quote: {}", s))?;

        let path_content = rest
            .strip_suffix('\'')
            .ok_or_else(|| format!("ExportTextPath missing closing quote: {}", s))?;

        // Handle both ClassName'ObjectPath' and ClassName'"ObjectPath"'
        // Strip inner quotes if present
        let object_path = path_content
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(path_content);

        Ok(Self {
            class_name: class_name.to_string(),
            object_path: object_path.to_string(),
        })
    }

    /// Creates a Blueprint reference: `Blueprint'"/path"'`
    pub fn blueprint(path: impl Into<String>) -> Self {
        Self {
            class_name: "Blueprint".to_string(),
            object_path: path.into(),
        }
    }

    /// Creates a WidgetBlueprint reference: `WidgetBlueprint'"/path"'`
    pub fn widget_blueprint(path: impl Into<String>) -> Self {
        Self {
            class_name: "WidgetBlueprint".to_string(),
            object_path: path.into(),
        }
    }

    /// Creates a Class reference: `Class'"/path"'`
    pub fn class(path: impl Into<String>) -> Self {
        Self {
            class_name: "Class".to_string(),
            object_path: path.into(),
        }
    }

    /// Returns the class name (e.g., "Blueprint", "Class").
    pub fn class_name(&self) -> &str {
        &self.class_name
    }

    /// Returns the object path (e.g., "/Game/MyBlueprint.MyBlueprint").
    pub fn object_path(&self) -> &str {
        &self.object_path
    }

    /// Serialize to the format: `ClassName'ObjectPath'` (without inner quotes)
    pub fn to_storage_string(&self) -> String {
        format!("{}'{}'", self.class_name, self.object_path)
    }

    /// Serialize to the format: `ClassName'"ObjectPath"'` (with inner quotes for file output)
    pub fn to_file_string(&self) -> String {
        format!("{}'\"{}\"'", self.class_name, self.object_path)
    }

    /// Returns true if both the class name and object path are non-empty.
    pub fn is_empty(&self) -> bool {
        self.class_name.is_empty() || self.object_path.is_empty()
    }
}

impl std::fmt::Debug for ExportTextPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Format as ExportTextPath { ClassName: "path" } for clarity
        write!(
            f,
            "ExportTextPath {{ {}: \"{}\" }}",
            self.class_name, self.object_path
        )
    }
}

impl std::fmt::Display for ExportTextPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}'\"{}\"'", self.class_name, self.object_path)
    }
}

impl std::str::FromStr for ExportTextPath {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

/// EdGraphNode base structure - base class for all editor graph nodes
/// From UEdGraphNode in UE4
#[derive(Debug, Clone, PartialEq)]
pub struct EdGraphNode {
    pub name: String,
    pub pins: Vec<Pin>,
    pub node_pos_x: i32,
    pub node_pos_y: i32,
    pub node_width: i32,
    pub node_height: i32,
    pub advanced_pin_display: ENodeAdvancedPins,
    pub display_as_disabled: bool,
    pub user_set_enabled_state: bool,
    pub is_node_enabled: bool,
    pub can_resize_node: bool,
    pub has_compiler_message: bool,
    pub comment_bubble_pinned: bool,
    pub comment_bubble_visible: bool,
    pub comment_bubble_make_visible: bool,
    pub can_rename_node: bool,
    pub node_upgrade_message: FText,
    pub node_comment: String,
    pub error_type: i32,
    pub error_msg: String,
    pub node_guid: Guid,
}

impl EdGraphNode {
    pub fn new(name: String) -> Self {
        Self {
            name,
            pins: Vec::new(),
            node_pos_x: 0,
            node_pos_y: 0,
            node_width: 0,
            node_height: 0,
            advanced_pin_display: ENodeAdvancedPins::NoPins,
            display_as_disabled: false,
            user_set_enabled_state: false,
            is_node_enabled: true,
            can_resize_node: false,
            has_compiler_message: false,
            comment_bubble_pinned: false,
            comment_bubble_visible: false,
            comment_bubble_make_visible: false,
            can_rename_node: false,
            node_upgrade_message: FText::new(String::new()),
            node_comment: String::new(),
            error_type: 0,
            error_msg: String::new(),
            node_guid: Guid::zero(),
        }
    }

    /// Parse custom properties for EdGraphNode (e.g., Pin)
    pub fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        // Check if it's a Pin custom property
        if parser.peek_str("Pin ") || parser.peek_str("Pin(") {
            parser.expect_str("Pin")?;
            parser.skip_whitespace();
            let pin = parse_pin(parser)?;
            self.pins.push(pin);
            Ok(())
        } else {
            // Unknown custom property type - don't consume input
            Ok(())
        }
    }

    /// Parse common EdGraphNode properties
    pub fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("NodePosX", None) => {
                self.node_pos_x = parser.parse_int()?;
                Ok(())
            }
            ("NodePosY", None) => {
                self.node_pos_y = parser.parse_int()?;
                Ok(())
            }
            ("NodeWidth", None) => {
                self.node_width = parser.parse_int()?;
                Ok(())
            }
            ("NodeHeight", None) => {
                self.node_height = parser.parse_int()?;
                Ok(())
            }
            ("bDisplayAsDisabled", None) => {
                self.display_as_disabled = parser.parse_bool()?;
                Ok(())
            }
            ("bUserSetEnabledState", None) => {
                self.user_set_enabled_state = parser.parse_bool()?;
                Ok(())
            }
            ("bIsNodeEnabled", None) => {
                self.is_node_enabled = parser.parse_bool()?;
                Ok(())
            }
            ("bCanResizeNode", None) => {
                self.can_resize_node = parser.parse_bool()?;
                Ok(())
            }
            ("bHasCompilerMessage", None) => {
                self.has_compiler_message = parser.parse_bool()?;
                Ok(())
            }
            ("bCommentBubblePinned", None) => {
                self.comment_bubble_pinned = parser.parse_bool()?;
                Ok(())
            }
            ("bCommentBubbleVisible", None) => {
                self.comment_bubble_visible = parser.parse_bool()?;
                Ok(())
            }
            ("bCommentBubbleMakeVisible", None) => {
                self.comment_bubble_make_visible = parser.parse_bool()?;
                Ok(())
            }
            ("bCanRenameNode", None) => {
                self.can_rename_node = parser.parse_bool()?;
                Ok(())
            }
            ("NodeUpgradeMessage", None) => {
                self.node_upgrade_message = parser.parse_text()?;
                Ok(())
            }
            ("NodeComment", None) => {
                self.node_comment = parser.parse_string()?;
                Ok(())
            }
            ("ErrorType", None) => {
                self.error_type = parser.parse_int()?;
                Ok(())
            }
            ("ErrorMsg", None) => {
                self.error_msg = parser.parse_string()?;
                Ok(())
            }
            ("NodeGuid", None) => {
                self.node_guid = parser.parse_guid()?;
                Ok(())
            }
            ("AdvancedPinDisplay", None) => {
                let value_str = parser.parse_string()?;
                self.advanced_pin_display = value_str.parse().map_err(|e| {
                    parser.error(format!("Invalid AdvancedPinDisplay value: {}", e))
                })?;
                Ok(())
            }
            (key, index) => {
                let key_with_index = if let Some(idx) = index {
                    format!("{}({})", key, idx)
                } else {
                    key.to_string()
                };
                Err(parser.error(format!("Unknown EdGraphNode property: {}", key_with_index)))
            }
        }
    }
}

impl Default for EdGraphNode {
    fn default() -> Self {
        Self::new(String::new())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct K2Node {
    pub base: EdGraphNode,
}

impl K2Node {
    pub fn new(name: String) -> Self {
        Self {
            base: EdGraphNode::new(name),
        }
    }

    /// Parse K2Node properties (delegates to base for now)
    pub fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        // K2Node has no specific properties, delegate to base
        self.base.parse_property(key, parser)
    }

    /// Parse K2Node custom properties (delegates to base)
    pub fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        // K2Node has no specific custom properties, delegate to base
        self.base.parse_custom_property(parser)
    }
}

impl Default for K2Node {
    fn default() -> Self {
        Self::new(String::new())
    }
}

impl_deref!(K2Node => EdGraphNode);

/// Member reference structure (MemberParent, MemberName, MemberGuid)
/// From FMemberReference in UE4
#[derive(Debug, Clone, PartialEq, Default)]
pub struct MemberReference {
    pub member_parent: Option<ExportTextPath>, // UObject* - Can be null
    pub member_scope: Option<String>,          // FString - Can be empty
    pub member_name: Option<String>,           // FName - Can be null
    pub member_guid: Option<String>,           // FGuid - Can be null
    pub self_context: bool,                    // bSelfContext
    pub was_deprecated: bool,                  // bWasDeprecated
}

impl MemberReference {
    pub fn is_default(&self) -> bool {
        self.member_parent.is_none()
            && self.member_scope.is_none()
            && self.member_name.is_none()
            && self.member_guid.is_none()
            && !self.self_context
            && !self.was_deprecated
    }
}

/// Graph reference structure for macro instances
/// From FGraphReference in UE4
#[derive(Debug, Clone, PartialEq, Default)]
pub struct FGraphReference {
    pub macro_graph: Option<ExportTextPath>, // UEdGraph* - Can be null
    pub graph_blueprint: Option<ExportTextPath>, // UBlueprint* - Can be null (Blueprint'"/Path/..."')
    pub graph_guid: Guid,                        // FGuid
}

impl FGraphReference {
    pub fn is_default(&self) -> bool {
        self.macro_graph.is_none() && self.graph_blueprint.is_none() && self.graph_guid.is_zero()
    }
}

impl std::fmt::Display for FGraphReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut parts = Vec::new();

        if let Some(ref mg) = self.macro_graph {
            parts.push(format!("MacroGraph={}", mg));
        }

        if let Some(ref gb) = self.graph_blueprint {
            parts.push(format!("GraphBlueprint={}", gb));
        }

        if !self.graph_guid.is_zero() {
            parts.push(format!("GraphGuid={}", self.graph_guid));
        }

        if parts.is_empty() {
            write!(f, "()")
        } else {
            write!(f, "({})", parts.join(","))
        }
    }
}

/// Self context info enum from UK2Node_Variable
/// Indicates whether a variable reference is in self context
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ESelfContextInfo {
    #[default]
    Unspecified,
    NotSelfContext,
}

impl std::str::FromStr for ESelfContextInfo {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Unspecified" => Ok(ESelfContextInfo::Unspecified),
            "NotSelfContext" => Ok(ESelfContextInfo::NotSelfContext),
            _ => Err(format!("Unknown ESelfContextInfo: {}", s)),
        }
    }
}

impl ESelfContextInfo {
    pub fn to_str(&self) -> &'static str {
        match self {
            ESelfContextInfo::Unspecified => "Unspecified",
            ESelfContextInfo::NotSelfContext => "NotSelfContext",
        }
    }
}

impl std::fmt::Display for ESelfContextInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

/// Pin definition for Blueprint nodes
/// From UEdGraphPin in UE4
#[derive(Debug, Clone, PartialEq)]
pub struct Pin {
    pub pin_id: Guid,
    pub pin_name: String,
    pub pin_friendly_name: Option<FText>,
    pub direction: EEdGraphPinDirection,
    pub pin_type: PinType,
    pub pin_tool_tip: String,
    pub default_value: String,
    pub autogenerated_default_value: String,
    pub default_object: Option<String>,
    pub default_text_value: FText,
    pub linked_to: Vec<PinIdentifier>,
    pub sub_pins: Vec<String>,
    pub parent_pin: Option<String>,
    pub reference_pass_through_connection: Option<String>,
    pub persistent_guid: Guid,
    pub hidden: bool,
    pub not_connectable: bool,
    pub default_value_is_read_only: bool,
    pub default_value_is_ignored: bool,
    pub advanced_view: bool,
    pub orphaned_pin: bool,
}

/// FEdGraphTerminalType - represents terminal type information for container pins
#[derive(Debug, Clone, PartialEq, Default)]
pub struct FEdGraphTerminalType {
    pub terminal_category: String,
    pub terminal_sub_category: String,
    pub terminal_sub_category_object: Option<ExportTextPath>,
    pub terminal_is_const: bool,
    pub terminal_is_weak_pointer: bool,
    pub terminal_is_uobject_wrapper: bool,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct PinType {
    pub pin_category: String,
    pub pin_sub_category: String,
    pub pin_sub_category_object: Option<ExportTextPath>,
    pub pin_sub_category_member_reference: Option<MemberReference>,
    pub pin_value_type: Option<FEdGraphTerminalType>,
    pub container_type: Option<String>,
    pub is_reference: bool,
    pub is_const: bool,
    pub is_weak_pointer: bool,
    pub is_uobject_wrapper: bool,
}

impl Default for Pin {
    fn default() -> Self {
        Self {
            pin_id: Guid::zero(),
            pin_name: String::new(),
            pin_friendly_name: None,
            direction: EEdGraphPinDirection::Input,
            pin_type: PinType::default(),
            pin_tool_tip: String::new(),
            default_value: String::new(),
            autogenerated_default_value: String::new(),
            default_object: None,
            default_text_value: FText::new(String::new()),
            linked_to: Vec::new(),
            sub_pins: Vec::new(),
            parent_pin: None,
            reference_pass_through_connection: None,
            persistent_guid: Guid::zero(),
            hidden: false,
            not_connectable: false,
            default_value_is_read_only: false,
            default_value_is_ignored: false,
            advanced_view: false,
            orphaned_pin: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeEvent {
    pub base: K2NodeEditablePinBase,
    pub event_signature_name: String,
    pub event_signature_class: Option<String>, // UClass* can be null
    pub event_reference: MemberReference,
    pub override_function: bool,
    pub internal_event: bool,
    pub custom_function_name: String,
    pub function_flags: u32,
}

impl_deref!(K2NodeEvent => K2NodeEditablePinBase);

impl ObjectTrait for K2NodeEvent {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("EventReference", None) => {
                self.event_reference = parser.parse_member_reference()?;
                Ok(())
            }
            ("EventSignatureName", None) => {
                self.event_signature_name = parser.parse_string()?;
                Ok(())
            }
            ("EventSignatureClass", None) => {
                self.event_signature_class = Some(parser.parse_string()?);
                Ok(())
            }
            ("bOverrideFunction", None) => {
                self.override_function = parser.parse_bool()?;
                Ok(())
            }
            ("bInternalEvent", None) => {
                self.internal_event = parser.parse_bool()?;
                Ok(())
            }
            ("CustomFunctionName", None) => {
                self.custom_function_name = parser.parse_string()?;
                Ok(())
            }
            ("FunctionFlags", None) => {
                self.function_flags = parser.parse_int()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // Event-specific properties first
        serialize_member_ref(buffer, indent, "EventReference", &self.event_reference);
        serialize_bool(buffer, indent, "bOverrideFunction", self.override_function);
        serialize_bool(buffer, indent, "bInternalEvent", self.internal_event);
        serialize_string(
            buffer,
            indent,
            "EventSignatureName",
            &self.event_signature_name,
        );
        serialize_optional_string(
            buffer,
            indent,
            "EventSignatureClass",
            &self.event_signature_class,
        );
        serialize_string(
            buffer,
            indent,
            "CustomFunctionName",
            &self.custom_function_name,
        );
        serialize_int(buffer, indent, "FunctionFlags", self.function_flags);
        // Delegate to K2NodeEditablePinBase (which handles bIsEditable and K2Node)
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        // Delegate to base (handles both Pin and UserDefinedPin)
        self.base.serialize_custom_properties(buffer, indent);
    }
}

impl K2NodeEvent {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeEditablePinBase {
                base: K2Node::new(name),
                is_editable: false,
                user_defined_pins: Vec::new(),
            },
            event_signature_name: String::new(),
            event_signature_class: None,
            event_reference: MemberReference::default(),
            override_function: false,
            internal_event: false,
            custom_function_name: String::new(),
            function_flags: 0,
        }
    }
}

/// K2NodeEditablePinBase - Base class for nodes with editable pins
/// Based on UK2Node_EditablePinBase from UE4
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeEditablePinBase {
    // UK2Node base
    pub base: K2Node,
    // UK2Node_EditablePinBase specific
    pub is_editable: bool,
    // Additional data for user-defined pins
    pub user_defined_pins: Vec<UserDefinedPin>,
}

/// K2NodeFunctionTerminator - Base class for function entry/result nodes
/// Based on UK2Node_FunctionTerminator from UE4
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeFunctionTerminator {
    // UK2Node_EditablePinBase base
    pub base: K2NodeEditablePinBase,
    // UK2Node_FunctionTerminator specific
    pub function_reference: MemberReference,
}

/// K2NodeFunctionEntry - Represents a function entry node
/// Based on UK2Node_FunctionEntry from UE4
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeFunctionEntry {
    // UK2Node_FunctionTerminator base
    pub base: K2NodeFunctionTerminator,
    // UK2NodeFunctionEntry specific
    pub metadata: FKismetUserDeclaredFunctionMetadata,
    pub extra_flags: u32,
    // Additional data
    pub local_variables: Vec<FBPVariableDescription>,
}

/// K2NodeFunctionResult - Represents a function result node
/// Based on UK2Node_FunctionResult from UE4
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeFunctionResult {
    // UK2Node_FunctionTerminator base
    pub base: K2NodeFunctionTerminator,
}

/// K2NodeVariableSet - Represents a variable setter node in Blueprint
/// Based on UK2NodeVariableSet from UE4
/// Inherits from UK2Node_Variable which has:
/// - VariableReference: FMemberReference
/// - SelfContextInfo: uint8_t (ESelfContextInfo enum)
/// - VariableSourceClass: UClass*
/// - VariableName: FName
/// - bSelfContext: bool
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeVariableSet {
    // UK2Node base
    pub base: K2Node,
    // UK2Node_Variable properties
    pub variable_reference: MemberReference,
    pub self_context_info: ESelfContextInfo,
    pub self_context_guid: Option<Guid>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UserDefinedPin {
    pub pin_name: String,
    pub pin_type: PinType,
    pub desired_direction: EEdGraphPinDirection,
    pub default_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FBPVariableDescription {
    pub var_name: String,
    pub var_guid: String,
    pub var_type: PinType,
    pub friendly_name: Option<String>,
    pub category: Option<FText>,
    pub property_flags: Option<u64>,
}

/// FKismetUserDeclaredFunctionMetadata - Metadata for user-declared functions
/// Based on FKismetUserDeclaredFunctionMetadata from UE4
#[derive(Debug, Clone, PartialEq, Default)]
pub struct FKismetUserDeclaredFunctionMetadata {
    pub tooltip: Option<FText>,
    pub category: Option<FText>,
    pub keywords: Option<FText>,
    pub compact_node_title: Option<FText>,
    pub instance_title_color: Option<FLinearColor>,
    pub deprecation_message: Option<String>,
    pub is_deprecated: bool,
    pub call_in_editor: bool,
    pub has_latent_functions: i8,
}

impl_deref!(K2NodeEditablePinBase => K2Node);

impl_deref!(K2NodeFunctionTerminator => K2NodeEditablePinBase);

impl_deref!(K2NodeFunctionEntry => K2NodeFunctionTerminator);

impl_deref!(K2NodeFunctionResult => K2NodeFunctionTerminator);

impl ObjectTrait for K2NodeEditablePinBase {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("bIsEditable", None) => {
                self.is_editable = parser.parse_bool()?;
                Ok(())
            }
            _ => {
                // Delegate to base K2Node
                self.base.parse_property(key, parser)
            }
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        // Handle UserDefinedPin custom properties
        if parser.peek_str("UserDefinedPin") {
            parser.expect_str("UserDefinedPin")?;
            parser.skip_whitespace();
            let udp = parse_user_defined_pin(parser)?;
            self.user_defined_pins.push(udp);
            Ok(())
        } else {
            // Delegate to base for Pin custom properties
            self.base.parse_custom_property(parser)
        }
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_bool(buffer, indent, "bIsEditable", self.is_editable);
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        // Serialize Pin custom properties first (from base K2Node)
        self.base.serialize_custom_properties(buffer, indent);
        // Then serialize UserDefinedPin custom properties
        for udp in &self.user_defined_pins {
            serialize_user_defined_pin(buffer, indent, udp);
        }
    }
}

impl ObjectTrait for K2NodeFunctionTerminator {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("FunctionReference", None) => {
                self.function_reference = parser.parse_member_reference()?;
                Ok(())
            }
            _ => {
                // Delegate to base EditablePinBase
                self.base.parse_property(key, parser)
            }
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_member_ref(
            buffer,
            indent,
            "FunctionReference",
            &self.function_reference,
        );
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

impl ObjectTrait for K2NodeFunctionEntry {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        // Handle function entry specific properties first
        match key {
            ("MetaData", None) => {
                self.metadata = parse_function_metadata(parser)?;
                Ok(())
            }
            ("LocalVariables", Some(index)) => {
                let local_var = parse_local_variable(parser)?;
                // Ensure the vector is large enough
                if index >= self.local_variables.len() {
                    self.local_variables.resize(
                        index + 1,
                        FBPVariableDescription {
                            var_name: String::new(),
                            var_guid: String::new(),
                            var_type: PinType::default(),
                            friendly_name: None,
                            category: None,
                            property_flags: None,
                        },
                    );
                }
                self.local_variables[index] = local_var;
                Ok(())
            }
            ("ExtraFlags", None) => {
                self.extra_flags = parser.parse_int()?;
                Ok(())
            }
            _ => {
                // Delegate to base FunctionTerminator (which handles FunctionReference, bIsEditable, etc.)
                self.base.parse_property(key, parser)
            }
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // Serialize MetaData if not default
        if !is_metadata_default(&self.metadata) {
            serialize_function_metadata(buffer, indent, &self.metadata);
        }

        // Serialize LocalVariables
        for (index, local_var) in self.local_variables.iter().enumerate() {
            serialize_local_variable(buffer, indent, index, local_var);
        }

        // FunctionTerminator will serialize FunctionReference
        // Function entry specific properties
        serialize_int(buffer, indent, "ExtraFlags", self.extra_flags);
        // Delegate to base (which handles EditablePinBase and K2Node)
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        // Delegate to base which handles UserDefinedPin serialization
        self.base.serialize_custom_properties(buffer, indent);
    }
}

impl K2NodeFunctionEntry {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeFunctionTerminator {
                base: K2NodeEditablePinBase {
                    base: K2Node::new(name),
                    is_editable: false,
                    user_defined_pins: Vec::new(),
                },
                function_reference: MemberReference::default(),
            },
            metadata: FKismetUserDeclaredFunctionMetadata::default(),
            extra_flags: 0,
            local_variables: Vec::new(),
        }
    }
}

impl ObjectTrait for K2NodeFunctionResult {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        // K2NodeFunctionResult has no additional properties beyond FunctionTerminator
        // Delegate to base
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // K2NodeFunctionResult has no additional properties beyond FunctionTerminator
        // Delegate to base
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

impl K2NodeFunctionResult {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeFunctionTerminator {
                base: K2NodeEditablePinBase {
                    base: K2Node::new(name),
                    is_editable: false,
                    user_defined_pins: Vec::new(),
                },
                function_reference: MemberReference::default(),
            },
        }
    }
}

impl_deref!(K2NodeVariableSet => K2Node);

impl ObjectTrait for K2NodeVariableSet {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        // Handle variable set specific properties first
        match key {
            ("VariableReference", None) => {
                self.variable_reference = parser.parse_member_reference()?;
                Ok(())
            }
            ("SelfContextInfo", None) => {
                let value_str = parser.parse_string()?;
                self.self_context_info = value_str
                    .parse()
                    .map_err(|e| parser.error(format!("Invalid SelfContextInfo value: {}", e)))?;
                Ok(())
            }
            ("SelfContextGuid", None) => {
                self.self_context_guid = Some(parser.parse_guid()?);
                Ok(())
            }
            _ => {
                // Not a variable set specific property, try base class
                self.base.parse_property(key, parser)
            }
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        // K2NodeVariableSet has no specific custom properties, delegate to base
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // Variable set specific properties
        serialize_member_ref(
            buffer,
            indent,
            "VariableReference",
            &self.variable_reference,
        );
        serialize_enum(buffer, indent, "SelfContextInfo", self.self_context_info);
        if let Some(ref guid) = self.self_context_guid {
            serialize_guid(buffer, indent, "SelfContextGuid", guid);
        }
        // Base K2Node properties (via EdGraphNode)
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

impl K2NodeVariableSet {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            variable_reference: MemberReference::default(),
            self_context_info: ESelfContextInfo::default(),
            self_context_guid: None,
        }
    }
}

/// K2NodeAddDelegate - Represents adding a delegate binding
/// Based on UK2NodeAddDelegate : public UK2Node_BaseMCDelegate
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeAddDelegate {
    pub base: K2Node,
    pub delegate_reference: MemberReference,
}

impl K2NodeAddDelegate {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            delegate_reference: MemberReference::default(),
        }
    }
}

impl_deref!(K2NodeAddDelegate => K2Node);

impl ObjectTrait for K2NodeAddDelegate {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("DelegateReference", None) => {
                self.delegate_reference = parser.parse_member_reference()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // AddDelegate-specific properties first
        serialize_member_ref(
            buffer,
            indent,
            "DelegateReference",
            &self.delegate_reference,
        );
        // Base K2Node properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// FOptionalPinFromProperty - Represents optional pin properties for struct member get nodes
#[derive(Debug, Clone, PartialEq)]
pub struct FOptionalPinFromProperty {
    pub property_name: String,
    pub property_friendly_name: String,
    pub property_tooltip: FText,
    pub category_name: String,
    pub show_pin: bool,
    pub can_toggle_visibility: bool,
    pub property_is_customized: bool,
    pub has_override_pin: bool,
    pub is_marked_for_advanced_display: bool,
    pub is_override_enabled: bool,
    pub is_set_value_pin_visible: bool,
    pub is_override_pin_visible: bool,
}

impl Default for FOptionalPinFromProperty {
    fn default() -> Self {
        Self {
            property_name: String::new(),
            property_friendly_name: String::new(),
            property_tooltip: FText::new(String::new()),
            category_name: String::new(),
            show_pin: false,
            can_toggle_visibility: false,
            property_is_customized: false,
            has_override_pin: false,
            is_marked_for_advanced_display: false,
            is_override_enabled: false,
            is_set_value_pin_visible: false,
            is_override_pin_visible: false,
        }
    }
}

/// K2NodeStructOperation - Base class for struct operations
/// Based on UK2NodeStructOperation : public UK2Node_Variable
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeStructOperation {
    pub base: K2Node,
    pub struct_type: Option<ExportTextPath>, // UScriptStruct* - stored as object reference string
}

impl K2NodeStructOperation {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            struct_type: None,
        }
    }

    pub fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("StructType", None) => {
                self.struct_type = Some(parser.parse_export_text_path()?);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    pub fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }
}

impl_deref!(K2NodeStructOperation => K2Node);

impl K2NodeStructOperation {
    /// Serialize K2NodeStructOperation properties
    pub fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_optional_export_text_path(buffer, indent, "StructType", &self.struct_type);
        self.base.serialize(buffer, indent);
    }

    /// Serialize custom properties
    pub fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeStructMemberGet - Base class for struct member get operations
/// Based on UK2NodeStructMemberGet : public UK2NodeStructOperation
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeStructMemberGet {
    pub base: K2NodeStructOperation,
    pub show_pin_for_properties: Vec<FOptionalPinFromProperty>,
}

impl K2NodeStructMemberGet {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeStructOperation::new(name),
            show_pin_for_properties: Vec::new(),
        }
    }
}

impl_deref!(K2NodeStructMemberGet => K2NodeStructOperation);

impl ObjectTrait for K2NodeStructMemberGet {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("ShowPinForProperties", Some(_index)) => {
                // Parse the struct value: (PropertyName="...",PropertyFriendlyName="..."...)
                parser.skip_whitespace();
                let value_str = parser.parse_parenthesized_value()?;

                // Parse the struct - for now just store it as a default entry
                // A full implementation would parse all fields from value_str
                let mut prop = FOptionalPinFromProperty::default();

                // Parse PropertyName
                if let Some(start) = value_str.find("PropertyName=\"") {
                    let rest = &value_str[start + 14..];
                    if let Some(end) = rest.find('"') {
                        prop.property_name = rest[..end].to_string();
                    }
                }

                // Parse PropertyFriendlyName
                if let Some(start) = value_str.find("PropertyFriendlyName=\"") {
                    let rest = &value_str[start + 22..];
                    if let Some(end) = rest.find('"') {
                        prop.property_friendly_name = rest[..end].to_string();
                    }
                }

                // Parse PropertyTooltip
                if let Some(start) = value_str.find("PropertyTooltip=\"") {
                    let rest = &value_str[start + 17..];
                    if let Some(end) = rest.find('"') {
                        prop.property_tooltip = FText::literal(rest[..end].to_string());
                    }
                }

                // Parse CategoryName
                if let Some(start) = value_str.find("CategoryName=\"") {
                    let rest = &value_str[start + 14..];
                    if let Some(end) = rest.find('"') {
                        prop.category_name = rest[..end].to_string();
                    }
                }

                // Parse bShowPin
                if value_str.contains("bShowPin=True") {
                    prop.show_pin = true;
                }

                // Parse bCanToggleVisibility
                if value_str.contains("bCanToggleVisibility=True") {
                    prop.can_toggle_visibility = true;
                }

                self.show_pin_for_properties.push(prop);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // StructMemberGet-specific properties
        serialize_show_pin_for_properties(buffer, indent, &self.show_pin_for_properties);
        // Base properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeBreakStruct - Represents a struct break node
/// Based on UK2NodeBreakStruct : public UK2NodeStructMemberGet
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeBreakStruct {
    pub base: K2NodeStructMemberGet,
    pub made_after_override_pin_removal: bool,
}

impl K2NodeBreakStruct {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeStructMemberGet::new(name),
            made_after_override_pin_removal: false,
        }
    }
}

impl_deref!(K2NodeBreakStruct => K2NodeStructMemberGet);

impl ObjectTrait for K2NodeBreakStruct {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("bMadeAfterOverridePinRemoval", None) => {
                self.made_after_override_pin_removal = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // BreakStruct-specific properties first
        serialize_bool(
            buffer,
            indent,
            "bMadeAfterOverridePinRemoval",
            self.made_after_override_pin_removal,
        );
        // StructMemberGet properties (from base)
        serialize_show_pin_for_properties(buffer, indent, &self.base.show_pin_for_properties);
        // Base properties (StructOperation and below)
        self.base.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeCallArrayFunction - Represents an array function call
/// Based on UK2NodeCallArrayFunction : public UK2NodeCallFunction
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeCallArrayFunction {
    pub base: K2NodeCallFunction,
}

impl K2NodeCallArrayFunction {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeCallFunction::new(name),
        }
    }
}

impl_deref!(K2NodeCallArrayFunction => K2NodeCallFunction);

impl ObjectTrait for K2NodeCallArrayFunction {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeCallFunction - Represents a function call node
/// Based on UK2NodeCallFunction : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeCallFunction {
    pub base: K2Node,
    pub is_pure_func: bool,
    pub is_const_func: bool,
    pub wants_enum_to_exec_expansion: bool,
    pub is_interface_call: bool,
    pub is_final_function: bool,
    pub function_reference: MemberReference,
}

impl K2NodeCallFunction {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            is_pure_func: false,
            is_const_func: false,
            wants_enum_to_exec_expansion: false,
            is_interface_call: false,
            is_final_function: false,
            function_reference: MemberReference::default(),
        }
    }
}

impl_deref!(K2NodeCallFunction => K2Node);

impl ObjectTrait for K2NodeCallFunction {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("bIsPureFunc", None) => {
                self.is_pure_func = parser.parse_bool()?;
                Ok(())
            }
            ("bIsConstFunc", None) => {
                self.is_const_func = parser.parse_bool()?;
                Ok(())
            }
            ("bWantsEnumToExecExpansion", None) => {
                self.wants_enum_to_exec_expansion = parser.parse_bool()?;
                Ok(())
            }
            ("bIsInterfaceCall", None) => {
                self.is_interface_call = parser.parse_bool()?;
                Ok(())
            }
            ("bIsFinalFunction", None) => {
                self.is_final_function = parser.parse_bool()?;
                Ok(())
            }
            ("FunctionReference", None) => {
                self.function_reference = parser.parse_member_reference()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // CallFunction-specific properties first - booleans before FunctionReference
        serialize_bool(buffer, indent, "bIsPureFunc", self.is_pure_func);
        serialize_bool(buffer, indent, "bIsConstFunc", self.is_const_func);
        serialize_bool(
            buffer,
            indent,
            "bWantsEnumToExecExpansion",
            self.wants_enum_to_exec_expansion,
        );
        serialize_bool(buffer, indent, "bIsInterfaceCall", self.is_interface_call);
        serialize_bool(buffer, indent, "bIsFinalFunction", self.is_final_function);
        serialize_member_ref(
            buffer,
            indent,
            "FunctionReference",
            &self.function_reference,
        );
        // Base K2Node properties (via EdGraphNode)
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeCommutativeAssociativeBinaryOperator - Represents binary operators like +, *, AND, OR
/// Based on UK2NodeCommutativeAssociativeBinaryOperator : public UK2NodeCallFunction
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeCommutativeAssociativeBinaryOperator {
    pub base: K2NodeCallFunction,
    pub num_additional_inputs: i32,
}

impl K2NodeCommutativeAssociativeBinaryOperator {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeCallFunction::new(name),
            num_additional_inputs: 0,
        }
    }
}

impl_deref!(K2NodeCommutativeAssociativeBinaryOperator => K2NodeCallFunction);

impl ObjectTrait for K2NodeCommutativeAssociativeBinaryOperator {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("NumAdditionalInputs", None) => {
                self.num_additional_inputs = parser.parse_int()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // Serialize NumAdditionalInputs if non-zero
        serialize_int(
            buffer,
            indent,
            "NumAdditionalInputs",
            self.num_additional_inputs,
        );
        // Delegate to base K2NodeCallFunction
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeComposite - Represents a collapsed graph/composite node
/// Based on UK2NodeComposite : public UK2NodeTunnel
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeComposite {
    pub base: K2Node,
    pub bound_graph: Option<String>,
}

impl K2NodeComposite {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            bound_graph: None,
        }
    }
}

impl_deref!(K2NodeComposite => K2Node);

impl ObjectTrait for K2NodeComposite {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("BoundGraph", None) => {
                self.bound_graph = Some(parser.parse_string()?);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeCustomEvent - Represents a custom event in Blueprint
/// Based on UK2NodeCustomEvent : public UK2NodeEvent
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeCustomEvent {
    pub base: K2NodeEvent,
    pub deprecation_message: String,
    pub is_deprecated: bool,
    pub call_in_editor: bool,
}

impl K2NodeCustomEvent {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeEvent::new(name),
            deprecation_message: String::new(),
            is_deprecated: false,
            call_in_editor: false,
        }
    }
}

impl_deref!(K2NodeCustomEvent => K2NodeEvent);

impl ObjectTrait for K2NodeCustomEvent {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("DeprecationMessage", None) => {
                self.deprecation_message = parser.parse_string()?;
                Ok(())
            }
            ("bIsDeprecated", None) => {
                self.is_deprecated = parser.parse_bool()?;
                Ok(())
            }
            ("bCallInEditor", None) => {
                self.call_in_editor = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeDynamicCast - Represents a dynamic cast operation
/// Based on UK2NodeDynamicCast : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeDynamicCast {
    pub base: K2Node,
    pub target_type: Option<ExportTextPath>,
    pub is_pure_cast: bool,
}

impl K2NodeDynamicCast {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            target_type: None,
            is_pure_cast: false,
        }
    }
}

impl_deref!(K2NodeDynamicCast => K2Node);

impl ObjectTrait for K2NodeDynamicCast {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("TargetType", None) => {
                self.target_type = Some(parser.parse_export_text_path()?);
                Ok(())
            }
            ("bIsPureCast", None) => {
                self.is_pure_cast = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // DynamicCast-specific properties first
        serialize_optional_export_text_path(buffer, indent, "TargetType", &self.target_type);
        serialize_bool(buffer, indent, "bIsPureCast", self.is_pure_cast);
        // Base K2Node properties (via EdGraphNode)
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeExecutionSequence - Represents a sequence node
/// Based on UK2NodeExecutionSequence : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeExecutionSequence {
    pub base: K2Node,
}

impl K2NodeExecutionSequence {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeExecutionSequence => K2Node);

impl ObjectTrait for K2NodeExecutionSequence {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeFormatText - Represents a format text node
/// Based on UK2NodeFormatText : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeFormatText {
    pub base: K2Node,
    pub pin_names: Vec<String>,
}

impl K2NodeFormatText {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            pin_names: Vec::new(),
        }
    }
}

impl_deref!(K2NodeFormatText => K2Node);

impl ObjectTrait for K2NodeFormatText {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("PinNames", Some(_index)) => {
                let value = parser.parse_string()?;
                self.pin_names.push(value);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // Serialize PinNames array
        for (index, pin_name) in self.pin_names.iter().enumerate() {
            buffer.push_str(indent);
            buffer.push_str(&format!("PinNames({})=\"", index));
            buffer.push_str(&escape_string(pin_name));
            buffer.push_str("\"\r\n");
        }
        // Base K2Node properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeGetEnumeratorNameAsString - Get enum name as string
/// Based on UK2NodeGetEnumeratorNameAsString : public UK2Node_GetEnumeratorName
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeGetEnumeratorNameAsString {
    pub base: K2Node,
}

impl K2NodeGetEnumeratorNameAsString {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeGetEnumeratorNameAsString => K2Node);

impl ObjectTrait for K2NodeGetEnumeratorNameAsString {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeIfThenElse - Represents a branch/if-then-else node
/// Based on UK2NodeIfThenElse : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeIfThenElse {
    pub base: K2Node,
}

impl K2NodeIfThenElse {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeIfThenElse => K2Node);

impl ObjectTrait for K2NodeIfThenElse {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeInputAction - Represents input action binding
/// Based on UK2NodeInputAction : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeInputAction {
    pub base: K2Node,
    pub input_action_name: String,
    pub consume_input: bool,
    pub execute_when_paused: bool,
    pub override_parent_binding: bool,
}

impl K2NodeInputAction {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            input_action_name: String::new(),
            consume_input: false,
            execute_when_paused: false,
            override_parent_binding: false,
        }
    }
}

impl_deref!(K2NodeInputAction => K2Node);

impl ObjectTrait for K2NodeInputAction {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("InputActionName", None) => {
                self.input_action_name = parser.parse_string()?;
                Ok(())
            }
            ("bConsumeInput", None) => {
                self.consume_input = parser.parse_bool()?;
                Ok(())
            }
            ("bExecuteWhenPaused", None) => {
                self.execute_when_paused = parser.parse_bool()?;
                Ok(())
            }
            ("bOverrideParentBinding", None) => {
                self.override_parent_binding = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeKnot - Represents a reroute/knot node
/// Based on UK2NodeKnot : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeKnot {
    pub base: K2Node,
}

impl K2NodeKnot {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeKnot => K2Node);

impl ObjectTrait for K2NodeKnot {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeMacroInstance - Represents a macro instance
/// Based on UK2NodeMacroInstance : public UK2NodeTunnel
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeMacroInstance {
    pub base: K2Node,
    pub macro_graph_reference: Option<FGraphReference>,
    pub resolved_wildcard_type: Option<PinType>,
}

impl K2NodeMacroInstance {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            macro_graph_reference: None,
            resolved_wildcard_type: None,
        }
    }
}

impl_deref!(K2NodeMacroInstance => K2Node);

impl ObjectTrait for K2NodeMacroInstance {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("MacroGraphReference", None) => {
                self.macro_graph_reference = Some(parser.parse_graph_reference()?);
                Ok(())
            }
            ("ResolvedWildcardType", None) => {
                self.resolved_wildcard_type = Some(parse_pin_type(parser)?);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        if let Some(ref graph_ref) = self.macro_graph_reference
            && !graph_ref.is_default()
        {
            buffer.push_str(indent);
            buffer.push_str("MacroGraphReference=");
            buffer.push_str(&graph_ref.to_string());
            buffer.push_str("\r\n");
        }
        if let Some(ref wildcard_type) = self.resolved_wildcard_type {
            buffer.push_str(indent);
            buffer.push_str("ResolvedWildcardType=");
            serialize_pin_type_value(wildcard_type, buffer);
            buffer.push_str("\r\n");
        }
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeMakeArray - Represents a make array node
/// Based on UK2NodeMakeArray : public UK2Node_MakeContainer
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeMakeArray {
    pub base: K2Node,
    pub num_inputs: i32,
}

impl K2NodeMakeArray {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            num_inputs: 0,
        }
    }
}

impl_deref!(K2NodeMakeArray => K2Node);

impl ObjectTrait for K2NodeMakeArray {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("NumInputs", None) => {
                self.num_inputs = parser.parse_int()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeSelect - Represents a select node
/// Based on UK2NodeSelect : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeSelect {
    pub base: K2Node,
    pub num_option_pins: i32,
    pub index_pin_type: Option<PinType>,
    pub enum_type: Option<ExportTextPath>,
    pub enum_entries: Vec<String>,
    pub enum_entry_friendly_names: Vec<FText>,
    pub reconstruct_node: bool,
}

impl K2NodeSelect {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            num_option_pins: 0,
            index_pin_type: None,
            enum_type: None,
            enum_entries: Vec::new(),
            enum_entry_friendly_names: Vec::new(),
            reconstruct_node: false,
        }
    }
}

impl_deref!(K2NodeSelect => K2Node);

impl ObjectTrait for K2NodeSelect {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("NumOptionPins", None) => {
                self.num_option_pins = parser.parse_int()?;
                Ok(())
            }
            ("IndexPinType", None) => {
                self.index_pin_type = Some(parse_pin_type(parser)?);
                Ok(())
            }
            ("Enum", None) => {
                self.enum_type = Some(parser.parse_export_text_path()?);
                Ok(())
            }
            ("EnumEntries", Some(_index)) => {
                self.enum_entries.push(parser.parse_string()?);
                Ok(())
            }
            ("EnumEntryFriendlyNames", Some(_index)) => {
                self.enum_entry_friendly_names.push(parser.parse_text()?);
                Ok(())
            }
            ("bReconstructNode", None) => {
                self.reconstruct_node = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // Serialize K2NodeSelect-specific properties
        if let Some(ref index_pin_type) = self.index_pin_type {
            buffer.push_str(indent);
            buffer.push_str("IndexPinType=(");

            // For IndexPinType, always include PinSubCategory even if empty
            let mut first = true;
            if !index_pin_type.pin_category.is_empty() {
                buffer.push_str("PinCategory=\"");
                buffer.push_str(&escape_string(&index_pin_type.pin_category));
                buffer.push('"');
                first = false;
            }

            if !index_pin_type.pin_category.is_empty() {
                if !first {
                    buffer.push(',');
                }
                buffer.push_str("PinSubCategory=\"");
                buffer.push_str(&escape_string(&index_pin_type.pin_sub_category));
                buffer.push('"');
            }

            buffer.push_str(")\r\n");
        }
        serialize_int(buffer, indent, "NumOptionPins", self.num_option_pins);
        serialize_optional_export_text_path(buffer, indent, "Enum", &self.enum_type);
        for (index, entry) in self.enum_entries.iter().enumerate() {
            buffer.push_str(indent);
            buffer.push_str(&format!("EnumEntries({})=\"", index));
            buffer.push_str(&escape_string(entry));
            buffer.push_str("\"\r\n");
        }
        for (index, friendly_name) in self.enum_entry_friendly_names.iter().enumerate() {
            buffer.push_str(indent);
            buffer.push_str(&format!("EnumEntryFriendlyNames({})=", index));
            buffer.push_str(&friendly_name.serialize());
            buffer.push_str("\r\n");
        }
        serialize_bool(buffer, indent, "bReconstructNode", self.reconstruct_node);
        // Delegate to base
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeSelf - Represents a self reference node
/// Based on UK2NodeSelf : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeSelf {
    pub base: K2Node,
}

impl K2NodeSelf {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeSelf => K2Node);

impl ObjectTrait for K2NodeSelf {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeSpawnActorFromClass - Represents spawning an actor
/// Based on UK2NodeSpawnActorFromClass : public UK2Node_ConstructObjectFromClass
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeSpawnActorFromClass {
    pub base: K2Node,
}

impl K2NodeSpawnActorFromClass {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeSpawnActorFromClass => K2Node);

impl ObjectTrait for K2NodeSpawnActorFromClass {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeTunnel - Represents a tunnel node
/// Based on UK2NodeTunnel : public UK2Node_EditablePinBase
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeTunnel {
    pub base: K2Node,
    pub output_source_node: Option<String>,
    pub input_sink_node: Option<String>,
    pub can_have_inputs: bool,
    pub can_have_outputs: bool,
}

impl K2NodeTunnel {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            output_source_node: None,
            input_sink_node: None,
            can_have_inputs: false,
            can_have_outputs: false,
        }
    }
}

impl_deref!(K2NodeTunnel => K2Node);

impl ObjectTrait for K2NodeTunnel {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("OutputSourceNode", None) => {
                self.output_source_node = Some(parser.parse_string()?);
                Ok(())
            }
            ("InputSinkNode", None) => {
                self.input_sink_node = Some(parser.parse_string()?);
                Ok(())
            }
            ("bCanHaveInputs", None) => {
                self.can_have_inputs = parser.parse_bool()?;
                Ok(())
            }
            ("bCanHaveOutputs", None) => {
                self.can_have_outputs = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeVariableGet - Represents a variable getter node
/// Based on UK2NodeVariableGet : public UK2Node_Variable
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeVariableGet {
    pub base: K2Node,
    pub variable_reference: MemberReference,
    pub self_context_info: ESelfContextInfo,
    pub is_pure_get: bool,
}

impl K2NodeVariableGet {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            variable_reference: MemberReference::default(),
            self_context_info: ESelfContextInfo::default(),
            is_pure_get: false,
        }
    }
}

impl_deref!(K2NodeVariableGet => K2Node);

impl ObjectTrait for K2NodeVariableGet {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("VariableReference", None) => {
                self.variable_reference = parser.parse_member_reference()?;
                Ok(())
            }
            ("SelfContextInfo", None) => {
                let value_str = parser.parse_string()?;
                self.self_context_info = value_str
                    .parse()
                    .map_err(|e| parser.error(format!("Invalid SelfContextInfo value: {}", e)))?;
                Ok(())
            }
            ("bIsPureGet", None) => {
                self.is_pure_get = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // VariableGet-specific properties first
        serialize_member_ref(
            buffer,
            indent,
            "VariableReference",
            &self.variable_reference,
        );
        serialize_enum(buffer, indent, "SelfContextInfo", self.self_context_info);
        serialize_bool(buffer, indent, "bIsPureGet", self.is_pure_get);
        // Base K2Node properties (via EdGraphNode)
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// UEdGraph - Represents an editor graph
/// Based on UEdGraph : public UObject
#[derive(Debug, Clone, PartialEq)]
pub struct UEdGraph {
    pub name: String,
    pub schema: Option<String>,
    pub nodes: Vec<String>,
    pub editable: bool,
    pub allow_deletion: bool,
    pub allow_renaming: bool,
    pub sub_graphs: Vec<String>,
    pub graph_guid: Guid,
    pub interface_guid: Guid,
}

impl UEdGraph {
    pub fn new(name: String) -> Self {
        Self {
            name,
            schema: None,
            nodes: Vec::new(),
            editable: true,
            allow_deletion: true,
            allow_renaming: true,
            sub_graphs: Vec::new(),
            graph_guid: Guid::zero(),
            interface_guid: Guid::zero(),
        }
    }
}

impl ObjectTrait for UEdGraph {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("Schema", None) => {
                self.schema = Some(parser.parse_string()?);
                Ok(())
            }
            ("bEditable", None) => {
                self.editable = parser.parse_bool()?;
                Ok(())
            }
            ("bAllowDeletion", None) => {
                self.allow_deletion = parser.parse_bool()?;
                Ok(())
            }
            ("bAllowRenaming", None) => {
                self.allow_renaming = parser.parse_bool()?;
                Ok(())
            }
            ("GraphGuid", None) => {
                self.graph_guid = parser.parse_guid()?;
                Ok(())
            }
            ("InterfaceGuid", None) => {
                self.interface_guid = parser.parse_guid()?;
                Ok(())
            }
            (key, index) => {
                let key_with_index = if let Some(idx) = index {
                    format!("{}({})", key, idx)
                } else {
                    key.to_string()
                };
                Err(parser.error(format!("Unknown UEdGraph property: {}", key_with_index)))
            }
        }
    }

    fn parse_custom_property(&mut self, _parser: &mut Parser) -> Result<(), ParseError> {
        Ok(())
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_optional_string(buffer, indent, "Schema", &self.schema);
        serialize_bool(buffer, indent, "bEditable", self.editable);
        serialize_bool(buffer, indent, "bAllowDeletion", self.allow_deletion);
        serialize_bool(buffer, indent, "bAllowRenaming", self.allow_renaming);
        serialize_guid(buffer, indent, "GraphGuid", &self.graph_guid);
        serialize_guid(buffer, indent, "InterfaceGuid", &self.interface_guid);
    }

    fn serialize_custom_properties(&self, _buffer: &mut String, _indent: &str) {}
}

#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeCreateWidget {
    pub base: K2Node,
}

impl K2NodeCreateWidget {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeCreateWidget => K2Node);

impl ObjectTrait for K2NodeCreateWidget {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UEdGraphNodeComment {
    pub base: EdGraphNode,
    pub comment_color: Option<FLinearColor>,
    pub font_size: i32,
    pub comment_bubble_visible_in_details_panel: bool,
    pub color_comment_bubble: bool,
    pub move_mode: u8,
    pub comment_depth: i32,
}

impl UEdGraphNodeComment {
    pub fn new(name: String) -> Self {
        let mut base = EdGraphNode::new(name);
        // Override defaults for Comment nodes (these are True by default for Comment nodes only)
        base.comment_bubble_pinned = true;
        base.comment_bubble_visible = true;

        Self {
            base,
            comment_color: None,
            font_size: 18,
            comment_bubble_visible_in_details_panel: true, // Default is True in Unreal Engine
            color_comment_bubble: false,
            move_mode: 0,
            comment_depth: 0,
        }
    }
}

impl_deref!(UEdGraphNodeComment => EdGraphNode);

impl ObjectTrait for UEdGraphNodeComment {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("CommentColor", None) => {
                let color_str = parser.parse_parenthesized_value()?;
                self.comment_color =
                    Some(FLinearColor::parse(&color_str).map_err(|e| parser.error(e))?);
                Ok(())
            }
            ("FontSize", None) => {
                self.font_size = parser.parse_int()?;
                Ok(())
            }
            ("bCommentBubbleVisible_InDetailsPanel", None) => {
                self.comment_bubble_visible_in_details_panel = parser.parse_bool()?;
                Ok(())
            }
            ("bColorCommentBubble", None) => {
                self.color_comment_bubble = parser.parse_bool()?;
                Ok(())
            }
            ("MoveMode", None) => {
                self.move_mode = parser.parse_int()?;
                Ok(())
            }
            ("CommentDepth", None) => {
                self.comment_depth = parser.parse_int()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // CommentColor first (if present)
        if let Some(ref color) = self.comment_color {
            buffer.push_str(indent);
            buffer.push_str("CommentColor=");
            buffer.push_str(&color.serialize());
            buffer.push_str("\r\n");
        }

        // Comment-specific properties (only serialize non-defaults)
        if !self.comment_bubble_visible_in_details_panel {
            buffer.push_str(indent);
            buffer.push_str("bCommentBubbleVisible_InDetailsPanel=False\r\n");
        }

        // For Comment nodes, we need to handle comment bubble properties differently
        // Base EdGraphNode serialization will handle regular node properties
        // but we need to override for Comment-specific defaults
        serialize_int(buffer, indent, "NodePosX", self.base.node_pos_x);
        serialize_int(buffer, indent, "NodePosY", self.base.node_pos_y);
        serialize_int(buffer, indent, "NodeWidth", self.base.node_width);
        serialize_int(buffer, indent, "NodeHeight", self.base.node_height);

        // For Comment nodes, serialize if False (default is True)
        if !self.base.comment_bubble_pinned {
            buffer.push_str(indent);
            buffer.push_str("bCommentBubblePinned=False\r\n");
        }
        if !self.base.comment_bubble_visible {
            buffer.push_str(indent);
            buffer.push_str("bCommentBubbleVisible=False\r\n");
        }

        serialize_string(buffer, indent, "NodeComment", &self.base.node_comment);
        serialize_guid(buffer, indent, "NodeGuid", &self.base.node_guid);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeAddComponentByClass - Add a component to an actor by class
/// Based on UK2NodeAddComponentByClass : public UK2NodeCallFunction
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeAddComponentByClass {
    pub base: K2NodeCallFunction,
    pub template_name: String,
    pub has_exposed_pins: bool,
}

impl K2NodeAddComponentByClass {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeCallFunction::new(name),
            template_name: String::new(),
            has_exposed_pins: false,
        }
    }
}

impl_deref!(K2NodeAddComponentByClass => K2NodeCallFunction);

impl ObjectTrait for K2NodeAddComponentByClass {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("TemplateName", None) => {
                self.template_name = parser.parse_string()?;
                Ok(())
            }
            ("bHasExposedPins", None) => {
                self.has_exposed_pins = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

impl_deref!(K2NodeComponentBoundEvent => K2NodeEvent);
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeComponentBoundEvent {
    pub base: K2NodeEvent,
    pub delegate_property_name: String,
    pub delegate_owner_class: Option<ExportTextPath>,
    pub component_property_name: String,
    pub delegate_property_display_name: Option<FText>,
}

impl K2NodeComponentBoundEvent {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeEvent::new(name),
            delegate_property_name: String::new(),
            delegate_owner_class: None,
            component_property_name: String::new(),
            delegate_property_display_name: None,
        }
    }
}

impl ObjectTrait for K2NodeComponentBoundEvent {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("DelegatePropertyName", None) => {
                self.delegate_property_name = parser.parse_string()?;
                Ok(())
            }
            ("DelegateOwnerClass", None) => {
                self.delegate_owner_class = Some(parser.parse_export_text_path()?);
                Ok(())
            }
            ("ComponentPropertyName", None) => {
                self.component_property_name = parser.parse_string()?;
                Ok(())
            }
            ("DelegatePropertyDisplayName", None) => {
                self.delegate_property_display_name = Some(parser.parse_text()?);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // ComponentBoundEvent-specific properties
        serialize_string(
            buffer,
            indent,
            "DelegatePropertyName",
            &self.delegate_property_name,
        );
        serialize_optional_export_text_path(
            buffer,
            indent,
            "DelegateOwnerClass",
            &self.delegate_owner_class,
        );
        serialize_string(
            buffer,
            indent,
            "ComponentPropertyName",
            &self.component_property_name,
        );
        serialize_optional_ftext(
            buffer,
            indent,
            "DelegatePropertyDisplayName",
            &self.delegate_property_display_name,
        );
        // Then Event properties
        serialize_member_ref(buffer, indent, "EventReference", &self.base.event_reference);
        serialize_bool(
            buffer,
            indent,
            "bOverrideFunction",
            self.base.override_function,
        );
        serialize_bool(buffer, indent, "bInternalEvent", self.base.internal_event);
        // CustomFunctionName before base properties
        serialize_string(
            buffer,
            indent,
            "CustomFunctionName",
            &self.base.custom_function_name,
        );
        // Then base K2Node properties (via EdGraphNode)
        self.base.base.serialize(buffer, indent);
        // Additional event properties
        serialize_string(
            buffer,
            indent,
            "EventSignatureName",
            &self.base.event_signature_name,
        );
        serialize_optional_string(
            buffer,
            indent,
            "EventSignatureClass",
            &self.base.event_signature_class,
        );
        serialize_int(buffer, indent, "FunctionFlags", self.base.function_flags);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeEnumLiteral - Represents an enum literal value
/// Based on UK2NodeEnumLiteral : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeEnumLiteral {
    pub base: K2Node,
    pub enum_type: Option<ExportTextPath>,
}

impl K2NodeEnumLiteral {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            enum_type: None,
        }
    }
}

impl_deref!(K2NodeEnumLiteral => K2Node);

impl ObjectTrait for K2NodeEnumLiteral {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("Enum", None) => {
                self.enum_type = Some(parser.parse_export_text_path()?);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_optional_export_text_path(buffer, indent, "Enum", &self.enum_type);
        // Base K2Node properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeGetArrayItem - Get an item from an array
/// Based on UK2NodeGetArrayItem : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeGetArrayItem {
    pub base: K2Node,
    pub return_by_ref_desired: bool,
}

impl K2NodeGetArrayItem {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            return_by_ref_desired: true, // Default is True in Unreal Engine
        }
    }
}

impl_deref!(K2NodeGetArrayItem => K2Node);

impl ObjectTrait for K2NodeGetArrayItem {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("bReturnByRefDesired", None) => {
                self.return_by_ref_desired = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // GetArrayItem-specific properties (only serialize if False, as True is default)
        if !self.return_by_ref_desired {
            buffer.push_str(indent);
            buffer.push_str("bReturnByRefDesired=False\r\n");
        }
        // Base properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeSwitchEnum - Switch/case statement for enums
/// Based on UK2NodeSwitchEnum : public UK2Node_Switch
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeSwitchEnum {
    pub base: K2Node,
    pub enum_type: Option<ExportTextPath>,
    pub enum_entries: Vec<String>,
    pub enum_friendly_names: Vec<FText>,
    pub pin_names: Vec<String>,
}

impl K2NodeSwitchEnum {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
            enum_type: None,
            enum_entries: Vec::new(),
            enum_friendly_names: Vec::new(),
            pin_names: Vec::new(),
        }
    }
}

impl_deref!(K2NodeSwitchEnum => K2Node);

impl ObjectTrait for K2NodeSwitchEnum {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("Enum", None) => {
                self.enum_type = Some(parser.parse_export_text_path()?);
                Ok(())
            }
            ("EnumEntries", Some(_index)) => {
                self.enum_entries.push(parser.parse_string()?);
                Ok(())
            }
            ("EnumFriendlyNames", Some(_index)) => {
                self.enum_friendly_names.push(parser.parse_text()?);
                Ok(())
            }
            ("PinNames", Some(_index)) => {
                self.pin_names.push(parser.parse_string()?);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_optional_export_text_path(buffer, indent, "Enum", &self.enum_type);

        // Serialize enum entries
        for (index, entry) in self.enum_entries.iter().enumerate() {
            buffer.push_str(indent);
            buffer.push_str(&format!("EnumEntries({})=\"", index));
            buffer.push_str(&escape_string(entry));
            buffer.push_str("\"\r\n");
        }

        // Serialize enum friendly names
        for (index, friendly_name) in self.enum_friendly_names.iter().enumerate() {
            if !friendly_name.as_str().is_empty() {
                buffer.push_str(indent);
                buffer.push_str(&format!("EnumFriendlyNames({})=", index));
                buffer.push_str(friendly_name.as_str());
                buffer.push_str("\r\n");
            }
        }

        // Serialize pin names
        for (index, pin_name) in self.pin_names.iter().enumerate() {
            buffer.push_str(indent);
            buffer.push_str(&format!("PinNames({})=\"", index));
            buffer.push_str(&escape_string(pin_name));
            buffer.push_str("\"\r\n");
        }

        // Base K2Node properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeEnumEquality - Represents an enum equality comparison
/// Based on UK2NodeEnumEquality : public UK2Node
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeEnumEquality {
    pub base: K2Node,
}

impl K2NodeEnumEquality {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeEnumEquality => K2Node);

impl ObjectTrait for K2NodeEnumEquality {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeEnumInequality - Represents an enum inequality comparison
/// Based on UK2NodeEnumInequality : public UK2NodeEnumEquality
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeEnumInequality {
    pub base: K2NodeEnumEquality,
}

impl K2NodeEnumInequality {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeEnumEquality::new(name),
        }
    }
}

impl_deref!(K2NodeEnumInequality => K2NodeEnumEquality);

impl ObjectTrait for K2NodeEnumInequality {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeGenericCreateObject - Represents a generic object creation node
/// Based on UK2NodeGenericCreateObject : public UK2Node_ConstructObjectFromClass
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeGenericCreateObject {
    pub base: K2Node,
}

impl K2NodeGenericCreateObject {
    pub fn new(name: String) -> Self {
        Self {
            base: K2Node::new(name),
        }
    }
}

impl_deref!(K2NodeGenericCreateObject => K2Node);

impl ObjectTrait for K2NodeGenericCreateObject {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        self.base.parse_property(key, parser)
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeStructMemberSet - Base class for struct member set operations
/// Based on UK2NodeStructMemberSet : public UK2NodeStructOperation
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeStructMemberSet {
    pub base: K2NodeStructOperation,
    pub show_pin_for_properties: Vec<FOptionalPinFromProperty>,
}

impl K2NodeStructMemberSet {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeStructOperation::new(name),
            show_pin_for_properties: Vec::new(),
        }
    }
}

impl_deref!(K2NodeStructMemberSet => K2NodeStructOperation);

impl ObjectTrait for K2NodeStructMemberSet {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("ShowPinForProperties", Some(_index)) => {
                // Parse the struct value: (PropertyName="...",PropertyFriendlyName="..."...)
                parser.skip_whitespace();
                let value_str = parser.parse_parenthesized_value()?;

                // Parse the struct - for now just store it as a default entry
                // A full implementation would parse all fields from value_str
                let mut prop = FOptionalPinFromProperty::default();

                // Parse PropertyName
                if let Some(start) = value_str.find("PropertyName=\"") {
                    let rest = &value_str[start + 14..];
                    if let Some(end) = rest.find('"') {
                        prop.property_name = rest[..end].to_string();
                    }
                }

                // Parse PropertyFriendlyName
                if let Some(start) = value_str.find("PropertyFriendlyName=\"") {
                    let rest = &value_str[start + 22..];
                    if let Some(end) = rest.find('"') {
                        prop.property_friendly_name = rest[..end].to_string();
                    }
                }

                // Parse PropertyTooltip
                if let Some(start) = value_str.find("PropertyTooltip=\"") {
                    let rest = &value_str[start + 17..];
                    if let Some(end) = rest.find('"') {
                        prop.property_tooltip = FText::literal(rest[..end].to_string());
                    }
                }

                // Parse CategoryName
                if let Some(start) = value_str.find("CategoryName=\"") {
                    let rest = &value_str[start + 14..];
                    if let Some(end) = rest.find('"') {
                        prop.category_name = rest[..end].to_string();
                    }
                }

                // Parse bShowPin
                if value_str.contains("bShowPin=True") {
                    prop.show_pin = true;
                }

                // Parse bCanToggleVisibility
                if value_str.contains("bCanToggleVisibility=True") {
                    prop.can_toggle_visibility = true;
                }

                self.show_pin_for_properties.push(prop);
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // StructMemberSet-specific properties
        serialize_show_pin_for_properties(buffer, indent, &self.show_pin_for_properties);
        // Base properties
        self.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// K2NodeMakeStruct - Represents a make struct node
/// Based on UK2NodeMakeStruct : public UK2NodeStructMemberSet
#[derive(Debug, Clone, PartialEq)]
pub struct K2NodeMakeStruct {
    pub base: K2NodeStructMemberSet,
    pub made_after_override_pin_removal: bool,
}

impl K2NodeMakeStruct {
    pub fn new(name: String) -> Self {
        Self {
            base: K2NodeStructMemberSet::new(name),
            made_after_override_pin_removal: false,
        }
    }
}

impl_deref!(K2NodeMakeStruct => K2NodeStructMemberSet);

impl ObjectTrait for K2NodeMakeStruct {
    fn parse_property(
        &mut self,
        key: (&str, Option<usize>),
        parser: &mut Parser,
    ) -> Result<(), ParseError> {
        match key {
            ("bMadeAfterOverridePinRemoval", None) => {
                self.made_after_override_pin_removal = parser.parse_bool()?;
                Ok(())
            }
            _ => self.base.parse_property(key, parser),
        }
    }

    fn parse_custom_property(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        self.base.parse_custom_property(parser)
    }

    fn serialize(&self, buffer: &mut String, indent: &str) {
        // MakeStruct-specific properties first
        serialize_bool(
            buffer,
            indent,
            "bMadeAfterOverridePinRemoval",
            self.made_after_override_pin_removal,
        );
        // StructMemberSet properties (from base)
        serialize_show_pin_for_properties(buffer, indent, &self.base.show_pin_for_properties);
        // Base properties (StructOperation and below)
        self.base.base.serialize(buffer, indent);
    }

    fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// Helper to extract class name from full path
/// E.g., "/Script/BlueprintGraph.K2NodeEvent" -> "K2NodeEvent"
pub fn extract_class_name(class_path: &str) -> Option<&str> {
    class_path.rsplit('.').next()
}

// ============================================================================
// Serialization support
// ============================================================================

/// Helper function to escape strings for serialization
pub fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('\'', "\\'")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Helper function to serialize a MemberReference
pub fn serialize_member_reference(buffer: &mut String, member_ref: &MemberReference) {
    let mut parts = Vec::new();

    if let Some(ref parent) = member_ref.member_parent {
        parts.push(format!("MemberParent={}", parent));
    }
    if let Some(ref scope) = member_ref.member_scope
        && !scope.is_empty()
    {
        parts.push(format!("MemberScope=\"{}\"", scope));
    }
    if let Some(ref name) = member_ref.member_name {
        parts.push(format!("MemberName=\"{}\"", name));
    }
    if let Some(ref guid) = member_ref.member_guid {
        parts.push(format!("MemberGuid={}", guid));
    }
    if member_ref.self_context {
        parts.push("bSelfContext=True".to_string());
    }
    if member_ref.was_deprecated {
        parts.push("bWasDeprecated=True".to_string());
    }

    if parts.is_empty() {
        buffer.push_str("()");
    } else {
        buffer.push('(');
        buffer.push_str(&parts.join(","));
        buffer.push(')');
    }
}

// ============================================================================
// Property serialization helpers
// ============================================================================

/// Serialize an integer property only if non-zero
pub fn serialize_int<T: std::fmt::Display + Default + PartialEq>(
    buffer: &mut String,
    indent: &str,
    name: &str,
    value: T,
) {
    if value != T::default() {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        use std::fmt::Write;
        let _ = write!(buffer, "{}", value);
        buffer.push_str("\r\n");
    }
}

/// Serialize a boolean property only if true
pub fn serialize_bool(buffer: &mut String, indent: &str, name: &str, value: bool) {
    if value {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push_str("=True\r\n");
    }
}

/// Serialize a bool property (always serializes, even for False)
pub fn serialize_bool_always(buffer: &mut String, indent: &str, name: &str, value: bool) {
    buffer.push_str(indent);
    buffer.push_str(name);
    buffer.push_str(if value { "=True\r\n" } else { "=False\r\n" });
}

/// Serialize a string property only if non-empty
pub fn serialize_string(buffer: &mut String, indent: &str, name: &str, value: &str) {
    if !value.is_empty() {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push_str("=\"");
        buffer.push_str(&escape_string(value));
        buffer.push_str("\"\r\n");
    }
}

/// Serialize a GUID property only if non-zero
pub fn serialize_guid(buffer: &mut String, indent: &str, name: &str, value: &Guid) {
    if !value.is_zero() {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        buffer.push_str(&value.to_hex_string());
        buffer.push_str("\r\n");
    }
}

/// Serialize ShowPinForProperties array
pub fn serialize_show_pin_for_properties(
    buffer: &mut String,
    indent: &str,
    properties: &[FOptionalPinFromProperty],
) {
    for (index, prop) in properties.iter().enumerate() {
        buffer.push_str(indent);
        buffer.push_str(&format!("ShowPinForProperties({})=(", index));

        let mut first = true;
        let mut add_comma = |buf: &mut String| {
            if !first {
                buf.push(',');
            }
            first = false;
        };

        if !prop.property_name.is_empty() {
            add_comma(buffer);
            buffer.push_str("PropertyName=\"");
            buffer.push_str(&escape_string(&prop.property_name));
            buffer.push('"');
        }

        if !prop.property_friendly_name.is_empty() {
            add_comma(buffer);
            buffer.push_str("PropertyFriendlyName=\"");
            buffer.push_str(&escape_string(&prop.property_friendly_name));
            buffer.push('"');
        }

        if !prop.property_tooltip.is_empty() {
            add_comma(buffer);
            buffer.push_str("PropertyTooltip=\"");
            buffer.push_str(&escape_string(prop.property_tooltip.as_str()));
            buffer.push('"');
        }

        if !prop.category_name.is_empty() {
            add_comma(buffer);
            buffer.push_str("CategoryName=\"");
            buffer.push_str(&escape_string(&prop.category_name));
            buffer.push('"');
        }

        if prop.show_pin {
            add_comma(buffer);
            buffer.push_str("bShowPin=True");
        }

        if prop.can_toggle_visibility {
            add_comma(buffer);
            buffer.push_str("bCanToggleVisibility=True");
        }

        if prop.property_is_customized {
            add_comma(buffer);
            buffer.push_str("bPropertyIsCustomized=True");
        }

        if prop.has_override_pin {
            add_comma(buffer);
            buffer.push_str("bHasOverridePin=True");
        }

        if prop.is_marked_for_advanced_display {
            add_comma(buffer);
            buffer.push_str("bIsMarkedForAdvancedDisplay=True");
        }

        if prop.is_override_enabled {
            add_comma(buffer);
            buffer.push_str("bIsOverrideEnabled=True");
        }

        if prop.is_set_value_pin_visible {
            add_comma(buffer);
            buffer.push_str("bIsSetValuePinVisible=True");
        }

        if prop.is_override_pin_visible {
            add_comma(buffer);
            buffer.push_str("bIsOverridePinVisible=True");
        }

        buffer.push_str(")\r\n");
    }
}

/// Serialize a MemberReference property only if non-default
pub fn serialize_member_ref(
    buffer: &mut String,
    indent: &str,
    name: &str,
    value: &MemberReference,
) {
    if !value.is_default() {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        serialize_member_reference(buffer, value);
        buffer.push_str("\r\n");
    }
}

/// Serialize an optional string property
pub fn serialize_optional_string(
    buffer: &mut String,
    indent: &str,
    name: &str,
    value: &Option<String>,
) {
    if let Some(s) = value
        && !s.is_empty()
    {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        buffer.push_str(s);
        buffer.push_str("\r\n");
    }
}

/// Serialize an optional ExportTextPath property (skips if None or empty)
pub fn serialize_optional_export_text_path(
    buffer: &mut String,
    indent: &str,
    name: &str,
    value: &Option<ExportTextPath>,
) {
    if let Some(path) = value
        && !path.is_empty()
    {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        buffer.push_str(&path.to_file_string());
        buffer.push_str("\r\n");
    }
}

/// Serialize an enum property only if non-default
pub fn serialize_enum<T: std::fmt::Display + Default + PartialEq>(
    buffer: &mut String,
    indent: &str,
    name: &str,
    value: T,
) {
    if value != T::default() {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        use std::fmt::Write;
        let _ = write!(buffer, "{}", value);
        buffer.push_str("\r\n");
    }
}

/// Serialize an optional FText property
pub fn serialize_optional_ftext(
    buffer: &mut String,
    indent: &str,
    name: &str,
    value: &Option<FText>,
) {
    if let Some(text) = value
        && !text.is_empty()
    {
        buffer.push_str(indent);
        buffer.push_str(name);
        buffer.push('=');
        buffer.push_str(&text.serialize());
        buffer.push_str("\r\n");
    }
}

/// Parse a Pin from CustomProperties Pin (...) format
pub fn parse_pin(parser: &mut Parser) -> Result<Pin, ParseError> {
    parser.expect_str("(")?;

    let mut pin = Pin::default();

    loop {
        parser.skip_whitespace();

        if parser.peek_str(")") {
            parser.advance(1);
            break;
        }

        // Parse property name
        let (key, _index) = parser.parse_property_name()?;
        parser.skip_whitespace();
        parser.expect_str("=")?;
        parser.skip_whitespace();

        // Parse property value based on key
        match key.as_str() {
            "PinId" => {
                pin.pin_id = parser.parse_guid()?;
            }
            "PinName" => {
                pin.pin_name = parser.parse_string()?;
            }
            "PinFriendlyName" => {
                pin.pin_friendly_name = Some(parser.parse_text()?);
            }
            "Direction" => {
                let dir_str = parser.parse_string()?;
                pin.direction =
                    EEdGraphPinDirection::parse(&dir_str).map_err(|e| parser.error(e))?;
            }
            "PinToolTip" => {
                pin.pin_tool_tip = parser.parse_string()?;
            }
            "DefaultValue" => {
                pin.default_value = parser.parse_string()?;
            }
            "AutogeneratedDefaultValue" => {
                pin.autogenerated_default_value = parser.parse_string()?;
            }
            "DefaultObject" => {
                let val = parser.parse_string()?;
                pin.default_object = if val == "None" || val.is_empty() {
                    None
                } else {
                    Some(val)
                };
            }
            "DefaultTextValue" => {
                pin.default_text_value = parser.parse_text()?;
            }
            "LinkedTo" => {
                // LinkedTo is an array of PinIdentifiers: (NodeName GUID,NodeName2 GUID2,)
                let value = parser.parse_parenthesized_value()?;
                if !value.is_empty() && value != "()" {
                    // Remove outer parentheses
                    let inner = value.trim_start_matches('(').trim_end_matches(')');
                    // Split by comma and parse each entry
                    for entry in inner.split(',') {
                        let entry = entry.trim();
                        if !entry.is_empty() {
                            let pin_id =
                                PinIdentifier::parse(entry).map_err(|e| parser.error(e))?;
                            pin.linked_to.push(pin_id);
                        }
                    }
                }
            }
            "SubPins" => {
                let value = parser.parse_parenthesized_value()?;
                if !value.is_empty() && value != "()" {
                    pin.sub_pins.push(value);
                }
            }
            "ParentPin" => {
                let val = parser.parse_string()?;
                pin.parent_pin = if val == "None" || val.is_empty() {
                    None
                } else {
                    Some(val)
                };
            }
            "ReferencePassThroughConnection" => {
                let val = parser.parse_string()?;
                pin.reference_pass_through_connection = if val == "None" || val.is_empty() {
                    None
                } else {
                    Some(val)
                };
            }
            "PersistentGuid" => {
                pin.persistent_guid = parser.parse_guid()?;
            }
            "bHidden" => {
                pin.hidden = parser.parse_bool()?;
            }
            "bNotConnectable" => {
                pin.not_connectable = parser.parse_bool()?;
            }
            "bDefaultValueIsReadOnly" => {
                pin.default_value_is_read_only = parser.parse_bool()?;
            }
            "bDefaultValueIsIgnored" => {
                pin.default_value_is_ignored = parser.parse_bool()?;
            }
            "bAdvancedView" => {
                pin.advanced_view = parser.parse_bool()?;
            }
            "bOrphanedPin" => {
                pin.orphaned_pin = parser.parse_bool()?;
            }
            key if key.starts_with("PinType.") => {
                // Parse PinType sub-properties
                let subkey = &key[8..]; // Skip "PinType."
                match subkey {
                    "PinCategory" => {
                        pin.pin_type.pin_category = parser.parse_string()?;
                    }
                    "PinSubCategory" => {
                        pin.pin_type.pin_sub_category = parser.parse_string()?;
                    }
                    "PinSubCategoryObject" => {
                        let val = parser.parse_string()?;
                        pin.pin_type.pin_sub_category_object = if val == "None" || val.is_empty() {
                            None
                        } else {
                            Some(ExportTextPath::parse(&val).map_err(|e| parser.error(e))?)
                        };
                    }
                    "PinSubCategoryMemberReference" => {
                        let member_ref = parser.parse_member_reference()?;
                        if !member_ref.is_default() {
                            pin.pin_type.pin_sub_category_member_reference = Some(member_ref);
                        }
                    }
                    "PinValueType" => {
                        pin.pin_type.pin_value_type = Some(parse_terminal_type(parser)?);
                    }
                    "ContainerType" => {
                        let val = parser.parse_string()?;
                        pin.pin_type.container_type = if val == "None" || val.is_empty() {
                            None
                        } else {
                            Some(val)
                        };
                    }
                    "bIsReference" => {
                        pin.pin_type.is_reference = parser.parse_bool()?;
                    }
                    "bIsConst" => {
                        pin.pin_type.is_const = parser.parse_bool()?;
                    }
                    "bIsWeakPointer" => {
                        pin.pin_type.is_weak_pointer = parser.parse_bool()?;
                    }
                    "bIsUObjectWrapper" => {
                        pin.pin_type.is_uobject_wrapper = parser.parse_bool()?;
                    }
                    _ => {
                        return Err(parser.error(format!("Unknown PinType property: {}", subkey)));
                    }
                }
            }
            _ => {
                return Err(parser.error(format!("Unknown Pin property: {}", key)));
            }
        }

        parser.skip_whitespace();
        if parser.peek_str(",") {
            parser.advance(1);
        }
    }

    Ok(pin)
}

/// Parse a PinType from a parenthesized struct format
/// Expected format: (PinCategory="...",PinSubCategoryObject=...,ContainerType=...)
pub fn parse_pin_type(parser: &mut Parser) -> Result<PinType, ParseError> {
    parser.expect_str("(")?;

    let mut pin_type = PinType::default();

    loop {
        parser.skip_whitespace();

        if parser.peek_str(")") {
            parser.advance(1);
            break;
        }

        // Parse property name
        let (key, _index) = parser.parse_property_name()?;
        parser.skip_whitespace();
        parser.expect_str("=")?;
        parser.skip_whitespace();

        // Parse property value based on key
        match key.as_str() {
            "PinCategory" => {
                pin_type.pin_category = parser.parse_string()?;
            }
            "PinSubCategory" => {
                pin_type.pin_sub_category = parser.parse_string()?;
            }
            "PinSubCategoryObject" => {
                let val = parser.parse_string()?;
                pin_type.pin_sub_category_object = if val == "None" || val.is_empty() {
                    None
                } else {
                    Some(ExportTextPath::parse(&val).map_err(|e| parser.error(e))?)
                };
            }
            "PinSubCategoryMemberReference" => {
                let member_ref = parser.parse_member_reference()?;
                if !member_ref.is_default() {
                    pin_type.pin_sub_category_member_reference = Some(member_ref);
                }
            }
            "PinValueType" => {
                pin_type.pin_value_type = Some(parse_terminal_type(parser)?);
            }
            "ContainerType" => {
                let val = parser.parse_string()?;
                pin_type.container_type = if val == "None" || val.is_empty() {
                    None
                } else {
                    Some(val)
                };
            }
            "bIsArray" => {
                // Note: bIsArray is deprecated in favor of ContainerType
                let _ = parser.parse_bool()?;
            }
            "bIsReference" => {
                pin_type.is_reference = parser.parse_bool()?;
            }
            "bIsConst" => {
                pin_type.is_const = parser.parse_bool()?;
            }
            "bIsWeakPointer" => {
                pin_type.is_weak_pointer = parser.parse_bool()?;
            }
            "bIsUObjectWrapper" => {
                pin_type.is_uobject_wrapper = parser.parse_bool()?;
            }
            _ => {
                return Err(parser.error(format!("Unknown PinType property: {}", key)));
            }
        }

        parser.skip_whitespace();
        if parser.peek_str(",") {
            parser.advance(1);
        }
    }

    Ok(pin_type)
}

/// Parse a FEdGraphTerminalType from a parenthesized struct format
/// Expected format: (TerminalCategory="struct",TerminalSubCategoryObject=ScriptStruct'"/Script/..."')
pub fn parse_terminal_type(parser: &mut Parser) -> Result<FEdGraphTerminalType, ParseError> {
    parser.expect_str("(")?;

    let mut terminal_type = FEdGraphTerminalType::default();

    loop {
        parser.skip_whitespace();

        if parser.peek_str(")") {
            parser.advance(1);
            break;
        }

        // Parse property name
        let (key, _index) = parser.parse_property_name()?;
        parser.skip_whitespace();
        parser.expect_str("=")?;
        parser.skip_whitespace();

        // Parse property value based on key
        match key.as_str() {
            "TerminalCategory" => {
                terminal_type.terminal_category = parser.parse_string()?;
            }
            "TerminalSubCategory" => {
                terminal_type.terminal_sub_category = parser.parse_string()?;
            }
            "TerminalSubCategoryObject" => {
                let val = parser.parse_string()?;
                terminal_type.terminal_sub_category_object = if val == "None" || val.is_empty() {
                    None
                } else {
                    Some(ExportTextPath::parse(&val).map_err(|e| parser.error(e))?)
                };
            }
            "bTerminalIsConst" => {
                terminal_type.terminal_is_const = parser.parse_bool()?;
            }
            "bTerminalIsWeakPointer" => {
                terminal_type.terminal_is_weak_pointer = parser.parse_bool()?;
            }
            "bTerminalIsUObjectWrapper" => {
                terminal_type.terminal_is_uobject_wrapper = parser.parse_bool()?;
            }
            _ => {
                return Err(parser.error(format!("Unknown FEdGraphTerminalType property: {}", key)));
            }
        }

        parser.skip_whitespace();
        if parser.peek_str(",") {
            parser.advance(1);
        }
    }

    Ok(terminal_type)
}

/// Serialize a FEdGraphTerminalType to parenthesized struct format
pub fn serialize_terminal_type(buffer: &mut String, terminal_type: &FEdGraphTerminalType) {
    buffer.push('(');
    let mut first = true;
    let mut add_comma = |buf: &mut String| {
        if !first {
            buf.push(',');
        }
        first = false;
    };

    // TerminalCategory
    if !terminal_type.terminal_category.is_empty() {
        add_comma(buffer);
        buffer.push_str("TerminalCategory=\"");
        buffer.push_str(&escape_string(&terminal_type.terminal_category));
        buffer.push('"');
    }

    // TerminalSubCategory
    if !terminal_type.terminal_sub_category.is_empty() {
        add_comma(buffer);
        buffer.push_str("TerminalSubCategory=\"");
        buffer.push_str(&escape_string(&terminal_type.terminal_sub_category));
        buffer.push('"');
    }

    // TerminalSubCategoryObject
    if let Some(ref obj) = terminal_type.terminal_sub_category_object {
        add_comma(buffer);
        buffer.push_str("TerminalSubCategoryObject=");
        buffer.push_str(&obj.to_string());
    }

    // bTerminalIsConst
    if terminal_type.terminal_is_const {
        add_comma(buffer);
        buffer.push_str("bTerminalIsConst=True");
    }

    // bTerminalIsWeakPointer
    if terminal_type.terminal_is_weak_pointer {
        add_comma(buffer);
        buffer.push_str("bTerminalIsWeakPointer=True");
    }

    // bTerminalIsUObjectWrapper
    if terminal_type.terminal_is_uobject_wrapper {
        add_comma(buffer);
        buffer.push_str("bTerminalIsUObjectWrapper=True");
    }

    buffer.push(')');
}

/// Serialize a Pin as a CustomProperties line
pub fn serialize_pin(buffer: &mut String, indent: &str, pin: &Pin) {
    buffer.push_str(indent);
    buffer.push_str("CustomProperties Pin (");

    let mut first = true;
    let mut add_comma = |buf: &mut String| {
        if !first {
            buf.push(',');
        }
        first = false;
    };

    // PinId - always present
    if !pin.pin_id.is_zero() {
        add_comma(buffer);
        buffer.push_str("PinId=");
        buffer.push_str(&pin.pin_id.to_hex_string());
    }

    // PinName - always present if not empty
    if !pin.pin_name.is_empty() {
        add_comma(buffer);
        buffer.push_str("PinName=\"");
        buffer.push_str(&escape_string(&pin.pin_name));
        buffer.push('"');
    }

    // PinFriendlyName - optional
    if let Some(ref friendly_name) = pin.pin_friendly_name
        && !friendly_name.is_empty()
    {
        add_comma(buffer);
        buffer.push_str("PinFriendlyName=");
        buffer.push_str(&friendly_name.serialize());
    }

    // PinToolTip - comes after PinFriendlyName, before Direction
    if !pin.pin_tool_tip.is_empty() {
        add_comma(buffer);
        buffer.push_str("PinToolTip=\"");
        buffer.push_str(&escape_string(&pin.pin_tool_tip));
        buffer.push('"');
    }

    // Direction - only serialize if it's Output (Input is the default and omitted)
    if pin.direction == EEdGraphPinDirection::Output {
        add_comma(buffer);
        buffer.push_str("Direction=\"");
        buffer.push_str(pin.direction.as_str());
        buffer.push('"');
    }

    // PinType properties - always serialize category, subcategory, and object even if empty/None
    if !pin.pin_type.pin_category.is_empty() {
        add_comma(buffer);
        buffer.push_str("PinType.PinCategory=\"");
        buffer.push_str(&escape_string(&pin.pin_type.pin_category));
        buffer.push('"');
    }

    // Always serialize PinSubCategory (even if empty)
    add_comma(buffer);
    buffer.push_str("PinType.PinSubCategory=\"");
    buffer.push_str(&escape_string(&pin.pin_type.pin_sub_category));
    buffer.push('"');

    // Always serialize PinSubCategoryObject (as "None" if None)
    add_comma(buffer);
    if let Some(ref obj) = pin.pin_type.pin_sub_category_object {
        buffer.push_str("PinType.PinSubCategoryObject=");
        buffer.push_str(&obj.to_string());
    } else {
        buffer.push_str("PinType.PinSubCategoryObject=None");
    }

    // Always serialize PinSubCategoryMemberReference (as () if None/default)
    add_comma(buffer);
    buffer.push_str("PinType.PinSubCategoryMemberReference=");
    if let Some(ref member_ref) = pin.pin_type.pin_sub_category_member_reference {
        serialize_member_reference(buffer, member_ref);
    } else {
        buffer.push_str("()");
    }

    if let Some(ref pin_value_type) = pin.pin_type.pin_value_type {
        add_comma(buffer);
        buffer.push_str("PinType.PinValueType=");
        serialize_terminal_type(buffer, pin_value_type);
    }

    // Always serialize ContainerType (as None if None, unquoted otherwise)
    add_comma(buffer);
    if let Some(ref container) = pin.pin_type.container_type {
        buffer.push_str("PinType.ContainerType=");
        buffer.push_str(container);
    } else {
        buffer.push_str("PinType.ContainerType=None");
    }

    // Always serialize boolean flags (as True or False)
    add_comma(buffer);
    buffer.push_str("PinType.bIsReference=");
    buffer.push_str(if pin.pin_type.is_reference {
        "True"
    } else {
        "False"
    });

    add_comma(buffer);
    buffer.push_str("PinType.bIsConst=");
    buffer.push_str(if pin.pin_type.is_const {
        "True"
    } else {
        "False"
    });

    add_comma(buffer);
    buffer.push_str("PinType.bIsWeakPointer=");
    buffer.push_str(if pin.pin_type.is_weak_pointer {
        "True"
    } else {
        "False"
    });

    add_comma(buffer);
    buffer.push_str("PinType.bIsUObjectWrapper=");
    buffer.push_str(if pin.pin_type.is_uobject_wrapper {
        "True"
    } else {
        "False"
    });

    // DefaultValue
    if !pin.default_value.is_empty() {
        add_comma(buffer);
        buffer.push_str("DefaultValue=\"");
        buffer.push_str(&escape_string(&pin.default_value));
        buffer.push('"');
    }

    // AutogeneratedDefaultValue
    if !pin.autogenerated_default_value.is_empty() {
        add_comma(buffer);
        buffer.push_str("AutogeneratedDefaultValue=\"");
        buffer.push_str(&escape_string(&pin.autogenerated_default_value));
        buffer.push('"');
    }

    // DefaultObject
    if let Some(ref obj) = pin.default_object {
        add_comma(buffer);
        buffer.push_str("DefaultObject=\"");
        buffer.push_str(obj);
        buffer.push('"');
    }

    // DefaultTextValue
    if !pin.default_text_value.is_empty() {
        add_comma(buffer);
        buffer.push_str("DefaultTextValue=");
        buffer.push_str(&pin.default_text_value.serialize());
    }

    // LinkedTo array - comes after default values
    if !pin.linked_to.is_empty() {
        add_comma(buffer);
        buffer.push_str("LinkedTo=(");
        for (i, link) in pin.linked_to.iter().enumerate() {
            if i > 0 {
                buffer.push(',');
            }
            buffer.push_str(&link.to_string());
        }
        // Add trailing comma (matches UE4 format)
        buffer.push(',');
        buffer.push(')');
    }

    // SubPins
    if !pin.sub_pins.is_empty() {
        add_comma(buffer);
        buffer.push_str("SubPins=(");
        for (i, sub_pin) in pin.sub_pins.iter().enumerate() {
            if i > 0 {
                buffer.push(',');
            }
            buffer.push_str(sub_pin);
        }
        buffer.push(')');
    }

    // ParentPin
    if let Some(ref parent) = pin.parent_pin {
        add_comma(buffer);
        buffer.push_str("ParentPin=\"");
        buffer.push_str(&escape_string(parent));
        buffer.push('"');
    }

    // ReferencePassThroughConnection
    if let Some(ref conn) = pin.reference_pass_through_connection {
        add_comma(buffer);
        buffer.push_str("ReferencePassThroughConnection=\"");
        buffer.push_str(&escape_string(conn));
        buffer.push('"');
    }

    // PersistentGuid - always serialize even if zero
    add_comma(buffer);
    buffer.push_str("PersistentGuid=");
    buffer.push_str(&pin.persistent_guid.to_hex_string());

    // Boolean flags - always serialize (as True or False)
    add_comma(buffer);
    buffer.push_str("bHidden=");
    buffer.push_str(if pin.hidden { "True" } else { "False" });

    add_comma(buffer);
    buffer.push_str("bNotConnectable=");
    buffer.push_str(if pin.not_connectable { "True" } else { "False" });

    add_comma(buffer);
    buffer.push_str("bDefaultValueIsReadOnly=");
    buffer.push_str(if pin.default_value_is_read_only {
        "True"
    } else {
        "False"
    });

    add_comma(buffer);
    buffer.push_str("bDefaultValueIsIgnored=");
    buffer.push_str(if pin.default_value_is_ignored {
        "True"
    } else {
        "False"
    });

    add_comma(buffer);
    buffer.push_str("bAdvancedView=");
    buffer.push_str(if pin.advanced_view { "True" } else { "False" });

    add_comma(buffer);
    buffer.push_str("bOrphanedPin=");
    buffer.push_str(if pin.orphaned_pin { "True" } else { "False" });

    // Add trailing comma before closing paren (matches UE4 format)
    buffer.push(',');

    buffer.push_str(")\r\n");
}

/// Parse a UserDefinedPin from a custom property
pub fn parse_user_defined_pin(parser: &mut Parser) -> Result<UserDefinedPin, ParseError> {
    parser.expect_str("(")?;

    let mut pin_name = String::new();
    let mut pin_type = PinType::default();
    let mut desired_direction = EEdGraphPinDirection::Input;
    let mut default_value = None;

    loop {
        parser.skip_whitespace();

        if parser.peek_str(")") {
            parser.advance(1);
            break;
        }

        // Parse property name
        let (key, _index) = parser.parse_property_name()?;
        parser.skip_whitespace();
        parser.expect_str("=")?;
        parser.skip_whitespace();

        // Parse property value based on key
        match key.as_str() {
            "PinName" => {
                pin_name = parser.parse_string()?;
            }
            "PinType" => {
                pin_type = parse_pin_type(parser)?;
            }
            "DesiredPinDirection" => {
                let dir_str = parser.parse_string()?;
                desired_direction =
                    EEdGraphPinDirection::parse(&dir_str).map_err(|e| parser.error(e))?;
            }
            "PinDefaultValue" => {
                default_value = Some(parser.parse_string()?);
            }
            _ => {
                return Err(parser.error(format!("Unknown UserDefinedPin property: {}", key)));
            }
        }

        parser.skip_whitespace();
        if parser.peek_str(",") {
            parser.advance(1);
        }
    }

    Ok(UserDefinedPin {
        pin_name,
        pin_type,
        desired_direction,
        default_value,
    })
}

/// Serialize a UserDefinedPin as a custom property
pub fn serialize_user_defined_pin(buffer: &mut String, indent: &str, pin: &UserDefinedPin) {
    buffer.push_str(indent);
    buffer.push_str("CustomProperties UserDefinedPin (");

    let mut first = true;
    let mut add_comma = |buf: &mut String| {
        if !first {
            buf.push(',');
        }
        first = false;
    };

    // PinName
    add_comma(buffer);
    buffer.push_str("PinName=\"");
    buffer.push_str(&escape_string(&pin.pin_name));
    buffer.push('"');

    // PinType
    add_comma(buffer);
    buffer.push_str("PinType=");
    serialize_pin_type_value(&pin.pin_type, buffer);

    // DesiredPinDirection
    add_comma(buffer);
    buffer.push_str("DesiredPinDirection=");
    buffer.push_str(pin.desired_direction.as_str());

    // PinDefaultValue
    if let Some(ref default_value) = pin.default_value {
        add_comma(buffer);
        buffer.push_str("PinDefaultValue=\"");
        buffer.push_str(&escape_string(default_value));
        buffer.push('"');
    }

    buffer.push_str(")\r\n");
}

/// Serialize a PinType value to parenthesized struct format
pub fn serialize_pin_type_value(pin_type: &PinType, buffer: &mut String) {
    buffer.push('(');
    let mut first = true;
    let mut add_comma = |buf: &mut String| {
        if !first {
            buf.push(',');
        }
        first = false;
    };

    // PinCategory
    if !pin_type.pin_category.is_empty() {
        add_comma(buffer);
        buffer.push_str("PinCategory=\"");
        buffer.push_str(&escape_string(&pin_type.pin_category));
        buffer.push('"');
    }

    // PinSubCategory
    if !pin_type.pin_sub_category.is_empty() {
        add_comma(buffer);
        buffer.push_str("PinSubCategory=\"");
        buffer.push_str(&escape_string(&pin_type.pin_sub_category));
        buffer.push('"');
    }

    // PinSubCategoryObject
    if let Some(ref obj) = pin_type.pin_sub_category_object {
        add_comma(buffer);
        buffer.push_str("PinSubCategoryObject=");
        buffer.push_str(&obj.to_string());
    }

    // PinSubCategoryMemberReference
    if let Some(ref member_ref) = pin_type.pin_sub_category_member_reference
        && !member_ref.is_default()
    {
        add_comma(buffer);
        buffer.push_str("PinSubCategoryMemberReference=");
        serialize_member_reference(buffer, member_ref);
    }

    // PinValueType
    if let Some(ref pin_value_type) = pin_type.pin_value_type {
        add_comma(buffer);
        buffer.push_str("PinValueType=");
        serialize_terminal_type(buffer, pin_value_type);
    }

    // ContainerType
    if let Some(ref container) = pin_type.container_type {
        add_comma(buffer);
        buffer.push_str("ContainerType=");
        buffer.push_str(container);
    }

    // Boolean flags - only serialize if true
    if pin_type.is_reference {
        add_comma(buffer);
        buffer.push_str("bIsReference=True");
    }

    if pin_type.is_const {
        add_comma(buffer);
        buffer.push_str("bIsConst=True");
    }

    if pin_type.is_weak_pointer {
        add_comma(buffer);
        buffer.push_str("bIsWeakPointer=True");
    }

    if pin_type.is_uobject_wrapper {
        add_comma(buffer);
        buffer.push_str("bIsUObjectWrapper=True");
    }

    buffer.push(')');
}

// Serialization implementations for base classes

impl EdGraphNode {
    /// Serialize EdGraphNode properties
    pub fn serialize(&self, buffer: &mut String, indent: &str) {
        serialize_int(buffer, indent, "NodePosX", self.node_pos_x);
        serialize_int(buffer, indent, "NodePosY", self.node_pos_y);
        serialize_int(buffer, indent, "NodeWidth", self.node_width);
        serialize_int(buffer, indent, "NodeHeight", self.node_height);
        serialize_enum(
            buffer,
            indent,
            "AdvancedPinDisplay",
            self.advanced_pin_display,
        );
        serialize_int(buffer, indent, "ErrorType", self.error_type);
        serialize_string(buffer, indent, "ErrorMsg", &self.error_msg);
        // Serialize if True (default is False for regular nodes)
        serialize_bool(
            buffer,
            indent,
            "bCommentBubblePinned",
            self.comment_bubble_pinned,
        );
        serialize_bool(
            buffer,
            indent,
            "bCommentBubbleVisible",
            self.comment_bubble_visible,
        );
        serialize_string(buffer, indent, "NodeComment", &self.node_comment);
        serialize_guid(buffer, indent, "NodeGuid", &self.node_guid);
    }

    /// Serialize custom properties like Pins
    pub fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        // Serialize Pins as CustomProperties
        for pin in &self.pins {
            serialize_pin(buffer, indent, pin);
        }
    }
}

impl K2Node {
    pub fn serialize(&self, buffer: &mut String, indent: &str) {
        self.base.serialize(buffer, indent);
    }

    pub fn serialize_custom_properties(&self, buffer: &mut String, indent: &str) {
        self.base.serialize_custom_properties(buffer, indent);
    }
}

/// Parse FKismetUserDeclaredFunctionMetadata from a parenthesized struct
/// Expected format: (Category=NSLOCTEXT(...),Tooltip=...,Keywords=...)
pub fn parse_function_metadata(
    parser: &mut Parser,
) -> Result<FKismetUserDeclaredFunctionMetadata, ParseError> {
    parser.expect_str("(")?;

    let mut metadata = FKismetUserDeclaredFunctionMetadata::default();

    loop {
        parser.skip_whitespace();

        if parser.peek_str(")") {
            parser.advance(1);
            break;
        }

        // Parse property name
        let (key, _index) = parser.parse_property_name()?;
        parser.skip_whitespace();
        parser.expect_str("=")?;
        parser.skip_whitespace();

        // Parse property value based on key
        match key.as_str() {
            "Tooltip" => {
                metadata.tooltip = Some(parser.parse_text()?);
            }
            "Category" => {
                metadata.category = Some(parser.parse_text()?);
            }
            "Keywords" => {
                metadata.keywords = Some(parser.parse_text()?);
            }
            "CompactNodeTitle" => {
                metadata.compact_node_title = Some(parser.parse_text()?);
            }
            "InstanceTitleColor" => {
                let color_str = parser.parse_string()?;
                metadata.instance_title_color =
                    Some(FLinearColor::parse(&color_str).map_err(|e| parser.error(e))?);
            }
            "DeprecationMessage" => {
                metadata.deprecation_message = Some(parser.parse_string()?);
            }
            "bIsDeprecated" => {
                metadata.is_deprecated = parser.parse_bool()?;
            }
            "bCallInEditor" => {
                metadata.call_in_editor = parser.parse_bool()?;
            }
            "HasLatentFunctions" => {
                metadata.has_latent_functions = parser.parse_int()?;
            }
            _ => {
                return Err(parser.error(format!(
                    "Unknown FKismetUserDeclaredFunctionMetadata property: {}",
                    key
                )));
            }
        }

        parser.skip_whitespace();
        if parser.peek_str(",") {
            parser.advance(1);
        }
    }

    Ok(metadata)
}

/// Parse LocalVariable from a parenthesized struct
/// Expected format: (VarName="...",VarGuid=...,VarType=(...),FriendlyName=...,Category=...,PropertyFlags=...)
pub fn parse_local_variable(parser: &mut Parser) -> Result<FBPVariableDescription, ParseError> {
    parser.expect_str("(")?;

    let mut var_name = String::new();
    let mut var_guid = String::new();
    let mut var_type = PinType::default();
    let mut friendly_name = None;
    let mut category = None;
    let mut property_flags = None;

    loop {
        parser.skip_whitespace();

        if parser.peek_str(")") {
            parser.advance(1);
            break;
        }

        // Parse property name
        let (key, _index) = parser.parse_property_name()?;
        parser.skip_whitespace();
        parser.expect_str("=")?;
        parser.skip_whitespace();

        // Parse property value based on key
        match key.as_str() {
            "VarName" => {
                var_name = parser.parse_string()?;
            }
            "VarGuid" => {
                var_guid = parser.parse_string()?;
            }
            "VarType" => {
                var_type = parser.parse_pin_type()?;
            }
            "FriendlyName" => {
                friendly_name = Some(parser.parse_string()?);
            }
            "Category" => {
                category = Some(parser.parse_text()?);
            }
            "PropertyFlags" => {
                property_flags = Some(parser.parse_int()?);
            }
            _ => {
                return Err(parser.error(format!("Unknown LocalVariable property: {}", key)));
            }
        }

        parser.skip_whitespace();
        if parser.peek_str(",") {
            parser.advance(1);
        }
    }

    Ok(FBPVariableDescription {
        var_name,
        var_guid,
        var_type,
        friendly_name,
        category,
        property_flags,
    })
}

/// Check if metadata is default (all fields are empty/default)
fn is_metadata_default(metadata: &FKismetUserDeclaredFunctionMetadata) -> bool {
    metadata.tooltip.is_none()
        && metadata.category.is_none()
        && metadata.keywords.is_none()
        && metadata.compact_node_title.is_none()
        && metadata.instance_title_color.is_none()
        && metadata.deprecation_message.is_none()
        && !metadata.is_deprecated
        && !metadata.call_in_editor
        && metadata.has_latent_functions == 0
}

/// Serialize FKismetUserDeclaredFunctionMetadata
fn serialize_function_metadata(
    buffer: &mut String,
    indent: &str,
    metadata: &FKismetUserDeclaredFunctionMetadata,
) {
    buffer.push_str(indent);
    buffer.push_str("MetaData=(");

    let mut first = true;
    let mut add_comma = |buf: &mut String| {
        if !first {
            buf.push(',');
        }
        first = false;
    };

    if let Some(ref tooltip) = metadata.tooltip {
        add_comma(buffer);
        buffer.push_str("Tooltip=");
        buffer.push_str(&tooltip.serialize());
    }

    if let Some(ref category) = metadata.category {
        add_comma(buffer);
        buffer.push_str("Category=");
        buffer.push_str(&category.serialize());
    }

    if let Some(ref keywords) = metadata.keywords {
        add_comma(buffer);
        buffer.push_str("Keywords=");
        buffer.push_str(&keywords.serialize());
    }

    if let Some(ref compact_title) = metadata.compact_node_title {
        add_comma(buffer);
        buffer.push_str("CompactNodeTitle=");
        buffer.push_str(&compact_title.serialize());
    }

    if let Some(ref color) = metadata.instance_title_color {
        add_comma(buffer);
        buffer.push_str("InstanceTitleColor=");
        buffer.push_str(&color.serialize());
    }

    if let Some(ref msg) = metadata.deprecation_message {
        add_comma(buffer);
        buffer.push_str("DeprecationMessage=\"");
        buffer.push_str(&escape_string(msg));
        buffer.push('"');
    }

    if metadata.is_deprecated {
        add_comma(buffer);
        buffer.push_str("bIsDeprecated=True");
    }

    if metadata.call_in_editor {
        add_comma(buffer);
        buffer.push_str("bCallInEditor=True");
    }

    if metadata.has_latent_functions != 0 {
        add_comma(buffer);
        buffer.push_str(&format!(
            "HasLatentFunctions={}",
            metadata.has_latent_functions
        ));
    }

    buffer.push_str(")\r\n");
}

/// Serialize LocalVariable
fn serialize_local_variable(
    buffer: &mut String,
    indent: &str,
    index: usize,
    var: &FBPVariableDescription,
) {
    buffer.push_str(indent);
    buffer.push_str(&format!("LocalVariables({})=(", index));

    let mut first = true;
    let mut add_comma = |buf: &mut String| {
        if !first {
            buf.push(',');
        }
        first = false;
    };

    // VarName
    add_comma(buffer);
    buffer.push_str("VarName=\"");
    buffer.push_str(&escape_string(&var.var_name));
    buffer.push('"');

    // VarGuid
    add_comma(buffer);
    buffer.push_str("VarGuid=");
    buffer.push_str(&var.var_guid);

    // VarType
    add_comma(buffer);
    buffer.push_str("VarType=");
    serialize_pin_type_value(&var.var_type, buffer);

    // FriendlyName
    if let Some(ref name) = var.friendly_name {
        add_comma(buffer);
        buffer.push_str("FriendlyName=\"");
        buffer.push_str(&escape_string(name));
        buffer.push('"');
    }

    // Category
    if let Some(ref cat) = var.category {
        add_comma(buffer);
        buffer.push_str("Category=");
        buffer.push_str(&cat.serialize());
    }

    // PropertyFlags
    if let Some(flags) = var.property_flags {
        add_comma(buffer);
        buffer.push_str(&format!("PropertyFlags={}", flags));
    }

    buffer.push_str(")\r\n");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_class_name() {
        assert_eq!(
            extract_class_name("/Script/BlueprintGraph.K2NodeEvent"),
            Some("K2NodeEvent")
        );
        assert_eq!(
            extract_class_name("/Script/UMG.VerticalBox"),
            Some("VerticalBox")
        );
        assert_eq!(extract_class_name("SimpleClass"), Some("SimpleClass"));
    }

    #[test]
    fn test_guid_from_bytes() {
        let bytes = [
            0x5A, 0xC7, 0x72, 0x68, 0x67, 0x5F, 0x47, 0x82, 0x87, 0x23, 0x17, 0xD5, 0x96, 0x81,
            0x60, 0x38,
        ];
        let guid = Guid::from_bytes(bytes);
        assert_eq!(guid.as_bytes(), &bytes);
    }

    #[test]
    fn test_guid_zero() {
        let guid = Guid::zero();
        assert!(guid.is_zero());
        assert_eq!(guid.to_hex_string(), "00000000000000000000000000000000");
    }

    #[test]
    fn test_guid_parse_hex() {
        let guid = Guid::parse_hex("5AC77268675F4782872317D596816038").unwrap();
        assert_eq!(guid.as_bytes()[0], 0x5A);
        assert_eq!(guid.as_bytes()[1], 0xC7);
        assert_eq!(guid.as_bytes()[15], 0x38);
    }

    #[test]
    fn test_guid_parse_hex_lowercase() {
        let guid = Guid::parse_hex("5ac77268675f4782872317d596816038").unwrap();
        assert_eq!(guid.as_bytes()[0], 0x5A);
        assert_eq!(guid.as_bytes()[1], 0xC7);
    }

    #[test]
    fn test_guid_parse_hex_invalid_length() {
        let result = Guid::parse_hex("5AC77268675F478287231");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("32 hex characters"));
    }

    #[test]
    fn test_guid_to_hex_string() {
        let bytes = [
            0x5A, 0xC7, 0x72, 0x68, 0x67, 0x5F, 0x47, 0x82, 0x87, 0x23, 0x17, 0xD5, 0x96, 0x81,
            0x60, 0x38,
        ];
        let guid = Guid::from_bytes(bytes);
        assert_eq!(guid.to_hex_string(), "5AC77268675F4782872317D596816038");
    }

    #[test]
    fn test_guid_roundtrip() {
        let original = "5AC77268675F4782872317D596816038";
        let guid = Guid::parse_hex(original).unwrap();
        assert_eq!(guid.to_hex_string(), original);
    }

    #[test]
    fn test_guid_display() {
        let guid = Guid::parse_hex("5AC77268675F4782872317D596816038").unwrap();
        assert_eq!(format!("{}", guid), "5AC77268675F4782872317D596816038");
        assert_eq!(
            format!("{:?}", guid),
            "Guid(\"5AC77268675F4782872317D596816038\")"
        );
    }
}
