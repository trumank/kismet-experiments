use crate::bytecode::address_index::AddressIndex;

use super::expr::{Expr, ExprKind};
use super::logger::{Logger, NullLogger};
use super::types::BytecodeOffset;
use std::collections::{HashMap, HashSet};

/// Unique identifier for a basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub usize);

/// Represents a unique (block, stack) state for flow-sensitive CFG analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BlockStackState {
    block_id: BlockId,
    stack: Vec<BytecodeOffset>,
}

/// Control flow terminator - the control flow decision at the end of a basic block
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Unconditional jump to target block
    Goto { target: BlockId },

    /// Conditional branch: if !condition goto false_target else true_target
    /// After flow analysis, both targets are statically known
    Branch {
        condition: Expr,
        true_target: BlockId,
        false_target: BlockId,
    },

    /// Dynamic jump with multiple or unknown targets (from pop operations)
    /// Actual targets are in the block's successors list
    /// Used when flow analysis determines >2 targets or when pop cannot be statically resolved
    DynamicJump,

    /// Return from function
    Return(Expr),

    /// No terminator - used when control flow is implicit (e.g., loop bodies)
    None,
}

/// A basic block - a maximal sequence of instructions with:
/// - Single entry point (first instruction)
/// - Single exit point (last instruction)
/// - No branches except at the end
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    /// Starting bytecode offset
    pub start_offset: BytecodeOffset,
    /// Ending bytecode offset (inclusive)
    pub end_offset: BytecodeOffset,
    /// Regular statements (assignments, calls, etc.)
    pub statements: Vec<Expr>,
    /// Control flow decision at end of block
    pub terminator: Terminator,
    /// Original terminator expression (for debugging)
    pub terminator_expr: Option<Expr>,
    /// Blocks that can follow this one
    pub successors: Vec<BlockId>,
    /// Blocks that can precede this one
    pub predecessors: Vec<BlockId>,
}

impl BasicBlock {
    pub fn new(id: BlockId, start_offset: BytecodeOffset) -> Self {
        Self {
            id,
            start_offset,
            end_offset: start_offset,
            statements: Vec::new(),
            terminator: Terminator::DynamicJump, // Temporary - will be set during edge building
            terminator_expr: None,
            successors: Vec::new(),
            predecessors: Vec::new(),
        }
    }
}

/// Control Flow Graph - represents the control flow structure of bytecode
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub blocks: Vec<BasicBlock>,
    pub entry_block: BlockId,
    /// Map from bytecode offset to block ID
    pub offset_to_block: HashMap<BytecodeOffset, BlockId>,
}

impl ControlFlowGraph {
    /// Build a CFG from a flat list of expressions
    pub fn from_expressions(expressions: &[Expr]) -> Self {
        Self::from_expressions_with_logger(expressions, &NullLogger)
    }

    /// Build a CFG from a flat list of expressions with a custom logger
    pub fn from_expressions_with_logger(expressions: &[Expr], logger: &dyn Logger) -> Self {
        if expressions.is_empty() {
            return Self {
                blocks: Vec::new(),
                entry_block: BlockId(0),
                offset_to_block: HashMap::new(),
            };
        }

        // Step 1: Identify leaders (first instruction of each basic block)
        let leaders = Self::identify_leaders(expressions);

        // Step 2: Create basic blocks
        let (blocks, offset_to_block) = Self::create_basic_blocks(expressions, &leaders);

        // Step 3: Build edges between blocks
        let blocks = Self::build_edges(expressions, blocks, &offset_to_block, logger);

        Self {
            blocks,
            entry_block: BlockId(0),
            offset_to_block,
        }
    }

    /// Identify leader instructions (start of basic blocks)
    /// Leaders are:
    /// 1. The first instruction
    /// 2. Any instruction that is a jump target
    /// 3. Any instruction immediately following a jump/branch
    fn identify_leaders(expressions: &[Expr]) -> HashSet<BytecodeOffset> {
        let mut leaders = HashSet::new();

        // First instruction is always a leader
        if let Some(first) = expressions.first() {
            leaders.insert(first.offset);
        }

        // Find all jump targets and instructions after jumps
        for (i, expr) in expressions.iter().enumerate() {
            match &expr.kind {
                ExprKind::Jump { target } => {
                    // Target is a leader
                    leaders.insert(*target);
                    // Instruction after jump is a leader (if it exists)
                    if let Some(next) = expressions.get(i + 1) {
                        leaders.insert(next.offset);
                    }
                }
                ExprKind::JumpIfNot { target, .. } => {
                    // Target is a leader
                    leaders.insert(*target);
                    // Instruction after conditional jump is a leader (fallthrough)
                    if let Some(next) = expressions.get(i + 1) {
                        leaders.insert(next.offset);
                    }
                }
                ExprKind::SwitchValue {
                    cases, end_offset, ..
                } => {
                    // All case targets are leaders
                    for case in cases {
                        leaders.insert(case.case_offset);
                        leaders.insert(case.next_offset);
                    }
                    // End offset is a leader
                    leaders.insert(*end_offset);
                    // Default case (next instruction) is a leader
                    if let Some(next) = expressions.get(i + 1) {
                        leaders.insert(next.offset);
                    }
                }
                ExprKind::PushExecutionFlow { push_offset } => {
                    // Target is a leader
                    leaders.insert(*push_offset);
                }
                ExprKind::PopExecutionFlow => {
                    // Unconditional jump to stack top - instruction after is a leader
                    if let Some(next) = expressions.get(i + 1) {
                        leaders.insert(next.offset);
                    }
                }
                ExprKind::PopExecutionFlowIfNot { .. } => {
                    // Conditional jump to stack top - instruction after is a leader (fallthrough)
                    if let Some(next) = expressions.get(i + 1) {
                        leaders.insert(next.offset);
                    }
                }
                ExprKind::Return(_) => {
                    // Instruction after return is a leader (if it exists)
                    if let Some(next) = expressions.get(i + 1) {
                        leaders.insert(next.offset);
                    }
                }
                _ => {}
            }
        }

        leaders
    }

    /// Create basic blocks from leaders
    fn create_basic_blocks(
        expressions: &[Expr],
        leaders: &HashSet<BytecodeOffset>,
    ) -> (Vec<BasicBlock>, HashMap<BytecodeOffset, BlockId>) {
        let mut blocks = Vec::new();
        let mut offset_to_block = HashMap::new();
        let mut current_block: Option<BasicBlock> = None;

        for expr in expressions.iter() {
            // Start a new block if this is a leader
            if leaders.contains(&expr.offset) {
                // Save the previous block
                if let Some(block) = current_block.take() {
                    blocks.push(block);
                }

                // Start new block
                let block_id = BlockId(blocks.len());
                offset_to_block.insert(expr.offset, block_id);
                current_block = Some(BasicBlock::new(block_id, expr.offset));
            }

            // Add expression to current block
            if let Some(ref mut block) = current_block {
                // Check if this is a terminator expression
                let is_terminator = matches!(
                    expr.kind,
                    ExprKind::Jump { .. }
                        | ExprKind::JumpIfNot { .. }
                        | ExprKind::PopExecutionFlow
                        | ExprKind::PopExecutionFlowIfNot { .. }
                        | ExprKind::Return(_)
                );

                if is_terminator {
                    // Store the terminator expression for later resolution
                    // Actual Terminator will be constructed in build_edges after flow analysis
                    block.terminator_expr = Some(expr.clone());
                } else {
                    // Regular statement - add to statements vec
                    block.statements.push(expr.clone());
                }

                block.end_offset = expr.offset;
            }
        }

        // Save the last block
        if let Some(block) = current_block {
            blocks.push(block);
        }

        (blocks, offset_to_block)
    }

    /// Helper to find the next block after the given block's end offset
    fn find_next_block(
        block_end_offset: BytecodeOffset,
        expressions: &[Expr],
        offset_to_block: &HashMap<BytecodeOffset, BlockId>,
    ) -> Option<BlockId> {
        // Find the first expression after block_end_offset
        for expr in expressions {
            if expr.offset.as_usize() > block_end_offset.as_usize() {
                return offset_to_block.get(&expr.offset).copied();
            }
        }
        None
    }

    /// Build edges between blocks based on control flow
    /// Uses flow-sensitive analysis to track execution flow stack for push/pop
    fn build_edges(
        expressions: &[Expr],
        mut blocks: Vec<BasicBlock>,
        offset_to_block: &HashMap<BytecodeOffset, BlockId>,
        logger: &dyn Logger,
    ) -> Vec<BasicBlock> {
        // Limits to prevent infinite state exploration
        const MAX_STACK_DEPTH: usize = 100;
        const MAX_STATES_EXPLORED: usize = 10_000;

        // Build a temporary map for quick lookup
        let mut successors_map: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

        // Track execution flow stack state for each block
        // Note: A block can have multiple stack states, so we track all of them
        let mut block_stack_states: HashMap<BlockId, Vec<Vec<BytecodeOffset>>> = HashMap::new();
        let mut worklist: Vec<(BlockId, Vec<BytecodeOffset>)> = Vec::new();
        // Track which (block, stack) combinations we've already processed
        let mut visited: HashSet<BlockStackState> = HashSet::new();

        // Start with entry block and empty stack
        worklist.push((BlockId(0), Vec::new()));

        while let Some((block_id, mut stack)) = worklist.pop() {
            // Limit the number of states we explore to prevent infinite loops
            if visited.len() >= MAX_STATES_EXPLORED {
                logger.warn(&format!(
                    "CFG analysis hit state limit ({}), some execution paths may be incomplete",
                    MAX_STATES_EXPLORED
                ));
                break;
            }

            // Limit stack depth to prevent unbounded growth
            if stack.len() > MAX_STACK_DEPTH {
                logger.warn(&format!(
                    "Execution flow stack depth exceeded {} at block {:?}, truncating",
                    MAX_STACK_DEPTH, block_id
                ));
                // Truncate stack to prevent unbounded growth
                stack.truncate(MAX_STACK_DEPTH);
            }

            // Skip if we've already processed this block with this exact stack state
            let state = BlockStackState {
                block_id,
                stack: stack.clone(),
            };
            if visited.contains(&state) {
                continue;
            }
            visited.insert(state);

            // Track this stack state for the block (blocks can have multiple states)
            block_stack_states
                .entry(block_id)
                .or_default()
                .push(stack.clone());

            let block = &blocks[block_id.0];
            let mut block_successors = Vec::new();

            // Process statements in the block to update stack state
            for stmt in &block.statements {
                match &stmt.kind {
                    ExprKind::PushExecutionFlow { push_offset } => {
                        stack.push(*push_offset);
                    }
                    ExprKind::SwitchValue {
                        cases, end_offset, ..
                    } => {
                        // Switch is a statement, not a terminator
                        // Multiple successors
                        let mut targets = HashSet::new();
                        for case in cases {
                            targets.insert(case.case_offset);
                        }
                        targets.insert(*end_offset);

                        for target in targets {
                            if let Some(&target_block) = offset_to_block.get(&target) {
                                block_successors.push(target_block);
                                worklist.push((target_block, stack.clone()));
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Process the terminator expression for control flow
            if let Some(ref term_expr) = block.terminator_expr {
                match &term_expr.kind {
                    ExprKind::Jump { target } => {
                        // Unconditional jump
                        if let Some(&target_block) = offset_to_block.get(target) {
                            block_successors.push(target_block);
                            worklist.push((target_block, stack.clone()));
                        }
                    }
                    ExprKind::JumpIfNot { target, .. } => {
                        // Conditional branch
                        // false branch: jump to target
                        if let Some(&false_target) = offset_to_block.get(target) {
                            block_successors.push(false_target);
                            worklist.push((false_target, stack.clone()));
                        }
                        // true branch: fallthrough to next block
                        if let Some(true_target) =
                            Self::find_next_block(block.end_offset, expressions, offset_to_block)
                        {
                            block_successors.push(true_target);
                            worklist.push((true_target, stack.clone()));
                        }
                    }
                    ExprKind::PopExecutionFlowIfNot { .. } => {
                        // Conditional pop
                        // 1. If condition is false, pop and jump to stack top
                        if let Some(&target_offset) = stack.last()
                            && let Some(&target_block) = offset_to_block.get(&target_offset)
                        {
                            block_successors.push(target_block);
                            let mut popped_stack = stack.clone();
                            popped_stack.pop();
                            worklist.push((target_block, popped_stack));
                        }

                        // 2. If condition is true, fallthrough
                        if let Some(fallthrough_target) =
                            Self::find_next_block(block.end_offset, expressions, offset_to_block)
                        {
                            block_successors.push(fallthrough_target);
                            worklist.push((fallthrough_target, stack.clone()));
                        }
                    }
                    ExprKind::PopExecutionFlow => {
                        // Unconditional pop from stack
                        if let Some(target_offset) = stack.pop()
                            && let Some(&target_block) = offset_to_block.get(&target_offset)
                        {
                            block_successors.push(target_block);
                            worklist.push((target_block, stack.clone()));
                        }
                    }
                    ExprKind::Return(_) => {
                        // No successors - exit block
                    }
                    _ => unreachable!("Invalid terminator expression"),
                }
            } else {
                // No terminator expression - fallthrough to next block
                if let Some(next_block) =
                    Self::find_next_block(block.end_offset, expressions, offset_to_block)
                {
                    block_successors.push(next_block);
                    worklist.push((next_block, stack.clone()));
                }
            }

            // Accumulate successors for this block (since we may visit it multiple times with different stacks)
            successors_map
                .entry(block_id)
                .or_default()
                .extend(block_successors);
        }

        // Deduplicate successors while preserving order
        // Order matters for conditional branches (e.g., PopExecutionFlowIfNot has specific semantics)
        for successors in successors_map.values_mut() {
            let mut seen = HashSet::new();
            successors.retain(|&x| seen.insert(x));
        }

        // Now build predecessors and set successors
        let mut predecessors_map: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

        for (&block_id, successors) in &successors_map {
            for &successor_id in successors {
                predecessors_map
                    .entry(successor_id)
                    .or_default()
                    .push(block_id);
            }
        }

        // Deduplicate predecessors while preserving order
        for predecessors in predecessors_map.values_mut() {
            let mut seen = HashSet::new();
            predecessors.retain(|&x| seen.insert(x));
        }

        // Now construct final Terminator enums based on terminator_expr and computed successors
        for block in &mut blocks {
            let successors = successors_map.get(&block.id).cloned().unwrap_or_default();

            block.terminator = if let Some(ref term_expr) = block.terminator_expr {
                match &term_expr.kind {
                    ExprKind::Jump { target } => {
                        // Convert bytecode offset to block ID
                        let target_block = offset_to_block
                            .get(target)
                            .copied()
                            .expect("jump target must be a valid block");
                        Terminator::Goto {
                            target: target_block,
                        }
                    }
                    ExprKind::JumpIfNot { condition, .. } => {
                        // Should have exactly 2 successors: false target and true target (fallthrough)
                        if successors.len() == 2 {
                            Terminator::Branch {
                                condition: *condition.clone(),
                                // Note: successors are ordered [false_target, true_target] based on insertion order
                                false_target: successors[0],
                                true_target: successors[1],
                            }
                        } else {
                            // Unexpected number of successors - use DynamicJump
                            Terminator::DynamicJump
                        }
                    }
                    ExprKind::PopExecutionFlowIfNot { condition } => {
                        // Should have 2 successors if both paths are reachable
                        if successors.len() == 2 {
                            Terminator::Branch {
                                condition: *condition.clone(),
                                // Note: successors are ordered [pop_target, fallthrough_target] based on insertion order
                                false_target: successors[0], // pop target (condition is false)
                                true_target: successors[1],  // fallthrough (condition is true)
                            }
                        } else {
                            // Multiple pop targets or single path - use DynamicJump
                            Terminator::DynamicJump
                        }
                    }
                    ExprKind::PopExecutionFlow => {
                        // Check if pop resolved to exactly one target
                        if successors.len() == 1 {
                            // Single target - convert to unconditional goto
                            if let Some(&target_block) = successors.first() {
                                Terminator::Goto {
                                    target: target_block,
                                }
                            } else {
                                Terminator::DynamicJump
                            }
                        } else {
                            // Multiple or no targets - dynamic jump
                            Terminator::DynamicJump
                        }
                    }
                    ExprKind::Return(val) => Terminator::Return(*val.clone()),
                    _ => unreachable!("Invalid terminator expression"),
                }
            } else {
                // No terminator expression - convert implicit fallthrough to explicit goto
                if successors.len() == 1 {
                    Terminator::Goto {
                        target: successors[0],
                    }
                } else if successors.is_empty() {
                    // No successors - this is a dead end (malformed bytecode or unreachable code)
                    logger.warn(&format!(
                        "Block {:?} has no terminator and no successors - likely unreachable code",
                        block.id
                    ));
                    Terminator::DynamicJump
                } else {
                    // Multiple successors without explicit terminator - dynamic jump
                    Terminator::DynamicJump
                }
            };

            block.successors = successors;
            block.predecessors = predecessors_map.get(&block.id).cloned().unwrap_or_default();
        }

        blocks
    }

    /// Get a basic block by ID
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock> {
        self.blocks.get(id.0)
    }

    /// Get the block that contains a given bytecode offset
    pub fn get_block_at_offset(&self, offset: BytecodeOffset) -> Option<&BasicBlock> {
        self.offset_to_block
            .get(&offset)
            .and_then(|id| self.get_block(*id))
    }

    /// Print CFG in a human-readable format
    pub fn print_debug(&self, _expressions: &[Expr], address_index: &AddressIndex) {
        use crate::formatters::cpp::CppFormatter;

        println!("Control Flow Graph:");
        println!("  Entry Block: {:?}", self.entry_block);
        println!("  Total Blocks: {}", self.blocks.len());
        println!();

        for block in &self.blocks {
            println!(
                "Block {:?} [0x{:X}..0x{:X}]:",
                block.id,
                block.start_offset.as_usize(),
                block.end_offset.as_usize()
            );
            println!("  Predecessors: {:?}", block.predecessors);
            println!("  Successors: {:?}", block.successors);
            println!("  Statements:");
            let mut formatter = CppFormatter::new(address_index, Default::default());
            formatter.set_indent_level(2);

            for stmt in &block.statements {
                print!("    0x{:X}: ", stmt.offset.as_usize());
                formatter.format_statement(stmt);
            }

            // Print terminator
            match &block.terminator {
                Terminator::Goto { target } => {
                    println!("    [goto {:?}]", target);
                }
                Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                } => {
                    println!(
                        "    [branch: if !(...) goto {:?} else {:?}]",
                        false_target, true_target
                    );
                }
                Terminator::DynamicJump => {
                    println!("    [dynamic-jump]");
                }
                Terminator::Return(expr) => {
                    print!("    [return ");
                    formatter.format_statement(expr);
                    print!("]");
                    println!();
                }
                Terminator::None => unreachable!(),
            }
            println!();
        }
    }

    /// Generate a DOT graph representation of the CFG
    pub fn to_dot(
        &self,
        _expressions: &[Expr],
        _address_index: &AddressIndex,
    ) -> crate::dot::Graph {
        use crate::dot::{Edge, Graph, Node, XmlTag};

        let mut graph = Graph::new("digraph");

        // Set graph attributes
        graph.base.graph_attributes.add("rankdir", "TB");
        graph.base.node_attributes.add("shape", "plaintext");
        graph.base.node_attributes.add("fontname", "monospace");
        graph.base.node_attributes.add("fontsize", "10");

        // Create a node for each block
        for block in &self.blocks {
            let mut table = XmlTag::new("TABLE")
                .attr("BORDER", "0")
                .attr("CELLBORDER", "1")
                .attr("CELLSPACING", "0")
                .attr("CELLPADDING", "4");

            // Header row with block ID
            let bgcolor = if block.id == self.entry_block {
                "lightgreen"
            } else if block.successors.is_empty() {
                "lightcoral"
            } else {
                "lightyellow"
            };

            table = table.child(
                XmlTag::new("TR").child(
                    XmlTag::new("TD")
                        .attr("BGCOLOR", bgcolor)
                        .attr("ALIGN", "center")
                        .child(format!("Block {:?}", block.id)),
                ),
            );

            // Address range row
            table = table.child(
                XmlTag::new("TR").child(
                    XmlTag::new("TD")
                        .attr("ALIGN", "left")
                        .attr("BGCOLOR", "lightgray")
                        .child(format!(
                            "0x{:X}..0x{:X}",
                            block.start_offset.as_usize(),
                            block.end_offset.as_usize()
                        )),
                ),
            );

            // Add statements
            for stmt in &block.statements {
                let instr_text = format!(
                    "0x{:X}: {}",
                    stmt.offset.as_usize(),
                    Self::format_expr_simple(stmt)
                );

                table = table.child(
                    XmlTag::new("TR").child(
                        XmlTag::new("TD")
                            .attr("ALIGN", "left")
                            .attr("BALIGN", "left")
                            .child(instr_text),
                    ),
                );
            }

            // Add terminator
            let term_text = match &block.terminator {
                Terminator::Goto { target } => format!("[goto {:?}]", target),
                Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                } => format!("[branch {true_target:?} / {false_target:?}]"),
                Terminator::DynamicJump => "[dynamic-jump]".to_string(),
                Terminator::Return(_) => "[return]".to_string(),
                Terminator::None => unreachable!(),
            };

            table = table.child(
                XmlTag::new("TR").child(
                    XmlTag::new("TD")
                        .attr("ALIGN", "left")
                        .attr("BALIGN", "left")
                        .attr("BGCOLOR", "lightblue")
                        .child(term_text),
                ),
            );

            let node_id = format!("block_{}", block.id.0);
            graph.base.nodes.push(Node::new_attr(
                &node_id,
                [("label", crate::dot::Id::Html(table.into()))],
            ));
        }

        // Add edges for successors
        for block in &self.blocks {
            let from_id = format!("block_{}", block.id.0);
            for &succ in &block.successors {
                let to_id = format!("block_{}", succ.0);
                graph.base.edges.push(Edge::new(from_id.clone(), to_id));
                // graph.base.edges.push(Edge::new_compass(
                //     from_id.clone(),
                //     Some("s"), // south (bottom) of source
                //     to_id,
                //     Some("n"), // north (top) of target
                //     Vec::<(crate::dot::Id, crate::dot::Id)>::new(),
                // ));
            }
        }

        graph
    }

    /// Simple expression formatter for DOT labels
    fn format_expr_simple(expr: &Expr) -> String {
        let debug_str = format!("{:?}", expr.kind);
        debug_str.chars().take(20).collect()
    }
}
