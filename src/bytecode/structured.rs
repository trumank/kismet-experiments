//! control flow structuring based on phoenix decompiler
//! https://github.com/angr/angr/blob/071ceda914755eb4933587af6e1e0536476fb8bb/angr/analyses/decompiler/structuring/phoenix.py
//! https://www.usenix.org/conference/usenixsecurity13/technical-sessions/presentation/schwartz

use super::cfg::{BasicBlock, BlockId, ControlFlowGraph, Terminator};
use super::logger::{Logger, NullLogger};
use super::loops::LoopInfo;
use crate::bytecode::address_index::AddressIndex;
use crate::bytecode::expr::Expr;
use crate::formatters::cpp::{CppFormatter, FormatContext};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoopType {
    /// condition checked before body
    While,
    /// condition checked after body
    DoWhile,
    /// no condition
    Endless,
}

#[derive(Debug, Clone)]
pub enum StructuredNode {
    Sequence {
        nodes: Vec<StructuredNode>,
    },

    Conditional {
        condition: Expr,
        true_branch: Box<StructuredNode>,
        false_branch: Option<Box<StructuredNode>>,
        /// original condition block ID
        condition_block: BlockId,
    },

    Loop {
        loop_type: LoopType,
        condition: Option<Expr>,
        body: Box<StructuredNode>,
        /// loop header block ID
        header: BlockId,
    },

    Break {
        /// target block where break jumps to
        target: BlockId,
    },

    Continue {
        /// loop header to continue to
        target: BlockId,
    },

    Code {
        block: BasicBlock,
    },

    Empty,
}

impl StructuredNode {
    /// Create a sequence from a vector of nodes
    pub fn sequence(nodes: Vec<StructuredNode>, logger: &dyn Logger) -> Self {
        // Filter out empty nodes
        let mut nodes: Vec<_> = nodes
            .into_iter()
            .filter(|n| !matches!(n, StructuredNode::Empty))
            .collect();

        if nodes.is_empty() {
            StructuredNode::Empty
        } else if nodes.len() == 1 {
            nodes.into_iter().next().unwrap()
        } else {
            // Strip implicit gotos between sequential blocks
            // If block i has a goto to block i+1, that's now implicit in the sequence
            for i in 0..nodes.len() - 1 {
                // Get the ID of the next block (if it's a Code node)
                if let Some(next_id) = Self::get_block_id(&nodes[i + 1]) {
                    logger.debug(&format!(
                        "  Sequence: stripping gotos to {:?} from node at index {}",
                        next_id, i
                    ));
                    // Strip goto to next_id from current node
                    nodes[i] = Self::strip_implicit_goto_helper(nodes[i].clone(), next_id, logger);
                }
            }
            StructuredNode::Sequence { nodes }
        }
    }

    /// Helper to get the block ID from a node (works for Code and structured nodes)
    /// For sequences, returns the ID of the first node in the sequence
    fn get_block_id(node: &StructuredNode) -> Option<BlockId> {
        match node {
            StructuredNode::Code { block } => Some(block.id),
            StructuredNode::Loop { header, .. } => Some(*header),
            StructuredNode::Conditional {
                condition_block, ..
            } => Some(*condition_block),
            StructuredNode::Sequence { nodes } => {
                // Recursively get ID from first node in sequence
                nodes.first().and_then(Self::get_block_id)
            }
            _ => None,
        }
    }

    /// Helper version of strip_implicit_goto that doesn't need &self
    fn strip_implicit_goto_helper(
        node: StructuredNode,
        implicit_target: BlockId,
        logger: &dyn Logger,
    ) -> StructuredNode {
        match node {
            StructuredNode::Code { mut block } => {
                logger.debug(&format!(
                    "    strip_implicit_goto_helper: checking Code block {:?}, terminator: {:?}",
                    block.id,
                    match &block.terminator {
                        Terminator::Goto { target } => format!("Goto({:?})", target),
                        Terminator::Branch { .. } => "Branch".to_string(),
                        Terminator::Return(_) => "Return".to_string(),
                        Terminator::DynamicJump => "DynamicJump".to_string(),
                        Terminator::None => "None".to_string(),
                    }
                ));
                match &block.terminator {
                    Terminator::Goto { target } if *target == implicit_target => {
                        logger.debug(&format!(
                            "  Stripping implicit sequential goto from block {:?} to {:?}",
                            block.id, target
                        ));
                        block.terminator = Terminator::None;
                    }
                    _ => {}
                }
                StructuredNode::Code { block }
            }
            StructuredNode::Sequence { mut nodes } => {
                logger.debug(&format!(
                    "    strip_implicit_goto_helper: recursing into Sequence with {} nodes",
                    nodes.len()
                ));
                // Only strip from the LAST node in the sequence
                // (the last node is the one that transitions to the next sequential node)
                if let Some(last) = nodes.pop() {
                    let stripped_last =
                        Self::strip_implicit_goto_helper(last, implicit_target, logger);
                    nodes.push(stripped_last);
                }
                StructuredNode::Sequence { nodes }
            }
            other => {
                logger.debug(
                    "    strip_implicit_goto_helper: node is not Code or Sequence, ignoring",
                );
                other
            }
        }
    }

    /// Create a conditional node
    pub fn conditional(
        condition: Expr,
        true_branch: StructuredNode,
        false_branch: Option<StructuredNode>,
        condition_block: BlockId,
    ) -> Self {
        StructuredNode::Conditional {
            condition,
            true_branch: Box::new(true_branch),
            false_branch: false_branch.map(Box::new),
            condition_block,
        }
    }

    /// Create a loop node
    pub fn loop_node(
        loop_type: LoopType,
        condition: Option<Expr>,
        body: StructuredNode,
        header: BlockId,
    ) -> Self {
        StructuredNode::Loop {
            loop_type,
            condition,
            body: Box::new(body),
            header,
        }
    }

    /// Create a code node from a basic block
    pub fn code(block: BasicBlock) -> Self {
        StructuredNode::Code { block }
    }

    /// Format this node with proper indentation
    pub fn format(&self, indent_level: usize, address_index: &AddressIndex) {
        let indent = "    ".repeat(indent_level);
        let mut formatter = CppFormatter::new(address_index, Default::default());

        match self {
            StructuredNode::Sequence { nodes } => {
                for node in nodes {
                    node.format(indent_level, address_index);
                }
            }

            StructuredNode::Conditional {
                condition,
                true_branch,
                false_branch,
                condition_block,
            } => {
                let cond_str = formatter.format_expr_inline(condition, &FormatContext::This);
                println!("{}// Block {:?}", indent, condition_block);
                println!("{}if ({}) {{", indent, cond_str);
                true_branch.format(indent_level + 1, address_index);
                if let Some(false_br) = false_branch {
                    println!("{}}} else {{", indent);
                    false_br.format(indent_level + 1, address_index);
                }
                println!("{}}}", indent);
            }

            StructuredNode::Loop {
                loop_type,
                condition,
                body,
                header,
            } => {
                println!("{}// Loop header: Block {:?}", indent, header);
                match loop_type {
                    LoopType::While => {
                        let cond_str = condition
                            .as_ref()
                            .map(|c| formatter.format_expr_inline(c, &FormatContext::This))
                            .unwrap_or_else(|| "true".to_string());
                        println!("{}while ({}) {{", indent, cond_str);
                        body.format(indent_level + 1, address_index);
                        println!("{}}}", indent);
                    }
                    LoopType::DoWhile => {
                        println!("{}do {{", indent);
                        body.format(indent_level + 1, address_index);
                        let cond_str = condition
                            .as_ref()
                            .map(|c| formatter.format_expr_inline(c, &FormatContext::This))
                            .unwrap_or_else(|| "true".to_string());
                        println!("{}}} while ({});", indent, cond_str);
                    }
                    LoopType::Endless => {
                        println!("{}loop {{", indent);
                        body.format(indent_level + 1, address_index);
                        println!("{}}}", indent);
                    }
                }
            }

            StructuredNode::Break { target } => {
                println!("{}break; // to Block {:?}", indent, target);
            }

            StructuredNode::Continue { target } => {
                println!("{}continue; // to Block {:?}", indent, target);
            }

            StructuredNode::Code { block } => {
                println!(
                    "{}// Block {:?} [0x{:X}..0x{:X}]",
                    indent,
                    block.id,
                    block.start_offset.as_usize(),
                    block.end_offset.as_usize()
                );

                // Format statements using CppFormatter (skip execution flow control)
                formatter.set_indent_level(indent_level);
                for stmt in &block.statements {
                    // Skip execution flow control instructions (internal VM state)
                    match &stmt.kind {
                        super::expr::ExprKind::PushExecutionFlow { .. }
                        | super::expr::ExprKind::PopExecutionFlow
                        | super::expr::ExprKind::PopExecutionFlowIfNot { .. } => {
                            // Skip - these are internal control flow mechanisms
                            continue;
                        }
                        _ => {}
                    }
                    formatter.format_statement(stmt);
                }

                // Format terminator if present
                match &block.terminator {
                    Terminator::Goto { target } => {
                        println!("{}goto Block {:?};", indent, target);
                    }
                    Terminator::Branch {
                        condition,
                        true_target,
                        false_target,
                    } => {
                        let cond_str =
                            formatter.format_expr_inline(condition, &FormatContext::This);
                        println!(
                            "{}if ({}) goto Block {:?}; else goto Block {:?};",
                            indent, cond_str, true_target, false_target
                        );
                    }
                    Terminator::DynamicJump => {
                        println!("{}// dynamic jump", indent);
                    }
                    Terminator::Return(expr) => {
                        let ret_str = formatter.format_expr_inline(expr, &FormatContext::This);
                        println!("{}return {};", indent, ret_str);
                    }
                    Terminator::None => {
                        // No terminator - control flow is implicit
                    }
                }
            }

            StructuredNode::Empty => {}
        }
    }
}

/// The result of structuring: a structured control flow graph
#[derive(Debug, Clone)]
pub struct StructuredGraph {
    /// The root node of the structured graph
    pub root: StructuredNode,
}

impl StructuredGraph {
    /// Print the structured graph in a human-readable format
    pub fn print(&self, address_index: &AddressIndex) {
        println!("Structured Control Flow:");
        println!();
        self.root.format(0, address_index);
    }
}

impl Region {
    /// Export the region graph to DOT format for visualization
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph Region {\n");
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n");
        dot.push_str("  rankdir=TB;\n\n");

        // Add nodes
        for (&node_id, node) in &self.nodes {
            let node_type = match node {
                StructuredNode::Sequence { .. } => "Sequence",
                StructuredNode::Conditional { .. } => "Conditional",
                StructuredNode::Loop { .. } => "Loop",
                StructuredNode::Code { .. } => "Code",
                _ => "Other",
            };

            let color = if node_id == self.head {
                "lightgreen"
            } else {
                "lightblue"
            };

            dot.push_str(&format!(
                "  n{} [label=\"{:?}\\n{}\", fillcolor={}, style=filled];\n",
                node_id.0, node_id, node_type, color
            ));
        }

        dot.push('\n');

        // Add edges
        for (&from, succs) in &self.edges {
            for &to in succs {
                dot.push_str(&format!("  n{} -> n{};\n", from.0, to.0));
            }
        }

        dot.push_str("}\n");
        dot
    }
}

/// Working region during structuring
#[derive(Debug, Clone)]
struct Region {
    /// Current graph being structured
    nodes: HashMap<BlockId, StructuredNode>,
    /// Edges in the current graph
    edges: HashMap<BlockId, Vec<BlockId>>,
    /// Reverse edges (predecessors)
    predecessors: HashMap<BlockId, Vec<BlockId>>,
    /// Entry node of this region
    head: BlockId,
}

impl Region {
    fn new(cfg: &ControlFlowGraph) -> Self {
        // Initialize region from CFG
        let mut nodes = HashMap::new();
        let mut edges = HashMap::new();
        let mut predecessors = HashMap::new();

        for block in &cfg.blocks {
            nodes.insert(block.id, StructuredNode::code(block.clone()));
            edges.insert(block.id, block.successors.clone());
            predecessors.insert(block.id, block.predecessors.clone());
        }

        Self {
            nodes,
            edges,
            predecessors,
            head: cfg.entry_block,
        }
    }

    /// Check if the region graph has cycles
    fn has_cycles(&self) -> bool {
        // Simple DFS-based cycle detection
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        fn has_cycle_util(
            node: BlockId,
            visited: &mut HashSet<BlockId>,
            rec_stack: &mut HashSet<BlockId>,
            edges: &HashMap<BlockId, Vec<BlockId>>,
        ) -> bool {
            visited.insert(node);
            rec_stack.insert(node);

            if let Some(successors) = edges.get(&node) {
                for &succ in successors {
                    if !visited.contains(&succ) {
                        if has_cycle_util(succ, visited, rec_stack, edges) {
                            return true;
                        }
                    } else if rec_stack.contains(&succ) {
                        return true;
                    }
                }
            }

            rec_stack.remove(&node);
            false
        }

        for &node in self.nodes.keys() {
            if !visited.contains(&node)
                && has_cycle_util(node, &mut visited, &mut rec_stack, &self.edges)
            {
                return true;
            }
        }

        false
    }

    fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Get successors of a node
    fn successors(&self, node: BlockId) -> &[BlockId] {
        self.edges.get(&node).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Get predecessors of a node
    fn predecessors(&self, node: BlockId) -> &[BlockId] {
        self.predecessors
            .get(&node)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Replace a node with a new node
    fn replace_node(&mut self, old: BlockId, new_id: BlockId, new_node: StructuredNode) {
        // Update nodes map
        self.nodes.remove(&old);
        self.nodes.insert(new_id, new_node);

        // Update edges: all edges pointing to old now point to new
        for edges in self.edges.values_mut() {
            for edge in edges.iter_mut() {
                if *edge == old {
                    *edge = new_id;
                }
            }
        }

        // Update predecessors: all predecessors of old are now predecessors of new
        for preds in self.predecessors.values_mut() {
            for pred in preds.iter_mut() {
                if *pred == old {
                    *pred = new_id;
                }
            }
        }

        // If old was the head, update head
        if self.head == old {
            self.head = new_id;
        }
    }

    /// Remove a node from the region
    fn remove_node(&mut self, node: BlockId) {
        self.nodes.remove(&node);
        self.edges.remove(&node);
        self.predecessors.remove(&node);

        // Remove from all edge lists
        for edges in self.edges.values_mut() {
            edges.retain(|&e| e != node);
        }
        for preds in self.predecessors.values_mut() {
            preds.retain(|&p| p != node);
        }
    }

    /// Remove an edge
    fn remove_edge(&mut self, from: BlockId, to: BlockId) {
        if let Some(edges) = self.edges.get_mut(&from) {
            edges.retain(|&e| e != to);
        }
        if let Some(preds) = self.predecessors.get_mut(&to) {
            preds.retain(|&p| p != from);
        }
    }
}

/// Phoenix-based control flow structuring algorithm
pub struct PhoenixStructurer<'a> {
    loop_info: &'a LoopInfo,
    region: Region,
    /// Edges that should not be removed during refinement
    protected_edges: HashSet<(BlockId, BlockId)>,
    /// Logger for debug output
    logger: &'a dyn Logger,
}

impl<'a> PhoenixStructurer<'a> {
    /// Create a new structurer (uses NullLogger by default)
    pub fn new(cfg: &'a ControlFlowGraph, loop_info: &'a LoopInfo) -> Self {
        Self::new_with_logger(cfg, loop_info, &NullLogger)
    }

    /// Create a new structurer with a custom logger
    pub fn new_with_logger(
        cfg: &'a ControlFlowGraph,
        loop_info: &'a LoopInfo,
        logger: &'a dyn Logger,
    ) -> Self {
        let region = Region::new(cfg);
        Self {
            loop_info,
            region,
            protected_edges: HashSet::new(),
            logger,
        }
    }

    /// Main structuring algorithm
    pub fn structure(mut self) -> Option<StructuredGraph> {
        const MAX_ITERATIONS: usize = 1000;
        let mut iteration = 0;

        while self.region.len() > 1 && iteration < MAX_ITERATIONS {
            iteration += 1;

            let has_cycles = self.region.has_cycles();

            // Try acyclic schema matching
            let mut progress = self.analyze_acyclic();

            // Try cyclic schema matching if we have cycles
            if has_cycles {
                progress |= self.analyze_cyclic();
            }

            if !progress {
                // Debug: show current state
                self.print_region_state();

                self.logger.warn(&format!(
                    "No progress made in structuring (iteration {}), {} nodes remaining",
                    iteration,
                    self.region.len()
                ));
                self.logger.info("\nFinal region state:");
                self.print_region_state();

                // Save DOT graph for visualization
                // let dot = self.region.to_dot();
                // render_dot_and_open(dot);

                break;
            }
        }

        if iteration >= MAX_ITERATIONS {
            self.logger.warn("Structuring hit iteration limit");
        }

        // Return the final result
        if self.region.len() == 1 {
            let root = self.region.nodes.values().next().cloned()?;
            Some(StructuredGraph { root })
        } else {
            self.logger.warn(&format!(
                "Could not fully structure the CFG ({} nodes remain)",
                self.region.len()
            ));
            self.logger
                .info("Returning partial structuring with remaining nodes as a sequence");

            // Collect remaining nodes in a deterministic order (by ID)
            let mut remaining_ids: Vec<_> = self.region.nodes.keys().copied().collect();
            remaining_ids.sort();

            let remaining_nodes: Vec<_> = remaining_ids
                .iter()
                .filter_map(|&id| self.region.nodes.get(&id).cloned())
                .collect();

            if remaining_nodes.is_empty() {
                return None;
            }

            // Return all remaining nodes as a sequence
            // This preserves gotos between them
            let root = if remaining_nodes.len() == 1 {
                remaining_nodes.into_iter().next().unwrap()
            } else {
                StructuredNode::Sequence {
                    nodes: remaining_nodes,
                }
            };

            Some(StructuredGraph { root })
        }
    }

    /// Analyze acyclic patterns
    fn analyze_acyclic(&mut self) -> bool {
        let mut any_match = false;

        // Get nodes in postorder (children before parents) so inner loops are structured first
        let nodes = self.get_postorder();

        for node_id in nodes {
            if !self.region.nodes.contains_key(&node_id) {
                continue;
            }

            // Try sequence pattern
            if self.match_sequence(node_id) {
                any_match = true;
                break;
            }

            // Try if-then-else pattern
            if self.match_ite(node_id) {
                any_match = true;
                break;
            }
        }

        any_match
    }

    /// Analyze cyclic patterns (loops)
    fn analyze_cyclic(&mut self) -> bool {
        let mut any_match = false;

        // Get nodes in postorder (children before parents) so inner loops are structured first
        let nodes = self.get_postorder();

        for node_id in nodes {
            if !self.region.nodes.contains_key(&node_id) {
                continue;
            }

            // Try while loop pattern
            if self.match_while_loop(node_id) {
                any_match = true;
                break;
            }

            // Try do-while loop pattern
            if self.match_dowhile_loop(node_id) {
                any_match = true;
                break;
            }

            // Try natural loop pattern (catch-all)
            if self.match_natural_loop(node_id) {
                any_match = true;
                break;
            }
        }

        any_match
    }

    /// Get nodes in DFS postorder (children before parents)
    /// This ensures inner loops are structured before outer loops
    fn get_postorder(&self) -> Vec<BlockId> {
        let mut visited = HashSet::new();
        let mut postorder = Vec::new();

        fn dfs(
            node: BlockId,
            region: &Region,
            visited: &mut HashSet<BlockId>,
            postorder: &mut Vec<BlockId>,
        ) {
            if visited.contains(&node) || !region.nodes.contains_key(&node) {
                return;
            }
            visited.insert(node);

            for &succ in region.successors(node) {
                dfs(succ, region, visited, postorder);
            }

            postorder.push(node);
        }

        dfs(self.region.head, &self.region, &mut visited, &mut postorder);
        // Return postorder (children before parents) so inner loops are structured first
        postorder
    }

    /// Match sequence pattern: A -> B where B has only one predecessor (A)
    fn match_sequence(&mut self, node_id: BlockId) -> bool {
        let succs = self.region.successors(node_id).to_vec();

        // Must have exactly one successor
        if succs.len() != 1 {
            return false;
        }

        let succ = succs[0];

        // Successor must have exactly one predecessor (this node)
        let succ_preds = self.region.predecessors(succ).to_vec();
        if succ_preds.len() != 1 || succ_preds[0] != node_id {
            return false;
        }

        // Don't merge if there's a back edge (would create a self-loop)
        if succ == node_id {
            return false;
        }

        self.logger
            .debug(&format!("match_sequence: {:?} -> {:?}", node_id, succ));

        // Merge the two nodes
        let node_1 = self.region.nodes.get(&node_id).cloned().unwrap();
        let node_2 = self.region.nodes.get(&succ).cloned().unwrap();

        // Create sequence
        let new_node = StructuredNode::sequence(vec![node_1, node_2], self.logger);

        // Get the edges of the successor (which become edges of the merged node)
        let succ_edges = self.region.successors(succ).to_vec();

        // Update the region
        self.region.nodes.insert(node_id, new_node);
        self.region.edges.insert(node_id, succ_edges.clone());

        // Update predecessors of the successor's successors BEFORE removing succ
        // (remove_node clears all references to succ from predecessor lists!)
        for &succ_succ in &succ_edges {
            let preds = self.region.predecessors.entry(succ_succ).or_default();

            for pred in preds.iter_mut() {
                if *pred == succ {
                    *pred = node_id;
                }
            }
        }

        // NOW remove the successor node (after we've updated all predecessors)
        self.region.remove_node(succ);

        self.logger
            .debug(&format!("match_sequence: FINAL state for {:?}:", node_id));
        self.logger.debug(&format!(
            "  successors: {:?}",
            self.region.successors(node_id)
        ));
        for &succ_succ in &succ_edges {
            self.logger.debug(&format!(
                "  succ {:?} predecessors: {:?}",
                succ_succ,
                self.region.predecessors(succ_succ)
            ));
        }

        true
    }

    /// Match if-then-else pattern: conditional with two branches that converge
    fn match_ite(&mut self, node_id: BlockId) -> bool {
        let succs = self.region.successors(node_id).to_vec();

        // Must have exactly two successors
        if succs.len() != 2 {
            return false;
        }

        // FIRST: Extract the actual true/false targets from the Branch terminator
        // This tells us which successor is the true branch and which is the false branch
        let node = self.region.nodes.get(&node_id).unwrap();

        let (condition, true_target, false_target) = match node {
            StructuredNode::Code { block } => {
                match &block.terminator {
                    Terminator::Branch {
                        condition,
                        true_target,
                        false_target,
                    } => (condition.clone(), *true_target, *false_target),
                    _ => return false, // Not a conditional branch
                }
            }
            StructuredNode::Sequence { nodes } => {
                if let Some(StructuredNode::Code { block }) = nodes.last() {
                    match &block.terminator {
                        Terminator::Branch {
                            condition,
                            true_target,
                            false_target,
                        } => (condition.clone(), *true_target, *false_target),
                        _ => return false,
                    }
                } else {
                    return false;
                }
            }
            _ => return false,
        };

        self.logger.debug(&format!(
            "DEBUG match_ite: node={:?}, true_target={:?}, false_target={:?}",
            node_id, true_target, false_target
        ));

        // Verify that the targets are in our successor list
        if !succs.contains(&true_target) || !succs.contains(&false_target) {
            self.logger.debug(&format!(
                "  ERROR: Branch targets not in successors! true={:?}, false={:?}, succs={:?}",
                true_target, false_target, succs
            ));
            return false;
        }

        // Check branch predecessors and detect fallthrough patterns
        let true_preds = self.region.predecessors(true_target).to_vec();
        let false_preds = self.region.predecessors(false_target).to_vec();

        // Get successors to detect fallthrough early
        let true_succs = self.region.successors(true_target).to_vec();
        let false_succs = self.region.successors(false_target).to_vec();

        // Detect if one branch falls through to the other
        let true_falls_to_false = true_succs.len() == 1 && true_succs[0] == false_target;
        let false_falls_to_true = false_succs.len() == 1 && false_succs[0] == true_target;

        // Validate predecessors, allowing fallthrough cases
        if true_preds.len() != 1 || true_preds[0] != node_id {
            return false;
        }

        // For false branch: either exactly one pred (this node), OR
        // two preds (this node and the true branch falling through)
        if false_preds.len() == 1 {
            if false_preds[0] != node_id {
                return false;
            }
        } else if false_preds.len() == 2 && true_falls_to_false {
            // True branch falls through to false branch - expected pattern
            if !false_preds.contains(&node_id) || !false_preds.contains(&true_target) {
                return false;
            }
        } else if false_preds.len() == 2 && false_falls_to_true {
            // False branch falls through to true branch - check true_preds was correct
            // Actually, wait - if false falls to true, then true has 2 preds, not false
            // This case is symmetric, handled below
            return false;
        } else {
            return false;
        }

        // Check if they converge to the same node or are both exits
        let merge_point = if true_succs.is_empty() && false_succs.is_empty() {
            // Both are exits - no merge point
            None
        } else if true_succs.len() == 1 && false_succs.is_empty() {
            // False is exit, true continues
            None
        } else if false_succs.len() == 1 && true_succs.is_empty() {
            // True is exit, false continues
            None
        } else if true_succs.len() == 1 && false_succs.len() == 1 && true_succs[0] == false_succs[0]
        {
            // Both converge to the same node
            Some(true_succs[0])
        } else if true_falls_to_false {
            // True branch falls through to false branch
            // Pattern: if (cond) { A }; B  where A falls through to B
            // B executes in both cases, so it's the merge/continuation point
            self.logger.debug(&format!(
                "  True branch {:?} falls through to false branch {:?}",
                true_target, false_target
            ));
            Some(false_target)
        } else if false_falls_to_true {
            // False branch falls through to true branch
            // Pattern: if (!cond) { A }; B  where A falls through to B
            self.logger.debug(&format!(
                "  False branch {:?} falls through to true branch {:?}",
                false_target, true_target
            ));
            Some(true_target)
        } else {
            // Doesn't match our pattern
            return false;
        };

        // Get the branch nodes and possibly negate condition for fallthrough
        // For fallthrough cases, we only use the non-fallthrough branch
        // Also strip implicit gotos to merge points
        let (final_condition, true_branch, false_branch_opt) = if true_falls_to_false {
            // Only the true branch is in the "then" clause
            // False branch becomes the merge point (continuation)
            let mut true_br = self.region.nodes.get(&true_target).cloned().unwrap();
            // Strip goto from true branch to false branch (merge point)
            true_br = Self::strip_implicit_goto(true_br, false_target, self.logger);
            (condition, true_br, None)
        } else if false_falls_to_true {
            // Swap: only the false branch is in the "then" clause
            // True branch becomes the merge point
            // Need to negate condition so false branch becomes the "then" clause
            let mut false_br = self.region.nodes.get(&false_target).cloned().unwrap();
            // Strip goto from false branch to true branch (merge point)
            false_br = Self::strip_implicit_goto(false_br, true_target, self.logger);
            let inverted_condition = Expr::new(
                condition.offset,
                super::expr::ExprKind::VirtualFunction {
                    func: super::refs::FunctionRef::ByName(super::types::Name::new("not")),
                    params: vec![condition.clone()],
                },
            );
            (inverted_condition, false_br, None)
        } else if let Some(merge) = merge_point {
            // Normal case with merge point: strip gotos to merge from both branches
            let mut true_br = self.region.nodes.get(&true_target).cloned().unwrap();
            let mut false_br = self.region.nodes.get(&false_target).cloned().unwrap();
            true_br = Self::strip_implicit_goto(true_br, merge, self.logger);
            false_br = Self::strip_implicit_goto(false_br, merge, self.logger);
            (condition, true_br, Some(false_br))
        } else {
            // No merge point (both branches exit or diverge) - keep terminators
            let true_br = self.region.nodes.get(&true_target).cloned().unwrap();
            let false_br = self.region.nodes.get(&false_target).cloned().unwrap();
            (condition, true_br, Some(false_br))
        };

        // Extract any statements from the block BEFORE the branch terminator
        // Since CFG analysis already separates statements from terminators,
        // we just need to preserve block.statements if they exist
        let statements_node = match node {
            StructuredNode::Code { block } if !block.statements.is_empty() => {
                // Create a Code node with just the statements (no terminator)
                let mut stmt_block = block.clone();
                stmt_block.terminator = Terminator::None;
                Some(StructuredNode::Code { block: stmt_block })
            }
            StructuredNode::Sequence { nodes } => {
                // For sequences, all nodes except the last should be preserved
                if nodes.len() > 1 {
                    Some(StructuredNode::sequence(
                        nodes[..nodes.len() - 1].to_vec(),
                        self.logger,
                    ))
                } else {
                    None
                }
            }
            _ => None,
        };

        // Create conditional node
        let conditional =
            StructuredNode::conditional(final_condition, true_branch, false_branch_opt, node_id);

        // If there are statements before the branch, wrap in a sequence
        let new_node = if let Some(stmts) = statements_node {
            self.logger.debug(&format!(
                "DEBUG match_ite: Preserving statements before conditional at {:?}",
                node_id
            ));
            StructuredNode::sequence(vec![stmts, conditional], self.logger)
        } else {
            conditional
        };

        // Update the region
        self.region.nodes.insert(node_id, new_node);

        // Update edges BEFORE removing nodes (so predecessors are still intact)
        if let Some(merge) = merge_point {
            self.logger
                .debug(&format!("match_ite: merge_point={:?}", merge));
            self.logger.debug(&format!(
                "DEBUG match_ite: merge preds BEFORE = {:?}",
                self.region.predecessors(merge)
            ));

            self.region.edges.insert(node_id, vec![merge]);

            // Update merge point's predecessors BEFORE removing true/false branches
            let preds = self.region.predecessors.entry(merge).or_default();

            // Remove old predecessors and add new one
            preds.retain(|&p| p != true_target && p != false_target);
            if !preds.contains(&node_id) {
                preds.push(node_id);
            }

            self.logger.debug(&format!(
                "DEBUG match_ite: merge preds AFTER = {:?}",
                self.region.predecessors(merge)
            ));
        } else {
            // Determine new edges based on which branch continues
            let new_edges = if !true_succs.is_empty() {
                true_succs
            } else if !false_succs.is_empty() {
                false_succs
            } else {
                vec![]
            };
            self.region.edges.insert(node_id, new_edges.clone());

            // Update successors' predecessors BEFORE removing true/false branches
            for &succ in &new_edges {
                let preds = self.region.predecessors.entry(succ).or_default();

                preds.retain(|&p| p != true_target && p != false_target);
                if !preds.contains(&node_id) {
                    preds.push(node_id);
                }
            }
        }

        // NOW remove the branch nodes (after we've updated all the edges)
        // For fallthrough cases, remove the branch that was absorbed, keep the merge point
        if true_falls_to_false {
            // true_target (Block_11) was absorbed into the if statement - REMOVE it
            // false_target (Block_15) is the merge point/continuation - KEEP it
            self.region.remove_node(true_target);
        } else if false_falls_to_true {
            // false_target was absorbed into the if statement - REMOVE it
            // true_target is the merge point/continuation - KEEP it
            self.region.remove_node(false_target);
        } else {
            // Normal case: both branches are separate from merge point - remove both
            self.region.remove_node(true_target);
            self.region.remove_node(false_target);
        }

        self.logger
            .debug(&format!("match_ite: FINAL state for {:?}:", node_id));
        self.logger.debug(&format!(
            "  successors: {:?}",
            self.region.successors(node_id)
        ));
        if let Some(merge) = merge_point {
            self.logger.debug(&format!(
                "  merge {:?} predecessors: {:?}",
                merge,
                self.region.predecessors(merge)
            ));
        }

        true
    }

    /// Strip implicit terminators from a node (gotos that are now represented by structured control flow)
    fn strip_implicit_goto(
        node: StructuredNode,
        implicit_target: BlockId,
        logger: &dyn Logger,
    ) -> StructuredNode {
        StructuredNode::strip_implicit_goto_helper(node, implicit_target, logger)
    }

    /// Strip terminators from a node that are internal to a loop
    /// (back edges to header, gotos to other blocks in the loop)
    fn strip_loop_internal_terminators(
        &self,
        node: StructuredNode,
        loop_header: BlockId,
        loop_info: &super::loops::Loop,
    ) -> StructuredNode {
        match node {
            StructuredNode::Code { mut block } => {
                // Check if the terminator is internal to the loop
                let should_strip = match &block.terminator {
                    Terminator::Goto { target } => {
                        // Strip if it's a back edge to header or goto to another loop block
                        *target == loop_header || loop_info.blocks.contains(target)
                    }
                    Terminator::Branch { .. } => {
                        // Keep branches - they're conditional logic
                        false
                    }
                    _ => false,
                };

                if should_strip {
                    // Replace with no terminator - control flow is implicit in the loop
                    self.logger.debug(&format!(
                        "  Stripping loop-internal terminator from block {:?}: {:?}",
                        block.id, block.terminator
                    ));
                    block.terminator = Terminator::None;
                }

                StructuredNode::Code { block }
            }
            StructuredNode::Sequence { nodes } => {
                // Recursively strip from all nodes in the sequence
                StructuredNode::sequence(
                    nodes
                        .into_iter()
                        .map(|n| self.strip_loop_internal_terminators(n, loop_header, loop_info))
                        .collect(),
                    self.logger,
                )
            }
            // Other node types pass through unchanged
            other => other,
        }
    }

    /// Extract statements from a header block (before the terminator)
    /// Returns None if the block has no statements or is already structured
    fn extract_header_statements(&self, node_id: BlockId) -> Option<StructuredNode> {
        let node = self.region.nodes.get(&node_id)?;

        match node {
            StructuredNode::Code { block } if !block.statements.is_empty() => {
                // Create a Code node with just the statements (no terminator)
                let mut stmt_block = block.clone();
                stmt_block.terminator = Terminator::None;
                Some(StructuredNode::Code { block: stmt_block })
            }
            StructuredNode::Sequence { nodes } => {
                // For sequences, all nodes except the last should be preserved
                if nodes.len() > 1 {
                    Some(StructuredNode::sequence(
                        nodes[..nodes.len() - 1].to_vec(),
                        self.logger,
                    ))
                } else if nodes.len() == 1 {
                    // Recursively extract from the single node
                    if let StructuredNode::Code { block } = &nodes[0]
                        && !block.statements.is_empty()
                    {
                        let mut stmt_block = block.clone();
                        stmt_block.terminator = Terminator::None;
                        return Some(StructuredNode::Code { block: stmt_block });
                    }
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Rewrite jumps inside a loop to break/continue statements
    /// - Jumps to loop header → Continue
    /// - Jumps to loop exit → Break
    /// - Other jumps preserved as goto
    fn rewrite_loop_jumps(
        loop_node: StructuredNode,
        loop_header: BlockId,
        loop_exit: BlockId,
        logger: &dyn Logger,
    ) -> StructuredNode {
        match loop_node {
            StructuredNode::Loop {
                loop_type,
                condition,
                body,
                header,
            } => {
                // Recursively rewrite the loop body
                let rewritten_body =
                    Self::rewrite_jumps_in_node(*body, loop_header, loop_exit, logger);
                StructuredNode::Loop {
                    loop_type,
                    condition,
                    body: Box::new(rewritten_body),
                    header,
                }
            }
            other => other,
        }
    }

    /// Rewrite jumps inside a loop with multiple exits (endless loops)
    fn rewrite_loop_jumps_multi_exit(
        loop_node: StructuredNode,
        loop_header: BlockId,
        loop_exits: &[BlockId],
        logger: &dyn Logger,
    ) -> StructuredNode {
        match loop_node {
            StructuredNode::Loop {
                loop_type,
                condition,
                body,
                header,
            } => {
                // Recursively rewrite the loop body
                let rewritten_body =
                    Self::rewrite_jumps_in_node_multi_exit(*body, loop_header, loop_exits, logger);
                StructuredNode::Loop {
                    loop_type,
                    condition,
                    body: Box::new(rewritten_body),
                    header,
                }
            }
            other => other,
        }
    }

    /// Recursively rewrite jumps in a structured node tree (multi-exit version)
    fn rewrite_jumps_in_node_multi_exit(
        node: StructuredNode,
        loop_header: BlockId,
        loop_exits: &[BlockId],
        logger: &dyn Logger,
    ) -> StructuredNode {
        match node {
            StructuredNode::Code { mut block } => {
                // Check if the terminator should be rewritten
                // Clone values we need before mutating
                let target_opt = match &block.terminator {
                    Terminator::Goto { target } => Some(*target),
                    _ => None,
                };

                if let Some(target) = target_opt {
                    if target == loop_header {
                        // Jump to loop header → Continue
                        logger.debug(&format!("  Rewriting goto {:?} → continue", target));
                        block.terminator = Terminator::None;
                        return StructuredNode::Sequence {
                            nodes: vec![
                                StructuredNode::Code { block },
                                StructuredNode::Continue {
                                    target: loop_header,
                                },
                            ],
                        };
                    } else if loop_exits.contains(&target) {
                        // Jump to any loop exit → Break
                        logger.debug(&format!("  Rewriting goto {:?} → break", target));
                        block.terminator = Terminator::None;
                        return StructuredNode::Sequence {
                            nodes: vec![
                                StructuredNode::Code { block },
                                StructuredNode::Break { target },
                            ],
                        };
                    }
                }

                // Check conditional branches
                if let Terminator::Branch {
                    condition,
                    true_target,
                    false_target,
                } = &block.terminator
                {
                    // Clone values we need before mutating
                    let true_tgt = *true_target;
                    let false_tgt = *false_target;
                    let cond = condition.clone();
                    let block_id = block.id;

                    // Check if either branch goes to loop header or exit
                    let true_is_continue = true_tgt == loop_header;
                    let true_is_break = loop_exits.contains(&true_tgt);
                    let false_is_continue = false_tgt == loop_header;
                    let false_is_break = loop_exits.contains(&false_tgt);

                    if true_is_continue || true_is_break || false_is_continue || false_is_break {
                        logger.debug(&format!(
                            "  Rewriting conditional branch: true={:?} (cont={}, brk={}), false={:?} (cont={}, brk={})",
                            true_tgt,
                            true_is_continue,
                            true_is_break,
                            false_tgt,
                            false_is_continue,
                            false_is_break
                        ));

                        // Only rewrite if BOTH branches are break/continue
                        // Otherwise, preserve the branch as-is
                        if (true_is_continue || true_is_break)
                            && (false_is_continue || false_is_break)
                        {
                            // Both branches are break/continue - rewrite fully
                            block.terminator = Terminator::None;

                            let true_node = if true_is_continue {
                                StructuredNode::Continue {
                                    target: loop_header,
                                }
                            } else {
                                StructuredNode::Break { target: true_tgt }
                            };

                            let false_node = if false_is_continue {
                                StructuredNode::Continue {
                                    target: loop_header,
                                }
                            } else {
                                StructuredNode::Break { target: false_tgt }
                            };

                            return StructuredNode::Sequence {
                                nodes: vec![
                                    StructuredNode::Code { block },
                                    StructuredNode::Conditional {
                                        condition: cond,
                                        true_branch: Box::new(true_node),
                                        false_branch: Some(Box::new(false_node)),
                                        condition_block: block_id,
                                    },
                                ],
                            };
                        }
                        // If only one branch is break/continue, leave the branch as-is for now
                        // TODO: Could split this into conditional break + fallthrough
                    }
                }
                StructuredNode::Code { block }
            }
            StructuredNode::Sequence { nodes } => {
                // Recursively rewrite all nodes in the sequence
                let rewritten_nodes: Vec<_> = nodes
                    .into_iter()
                    .map(|n| {
                        Self::rewrite_jumps_in_node_multi_exit(n, loop_header, loop_exits, logger)
                    })
                    .collect();
                StructuredNode::sequence(rewritten_nodes, logger)
            }
            StructuredNode::Conditional {
                condition,
                true_branch,
                false_branch,
                condition_block,
            } => {
                // Recursively rewrite branches
                let rewritten_true = Self::rewrite_jumps_in_node_multi_exit(
                    *true_branch,
                    loop_header,
                    loop_exits,
                    logger,
                );
                let rewritten_false = false_branch.map(|fb| {
                    Self::rewrite_jumps_in_node_multi_exit(*fb, loop_header, loop_exits, logger)
                });
                StructuredNode::Conditional {
                    condition,
                    true_branch: Box::new(rewritten_true),
                    false_branch: rewritten_false.map(Box::new),
                    condition_block,
                }
            }
            // Don't recurse into nested loops
            StructuredNode::Loop { .. } => node,
            // Other node types pass through unchanged
            other => other,
        }
    }

    /// Recursively rewrite jumps in a structured node tree
    fn rewrite_jumps_in_node(
        node: StructuredNode,
        loop_header: BlockId,
        loop_exit: BlockId,
        logger: &dyn Logger,
    ) -> StructuredNode {
        match node {
            StructuredNode::Code { mut block } => {
                // Check if the terminator should be rewritten
                // Clone values we need before mutating
                let target_opt = match &block.terminator {
                    Terminator::Goto { target } => Some(*target),
                    _ => None,
                };

                if let Some(target) = target_opt {
                    if target == loop_header {
                        // Jump to loop header → Continue
                        logger.debug(&format!("  Rewriting goto {:?} → continue", target));
                        block.terminator = Terminator::None;
                        return StructuredNode::Sequence {
                            nodes: vec![
                                StructuredNode::Code { block },
                                StructuredNode::Continue {
                                    target: loop_header,
                                },
                            ],
                        };
                    } else if target == loop_exit {
                        // Jump to loop exit → Break
                        logger.debug(&format!("  Rewriting goto {:?} → break", target));
                        block.terminator = Terminator::None;
                        return StructuredNode::Sequence {
                            nodes: vec![
                                StructuredNode::Code { block },
                                StructuredNode::Break { target: loop_exit },
                            ],
                        };
                    }
                }

                // Check conditional branches
                if let Terminator::Branch {
                    condition,
                    true_target,
                    false_target,
                } = &block.terminator
                {
                    // Clone values we need before mutating
                    let true_tgt = *true_target;
                    let false_tgt = *false_target;
                    let cond = condition.clone();
                    let block_id = block.id;

                    // Check if either branch goes to loop header or exit
                    let true_is_continue = true_tgt == loop_header;
                    let true_is_break = true_tgt == loop_exit;
                    let false_is_continue = false_tgt == loop_header;
                    let false_is_break = false_tgt == loop_exit;

                    if true_is_continue || true_is_break || false_is_continue || false_is_break {
                        logger.debug(&format!(
                            "  Rewriting conditional branch: true={:?} (cont={}, brk={}), false={:?} (cont={}, brk={})",
                            true_tgt,
                            true_is_continue,
                            true_is_break,
                            false_tgt,
                            false_is_continue,
                            false_is_break
                        ));

                        // Only rewrite if BOTH branches are break/continue
                        if (true_is_continue || true_is_break)
                            && (false_is_continue || false_is_break)
                        {
                            block.terminator = Terminator::None;

                            let true_node = if true_is_continue {
                                StructuredNode::Continue {
                                    target: loop_header,
                                }
                            } else {
                                StructuredNode::Break { target: loop_exit }
                            };

                            let false_node = if false_is_continue {
                                StructuredNode::Continue {
                                    target: loop_header,
                                }
                            } else {
                                StructuredNode::Break { target: loop_exit }
                            };

                            return StructuredNode::Sequence {
                                nodes: vec![
                                    StructuredNode::Code { block },
                                    StructuredNode::Conditional {
                                        condition: cond,
                                        true_branch: Box::new(true_node),
                                        false_branch: Some(Box::new(false_node)),
                                        condition_block: block_id,
                                    },
                                ],
                            };
                        }
                        // If only one branch is break/continue, leave the branch as-is for now
                    }
                }
                StructuredNode::Code { block }
            }
            StructuredNode::Sequence { nodes } => {
                // Recursively rewrite all nodes in the sequence
                let rewritten_nodes: Vec<_> = nodes
                    .into_iter()
                    .map(|n| Self::rewrite_jumps_in_node(n, loop_header, loop_exit, logger))
                    .collect();
                StructuredNode::sequence(rewritten_nodes, logger)
            }
            StructuredNode::Conditional {
                condition,
                true_branch,
                false_branch,
                condition_block,
            } => {
                // Recursively rewrite branches
                let rewritten_true =
                    Self::rewrite_jumps_in_node(*true_branch, loop_header, loop_exit, logger);
                let rewritten_false = false_branch
                    .map(|fb| Self::rewrite_jumps_in_node(*fb, loop_header, loop_exit, logger));
                StructuredNode::Conditional {
                    condition,
                    true_branch: Box::new(rewritten_true),
                    false_branch: rewritten_false.map(Box::new),
                    condition_block,
                }
            }
            // Don't recurse into nested loops
            StructuredNode::Loop { .. } => node,
            // Other node types pass through unchanged
            other => other,
        }
    }

    /// Match while loop pattern: header with back edge from latch
    fn match_while_loop(&mut self, node_id: BlockId) -> bool {
        // Skip if already structured (not a simple Code block)
        if let Some(node) = self.region.nodes.get(&node_id) {
            match node {
                StructuredNode::Code { .. } => {
                    // Continue - this is a basic block we can match
                }
                _ => {
                    self.logger.debug(&format!(
                        "DEBUG match_while_loop: skipping {:?}, already structured",
                        node_id
                    ));
                    return false;
                }
            }
        }

        // Check if this node is a loop header
        let loop_opt = self.loop_info.loops.iter().find(|l| l.header == node_id);
        let Some(loop_info) = loop_opt else {
            return false;
        };

        self.logger.debug(&format!(
            "match_while_loop: attempting to match {:?}",
            node_id
        ));

        // For a while loop, the header should have a condition that exits the loop
        let succs = self.region.successors(node_id).to_vec();
        if succs.len() != 2 {
            return false;
        }

        // Determine which successor is inside the loop and which is outside
        let (_body_succ, exit_succ) = if loop_info.blocks.contains(&succs[0]) {
            (succs[0], succs[1])
        } else if loop_info.blocks.contains(&succs[1]) {
            (succs[1], succs[0])
        } else {
            return false; // Neither successor is in the loop body?
        };

        // Extract header statements (before the branch terminator)
        let header_stmts = self.extract_header_statements(node_id);

        // Extract condition from the header block
        // The node might be a Code block or a Sequence wrapping a Code block
        let node = self.region.nodes.get(&node_id).unwrap();

        let condition = match node {
            StructuredNode::Code { block } => match &block.terminator {
                Terminator::Branch { condition, .. } => condition.clone(),
                _ => return false,
            },
            StructuredNode::Sequence { nodes } => {
                // The last node in the sequence might have the branch
                if let Some(StructuredNode::Code { block }) = nodes.last() {
                    match &block.terminator {
                        Terminator::Branch { condition, .. } => condition.clone(),
                        _ => return false,
                    }
                } else {
                    return false;
                }
            }
            _ => return false,
        };

        // Collect all loop body blocks (excluding header)
        let mut body_blocks: Vec<_> = loop_info
            .blocks
            .iter()
            .filter(|&&b| b != node_id && self.region.nodes.contains_key(&b))
            .copied()
            .collect();
        body_blocks.sort();

        // Create loop body as a sequence of blocks
        // Note: With postorder traversal, nested loops are already structured
        // Strip terminators that are internal to the loop (back edges, internal gotos)
        let body_nodes: Vec<_> = body_blocks
            .iter()
            .filter_map(|&b| {
                let node = self.region.nodes.get(&b).cloned()?;
                Some(self.strip_loop_internal_terminators(node, node_id, loop_info))
            })
            .collect();

        let loop_body = if body_nodes.is_empty() {
            StructuredNode::Empty
        } else {
            StructuredNode::sequence(body_nodes, self.logger)
        };

        // Create loop node
        // If header has statements, we need to use an endless loop with break pattern
        // to preserve those statements: loop { header_stmts; if (!cond) break; body }
        let loop_node = if let Some(header) = header_stmts {
            self.logger.debug("DEBUG match_while_loop: Header has statements, using endless loop with break pattern");

            // Create inverted condition for break
            let inverted_condition = Expr::new(
                condition.offset,
                super::expr::ExprKind::VirtualFunction {
                    func: super::refs::FunctionRef::ByName(super::types::Name::new("not")),
                    params: vec![condition.clone()],
                },
            );

            // Create break node
            let break_node = StructuredNode::Break { target: exit_succ };

            // Create conditional: if (!cond) break;
            let break_conditional =
                StructuredNode::conditional(inverted_condition, break_node, None, node_id);

            // Build loop body: header_stmts; if (!cond) break; body
            let mut full_body_nodes = vec![header, break_conditional];
            if !matches!(loop_body, StructuredNode::Empty) {
                full_body_nodes.push(loop_body);
            }

            let full_body = StructuredNode::sequence(full_body_nodes, self.logger);

            StructuredNode::loop_node(LoopType::Endless, None, full_body, node_id)
        } else {
            // No header statements, use regular while loop
            StructuredNode::loop_node(LoopType::While, Some(condition), loop_body, node_id)
        };

        self.logger.debug(&format!(
            "DEBUG match_while_loop: CREATED While loop at {:?}",
            node_id
        ));

        // Rewrite jumps to breaks and continues
        let loop_node = Self::rewrite_loop_jumps(loop_node, node_id, exit_succ, self.logger);

        // Update the region - replace header with loop node
        self.region.nodes.insert(node_id, loop_node);

        // Remove loop body blocks from region
        for &block in &body_blocks {
            self.region.remove_node(block);
        }

        // Update edges - loop points ONLY to the header's exit
        // Multi-exit paths from loop body are preserved as gotos in the loop body itself
        self.region.edges.insert(node_id, vec![exit_succ]);

        // Update exit's predecessors
        let preds = self.region.predecessors.entry(exit_succ).or_default();
        preds.retain(|p| !loop_info.blocks.contains(p) || *p == node_id);
        if !preds.contains(&node_id) {
            preds.push(node_id);
        }

        true
    }

    /// Match do-while loop pattern: loop with condition at the end
    fn match_dowhile_loop(&mut self, node_id: BlockId) -> bool {
        // Skip if already structured (not a simple Code block)
        if let Some(node) = self.region.nodes.get(&node_id) {
            match node {
                StructuredNode::Code { .. } => {
                    // Continue - this is a basic block we can match
                }
                _ => {
                    return false;
                }
            }
        }

        // For do-while, we need to find a loop where the latch (last block) has the condition
        let loop_opt = self
            .loop_info
            .loops
            .iter()
            .find(|l| l.header == node_id && !l.back_edges.is_empty());

        let Some(loop_info) = loop_opt else {
            return false;
        };

        // Get the latch block (source of back edge)
        let (latch, _) = loop_info.back_edges[0];

        // Check if latch has 2 successors (one back to header, one exit)
        let latch_succs = self.region.successors(latch).to_vec();
        if latch_succs.len() != 2 {
            return false;
        }

        // One should be the header, the other is the exit
        let exit_succ = if latch_succs[0] == node_id {
            latch_succs[1]
        } else if latch_succs[1] == node_id {
            latch_succs[0]
        } else {
            return false; // Neither points back to header?
        };

        // Extract condition from latch
        // The latch might be a Code block or a Sequence wrapping a Code block
        let latch_node = self.region.nodes.get(&latch).unwrap();

        let condition = match latch_node {
            StructuredNode::Code { block } => match &block.terminator {
                Terminator::Branch { condition, .. } => condition.clone(),
                _ => return false,
            },
            StructuredNode::Sequence { nodes } => {
                // The last node in the sequence might have the branch
                if let Some(StructuredNode::Code { block }) = nodes.last() {
                    match &block.terminator {
                        Terminator::Branch { condition, .. } => condition.clone(),
                        _ => return false,
                    }
                } else {
                    return false;
                }
            }
            _ => return false,
        };

        // Extract header statements (before any terminator)
        let header_stmts = self.extract_header_statements(node_id);

        // Collect all loop body blocks (excluding header for now)
        let body_blocks: Vec<_> = loop_info
            .blocks
            .iter()
            .filter(|&&b| b != node_id && self.region.nodes.contains_key(&b))
            .copied()
            .collect();

        // Create loop body as a sequence of blocks
        // Note: With postorder traversal, nested loops are already structured
        // Strip terminators that are internal to the loop
        let mut body_nodes: Vec<_> = Vec::new();

        // For do-while, include header statements at the beginning
        if let Some(header) = header_stmts {
            body_nodes.push(header);
        }

        // Add the rest of the body blocks
        let rest_nodes: Vec<_> = body_blocks
            .iter()
            .filter_map(|&b| {
                let node = self.region.nodes.get(&b).cloned()?;
                Some(self.strip_loop_internal_terminators(node, node_id, loop_info))
            })
            .collect();

        body_nodes.extend(rest_nodes);

        let loop_body = StructuredNode::sequence(body_nodes, self.logger);

        // Create do-while loop node
        let loop_node =
            StructuredNode::loop_node(LoopType::DoWhile, Some(condition), loop_body, node_id);

        // Rewrite jumps to breaks and continues
        let loop_node = Self::rewrite_loop_jumps(loop_node, node_id, exit_succ, self.logger);

        // Update the region
        self.region.nodes.insert(node_id, loop_node);

        // Remove loop body blocks
        for &block in &body_blocks {
            if block != node_id {
                self.region.remove_node(block);
            }
        }

        // Update edges - loop points ONLY to the latch's exit
        // Multi-exit paths from loop body are preserved as gotos in the loop body itself
        self.region.edges.insert(node_id, vec![exit_succ]);

        // Update exit's predecessors
        let preds = self.region.predecessors.entry(exit_succ).or_default();
        preds.retain(|p| !loop_info.blocks.contains(p) || *p == node_id);
        if !preds.contains(&node_id) {
            preds.push(node_id);
        }

        true
    }

    /// Match natural loop pattern: any loop structure (catch-all)
    fn match_natural_loop(&mut self, node_id: BlockId) -> bool {
        // Skip if already structured (not a simple Code block)
        if let Some(node) = self.region.nodes.get(&node_id) {
            match node {
                StructuredNode::Code { .. } => {
                    // Continue - this is a basic block we can match
                }
                _ => {
                    self.logger.debug(&format!(
                        "DEBUG match_natural_loop: skipping {:?}, already structured",
                        node_id
                    ));
                    return false;
                }
            }
        }

        // Find a loop with this header
        let loop_opt = self.loop_info.loops.iter().find(|l| l.header == node_id);
        let Some(loop_info) = loop_opt else {
            return false;
        };

        self.logger.debug(&format!(
            "DEBUG match_natural_loop: attempting to match {:?}",
            node_id
        ));

        // Extract header statements (before any terminator)
        let header_stmts = self.extract_header_statements(node_id);

        // For natural loops, we just wrap all the blocks in an endless loop
        // Collect all loop blocks (excluding header for now)
        let body_blocks: Vec<_> = loop_info
            .blocks
            .iter()
            .filter(|&&b| b != node_id && self.region.nodes.contains_key(&b))
            .copied()
            .collect();

        // Create loop body as a sequence of blocks
        // Note: With postorder traversal, nested loops are already structured
        // Strip terminators that are internal to the loop
        let mut body_nodes: Vec<_> = Vec::new();

        // Include header statements at the beginning
        if let Some(header) = header_stmts {
            body_nodes.push(header);
        }

        // Add the rest of the body blocks
        let rest_nodes: Vec<_> = body_blocks
            .iter()
            .filter_map(|&b| {
                let node = self.region.nodes.get(&b).cloned()?;
                Some(self.strip_loop_internal_terminators(node, node_id, loop_info))
            })
            .collect();

        body_nodes.extend(rest_nodes);

        if body_nodes.is_empty() {
            return false;
        }

        let loop_body = StructuredNode::sequence(body_nodes, self.logger);

        // Create endless loop
        let loop_node = StructuredNode::loop_node(LoopType::Endless, None, loop_body, node_id);

        self.logger.debug(&format!(
            "DEBUG match_natural_loop: CREATED Endless loop at {:?}",
            node_id
        ));

        // Find exit successors (successors outside the loop)
        let mut exit_succs = Vec::new();
        for &block in &body_blocks {
            for &succ in self.region.successors(block) {
                if !loop_info.blocks.contains(&succ) && !exit_succs.contains(&succ) {
                    exit_succs.push(succ);
                }
            }
        }

        // Rewrite jumps to breaks and continues (for endless loops, any exit becomes a break)
        let loop_node =
            Self::rewrite_loop_jumps_multi_exit(loop_node, node_id, &exit_succs, self.logger);

        // Update the region
        self.region.nodes.insert(node_id, loop_node);

        // Remove loop body blocks
        for &block in &body_blocks {
            if block != node_id {
                self.region.remove_node(block);
            }
        }

        // Update edges
        self.region.edges.insert(node_id, exit_succs.clone());

        // Update exits' predecessors
        for &exit in &exit_succs {
            let preds = self.region.predecessors.entry(exit).or_default();
            preds.retain(|p| !loop_info.blocks.contains(p) || *p == node_id);
            if !preds.contains(&node_id) {
                preds.push(node_id);
            }
        }

        true
    }

    /// Print the current state of the region for debugging
    fn print_region_state(&self) {
        self.logger.debug("\n=== Region State ===");
        self.logger.debug(&format!("Nodes: {}", self.region.len()));
        self.logger
            .debug(&format!("Has cycles: {}", self.region.has_cycles()));
        self.logger.debug("\nNodes and their edges:");

        let mut node_ids: Vec<_> = self.region.nodes.keys().copied().collect();
        node_ids.sort();

        for node_id in node_ids {
            let node = &self.region.nodes[&node_id];
            let succs = self.region.successors(node_id);
            let preds = self.region.predecessors(node_id);

            self.logger.debug(&format!("  {:?}:", node_id));
            self.logger
                .debug(&format!("    Type: {}", Self::node_type_name(node)));
            self.logger.debug(&format!("    Predecessors: {:?}", preds));
            self.logger.debug(&format!("    Successors: {:?}", succs));

            // Show what kind of node this is
            match node {
                StructuredNode::Code { block } => {
                    self.logger
                        .debug(&format!("    Terminator: {:?}", block.terminator));
                }
                StructuredNode::Conditional {
                    condition_block, ..
                } => {
                    self.logger
                        .debug(&format!("    Condition block: {:?}", condition_block));
                }
                StructuredNode::Loop {
                    loop_type, header, ..
                } => {
                    self.logger.debug(&format!(
                        "    Loop type: {:?}, header: {:?}",
                        loop_type, header
                    ));
                }
                _ => {}
            }
        }

        // Show why patterns might not be matching
        self.logger.debug("\nPattern matching analysis:");
        for node_id in self.region.nodes.keys() {
            let succs = self.region.successors(*node_id);
            let _preds = self.region.predecessors(*node_id);

            if succs.len() == 1 {
                let succ = succs[0];
                let succ_preds = self.region.predecessors(succ);
                if succ_preds.len() == 1 {
                    self.logger.debug(&format!(
                        "  {:?} -> {:?}: Could be sequence (single pred/succ)",
                        node_id, succ
                    ));
                } else {
                    self.logger.debug(&format!(
                        "  {:?} -> {:?}: NOT sequence (succ has {} preds)",
                        node_id,
                        succ,
                        succ_preds.len()
                    ));
                }
            }

            if succs.len() == 2 {
                let left = succs[0];
                let right = succs[1];
                let left_preds = self.region.predecessors(left);
                let right_preds = self.region.predecessors(right);

                if left_preds.len() == 1 && right_preds.len() == 1 {
                    self.logger.debug(&format!(
                        "  {:?}: Could be ITE (both branches have single pred)",
                        node_id
                    ));
                } else {
                    self.logger.debug(&format!(
                        "  {:?}: NOT ITE (left has {} preds, right has {} preds)",
                        node_id,
                        left_preds.len(),
                        right_preds.len()
                    ));
                }
            }
        }
        self.logger.debug("===================\n");
    }

    /// Get a human-readable name for the node type
    fn node_type_name(node: &StructuredNode) -> &'static str {
        match node {
            StructuredNode::Sequence { .. } => "Sequence",
            StructuredNode::Conditional { .. } => "Conditional",
            StructuredNode::Loop { .. } => "Loop",
            StructuredNode::Break { .. } => "Break",
            StructuredNode::Continue { .. } => "Continue",
            StructuredNode::Code { .. } => "Code",
            StructuredNode::Empty => "Empty",
        }
    }
}
