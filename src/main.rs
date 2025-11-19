use clap::{Parser, Subcommand, ValueEnum};
use std::fs;
use std::panic;

mod bytecode;
mod dot;
mod formatters;
mod paste;

use crate::{
    bytecode::{
        address_index::AddressIndex,
        cfg::{ControlFlowGraph, Terminator},
        dominators::{DominatorTree, PostDominatorTree},
        expr::{ExprKind, collect_referenced_offsets},
        logger::NullLogger,
        loops::LoopInfo,
        parser::ScriptParser,
        reader::ScriptReader,
        structured::PhoenixStructurer,
    },
    formatters::{asm::AsmFormatter, cpp::CppFormatter},
};

#[derive(Debug)]
struct FunctionStats {
    name: String,
    script_size: usize,
    cfg_built: bool,
    num_blocks: usize,
    num_loops: usize,
    structure_succeeded: bool,
    structure_error: String,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum OutputFormat {
    Cpp,
    Asm,
    Analyze,
    Structured,
    Dot,
    Cfg,
    Paste,
}

#[derive(Parser, Debug)]
#[command(name = "jmap-kismet")]
#[command(about = "JMAP bytecode analysis and decompilation tool")]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Disassemble and analyze functions
    Disassemble {
        /// Path to the JMAP file
        jmap_file: String,

        /// Filter functions by name (optional)
        #[arg(short, long)]
        filter: Option<String>,

        /// Output format
        #[arg(short = 'o', long, default_value = "cpp")]
        format: OutputFormat,

        /// Show block ID comments in structured output
        #[arg(long)]
        show_block_ids: bool,

        /// Show bytecode offset comments in structured output
        #[arg(long)]
        show_bytecode_offsets: bool,

        /// Show terminator expressions as comments in structured output
        #[arg(long)]
        show_terminator_exprs: bool,
    },
    /// Generate CSV statistics for all functions
    Stats {
        /// Path to the JMAP file
        jmap_file: String,

        /// Filter functions by name (optional)
        #[arg(short, long)]
        filter: Option<String>,

        /// Output CSV file path (defaults to stdout)
        #[arg(short, long)]
        output: Option<String>,
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Disassemble {
            jmap_file,
            filter,
            format,
            show_block_ids,
            show_bytecode_offsets,
            show_terminator_exprs,
        } => {
            run_disassemble(
                &jmap_file,
                filter,
                format,
                show_block_ids,
                show_bytecode_offsets,
                show_terminator_exprs,
            );
        }
        Commands::Stats {
            jmap_file,
            filter,
            output,
        } => {
            run_stats(&jmap_file, filter, output);
        }
    }
}

fn load_jmap(jmap_file: &str) -> jmap::Jmap {
    eprintln!("Loading JMAP file: {}", jmap_file);

    let jmap_data = match fs::read_to_string(jmap_file) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    let jmap: jmap::Jmap = match serde_json::from_str(&jmap_data) {
        Ok(jmap) => jmap,
        Err(e) => {
            eprintln!("Error parsing JMAP JSON: {}", e);
            std::process::exit(1);
        }
    };

    eprintln!("Loaded JMAP with {} objects", jmap.objects.len());

    jmap
}

fn collect_function_stats(
    name: &str,
    script: &[u8],
    jmap: &jmap::Jmap,
    address_index: &AddressIndex,
) -> FunctionStats {
    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        let reader = ScriptReader::new(
            script,
            jmap.names.as_ref().expect("name map is required"),
            address_index,
        );
        let mut parser = ScriptParser::new(reader);
        let expressions = parser.parse_all();

        // Try to build CFG
        let logger = NullLogger;
        let cfg_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            ControlFlowGraph::from_expressions_with_logger(&expressions, &logger)
        }));

        let cfg = match cfg_result {
            Ok(cfg) => cfg,
            Err(_) => return (false, 0, 0, false, "cfg_panic".to_string()),
        };

        let cfg_built = !cfg.blocks.is_empty();
        let num_blocks = cfg.blocks.len();

        // Try to analyze loops and structure
        let (num_loops, structure_succeeded, structure_error) = if cfg_built {
            let dom_tree = DominatorTree::compute(&cfg);
            let loop_info = LoopInfo::analyze(&cfg, &dom_tree);
            let num_loops = loop_info.loops.len();

            let structure_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                let structurer = PhoenixStructurer::new_with_logger(&cfg, &loop_info, &logger);
                structurer.structure().is_some()
            }));

            match structure_result {
                Ok(succeeded) => {
                    if succeeded {
                        (num_loops, true, String::new())
                    } else {
                        (num_loops, false, "structure_failed".to_string())
                    }
                }
                Err(_) => (num_loops, false, "structure_panic".to_string()),
            }
        } else {
            (0, false, "cfg_empty".to_string())
        };

        (
            cfg_built,
            num_blocks,
            num_loops,
            structure_succeeded,
            structure_error,
        )
    }));

    let (cfg_built, num_blocks, num_loops, structure_succeeded, structure_error) = match result {
        Ok(stats) => stats,
        Err(_) => (false, 0, 0, false, "parser_panic".to_string()),
    };

    FunctionStats {
        name: name.to_string(),
        script_size: script.len(),
        cfg_built,
        num_blocks,
        num_loops,
        structure_succeeded,
        structure_error,
    }
}

fn generate_csv(stats: &[FunctionStats]) -> String {
    let mut output = String::from(
        "function_name,script_size,cfg_built,num_blocks,num_loops,structure_succeeded,structure_error\n",
    );
    for stat in stats {
        output.push_str(&format!(
            "\"{}\",{},{},{},{},{},\"{}\"\n",
            stat.name.replace('\"', "\"\""),
            stat.script_size,
            stat.cfg_built,
            stat.num_blocks,
            stat.num_loops,
            stat.structure_succeeded,
            stat.structure_error
        ));
    }
    output
}

fn run_stats(jmap_file: &str, filter: Option<String>, output: Option<String>) {
    // Set a custom panic hook to suppress panic messages during stats collection
    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {
        // Silently ignore panics - they're caught and reported in the CSV
    }));

    let jmap = load_jmap(jmap_file);

    // Build address index for resolving object and property references
    let address_index = AddressIndex::new(&jmap);
    eprintln!(
        "Built address index with {} entries",
        address_index.object_index.len() + address_index.property_index.len()
    );

    let mut stats: Vec<FunctionStats> = Vec::new();

    for (name, obj) in &jmap.objects {
        if let jmap::ObjectType::Function(func) = obj {
            // Apply filter if specified
            if let Some(ref filter_str) = filter
                && !name.contains(filter_str)
            {
                continue;
            }

            let script = &func.r#struct.script;
            if script.is_empty() {
                continue;
            }

            stats.push(collect_function_stats(name, script, &jmap, &address_index));
        }
    }

    // Restore the default panic hook
    panic::set_hook(default_hook);

    let csv_output = generate_csv(&stats);

    // Write to file or stdout
    if let Some(output_path) = output {
        if let Err(e) = fs::write(&output_path, csv_output) {
            eprintln!("Error writing CSV file: {}", e);
            std::process::exit(1);
        }
        eprintln!("CSV written to: {}", output_path);
        eprintln!("Processed {} functions", stats.len());
    } else {
        print!("{}", csv_output);
        eprintln!("Processed {} functions", stats.len());
    }
}

fn print_function_header(name: &str, func: &jmap::Function) {
    println!("\n{}", "=".repeat(80));
    println!("Function: {}", name);
    println!("Address: {:?}", func.r#struct.object.address);
    println!("Flags: {:?}", func.function_flags);
    println!("Script size: {} bytes", func.r#struct.script.len());
    println!("{}\n", "=".repeat(80));
}

fn format_as_asm(
    expressions: &[bytecode::expr::Expr],
    address_index: &AddressIndex,
    referenced_offsets: std::collections::HashSet<bytecode::types::BytecodeOffset>,
) {
    let mut formatter = AsmFormatter::new(address_index, referenced_offsets);
    formatter.format(expressions);
}

fn format_as_cpp(
    expressions: &[bytecode::expr::Expr],
    address_index: &AddressIndex,
    referenced_offsets: std::collections::HashSet<bytecode::types::BytecodeOffset>,
) {
    let mut formatter = CppFormatter::new(address_index, referenced_offsets);
    formatter.format(expressions);
}

fn format_as_analyze(expressions: &[bytecode::expr::Expr], address_index: &AddressIndex) {
    let cfg = ControlFlowGraph::from_expressions(expressions);
    cfg.print_debug(expressions, address_index);

    println!("\n{}", "=".repeat(80));
    let dom_tree = DominatorTree::compute(&cfg);
    dom_tree.print_debug();

    println!("\n{}", "=".repeat(80));
    let loop_info = LoopInfo::analyze(&cfg, &dom_tree);
    loop_info.print_debug();

    println!("\n{}", "=".repeat(80));
    let post_dom_tree = PostDominatorTree::compute(&cfg);
    post_dom_tree.print_debug();

    println!("\n{}", "=".repeat(80));
    let structurer = PhoenixStructurer::new(&cfg, &loop_info);
    if let Some(structured) = structurer.structure() {
        structured.print(address_index);
    } else {
        eprintln!("Failed to fully structure the control flow");
    }
}

fn format_as_structured(expressions: &[bytecode::expr::Expr], address_index: &AddressIndex) {
    let cfg = ControlFlowGraph::from_expressions(expressions);
    let dom_tree = DominatorTree::compute(&cfg);
    let loop_info = LoopInfo::analyze(&cfg, &dom_tree);

    let structurer = PhoenixStructurer::new(&cfg, &loop_info);
    if let Some(structured) = structurer.structure() {
        structured.print(address_index);
    } else {
        eprintln!("Failed to fully structure the control flow");
    }
}

fn format_as_dot(expressions: &[bytecode::expr::Expr], address_index: &AddressIndex) {
    let cfg = ControlFlowGraph::from_expressions(expressions);
    let graph = cfg.to_dot(expressions, address_index);

    let mut output = String::new();
    graph
        .write(&mut output)
        .expect("Failed to generate DOT output");

    render_dot_and_open(output);
}

fn format_as_cfg(
    expressions: &[bytecode::expr::Expr],
    address_index: &AddressIndex,
    referenced_offsets: std::collections::HashSet<bytecode::types::BytecodeOffset>,
) {
    let cfg = ControlFlowGraph::from_expressions(expressions);

    for block in &cfg.blocks {
        println!(
            "{}:",
            formatters::theme::Theme::label(format!("Block_{}", block.id.0))
        );

        let mut formatter = CppFormatter::new(address_index, referenced_offsets.clone());
        formatter.set_indent_level(1);
        for stmt in &block.statements {
            match &stmt.kind {
                ExprKind::PushExecutionFlow { .. }
                | ExprKind::PopExecutionFlow
                | ExprKind::PopExecutionFlowIfNot { .. } => {
                    continue;
                }
                _ => {
                    formatter.format_statement(stmt);
                }
            }
        }

        match &block.terminator {
            Terminator::Goto { target } => {
                println!(
                    "    goto {};",
                    formatters::theme::Theme::label(format!("Block_{}", target.0))
                );
            }
            Terminator::Branch {
                condition,
                true_target,
                false_target,
            } => {
                let cond_str =
                    formatter.format_expr_inline(condition, &formatters::cpp::FormatContext::This);
                println!(
                    "    if ({}) goto {}; else goto {};",
                    cond_str,
                    formatters::theme::Theme::label(format!("Block_{}", true_target.0)),
                    formatters::theme::Theme::label(format!("Block_{}", false_target.0))
                );
            }
            Terminator::DynamicJump => {
                println!("    // dynamic jump");
            }
            Terminator::Return(expr) => {
                let ret_str =
                    formatter.format_expr_inline(expr, &formatters::cpp::FormatContext::This);
                println!("    return {};", ret_str);
            }
            Terminator::None => unreachable!(),
        }

        println!();
    }
}

fn run_disassemble(
    jmap_file: &str,
    filter: Option<String>,
    format: OutputFormat,
    _show_block_ids: bool,
    _show_bytecode_offsets: bool,
    _show_terminator_exprs: bool,
) {
    let jmap = load_jmap(jmap_file);

    // Build address index for resolving object and property references
    let address_index = AddressIndex::new(&jmap);
    eprintln!(
        "Built address index with {} entries",
        address_index.object_index.len() + address_index.property_index.len()
    );

    // Count and disassemble functions
    let mut function_count = 0;
    let mut disassembled_count = 0;

    for (name, obj) in &jmap.objects {
        if let jmap::ObjectType::Function(func) = obj {
            function_count += 1;

            // Skip ExecuteUbergraph functions
            if name.contains("ExecuteUbergraph") {
                continue;
            }

            // Apply filter if specified
            if let Some(ref filter_str) = filter
                && !name.contains(filter_str)
            {
                continue;
            }

            let script = &func.r#struct.script;
            if script.is_empty() {
                continue;
            }

            disassembled_count += 1;

            print_function_header(name, func);

            // Parse bytecode to IR
            let reader = ScriptReader::new(
                script,
                jmap.names.as_ref().expect("name map is required"),
                &address_index,
            );
            let mut parser = ScriptParser::new(reader);
            let expressions = parser.parse_all();

            // Collect all referenced bytecode offsets
            let referenced_offsets = collect_referenced_offsets(&expressions);

            // Format based on output type
            match format {
                OutputFormat::Asm => {
                    format_as_asm(&expressions, &address_index, referenced_offsets)
                }
                OutputFormat::Cpp => {
                    format_as_cpp(&expressions, &address_index, referenced_offsets)
                }
                OutputFormat::Analyze => format_as_analyze(&expressions, &address_index),
                OutputFormat::Structured => format_as_structured(&expressions, &address_index),
                OutputFormat::Dot => format_as_dot(&expressions, &address_index),
                OutputFormat::Cfg => {
                    format_as_cfg(&expressions, &address_index, referenced_offsets)
                }
                OutputFormat::Paste => {
                    paste::format_as_paste(&expressions, &address_index, name, func)
                }
            }
        }
    }

    println!("\n{}", "=".repeat(80));
    println!("Summary:");
    println!("  Total functions: {}", function_count);
    println!("  Disassembled: {}", disassembled_count);
    println!("{}", "=".repeat(80));
}

fn render_dot_and_open(dot: String) {
    let dot_path = "/tmp/graph.dot";
    let svg_path = "/tmp/graph.svg";

    if let Err(e) = std::fs::write(dot_path, &dot) {
        eprintln!("Failed to write DOT file: {}", e);
    } else {
        eprintln!("Graph saved to: {}", dot_path);

        // Generate SVG with dot
        match std::process::Command::new("dot")
            .arg("-Tsvg")
            .arg(dot_path)
            .arg("-o")
            .arg(svg_path)
            .status()
        {
            Ok(status) if status.success() => {
                eprintln!("SVG generated: {}", svg_path);

                // Open in Firefox
                match std::process::Command::new("firefox").arg(svg_path).spawn() {
                    Ok(_) => eprintln!("Opened in Firefox"),
                    Err(e) => eprintln!("Failed to open Firefox: {}", e),
                }
            }
            Ok(status) => eprintln!("dot command failed with status: {}", status),
            Err(e) => eprintln!("Failed to run dot: {}", e),
        }
    }
}
