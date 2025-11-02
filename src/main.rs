use clap::{Parser, Subcommand, ValueEnum};
use std::fs;
use std::panic;

mod bytecode;
mod dot;
mod formatters;

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

#[derive(Debug, Clone, Copy, ValueEnum)]
enum OutputFormat {
    Cpp,
    Asm,
    Analyze,
    Structured,
    Dot,
    Cfg,
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

            // Parse bytecode to IR - wrap in panic handler
            let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                let reader = ScriptReader::new(
                    script,
                    jmap.names.as_ref().expect("name map is required"),
                    &address_index,
                );
                let mut parser = ScriptParser::new(reader);
                let expressions = parser.parse_all();

                // Try to build CFG (with NullLogger to suppress debug output)
                let logger = NullLogger;
                let cfg_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                    ControlFlowGraph::from_expressions_with_logger(&expressions, &logger)
                }));

                let cfg = match cfg_result {
                    Ok(cfg) => cfg,
                    Err(_) => {
                        // CFG generation panicked
                        return (false, 0, 0, false, "cfg_panic".to_string());
                    }
                };

                let cfg_built = !cfg.blocks.is_empty();
                let num_blocks = cfg.blocks.len();

                // Try to analyze loops
                let (num_loops, structure_succeeded, structure_error) = if cfg_built {
                    let dom_tree = DominatorTree::compute(&cfg);
                    let loop_info = LoopInfo::analyze(&cfg, &dom_tree);
                    let num_loops = loop_info.loops.len();

                    // Try to structure (with NullLogger to suppress debug output)
                    let structure_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        let structurer =
                            PhoenixStructurer::new_with_logger(&cfg, &loop_info, &logger);
                        structurer.structure().is_some()
                    }));

                    let (structure_succeeded, structure_error) = match structure_result {
                        Ok(succeeded) => {
                            if succeeded {
                                (true, "".to_string())
                            } else {
                                (false, "structure_failed".to_string())
                            }
                        }
                        Err(_) => {
                            // Structuring panicked
                            (false, "structure_panic".to_string())
                        }
                    };

                    (num_loops, structure_succeeded, structure_error)
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

            let (cfg_built, num_blocks, num_loops, structure_succeeded, structure_error) =
                match result {
                    Ok(stats) => stats,
                    Err(_) => {
                        // Parser or overall processing panicked
                        (false, 0, 0, false, "parser_panic".to_string())
                    }
                };

            stats.push(FunctionStats {
                name: name.clone(),
                script_size: script.len(),
                cfg_built,
                num_blocks,
                num_loops,
                structure_succeeded,
                structure_error,
            });
        }
    }

    // Generate CSV output
    let csv_output = {
        let mut output = String::from(
            "function_name,script_size,cfg_built,num_blocks,num_loops,structure_succeeded,structure_error\n",
        );
        for stat in &stats {
            output.push_str(&format!(
                "\"{}\",{},{},{},{},{},\"{}\"\n",
                stat.name.replace('"', "\"\""),
                stat.script_size,
                stat.cfg_built,
                stat.num_blocks,
                stat.num_loops,
                stat.structure_succeeded,
                stat.structure_error
            ));
        }
        output
    };

    // Restore the default panic hook
    panic::set_hook(default_hook);

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
            if name.contains("ExecuteUbergraph") {
                continue;
            }
            // if func.r#struct.script.len() < 10000 {
            //     continue;
            // }

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

            println!("\n{}", "=".repeat(80));
            println!("Function: {}", name);
            println!("Address: {:?}", func.r#struct.object.address);
            println!("Flags: {:?}", func.function_flags);
            println!("Script size: {} bytes", script.len());
            println!("{}\n", "=".repeat(80));

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
                    let mut formatter = AsmFormatter::new(&address_index, referenced_offsets);
                    formatter.format(&expressions);
                }
                OutputFormat::Cpp => {
                    let mut formatter = CppFormatter::new(&address_index, referenced_offsets);
                    formatter.format(&expressions);
                }
                OutputFormat::Analyze => {
                    // Build and display the Control Flow Graph
                    let cfg = ControlFlowGraph::from_expressions(&expressions);
                    cfg.print_debug(&expressions, &address_index);

                    // Compute and display dominator tree
                    println!("\n{}", "=".repeat(80));
                    let dom_tree = DominatorTree::compute(&cfg);
                    dom_tree.print_debug();

                    // Detect and display loops
                    println!("\n{}", "=".repeat(80));
                    let loop_info = LoopInfo::analyze(&cfg, &dom_tree);
                    loop_info.print_debug();

                    // Compute and display post-dominator tree
                    println!("\n{}", "=".repeat(80));
                    let post_dom_tree = PostDominatorTree::compute(&cfg);
                    post_dom_tree.print_debug();

                    // Compute and display structured statements
                    println!("\n{}", "=".repeat(80));
                    let structurer = PhoenixStructurer::new(&cfg, &loop_info);
                    if let Some(structured) = structurer.structure() {
                        structured.print(&address_index);
                    } else {
                        eprintln!("Failed to fully structure the control flow");
                    }
                }
                OutputFormat::Structured => {
                    // Build CFG and analysis
                    let cfg = ControlFlowGraph::from_expressions(&expressions);
                    let dom_tree = DominatorTree::compute(&cfg);
                    let loop_info = LoopInfo::analyze(&cfg, &dom_tree);

                    // Structure the control flow
                    let structurer = PhoenixStructurer::new(&cfg, &loop_info);

                    if let Some(structured) = structurer.structure() {
                        structured.print(&address_index);
                    } else {
                        eprintln!("Failed to fully structure the control flow");
                    }
                }
                OutputFormat::Dot => {
                    // Build CFG and generate DOT graph
                    let cfg = ControlFlowGraph::from_expressions(&expressions);
                    let graph = cfg.to_dot(&expressions, &address_index);

                    let mut output = String::new();
                    graph
                        .write(&mut output)
                        .expect("Failed to generate DOT output");

                    render_dot_and_open(output);
                }
                OutputFormat::Cfg => {
                    // Build CFG and print in flat format with block IDs
                    let cfg = ControlFlowGraph::from_expressions(&expressions);

                    // Print blocks in order
                    for block in &cfg.blocks {
                        // Print block header as a styled label using Theme
                        println!(
                            "{}:",
                            formatters::theme::Theme::label(format!("Block_{}", block.id.0))
                        );

                        // Print statements using CppFormatter, filtering out execution flow ops
                        let mut formatter =
                            CppFormatter::new(&address_index, referenced_offsets.clone());
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

                        // Print CFG terminator instead of expression terminator
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
                                let cond_str = formatter.format_expr_inline(
                                    condition,
                                    &formatters::cpp::FormatContext::This,
                                );
                                println!(
                                    "    if ({}) goto {}; else goto {};",
                                    cond_str,
                                    formatters::theme::Theme::label(format!(
                                        "Block_{}",
                                        true_target.0
                                    )),
                                    formatters::theme::Theme::label(format!(
                                        "Block_{}",
                                        false_target.0
                                    ))
                                );
                            }
                            Terminator::DynamicJump => {
                                println!("    // dynamic jump");
                            }
                            Terminator::Return(expr) => {
                                let ret_str = formatter.format_expr_inline(
                                    expr,
                                    &formatters::cpp::FormatContext::This,
                                );
                                println!("    return {};", ret_str);
                            }
                            Terminator::None => unreachable!(),
                        }

                        println!();
                    }
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
