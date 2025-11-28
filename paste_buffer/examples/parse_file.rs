use paste_buffer::{Parser, Serializer};
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.cp>", args[0]);
        eprintln!("Example: {} tests/test.cp", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    println!("Reading file: {}", filename);
    let content = fs::read_to_string(filename).expect("Failed to read file");

    println!("Parsing...");
    let mut parser = Parser::new(content);

    match parser.parse() {
        Ok(buffer) => {
            println!("✓ Successfully parsed!");

            println!("{:#?}", buffer);

            println!("\nSerializing back to text...");
            let mut serializer = Serializer::new();
            let serialized = serializer.serialize(&buffer);
            println!("✓ Serialized to {} bytes", serialized.len());
        }
        Err(e) => {
            eprintln!("✗ Parse error:\n{}", e);
            std::process::exit(1);
        }
    }
}
