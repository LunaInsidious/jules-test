use mylang::lexer::Lexer;
use mylang::parser::Parser;
use mylang::evaluator::eval;
use mylang::environment::Environment;
use mylang::object::Object; // For inspecting the object
use std::io::{self, Write}; // For input/output

fn main() {
    println!("Mylang REPL (v0.1.0)");
    println!("Type 'exit' or press Ctrl-D to exit.");

    let mut env = Environment::new();

    loop {
        print!(">> ");
        io::stdout().flush().unwrap_or_else(|e| {
            eprintln!("Error flushing stdout: {}", e);
        });

        let mut input_line = String::new();
        match io::stdin().read_line(&mut input_line) {
            Ok(0) => {
                // EOF reached (Ctrl-D)
                println!("\nExiting REPL.");
                break;
            }
            Ok(_) => {
                let line_trimmed = input_line.trim();
                if line_trimmed == "exit" {
                    println!("Exiting REPL.");
                    break;
                }
                if line_trimmed.is_empty() {
                    continue;
                }

                let lexer = Lexer::new(line_trimmed.to_string());
                let mut parser = Parser::new(lexer);
                let program_node = parser.parse_program(); // program_node is ast::Program

                let errors = parser.get_errors();
                if !errors.is_empty() {
                    for error in errors {
                        eprintln!("Parser Error: {}", error);
                    }
                    continue;
                }

                // ast::Program implements ast::Node, so we can Box it.
                match eval(Box::new(program_node), &mut env) {
                    Some(evaluated_object) => {
                        // Print the inspected object, unless it's specifically Null from a let statement
                        // (or other statements that shouldn't print anything in a REPL).
                        // For now, let's print all Some results.
                        // REPLs often don't print `null` for `let x = 5;` but do for `x;` if x is null.
                        // Our current eval returns Some(Object::Null) for let statements.
                        // We can choose to print it or not based on typical REPL behavior.
                        // For now, printing all Some results for clarity.
                        println!("{}", evaluated_object.inspect());
                    }
                    None => {
                        // This case might occur if eval truly returns None (e.g. unhandled node type),
                        // or if an error occurred that wasn't converted to an Object::Error.
                        // Our current evaluator returns Some(Object::Error(...)) for runtime errors.
                        // So, if eval returns None, it's likely an issue in eval logic itself.
                        // eprintln!("Evaluation returned no object (or an unhandled error).");
                        // For now, if eval returns None, we'll print nothing, assuming errors are Object::Error
                    }
                }
            }
            Err(error) => {
                eprintln!("Error reading input: {}", error);
                break; // Exit on read error
            }
        }
    }
}
