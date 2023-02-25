use crate::ast::*;
use crate::evaluator::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use std::io::{self, Write};
use std::rc::Rc;

pub struct REPL {}

impl REPL {
    pub fn run() -> io::Result<()> {
        println!("Welcome to the REPL.");
        println!("Enter 'q' to quit the intrepreter.");
        let environment = Rc::new(Environment::global());
        loop {
            print!(">>> ");
            io::stdout().flush().unwrap();
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer)?;
            if buffer.as_str().trim() == "q" {
                break;
            }
            if buffer.as_str().trim() == "env" || buffer.as_str().trim() == "" {
                println!("{}", environment.inspect());
                continue;
            }
            let mut lexer = Lexer::new(buffer);
            let mut parser = Parser::new(lexer);
            match parser.parse_program() {
                Ok(program) => match eval(&program, &environment) {
                    Ok(value) => println!("{}", value.inspect()),
                    Err(e) => {
                        println!("You got an Evaluation error: {e}\nBut that's okay 😸");
                        continue;
                    }
                },
                Err(e) => {
                    println!("You got an Parsing error: {e}\nBut that's okay 😻");
                    continue;
                }
            }
        }

        println!("Farewell!");
        Ok(())
    }
}
