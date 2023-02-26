use crate::ast::*;
use crate::compiler::*;
use crate::evaluator::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::vm::*;
use std::io::{self, Write};
use std::rc::Rc;

pub struct REPL {}

impl REPL {
    pub fn run() -> io::Result<()> {
        println!("Welcome to the REPL.");
        println!("Enter 'q' to quit the intrepreter.");

        loop {
            print!(">>> ");
            io::stdout().flush().unwrap();
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer)?;

            if buffer.as_str().trim() == "q" {
                break;
            }

            let mut lexer = Lexer::new(buffer);
            let mut parser = Parser::new(lexer);

            match parser.parse_program() {
                Ok(program) => {
                    let mut compiler = Compiler::default();
                    compiler.compile(program);
                    let mut vm = VM::new(compiler.bytecode());
                    vm.run();
                    let top = vm.stack_top().unwrap();
                    println!("{top:?}",);
                }
                Err(err) => println!("Eeeek, failed to parse program."),
            }
        }

        println!("Farewell!");
        Ok(())
    }
}
