#![allow(clippy::upper_case_acronyms)]
#![allow(dead_code, unused)]

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    repl::REPL::run();
}
