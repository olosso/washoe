#![allow(clippy::upper_case_acronyms, clippy::useless_format)]
#![allow(dead_code, unused)]

mod ast;
mod code;
mod compiler;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod vm;

fn main() {
    repl::REPL::run();
}
