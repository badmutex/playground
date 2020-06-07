use std::env;
mod ast;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub calc);

fn main() {
    let expr: String = env::args().nth(1).unwrap();
    let val = calc::ExprParser::new().parse(&expr);
    println!("{:#?}", val);
    println!("{}", val.unwrap().eval());
}
