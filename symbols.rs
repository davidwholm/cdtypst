#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! typst = "0.12.0"
//! ```

use typst::foundations::*;
use typst::symbols::sym;

fn get_symbols(scope: &Scope) {
    for (id, val, _) in scope.iter() {
        match val {
            Value::Symbol(sym) => {
                for (inner_id, val) in sym.variants() {
                    let full_id: String = if inner_id.is_empty() { id.into() } else { vec![id, inner_id].join(".") };
                    print!("(\"{}\" . ", full_id);
                    let ch = val.char();
                    if ch == '(' || ch == ')' || ch == ';' || ch == '\\' || ch == '"' {
                      println!("?\\{} )", ch);
                    } else {
                      println!("?{} )", ch);
                    }
                }
            },
            _ => unreachable!(),
        }
    }
}

fn main() {
    print!("(");
    get_symbols(sym().scope());
    print!(")");
}
