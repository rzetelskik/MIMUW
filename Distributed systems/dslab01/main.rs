mod public_test;
mod solution;

use crate::solution::Fibonacci;
use std::env;
use std::process;

fn parse_arg() -> usize {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: cargo run <index of Fibonacci number>");
        process::exit(1);
    }

    match args.get(1).unwrap().parse::<usize>() {
        Ok(n) => n,
        Err(_) => {
            println!("The provided value cannot be converted to usize!");
            process::exit(1);
        }
    }
}

fn main() {
    let nth = parse_arg();
    println!("Calculating the {}-th Fibbonaci number...", nth);

    println!("fibonacci(): {}", Fibonacci::fibonacci(nth));

    let mut fib = Fibonacci::new();
    match fib.nth(nth) {
        Some(num) => println!("iterator: {}", num),
        None => println!("Sorry, the n-th Fibonacci number doesn't fit u128."),
    }
}
