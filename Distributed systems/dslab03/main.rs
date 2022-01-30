mod public_test;
mod solution;

use std::env;
use std::process;

fn parse_args() -> usize {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => 10,
        2 => match args.get(1).unwrap().parse() {
            Ok(n) => n,
            Err(_) => {
                println!("Provide an unsigned number as the program argument!");
                process::exit(1);
            }
        },
        _ => {
            println!("Provide only one argument: an index of the Fibonacci number.");
            process::exit(1);
        }
    }
}

fn main() {
    solution::fib(parse_args());
}

// Sample result of `cargo run`:
//
// Inside 109630159281952332903990402733657913010, value: 1
// Inside 90776690821287183793072922585987852479, value: 2
// Inside 109630159281952332903990402733657913010, value: 3
// Inside 90776690821287183793072922585987852479, value: 5
// Inside 109630159281952332903990402733657913010, value: 8
// Inside 90776690821287183793072922585987852479, value: 13
// Inside 109630159281952332903990402733657913010, value: 21
// Inside 90776690821287183793072922585987852479, value: 34
// Inside 109630159281952332903990402733657913010, value: 55
//
// The identifiers are random, of course.
