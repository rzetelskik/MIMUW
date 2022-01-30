// An exemplary import from the standard library:
use std::ops::Add;

/// A simple function.
///
/// Note that doc comments in Rust start with `///` and support **Markdown**!
/// They can be then easily converted (by calling `cargo doc`) to a website, just
/// like [The Rust Standard Library Docs](https://doc.rust-lang.org/std/index.html).
/// Moreover, the doc comments may include examples which can be then
/// automatically tested!
fn simple_function() {
    // println! is a macro for printing:
    println!("Hello world!")

    // No semicolon after the last expression of the function, so this value
    // (the result of the macro call) is returned from the function.
}

fn may_panic() {
    // Result<T, E> is a return type of all operations that may fail.
    // It represents either the correct result or a failure. E.g.:
    let parsed_or_not: Result<u8, std::num::ParseIntError> = "7".to_string().parse::<u8>();

    // unwrap() calls panic! when the result is a failure (by default, in
    // a full-fledged OS, panic! stops the application and unwinds the stack).
    // Otherwise it returs the value. Let's try to get the number:
    println!("Parsed: {}", parsed_or_not.unwrap());
}

// The compiler warns about unused functions, but the warning can be suppressed
// with a special attribute. It also warns about unused variables, but
// the warning can be suppressed by prefixing them with an underscore. E.g.:
#[allow(dead_code)]
fn unused_function(_unused_variable: String) {
    // A macro to mark unimplemented parts. It panics when executed:
    unimplemented!()
}

#[allow(unused_assignments)]
fn mutability() {
    let i1: i32 = 42;
    // Won't compile:
    // i1 = 43;
    println!("Immutable i32: {}", i1);

    let mut i2: i32 = 42;
    i2 = 43;
    println!("Mutable i32: {}", i2);

    let a1: [u32; 2] = [0, 1];
    // Won't compile:
    // a1[0] = 42;
    println!("Immutable array with two u32 values: {:?}", a1);
    // Above `{:?}` marker displays arrays for debugging purposes.
    // Arrays don't implement the user-friendly formatting (the `{}` marker).

    let mut a2: [u32; 2] = [0, 1];
    a2[0] = 42;
    println!("Mutable array with two u32 values: {:?}", a2);

    let v1: Vec<i64> = vec![0, 1];
    // Above `vec!` is a macro which simplifies creation of vectors.
    // Won't compile:
    // v1.truncate(1);
    println!(
        "Immutable vector (heap-allocated) with two i64 values: {:?}",
        v1
    );

    let mut v2: Vec<i64> = vec![0, 1];
    v2.truncate(1);
    println!(
        "Mutable vector (heap-allocated) with two i64 values: {:?}",
        v2
    );

    let s1: String = String::from("Hello");
    // Won't compile:
    // s1.push('!');
    println!("Immutable string (with heap-allocated buffer): {}", s1);

    let mut s2: String = String::from("Hello");
    s2.push('!');
    println!("Mutable string (with heap-allocated buffer): {}", s2);
}

fn ownership_copy_move_clone() {
    let i1 = 42u32;
    // Above type of the variable is inferred from the type of the value: u32.
    let v1 = vec![0, 1];
    // Above type of the vector's elements defaults to i32.

    let i2 = i1;
    // Above value is copied so both variables own their own values.
    let v2 = v1;
    // Above value is moved so `v1` no longer owns the vector.
    let v3 = v2.clone();
    // Above value is cloned so `v3` owns a copy of the vector.

    println!("Old integer: {}", i1);
    // Won't compile:
    // println!("Old vector: {:?}", v1);
    println!("New integer: {}", i2);
    println!("Moved vector: {:?}", v2);
    println!("Cloned vector: {:?}", v3);
} // `v2` and `v3` go out of scope, so their values are dropped (the vectors are
  //  deallocated). `v1` also goes out of scope but since it doesn't own any value
  //  here, nothing is dropped (thus the vector isn't dropped twice).

fn ownership_functions() {
    let mut v: Vec<u8> = vec![0, 1];

    v = print_by_value_and_return(v);
    // Above vector is moved out of the variable to the function. Then it is
    // returned by the function and reassigned (moved) to the variable.

    print_by_value(v);
    // Above vector is moved out of the variable to the function.

    // Won't compile, `v` no longer owns the vector:
    // println!("Vector: {:?}", v);
}

fn print_by_value_and_return(v: Vec<u8>) -> Vec<u8> {
    println!("The vector: {:?}", v);
    v
    // The last expression of a function is its return value. There is no need
    // to write `return v;` (but one still can do so).
}

fn print_by_value(v: Vec<u8>) {
    println!("The vector: {:?}", v);
} // `v` goes out of scope so the vector is dropped when the function ends.

fn references_borrowing() {
    let mut v: Vec<u8> = vec![0, 1];

    print_by_reference(&v);
    // Above function call performs immutable borrowing.

    print_by_slice(&v[0..2]);
    // Above function call performs immutable borrowing
    // of the range [0th element, 2nd element) of the vector.

    // Multiple immutable references are safe:
    let v_imm_ref_1 = &v;
    let v_imm_ref_2 = &v;

    // Won't compile, there are already immutable references:
    // let v_mut_ref = &mut v;

    print_by_reference(v_imm_ref_2);
    print_by_reference(v_imm_ref_1);

    // The above immutable references are not used below so it is now safe
    // to have a mutable reference:
    let v_mut_ref = &mut v;
    add_by_mut_reference(v_mut_ref);
    print_by_reference(v_mut_ref);
}

fn print_by_reference(v: &Vec<u8>) {
    println!("The vector: {:?}", v);
}

fn add_by_mut_reference(v: &mut Vec<u8>) {
    v.push(42);
}

fn print_by_slice(v: &[u8]) {
    println!("The vector: {:?}", v);
}

// Won't compile, the lifetime of the returned reference is longer than
// the lifetime of the value:
// fn create_vector() -> &Vec<u8> {
//    let v: Vec<u8> = vec![0, 1];
//    &v
// }

// A definition of a struct. A #[derive] annotation is used to automatically
// implement Debug, Copy and Clone traits for it:
#[derive(Debug, Copy, Clone)]
struct ChattyInteger {
    num: i32,
}

// An implementation of the Add trait for the ChattyInteger struct
// (the Add trait defines implementation of the addition operator `+`):
impl Add for ChattyInteger {
    // Traits can have not only associated methods, but also associated types:
    type Output = Self; // Here Self refers to ChattyInteger.

    // A method:
    fn add(self, other: Self) -> Self {
        println!("Adding...");

        // Create (and return from the method) a new struct:
        Self {
            num: self.num + other.num,
        }
    }
}

// Now two ChattyIntegers can be added:
fn struct_example() {
    let i1 = ChattyInteger { num: 7 };
    let i2 = ChattyInteger { num: 8 };

    // Add them using the `+` operator:
    println!("Sum {:?}", i1 + i2);

    // Add them calling the add() method directly:
    println!("Sum {:?}", i1.add(i2));

    // Add them calling the add() method directly and using a more explicit syntax:
    println!("Sum {:?}", ChattyInteger::add(i1, i2));

    // Add them calling the add() method directly and using a fully qualified
    // syntax. It would be useful if ChattyInteger also implemented an add()
    // method, but we wanted to call the add() method of the Add trait
    // implemented for ChattyInteger:
    println!("Sum {:?}", <ChattyInteger as Add>::add(i1, i2));
}

// A simple test of the `+` implementation for ChattyInteger.
// Tests are usually implemented in a separate module which is marked with
// a #[cfg] annotation to be compiled only when `cargo test` is run:
#[cfg(test)]
mod tests {
    // Make ChattyInteger from the parent module accessible here as ChattyInteger:
    use super::ChattyInteger;

    #[test]
    fn chattyinteger_sum() {
        let i1 = ChattyInteger { num: 7 };
        let i2 = ChattyInteger { num: 8 };
        let sum = i1 + i2;
        assert_eq!(sum.num, 15);
    }
}

// A definition of an enum:
enum ProcessingUnit {
    // An option with an unnamed field:
    Cpu(u32),

    // An option with named fields (it looks similar to the struct)
    // and a lifetime parameter specifying the lifetime of the reference:
    Gpu { frequency: u128, name: &'static str },

    // An option without any fields:
    Tpu,
}

fn calculate_cost(pr: ProcessingUnit) -> u32 {
    // A simple match for the enum. Note that the match is an expression and
    // its value is the last expression at the matching branch:
    let cost_unit = match pr {
        ProcessingUnit::Cpu(v) => v,
        ProcessingUnit::Gpu {
            frequency: _freq, // freq is prefixed with _ because it is unused.
            name,
        } => name.len() as u32,
        ProcessingUnit::Tpu => 7,
    };

    // Integers can be pattern matched too:
    match cost_unit {
        0x00..=0xFF => 11,
        _ => 8,
    }
    // Above match is the last expression of the function so its value is returned.
}

fn enum_example() {
    println!(
        "Calculated cost: {}",
        calculate_cost(ProcessingUnit::Tpu)
            + calculate_cost(ProcessingUnit::Gpu {
                frequency: 10,
                name: "name"
            })
            + calculate_cost(ProcessingUnit::Cpu(11))
    );
}

// A definition of a generic enum that implements a generic list. Every distinct
// type gets its own compiled version of this enum, like in C++:
#[derive(Debug)]
enum List<T> {
    Head(T, Box<List<T>>),
    Nil,
}

// Functions can be generic too:
fn add_element_to_list<T>(list: List<T>, elem: T) -> List<T> {
    List::Head(elem, Box::new(list))
}

// An implementation of the Clone trait for List. A type parameter
// and a trait bound is needed:
impl<T> Clone for List<T>
where
    T: Clone, // Type T must implement Clone.
{
    fn clone(&self) -> Self {
        match self {
            List::Head(val, list) => List::Head(val.clone(), list.clone()),
            List::Nil => List::Nil,
        }
    }
}

fn generic_type_example() {
    println!(
        "Adding integer to list: {:?}",
        add_element_to_list(List::Nil, 7)
    );
    println!(
        "Adding string to list: {:?}",
        add_element_to_list(List::Nil, "test".to_string())
    );
}

/// The main function of this example.
fn main() {
    simple_function();
    may_panic();
    mutability();
    ownership_copy_move_clone();
    ownership_functions();
    references_borrowing();
    struct_example();
    enum_example();
    generic_type_example();
}
