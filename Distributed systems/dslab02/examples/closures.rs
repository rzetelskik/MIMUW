fn simple_closure() {
    // Closure is a function which can be created with a special
    // syntax. The closure below is equivalent to defining
    // an `inc(u32)` function:
    let inc: fn(u32) -> u32 = |x: u32| x + 1;
    println!("simple_closure: {}", inc(inc(0)))
}

fn closure_environment() {
    let allowed_numbers = vec![2, 7];
    let vec = vec![1, 2, 3, 4, 5, 6];

    // The closure used below to filter the vector captures its environment:
    // the immutable reference to `allowed_numbers` is passed into the closure.
    // This is the default way the capturing of the environment works. Note that
    // the closure cannot outlive any of the borrowed references, the same as
    // any value in Rust:
    let filtered: Vec<i32> = vec
        .into_iter()
        .filter(|x| allowed_numbers.contains(x))
        .collect();

    println!("closure_environment: {:?}", filtered);
}

fn environment_capture_rules() {
    use std::mem;
    let movable = Box::new(3);
    let borrowed = Box::new(3);
    let mut not_captured = Box::new(3);

    // When capturing the environment, the compiler analyzes each variable
    // whether it shall be passed as an immutable reference, or as a mutable
    // reference, or it shall be moved into the closure. In the example below,
    // `mem::drop()` requires `T` so the `movable` variable has to be captured
    // by value.  To this end, if a type implements the `Copy` trait, the value
    // is copied to the closure. If a type does not implement the `Copy` trait,
    // as is the case below, the value is moved into the closure:
    let consume = || {
        println!("`movable`: {:?}", movable);

        // Dispose the value:
        mem::drop(movable);
        // `mem::drop()` has a very interesting implementation. We suggest
        // reading its source or the documentation.

        // Won't compile, the value of `movable` was already moved (disposed):
        // println!("`movable`: {:?}", movable);

        // It is enough to have in this closure an immutable reference to
        // `borrowed`, so it is captured by an immutable reference:
        println!("`borrowed`: {:?}", borrowed);
    };

    // Variables not used in the closure are not captured to its environment:
    *not_captured = 4;

    // Run `consume()`:
    consume();

    // Won't compile, the value of `movable` was already moved:
    // consume();

    // `borrowed` was immutably borrowed by the closure, so it can still be used:
    println!("`borrowed`: {:?}", borrowed);
}

fn closure_moving_values() {
    let mut vec: Vec<u32> = vec![];

    // The `move` keyword forces moving the value of `vec` into a closure
    // (the ownership is transferred to the closure):
    let mut add_to_vec = move |x: u32| {
        vec.push(x);
        println!("closure_moving_values: {:?}", vec);
    };

    // Won't compile, the value of `vec` was moved to the closure:
    // println!("{:?}", vec);

    add_to_vec(1);
    add_to_vec(2);
}

fn closure_mutable_borrow_of_ownership() {
    let mut vec: Vec<u32> = vec![];

    {
        // Below is no `move` keyword, so the value of `vec` is mutably borrowed:
        let mut add_to_vec = |x: u32| {
            vec.push(x);
            println!("closure_mutable_borrow_of_ownership: {:?}", vec);
        };

        add_to_vec(1);
        add_to_vec(2);
    } // `add_to_vec` goes out of scope so the reference to `vec` is dropped.

    // `vec` can still be used:
    println!("{:?}", vec);
}

// Closure is also a trait: `Fn<Args>`. This way, closures can be stored
// in structs and returned from functions:
fn closure_trait() -> Box<dyn Fn(u32) -> bool> {
    let even: Box<dyn Fn(u32) -> bool> = Box::new(|x: u32| x % 2 == 0);

    println!("closure_trait: {:?}", even(8));

    even
}

// More specifically, there are three different types of closures:
// * `FnOnce<Args> – a closure that can modify its environment, and that can be
//   run only once (if you find it weird to restrict a closure to run only once,
//   consider again the example `environment_capture_rules()`),
// * `FnMut<Args>` – a closure that can modify its environment, and that can be
//   run multiple times (it is a subtrait of `FnOnce`),
// * `Fn<Args>` – a closure that cannot modify its environment, and that can be
//   run multiple times (it is a subtrait of `FnMut`).
struct ClosureHolder {
    #[allow(clippy::unused_unit)]
    fn_once_closure: Box<dyn FnOnce(u32) -> ()>,
    // If the returned type is the unit, it does not have to be specified:
    fn_once_closure_2: Box<dyn FnOnce(u32)>,

    fn_mut_closure: Box<dyn FnMut(u32)>,

    fn_closure: Box<dyn Fn(u32)>,
}

fn closures_traits() {
    let mut env1 = vec![1];
    let mut env2 = vec![2];
    let mut env3 = vec![3];
    #[allow(unused_mut)]
    let mut env4 = vec![4];

    let mut holder = ClosureHolder {
        fn_once_closure: Box::new(move |x| {
            env1.push(x);
            println!("FnOnce: {:?}", env1)
        }),
        fn_once_closure_2: Box::new(move |x| {
            env2.push(x);
            println!("FnOnce: {:?}", env2)
        }),
        fn_mut_closure: Box::new(move |x| {
            env3.push(x);
            println!("FnMut: {:?}", env3)
        }),
        fn_closure: Box::new(move |_x| {
            // Won't compile, `Fn` cannot modify the environment:
            // env4.push(x);
            println!("Fn: {:?}", env4)
        }),
    };

    (holder.fn_once_closure)(1);
    // Won't compile, this closure was already run:
    // (holder.once_closure)(2);

    (holder.fn_once_closure_2)(3);

    (holder.fn_mut_closure)(4);
    (holder.fn_mut_closure)(4);

    (holder.fn_closure)(6);
    (holder.fn_closure)(6);
}

fn main() {
    simple_closure();
    closure_environment();
    environment_capture_rules();
    closure_moving_values();
    closure_mutable_borrow_of_ownership();
    let _ = closure_trait();
    closures_traits();
}
