use std::ops::Deref;
use std::rc::Rc;

fn box_example() {
    let array_on_heap: Box<[u32]> = Box::new([1, 2, 3, 4, 5, 42]);

    println!("An array stored on the heap: {:?}", array_on_heap);

    // Iterate over the array. Use dereferencing (`deref()`) to access
    // the array wrapped in the Box (i.e., to obtain `&T` from `Box<T>`):
    for i in array_on_heap.deref().iter() {
        println!("Integer from heap: {}", i);
    }

    // However, it is not necessary to explicitly use `deref()` here,
    // as the compiler can dereference it implicitly:
    for i in array_on_heap.iter() {
        println!("Integer from heap: {}", i);
    }

    // Note that in Rust you can define a function inside a function!
    fn take_box_ownership(mut boxed: Box<[u32]>) {
        *boxed.get_mut(0).unwrap() = 14;
        // Above we use `*` to dereference.
        println!("Owned box: {:#?}", boxed);
    }
    take_box_ownership(array_on_heap);
    // `array_on_heap` cannot be used anymore as the value was moved.
}

fn rc_example() {
    let array_on_heap: Rc<[u32]> = Rc::new([1, 2, 3, 4, 5, 42]);

    let array_on_heap_2 = array_on_heap.clone();
    // Above variable is a new reference to the same array.

    println!(
        "Two references to an array on the heap: {:?} and {:?}",
        array_on_heap, array_on_heap_2
    );

    // Iterate over the array dereferencing explicitly:
    for i in array_on_heap.deref().iter() {
        println!("Integer from heap: {}", i);
    }

    // Iterate over the array dereferencing implicitly:
    for i in array_on_heap.iter() {
        println!("Integer from heap: {}", i);
    }

    // Note that it is not possible to mutate data inside `Rc` if
    // there are other pointers to the data (it would not be safe!).
    // (If you need such a structure read about `Cell`/`RefCell`).
    fn take_rc_ownership(rc: Rc<[u32]>) {
        println!("Owned rc: {:#?}", rc);
    } // The reference counter is decreased automatically by Rust
      // when `rc` goes out of scope.

    take_rc_ownership(array_on_heap.clone());
    // In the above function call a new pointer is moved to the function,
    // as `array_on_heap` is cloned (so its reference counter is increased
    // by one). `array_on_heap` is still a valid reference.

    take_rc_ownership(array_on_heap);
    // Above function call moves the pointer to the function.

    // Won't compile, the pointer was already moved out of the variable:
    // take_rc_ownership(array_on_heap);
}

fn main() {
    box_example();
    rc_example();
}
