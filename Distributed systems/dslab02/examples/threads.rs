use std::thread::spawn;

fn simple_thread() {
    let thread = spawn(|| {
        println!("Inside a thread");
    });

    // Waiting for the thread to finish:
    thread.join().unwrap()
}

fn panicking_thread() {
    let thread = spawn(|| {
        // Panic in the thread:
        panic!("Panic attack!")
    });

    // The main thread still runs. However,
    // the result of `join()` is `Err` now:
    assert!(thread.join().is_err())
}

fn moving_values_into_thread() {
    let mut v = vec![1, 2, 3, 4];

    // Not every value can be moved into another thread, only these which are
    // Send. Most types are Send, but some are not (e.g., raw pointers and Rc).
    let thread = spawn(move || {
        v.push(5);
        println!("{:?}", v);
    });
    thread.join().unwrap();
}

fn main() {
    simple_thread();
    panicking_thread();
    moving_values_into_thread();
}
