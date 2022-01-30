use futures::future::{join, join_all, ready};
use futures::{FutureExt, TryFutureExt};


fn simple_future() {
    // Crate a future that is immediately ready with
    // a value which is then transformed:
    let future = ready(7).map(|x| x * 2);

    // Run the future to completion in the current thread:
    let result = futures::executor::block_on(future);

    println!("Result of the simple future: {}", result);
}


// Futures can be combined into a new future:
fn futures_combining() {
    // Create a future that is immediately ready with a success value:
    let future: futures::future::Ready<std::result::Result<i32, ()>> =
        futures::future::ok(7);

    // Create a new future which will execute another future (created by
    // a closure) when the future successes:
    let combined_future = future.and_then(|x| futures::future::ok(x * x));

    // Run the future to completion in the current thread:
    let result = futures::executor::block_on(combined_future).unwrap();

    println!("Result of the combined future: {}", result);
}


// Futures can be joined into a new future:
fn futures_joining() {
    // Create a vector of futures:
    let futures = vec![ready(1), ready(2), ready(3)];

    // Create a new future which represents a collection
    // of outputs of the futures:
    let joined_future = join_all(futures);

    // Run the future to completion in the current thread:
    let result = futures::executor::block_on(joined_future);

    println!("Result of the joined future: {:?}", result);
}


// Function can be made asynchronous (which turns their return type into a future)
// by using the `async` keyword:
async fn async_function() {
    println!("I'm an async function.");
}

fn run_async_function() {
    // Create a future by calling an asynchronous function:
    let future = async_function();

    // Run the future to completion in the current thread:
    futures::executor::block_on(future);
}


async fn action1_step1() -> String {
    String::from("Step 1 of Action 1")
}

async fn action1_step2(str: String) {
    println!("Step 2 of Action 1 follows {}", str);
}

// By using `.await` we do not block the thread until each step is completed.
// Contrarily, we make it possible to advance the other action when the futures
// are not completed:
async fn action1() {
    // Run the first step and wait until it completes:
    let partial_result = action1_step1().await;

    // Run the second step (using the result of the first step)
    // and wait until it completes:
    action1_step2(partial_result).await;
}

async fn action2_step1() -> String {
    String::from("Step 1 of Action 2")
}

async fn action2_step2(str: String) {
    println!("Step 2 of Action 2 follows {}", str);
}

// By using `.await` we do not block the thread until each step is completed.
// Contrarily, we make it possible to advance the other action when the futures
// are not completed:
async fn action2() {
    // Run the first step and wait until it completes:
    let partial_result = action2_step1().await;

    // Run the second step (using the result of the first step)
    // and wait until it completes:
    action2_step2(partial_result).await;
}

fn run_asychronous_actions() {
    // Join two futures into a new future:
    let future = join(action1(), action2());

    // Run the future to completion in the current thread:
    futures::executor::block_on(future);
}


fn main() {
    simple_future();
    futures_combining();
    futures_joining();
    run_async_function();
    run_asychronous_actions();
}
