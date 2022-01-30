use std::future::Future;
use std::pin::Pin;

// Generic function that runs the provided async closure:
async fn run_async_closure<Arg, Ret>(
    closure: Box<dyn FnOnce(Arg) -> Pin<Box<dyn Future<Output = Ret>>>>,
    arg: Arg,
) -> Ret {
    closure(arg).await
}

#[tokio::main]
async fn main() {
    // Create and store an async closure:
    let closure: Box<dyn FnOnce(u32) -> Pin<Box<dyn Future<Output = u32>>>> = Box::new(move |x| {
        Box::pin(async move {
            println!("I am inside the async closure: {}", x);
            x + 1
        })
    });

    // Pass the async closure to the function:
    let ret = run_async_closure(closure, 5).await;

    println!("Result of the async closure: {}", ret);
}
