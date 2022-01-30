use std::net::SocketAddr;
use std::ops::DerefMut;
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tokio::runtime::{Builder, Runtime};
use tokio::time::Duration;

fn tokio_runtimes() {
    // Create a single-threaded runtime.
    // All tasks are executed by the current thread:
    let single_thread_runtime = Builder::new_current_thread().build().unwrap();

    // Run a future to completion on the runtime:
    single_thread_runtime.block_on(async {
        println!("I am a future inside the single-threaded runtime.");
    });


    // Create a mult-threaded runtime, the default one.
    // Tasks are executed by an automatically created thread pool:
    let multi_threaded_runtime = Runtime::new().unwrap();

    // Run a future to completion on the runtime:
    multi_threaded_runtime.block_on(async {
        println!("I am a future inside the multi-threaded runtime.");
    });
}

// Usually, the runtime is created using the `#[tokio::main]` macro.
// Then calling the `tokio_macro()` function is, in fact, calling
// an automatically generated synchronous function, which builds the runtime
// and runs `block_on()` passing as the future the body of the function:
#[tokio::main]
async fn tokio_macro() {
    println!("I am a future inside a multi-threaded runtime created by macro.")
}

// Use `tokio::spawn()` to spawn a new asynchronous task. The new task will be
// executed concurrently to the other tasks (either by the current thread, or by
// another thread, depending on the runtime type). Spawning tasks is an important
// concept in Tokio:
#[tokio::main]
async fn tokio_spawning_tasks() {
    // Spawn a new task:
    let task = tokio::spawn(async {
        tokio::time::sleep(Duration::from_millis(200)).await;
        println!("I am inside the spawned task.");
        42
    });

    // The task spawned above is now blocked, sleeping 200 ms. However, because
    // it is executed asynchronously, the main task is continued to be executed
    // (you can try to add some delay also here to see that one task indeed does
    // not block the other one).

    println!("I am inside the main task.");

    // Spawning the task returned `JoinHandle`, which can be used to await
    // its completion:
    let result = task.await;

    // The result of the task is `Result`. It contains the result of the task
    // (when it succeeded), or an error (when the task panicked):
    match result {
        Ok(x) => println!("The result of the spawned task is: {}.", x),
        Err(e) => println!("The spawned task panicked: {}.", e)
    }
}

// Tokio provides an API for filesystem operations.
// As of now it does not use the operating system's asynchronous IO. Instead,
// under the hood it wraps blocking calls in `tokio::task::spawn_blocking()`:
#[tokio::main]
async fn tokio_filesystem() {
    let mut buffer = String::new();

    // Asynchronously open this file:
    let mut file = tokio::fs::File::open("examples/tokio.rs").await.unwrap();

    // Asynchronously read content of the file:
    file.read_to_string(&mut buffer).await.unwrap();

    println!("Read from the file: {}...", &buffer[..24]);

}

// Tokio provides API for network operations.
// Here is a TCP example (handling UDP is simillar):
#[tokio::main]
async fn tokio_network() {
    // Create a TCP socket server listening for connections on the address:
    let addr = "127.0.0.1:0".parse::<SocketAddr>().unwrap();
    let listener = TcpListener::bind(&addr).await.unwrap();
    let addr = listener.local_addr().unwrap();

    // In an asynchronous task...
    let task = tokio::spawn(async move {
        // ...open a client connection...
        let mut s = TcpStream::connect(addr).await.unwrap();
        // ...and send some data:
        s.write_all(b"Hello world network!").await.unwrap();
    });

    // Accept the incoming connection...
    let (mut s, _) = listener.accept().await.unwrap();
    // ...and read the sent data:
    let mut data = vec![];
    s.read_to_end(&mut data).await.unwrap();

    println!("Received over TCP: '{}'", String::from_utf8(data).unwrap());

    // Wait until the task completes successfully:
    assert!(matches!(task.await, Ok(_)));
}


// Tokio provides synchronization primitives:
#[tokio::main]
async fn tokio_sync() {
    // Standard (synchronous) Mutex should be used if it is expected to block
    // briefly or not at all:
    let shared_data = std::sync::Mutex::new(7);
    {
        *shared_data.lock().unwrap().deref_mut() = 10;
    }

    // Tokio (asynchronous) Mutex is more expensive, but can be hold across
    // `.await` points, and its `lock()` is an asynchronous method:
    let tokio_mutex = tokio::sync::Mutex::new(7);
    {
        *tokio_mutex.lock().await = 10;
        // ...here can be other `.await` points...
    }

    // Synchronizing accesses to some data from multiple Tokio task looks almost
    // the same as in a standard multi-threaded code:
    let data = Arc::new(tokio::sync::Mutex::new(7));
    let data_cloned = data.clone();

    tokio::spawn(async move {
        let mut lock = data_cloned.lock().await;
        *lock += 1;
    });

    let mut lock = data.lock().await;
    *lock += 1;

    // Tokio provides also other synchronization primitives, for example,
    // a counting semaphore. The semaphore below permits up to 4 concurrent
    // accesses:
    let tokio_semaphore = tokio::sync::Semaphore::new(4);
    {
        let _permit = tokio_semaphore.acquire().await.unwrap();
        // I am one of 4 accesses to some shared resource.
    }
}


fn main() {
    tokio_runtimes();
    tokio_macro();
    tokio_spawning_tasks();
    tokio_filesystem();
    tokio_network();
    tokio_sync();
}
