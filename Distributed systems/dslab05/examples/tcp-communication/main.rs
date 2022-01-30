mod client;
mod server;

use crossbeam_channel::unbounded;
use std::net::{TcpListener, TcpStream};
use std::time::Duration;

fn tcp_stream_api_example(stream: &mut TcpStream) {
    // Set the value of the `TCP_NODELAY` option for this socket:
    stream.set_nodelay(true).unwrap();

    // Get the value of the `TCP_TTL` option for this socket:
    println!("TTL: {}", stream.ttl().unwrap());

    // Get the write timeout of this socket:
    println!("Write timeout: {:#?}", stream.write_timeout().unwrap());

    // Set the read timeout for this socket:
    stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .unwrap();

    {
        // Cloning the stream creates a new handle to the same OS socket:
        let _cloned = stream.try_clone().unwrap();
    }
}

fn tcp_listener_api_example() {
    // Create a `TcpListener` by binding to a local address:
    let listener = TcpListener::bind("127.0.0.1:8888").unwrap();

    // Get an interator over incoming connections:
    for _stream in listener.incoming().take(0) {
        // Process TcpStream...

        // In this simple example there are not any incoming connections,
        // so no elements are actually taken from the iterator (`take(0)`)
        // not to block the execution.
    }

    // Alternatively, a loop calling the `accept()` method can be used:
    while false {
        match listener.accept() {
            Ok((mut stream, addr)) => {
                println!("New client: {:?}", addr);
                tcp_stream_api_example(&mut stream);
            }
            Err(e) => println!("Couldn't get client: {:?}", e),
        }

        // Again, in this simple example there are not any incoming connections,
        // so the loop is not actually run (`while false`) not to block
        // the execution.
    }

    // Get the local socket address of this socket:
    println!("Local address: {:#?}", listener.local_addr().unwrap());

    {
        // Cloning the socket creates a new handle to the same OS socket:
        let _clone = listener.try_clone().unwrap();
    }

    // Get the value of the `IP_TTL` option for this socket:
    println!("Time To Live: {}", listener.ttl().unwrap());

    // Reading from socket can be configured to return immediately.
    // Then when there are no bytes, a `WouldBlock` error is returned:
    listener.set_nonblocking(true).unwrap();
}

fn run_client_server() {
    // We will use a channel be notified when the server is ready
    // to accept requests:
    let (server_ready_notify, server_ready_recv) = unbounded();

    // Run the server in another thread:
    let thread = std::thread::spawn(move || {
        server::echo_once(server_ready_notify)
    });

    // Wait until the server is ready to accept requests:
    server_ready_recv.recv().unwrap();

    // Run the client:
    client::send_msg();

    // Wait until the thread finishes:
    thread.join().unwrap();
}

fn main() {
    tcp_listener_api_example();

    println!("Run client <-> server example:");
    run_client_server();
}
