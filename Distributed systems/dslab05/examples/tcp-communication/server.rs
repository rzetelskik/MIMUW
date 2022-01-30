use crossbeam_channel::Sender;
use std::io::{Read, Write};
use std::net::TcpListener;

// This server echoes incoming requests.
// For simplicity, it finishes itself after serving one request:
pub fn echo_once(server_up_notify: Sender<()>) {
    // Bind to 127.0.0.1:8889:
    let socket = TcpListener::bind("127.0.0.1:8889").unwrap();

    // Notify the example that it is ready to serve requests:
    server_up_notify.send(()).unwrap();

    // Get the first element from the iterator over incoming connections:
    for stream in socket.incoming().take(1) {
        println!("new connection!");
        let mut buf = Vec::new();
        let mut stream = stream.unwrap();

        // Receive all data:
        stream.read_to_end(&mut buf).unwrap();

        println!(
            "[Server] Received: '{}'.",
            std::str::from_utf8(&buf).unwrap()
        );

        // Echo the message:
        stream.write_all(&buf).unwrap();
    }
}
