use std::io::{Read, Write};
use std::net::{Shutdown, TcpStream};

// This client sends one message to the echo server:
pub fn send_msg() {
    // Connect to a remote host (if you need to specify connecting
    // timeout, use `connect_timeout()` instead):
    let mut stream = TcpStream::connect("127.0.0.1:8889").unwrap();

    // Send the message:
    let msg = "This is test";
    println!("[Client] Sending: '{}'.", msg);
    stream.write_all(msg.as_bytes()).unwrap();

    // Shutdown the write half of this connection (any currently blocked
    // or future write attempt will return an error):
    stream.shutdown(Shutdown::Write).unwrap();

    // Receive the response. The `read_to_end()` method reads all bytes until
    // `EOF` and appends them to the buffer, extenting the vector if needed:
    let mut buf = Vec::new();
    stream.read_to_end(&mut buf).unwrap();
    println!(
        "[Client] Received: '{}'.",
        std::string::String::from_utf8(buf).unwrap()
    );
}
