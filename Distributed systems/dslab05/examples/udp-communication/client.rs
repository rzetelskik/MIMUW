use std::net::UdpSocket;
use std::time::Duration;

// This client sends two messages to the echo server:
pub fn client() {
    // Bind to a socket:
    let socket = UdpSocket::bind("0.0.0.0:0").unwrap();


    // Send the first message directly specifying the address of the recipient:
    let count = socket
        .send_to(b"testing UTF-8 string", "127.0.0.1:8889")
        .unwrap();
    println!("[Client] Sent {} bytes.", count);

    // Receive the response:
    let mut buf = [0; 32];
    let count = socket.recv(&mut buf).unwrap();
    println!(
        "[Client] Received: '{}'.",
        std::str::from_utf8(&buf[..count]).unwrap()
    );


    // Connect the socket to the specified remote address:
    socket.connect("127.0.0.1:8889").unwrap();

    // Now send the message. Since the socket is connected to the remote address,
    // the recipient does not have to be specified in the `send()` method:
    let count = socket.send(b"using connect").unwrap();
    println!("[Client] Sent {} bytes.", count);

    // Set the read timeout:
    socket
        .set_read_timeout(Some(Duration::from_secs(1)))
        .unwrap();

    // Receive the response (data coming from other remote addresses than
    // the socket is connected to is automatically filtered out):
    let count = socket.recv(&mut buf).unwrap();
    println!(
        "[Client] Received: '{}'.",
        std::str::from_utf8(&buf[..count]).unwrap()
    );
}
