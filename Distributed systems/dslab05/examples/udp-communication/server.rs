use crossbeam_channel::Sender;
use std::net::UdpSocket;

// This server echoes incoming requests.
// For simplicity, it finishes itself after serving two requests:
pub fn echo_server(server_ready_notify: Sender<()>) {
    // Bind to 127.0.0.1:8889:
    let socket = UdpSocket::bind("127.0.0.1:8889").unwrap();

    // Notify the example that it is ready to serve requests:
    server_ready_notify.send(()).unwrap();

    let mut buf = [0; 20];

    for _ in 0..2 {
        // Receive incoming data (it blocks until some data is available):
        let (count, addr) = socket.recv_from(&mut buf).unwrap();
        println!(
            "[Server] Received {} bytes. Message: '{}'.",
            count,
            std::str::from_utf8(&buf[..count]).unwrap()
        );
        // Echo the message:
        socket.send_to(&buf[..count], addr).unwrap();
    }
}
