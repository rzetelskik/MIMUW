mod client;
mod server;

use crossbeam_channel::unbounded;
use std::net::{SocketAddr, UdpSocket};

fn bind_api_example() {
    {
        // Binding to IP 0.0.0.0 results in listening on all interfaces:
        let addr1 = SocketAddr::from(([0, 0, 0, 0], 8877));
        // `bind()` requires the argument to implement the `ToSocketAddrs` trait:
        let _socket1 = UdpSocket::bind(addr1).unwrap();

        // Binding to port 0 results in binding to a random port:
        let addr2 = SocketAddr::from(([127, 0, 0, 1], 0));

        // `ToSocketAddrs` may return multiple addresses. In such a case
        // `bind()` will try them one by one untill one succeeds:
        // to multiple soc
        let addrs = [addr1, addr2];
        let _socket2 = UdpSocket::bind(&addrs[..]).unwrap();

    } // The sockets are closed here.
}

fn socket_api_example() {
    let any_local_port = SocketAddr::from(([127, 0, 0, 1], 0));
    let socket = UdpSocket::bind(any_local_port).unwrap();

    println!("Socket address: {}", socket.local_addr().unwrap());

    {
        // Cloning the socket creates a new handle to the same OS socket
        // (it reads from and writes to the same socket):
        let _cloned_socket = socket.try_clone().unwrap();
    }

    // Get values of options (here `SO_BROADCAST` and `IP_TTL`) for this socket:
    println!(
        "Is broadcast: {}, Time To Live: {}",
        socket.broadcast().unwrap(),
        socket.ttl().unwrap()
    );

    // Set the value of the `SO_BROADCAST` option:
    socket.set_broadcast(true).unwrap();

    // Reading from socket can be configured to return immediately.
    // Then when there are no bytes, a `WouldBlock` error is returned:
    socket.set_nonblocking(true).unwrap();
}

fn run_client_server() {
    // We will use a channel be notified when the server is ready
    // to accept requests:
    let (server_ready_notify, server_ready_recv) = unbounded();

    // Run the server in another thread:
    let thread = std::thread::spawn(move || {
        server::echo_server(server_ready_notify)
    });

    // Wait until the server is ready to accept requests:
    server_ready_recv.recv().unwrap();

    // Run the client:
    client::client();

    // Wait until the thread finishes:
    thread.join().unwrap();
}

fn main() {
    bind_api_example();
    socket_api_example();

    println!("Run client <-> server example:");
    run_client_server();
}
