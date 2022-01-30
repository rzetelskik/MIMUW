mod certs;
mod public_test;
mod solution;

use std::net::{TcpListener, TcpStream};

use crate::certs::{ROOT_CERT, SERVER_FULL_CHAIN, SERVER_PRIVATE_KEY};
use crate::solution::{SecureClient, SecureServer};

fn setup_process(mac_key: &[u8]) -> (SecureClient<TcpStream>, SecureServer<TcpStream>) {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    println!("Server will listening on: {}", listener.local_addr().unwrap());

    let client = SecureClient::new(
        TcpStream::connect(("127.0.0.1", listener.local_addr().unwrap().port())).unwrap(),
        mac_key,
        ROOT_CERT,
    );

    let server = SecureServer::new(
        listener.incoming().next().unwrap().unwrap(),
        mac_key,
        SERVER_PRIVATE_KEY,
        SERVER_FULL_CHAIN,
    );

    (client, server)
}

fn main() {
    let mac_key = vec![17, 18];

    let (mut target_client, mut target_server) = setup_process(&mac_key);

    let target_thread = std::thread::spawn(move || {
        let received = target_server.recv_message().unwrap();
        println!(
            "[Server] Received message: '{}'.",
            std::str::from_utf8(received.as_ref()).unwrap()
        );
    });

    target_client.send_msg(b"Hello World!".to_vec());
    assert!(matches!(target_thread.join(), Ok(_)));
}
