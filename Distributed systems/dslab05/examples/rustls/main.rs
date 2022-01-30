use std::io::{Read, Write};
use std::sync::Arc;
use std::net::{TcpListener, TcpStream};
use std::convert::TryInto;
use rustls::{ClientConnection, RootCertStore, ServerConnection, StreamOwned};

mod certs;

const SERVER_ADDRESS: &str = "127.0.0.1:8081";

// Wrap `TcpStream` of a client in TLS. Writing to/reading from the new stream
// will automatically apply TLS to the outgoing/incoming data:
fn client_stream(stream: TcpStream) -> StreamOwned<ClientConnection, TcpStream> {
    // Create an empty store for root certificates:
    let mut root_store = RootCertStore::empty();

    // Add to the store the root certificate of the server:
    root_store.add_parsable_certificates(
        &rustls_pemfile::certs(&mut certs::ROOT_CERT.as_bytes()).unwrap(),
    );

    // Create a TLS configuration for the client:
    let client_config = rustls::ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(root_store)
        .with_no_client_auth();

    // Create a TLS connection using the configuration prepared above.
    // "localhost" is the name of the remote server:
    let connection = rustls::ClientConnection::new(
        Arc::new(client_config),
        "localhost".try_into().unwrap()
    ).unwrap();

    // Wrap the TCP stream in TLS:
    rustls::StreamOwned::new(connection, stream)
}

// Wrap `TcpStream` of a server in TLS. Writing to/reading from the new stream
// will automatically apply TLS to the outgoing/incoming data:
fn server_stream(stream: TcpStream) -> StreamOwned<ServerConnection, TcpStream> {
    // Load the certificate chain for the server:
    let certs = rustls_pemfile::certs(&mut certs::SERVER_FULL_CHAIN.as_bytes())
        .unwrap()
        .iter()
        .map(|v| rustls::Certificate(v.clone()))
        .collect();

    // Load the private key for the server (for simplicity, we assume there is
    // provided one valid key and it is a RSA private key):
    let private_key = rustls::PrivateKey(
        rustls_pemfile::rsa_private_keys(&mut certs::SERVER_PRIVATE_KEY.as_bytes())
            .unwrap()
            .first()
            .unwrap()
            .to_vec(),
    );

    // Create a TLS configuration for the server:
    let server_config = rustls::ServerConfig::builder()
        .with_safe_defaults()
        .with_no_client_auth()
        .with_single_cert(certs, private_key)
        .unwrap();

    // Create a TLS connection using the configuration prepared above:
    let connection = rustls::ServerConnection::new(Arc::new(server_config)).unwrap();

    // Wrap the TCP strem in TLS:
    rustls::StreamOwned::new(connection, stream)
}

// This client sends one message to the server and waits for the acknowledgment.
// For simplicity, sizes of the messages are fixed:
fn client() {
    // Connect to the remote host:
    let client_raw_link = TcpStream::connect(SERVER_ADDRESS).unwrap();

    // Wrap the stream in TLS:
    let mut client = client_stream(client_raw_link);

    // Send the message:
    client.write_all(b"Hello encrypted world!").unwrap();

    // Receive the acknowledgment:
    let mut data = vec![0; 5];
    client.take(4).read_to_end(data.as_mut()).unwrap();
    println!(
        "[Client] Received: '{}'.",
        std::str::from_utf8(data.as_ref()).unwrap()
    );
}


// This server receives incoming data and acknowledges the receipt.
// For simplicity, it serves only the first incoming request and finishes
// itself afterwards. Moreover, sizes of the messages are fixed:
fn server() {
    // Bind to the specified address:
    let listener = TcpListener::bind(SERVER_ADDRESS).unwrap();

    // Accept the first incoming connection and create `TcpStream` for it:
    let server_raw_stream = listener.incoming().next().unwrap().unwrap();

    // Wrap the stream in TLS:
    let mut server = server_stream(server_raw_stream);

    // Receive incoming data:
    let mut data = vec![0; 22];
    server.read_exact(data.as_mut()).unwrap();
    println!(
        "[Server] Received: '{}'.",
        std::str::from_utf8(data.as_ref()).unwrap()
    );

    // Reply with the acknowledgment:
    server.write_all(b"Done").unwrap();
}

fn main() {
    let client_thread = std::thread::spawn(|| {
        client();
    });

    server();

    client_thread.join().unwrap();
}
