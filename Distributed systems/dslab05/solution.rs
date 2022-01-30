use std::io::{Read, Write};

// You can add here other imports from std or crates listed in Cargo.toml.
use rustls::{ClientConnection, RootCertStore, ServerConnection, StreamOwned};
use std::sync::Arc;
use std::convert::TryInto;
use hmac::{Hmac, Mac, NewMac};
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;

pub struct SecureClient<L: Read + Write> {
    // Add here any fields you need.
    client: StreamOwned<ClientConnection, L>,
    hmac_key: Vec<u8>,
}

pub struct SecureServer<L: Read + Write> {
    // Add here any fields you need.
    server: StreamOwned<ServerConnection, L>,
    hmac_key: Vec<u8>,
}

impl<L: Read + Write> SecureClient<L> {
    /// Creates a new instance of SecureClient.
    ///
    /// SecureClient communicates with SecureServer via `link`.
    /// The messages include a HMAC tag calculated using `hmac_key`.
    /// A certificate of SecureServer is signed by `root_cert`.
    pub fn new(link: L, hmac_key: &[u8], root_cert: &str) -> Self {
        let mut root_store = RootCertStore::empty();
        
        root_store.add_parsable_certificates(
            &rustls_pemfile::certs(&mut root_cert.as_bytes()).unwrap(),
        );

        let client_config = rustls::ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(root_store)
        .with_no_client_auth();

        let connection = rustls::ClientConnection::new(
            Arc::new(client_config),
            "localhost".try_into().unwrap()
        ).unwrap();

        let client = rustls::StreamOwned::new(connection, link);

        SecureClient{
            client,
            hmac_key: hmac_key.to_vec(),
        }
    }

    /// Sends the data to the server. The sent message follows the
    /// format specified in the description of the assignment.
    pub fn send_msg(&mut self, data: Vec<u8>) {
        let size: u32 = data.len().try_into().unwrap();
        let mut mac = HmacSha256::new_from_slice(&self.hmac_key).unwrap();
        mac.update(&data);
        let tag: [u8; 32] = mac.finalize().into_bytes().into();


        self.client.write(&size.to_be_bytes()).unwrap();
        self.client.write(&data).unwrap();
        self.client.write(&tag).unwrap();
    }
}

impl<L: Read + Write> SecureServer<L> {
    /// Creates a new instance of SecureServer.
    ///
    /// SecureServer receives messages from SecureClients via `link`.
    /// HMAC tags of the messages are verified against `hmac_key`.
    /// The private key of the SecureServer's certificate is `server_private_key`,
    /// and the full certificate chain is `server_full_chain`.
    pub fn new(
        link: L,
        hmac_key: &[u8],
        server_private_key: &str,
        server_full_chain: &str,
    ) -> Self {
        let certs = rustls_pemfile::certs(&mut server_full_chain.as_bytes())
            .unwrap()
            .iter()
            .map(|v| rustls::Certificate(v.clone()))
            .collect();

        let private_key = rustls::PrivateKey(
            rustls_pemfile::rsa_private_keys(&mut server_private_key.as_bytes())
                .unwrap()
                .first()
                .unwrap()
                .to_vec(),
        );

        let server_config = rustls::ServerConfig::builder()
            .with_safe_defaults()
            .with_no_client_auth()
            .with_single_cert(certs, private_key)
            .unwrap();

        let connection = rustls::ServerConnection::new(Arc::new(server_config)).unwrap();
    
        let server = rustls::StreamOwned::new(connection, link);
        SecureServer{
            server,
            hmac_key: hmac_key.to_vec(),
        }
    }

    /// Receives the next incoming message and returns the message's content
    /// (i.e., without the message size and without the HMAC tag) if the
    /// message's HMAC tag is correct. Otherwise returns `SecureServerError`.
    pub fn recv_message(&mut self) -> Result<Vec<u8>, SecureServerError> {
        let mut sizebytes: [u8; 4] = [0; 4];

        self.server.read_exact(sizebytes.as_mut()).unwrap();
        let size: u32 = u32::from_be_bytes(sizebytes);

        let mut data: Vec<u8> = vec![0; size.try_into().unwrap()];
        self.server.read_exact(data.as_mut()).unwrap();
        
        let mut tag: [u8; 32] = [0; 32];
        self.server.read_exact(tag.as_mut()).unwrap();

        let mut mac = HmacSha256::new_from_slice(&self.hmac_key).unwrap();
        mac.update(&data);
        if !mac.verify(&tag).is_ok() {
            return Err(SecureServerError::InvalidHmac);
        }
       
        return Ok(data);
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum SecureServerError {
    /// The HMAC tag of a message is invalid.
    InvalidHmac,
}

// You can add any private types, structs, consts, functions, methods, etc., you need.
