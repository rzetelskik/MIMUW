#[cfg(test)]
pub(crate) mod tests {
    use crate::certs::{ROOT_CERT, SERVER_FULL_CHAIN, SERVER_PRIVATE_KEY};
    use crate::solution::{SecureClient, SecureServer, SecureServerError};
    use crossbeam_channel::{unbounded, Sender};
    use ntest::timeout;
    use std::io::{Read, Write};
    use std::net::{TcpListener, TcpStream};
    use std::ops::Deref;
    use std::sync::{Arc, Barrier};

    #[test]
    #[timeout(300)]
    fn secure_link_correctly_transmits_messages() {
        // Given:
        let msgs = vec![
            "".to_string(),
            "hello".to_string(),
            "a bit longer message to transmit".to_string(),
        ];
        let hmac_key = vec![80, 81];
        let msgs_clone = msgs.clone();
        let (tx, _) = unbounded();

        run_communication(
            tx,
            Box::new(move |client| {
                // When:
                for msg in msgs_clone {
                    client.send_msg(msg.as_bytes().to_vec());
                }
            }),
            &hmac_key,
            &mut |server| {
                // Then:
                for msg in &msgs {
                    assert_eq!(msg.as_bytes(), server.recv_message().unwrap().deref())
                }
            },
            &hmac_key,
        );
    }

    #[test]
    #[timeout(300)]
    fn invalid_hmac_is_rejected() {
        // Given:
        let msg = "Server and client have different HMAC keys";
        let client_key = vec![80, 81];
        let server_key = vec![81, 82];
        let (tx, _) = unbounded();

        run_communication(
            tx,
            Box::new(move |client| {
                // When:
                client.send_msg(msg.as_bytes().to_vec());
            }),
            &client_key,
            &mut |server| {
                // Then:
                assert_eq!(server.recv_message(), Err(SecureServerError::InvalidHmac));
            },
            &server_key,
        );
    }


    // Below are test utils:

    pub(crate) struct RawDataObserver {
        stream: TcpStream,
        tx: Sender<u8>,
    }

    impl Read for RawDataObserver {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            self.stream.read(buf)
        }
    }

    impl Write for RawDataObserver {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            for byte in buf {
                let _ = self.tx.send(*byte);
            }
            self.stream.write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.stream.flush()
        }
    }

    /// Runs the client and the server making sure that neither the client nor
    /// the server will be dropped, and thus the TCP connection will be dropped,
    /// before the they both finish their execution.
    pub(crate) fn run_communication(
        tx: Sender<u8>,
        client_code: Box<dyn FnOnce(&mut SecureClient<RawDataObserver>) + Send>,
        client_hmac_key: &[u8],
        server_code: &mut dyn FnMut(&mut SecureServer<TcpStream>),
        server_hmac_key: &[u8],
    ) {
        let (mut client, mut server) = setup_tcp_based_secure_communication(
            tx,
            client_hmac_key,
            server_hmac_key
        );

        let barrier = Arc::new(Barrier::new(2));
        let barrier_c = barrier.clone();

        let client = std::thread::spawn(move || {
            client_code(&mut client);

            barrier_c.wait();
        });

        server_code(&mut server);

        barrier.wait();
        assert!(matches!(client.join(), Ok(())));
    }

    pub(crate) fn setup_tcp_based_secure_communication(
        tx: Sender<u8>,
        client_hmac_key: &[u8],
        server_hmac_key: &[u8],
    ) -> (SecureClient<RawDataObserver>, SecureServer<TcpStream>) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let client =
            TcpStream::connect(("127.0.0.1", listener.local_addr().unwrap().port())).unwrap();
        let client_data_observer = RawDataObserver { stream: client, tx };
        let server = listener.incoming().next().unwrap().unwrap();

        (
            SecureClient::new(client_data_observer, client_hmac_key, ROOT_CERT),
            SecureServer::new(server, server_hmac_key, SERVER_PRIVATE_KEY, SERVER_FULL_CHAIN),
        )
    }
}
