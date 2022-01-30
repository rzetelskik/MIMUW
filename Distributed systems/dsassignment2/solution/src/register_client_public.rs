use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

use async_channel::{Receiver, Sender, unbounded};
use log::error;
use tokio::net::TcpStream;
use tokio::sync::mpsc;

use crate::{RegisterCommand, SectorIdx, serialize_register_command, SystemRegisterCommand};

#[async_trait::async_trait]
/// We do not need any public implementation of this trait. It is there for use
/// in AtomicRegister. In our opinion it is a safe bet to say some structure of
/// this kind must appear in your solution.
pub trait RegisterClient: core::marker::Send + core::marker::Sync {
    /// Sends a system message to a single process.
    async fn send(&self, msg: Send);

    /// Broadcasts a system message to all processes in the system, including self.
    async fn broadcast(&self, msg: Broadcast);
}

pub struct Broadcast {
    pub cmd: Arc<SystemRegisterCommand>,
}

pub struct Send {
    pub cmd: Arc<SystemRegisterCommand>,
    /// Identifier of the target process. Those start at 1.
    pub target: usize,
}

struct InternalRegisterClient {
    self_ident: u8,
    self_tx: Sender<Box<SystemRegisterCommand>>,
    process_count: usize,
    txs: Vec<Sender<Box<SystemRegisterCommand>>>,
    rebroadcast_tx: mpsc::UnboundedSender<Box<SystemRegisterCommand>>,
}

impl InternalRegisterClient {
    fn new(self_ident: u8, self_tx: Sender<Box<SystemRegisterCommand>>, tcp_locations: Vec<(String, u16)>, internal_ack_rx: mpsc::UnboundedReceiver<SectorIdx>, hmac_system_key: [u8; 64]) -> Self {
        let (txs, rxs): (Vec<Sender<Box<SystemRegisterCommand>>>, Vec<Receiver<Box<SystemRegisterCommand>>>) = (0..tcp_locations.len()).map(|_| unbounded()).unzip();

        let (rebroadcast_tx, rebroadcast_rx) = mpsc::unbounded_channel::<Box<SystemRegisterCommand>>();

        tokio::spawn(Self::start_rebroadcast_controller(rebroadcast_rx, internal_ack_rx, txs.clone()));

        for (i, (target, rx)) in tcp_locations.iter().cloned().zip(rxs.into_iter()).enumerate() {
            if i == ((self_ident - 1) as usize) {
                continue;
            }

            tokio::spawn(Self::start_connection_loop(target, rx, hmac_system_key));
        }

        Self {
            self_ident,
            self_tx,
            process_count: tcp_locations.len(),
            txs,
            rebroadcast_tx,
        }
    }

    async fn start_connection_loop(target: (String, u16), rx: Receiver<Box<SystemRegisterCommand>>, hmac_system_key: [u8; 64]) {
        loop {
            tokio::time::sleep(tokio::time::Duration::from_millis(300)).await;

            match tokio::time::timeout(tokio::time::Duration::from_millis(1000), TcpStream::connect(target.clone())).await {
                Ok(res) => match res {
                    Ok(tcp_stream) => {
                        Self::handle_writes(rx.clone(), tcp_stream, hmac_system_key).await;
                    }
                    Err(err) => error!("Can't connect to target {}:{}: {:?}", target.0, target.1, err)
                }
                Err(err) => error!("Timed out trying to connect to target {}:{}: {:?}", target.0, target.1, err)
            }
        }
    }

    async fn handle_writes(rx: Receiver<Box<SystemRegisterCommand>>, mut stream: TcpStream, hmac_system_key: [u8; 64]) {
        while let Ok(cmd) = rx.recv().await {
            let command = RegisterCommand::System(cmd.deref().clone());

            if let Err(err) = serialize_register_command(&command, &mut stream, &hmac_system_key).await {
                error!("Can't send command to stream: {:?}", err);
                break
            }
        }
    }

    async fn start_rebroadcast_controller(mut rebroadcast_rx: mpsc::UnboundedReceiver<Box<SystemRegisterCommand>>, mut internal_ack_rx: mpsc::UnboundedReceiver<SectorIdx>, txs: Vec<Sender<Box<SystemRegisterCommand>>>) {
        let mut interval = tokio::time::interval(tokio::time::Duration::from_millis(5000));
        interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

        let mut sent = HashMap::<SectorIdx, Box<SystemRegisterCommand>>::new();

        loop {
            tokio::select! {
                Some(cmd) = rebroadcast_rx.recv() => {
                    sent.insert(cmd.header.sector_idx, cmd);
                },
                Some(sector_idx) = internal_ack_rx.recv() => {
                    sent.remove(&sector_idx);
                },
                _ = interval.tick() => {
                    for (_, cmd) in sent.iter() {
                        for tx in txs.iter() {
                            let _ = tx.send(cmd.clone()).await;
                        }
                    }
                },
            }
        }
    }
}

#[async_trait::async_trait]
impl RegisterClient for InternalRegisterClient {
    async fn send(&self, msg: Send) {
        let tx = if msg.target == (self.self_ident as usize) {
            &self.self_tx
        } else {
            self.txs.get(msg.target - 1).unwrap()
        };

        let _ = tx.send(Box::new(msg.cmd.deref().clone())).await;
    }

    async fn broadcast(&self, msg: Broadcast) {
        for i in 1..=self.process_count {
            self.send(Send { cmd: msg.cmd.clone(), target: i }).await;
        }

        let _ = self.rebroadcast_tx.send(Box::new(msg.cmd.deref().clone()));
    }
}

pub(super) fn build_register_client(
    self_ident: u8,
    self_tx: Sender<Box<SystemRegisterCommand>>,
    tcp_locations: Vec<(String, u16)>,
    internal_ack_rx: mpsc::UnboundedReceiver<SectorIdx>,
    hmac_system_key: [u8; 64]) -> Arc<dyn RegisterClient> {
    Arc::new(InternalRegisterClient::new(self_ident, self_tx, tcp_locations, internal_ack_rx, hmac_system_key))
}
