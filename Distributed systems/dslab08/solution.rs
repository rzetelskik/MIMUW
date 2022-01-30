use std::collections::{HashMap, HashSet};
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use bytes::BytesMut;
use executor::{Handler, ModuleRef, System, Tick};
use log::debug;
use serde::{Deserialize, Serialize};
use tokio::net::UdpSocket;
use uuid::Uuid;

/// A message which disables a process. Used for testing
pub struct Disable;

pub struct FailureDetectorModule {
    enabled: bool,
    socket: Arc<UdpSocket>,
    ident: Uuid,
    all_idents: HashSet<Uuid>,
    addresses: HashMap<Uuid, SocketAddr>,
    alive: HashSet<Uuid>,
    suspected: HashSet<Uuid>,
}

impl FailureDetectorModule {
    pub async fn new(
        system: &mut System,
        delay: Duration,
        addresses: &HashMap<Uuid, SocketAddr>,
        ident: Uuid,
        all_idents: HashSet<Uuid>,
    ) -> ModuleRef<Self> {
        let addr = addresses.get(&ident).unwrap();
        let socket = Arc::new(UdpSocket::bind(addr).await.unwrap());

        let module_ref = system
            .register_module(Self {
                enabled: true,
                socket: socket.clone(),
                ident,
                all_idents: all_idents.clone(),
                addresses: addresses.clone(),
                alive: all_idents.clone(),
                suspected: HashSet::new(),
            })
            .await;

        tokio::spawn(deserialize_and_forward(socket, module_ref.clone()));

        system.request_tick(&module_ref, delay).await;

        module_ref
    }
}

/// New operation arrived on socket.
#[async_trait::async_trait]
impl Handler<DetectorOperationUdp> for FailureDetectorModule {
    async fn handle(&mut self, item: DetectorOperationUdp) {
        if self.enabled {
            let target = item.1;

            match item.0 {
                DetectorOperation::HeartbeatRequest => {
                    let _res = self.socket.send_to(bincode::serialize(&DetectorOperation::HeartbeatResponse(self.ident)).unwrap().as_slice(), target).await;
                }
                DetectorOperation::HeartbeatResponse(uuid) => {
                    self.alive.insert(uuid);
                }
                DetectorOperation::AliveRequest => {
                    let alive = (&self.all_idents - &self.suspected).iter().cloned().collect();
                    let _res = self.socket.send_to(bincode::serialize(&DetectorOperation::AliveInfo(alive)).unwrap().as_slice(), target).await;
                }
                _ => {}
            }
        }
    }
}

/// Called periodically to check send broadcast and update alive processes.
#[async_trait::async_trait]
impl Handler<Tick> for FailureDetectorModule {
    async fn handle(&mut self, _msg: Tick) {
        if self.enabled {
            for (p, paddr) in self.addresses.iter() {
                if !(self.alive.contains(p) || self.suspected.contains(p)) {
                    self.suspected.insert(p.clone());
                } else if self.alive.contains(p) && self.suspected.contains(p) {
                    self.suspected.remove(p);
                }

                let _res = self.socket.send_to(bincode::serialize(&DetectorOperation::HeartbeatRequest).unwrap().as_slice(), paddr).await;
            }

            self.alive.clear();
        }
    }
}

#[async_trait::async_trait]
impl Handler<Disable> for FailureDetectorModule {
    async fn handle(&mut self, _msg: Disable) {
        self.enabled = false;
    }
}

async fn deserialize_and_forward(
    socket: Arc<UdpSocket>,
    module_ref: ModuleRef<FailureDetectorModule>,
) {
    let mut buffer = BytesMut::new();
    while socket.readable().await.is_ok() {
        if let Ok((_, sender)) = socket.try_recv_buf_from(&mut buffer) {
            match bincode::deserialize(&buffer.split()) {
                Ok(msg) => module_ref.send(DetectorOperationUdp(msg, sender)).await,
                Err(err) => {
                    debug!("Invalid format of detector operation ({})!", err);
                }
            }
        }
    }
}

struct DetectorOperationUdp(DetectorOperation, SocketAddr);

#[derive(Serialize, Deserialize)]
pub enum DetectorOperation {
    /// Request to receive a heartbeat.
    HeartbeatRequest,
    /// Response to heartbeat, contains uuid of the receiver of HeartbeatRequest.
    HeartbeatResponse(Uuid),
    /// Request to receive information about working processes.
    AliveRequest,
    /// Vector of processes which are alive according to AliveRequest receiver.
    AliveInfo(Vec<Uuid>),
}
