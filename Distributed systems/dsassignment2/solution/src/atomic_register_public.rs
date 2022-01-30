use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use log::*;
use uuid::Uuid;

use crate::{Broadcast, ClientRegisterCommand, ClientRegisterCommandContent, OperationComplete,
            OperationReturn, ReadReturn, RegisterClient, SectorsManager, SectorVec, StableStorage,
            StatusCode, SystemCommandHeader, SystemRegisterCommand, SystemRegisterCommandContent};

#[async_trait::async_trait]
pub trait AtomicRegister: Send + Sync {
    /// Send client command to the register. After it is completed, we expect
    /// callback to be called. Note that completion of client command happens after
    /// delivery of multiple system commands to the register, as the algorithm specifies.
    async fn client_command(
        &mut self,
        cmd: ClientRegisterCommand,
        operation_complete: Box<
            dyn FnOnce(OperationComplete) -> Pin<Box<dyn Future<Output=()> + Send>>
            + Send
            + Sync,
        >,
    );

    /// Send system command to the register.
    async fn system_command(&mut self, cmd: SystemRegisterCommand);
}

struct InternalAtomicRegister {
    self_ident: u8,
    metadata: Box<dyn StableStorage>,
    register_client: Arc<dyn RegisterClient>,
    sectors_manager: Arc<dyn SectorsManager>,
    processes_count: usize,
    readlist: HashMap<u8, (u64, u8, SectorVec)>,
    acklist: HashSet<u8>,
    reading: bool,
    writing: bool,
    write_phase: bool,
    writeval: SectorVec,
    readval: SectorVec,
    rid: u64,
    callback: Option<Box<dyn FnOnce(OperationComplete) -> Pin<Box<dyn Future<Output=()> + Send>> + Send + Sync>>,
}

impl InternalAtomicRegister {
    async fn new(
        self_ident: u8,
        metadata: Box<dyn StableStorage>,
        register_client: Arc<dyn RegisterClient>,
        sectors_manager: Arc<dyn SectorsManager>,
        processes_count: usize,
    ) -> Self {
        let rid = metadata.get("rid").await.map_or(0u64, |data| u64::from_ne_bytes(data.try_into().unwrap()));

        Self {
            self_ident,
            metadata,
            register_client,
            sectors_manager,
            processes_count,
            readlist: HashMap::new(),
            acklist: HashSet::new(),
            reading: false,
            writing: false,
            write_phase: false,
            writeval: SectorVec(vec![0u8; 4096]),
            readval: SectorVec(vec![0u8; 4096]),
            rid,
            callback: None,
        }
    }
}

#[async_trait::async_trait]
impl AtomicRegister for InternalAtomicRegister {
    async fn client_command(
        &mut self,
        cmd: ClientRegisterCommand,
        operation_complete: Box<
            dyn FnOnce(OperationComplete) -> Pin<Box<dyn Future<Output=()> + Send>>
            + Send
            + Sync,
        >,
    ) {
        self.callback = Some(operation_complete);

        self.rid += 1;
        self.metadata.put("rid", &self.rid.to_ne_bytes()).await.expect("can't store rid");

        self.readlist.clear();
        self.acklist.clear();

        match cmd.content {
            ClientRegisterCommandContent::Read => {
                debug!("[Process {}] Received a READ command", self.self_ident);
                self.reading = true;
            }
            ClientRegisterCommandContent::Write { data } => {
                debug!("[Process {}] Received a WRITE command", self.self_ident);
                self.writing = true;
                self.writeval = data;
            }
        }

        self.register_client.broadcast(Broadcast {
            cmd: Arc::new(SystemRegisterCommand {
                header: SystemCommandHeader {
                    process_identifier: self.self_ident,
                    msg_ident: Uuid::new_v4(),
                    read_ident: self.rid,
                    sector_idx: cmd.header.sector_idx,
                },
                content: SystemRegisterCommandContent::ReadProc,
            })
        }).await;

        debug!("[Process {}] Broadcasted a READPROC command", self.self_ident);
    }

    async fn system_command(&mut self, cmd: SystemRegisterCommand) {
        match cmd.content {
            SystemRegisterCommandContent::ReadProc => {
                debug!("[Process {}] Received a READPROC command from {}", self.self_ident, cmd.header.process_identifier);

                let (ts, wr) = self.sectors_manager.read_metadata(cmd.header.sector_idx).await;
                let val = self.sectors_manager.read_data(cmd.header.sector_idx).await;

                self.register_client.send(crate::Send {
                    cmd: Arc::new(SystemRegisterCommand {
                        header: SystemCommandHeader {
                            process_identifier: self.self_ident,
                            msg_ident: cmd.header.msg_ident,
                            read_ident: cmd.header.read_ident,
                            sector_idx: cmd.header.sector_idx,
                        },
                        content: SystemRegisterCommandContent::Value {
                            timestamp: ts,
                            write_rank: wr,
                            sector_data: val,
                        },
                    }),
                    target: cmd.header.process_identifier as usize,
                }).await;

                debug!("[Process {}] Sent a VALUE command to {}", self.self_ident, cmd.header.process_identifier);
            }
            SystemRegisterCommandContent::Value { timestamp, write_rank, sector_data } if cmd.header.read_ident == self.rid && !self.write_phase => {
                debug!("[Process {}] Received a VALUE command from {}", self.self_ident, cmd.header.process_identifier);

                self.readlist.insert(cmd.header.process_identifier, (timestamp, write_rank, sector_data));

                if 2 * self.readlist.len() > self.processes_count && (self.reading || self.writing) {
                    let (ts, wr) = self.sectors_manager.read_metadata(cmd.header.sector_idx).await;
                    let val = self.sectors_manager.read_data(cmd.header.sector_idx).await;

                    self.readlist.insert(self.self_ident, (ts, wr, val));
                    let (maxts, rr, readval) = self.readlist.iter().max_by(|a, b| a.1.1.cmp(&b.1.1)).map(|(_, v)| v).unwrap().clone();
                    self.readval = readval;
                    self.readlist.clear();
                    self.acklist.clear();
                    self.write_phase = true;

                    if self.reading {
                        self.register_client.broadcast(Broadcast {
                            cmd: Arc::new(SystemRegisterCommand {
                                header: SystemCommandHeader {
                                    process_identifier: self.self_ident,
                                    msg_ident: cmd.header.msg_ident,
                                    read_ident: cmd.header.read_ident,
                                    sector_idx: cmd.header.sector_idx,
                                },
                                content: SystemRegisterCommandContent::WriteProc {
                                    timestamp: maxts,
                                    write_rank: rr,
                                    data_to_write: self.readval.clone(),
                                },
                            })
                        }).await;
                    } else {
                        let ts = maxts + 1;
                        let wr = self.self_ident;
                        let val = self.writeval.clone();

                        self.sectors_manager.write(cmd.header.sector_idx, &(val.clone(), ts.clone(), wr.clone())).await;

                        self.register_client.broadcast(Broadcast {
                            cmd: Arc::new(SystemRegisterCommand {
                                header: SystemCommandHeader {
                                    process_identifier: self.self_ident,
                                    msg_ident: cmd.header.msg_ident,
                                    read_ident: cmd.header.read_ident,
                                    sector_idx: cmd.header.sector_idx,
                                },
                                content: SystemRegisterCommandContent::WriteProc {
                                    timestamp: ts,
                                    write_rank: wr,
                                    data_to_write: val,
                                },
                            })
                        }).await;
                    }
                }
            }
            SystemRegisterCommandContent::WriteProc { timestamp, write_rank, data_to_write } => {
                debug!("[Process {}] Received a WRITEPROC command from process {}", self.self_ident, cmd.header.process_identifier);
                let (ts, wr) = self.sectors_manager.read_metadata(cmd.header.sector_idx).await;

                if (timestamp, write_rank) > (ts, wr) {
                    self.sectors_manager.write(cmd.header.sector_idx, &(data_to_write, timestamp, write_rank)).await;
                }

                self.register_client.send(crate::Send {
                    cmd: Arc::new(SystemRegisterCommand {
                        header: SystemCommandHeader {
                            process_identifier: self.self_ident,
                            msg_ident: cmd.header.msg_ident,
                            read_ident: cmd.header.read_ident,
                            sector_idx: cmd.header.sector_idx,
                        },
                        content: SystemRegisterCommandContent::Ack,
                    }),
                    target: cmd.header.process_identifier as usize,
                }).await;
            }
            SystemRegisterCommandContent::Ack if cmd.header.read_ident == self.rid && self.write_phase => {
                debug!("[Process {}] Received an ACK command from process {}", self.self_ident, cmd.header.process_identifier);

                self.acklist.insert(cmd.header.process_identifier);
                if 2 * self.acklist.len() > self.processes_count && (self.reading || self.writing) {
                    self.acklist.clear();
                    self.write_phase = false;

                    if self.reading {
                        self.reading = false;

                        let op = OperationComplete {
                            status_code: StatusCode::Ok,
                            request_identifier: 0,
                            op_return: OperationReturn::Read(ReadReturn {
                                read_data: Some(self.readval.clone())
                            }),
                        };
                        self.callback.take().unwrap()(op).await;
                    } else {
                        self.writing = false;

                        let op = OperationComplete {
                            status_code: StatusCode::Ok,
                            request_identifier: 0,
                            op_return: OperationReturn::Write,
                        };

                        if self.callback.is_some() {
                            self.callback.take().unwrap()(op).await;
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Idents are numbered starting at 1 (up to the number of processes in the system).
/// Storage for atomic register algorithm data is separated into StableStorage.
/// Communication with other processes of the system is to be done by register_client.
/// And sectors must be stored in the sectors_manager instance.
pub async fn build_atomic_register(
    self_ident: u8,
    metadata: Box<dyn StableStorage>,
    register_client: Arc<dyn RegisterClient>,
    sectors_manager: Arc<dyn SectorsManager>,
    processes_count: usize,
) -> Box<dyn AtomicRegister> {
    Box::new(InternalAtomicRegister::new(
        self_ident,
        metadata,
        register_client,
        sectors_manager,
        processes_count,
    ).await)
}
