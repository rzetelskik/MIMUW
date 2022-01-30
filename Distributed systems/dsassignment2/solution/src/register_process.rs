use std::future::Future;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;

use async_channel::Receiver;
use hmac::{Mac, NewMac};
use log::{debug, error};
use tokio::io::{AsyncWrite, AsyncWriteExt, Error};
use tokio::net::TcpStream;
use tokio::sync::{mpsc, Mutex, OwnedSemaphorePermit, Semaphore};
use tokio::sync::mpsc::UnboundedSender;

use crate::atomic_register_public::*;
use crate::common::*;
use crate::domain::*;
use crate::register_client_public::*;
use crate::sectors_manager_public::*;
use crate::stable_storage_public::*;
use crate::transfer_public::*;

#[derive(Debug, Clone)]
pub(crate) struct RegisterProcessConfig {
    pub self_ident: u8,
    pub processes_count: usize,
    pub hmac_system_key: [u8; 64],
    pub hmac_client_key: [u8; 32],
    pub max_sector: u64,
    pub listen_addr: (String, u16),
    pub data_path: PathBuf,
    pub metadata_path: PathBuf,
}

impl RegisterProcessConfig {
    pub(crate) fn new(config: Configuration) -> Self {
        Self {
            self_ident: config.public.self_rank,
            processes_count: config.public.tcp_locations.len(),
            hmac_system_key: config.hmac_system_key,
            hmac_client_key: config.hmac_client_key,
            max_sector: config.public.max_sector,
            listen_addr: config
                .public
                .tcp_locations
                .get((config.public.self_rank - 1) as usize)
                .expect("Can't get a TCP location").clone(),
            data_path: config.public.storage_dir.as_path().join("data"),
            metadata_path: config.public.storage_dir.as_path().join("metadata"),
        }
    }
}

type Callback = Box<
    dyn FnOnce(OperationComplete) -> Pin<Box<dyn Future<Output=()> + core::marker::Send>>
    + core::marker::Send
    + Sync,
>;

#[derive(Debug, Clone)]
pub(crate) struct WorkerRef {
    pub semaphore: Arc<Semaphore>,
    pub tx: UnboundedSender<Box<WorkerCommand>>,
}

pub(crate) enum WorkerCommand {
    System(SystemRegisterCommand),
    Client(ClientRegisterCommand, Callback),
}

#[repr(u8)]
enum OperationCompleteType {
    Read = 0x41u8,
    Write = 0x42u8,
}

pub(crate) async fn run_worker(config: Arc<RegisterProcessConfig>, worker_ident: usize, mut rx: mpsc::UnboundedReceiver<Box<WorkerCommand>>,
                               register_client: Arc<dyn RegisterClient>, sectors_manager: Arc<dyn SectorsManager>) {
    let local_path = config.metadata_path.as_path().join(format!("{:X}", worker_ident));
    let _ = tokio::fs::create_dir_all(local_path.as_path()).await;
    let metadata = build_stable_storage(local_path);
    let mut atomic_register = build_atomic_register(config.self_ident, metadata, register_client, sectors_manager, config.processes_count).await;

    while let Some(cmd) = rx.recv().await {
        match *cmd {
            WorkerCommand::Client(cmd, callback) => {
                debug!("[Process {}] Worker {} received a client command", config.self_ident, worker_ident);
                atomic_register.client_command(cmd, callback).await;
            }
            WorkerCommand::System(cmd) => {
                debug!("[Process {}] Worker {} received a system command", config.self_ident, worker_ident);
                atomic_register.system_command(cmd).await;
            }
        }
    }
}

pub(crate) async fn run_self_fanout(feedback_rx: Receiver<Box<SystemRegisterCommand>>, worker_txs: Vec<UnboundedSender<Box<WorkerCommand>>>) {
    while let Ok(cmd) = feedback_rx.recv().await {
        let tx = worker_txs.get((cmd.header.sector_idx % WORKERS_NUM) as usize).unwrap().clone();
        let _ = tx.send(Box::new(WorkerCommand::System(*cmd)));
    }
}

pub(crate) async fn handle_connection(config: Arc<RegisterProcessConfig>, stream: TcpStream, sockaddr: SocketAddr, internal_ack_tx: UnboundedSender<SectorIdx>, worker_refs: Vec<WorkerRef>) {
    debug!("[Process {}] Accepted new connection from {}", config.self_ident, sockaddr.to_string());
    let (mut read_stream, write_stream) = stream.into_split();
    let write_stream = Arc::new(Mutex::new(write_stream));

    loop {
        match deserialize_register_command(&mut read_stream, &config.hmac_system_key, &config.hmac_client_key.clone()).await {
            Ok((cmd, hmac_valid)) => {
                handle_command(config.clone(), cmd, hmac_valid, write_stream.clone(), sockaddr, internal_ack_tx.clone(), worker_refs.clone()).await;
            }
            Err(_) => {
                error!("[Process {}] Broken connection {}", config.self_ident, sockaddr.to_string());
                break;
            }
        }
    }
}

pub(crate) async fn handle_command(config: Arc<RegisterProcessConfig>, cmd: RegisterCommand, hmac_valid: bool, stream: Arc<Mutex<dyn AsyncWrite + core::marker::Send + Unpin>>, sockaddr: SocketAddr, internal_ack_tx: UnboundedSender<SectorIdx>, worker_refs: Vec<WorkerRef>) {
    if hmac_valid {
        debug!("[Process {}] Received a valid message from {}", config.self_ident, sockaddr.to_string());

        match cmd.clone() {
            RegisterCommand::Client(cmd) => {
                if cmd.header.sector_idx < config.max_sector {
                    let worker = &worker_refs[(cmd.header.sector_idx % WORKERS_NUM) as usize];
                    let permit = worker.semaphore.clone().acquire_owned().await.unwrap();

                    let callback: Callback = get_callback(config, cmd.clone(), stream.clone(), sockaddr.clone(), internal_ack_tx.clone(), permit);

                    let _ = worker.tx.send(Box::new(WorkerCommand::Client(cmd, callback)));
                } else {
                    let op = OperationComplete {
                        status_code: StatusCode::InvalidSectorIndex,
                        request_identifier: cmd.header.request_identifier,
                        op_return: match cmd.content {
                            ClientRegisterCommandContent::Read => OperationReturn::Read(ReadReturn { read_data: None }),
                            ClientRegisterCommandContent::Write { .. } => OperationReturn::Write
                        },
                    };

                    if let Err(err) = serialize_operation_complete(&op, stream.clone(), &config.hmac_client_key).await {
                        error!("[Process {}] Can't send operation complete {}: {:?}", config.self_ident, sockaddr.to_string(), err);
                    }
                }
            }
            RegisterCommand::System(cmd) => {
                let worker = &worker_refs[(cmd.header.sector_idx % WORKERS_NUM) as usize];
                let _ = worker.tx.send(Box::new(WorkerCommand::System(cmd)));
            }
        }
    } else {
        debug!("[Process {}] Received an invalid message from {}", config.self_ident, sockaddr.to_string());

        match cmd {
            RegisterCommand::Client(ClientRegisterCommand { header, content }) => {
                let op = OperationComplete {
                    status_code: StatusCode::AuthFailure,
                    request_identifier: header.request_identifier,
                    op_return: match content {
                        ClientRegisterCommandContent::Read => {
                            OperationReturn::Read(ReadReturn { read_data: None })
                        }
                        ClientRegisterCommandContent::Write { .. } => {
                            OperationReturn::Write
                        }
                    },
                };

                if let Err(err) =  serialize_operation_complete(&op, stream.clone(), &config.hmac_client_key).await {
                    error!("[Process {}] Can't send operation complete to {}: {:?}", config.self_ident, sockaddr.to_string(), err);
                }
            }
            _ => {
                debug!("[Process {}] Discarding an invalid command from {}", config.self_ident, sockaddr.to_string());
            }
        }
    }
}

fn get_callback(config: Arc<RegisterProcessConfig>, cmd: ClientRegisterCommand, stream: Arc<Mutex<dyn AsyncWrite + core::marker::Send + Unpin>>, sockaddr: SocketAddr, internal_ack_tx: UnboundedSender<SectorIdx>, permit: OwnedSemaphorePermit) -> Callback {
    Box::new(move |mut op_complete| Box::pin(async move {
        op_complete.request_identifier = cmd.header.request_identifier;

        internal_ack_tx.send(cmd.header.sector_idx).unwrap();
        if let Err(err) = serialize_operation_complete(&op_complete, stream, &config.hmac_client_key).await {
            error!("[Process {}] Can't send operation complete to {}: {:?}", config.self_ident, sockaddr.to_string(), err);
        }
        drop(permit);

        debug!("[Process {}] Completed a {} operation on sector {}", config.self_ident, match cmd.content {
                                                ClientRegisterCommandContent::Read => "READ",
                                                ClientRegisterCommandContent::Write { .. } => "WRITE"
                                            }, cmd.header.sector_idx);
    }))
}

async fn serialize_operation_complete(operation_complete: &OperationComplete, writer: Arc<Mutex<dyn AsyncWrite + core::marker::Send + Unpin>>, hmac_client_key: &[u8]) -> Result<(), Error> {
    let mut buffer = Vec::<u8>::new();
    let (msg_type, content) = unpack_operation_return(&operation_complete.op_return);

    buffer.write(&MAGIC_NUMBER).await?;
    buffer.write(&mut ([0u8; 2] as [u8; 2])).await?;
    buffer.write(&(operation_complete.status_code as u8).to_be_bytes()).await?;
    buffer.write(&(msg_type as u8).to_be_bytes()).await?;
    buffer.write(&operation_complete.request_identifier.to_be_bytes()).await?;

    if let Some(Some(SectorVec(data))) = content {
        buffer.write(data).await?;
    }

    let mut mac = HmacSha256::new_from_slice(hmac_client_key).unwrap();
    mac.update(&buffer);
    let tag: [u8; 32] = mac.finalize().into_bytes().into();

    buffer.write(&tag).await?;

    writer.lock().await.write_all(&buffer).await
}

fn unpack_operation_return(operation_return: &OperationReturn) -> (OperationCompleteType, Option<&Option<SectorVec>>) {
    match operation_return {
        OperationReturn::Read(ReadReturn { read_data }) => (OperationCompleteType::Read, Some(read_data)),
        OperationReturn::Write => (OperationCompleteType::Write, None)
    }
}