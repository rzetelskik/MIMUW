use std::sync::Arc;

use async_channel::unbounded;
use log::info;
use tokio::net::TcpListener;
use tokio::sync::{mpsc, Semaphore};
use tokio::task::JoinHandle;

pub use atomic_register_public::*;
use common::*;
pub use register_client_public::*;
use register_process::*;
pub use sectors_manager_public::*;
pub use stable_storage_public::*;
pub use transfer_public::*;

pub use crate::domain::*;

mod domain;
mod sectors_manager_public;
mod atomic_register_public;
mod transfer_public;
mod register_client_public;
mod stable_storage_public;
mod common;
mod register_process;


pub async fn run_register_process(config: Configuration) {
    let tcp_locations = config.public.tcp_locations.clone();
    let rpc = Arc::new(RegisterProcessConfig::new(config));
    let _ = tokio::fs::create_dir(rpc.data_path.as_path()).await;
    let _ = tokio::fs::create_dir(rpc.metadata_path.as_path()).await;

    let listener = TcpListener::bind(rpc.listen_addr.clone())
        .await
        .expect("Can't open TCP socket");

    info!("[Process {}] Listening on {}", rpc.self_ident, listener.local_addr().unwrap());

    let (feedback_tx, feedback_rx) = unbounded::<Box<SystemRegisterCommand>>();
    let (internal_ack_tx, internal_ack_rx) = mpsc::unbounded_channel::<SectorIdx>();

    let register_client = build_register_client(rpc.self_ident, feedback_tx, tcp_locations.clone(), internal_ack_rx, rpc.hmac_system_key.clone());
    let storage_dir = rpc.data_path.clone();
    let sectors_manager = tokio::task::spawn_blocking(move || build_sectors_manager(storage_dir)).await.unwrap();

    let (worker_txs, worker_rxs): (Vec<mpsc::UnboundedSender<Box<WorkerCommand>>>, Vec<mpsc::UnboundedReceiver<Box<WorkerCommand>>>) = (0..WORKERS_NUM).map(|_| mpsc::unbounded_channel()).unzip();
    let worker_refs: Vec<WorkerRef> = worker_txs
        .iter()
        .cloned()
        .map(|tx| WorkerRef {
            semaphore: Arc::new(Semaphore::new(1)),
            tx,
        })
        .collect();
    let worker_handles: Vec<JoinHandle<()>> = worker_rxs
        .into_iter()
        .enumerate()
        .map(|(worker_ident, rx)| tokio::spawn(
            run_worker(
                rpc.clone(),
                worker_ident,
                rx,
                register_client.clone(),
                sectors_manager.clone(),
            )
        ))
        .collect();

    let fanout_handle = tokio::spawn(run_self_fanout(feedback_rx.clone(), worker_txs.clone()));

    while let Ok((stream, sockaddr)) = listener.accept().await {
        tokio::spawn(handle_connection(rpc.clone(), stream, sockaddr, internal_ack_tx.clone(), worker_refs.clone()));
    }

    for handle in worker_handles.into_iter() {
        handle.await.unwrap();
    }

    fanout_handle.await.unwrap();
}
