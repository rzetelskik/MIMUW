mod public_test;
mod solution;

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use tokio::sync::Mutex;

use log::LevelFilter;
use std::time::Duration;
use uuid::Uuid;

use crate::solution::{Disable, ProcessConfig, ProcessState, Raft, RaftMessage, StableStorage};
use executor::{Handler, Message, ModuleRef, System};

#[tokio::main]
async fn main() {
    // Your solution may do some logging, so progress will be visible:
    env_logger::builder().filter_level(LevelFilter::Info).init();

    let sender = ExecutorSender::default();
    let mut system = System::new().await;

    // In real implementations timeouts have to be randomized, but this
    // assignment requires not to do so for testing purposes:
    let (raft_process0, id0) = build_process(
        &mut system,
        Duration::from_millis(500),
        2,
        Box::new(sender.clone()),
    )
    .await;
    let (raft_process1, id1) = build_process(
        &mut system,
        Duration::from_millis(1000),
        2,
        Box::new(sender.clone()),
    )
    .await;
    sender.insert(id0, Box::new(raft_process0.clone())).await;
    sender.insert(id1, Box::new(raft_process1)).await;
    tokio::time::sleep(Duration::from_millis(2000)).await;

    // `Disable` makes it possible to simulate network partitions:
    raft_process0.send(Disable).await;

    system.shutdown().await;
}

async fn build_process(
    system: &mut System,
    election_timeout: Duration,
    processes_count: usize,
    sender: Box<dyn crate::solution::Sender>,
) -> (ModuleRef<Raft>, Uuid) {
    let self_id = Uuid::new_v4();
    let config = ProcessConfig {
        self_id,
        election_timeout,
        processes_count,
    };

    (
        Raft::new(system, config, Box::new(RamStorage::default()), sender).await,
        self_id,
    )
}

#[derive(Clone, Default)]
struct ExecutorSender {
    processes: Arc<Mutex<HashMap<Uuid, BoxedRecipient<RaftMessage>>>>,
}

impl ExecutorSender {
    async fn insert(&self, id: Uuid, addr: BoxedRecipient<RaftMessage>) {
        self.processes.lock().await.insert(id, addr);
    }
}

#[async_trait::async_trait]
impl crate::solution::Sender for ExecutorSender {
    async fn send(&self, target: &Uuid, msg: RaftMessage) {
        if let Some(addr) = self.processes.lock().await.get(target) {
            let addr = addr.clone_to_box();
            addr.send(msg).await;
        }
    }

    async fn broadcast(&self, msg: RaftMessage) {
        let map = self.processes.lock().await;
        for addr in map.values() {
            let addr = addr.clone_to_box();
            addr.send(msg).await;
        }
    }
}

#[derive(Default, Clone)]
struct RamStorage {
    state: Arc<std::sync::Mutex<Option<ProcessState>>>,
}

impl StableStorage for RamStorage {
    fn put(&mut self, state: &ProcessState) {
        *self.state.lock().unwrap().deref_mut() = Some(*state);
    }

    fn get(&self) -> Option<ProcessState> {
        *self.state.lock().unwrap().deref()
    }
}

type BoxedRecipient<M> = Box<dyn Recipient<M>>;

#[async_trait::async_trait]
pub trait Recipient<M>: Send + Sync + 'static
where
    M: Message,
{
    async fn send(&self, msg: M);
    fn clone_to_box(&self) -> BoxedRecipient<M>;
}

#[async_trait::async_trait]
impl<M, T> Recipient<M> for ModuleRef<T>
where
    M: Message,
    T: Handler<M> + Send,
{
    async fn send(&self, msg: M) {
        self.send(msg).await;
    }

    fn clone_to_box(&self) -> BoxedRecipient<M> {
        Box::new(self.clone())
    }
}
