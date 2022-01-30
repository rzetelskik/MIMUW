mod public_test;
mod solution;

use solution::{Action, Edit, EditRequest, EditorClient, Operation, Process, ReliableBroadcast};
use std::time::Duration;

use executor::{Handler, ModuleRef, System};

#[tokio::main]
async fn main() {
    // Set the system up:
    let mut system = System::new().await;

    let broadcast = SimpleBroadcast::new(&mut system).await;
    let client0 = SimpleClient::new(&mut system, 0).await;
    let client1 = SimpleClient::new(&mut system, 1).await;

    let processes = [
        Process::new(
            &mut system,
            0,
            Box::new(broadcast.clone()),
            Box::new(client0.clone()),
        )
        .await,
        Process::new(
            &mut system,
            1,
            Box::new(broadcast.clone()),
            Box::new(client1.clone()),
        )
        .await,
    ];

    client0
        .send(InitClient {
            process: processes[0].clone(),
        })
        .await;
    client1
        .send(InitClient {
            process: processes[1].clone(),
        })
        .await;

    broadcast.send(InitBroadcast { processes }).await;

    // Simulate the example from the diagram:
    client0
        .send(InternalClientCommand {
            action: Action::Insert { idx: 0, ch: 'i' },
        })
        .await;

    tokio::time::sleep(Duration::from_millis(1000)).await;

    client0
        .send(InternalClientCommand {
            action: Action::Insert { idx: 0, ch: 'H' },
        })
        .await;
    client1
        .send(InternalClientCommand {
            action: Action::Insert { idx: 1, ch: '!' },
        })
        .await;

    tokio::time::sleep(Duration::from_millis(1000)).await;

    system.shutdown().await;
}

/// Examplary client implementation.
///
/// This client itself has an asynchronous interface (`InternalClientCommand`),
/// so requesting edits is prone to races: use it for demos with care!
struct SimpleClient<const N: usize> {
    rank: usize,
    text: String,
    num_applied: usize,
    process: Option<ModuleRef<Process<N>>>,
}

struct InitClient<const N: usize> {
    process: ModuleRef<Process<N>>,
}

struct InternalClientCommand {
    action: Action,
}

impl<const N: usize> EditorClient for SimpleClient<N> {}

impl<const N: usize> SimpleClient<N> {
    pub(crate) async fn new(system: &mut System, rank: usize) -> ModuleRef<Self> {
        let self_ref = system
            .register_module(Self {
                rank,
                text: String::new(),
                num_applied: 0,
                process: None,
            })
            .await;
        self_ref
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<InitClient<N>> for SimpleClient<N> {
    async fn handle(&mut self, msg: InitClient<N>) {
        self.process = Some(msg.process);
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<InternalClientCommand> for SimpleClient<N> {
    async fn handle(&mut self, msg: InternalClientCommand) {
        match msg.action {
            Action::Insert { idx, .. } => {
                assert!(idx <= self.text.len(), "Invalid idx!");
            }
            Action::Delete { idx } => {
                assert!(idx < self.text.len(), "Invalid idx!");
            }
            Action::Nop => {
                panic!("Client cannot request NOP!")
            }
        }

        let request = EditRequest {
            num_applied: self.num_applied,
            action: msg.action,
        };

        self.process.as_ref().unwrap().send(request).await;
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<Edit> for SimpleClient<N> {
    async fn handle(&mut self, msg: Edit) {
        let action = msg.action;
        action.apply_to(&mut self.text);
        self.num_applied += 1;

        println!(
            "Client {}: '{}' ({})",
            self.rank, self.text, self.num_applied
        );
    }
}

/// Examplary broadcast implementation.
///
/// It uses `sleep()` to mimic network latency.
struct SimpleBroadcast<const N: usize> {
    processes: Option<[ModuleRef<Process<N>>; N]>,
}

struct InitBroadcast<const N: usize> {
    processes: [ModuleRef<Process<N>>; N],
}

impl<const N: usize> ReliableBroadcast<N> for SimpleBroadcast<N> {}

impl<const N: usize> SimpleBroadcast<N> {
    pub(crate) async fn new(system: &mut System) -> ModuleRef<Self> {
        let self_ref = system.register_module(Self { processes: None }).await;
        self_ref
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<InitBroadcast<N>> for SimpleBroadcast<N> {
    async fn handle(&mut self, msg: InitBroadcast<N>) {
        self.processes = Some(msg.processes);
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<Operation> for SimpleBroadcast<N> {
    async fn handle(&mut self, msg: Operation) {
        tokio::time::sleep(Duration::from_millis(200)).await;
        for i in 0..N {
            if i != msg.process_rank {
                self.processes.as_ref().unwrap()[i].send(msg.clone()).await;
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }
}
