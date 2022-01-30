#[cfg(test)]
pub(crate) mod tests {
    use async_channel::{unbounded, Receiver, Sender};
    use executor::{Handler, ModuleRef, System};
    use ntest::timeout;
    use std::{convert::TryInto, time::Duration, usize};

    use crate::solution::{
        Action, Edit, EditRequest, EditorClient, Operation, Process, ReliableBroadcast,
        ReliableBroadcastRef,
    };

    #[tokio::test]
    #[timeout(200)]
    async fn two_processes_one_issues_one_edit() {
        const NUM_PROCESSES: usize = 2;
        const TEXT: &str = "";

        let mut system = System::new().await;
        let (b, mut c) = build_system::<NUM_PROCESSES>(&mut system, TEXT).await;

        // P0, edit0:
        c[0].request(Action::Insert { idx: 0, ch: 'i' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: 'i' }, "i")
        );

        // P0, edit0 -> P1:
        b.forward(0, 1).await;
        assert_eq!(c[1].receive().await, (Action::Nop, ""));
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: 'i' }, "i")
        );

        // P1, NOP -> P0:
        b.forward(1, 0).await;
        assert_eq!(c[0].receive().await, (Action::Nop, "i"));

        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(200)]
    async fn example_from_diagram() {
        const NUM_PROCESSES: usize = 2;
        const TEXT: &str = "";

        let mut system = System::new().await;
        let (b, mut c) = build_system::<NUM_PROCESSES>(&mut system, TEXT).await;

        // P0, edit0:
        c[0].request(Action::Insert { idx: 0, ch: 'i' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: 'i' }, "i")
        );

        // P0, edit0 -> P1:
        b.forward(0, 1).await;
        assert_eq!(c[1].receive().await, (Action::Nop, ""));
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: 'i' }, "i")
        );

        // P1, NOP -> P0:
        b.forward(1, 0).await;
        assert_eq!(c[0].receive().await, (Action::Nop, "i"));

        // P0, edit1:
        c[0].request(Action::Insert { idx: 0, ch: 'H' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: 'H' }, "Hi")
        );

        // P1, edit1:
        c[1].request(Action::Insert { idx: 1, ch: '!' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: '!' }, "i!")
        );

        // P0, edit1 -> P1:
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: 'H' }, "Hi!")
        );

        // P1, edit1 -> P0:
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 2, ch: '!' }, "Hi!")
        );

        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(200)]
    async fn two_processes_each_issues_two_edits() {
        const NUM_PROCESSES: usize = 2;
        const TEXT: &str = "";

        let mut system = System::new().await;
        let (b, mut c) = build_system::<NUM_PROCESSES>(&mut system, TEXT).await;

        // P0, edit0:
        c[0].request(Action::Insert { idx: 0, ch: '0' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "0")
        );

        // P1, edit0:
        c[1].request(Action::Insert { idx: 0, ch: '1' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: '1' }, "1")
        );

        // P1, edit0 -> P0:
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 1, ch: '1' }, "01")
        );

        // P0, edit0 -> P1:
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "01")
        );

        // P1, edit1:
        c[1].request(Action::Insert { idx: 1, ch: '2' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: '2' }, "021")
        );

        // P0, edit1:
        c[0].request(Action::Insert { idx: 1, ch: '3' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 1, ch: '3' }, "031")
        );

        // P0, edit1 -> P1:
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: '3' }, "0321")
        );

        // P1, edit1 -> P0:
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 2, ch: '2' }, "0321")
        );

        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(1000)]
    async fn two_processes_each_issues_two_edits_at_once() {
        const NUM_PROCESSES: usize = 2;
        const TEXT: &str = "x";

        let mut system = System::new().await;
        let (b, mut c) = build_system::<NUM_PROCESSES>(&mut system, TEXT).await;

        // P0, edit0 & edit1:
        c[0].request(Action::Insert { idx: 0, ch: '0' }).await;
        c[0].request(Action::Insert { idx: 1, ch: '1' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "0x")
        );
        assert!(c[0].no_receive().await);

        // P1, edit0 & edit1:
        c[1].request(Action::Insert { idx: 1, ch: '2' }).await;
        c[1].request(Action::Insert { idx: 0, ch: '3' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: '2' }, "x2")
        );
        assert!(c[1].no_receive().await);

        // P1, edit0 -> P0:
        b.forward(1, 0).await;
        assert!(b.no_forward(1, 0).await);
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 2, ch: '2' }, "0x2")
        );

        // P0, edit0 -> P1:
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "0x2")
        );

        // P0, edit1 -> client:
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 3, ch: '1' }, "0x21")
        );

        // P1, edit1 -> client:
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: '3' }, "03x2")
        );

        // P1, edit1 -> P0:
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 1, ch: '3' }, "03x21")
        );

        // P0, edit1 -> P1:
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 4, ch: '1' }, "03x21")
        );

        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(200)]
    async fn three_processes_each_issues_one_edit() {
        const NUM_PROCESSES: usize = 3;
        const TEXT: &str = "";

        let mut system = System::new().await;
        let (b, mut c) = build_system::<NUM_PROCESSES>(&mut system, TEXT).await;

        // P0, edit0:
        c[0].request(Action::Insert { idx: 0, ch: '0' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "0")
        );

        // P1, edit0:
        c[1].request(Action::Insert { idx: 0, ch: '1' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: '1' }, "1")
        );

        // P2, edit0:
        c[2].request(Action::Insert { idx: 0, ch: '2' }).await;
        assert_eq!(
            c[2].receive().await,
            (Action::Insert { idx: 0, ch: '2' }, "2")
        );

        // P1, edit0 -> P0:
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 1, ch: '1' }, "01")
        );

        // P2, edit0 -> P0:
        b.forward(2, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 2, ch: '2' }, "012")
        );

        // P2, edit0 -> P1:
        b.forward(2, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: '2' }, "12")
        );

        // P0, edit0 -> P1:
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "012")
        );

        // P0, edit0 -> P2:
        b.forward(0, 2).await;
        assert_eq!(
            c[2].receive().await,
            (Action::Insert { idx: 0, ch: '0' }, "02")
        );

        // P1, edit0 -> P2:
        b.forward(1, 2).await;
        assert_eq!(
            c[2].receive().await,
            (Action::Insert { idx: 1, ch: '1' }, "012")
        );

        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(200)]
    async fn transformations_insert() {
        const NUM_PROCESSES: usize = 2;
        const TEXT: &str = "0";

        let mut system = System::new().await;
        let (b, mut c) = build_system::<NUM_PROCESSES>(&mut system, TEXT).await;

        c[0].request(Action::Insert { idx: 0, ch: 'a' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 0, ch: 'a' }, "a0")
        );

        c[1].request(Action::Insert { idx: 1, ch: 'b' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: 'b' }, "0b")
        );

        // else: insert(p1 + 1, c1, r1)
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 2, ch: 'b' }, "a0b")
        );

        // if p1 < p2: insert(p1, c1, r1)
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 0, ch: 'a' }, "a0b")
        );


        c[0].request(Action::Insert { idx: 1, ch: 'c' }).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 1, ch: 'c' }, "ac0b")
        );

        c[1].request(Action::Insert { idx: 1, ch: 'd' }).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: 'd' }, "ad0b")
        );

        // else: insert(p1 + 1, c1, r1)
        b.forward(1, 0).await;
        assert_eq!(
            c[0].receive().await,
            (Action::Insert { idx: 2, ch: 'd' }, "acd0b")
        );

        // if p1 = p2 and r1 < r2: insert(p1, c1, r1)
        b.forward(0, 1).await;
        assert_eq!(
            c[1].receive().await,
            (Action::Insert { idx: 1, ch: 'c' }, "acd0b")
        );

        system.shutdown().await;
    }


    pub(crate) async fn build_system<const N: usize>(
        system: &mut System,
        initial_text: &str,
    ) -> (ControllableBroadcast<N>, [ControllableClient<N>; N]) {
        // Channels for broadcast:
        let (channels_in, channels_out): (Vec<_>, Vec<_>) =
            std::iter::repeat_with(|| unbounded()).take(N * N).unzip();
        let senders: [[Sender<Operation>; N]; N] = channels_in
            .chunks(N)
            .map(|v| v.to_vec().try_into().unwrap())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        let receivers: [[Receiver<Operation>; N]; N] = channels_out
            .chunks(N)
            .map(|v| v.to_vec().try_into().unwrap())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        // Broadcast – executor system part:
        let broadcast_module = ControllableBroadcastModule::new(system, senders).await;

        // Clients & processes:
        let mut clients_vec = Vec::new();
        let mut processes_vec = Vec::new();
        for rank in 0..N {
            let (c, p) = build_client_and_process(
                system,
                rank,
                initial_text,
                Box::new(broadcast_module.clone()),
            )
            .await;
            clients_vec.push(c);
            processes_vec.push(p);
        }
        // "unwrap()" without implementing `Debug`:
        let processes = match processes_vec.try_into() {
            Ok(array) => array,
            Err(_) => panic!("Cannot convert processes vec into array!"),
        };
        // "unwrap()" without implementing `Debug`:
        let clients = match clients_vec.try_into() {
            Ok(array) => array,
            Err(_) => panic!("Cannot convert clients vec into array!"),
        };

        // Broadcast – synchronous part:
        let broadcast = ControllableBroadcast::new(processes, receivers);

        (broadcast, clients)
    }

    async fn build_client_and_process<const N: usize>(
        system: &mut System,
        rank: usize,
        initial_text: &str,
        broadcast: Box<dyn ReliableBroadcastRef<N>>,
    ) -> (ControllableClient<N>, ModuleRef<Process<N>>) {
        let (sender, receiver) = unbounded();
        let client_module = ControllableClientModule::<N>::new(system, sender).await;
        let process = Process::new(system, rank, broadcast, Box::new(client_module)).await;
        let client = ControllableClient::new(initial_text, process.clone(), receiver);
        (client, process)
    }

    /// Client – executor system part (module).
    /// Receives messages from the process and forwards to the synchronous part.
    ///
    /// This client consists of two parts: the executor system part (module) and
    /// the synchronous part. The parts are connected via async-channel.
    pub(crate) struct ControllableClientModule<const N: usize> {
        to_client: Sender<Edit>,
    }

    impl<const N: usize> EditorClient for ControllableClientModule<N> {}

    impl<const N: usize> ControllableClientModule<N> {
        async fn new(system: &mut System, to_client: Sender<Edit>) -> ModuleRef<Self> {
            let self_ref = system.register_module(Self { to_client }).await;
            self_ref
        }
    }

    #[async_trait::async_trait]
    impl<const N: usize> Handler<Edit> for ControllableClientModule<N> {
        async fn handle(&mut self, msg: Edit) {
            self.to_client.send(msg).await.unwrap();
        }
    }

    /// Client – synchronous part.
    /// Allows tests to fully control client operations.
    ///
    /// This client consists of two parts: the executor system part (module) and
    /// the synchronous part. The parts are connected via async-channel.
    pub(crate) struct ControllableClient<const N: usize> {
        text: String,
        num_applied: usize,
        to_process: ModuleRef<Process<N>>,
        from_module: Receiver<Edit>,
    }

    impl<const N: usize> ControllableClient<N> {
        pub(crate) fn new(
            initial_text: &str,
            to_process: ModuleRef<Process<N>>,
            from_module: Receiver<Edit>,
        ) -> Self {
            Self {
                text: initial_text.to_string(),
                num_applied: 0,
                to_process,
                from_module,
            }
        }

        /// Receive and apply all edits.
        pub(crate) async fn receive_all(&mut self) -> &str {
            while !self.from_module.is_empty() {
                let edit = self.from_module.recv().await.unwrap();
                let action = edit.action;
                action.apply_to(&mut self.text);
                self.num_applied += 1;
            }

            self.text.as_str()
        }

        /// Receive single edit.
        pub(crate) async fn receive(&mut self) -> (Action, &str) {
            let edit = self.from_module.recv().await.unwrap();
            let action = edit.action;
            action.apply_to(&mut self.text);
            self.num_applied += 1;

            (action, self.text.as_str())
        }

        /// Are there no edits to be received?
        pub(crate) async fn no_receive(&mut self) -> bool {
            tokio::time::sleep(Duration::from_millis(200)).await;
            self.from_module.is_empty()
        }

        /// Send edit request to the process.
        pub(crate) async fn request(&self, action: Action) {
            self.to_process
                .send(EditRequest {
                    num_applied: self.num_applied,
                    action,
                })
                .await;
        }
    }

    /// Broadcast – executor system part (module).
    /// Receives messages from processes and forwards to the synchronous part.
    ///
    /// This broadcast consists of two parts: the executor system part (module)
    /// and the synchronous part. The parts are connected via async-channels.
    struct ControllableBroadcastModule<const N: usize> {
        channels: [[Sender<Operation>; N]; N],
    }

    impl<const N: usize> ReliableBroadcast<N> for ControllableBroadcastModule<N> {}

    impl<const N: usize> ControllableBroadcastModule<N> {
        pub(crate) async fn new(
            system: &mut System,
            channels: [[Sender<Operation>; N]; N],
        ) -> ModuleRef<Self> {
            let self_ref = system.register_module(Self { channels }).await;
            self_ref
        }
    }

    #[async_trait::async_trait]
    impl<const N: usize> Handler<Operation> for ControllableBroadcastModule<N> {
        async fn handle(&mut self, msg: Operation) {
            for i in 0..N {
                if i != msg.process_rank {
                    self.channels[msg.process_rank][i]
                        .send(msg.clone())
                        .await
                        .unwrap();
                }
            }
        }
    }

    /// Broadcast – synchronous part.
    /// Allows tests to fully control messages broadcasting.
    ///
    /// This broadcast consists of two parts: the executor system part (module)
    /// and the synchronous part. The parts are connected via async-channels.
    pub(crate) struct ControllableBroadcast<const N: usize> {
        processes: [ModuleRef<Process<N>>; N],
        channels: [[Receiver<Operation>; N]; N],
    }

    impl<const N: usize> ControllableBroadcast<N> {
        pub(crate) fn new(
            processes: [ModuleRef<Process<N>>; N],
            channels: [[Receiver<Operation>; N]; N],
        ) -> Self {
            Self {
                processes,
                channels,
            }
        }

        /// Forward single message.
        pub(crate) async fn forward(&self, from: usize, to: usize) {
            let msg = self.channels[from][to].recv().await.unwrap();
            self.processes[to].send(msg).await;
        }

        /// Try to forward all possible messages.
        pub(crate) async fn forward_all(&self) {
            let mut sth_forwarded = false;

            loop {
                tokio::task::yield_now().await;
                // tokio::time::sleep(Duration::from_millis(200)).await;
                for from in 0..N {
                    for to in 0..N {
                        while !self.channels[from][to].is_empty() {
                            let msg = self.channels[from][to].recv().await.unwrap();
                            self.processes[to].send(msg).await;
                            sth_forwarded = true;
                        }
                    }
                }

                if !sth_forwarded {
                    break;
                } else {
                    sth_forwarded = false;
                }
            }
        }

        /// Are there no messages to be forwarded?
        pub(crate) async fn no_forward(&self, from: usize, to: usize) -> bool {
            tokio::time::sleep(Duration::from_millis(200)).await;
            self.channels[from][to].is_empty()
        }
    }
}
