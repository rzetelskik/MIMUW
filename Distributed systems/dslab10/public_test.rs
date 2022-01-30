#[cfg(test)]
pub(crate) mod tests {
    use async_channel::unbounded;
    use ntest::timeout;
    use std::time::Duration;
    use uuid::Uuid;

    use crate::solution::{
        Disable, ProcessConfig, Raft, RaftMessage, RaftMessageContent, RaftMessageHeader,
    };
    use crate::{ExecutorSender, RamStorage};
    use executor::{Handler, ModuleRef, System};

    #[tokio::test]
    #[timeout(1500)]
    async fn single_process_transitions_to_leader() {
        // Given:
        let mut system = System::new().await;

        let (tx, rx) = unbounded();
        let sender = ExecutorSender::default();
        let self_id = Uuid::new_v4();
        let raft = Raft::new(
            &mut system,
            ProcessConfig {
                self_id,
                election_timeout: Duration::from_millis(200),
                processes_count: 1,
            },
            Box::new(RamStorage::default()),
            Box::new(sender.clone()),
        )
        .await;
        let spy = RaftSpy::new(&mut system, raft, tx).await;
        sender.insert(self_id, Box::new(spy)).await;

        // When:
        tokio::time::sleep(Duration::from_millis(700)).await;
        let msgs = extract_messages(&rx);

        // Then:
        assert_has_heartbeat_from_leader(self_id, &msgs);

        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(1500)]
    async fn new_leader_is_elected_after_crash() {
        // Given:
        let mut system = System::new().await;

        let (tx, rx) = unbounded();
        let sender = ExecutorSender::default();
        let mut ids = vec![];
        let mut rafts = vec![];
        for i in 0..4 {
            let id = Uuid::new_v4();
            ids.push(id);
            rafts.push(
                Raft::new(
                    &mut system,
                    ProcessConfig {
                        self_id: id,
                        election_timeout: Duration::from_millis(200 * (i + 1)),
                        processes_count: 4,
                    },
                    Box::new(RamStorage::default()),
                    Box::new(sender.clone()),
                )
                .await,
            );
        }
        for i in 0..3 {
            sender.insert(ids[i], Box::new(rafts[i].clone())).await;
        }
        let spy = RaftSpy::new(&mut system, rafts.last().unwrap().clone(), tx).await;
        sender.insert(*ids.last().unwrap(), Box::new(spy)).await;

        // When:
        tokio::time::sleep(Duration::from_millis(400)).await;
        rafts[0].send(Disable).await;
        tokio::time::sleep(Duration::from_millis(600)).await;
        let msgs = extract_messages(&rx);
        let heartbeats = heartbeats_from_leader(&msgs);

        // Then:
        let first_leader_heartbeats = heartbeats
            .iter()
            .take_while(|(_, leader_id)| *leader_id == ids[0]);
        assert!(first_leader_heartbeats.clone().count() > 0);
        assert!(first_leader_heartbeats
            .clone()
            .all(|(header, _)| header.term == 1));
        let second_leader_heartbeats = heartbeats
            .iter()
            .skip_while(|(_, leader_id)| *leader_id == ids[0]);
        assert!(second_leader_heartbeats.clone().count() > 0);
        assert!(second_leader_heartbeats
            .clone()
            .all(|(header, leader_id)| *leader_id == ids[1] && header.term == 2));
        assert_eq!(
            first_leader_heartbeats.count() + second_leader_heartbeats.count(),
            heartbeats.len()
        );

        system.shutdown().await;
    }

    fn assert_has_heartbeat_from_leader(expected_leader: Uuid, msgs: &Vec<RaftMessage>) {
        heartbeats_from_leader(msgs)
            .iter()
            .map(|t| t.1)
            .find(|leader_id| leader_id == &expected_leader)
            .expect("No heartbeat from expected leader!");
    }

    pub(crate) fn heartbeats_from_leader(
        msgs: &Vec<RaftMessage>,
    ) -> Vec<(RaftMessageHeader, Uuid)> {
        let mut res = Vec::new();
        for msg in msgs {
            match msg.content {
                RaftMessageContent::Heartbeat { leader_id } => {
                    res.push((msg.header, leader_id));
                }
                _ => {}
            }
        }
        res
    }

    pub(crate) fn extract_messages<T>(rx: &async_channel::Receiver<T>) -> Vec<T> {
        let mut msgs = Vec::new();
        while let Ok(msg) = rx.try_recv() {
            msgs.push(msg);
        }
        msgs
    }

    pub(crate) struct RaftSpy {
        pub(crate) raft: ModuleRef<Raft>,
        pub(crate) tx: async_channel::Sender<RaftMessage>,
    }

    impl RaftSpy {
        pub(crate) async fn new(
            system: &mut System,
            raft: ModuleRef<Raft>,
            tx: async_channel::Sender<RaftMessage>,
        ) -> ModuleRef<Self> {
            system.register_module(Self { raft, tx }).await
        }
    }

    #[async_trait::async_trait]
    impl Handler<RaftMessage> for RaftSpy {
        async fn handle(&mut self, msg: RaftMessage) {
            let raft = self.raft.clone();
            let tx = self.tx.clone();

            let _ = tx.send(msg).await;
            raft.send(msg).await
        }
    }

    #[async_trait::async_trait]
    impl Handler<Disable> for RaftSpy {
        async fn handle(&mut self, msg: Disable) {
            self.raft.send(msg).await;
        }
    }
}
