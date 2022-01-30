use assignment_2_solution::{
    build_atomic_register, build_sectors_manager, Broadcast, ClientCommandHeader,
    ClientRegisterCommand, ClientRegisterCommandContent, RegisterClient, SectorVec, SectorsManager,
    Send,
};
use assignment_2_test_utils::atomic_register::*;
use async_channel::{unbounded, Sender};
use ntest::timeout;
use std::sync::Arc;
use std::time::Duration;
use tempfile::tempdir;

#[tokio::test]
#[timeout(200)]
async fn read_triggers_broadcast() {
    // given
    let (tx, rx) = unbounded();
    let root_drive_dir = tempdir().unwrap();
    let mut register = build_atomic_register(
        1,
        Box::new(RamStableStorage::default()),
        Arc::new(DummyRegisterClient::new(tx)),
        build_sectors_manager(root_drive_dir.into_path()),
        1,
    )
    .await;

    // when
    register
        .client_command(
            ClientRegisterCommand {
                header: ClientCommandHeader {
                    request_identifier: 7,
                    sector_idx: 0,
                },
                content: ClientRegisterCommandContent::Read,
            },
            Box::new(|_op_complete| Box::pin(async {})),
        )
        .await;

    // then
    assert!(matches!(rx.recv().await, Ok(ClientMsg::Broadcast(_))));
}

#[tokio::test]
#[timeout(2000)]
async fn majority_completes_operations_after_crash() {
    // given
    let (tx_client, rx_client) = unbounded();
    let (tx_op_c, rx_op_c) = unbounded();
    let processes_count = 3;

    let mut drive = RamDrive::default();
    let mut registers = build_registers(tx_client, processes_count, &mut drive).await;

    send_client_cmd(
        &mut registers,
        0,
        ClientRegisterCommand {
            header: ClientCommandHeader {
                request_identifier: 7,
                sector_idx: 2,
            },
            content: ClientRegisterCommandContent::Write {
                data: SectorVec(vec![200; 4096]),
            },
        },
        Box::new(|_op_c| Box::pin(async move { tx_op_c.send(()).await.unwrap() })),
    )
    .await;

    registers.get_mut(2).unwrap().take();

    // when
    propagate_all_messages(&mut registers, &rx_client).await;

    // then
    assert_eq!(rx_op_c.recv().await, Ok(()));
    assert_eq!(drive.read_data(2).await, SectorVec(vec![200; 4096]));
}

#[tokio::test]
#[timeout(7000)]
async fn operations_are_not_completed_without_majority() {
    // given
    let (tx_client, rx_client) = unbounded();
    let (tx_op_c, rx_op_c) = unbounded();
    let processes_count = 3;

    let mut drive = RamDrive::default();
    let mut registers = build_registers(tx_client, processes_count, &mut drive).await;

    send_client_cmd(
        &mut registers,
        0,
        ClientRegisterCommand {
            header: ClientCommandHeader {
                request_identifier: 7,
                sector_idx: 2,
            },
            content: ClientRegisterCommandContent::Write {
                data: SectorVec(vec![200; 4096]),
            },
        },
        Box::new(|_op_c| Box::pin(async move { tx_op_c.send(()).await.unwrap() })),
    )
    .await;

    registers.get_mut(1).unwrap().take();
    registers.get_mut(2).unwrap().take();

    // when
    propagate_all_messages(&mut registers, &rx_client).await;

    // then
    assert!(matches!(
        tokio::time::timeout(Duration::from_millis(200), rx_op_c.recv()).await,
        Err(_)
    ));
    assert_eq!(drive.read_data(2).await, SectorVec(vec![0; 4096]));
}

enum ClientMsg {
    Send(Send),
    Broadcast(Broadcast),
}

struct DummyRegisterClient {
    tx: Sender<ClientMsg>,
}

impl DummyRegisterClient {
    fn new(tx: Sender<ClientMsg>) -> Self {
        Self { tx }
    }
}

#[async_trait::async_trait]
impl RegisterClient for DummyRegisterClient {
    async fn send(&self, msg: Send) {
        self.tx.send(ClientMsg::Send(msg)).await.unwrap();
    }

    async fn broadcast(&self, msg: Broadcast) {
        self.tx.send(ClientMsg::Broadcast(msg)).await.unwrap();
    }
}
