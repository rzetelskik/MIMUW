use assignment_2_solution::{
    build_atomic_register, AtomicRegister, Broadcast, ClientRegisterCommand, OperationComplete,
    RegisterClient, SectorVec, SectorsManager, Send, StableStorage,
};
use async_channel::{Receiver, Sender};
use std::collections::HashMap;
use std::future::Future;
use std::ops::Deref;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::time::Duration;

pub async fn build_registers(
    tx_client: Sender<Send>,
    processes_count: usize,
    drive: &mut RamDrive,
) -> Vec<Option<Box<dyn AtomicRegister>>> {
    let register_client = BufferClient {
        processes_count,
        buffer: tx_client,
    };
    futures::future::join_all((0..processes_count).into_iter().map(|ident| {
        build_atomic_register(
            ident as u8 + 1,
            Box::new(RamStableStorage::default()),
            Arc::new(register_client.clone()),
            Arc::new(drive.clone()),
            processes_count,
        )
    }))
    .await
    .into_iter()
    .map(Some)
    .collect()
}

#[derive(Clone, Default)]
pub struct RamStableStorage {
    map: Arc<Mutex<HashMap<String, Vec<u8>>>>,
}

#[async_trait::async_trait]
impl StableStorage for RamStableStorage {
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String> {
        let mut map = self.map.lock().unwrap();
        map.insert(key.to_owned(), value.to_vec());
        Ok(())
    }

    async fn get(&self, key: &str) -> Option<Vec<u8>> {
        let map = self.map.lock().unwrap();
        map.get(key).map(Clone::clone)
    }
}

#[derive(Clone, Default)]
pub struct RamDrive {
    map: Arc<Mutex<HashMap<u64, (SectorVec, u64, u8)>>>,
}

#[async_trait::async_trait]
impl SectorsManager for RamDrive {
    async fn read_data(&self, idx: u64) -> SectorVec {
        let map = self.map.lock().unwrap();
        match map.get(&idx) {
            Some((sector_data, _, _)) => sector_data.clone(),
            None => SectorVec(vec![0; 4096]),
        }
    }

    async fn read_metadata(&self, idx: u64) -> (u64, u8) {
        let map = self.map.lock().unwrap();
        match map.get(&idx) {
            Some((_, timestamp, rank)) => (*timestamp, *rank),
            None => (0, 0),
        }
    }

    async fn write(&self, idx: u64, sector: &(SectorVec, u64, u8)) {
        let mut map = self.map.lock().unwrap();
        map.insert(idx, sector.clone());
    }
}

#[derive(Clone)]
pub struct BufferClient {
    processes_count: usize,
    buffer: Sender<Send>,
}

#[async_trait::async_trait]
impl RegisterClient for BufferClient {
    async fn send(&self, msg: Send) {
        self.buffer.send(msg).await.unwrap();
    }

    async fn broadcast(&self, msg: Broadcast) {
        for target in 0..self.processes_count {
            self.send(Send {
                cmd: msg.cmd.clone(),
                target: target + 1,
            })
            .await
        }
    }
}

pub async fn propagate_all_messages(
    registers: &mut Vec<Option<Box<dyn AtomicRegister>>>,
    rx_client: &Receiver<Send>,
) {
    while let Ok(Ok(msg)) = tokio::time::timeout(Duration::from_millis(100), rx_client.recv()).await
    {
        if let Some(register) = registers.get_mut(msg.target - 1).unwrap() {
            register.system_command(msg.cmd.deref().clone()).await;
        }
    }
}

pub async fn send_client_cmd(
    registers: &mut Vec<Option<Box<dyn AtomicRegister>>>,
    target: usize,
    cmd: ClientRegisterCommand,
    operation_complete: Box<
        dyn FnOnce(OperationComplete) -> Pin<Box<dyn Future<Output = ()> + std::marker::Send>>
            + std::marker::Send
            + Sync,
    >,
) {
    registers
        .get_mut(target)
        .unwrap()
        .as_mut()
        .unwrap()
        .client_command(cmd, operation_complete)
        .await;
}
