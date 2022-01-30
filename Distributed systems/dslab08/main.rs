mod public_test;
mod solution;

use crate::solution::{DetectorOperation, FailureDetectorModule};
use std::collections::{HashMap, HashSet};
use std::net::{SocketAddr, ToSocketAddrs};
use std::time::Duration;
use tokio::net::UdpSocket;
use tokio::time::sleep;
use uuid::Uuid;

use executor::System;

fn unwrap_alive_info(alive_info: DetectorOperation) -> Vec<Uuid> {
    match alive_info {
        DetectorOperation::AliveInfo(alive) => alive,
        _ => panic!("invalid type"),
    }
}

#[tokio::main]
async fn main() {
    let mut system = System::new().await;
    let delay = Duration::from_millis(50);
    let addr1 = (
        Uuid::new_v4(),
        ("127.0.0.1", 9121)
            .to_socket_addrs()
            .unwrap()
            .next()
            .unwrap(),
    );
    let addr2 = (
        Uuid::new_v4(),
        ("127.0.0.1", 9122)
            .to_socket_addrs()
            .unwrap()
            .next()
            .unwrap(),
    );
    let all_idents: HashSet<Uuid> = [addr1, addr2].iter().map(|v| v.0).collect();
    let addresses: HashMap<Uuid, SocketAddr> = [addr1, addr2].iter().cloned().collect();

    let _detector_1 =
        FailureDetectorModule::new(&mut system, delay, &addresses, addr1.0, all_idents.clone())
            .await;
    let _detector_2 =
        FailureDetectorModule::new(&mut system, delay, &addresses, addr2.0, all_idents.clone())
            .await;

    let socket = UdpSocket::bind("127.0.0.1:0").await.unwrap();

    sleep(delay * 3).await;

    let mut buf = [0; 512];

    socket
        .send_to(
            bincode::serialize(&DetectorOperation::AliveRequest)
                .unwrap()
                .as_slice(),
            &addr1.1,
        )
        .await
        .expect("cannot send?");

    let len = socket.recv(&mut buf).await.unwrap();
    let alive_info =
        unwrap_alive_info(bincode::deserialize(&buf[..len]).expect("Invalid format of alive info!"));
    println!("Alive according to the first process: {:?}", alive_info);

    sleep(delay).await;

    socket
        .send_to(
            bincode::serialize(&DetectorOperation::AliveRequest)
                .unwrap()
                .as_slice(),
            &addr2.1,
        )
        .await
        .expect("cannot send?");

    let len = socket.recv(&mut buf).await.unwrap();
    let alive_info =
        unwrap_alive_info(bincode::deserialize(&buf[..len]).expect("Invalid format of alive info!"));
    println!("Alive according to the second process: {:?}", alive_info);

    system.shutdown().await;
}
