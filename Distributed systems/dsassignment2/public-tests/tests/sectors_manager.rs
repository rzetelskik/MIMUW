use assignment_2_solution::{build_sectors_manager, SectorVec};
use ntest::timeout;
use rand::Rng;
use std::sync::Arc;
use tempfile::tempdir;

#[tokio::test]
#[timeout(300)]
async fn drive_can_store_data() {
    // given
    let root_drive_dir = tempdir().unwrap();
    let sectors_manager = build_sectors_manager(root_drive_dir.into_path());

    // when
    sectors_manager
        .write(0, &(SectorVec(vec![2; 4096]), 1, 1))
        .await;
    let data = sectors_manager.read_data(0).await;

    // then
    assert_eq!(sectors_manager.read_metadata(0).await, (1, 1));
    assert_eq!(data.0.len(), 4096);
    assert_eq!(data.0, vec![2; 4096])
}

#[tokio::test]
#[timeout(200)]
async fn data_survives_crash() {
    // given
    let root_drive_dir = tempdir().unwrap();
    {
        let sectors_manager = build_sectors_manager(root_drive_dir.path().to_path_buf());
        sectors_manager
            .write(1, &(SectorVec(vec![7; 4096]), 1, 2))
            .await;
    }

    let sectors_manager = build_sectors_manager(root_drive_dir.path().to_path_buf());

    // when
    let (timestamp, write_rank) = sectors_manager.read_metadata(1).await;
    let data = sectors_manager.read_data(1).await;

    // then
    assert_eq!(timestamp, 1);
    assert_eq!(write_rank, 2);
    assert_eq!(data.0, vec![7; 4096]);
}

#[tokio::test]
#[timeout(5000)]
async fn concurrent_operation_on_different_sectors() {
    // given
    let root_drive_dir = tempdir().unwrap();
    let sectors_manager = Arc::new(build_sectors_manager(root_drive_dir.path().to_path_buf()));
    let tasks: usize = 10;
    let sectors_batch = 16;
    let mut task_handles = vec![];

    // when
    for i in 0..tasks {
        let sectors_manager = sectors_manager.clone();
        task_handles.push(tokio::spawn(async move {
            let sectors_start = sectors_batch * i;
            let sectors_end = sectors_start + sectors_batch;

            for sector_idx in sectors_start..sectors_end {
                let sector_idx = sector_idx as u64;
                let data = SectorVec(
                    (0..4096)
                        .map(|_| rand::thread_rng().gen_range(0..255))
                        .collect(),
                );

                sectors_manager
                    .write(sector_idx, &(data.clone(), 1, 1))
                    .await;
                assert_eq!(sectors_manager.read_metadata(sector_idx).await, (1, 1));
                assert_eq!(sectors_manager.read_data(sector_idx).await, data);
            }
        }));
    }

    // then
    for handle in task_handles {
        assert!(handle.await.is_ok())
    }
}
