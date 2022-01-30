use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use tokio::fs::{File, read, rename};
use tokio::io::AsyncWriteExt;
use tokio::sync::RwLock;

use crate::domain::{SectorIdx, SectorVec};
use crate::common::*;


#[async_trait::async_trait]
pub trait SectorsManager: Send + Sync {
    /// Returns 4096 bytes of sector data by index.
    async fn read_data(&self, idx: SectorIdx) -> SectorVec;

    /// Returns timestamp and write rank of the process which has saved this data.
    /// Timestamps and ranks are relevant for atomic register algorithm, and are described
    /// there.
    async fn read_metadata(&self, idx: SectorIdx) -> (u64, u8);

    /// Writes a new data, along with timestamp and write rank to some sector.
    async fn write(&self, idx: SectorIdx, sector: &(SectorVec, u64, u8));
}

struct InternalSectorsManager {
    storage_dir: PathBuf,
    metadata: Vec<RwLock<HashMap<SectorIdx, (u64, u8)>>>,
}

impl InternalSectorsManager {
    fn new(storage_dir: PathBuf) -> Self {
        Self {
            metadata: Self::recover_metadata(storage_dir.clone()).into_iter().map(|m| RwLock::new(m)).collect(),
            storage_dir,
        }
    }

    fn recover_metadata(storage_dir: PathBuf) -> Vec<HashMap<SectorIdx, (u64, u8)>> {
        let mut metadata: Vec<HashMap<SectorIdx, (u64, u8)>> = (0..WORKERS_NUM).map(|_| HashMap::new()).collect();

        for entry in std::fs::read_dir(storage_dir.as_path()).unwrap() {
            let dir = entry.unwrap();
            let sliced: Vec<String> = dir.file_name().to_string_lossy().into_owned().split("_").map(|s| s.to_string()).collect();

            if sliced.len() != 3 {
                std::fs::remove_file(dir.path()).unwrap();
                continue;
            }

            let idx: SectorIdx = sliced[0].parse().unwrap();
            let ts: u64 = sliced[1].parse().unwrap();
            let wr: u8 = sliced[2].parse().unwrap();

            match metadata[(idx % WORKERS_NUM) as usize].entry(idx.clone()) {
                Entry::Occupied(mut o) => {
                    let (o_ts, o_wr) = o.get_mut();
                    if (*o_ts, *o_wr) < (ts, wr) {
                        std::fs::remove_file(Self::file_path(&storage_dir, &idx, o_ts, o_wr)).unwrap();
                        *o_ts = ts;
                        *o_wr = wr;
                    } else {
                        std::fs::remove_file(dir.path()).unwrap();
                    }
                }
                Entry::Vacant(v) => {
                    v.insert((ts, wr));
                }
            }
        }

        metadata
    }

    fn file_path(storage_dir: &PathBuf, idx: &SectorIdx, ts: &u64, wr: &u8) -> PathBuf {
        storage_dir.as_path().join(format!("{}_{}_{}", idx, ts, wr))
    }

    fn file_path_tmp(&self, idx: &SectorIdx) -> PathBuf {
        self.storage_dir.as_path().join(format!(".tmp_{}", idx))
    }
}

#[async_trait::async_trait]
impl SectorsManager for InternalSectorsManager {
    async fn read_data(&self, idx: SectorIdx) -> SectorVec {
        match self.metadata[(idx % WORKERS_NUM) as usize].read().await.get(&idx) {
            Some((ts, wr)) => {
                SectorVec(read(InternalSectorsManager::file_path(&self.storage_dir, &idx, ts, wr)).await.unwrap())
            }
            None => SectorVec(vec![0u8; 4096])
        }
    }

    async fn read_metadata(&self, idx: SectorIdx) -> (u64, u8) {
        *self.metadata[(idx % WORKERS_NUM) as usize].read().await.get(&idx).unwrap_or(&(0, 0))
    }

    async fn write(&self, idx: SectorIdx, sector: &(SectorVec, u64, u8)) {
        let (SectorVec(data), ts, wr) = sector;

        let dest = InternalSectorsManager::file_path(&self.storage_dir, &idx, ts, wr);
        let tmp = self.file_path_tmp(&idx);

        let mut tmp_file = File::create(tmp.as_path()).await.unwrap();
        tmp_file.write(&data).await.unwrap();
        tmp_file.sync_data().await.unwrap();
        drop(tmp_file);

        rename(tmp.as_path(), dest.as_path()).await.unwrap();
        let dest_file = File::open(dest.as_path()).await.unwrap();
        dest_file.sync_data().await.unwrap();
        drop(dest_file);

        if let Some((o_ts, o_wr)) = self.metadata[(idx % WORKERS_NUM) as usize].write().await.insert(idx, (*ts, *wr)) {
            let _ = tokio::fs::remove_file(InternalSectorsManager::file_path(&self.storage_dir, &idx, &o_ts, &o_wr)).await;
        }
    }
}

/// Path parameter points to a directory to which this method has exclusive access.
pub fn build_sectors_manager(storage_dir: PathBuf) -> Arc<dyn SectorsManager> {
    Arc::new(InternalSectorsManager::new(storage_dir))
}
