use std::path::PathBuf;

use sha2::{Digest, Sha256};
use tokio::fs::{File, read, rename};
use tokio::io::AsyncWriteExt;

#[async_trait::async_trait]
/// A helper trait for small amount of durable metadata needed by the register algorithm
/// itself. Again, it is only for AtomicRegister definition. StableStorage in unit tests
/// is durable, as one could expect.
pub trait StableStorage: Send + Sync {
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String>;

    async fn get(&self, key: &str) -> Option<Vec<u8>>;
}

struct InternalStableStorage {
    storage_dir: PathBuf,
}

impl InternalStableStorage {
    fn get_filename(key: &str) -> Result<String, String> {
        if key.len() > 255 {
            return Err("invalid key".to_string());
        }

        Ok(format!(
            "{:X}",
            Sha256::new().chain(key.as_bytes()).finalize()
        ))
    }

    fn new(storage_dir: PathBuf) -> Self {
        Self { storage_dir }
    }
}

#[async_trait::async_trait]
impl StableStorage for InternalStableStorage {
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String> {
        let hash = InternalStableStorage::get_filename(key)?;

        if value.len() > 65535 {
            return Err("invalid value".to_string());
        }

        let tmp = self.storage_dir.as_path().join(format!(".tmp_{}", hash));
        let mut tmp_file = File::create(tmp.as_path()).await.map_err(|e| e.to_string())?;

        tmp_file.write_all(value).await.map_err(|e| e.to_string())?;
        tmp_file.sync_data().await.map_err(|e| e.to_string())?;
        drop(tmp_file);

        let dest = self.storage_dir.as_path().join(hash);

        rename(tmp.as_path(), dest.as_path()).await.map_err(|e| e.to_string())?;

        let dest_file = File::open(dest.as_path()).await.map_err(|e| e.to_string())?;
        dest_file.sync_data().await.map_err(|e| e.to_string())?;
        drop(dest_file);

        Ok(())
    }

    async fn get(&self, key: &str) -> Option<Vec<u8>> {
        let hash = InternalStableStorage::get_filename(key).ok()?;
        let mut dir = self.storage_dir.clone();
        dir.push(hash);

        if !dir.exists() {
            return None;
        }

        read(dir).await.ok()
    }
}

pub(super) fn build_stable_storage(storage_dir: PathBuf) -> Box<dyn StableStorage> {
    Box::new(InternalStableStorage::new(storage_dir))
}
