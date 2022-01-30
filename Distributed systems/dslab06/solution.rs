use std::path::PathBuf;
use tokio::fs::{read, rename, File};
use tokio::io::AsyncWriteExt;
use sha2::{Digest, Sha256};

// You can add here other imports from std or crates listed in Cargo.toml.

// You can add any private types, structs, consts, functions, methods, etc., you need.

#[async_trait::async_trait]
pub trait StableStorage: Send + Sync {
    /// Stores `value` under  `key`.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String>;

    /// Retrieves value stored under `key`.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn get(&self, key: &str) -> Option<Vec<u8>>;
}

struct SStorage {
    root_storage_dir: PathBuf,
}

impl SStorage {
    fn get_filename(key: &str) -> Result<String, String> {
        if key.len() > 255 {
            return Err("invalid key".to_string());
        }

        Ok(format!("{:X}", Sha256::new().chain(key.as_bytes()).finalize()))
    }
}

#[async_trait::async_trait]
impl StableStorage for SStorage {
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String> {
        let hash = SStorage::get_filename(key)?;

        if value.len() > 65535 {
            return Err("invalid value".to_string());
        }

        let tmp = self.root_storage_dir.as_path().join(format!("{}_tmp", hash));
        let mut tmp_file = File::create(tmp.as_path()).await.unwrap();

        tmp_file.write_all(value).await.unwrap();

        tmp_file.sync_data().await.unwrap();

        let dest = self.root_storage_dir.as_path().join(hash);

        rename(tmp.as_path(), dest.as_path()).await.unwrap();

        let dest_file = File::open(dest.as_path()).await.unwrap();
        dest_file.sync_data().await.unwrap();

        Ok(())
    }

    async fn get(&self, key: &str) -> Option<Vec<u8>> {
        let hash = SStorage::get_filename(key).ok()?;
        let mut dir = self.root_storage_dir.clone();
        dir.push(hash);

        if !dir.exists() {
            return None;
        }

        Some(read(dir).await.unwrap())
    }
}

/// Creates a new instance of stable storage.
pub async fn build_stable_storage(root_storage_dir: PathBuf) -> Box<dyn StableStorage> {
    return Box::new(SStorage{
        root_storage_dir
    })
}