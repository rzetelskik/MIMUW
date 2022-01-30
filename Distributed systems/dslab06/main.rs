mod public_test;
mod solution;

use tempfile::tempdir;

use crate::solution::build_stable_storage;


#[tokio::main]
async fn main() {
    let root_storage_dir = tempdir().unwrap();
    let build_storage = || build_stable_storage(root_storage_dir.path().to_path_buf());

    {
        let mut storage = build_storage().await;
        storage.put("key", "value".as_bytes()).await.unwrap();
    } // "crash"

    {
        let storage = build_storage().await;
        let value = String::from_utf8(storage.get("key").await.unwrap()).unwrap();
        println!("Recovered value: '{}'", value);
    }
}
