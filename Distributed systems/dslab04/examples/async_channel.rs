use async_channel::unbounded;

#[tokio::main]
async fn main() {
    let (tx, rx) = unbounded();
    let tx_cloned = tx.clone();

    tokio::spawn(async move {
        // Send a message asynchronously
        // (it waits when the channel is full):
        tx_cloned.send(7).await.unwrap();
    });

    tokio::spawn(async move {
        // Send a message synchronously
        // (it returns an error when the channel is full):
        tx.try_send(8).unwrap();
    });

    // Receive the messages:
    println!("Received: {}", rx.recv().await.unwrap());
    println!("Received: {}", rx.recv().await.unwrap());
}
