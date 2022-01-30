// Put the macro on top of trait definition:
#[async_trait::async_trait]
trait FutureInt {
    async fn get(&self) -> i32;
}

struct FutureInt5 {}

// Put the macro on top of every trait implementation:
#[async_trait::async_trait]
impl FutureInt for FutureInt5 {
    async fn get(&self) -> i32 {
        5
    }
}

#[tokio::main]
async fn main() {
    println!(
        "Async trait: {}",
        FutureInt5 {}.get().await
    );
}
