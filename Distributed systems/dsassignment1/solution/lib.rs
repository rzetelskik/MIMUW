    use std::time::Duration;
use async_channel::{unbounded, Sender, Receiver};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

pub trait Message: Send + 'static {}
impl<T: Send + 'static> Message for T {}

/// A trait for modules capable of handling messages of type `M`.
#[async_trait::async_trait]
pub trait Handler<M: Message>
where
    M: Message,
{
    /// Handles the message.
    async fn handle(&mut self, msg: M);
}

#[async_trait::async_trait]
trait Handlee<T>: Send + 'static
where
    T: Send,
{
    async fn get_handled(self: Box<Self>, module: &mut T);
}

#[async_trait::async_trait]
impl<M, T> Handlee<T> for M
where
    T: Handler<M> + Send,
    M: Message,
{
    async fn get_handled(self: Box<Self>, module: &mut T) {
        module.handle(*self).await
    }
}

#[async_trait::async_trait]
trait Closeable
{
    fn close(&self) -> bool;
}

#[async_trait::async_trait]
impl<T> Closeable for Receiver<T> {
    fn close(&self) -> bool {
        self.close()
    }
}


/// The message sent as a result of calling `System::request_tick()`.
#[derive(Debug, Clone)]
pub struct Tick {}

// You can add fields to this struct.
pub struct System {
    finish: Arc<AtomicBool>,
    handles: Vec<tokio::task::JoinHandle<()>>,
    tick_handles: Vec<tokio::task::JoinHandle<()>>,
    rxs: Vec<Box<dyn Closeable>>
}

impl System {
    /// Schedules a `Tick` message to be sent to the given module periodically
    /// with the given interval. The first tick is sent immediately.
    pub async fn request_tick<T: Handler<Tick> + Send>(
        &mut self,
        requester: &ModuleRef<T>,
        delay: Duration,
    ) {
        if self.finish.load(Ordering::Relaxed) {
            panic!();
        }

        let requester_cloned = requester.clone();
        let finish_cloned = self.finish.clone();

        let handle = tokio::spawn(async move {
            let mut interval = tokio::time::interval(delay);

            loop {
                interval.tick().await;
                if finish_cloned.load(Ordering::Relaxed) {
                    break;
                }
                
                requester_cloned.send(Tick{}).await;
            };
        });

        self.tick_handles.push(handle);
    }

    /// Registers the module in the system.
    /// Returns a `ModuleRef`, which can be used then to send messages to the module.
    pub async fn register_module<T: Send + 'static>(&mut self, module: T) -> ModuleRef<T> {
        if self.finish.load(Ordering::Relaxed) {
            panic!();
        }

        let (tx, rx): (Sender<Box<dyn Handlee<T>>>, Receiver<Box<dyn Handlee<T>>>) = unbounded();
        let rx_cloned = rx.clone();
        let finish_cloned = self.finish.clone();

        let mut mut_module = module;
        let handle = tokio::spawn(async move {
            while !finish_cloned.load(Ordering::Relaxed) {
                match rx_cloned.recv().await {
                    Ok(msg) => {
                        if finish_cloned.load(Ordering::Relaxed) {
                            break;
                        }
                        msg.get_handled(&mut mut_module).await;
                    }
                    Err(_) => {
                        break;
                    }
                }
            }
        });

        self.handles.push(handle);
        self.rxs.push(Box::new(rx));

        ModuleRef{
            tx
        }
    }

    /// Creates and starts a new instance of the system.
    pub async fn new() -> Self {
        System{
            finish: Arc::new(AtomicBool::new(false)),
            handles: Vec::new(),
            tick_handles: Vec::new(),
            rxs: Vec::new(),
        }
    }

    /// Gracefully shuts the system down.
    pub async fn shutdown(&mut self) {
        if self.finish.load(Ordering::Relaxed) {
            panic!();
        }

        self.finish.store(true, Ordering::Relaxed);

        for rx in self.rxs.iter_mut() {
            rx.close();
        }

        for handle in self.tick_handles.iter_mut() {
            let _ = handle.await;
        }

        for handle in self.handles.iter_mut() {
            let _ = handle.await;
        }
    }
}

/// A reference to a module used for sending messages.
// You can add fields to this struct.
pub struct ModuleRef<T: Send + 'static> {
    tx: Sender<Box<dyn Handlee<T>>>,
}

impl<T: Send> ModuleRef<T> {
    /// Sends the message to the module.
    pub async fn send<M: Message>(&self, msg: M)
    where
        T: Handler<M>,
    {
        let _ = self.tx.send(Box::new(msg)).await;
    }
}

impl<T: Send> Clone for ModuleRef<T> {
    /// Creates a new reference to the same module.
    fn clone(&self) -> Self {
        ModuleRef{
            tx: self.tx.clone(),
        }
    }
}
