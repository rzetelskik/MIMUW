use crossbeam_channel::{unbounded, Sender};

// The message, Ball, is just a single String
// as it suffices for the simple system:
type Ball = String;

// The PingPong module is implemented as a struct:
struct PingPong {
    /// Name of the module (here "ping" or "pong").
    name: &'static str,
    /// Number of messages received by this module.
    received_messages: usize,
    /// Reference to the queue of the other module's executor.
    other_queue: Sender<Ball>,
}

impl PingPong {
    /// Create new PingPong module wit the provided name.
    fn new(name: &'static str, other_queue: Sender<Ball>) -> Self {
        PingPong {
            name,
            received_messages: 0,
            other_queue,
        }
    }

    /// Handle the received message, Ball, replying with a new Ball.
    fn handler(&mut self, msg: Ball) {
        self.received_messages += 1;
        println!(
            "The {} receives a {}-ball. It has received {} message(s) so far.",
            self.name, msg, self.received_messages
        );

        self.other_queue.send(Ball::from(self.name)).unwrap();
    }
}


// This example presents a simplified executor system with two PingPong modules
// exchanging Ball messages.
//
// The executor system is hugely simplified: there is only one executor (the
// system is single-threaded), it supports only one module type (PingPong) and
// only one simple message type (Ball), the modules are registered statically,
// it assumes there are exactly two modules and they have particular names, it
// runs only for a specified number or steps, etc.
fn main() {
    // Create a channel. It will be the queue:
    let (tx, rx) = unbounded();

    // Create the modules:
    let mut ping = PingPong::new("ping", tx.clone());
    let mut pong = PingPong::new("pong", tx.clone());

    // Send the initial Ball:
    tx.send(Ball::from("ping")).unwrap();

    // Run the executor for 5 steps:
    for _ in 0..5 {
        let msg = rx.recv().unwrap();
        match msg.as_str() {
            // Message "ping" comes from the "ping" module
            // and targets the "pong" module:
            "ping" => pong.handler(msg),

            // Message "pong" comes from the "pong" module
            // and targets the "ping" module:
            "pong" => ping.handler(msg),

            _ => panic!("Unsupported Ball({})!", msg),
        };
    }
}
