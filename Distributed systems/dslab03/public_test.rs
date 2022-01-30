#[cfg(test)]
mod tests {
    use crate::solution::{FibonacciSystemMessage, FibonacciModule, fib};
    use crossbeam_channel::unbounded;
    use ntest::timeout;

    #[test]
    #[timeout(200)]
    fn fib_ends() {
        fib(10);
    }

    #[test]
    fn create_registers_new_module() {
        let (tx, rx) = unbounded();
        FibonacciModule::create(0, 7, tx);

        assert_eq!(rx.len(), 1);
        match rx.try_recv().unwrap() {
            FibonacciSystemMessage::RegisterModule(_) => {}
            _ => panic!("Creating module resulted in a different message than RegisterModule!"),
        }
    }

}
