#[cfg(test)]
mod tests {
    use crate::solution::Threadpool;
    use crossbeam_channel::unbounded;
    use ntest::timeout;
    use std::sync::{Arc};

    #[test]
    #[timeout(200)]
    fn smoke_test() {
        let (tx, rx) = unbounded();
        let pool = Threadpool::new(1);

        pool.submit(Box::new(move || {
            tx.send(14).unwrap();
        }));

        assert_eq!(14, rx.recv().unwrap());
    }


    #[test]
    #[timeout(200)]
    fn threadpool_is_sync() {
        let send_only_when_threadpool_is_sync = Arc::new(Threadpool::new(1));
        let (tx, rx) = unbounded();

        let _handle = std::thread::spawn(move || {
            tx.send(send_only_when_threadpool_is_sync).unwrap();
        });

        rx.recv().unwrap();
    }

    #[test]
    #[timeout(200)]
    fn many_threads() {
        let count = 100;
        let (tx, rx) = unbounded();
        let pool = Threadpool::new(count);

        let shared = Arc::new(tx);

        for i in 0..count {
            let shared_clone = shared.clone();
            pool.submit(Box::new(move || {
                shared_clone.send(i).unwrap();
            }));
        }

        let mut vector = vec![];
        for _ in 0..count {
            vector.push(rx.recv().unwrap());
        }
        vector.sort();
        assert_eq!(vector, (0..count).collect::<Vec<usize>>());
    }

    #[test]
    fn concurrency() {
        fn long_computation() -> u64 {
            let mut k : u64 = 0;
            for i in 0..10000000 {
                k *= i;
            }
            return k;
        }

        fn for_x_threads(count: usize) {
            let (tx, rx) = unbounded();
            let pool = Threadpool::new(count);

            for _ in 0..count {
                pool.submit(Box::new(|| {long_computation();}));
            }
            pool.submit(Box::new(move || {
                tx.send(14).unwrap();
            }));

            assert_eq!(14, rx.recv().unwrap());
        }

        fn measure_for_x_threads(count: usize) -> std::time::Duration {
            let instant = std::time::Instant::now();
            for_x_threads(count);
            instant.elapsed()
        }

        let single_threaded = measure_for_x_threads(1);

        // CRUCIAL!!! : Assuming  n := number of your CPU cores, put n-1 below!
        // (if you can't be sure, 3 should do)
        let multi_threaded = measure_for_x_threads(5); // <-- put n-1 here!

        let factor = 2; // <-- pure heuresis
        println!("Single-threaded time: {:?}\nMulti-threaded time: {:?}", single_threaded, multi_threaded);
        assert!(multi_threaded < single_threaded ||
                multi_threaded - single_threaded < single_threaded / factor,
                "The execution must not have been parallel!")

    }
}
