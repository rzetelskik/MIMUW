use std::sync::{Arc, Condvar, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::JoinHandle;
use std::collections::VecDeque;
use std::ops::{Deref, DerefMut};

type Task = Box<dyn FnOnce() + Send>;

// You can define new types (e.g., structs) if you need.
// However, they shall not be public (i.e., do not use the `pub` keyword).

/// The thread pool.
pub struct Threadpool {
    // Add here any fields you need.
    // We suggest storing handles of the worker threads, submitted tasks,
    // and an information whether the pool is running or it is to be finished.
    handles: Option<Vec<JoinHandle<()>>>,
    queue: Arc<(Mutex<VecDeque<Task>>, Condvar)>,
    finish: Arc<AtomicBool>
}

impl Threadpool {
    /// Create new thread pool with `workers_count` workers.
    pub fn new(workers_count: usize) -> Self {
        let queue: Arc<(Mutex<VecDeque<Task>>, Condvar)> = Arc::new((Mutex::new(VecDeque::new()), Condvar::new()));
        let mut handles: Vec<JoinHandle<()>> = Vec::with_capacity(workers_count);
        let finish: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));

        for _ in 0..workers_count {
            let qc = queue.clone();
            let fc = finish.clone();
            let h: JoinHandle<()> = std::thread::spawn(move || Self::worker_loop(qc, fc));  
            handles.push(h);         
        }
 
        Self {
            handles: Some(handles),
            queue,
            finish
        }
    }

    /// Submit a new task.
    pub fn submit(&self, task: Task) {
        if self.finish.load(Ordering::Relaxed) {
            return
        }

        let (lock, cond) = self.queue.deref();
        let mut guard = lock.lock().unwrap();

        guard.deref_mut().push_back(task);
        cond.notify_all();
    }

    // We suggest extracting the implementation of the worker to an associated
    // function, like this one (however, it is not a part of the public
    // interface, so you can delete it if you implement it differently):
    fn worker_loop(queue: Arc<(Mutex<VecDeque<Task>>, Condvar)>, finish: Arc<AtomicBool>) {
        loop {
            let (lock, cond) = queue.deref();
            let mut guard = lock.lock().unwrap();
            
            while !finish.load(Ordering::Relaxed) && guard.deref().is_empty(){
                guard = cond.wait(guard).unwrap();
            }

            if guard.deref().is_empty() {
                return
            }

            let task: Task = guard.deref_mut().pop_front().unwrap();
            std::mem::drop(guard);

            task();
        }
    }
}

impl Drop for Threadpool {
    /// Gracefully end the thread pool.
    ///
    /// It waits until all submitted tasks are executed,
    /// and until all threads are joined.
    fn drop(&mut self) {
        self.finish.store(true, Ordering::Relaxed);
        let (_, cond) = self.queue.deref();
        cond.notify_all();

        for h in self.handles.take().unwrap() {
            h.join().unwrap();
        }
    }
}
