pub struct Fibonacci {
    // Add here any fields you need.
    num_1: Option<u128>,
    num_2: Option<u128>
}

impl Fibonacci {
    /// Create new `Fibonacci`.
    pub fn new() -> Fibonacci {
        Fibonacci {
            num_1: Some(1),
            num_2: Some(0)
        }
    }

    /// Calculate the n-th Fibonacci number.
    ///
    /// This shall not change the state of the iterator.
    /// The calculations shall wrap around at the boundary of u8.
    /// The calculations might be slow (recursive calculations are acceptable).
    pub fn fibonacci(n: usize) -> u8 {
        match n {
            0 => 0,
            1 => 1,
            _ => Self::fibonacci(n - 1).wrapping_add(Self::fibonacci(n - 2)),
        }
    }
}

impl Iterator for Fibonacci {
    type Item = u128;

    /// Calculate the next Fibonacci number.
    ///
    /// The first call to `next()` shall return the 0th Fibonacci number (i.e., `0`).
    /// The calculations shall not overflow and shall not wrap around. If the result
    /// doesn't fit u128, the sequence shall end (the iterator shall return `None`).
    /// The calculations shall be fast (recursive calculations are **un**acceptable).
    fn next(&mut self) -> Option<Self::Item> {
        let tmp = self.num_2;
        self.num_2 = self.num_1;
        self.num_1 = self.num_1.and_then(|x| x.checked_add(tmp.unwrap()));

        tmp
    }
}
