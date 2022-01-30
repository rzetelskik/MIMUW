// A custom struct:
struct Droppable {
    name: &'static str,
}

// A custom implementation of the Drop trait for the struct:
impl Drop for Droppable {
    // Rust calls automatically `drop()` for each field of a struct. A custom
    // implementation of the Drop trait needs only to dealocacte resources
    // introduced by the struct. Hence this `drop()` implementation does not
    // actually deallocate anything:
    fn drop(&mut self) {
        println!("> Dropping {}", self.name);
    }
}

fn custom_drop_example() {
    let _droppable = Droppable { name: "test value" };

    // Won't compile as Rust does not allow explicit calls to drop:
    // _droppable.drop();
} // `Droppable::drop()` is called automatically here.

fn file_drop_example() {
    // Open a directory:
    let dir_path = std::env::current_dir().unwrap();

    // Get files present in the directory:
    let dir_filepaths = std::fs::read_dir(dir_path).unwrap();

    // Open each file:
    for filepath in dir_filepaths {
        let _file = std::fs::File::open(filepath.unwrap().path()).unwrap();
        // There is no `close()` in Rust.
    } // Dropping `_file` closes the opened file.
}

fn main() {
    custom_drop_example();
    file_drop_example();
}
