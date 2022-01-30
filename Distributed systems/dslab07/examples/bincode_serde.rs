use serde::{Deserialize, Serialize};

// Serde provides the `#[derive(Serialize, Deserialize)]` macros to
// automatically generate implementations of the `Serialize` and `Deserialize`
// traits for custom data structures. To use it, you need to add
// `features = ["derive"]`  to the Serde dependency in Cargo.toml:
#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Entity {
    x: f32,
    y: f32,
}

fn main() {
    let entity = Entity { x: 1.0, y: -11.0 };

    // Serialize the object:
    let serialized: Vec<u8> = bincode::serialize(&entity).unwrap();
    println!("Serialized entity: {:?}", serialized);

    // Deserialize the object:
    let deserialized: Entity = bincode::deserialize(&serialized[..]).unwrap();
    assert_eq!(entity, deserialized);
}
