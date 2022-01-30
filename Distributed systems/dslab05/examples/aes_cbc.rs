use rand::prelude::*;

use aes::Aes128;
use block_modes::block_padding::Pkcs7;
use block_modes::{BlockMode, Cbc};

// Create a type alias:
type Aes128Cbc = Cbc<Aes128, Pkcs7>;

fn encrypt(message: &str, key: &[u8], iv: &[u8]) -> Vec<u8> {
    // Create a new block mode instance from the key and the IV:
    let cipher = Aes128Cbc::new_from_slices(key, iv).unwrap();

    // Encrypt the message:
    cipher.encrypt_vec(message.as_bytes())
}

fn decrypt(encrypted_message: &[u8], key: &[u8], iv: &[u8]) -> String {
    // Create a new block mode instance from the key and the IV:
    let cipher = Aes128Cbc::new_from_slices(key, iv).unwrap();

    // Decrypt the message:
    let decrypted = cipher.decrypt_vec(encrypted_message).unwrap();

    // Convert the decrypted message to String:
    String::from_utf8(decrypted).unwrap()
}

fn main() {
    let message = "AES is fast for large amounts of data";

    // Generate random key and initialization vector:
    let key = rand::thread_rng().gen::<[u8; 16]>();
    let iv = rand::thread_rng().gen::<[u8; 16]>();

    // Encrypt the message:
    let encrypted = encrypt(message, &key, &iv);
    println!("Encrypted data: {:?}", encrypted);

    // Decrypt the message:
    let decrypted = decrypt(&encrypted, &key, &iv);
    println!("Decrypted data: '{}'", decrypted);
}
