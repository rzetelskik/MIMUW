use rand::rngs::OsRng;
use rsa::{PaddingScheme, PublicKey, RsaPrivateKey, RsaPublicKey};

fn encrypt(message: &str, public_key: &RsaPublicKey) -> Vec<u8> {
    // Use the PKCS1v15 scheme:
    let padding = PaddingScheme::new_pkcs1v15_encrypt();

    // Encrypt the message:
    public_key
        .encrypt(&mut OsRng, padding, message.as_bytes())
        .unwrap()
}

fn decrypt(encrypted_message: &[u8], private_key: &RsaPrivateKey) -> String {
    // Use the PKCS1v15 scheme:
    let padding = PaddingScheme::new_pkcs1v15_encrypt();

    // Decrypt the message:
    let decrypted = private_key.decrypt(padding, encrypted_message).unwrap();

    // Convert the decrypted message to String:
    String::from_utf8(decrypted).unwrap()
}

fn main() {
    let message = "Secret message";

    // Generate a new 256-bit private key (the example uses a short key
    // as it is faster to generate, but for actual security applications
    // longer keys are recommend):
    let private_key = RsaPrivateKey::new(&mut OsRng, 256).unwrap();

    // Derive a public key from the private key:
    let public_key = RsaPublicKey::from(&private_key);

    // Encrypt the message (the public key is used):
    let encrypted = encrypt(message, &public_key);
    println!("Encrypted data: {:?}", encrypted);

    // Decrypt the message (the private key is used):
    let decrypted = decrypt(&encrypted, &private_key);
    println!("Decrypted data: '{}'", decrypted);
}
