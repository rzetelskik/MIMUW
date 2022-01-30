use hmac::{Hmac, Mac, NewMac};
use sha2::Sha256;

// Create a type alias:
type HmacSha256 = Hmac<Sha256>;

fn calculate_hmac_tag(message: &str, secret_key: &[u8]) -> [u8; 32] {
    // Initialize a new MAC instance from the secret key:
    let mut mac = HmacSha256::new_from_slice(secret_key).unwrap();

    // Calculate MAC for the data (one can provide it in multiple portions):
    mac.update(message.as_bytes());

    // Finalize the computations of MAC and obtain the resulting tag:
    let tag = mac.finalize().into_bytes();

    tag.into()
}

fn verify_hmac_tag(tag: &[u8], message: &str, secret_key: &[u8]) -> bool {
    // Initialize a new MAC instance from the secret key:
    let mut mac = HmacSha256::new_from_slice(secret_key).unwrap();

    // Calculate MAC for the data (one can provide it in multiple portions):
    mac.update(message.as_bytes());

    // Verify the tag:
    mac.verify(tag).is_ok()
}

fn main() {
    let msg = "Message requiring authorization";
    let secret_key = [1, 2, 3];

    // Generate HMAC tag:
    let tag = calculate_hmac_tag(msg, &secret_key);
    println!("HMAC tag: {:?}", tag);

    // Verify HMAC tag:
    let verified = verify_hmac_tag(&tag, msg, &secret_key);
    println!("Tag is valid for the message: {}", verified);
}
