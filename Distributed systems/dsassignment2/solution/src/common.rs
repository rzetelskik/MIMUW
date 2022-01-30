use hmac::Hmac;
use sha2::Sha256;

pub(crate) type HmacSha256 = Hmac<Sha256>;

pub(crate) const WORKERS_NUM: u64 = 32;
