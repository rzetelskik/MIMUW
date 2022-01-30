use assignment_2_solution::MAGIC_NUMBER;
use std::convert::TryInto;

pub fn assert_system_cmd_header(
    serialized: &[u8],
    msg_ident: &[u8; 16],
    process_identifier: u8,
    msg_type: u8,
    read_ident: u64,
    sector_idx: u64,
) {
    assert_eq!(&serialized[0..4], MAGIC_NUMBER.as_ref());
    assert_eq!(*serialized.get(6).unwrap(), process_identifier);
    assert_eq!(*serialized.get(7).unwrap(), msg_type);
    assert_eq!(&serialized[8..24], msg_ident);
    assert_eq!(
        u64::from_be_bytes(serialized[24..32].try_into().unwrap()),
        read_ident
    );
    assert_eq!(
        u64::from_be_bytes(serialized[32..40].try_into().unwrap()),
        sector_idx
    );
}
