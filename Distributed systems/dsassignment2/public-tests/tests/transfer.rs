use assignment_2_solution::{
    deserialize_register_command, serialize_register_command, ClientCommandHeader,
    ClientRegisterCommand, ClientRegisterCommandContent, RegisterCommand, SystemCommandHeader,
    SystemRegisterCommand, SystemRegisterCommandContent,
};
use assignment_2_test_utils::transfer::*;
use ntest::timeout;
use uuid::Uuid;

#[tokio::test]
#[timeout(200)]
async fn serialize_deserialize_is_identity() {
    // given
    let request_identifier = 7;
    let sector_idx = 8;
    let register_cmd = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier,
            sector_idx,
        },
        content: ClientRegisterCommandContent::Read,
    });
    let mut sink: Vec<u8> = Vec::new();

    // when
    serialize_register_command(&register_cmd, &mut sink, &[0x00_u8; 32])
        .await
        .expect("Could not serialize?");
    let mut slice: &[u8] = &sink[..];
    let data_read: &mut (dyn tokio::io::AsyncRead + Send + Unpin) = &mut slice;
    let (deserialized_cmd, hmac_valid) =
        deserialize_register_command(data_read, &[0x00_u8; 64], &[0x00_u8; 32])
            .await
            .expect("Could not deserialize");

    // then
    assert!(hmac_valid);
    match deserialized_cmd {
        RegisterCommand::Client(ClientRegisterCommand {
            header,
            content: ClientRegisterCommandContent::Read,
        }) => {
            assert_eq!(header.sector_idx, sector_idx);
            assert_eq!(header.request_identifier, request_identifier);
        }
        _ => panic!("Expected Read command"),
    }
}

#[tokio::test]
#[timeout(200)]
async fn serialized_read_proc_cmd_has_correct_format() {
    // given
    let read_ident = 1471771_u64;
    let sector_idx = 4525787855454_u64;
    let process_identifier = 147_u8;
    let msg_ident = [7; 16];

    let read_proc_cmd = RegisterCommand::System(SystemRegisterCommand {
        header: SystemCommandHeader {
            process_identifier,
            msg_ident: Uuid::from_slice(&msg_ident).unwrap(),
            sector_idx,
            read_ident,
        },
        content: SystemRegisterCommandContent::ReadProc,
    });
    let mut serialized: Vec<u8> = Vec::new();

    // when
    serialize_register_command(&read_proc_cmd, &mut serialized, &[0x00_u8; 64])
        .await
        .expect("Could not write to vector?");
    serialized.truncate(serialized.len() - 32);

    // then
    assert_eq!(serialized.len(), 40);
    assert_system_cmd_header(
        serialized.as_slice(),
        &msg_ident,
        process_identifier,
        3,
        read_ident,
        sector_idx,
    );
}
