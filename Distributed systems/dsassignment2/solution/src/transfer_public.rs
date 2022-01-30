use hmac::{Hmac, Mac, NewMac};
use sha2::Sha256;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, Error, ErrorKind};
use uuid::Uuid;

use crate::{
    ClientCommandHeader, ClientRegisterCommand, ClientRegisterCommandContent, MAGIC_NUMBER,
    RegisterCommand, SectorVec, SystemCommandHeader, SystemRegisterCommand, SystemRegisterCommandContent,
};

type HmacSha256 = Hmac<Sha256>;

#[repr(u8)]
pub enum ClientRegisterCommandType {
    Read = 0x01u8,
    Write = 0x02u8,
}

impl Into<ClientRegisterCommandType> for u8 {
    fn into(self) -> ClientRegisterCommandType {
        match self {
            0x01u8 => ClientRegisterCommandType::Read,
            0x02u8 => ClientRegisterCommandType::Write,
            _ => panic!(),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum SystemRegisterCommandType {
    ReadProc = 0x03u8,
    Value = 0x04u8,
    WriteProc = 0x05u8,
    Ack = 0x06u8,
}

impl Into<SystemRegisterCommandType> for u8 {
    fn into(self) -> SystemRegisterCommandType {
        match self {
            0x03u8 => SystemRegisterCommandType::ReadProc,
            0x04u8 => SystemRegisterCommandType::Value,
            0x05u8 => SystemRegisterCommandType::WriteProc,
            0x06u8 => SystemRegisterCommandType::Ack,
            _ => panic!(),
        }
    }
}

pub fn unpack_client_command_content(
    content: &ClientRegisterCommandContent,
) -> (ClientRegisterCommandType, Option<&SectorVec>) {
    match content {
        ClientRegisterCommandContent::Read => (ClientRegisterCommandType::Read, None),
        ClientRegisterCommandContent::Write { data } => {
            (ClientRegisterCommandType::Write, Some(data))
        }
    }
}

pub fn unpack_system_command_content(
    content: &SystemRegisterCommandContent,
) -> (SystemRegisterCommandType, Option<(&u64, &u8, &SectorVec)>) {
    match content {
        SystemRegisterCommandContent::ReadProc => (SystemRegisterCommandType::ReadProc, None),
        SystemRegisterCommandContent::Value {
            timestamp,
            write_rank,
            sector_data,
        } => (
            SystemRegisterCommandType::Value,
            Some((timestamp, write_rank, sector_data)),
        ),
        SystemRegisterCommandContent::WriteProc {
            timestamp,
            write_rank,
            data_to_write,
        } => (
            SystemRegisterCommandType::WriteProc,
            Some((timestamp, write_rank, data_to_write)),
        ),
        SystemRegisterCommandContent::Ack => (SystemRegisterCommandType::Ack, None),
    }
}

pub async fn deserialize_register_command(
    data: &mut (dyn AsyncRead + Send + Unpin),
    hmac_system_key: &[u8; 64],
    hmac_client_key: &[u8; 32],
) -> Result<(RegisterCommand, bool), Error> {
    let mut buf = [0u8; 8];
    data.read_exact(&mut buf[0..4]).await?;

    while !buf[0..4].eq(&MAGIC_NUMBER) {
        buf[0..4].rotate_left(1);
        data.read_exact(&mut buf[3..4]).await?;
    }

    data.read_exact(&mut buf[4..8]).await?;
    let msg_type = buf[7];

    match msg_type {
        0x01u8..=0x02u8 => {
            let mut mac = HmacSha256::new_from_slice(hmac_client_key).unwrap();
            mac.update(&buf);

            data.read_exact(&mut buf).await?;
            mac.update(&buf);
            let request_identifier: u64 = u64::from_be_bytes(buf);

            data.read_exact(&mut buf).await?;
            mac.update(&buf);
            let sector_idx: u64 = u64::from_be_bytes(buf);

            let msg_type: ClientRegisterCommandType = msg_type.into();
            let content = match msg_type {
                ClientRegisterCommandType::Read => ClientRegisterCommandContent::Read,
                ClientRegisterCommandType::Write => {
                    let mut buf: [u8; 4096] = [0; 4096];
                    data.read_exact(&mut buf).await?;
                    mac.update(&buf);

                    ClientRegisterCommandContent::Write {
                        data: SectorVec(buf.to_vec()),
                    }
                }
            };

            let mut tag: [u8; 32] = [0; 32];
            data.read_exact(&mut tag).await?;

            Ok((
                RegisterCommand::Client(ClientRegisterCommand {
                    header: ClientCommandHeader {
                        request_identifier,
                        sector_idx,
                    },
                    content,
                }),
                mac.verify(&tag).is_ok(),
            ))
        }
        0x03u8..=0x06u8 => {
            let mut mac = HmacSha256::new_from_slice(hmac_system_key).unwrap();
            mac.update(&buf);

            let process_identifier = buf[6];

            let mut buf = [0u8; 16];
            data.read_exact(&mut buf).await?;
            mac.update(&buf);
            let msg_ident = Uuid::from_slice(&buf);

            let mut buf = [0u8; 8];
            data.read_exact(&mut buf).await?;
            mac.update(&buf);
            let read_ident = u64::from_be_bytes(buf);

            data.read_exact(&mut buf).await?;
            mac.update(&buf);
            let sector_idx = u64::from_be_bytes(buf);

            let msg_type: SystemRegisterCommandType = msg_type.into();
            let content = match msg_type {
                SystemRegisterCommandType::ReadProc => SystemRegisterCommandContent::ReadProc,
                SystemRegisterCommandType::Ack => SystemRegisterCommandContent::Ack,
                SystemRegisterCommandType::Value | SystemRegisterCommandType::WriteProc => {
                    data.read_exact(&mut buf).await?;
                    mac.update(&buf);
                    let timestamp = u64::from_be_bytes(buf);

                    data.read_exact(&mut buf).await?;
                    mac.update(&buf);
                    let write_rank = buf[7];

                    let mut buf: [u8; 4096] = [0; 4096];
                    data.read_exact(&mut buf).await?;
                    mac.update(&buf);

                    match msg_type {
                        SystemRegisterCommandType::Value => {
                            SystemRegisterCommandContent::Value {
                                timestamp,
                                write_rank,
                                sector_data: SectorVec(buf.to_vec()),
                            }
                        }
                        SystemRegisterCommandType::WriteProc => {
                            SystemRegisterCommandContent::WriteProc {
                                timestamp,
                                write_rank,
                                data_to_write: SectorVec(buf.to_vec()),
                            }
                        }
                        _ => panic!(),
                    }
                }
            };

            let mut tag: [u8; 32] = [0; 32];
            data.read_exact(&mut tag).await?;

            Ok((
                RegisterCommand::System(SystemRegisterCommand {
                    header: SystemCommandHeader {
                        process_identifier,
                        msg_ident: msg_ident.unwrap(),
                        read_ident,
                        sector_idx,
                    },
                    content,
                }),
                mac.verify(&tag).is_ok(),
            ))
        }
        _ => Err(Error::new(
            ErrorKind::InvalidData,
            "unsupported message type",
        )),
    }
}

pub async fn serialize_register_command(
    cmd: &RegisterCommand,
    writer: &mut (dyn AsyncWrite + Send + Unpin),
    hmac_key: &[u8],
) -> Result<(), Error> {
    let mut buffer = Vec::<u8>::new();

    buffer.write(&MAGIC_NUMBER).await?;

    match cmd {
        RegisterCommand::Client(cmd) => {
            let (msg_type, content) = unpack_client_command_content(&cmd.content);

            buffer.write(&mut ([0u8; 3] as [u8; 3])).await?;
            buffer.write(&(msg_type as u8).to_be_bytes()).await?;
            buffer
                .write(&cmd.header.request_identifier.to_be_bytes())
                .await?;
            buffer.write(&cmd.header.sector_idx.to_be_bytes()).await?;

            if let Some(SectorVec(data)) = content {
                buffer.write(&data).await?;
            }
        }
        RegisterCommand::System(cmd) => {
            let (msg_type, content) = unpack_system_command_content(&cmd.content);

            buffer.write(&0u16.to_be_bytes()).await?;
            buffer
                .write(&cmd.header.process_identifier.to_be_bytes())
                .await?;
            buffer.write(&(msg_type as u8).to_be_bytes()).await?;
            buffer.write(cmd.header.msg_ident.as_bytes()).await?;
            buffer.write(&cmd.header.read_ident.to_be_bytes()).await?;
            buffer.write(&cmd.header.sector_idx.to_be_bytes()).await?;

            if let Some((timestamp, write_rank, SectorVec(data))) = content {
                buffer.write(&timestamp.to_be_bytes()).await?;
                buffer.write(&mut ([0u8; 7] as [u8; 7])).await?;
                buffer.write(&write_rank.to_be_bytes()).await?;
                buffer.write(&data).await?;
            }
        }
    }

    let mut mac = HmacSha256::new_from_slice(hmac_key).unwrap();
    mac.update(&buffer);
    let tag: [u8; 32] = mac.finalize().into_bytes().into();

    buffer.write(&tag).await?;

    writer.write_all(&buffer).await
}
