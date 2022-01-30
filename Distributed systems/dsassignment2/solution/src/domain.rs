use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use uuid::Uuid;

pub static MAGIC_NUMBER: [u8; 4] = [0x61, 0x74, 0x64, 0x64];

pub struct Configuration {
    /// Hmac key to verify and sign internal requests.
    pub hmac_system_key: [u8; 64],
    /// Hmac key to verify client requests.
    pub hmac_client_key: [u8; 32],
    /// Part of configuration which is safe to share with external world.
    pub public: PublicConfiguration,
}

#[derive(Debug)]
pub struct PublicConfiguration {
    /// Storage for durable data.
    pub storage_dir: PathBuf,
    /// Host and port, indexed by identifiers, of every process, including itself
    /// (subtract 1 from self_rank to obtain index in this array).
    pub tcp_locations: Vec<(String, u16)>,
    /// Identifier of this process. Identifiers start at 1.
    pub self_rank: u8,
    /// First NOT supported sector index.
    pub max_sector: u64,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SectorVec(pub Vec<u8>);

pub type SectorIdx = u64;

#[derive(Debug, Clone)]
pub enum RegisterCommand {
    Client(ClientRegisterCommand),
    System(SystemRegisterCommand),
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
/// Repr u8 macro marks this enum as translatable to a single byte. So `Ok` is 0x0,
/// and consecutive values are consecutive numbers. Use (status_code as u8) syntax.
pub enum StatusCode {
    /// Command completed successfully
    Ok,
    /// Invalid HMAC signature
    AuthFailure,
    /// Sector index is out of range <0, Configuration.max_sector)
    InvalidSectorIndex,
}

#[derive(Debug, Clone)]
pub struct ClientRegisterCommand {
    pub header: ClientCommandHeader,
    pub content: ClientRegisterCommandContent,
}

#[derive(Debug, Clone)]
pub struct SystemRegisterCommand {
    pub header: SystemCommandHeader,
    pub content: SystemRegisterCommandContent,
}

#[derive(Debug, Clone)]
pub enum SystemRegisterCommandContent {
    ReadProc,
    Value {
        timestamp: u64,
        write_rank: u8,
        sector_data: SectorVec,
    },
    WriteProc {
        timestamp: u64,
        write_rank: u8,
        data_to_write: SectorVec,
    },
    Ack,
}

#[derive(Debug, Clone)]
pub enum ClientRegisterCommandContent {
    Read,
    Write { data: SectorVec },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ClientCommandHeader {
    pub request_identifier: u64,
    pub sector_idx: SectorIdx,
}

#[derive(Debug, Clone, Copy)]
pub struct SystemCommandHeader {
    pub process_identifier: u8,
    pub msg_ident: Uuid,
    pub read_ident: u64,
    pub sector_idx: SectorIdx,
}

#[derive(Debug, Clone)]
pub struct OperationComplete {
    pub status_code: StatusCode,
    pub request_identifier: u64,
    pub op_return: OperationReturn,
}

#[derive(Debug, Clone)]
pub enum OperationReturn {
    Read(ReadReturn),
    Write,
}

#[derive(Debug, Clone)]
pub struct ReadReturn {
    pub read_data: Option<SectorVec>,
}
