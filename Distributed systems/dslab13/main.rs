mod public_test;
mod solution;

use std::array::IntoIter;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use async_channel::{unbounded, Sender};

use executor::{Handler, ModuleRef, System};

use crate::solution::{
    chord_id_max, chord_id_min, ChordAddr, ChordId, ChordMessage, ChordNode, Internet,
    InternetMessage, CHORD_FINGER_TABLE_MAX_ENTRIES, CHORD_RING_TABLE_MAX_ENTRIES,
};

pub(crate) struct ChordSystemConfig {
    pub(crate) ring_bits: usize,
    pub(crate) ring_redundancy: usize,
    pub(crate) all_nodes: Arc<BTreeMap<ChordId, ChordAddr>>,
    _private: (),
}

#[tokio::main]
async fn main() {
    // The system is a ring with 4-bit identifiers
    // in which all 16 nodes are active. There is
    // only one successor and predecessor link
    // and all four finger links per node.
    let config = ChordSystemConfig::new(
        4, // Chord identifiers have four bits
        1, // only one successor and predecessor per node
        IntoIter::new([
            // all nodes' (Chord identifiers, transport-layer addresses)
            (0, 100),
            (1, 101),
            (2, 102),
            (3, 103),
            (4, 104),
            (5, 105),
            (6, 106),
            (7, 107),
            (8, 108),
            (9, 109),
            (10, 110),
            (11, 111),
            (12, 112),
            (13, 113),
            (14, 114),
            (15, 115),
        ]), // this actually fills the entire 2^4 identifier space
    );
    let (src_id, dst_id) = parse_command_line_args(&config);
    let mut system = System::new().await;
    let (net_ref, node_refs) = config.setup_system(&mut system).await;
    // Wire all nodes in a Chord ring (i.e., fill
    // in their routing states based on global
    // information from an oracle: the system config).
    wire_all_chord_nodes_given_global_info_from_oracle(node_refs.values(), &config.all_nodes).await;
    println!(
        "In a Chord ring with {}-bit identifiers and {} successor/predecessor links,",
        config.ring_bits, config.ring_redundancy
    );
    println!(
        "routing a message from node {} to node {} yields the following hops:",
        src_id, dst_id
    );
    // Perform routing between the pair of nodes
    // given as command-line arguments.
    let hops = chord_route_from_node_to_node(&net_ref, &config, &src_id, &dst_id).await;
    let mut not_first = false;
    for &hop in &hops {
        if not_first {
            print!(" -> ");
        } else {
            not_first = true;
        }
        print!("{}", hop);
    }
    println!();
    system.shutdown().await;
}

impl ChordSystemConfig {
    pub(crate) fn new<I>(
        ring_bits: usize,
        ring_redundancy: usize,
        all_node_iter: I,
    ) -> ChordSystemConfig
    where
        I: Iterator<Item = (ChordId, ChordAddr)>,
    {
        assert!(ring_bits >= 1);
        assert!(ring_bits <= CHORD_FINGER_TABLE_MAX_ENTRIES);
        assert!(ring_redundancy >= 1);
        assert!(ring_redundancy <= CHORD_RING_TABLE_MAX_ENTRIES);
        let res = ChordSystemConfig {
            ring_bits,
            ring_redundancy,
            all_nodes: Arc::new(all_node_iter.collect::<BTreeMap<_, _>>()),
            _private: (),
        };
        assert!(
            res.all_nodes
                .iter()
                .filter(|(&k, &_v)| { k < chord_id_min(ring_bits) || k > chord_id_max(ring_bits) })
                .count()
                == 0
        );
        res
    }

    pub(crate) async fn setup_system(
        &self,
        system: &mut System,
    ) -> (ModuleRef<Internet>, HashMap<ChordId, ModuleRef<ChordNode>>) {
        let net_ref = Internet::register(system).await;
        let mut node_refs = HashMap::with_capacity(self.all_nodes.len());
        for (&id, &addr) in self.all_nodes.iter() {
            let node_ref = ChordNode::register(
                system,
                net_ref.clone(),
                self.ring_bits,
                self.ring_redundancy,
                &id,
                &addr,
            )
            .await;
            connect_chord_node_to_internet(&net_ref, &node_ref, &addr).await;
            node_refs.insert(id, node_ref);
        }
        (net_ref, node_refs)
    }
}

async fn connect_chord_node_to_internet(
    net_ref: &ModuleRef<Internet>,
    node_ref: &ModuleRef<ChordNode>,
    addr: &ChordAddr,
) {
    let (node_online_tx, node_online_rx) = unbounded();
    net_ref
        .send(ConnectNodeToInternetMessage {
            addr: *addr,
            node_ref: node_ref.clone(),
            conn_notifier: node_online_tx,
        })
        .await;
    node_online_rx.recv().await.unwrap();
}

struct ConnectNodeToInternetMessage {
    addr: ChordAddr,
    node_ref: ModuleRef<ChordNode>,
    conn_notifier: Sender<bool>,
}

#[async_trait::async_trait]
impl Handler<ConnectNodeToInternetMessage> for Internet {
    async fn handle(&mut self, msg: ConnectNodeToInternetMessage) {
        self.connect_node(&msg.addr, &msg.node_ref).await;
        msg.conn_notifier.send(true).await.unwrap();
    }
}

pub(crate) async fn wire_all_chord_nodes_given_global_info_from_oracle<'a, I>(
    node_refs: I,
    all_nodes: &Arc<BTreeMap<ChordId, ChordAddr>>,
) where
    I: Iterator<Item = &'a ModuleRef<ChordNode>> + Clone,
{
    let (ack_tx, ack_rx) = unbounded();
    for node_ref in node_refs.clone() {
        node_ref
            .send(WireChordNodeGivenGlobalInformationFromOracle {
                all_nodes: all_nodes.clone(),
                ack_notifier: ack_tx.clone(),
            })
            .await;
    }
    for _node_ref in node_refs {
        ack_rx.recv().await.unwrap();
    }
}

struct WireChordNodeGivenGlobalInformationFromOracle {
    all_nodes: Arc<BTreeMap<ChordId, ChordAddr>>,
    ack_notifier: Sender<bool>,
}

#[async_trait::async_trait]
impl Handler<WireChordNodeGivenGlobalInformationFromOracle> for ChordNode {
    async fn handle(&mut self, msg: WireChordNodeGivenGlobalInformationFromOracle) {
        self.recreate_links_from_oracle(&*msg.all_nodes);
        msg.ack_notifier.send(true).await.unwrap();
    }
}

pub(crate) async fn chord_route_from_node_to_node(
    net_ref: &ModuleRef<Internet>,
    cfg: &ChordSystemConfig,
    src_id: &ChordId,
    dst_id: &ChordId,
) -> Vec<ChordId> {
    assert!(cfg.all_nodes.contains_key(src_id));
    let (result_tx, result_rx) = unbounded();
    let node_addr = cfg.all_nodes.get(src_id).unwrap();
    net_ref
        .send(InternetMessage::new(
            node_addr,
            node_addr,
            ChordMessage::new(dst_id, result_tx),
        ))
        .await;
    result_rx.recv().await.unwrap()
}

fn parse_command_line_args(cfg: &ChordSystemConfig) -> (ChordId, ChordId) {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        println!("ERROR: Two command-line arguments required: <src_node_id> <dst_node_id>!");
        std::process::exit(1);
    }
    let src_id = parse_chord_id_or_exit(args.get(1).unwrap(), "<src_node_id", cfg);
    let dst_id = parse_chord_id_or_exit(args.get(2).unwrap(), "<dst_node_id", cfg);
    (src_id, dst_id)
}

fn parse_chord_id_or_exit(s: &str, name: &str, cfg: &ChordSystemConfig) -> ChordId {
    match s.parse::<ChordId>() {
        Ok(id) => {
            if id < chord_id_min(cfg.ring_bits) || id > chord_id_max(cfg.ring_bits) {
                println!("ERROR: The value of {} is out of range!", name);
                println!(
                    "       It must be a number in [{}..{}]!",
                    chord_id_min(cfg.ring_bits),
                    chord_id_max(cfg.ring_bits)
                );
                std::process::exit(1);
            } else {
                id
            }
        }
        Err(_) => {
            println!("ERROR: The value of {} is invalid!", name);
            println!(
                "       It must be a number in [{}..{}]!",
                chord_id_min(cfg.ring_bits),
                chord_id_max(cfg.ring_bits)
            );
            std::process::exit(1);
        }
    }
}
