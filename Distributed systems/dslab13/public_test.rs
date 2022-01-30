#[cfg(test)]
pub(crate) mod tests {
    use async_channel::{unbounded, Sender};
    use ntest::timeout;
    use std::array;
    use std::collections::HashMap;

    use crate::solution::{
        chord_id_advance_by, chord_id_distance, chord_id_max, ChordId, ChordLinkId, ChordNode,
        ChordRoutingState, Internet, CHORD_FINGER_TABLE_MAX_ENTRIES, CHORD_RING_TABLE_MAX_ENTRIES,
    };
    use crate::{
        chord_route_from_node_to_node, wire_all_chord_nodes_given_global_info_from_oracle,
        ChordSystemConfig,
    };
    use executor::{Handler, ModuleRef, System};

    #[tokio::test]
    #[timeout(2000)]
    async fn wiring_full_four_bit_ring_with_no_redundancy_should_have_all_links() {
        env_logger::init();

        // Given:
        let cfg = ChordSystemConfig::new(
            4,
            1,
            array::IntoIter::new([
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
            ]),
        );
        let mut system = System::new().await;
        let (_net_ref, node_refs) = cfg.setup_system(&mut system).await;

        // When:
        wire_all_chord_nodes_given_global_info_from_oracle(node_refs.values(), &cfg.all_nodes)
            .await;

        // Then:
        for &id in cfg.all_nodes.keys() {
            let id_plus = |d| chord_id_advance_by(cfg.ring_bits, &id, d);
            check_chord_node_routing_state(
                &id,
                &node_refs,
                build_chord_node_routing_state(
                    &cfg,
                    // successors
                    [id_plus(&1)],
                    // predecessors
                    [id_plus(&15)],
                    // fingers at subsequent levels
                    [
                        (0, id_plus(&1)),
                        (1, id_plus(&2)),
                        (2, id_plus(&4)),
                        (3, id_plus(&8)),
                    ],
                ),
            )
            .await;
        }
        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(2000)]
    async fn wiring_singleton_max_bit_ring_with_max_redundancy_should_have_no_links() {
        // Given:
        let cfg = ChordSystemConfig::new(
            CHORD_FINGER_TABLE_MAX_ENTRIES,
            CHORD_RING_TABLE_MAX_ENTRIES,
            array::IntoIter::new([(42, 19)]),
        );
        let mut system = System::new().await;
        let (_net_ref, node_refs) = cfg.setup_system(&mut system).await;

        // When:
        wire_all_chord_nodes_given_global_info_from_oracle(node_refs.values(), &cfg.all_nodes)
            .await;

        // Then:
        for &id in cfg.all_nodes.keys() {
            check_chord_node_routing_state(
                &id,
                &node_refs,
                build_chord_node_routing_state(
                    &cfg,
                    // successors
                    [],
                    // predecessors
                    [],
                    // fingers at subsequent levels
                    [],
                ),
            )
            .await;
        }
        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(2000)]
    async fn routing_in_singleton_max_bit_ring_should_only_node_accept_everything() {
        // Given:
        let cfg = ChordSystemConfig::new(
            CHORD_FINGER_TABLE_MAX_ENTRIES,
            CHORD_RING_TABLE_MAX_ENTRIES,
            array::IntoIter::new([(42, 19)]),
        );
        let mut system = System::new().await;
        let (net_ref, _node_refs) = cfg.setup_system(&mut system).await;

        // When and Then:
        check_chord_routing_hops(&cfg, &net_ref, &42, &42, [42]).await;
        check_chord_routing_hops(&cfg, &net_ref, &42, &43, [42]).await;
        check_chord_routing_hops(&cfg, &net_ref, &42, &64, [42]).await;
        check_chord_routing_hops(&cfg, &net_ref, &42, &chord_id_max(cfg.ring_bits), [42]).await;
        check_chord_routing_hops(&cfg, &net_ref, &42, &0, [42]).await;
        system.shutdown().await;
    }

    #[tokio::test]
    #[timeout(2000)]
    async fn routing_in_full_four_bit_ring_with_no_redundancy_should_use_all_links() {
        // Given:
        let cfg = ChordSystemConfig::new(
            4,
            1,
            array::IntoIter::new([
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
            ]),
        );
        let mut system = System::new().await;
        let (net_ref, node_refs) = cfg.setup_system(&mut system).await;
        for &id in node_refs.keys() {
            let node_ref = &node_refs.get(&id).unwrap();
            replace_chord_node_routing_state(
                &node_ref,
                &build_chord_node_routing_state(
                    &cfg,
                    [chord_id_advance_by(cfg.ring_bits, &id, &1)],
                    [chord_id_advance_by(
                        cfg.ring_bits,
                        &id,
                        &chord_id_max(cfg.ring_bits),
                    )],
                    [
                        (0, chord_id_advance_by(cfg.ring_bits, &id, &1)),
                        (1, chord_id_advance_by(cfg.ring_bits, &id, &2)),
                        (2, chord_id_advance_by(cfg.ring_bits, &id, &4)),
                        (3, chord_id_advance_by(cfg.ring_bits, &id, &8)),
                    ],
                ),
            )
            .await;
        }

        // When and Then:
        check_chord_routing_hops(&cfg, &net_ref, &4, &2, [4, 12, 0, 2]).await;
        check_chord_routing_hops(&cfg, &net_ref, &4, &3, [4, 3]).await;
        for &src_id in cfg.all_nodes.keys() {
            for &dst_id in cfg.all_nodes.keys() {
                let num_expected_hops =
                    if &chord_id_advance_by(cfg.ring_bits, &dst_id, &1) == &src_id {
                        // Use the predecessor link.
                        2_usize
                    } else {
                        // Use the successor link or appropriate finger links.
                        chord_id_distance(cfg.ring_bits, &src_id, &dst_id).count_ones() as usize + 1
                    };
                let hops = chord_route_from_node_to_node(&net_ref, &cfg, &src_id, &dst_id).await;
                assert!(
                    hops.len() == num_expected_hops,
                    "routing hops from node {} to node {} do not match: expected {} hops but got {} ({:?})",
                    src_id,
                    dst_id,
                    num_expected_hops,
                    hops.len(),
                    &hops[0..]
                );
            }
        }
        system.shutdown().await;
    }

    pub(crate) fn build_chord_node_routing_state<const REDUNDANCY: usize, const FINGERS: usize>(
        cfg: &ChordSystemConfig,
        succs: [ChordId; REDUNDANCY],
        preds: [ChordId; REDUNDANCY],
        fingers: [(usize, ChordId); FINGERS],
    ) -> ChordRoutingState {
        assert!(cfg.ring_redundancy >= REDUNDANCY);
        let mut succ_table = Vec::with_capacity(cfg.ring_redundancy);
        let mut pred_table = Vec::with_capacity(cfg.ring_redundancy);
        for i in 0..succs.len() {
            assert!(cfg.all_nodes.contains_key(&succs[i]));
            assert!(cfg.all_nodes.contains_key(&preds[i]));
            succ_table.push(Some(ChordLinkId {
                id: succs[i].clone(),
                addr: cfg.all_nodes.get(&succs[i]).unwrap().clone(),
            }));
            pred_table.push(Some(ChordLinkId {
                id: preds[i].clone(),
                addr: cfg.all_nodes.get(&preds[i]).unwrap().clone(),
            }));
        }
        for _i in succs.len()..cfg.ring_redundancy {
            succ_table.push(None);
            pred_table.push(None);
        }
        let mut finger_table = vec![None; cfg.ring_bits];
        for (idx, id) in fingers {
            assert!(cfg.all_nodes.contains_key(&id));
            assert!(idx < cfg.ring_bits);
            assert!(finger_table[idx] == None);
            finger_table[idx] = Some(ChordLinkId {
                id: id.clone(),
                addr: cfg.all_nodes.get(&id).unwrap().clone(),
            });
        }
        ChordRoutingState {
            succ_table: succ_table,
            pred_table: pred_table,
            finger_table: finger_table,
        }
    }

    pub(crate) async fn check_chord_node_routing_state(
        id: &ChordId,
        node_refs: &HashMap<ChordId, ModuleRef<ChordNode>>,
        expected_rs: ChordRoutingState,
    ) {
        let actual_rs = fetch_chord_node_routing_state(node_refs.get(id).unwrap()).await;
        check_chord_node_link_vector_match(
            id,
            "successor table",
            &actual_rs.succ_table,
            &expected_rs.succ_table,
        );
        check_chord_node_link_vector_match(
            id,
            "predecessor table",
            &actual_rs.pred_table,
            &expected_rs.pred_table,
        );
        check_chord_node_link_vector_match(
            id,
            "finger table",
            &actual_rs.finger_table,
            &expected_rs.finger_table,
        );
    }

    fn check_chord_node_link_vector_match(
        id: &ChordId,
        vec_name: &str,
        actual_vec: &Vec<Option<ChordLinkId>>,
        expected_vec: &Vec<Option<ChordLinkId>>,
    ) {
        assert!(
            actual_vec.len() == expected_vec.len(),
            "{}s of node {} do not match: expected length {} but got {}",
            vec_name,
            id,
            expected_vec.len(),
            actual_vec.len()
        );
        for idx in 0..actual_vec.len() {
            assert!(
                actual_vec[idx] == expected_vec[idx],
                "{}s of node {} do not match: at position {} expected {:?} but got {:?}",
                vec_name,
                id,
                idx,
                expected_vec[idx],
                actual_vec[idx]
            );
        }
    }

    struct FechChordNodeRoutingState {
        reply_sender: Sender<ChordRoutingState>,
    }

    #[async_trait::async_trait]
    impl Handler<FechChordNodeRoutingState> for ChordNode {
        async fn handle(&mut self, msg: FechChordNodeRoutingState) {
            msg.reply_sender
                .send(self.fetch_routing_state())
                .await
                .unwrap();
        }
    }

    async fn fetch_chord_node_routing_state(node_ref: &ModuleRef<ChordNode>) -> ChordRoutingState {
        let (reply_tx, reply_rx) = unbounded();
        node_ref
            .send(FechChordNodeRoutingState {
                reply_sender: reply_tx,
            })
            .await;
        reply_rx.recv().await.unwrap()
    }

    struct ReplaceChordNodeRoutingState {
        rs: ChordRoutingState,
        ack_sender: Sender<()>,
    }

    #[async_trait::async_trait]
    impl Handler<ReplaceChordNodeRoutingState> for ChordNode {
        async fn handle(&mut self, msg: ReplaceChordNodeRoutingState) {
            self.replace_routing_state(msg.rs);
            msg.ack_sender.send(()).await.unwrap();
        }
    }

    pub(crate) async fn replace_chord_node_routing_state(
        node_ref: &ModuleRef<ChordNode>,
        rs: &ChordRoutingState,
    ) {
        let (reply_tx, reply_rx) = unbounded();
        node_ref
            .send(ReplaceChordNodeRoutingState {
                rs: rs.clone(),
                ack_sender: reply_tx,
            })
            .await;
        reply_rx.recv().await.unwrap()
    }

    pub(crate) async fn check_chord_routing_hops<const NUM_HOPS: usize>(
        cfg: &ChordSystemConfig,
        net_ref: &ModuleRef<Internet>,
        src_id: &ChordId,
        dst_id: &ChordId,
        expected_hops: [ChordId; NUM_HOPS],
    ) {
        let hops = chord_route_from_node_to_node(net_ref, cfg, src_id, dst_id).await;
        assert!(
            hops.len() == NUM_HOPS,
            "routing hops from node {} to node {} do not match: expected {:?} but got {:?}",
            src_id,
            dst_id,
            &expected_hops[0..],
            &hops[0..]
        );
        assert!(
            hops.iter()
                .zip((&expected_hops[0..]).iter())
                .filter(|&(a, b)| a != b)
                .count()
                == 0,
            "routing hops from node {} to node {} do not match: expected {:?} but got {:?}",
            src_id,
            dst_id,
            &expected_hops[0..],
            &hops[0..]
        );
    }
}
