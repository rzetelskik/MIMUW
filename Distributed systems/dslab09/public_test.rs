mod tests {
    use async_channel::unbounded;
    use ntest::timeout;

    use crate::solution::{
        register_node, register_store, CyberStore2047, Node, Product, ProductType, Transaction,
        TransactionMessage, TwoPhaseResult,
    };
    use executor::System;
    use uuid::Uuid;

    #[tokio::test]
    #[timeout(300)]
    async fn transaction_with_two_nodes_completes() {
        // Given:
        let mut system = System::new().await;
        let (transaction_done_tx, transaction_done_rx) = unbounded();
        let products = vec![Product {
            identifier: Uuid::new_v4(),
            pr_type: ProductType::Electronics,
            price: 180,
        }];
        let node0 = register_node(&mut system, Node::new(products.clone())).await;
        let node1 = register_node(&mut system, Node::new(products)).await;
        let cyber_store =
            register_store(&mut system, CyberStore2047::new(vec![node0, node1])).await;

        // When:
        cyber_store
            .send(TransactionMessage {
                transaction: Transaction {
                    pr_type: ProductType::Electronics,
                    shift: -50,
                },
                completed_callback: Box::new(|result| {
                    Box::pin(async move {
                        transaction_done_tx.send(result).await.unwrap();
                    })
                }),
            })
            .await;

        // Then:
        assert_eq!(
            TwoPhaseResult::Ok,
            transaction_done_rx.recv().await.unwrap()
        );
        system.shutdown().await;
    }
}
