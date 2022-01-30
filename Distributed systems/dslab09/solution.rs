use std::future::Future;
use std::pin::Pin;

use async_channel::Sender;
use executor::{Handler, ModuleRef, System};
use uuid::Uuid;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub(crate) enum ProductType {
    Electronics,
    Toys,
    Books,
}

#[derive(Clone)]
pub(crate) struct StoreMsg {
    sender: ModuleRef<CyberStore2047>,
    content: StoreMsgContent,
}

#[derive(Clone, Debug)]
pub(crate) enum StoreMsgContent {
    /// Transaction Manager initiates voting for the transaction.
    RequestVote(Transaction),
    /// If every process is ok with transaction, TM issues commit.
    Commit,
    /// System-wide abort.
    Abort,
}

#[derive(Clone)]
pub(crate) struct NodeMsg {
    sender: ModuleRef<Node>,
    content: NodeMsgContent,
}

#[derive(Clone, Debug)]
pub(crate) enum NodeMsgContent {
    /// Process replies to TM whether it can/cannot commit the transaction.
    RequestVoteResponse(TwoPhaseResult),
    /// Process acknowledges to TM committing/aborting the transaction.
    FinalizationAck,
}

pub(crate) struct TransactionMessage {
    /// Request to change price.
    pub(crate) transaction: Transaction,

    /// Called after 2PC completes (i.e., the transaction was decided to be
    /// committed/aborted by CyberStore2047). This must be called after responses
    /// from all processes acknowledging commit or abort are collected.
    pub(crate) completed_callback:
    Box<dyn FnOnce(TwoPhaseResult) -> Pin<Box<dyn Future<Output=()> + Send>> + Send>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum TwoPhaseResult {
    Ok,
    Abort,
}

#[derive(Copy, Clone)]
pub(crate) struct Product {
    pub(crate) identifier: Uuid,
    pub(crate) pr_type: ProductType,
    pub(crate) price: u64,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Transaction {
    pub(crate) pr_type: ProductType,
    pub(crate) shift: i32,
}

pub(crate) struct ProductPriceQuery {
    pub(crate) product_ident: Uuid,
    pub(crate) result_sender: Sender<ProductPrice>,
}

pub(crate) struct ProductPrice(pub(crate) Option<u64>);

/// Message which disables a node. Used for testing.
pub(crate) struct Disable;

/// Register and initialize a CyberStore2047 module.
pub(crate) async fn register_store(
    system: &mut System,
    store: CyberStore2047,
) -> ModuleRef<CyberStore2047> {
    let module_ref = system.register_module(store).await;
    module_ref.clone().send(Init { module_ref: module_ref.clone() }).await;

    module_ref
}

/// Register and initialize a Node module.
pub(crate) async fn register_node(system: &mut System, node: Node) -> ModuleRef<Node> {
    let module_ref = system.register_module(node).await;
    module_ref.clone().send(Init { module_ref: module_ref.clone() }).await;

    module_ref
}

/// CyberStore2047.
/// This structure serves as TM.
// Add any fields you need.
pub(crate) struct CyberStore2047 {
    module_ref: Option<ModuleRef<CyberStore2047>>,
    nodes: Vec<ModuleRef<Node>>,
    responses: usize,
    abort: bool,
    callback: Option<Box<dyn FnOnce(TwoPhaseResult) -> Pin<Box<dyn Future<Output=()> + Send>> + Send>>,
}

impl CyberStore2047 {
    pub(crate) fn new(nodes: Vec<ModuleRef<Node>>) -> Self {
        Self {
            module_ref: None,
            nodes,
            responses: 0,
            abort: false,
            callback: None,
        }
    }

    pub(crate) async fn broadcast(&mut self, msg: StoreMsg) {
        for node in self.nodes.iter() {
            node.send(msg.clone()).await;
        }
    }
}

/// Node of CyberStore2047.
/// This structure serves as a process of the distributed system.
// Add any fields you need.
pub(crate) struct Node {
    products: Vec<Product>,
    pending_transaction: Option<Transaction>,
    enabled: bool,
    module_ref: Option<ModuleRef<Node>>,
}

impl Node {
    pub(crate) fn new(products: Vec<Product>) -> Self {
        Self {
            products,
            pending_transaction: None,
            enabled: true,
            module_ref: None,
        }
    }
}

struct Init<T: Send + 'static> {
    module_ref: ModuleRef<T>,
}

#[async_trait::async_trait]
impl Handler<Init<Self>> for CyberStore2047 {
    async fn handle(&mut self, msg: Init<Self>) {
        self.module_ref = Some(msg.module_ref);
    }
}

#[async_trait::async_trait]
impl Handler<Init<Self>> for Node {
    async fn handle(&mut self, msg: Init<Self>) {
        self.module_ref = Some(msg.module_ref);
    }
}


#[async_trait::async_trait]
impl Handler<NodeMsg> for CyberStore2047 {
    async fn handle(&mut self, msg: NodeMsg) {
        self.responses += 1;

        match msg.content {
            NodeMsgContent::RequestVoteResponse(res) => {
                match res {
                    TwoPhaseResult::Ok => {}
                    TwoPhaseResult::Abort => {
                        self.abort = true;
                    }
                }

                if self.responses == self.nodes.len() {
                    self.broadcast(StoreMsg { sender: self.module_ref.clone().unwrap(), content: if self.abort { StoreMsgContent::Abort } else { StoreMsgContent::Commit } }).await;
                    self.responses = 0;
                }
            }
            NodeMsgContent::FinalizationAck => {
                if self.responses == self.nodes.len() {
                    let callback = self.callback.take().unwrap();
                    callback(if self.abort { TwoPhaseResult::Abort } else { TwoPhaseResult::Ok }).await;
                    self.responses = 0;
                    self.abort = false;
                }
            }
        }
    }
}

#[async_trait::async_trait]
impl Handler<StoreMsg> for Node {
    async fn handle(&mut self, msg: StoreMsg) {
        if self.enabled {
            match msg.content {
                StoreMsgContent::RequestVote(transaction) => {
                    let ok = self.products.iter().filter(|p| p.pr_type == transaction.pr_type).all(|p| if transaction.shift.is_negative() { p.price > transaction.shift.wrapping_abs() as u64 } else { p.price.checked_add(transaction.shift as u64).is_some() });
                    self.pending_transaction = if ok { Some(transaction) } else { None };
                    msg.sender.clone().send(NodeMsg {
                        sender: self.module_ref.clone().unwrap(),
                        content: NodeMsgContent::RequestVoteResponse(
                            if ok { TwoPhaseResult::Ok } else { TwoPhaseResult::Abort }
                        ),
                    }).await;
                }
                StoreMsgContent::Commit => {
                    let pr_type = self.pending_transaction.unwrap().pr_type;
                    let shift = self.pending_transaction.unwrap().shift;
                    self.products.iter_mut().filter(|p| p.pr_type == pr_type).for_each(|p| { p.price = if shift.is_negative() { p.price - shift.wrapping_abs() as u64 } else { p.price + shift as u64 } });
                    msg.sender.clone().send(NodeMsg { sender: self.module_ref.clone().unwrap(), content: NodeMsgContent::FinalizationAck }).await;
                }
                StoreMsgContent::Abort => {
                    msg.sender.clone().send(NodeMsg { sender: self.module_ref.clone().unwrap(), content: NodeMsgContent::FinalizationAck }).await;
                }
            }
        }
    }
}

#[async_trait::async_trait]
impl Handler<ProductPriceQuery> for Node {
    async fn handle(&mut self, msg: ProductPriceQuery) {
        if self.enabled {
            let _ = msg.result_sender.send(ProductPrice(self.products.iter().find_map(|p| if p.identifier == msg.product_ident { Some(p.price) } else { None }))).await;
        }
    }
}

#[async_trait::async_trait]
impl Handler<Disable> for Node {
    async fn handle(&mut self, _msg: Disable) {
        self.enabled = false;
    }
}

#[async_trait::async_trait]
impl Handler<TransactionMessage> for CyberStore2047 {
    async fn handle(&mut self, msg: TransactionMessage) {
        self.callback = Some(msg.completed_callback);
        self.broadcast(StoreMsg { sender: self.module_ref.clone().unwrap(), content: StoreMsgContent::RequestVote(msg.transaction) }).await;
    }
}