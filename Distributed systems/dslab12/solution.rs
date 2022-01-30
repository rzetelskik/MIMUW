use std::collections::{HashSet, VecDeque};
use executor::{Handler, ModuleRef, System};

/// Marker trait indicating that a broadcast implementation provides
/// guarantees specified in the assignment description.
pub(crate) trait ReliableBroadcast<const N: usize> {}

#[async_trait::async_trait]
pub(crate) trait ReliableBroadcastRef<const N: usize>: Send + Sync + 'static {
    async fn send(&self, msg: Operation);
}

#[async_trait::async_trait]
impl<T, const N: usize> ReliableBroadcastRef<N> for ModuleRef<T>
    where
        T: ReliableBroadcast<N> + Handler<Operation> + Send,
{
    async fn send(&self, msg: Operation) {
        self.send(msg).await;
    }
}

/// Marker trait indicating that a client implementation
/// follows specification from the assignment description.
pub(crate) trait EditorClient {}

#[async_trait::async_trait]
pub(crate) trait ClientRef: Send + Sync + 'static {
    async fn send(&self, msg: Edit);
}

#[async_trait::async_trait]
impl<T> ClientRef for ModuleRef<T>
    where
        T: EditorClient + Handler<Edit> + Send,
{
    async fn send(&self, msg: Edit) {
        self.send(msg).await;
    }
}

/// Actions (edits) which can be applied to a text.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Debug))]
pub(crate) enum Action {
    /// Insert the character at the position.
    Insert { idx: usize, ch: char },
    /// Delete a character at the position.
    Delete { idx: usize },
    /// A _do nothing_ operation. `Nop` cannot be issued by a client.
    /// `Nop` can only be issued by a process or result from a transformation.
    Nop,
}

impl Action {
    /// Apply the action to the text.
    pub(crate) fn apply_to(&self, text: &mut String) {
        match self {
            Action::Insert { idx, ch } => {
                text.insert(*idx, *ch);
            }
            Action::Delete { idx } => {
                text.remove(*idx);
            }
            Action::Nop => {
                // Do nothing.
            }
        }
    }
}

/// Client's request to edit the text.
#[derive(Clone)]
pub(crate) struct EditRequest {
    /// Total number of operations a client has applied to its text so far.
    pub(crate) num_applied: usize,
    /// Action (edit) to be applied to a text.
    pub(crate) action: Action,
}

/// Response to a client with action (edit) it should apply to its text.
#[derive(Clone)]
pub(crate) struct Edit {
    pub(crate) action: Action,
}

#[derive(Clone)]
pub(crate) struct Operation {
    /// Rank of a process which issued this operation.
    pub(crate) process_rank: usize,
    /// Action (edit) to be applied to a text.
    pub(crate) action: Action,
}

impl Operation {
    // Add any methods you need.
    pub(crate) fn transform(&self, op2: &Operation) -> Operation {
        let (Operation { process_rank: r1, action: a1 }, Operation { process_rank: r2, action: a2 }) = (self.clone(), op2.clone());
        let action: Action = match (a1, a2) {
            (Action::Insert { idx: p1, ch: c1 }, Action::Insert { idx: p2, ch: _c2 }) => {
                if p1 < p2 || (p1 == p2 && r1 < r2) {
                    Action::Insert { idx: p1, ch: c1 }
                } else {
                    Action::Insert { idx: p1 + 1, ch: c1 }
                }
            }
            (Action::Delete { idx: p1 }, Action::Delete { idx: p2 }) => {
                if p1 < p2 {
                    Action::Delete { idx: p1 }
                } else if p1 == p2 {
                    Action::Nop
                } else {
                    Action::Delete { idx: p1 - 1 }
                }
            }
            (Action::Insert { idx: p1, ch: c1 }, Action::Delete { idx: p2 }) => {
                if p1 <= p2 {
                    Action::Insert { idx: p1, ch: c1 }
                } else {
                    Action::Insert { idx: p1 - 1, ch: c1 }
                }
            }
            (Action::Delete { idx: p1 }, Action::Insert { idx: p2, ch: _c2 }) => {
                if p1 < p2 {
                    Action::Delete { idx: p1 }
                } else {
                    Action::Delete { idx: p1 + 1 }
                }
            }
            (Action::Nop, _) => Action::Nop,
            (a1, Action::Nop) => a1
        };

        Operation { process_rank: r1, action }
    }
}

/// Process of the system.
pub(crate) struct Process<const N: usize> {
    /// Rank of the process.
    rank: usize,
    /// Reference to the broadcast module.
    broadcast: Box<dyn ReliableBroadcastRef<N>>,
    /// Reference to the process's client.
    client: Box<dyn ClientRef>,

    // Add any fields you need.
    log: Vec<Operation>,
    buf: VecDeque<EditRequest>,
    responses: HashSet<usize>,
    next_round: VecDeque<Operation>,
}

impl<const N: usize> Process<N> {
    pub(crate) async fn new(
        system: &mut System,
        rank: usize,
        broadcast: Box<dyn ReliableBroadcastRef<N>>,
        client: Box<dyn ClientRef>,
    ) -> ModuleRef<Self> {
        let self_ref = system
            .register_module(Self {
                rank,
                broadcast,
                client,
                // Add any fields you need.
                log: Vec::new(),
                buf: VecDeque::new(),
                responses: HashSet::new(),
                next_round: VecDeque::new(),
            })
            .await;
        self_ref
    }

    // Add any methods you need.

    pub(crate) fn is_in_round(&self) -> bool {
        self.responses.len() > 0
    }

    pub(crate) async fn start_round(&mut self, request: EditRequest) {
        let mut op1 = Operation { process_rank: N + 1, action: request.action };
        for op2 in self.log[request.num_applied..].iter() {
            op1 = op1.transform(op2);
        }
        op1.process_rank = self.rank;
        self.log.push(op1.clone());
        self.responses.insert(self.rank);
        self.broadcast.send(op1.clone()).await;
        self.client.send(Edit { action: op1.action.clone() }).await;
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<Operation> for Process<N> {
    async fn handle(&mut self, msg: Operation) {
        if self.responses.contains(&msg.process_rank) {
            self.next_round.push_back(msg);
            return;
        }

        if !self.is_in_round() {
            self.start_round(EditRequest { num_applied: self.log.len(), action: Action::Nop }).await;
        }

        let mut op1 = msg.clone();
        for op2 in self.log[self.log.len() - self.responses.len()..].iter() {
            op1 = op1.transform(op2);
        }
        self.log.push(op1.clone());
        self.responses.insert(msg.process_rank);

        self.client.send(Edit { action: op1.action.clone() }).await;

        if self.responses.len() == N {
            self.responses = HashSet::new();

            match self.buf.pop_front() {
                Some(req) => self.start_round(req).await,
                None => {}
            }

            while let Some(op) = self.next_round.pop_front() {
                self.handle(op).await;
            }
        }
    }
}

#[async_trait::async_trait]
impl<const N: usize> Handler<EditRequest> for Process<N> {
    async fn handle(&mut self, request: EditRequest) {
        if self.is_in_round() {
            self.buf.push_back(request);
        } else {
            self.start_round(request).await;
        }
    }
}
