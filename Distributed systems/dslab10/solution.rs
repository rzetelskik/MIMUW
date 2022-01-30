use std::collections::HashSet;
use std::ops::Div;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use executor::{Handler, ModuleRef, System};
use uuid::Uuid;

/// State of a Raft process.
/// It shall be kept in stable storage, and updated before replying to messages.
#[derive(Default, Clone, Copy)]
pub(crate) struct ProcessState {
    /// Number of the current term. `0` at boot.
    pub(crate) current_term: u64,
    /// Identifier of a process which has received this process' vote.
    /// `None if this process has not voted in this term.
    voted_for: Option<Uuid>,
    /// Identifier of a process which is thought to be the leader.
    leader_id: Option<Uuid>,
}

/// Configuration of a Raft process.
#[derive(Copy, Clone)]
pub(crate) struct ProcessConfig {
    /// UUID of the process.
    pub(crate) self_id: Uuid,
    /// Timeout used by this process.
    /// In contrast to real-world applications, in this assignment,
    /// the timeout shall not be randomized!
    pub(crate) election_timeout: Duration,
    /// Number of processes in the system.
    /// In the assignment, there is a constant number of processes.
    pub(crate) processes_count: usize,
}

#[derive(Copy, Clone)]
pub(crate) struct RaftMessage {
    pub(crate) header: RaftMessageHeader,
    pub(crate) content: RaftMessageContent,
}

struct Timeout;

struct Init {
    self_ref: ModuleRef<Raft>,
}

/// Message disabling a process. Used for testing to simulate failures.
pub(crate) struct Disable;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) struct RaftMessageHeader {
    /// Term of the process which issues the message.
    pub(crate) term: u64,
}

#[derive(Copy, Clone)]
pub(crate) enum RaftMessageContent {
    Heartbeat {
        /// Id of the process issuing the message, which claims to be the leader.
        leader_id: Uuid,
    },
    HeartbeatResponse,
    RequestVote {
        /// Id of the process issuing the message, which is a candidate.
        candidate_id: Uuid,
    },
    RequestVoteResponse {
        /// Whether the vote was granted or not.
        granted: bool,
        /// Id of the process issuing the message, which grants (or not) the vote.
        source: Uuid,
    },
}

pub(crate) trait StableStorage: Send {
    fn put(&mut self, state: &ProcessState);

    fn get(&self) -> Option<ProcessState>;
}

#[async_trait::async_trait]
pub(crate) trait Sender: Send + Sync {
    async fn send(&self, target: &Uuid, msg: RaftMessage);

    async fn broadcast(&self, msg: RaftMessage);
}

/// Process of Raft.
pub struct Raft {
    state: ProcessState,
    config: ProcessConfig,
    storage: Box<dyn StableStorage>,
    sender: Box<dyn Sender>,
    process_type: ProcessType,
    timer_abort: Arc<AtomicBool>,
    enabled: bool,
    self_ref: Option<ModuleRef<Self>>,
}

impl Raft {
    pub(crate) async fn new(
        system: &mut System,
        config: ProcessConfig,
        storage: Box<dyn StableStorage>,
        sender: Box<dyn Sender>,
    ) -> ModuleRef<Self> {
        let state = storage.get().unwrap_or_default();

        let self_ref = system
            .register_module(Self {
                state,
                config,
                storage,
                sender,
                process_type: Default::default(),
                timer_abort: Arc::new(AtomicBool::new(false)),
                enabled: true,
                self_ref: None,
            })
            .await;
        self_ref
            .send(Init {
                self_ref: self_ref.clone(),
            })
            .await;
        self_ref
    }

    fn reset_timer(&mut self, interval: Duration) {
        self.timer_abort.store(true, Ordering::Relaxed);
        self.timer_abort = Arc::new(AtomicBool::new(false));
        tokio::spawn(run_timer(
            self.self_ref.as_ref().unwrap().clone(),
            interval,
            self.timer_abort.clone(),
        ));
    }

    /// Set the process's term to the higher number.
    fn update_term(&mut self, new_term: u64) {
        assert!(self.state.current_term < new_term);
        self.state.current_term = new_term;
        self.state.voted_for = None;
        self.state.leader_id = None;
        // No reliable state update called here, must be called separately.
    }

    /// Reliably save the state.
    fn update_state(&mut self) {
        self.storage.put(&self.state);
    }

    /// Handle the received heartbeat.
    async fn heartbeat(&mut self, leader_id: Uuid, leader_term: u64) {
        if leader_term >= self.state.current_term {
            self.state.leader_id = Some(leader_id);
            self.update_state();

            // Update the volatile state:
            match &mut self.process_type {
                ProcessType::Follower => {
                    self.reset_timer(self.config.election_timeout);
                }
                ProcessType::Candidate { .. } => {
                    self.process_type = ProcessType::Follower;
                    self.reset_timer(self.config.election_timeout);
                }
                ProcessType::Leader => {
                    log::debug!("Ignore, heartbeat from self")
                }
            };
        }

        // Response is always sent, so information about the term is disseminated:
        self.sender
            .send(
                &leader_id,
                RaftMessage {
                    header: RaftMessageHeader {
                        term: self.state.current_term,
                    },
                    content: RaftMessageContent::HeartbeatResponse,
                },
            )
            .await;
    }

    fn grant_vote(&mut self, candidate_id: Uuid, granted: &mut bool) {
        self.state.voted_for = Some(candidate_id);
        self.update_state();
        *granted = true;
    }

    async fn vote(&mut self, candidate_id: Uuid, candidate_term: u64) {
        if candidate_term >= self.state.current_term {
            let mut granted = false;
            match &mut self.process_type {
                ProcessType::Candidate { .. } => {
                    // Turn into follower if voting for other process.
                    if candidate_id != self.config.self_id {
                        self.process_type = ProcessType::Follower;
                    }
                    self.grant_vote(candidate_id, &mut granted);
                }
                ProcessType::Follower => {
                    if self.state.voted_for == self.state.leader_id {
                        self.grant_vote(candidate_id, &mut granted);
                    }
                }
                ProcessType::Leader => {
                    // Shouldn't ever happen, deny request.
                    log::warn!("Leader received vote request.");
                }
            }

            self.sender
                .send(
                    &candidate_id,
                    RaftMessage {
                        header: RaftMessageHeader {
                            term: self.state.current_term,
                        },
                        content: RaftMessageContent::RequestVoteResponse {
                            granted,
                            source: self.config.self_id,
                        },
                    },
                )
                .await;
        }
    }


    /// Common message processing.
    fn msg_received(&mut self, msg: &RaftMessage) {
        if msg.header.term > self.state.current_term {
            self.update_term(msg.header.term);
            self.process_type = ProcessType::Follower;
        }
    }

    /// Broadcast heartbeat.
    async fn broadcast_heartbeat(&mut self) {
        self.sender
            .broadcast(RaftMessage {
                header: RaftMessageHeader {
                    term: self.state.current_term,
                },
                content: RaftMessageContent::Heartbeat {
                    leader_id: self.config.self_id,
                },
            })
            .await;
    }

    async fn broadcast_vote_request(&mut self) {
        self.sender
            .broadcast(RaftMessage {
                header: RaftMessageHeader {
                    term: self.state.current_term
                },
                content: RaftMessageContent::RequestVote {
                    candidate_id: self.config.self_id
                },
            })
            .await;
    }

    async fn nominate_self(&mut self) {
        self.process_type = ProcessType::Candidate { votes_received: HashSet::new() };
        self.update_term(self.state.current_term + 1);
        self.update_state();

        self.broadcast_vote_request().await;
    }

    fn take_leadership(&mut self) {
        self.process_type = ProcessType::Leader;
        self.reset_timer(self.config.election_timeout.div(10));
    }
}

async fn run_timer(raft_ref: ModuleRef<Raft>, interval: Duration, abort: Arc<AtomicBool>) {
    let mut interval = tokio::time::interval(interval);
    interval.tick().await;
    interval.tick().await;
    while !abort.load(Ordering::Relaxed) {
        raft_ref.send(Timeout).await;
        interval.tick().await;
    }
}

#[async_trait::async_trait]
impl Handler<Init> for Raft {
    async fn handle(&mut self, msg: Init) {
        if self.enabled {
            self.self_ref = Some(msg.self_ref);
            self.reset_timer(self.config.election_timeout);
        }
    }
}

/// Handle timer timeout.
#[async_trait::async_trait]
impl Handler<Timeout> for Raft {
    async fn handle(&mut self, _: Timeout) {
        println!("Process {} timed out. Enabled status: {}", self.config.self_id, self.enabled);
        if self.enabled {
            match &mut self.process_type {
                ProcessType::Follower {} => {
                    if self.state.voted_for == self.state.leader_id {
                        self.nominate_self().await;
                    }
                }
                ProcessType::Candidate { .. } => {
                    self.nominate_self().await;
                }
                ProcessType::Leader => {
                    self.broadcast_heartbeat().await;
                }
            }
        }
    }
}

#[async_trait::async_trait]
impl Handler<RaftMessage> for Raft {
    async fn handle(&mut self, msg: RaftMessage) {
        if self.enabled {
            // Update common state:
            self.msg_received(&msg);

            // TODO message specific processing. Heartbeat is given as an example:
            match (&mut self.process_type, msg.content) {
                (_, RaftMessageContent::Heartbeat { leader_id }) => {
                    self.heartbeat(leader_id, msg.header.term).await;
                }
                (_, RaftMessageContent::RequestVote { candidate_id }) => {
                    self.vote(candidate_id, msg.header.term).await;
                }
                (ProcessType::Candidate { votes_received }, RaftMessageContent::RequestVoteResponse { granted, source }) => {
                    if granted {
                        votes_received.insert(source);

                        if 2 * votes_received.len() > self.config.processes_count {
                            self.take_leadership();
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

#[async_trait::async_trait]
impl Handler<Disable> for Raft {
    async fn handle(&mut self, _: Disable) {
        self.enabled = false;
    }
}

/// State of a Raft process with a corresponding (volatile) information.
enum ProcessType {
    Follower,
    Candidate { votes_received: HashSet<Uuid> },
    Leader,
}

impl Default for ProcessType {
    fn default() -> Self {
        ProcessType::Follower
    }
}
