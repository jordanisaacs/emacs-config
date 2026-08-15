use anyhow::{Context, Result};
use futures::stream::StreamExt;
use globset::{Glob, GlobSet, GlobSetBuilder};
use inotify::{EventMask, Inotify, WatchDescriptor, WatchMask, Watches};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::{mpsc, Mutex, Semaphore};
use tokio::task::JoinSet;
use walkdir::WalkDir;

const SUBSCRIBE_CONCURRENCY: usize = 8;

const KIND_CREATE: u32 = 1;
const KIND_CHANGE: u32 = 2;
const KIND_DELETE: u32 = 4;
const KIND_ALL: u32 = KIND_CREATE | KIND_CHANGE | KIND_DELETE;

fn watch_mask() -> WatchMask {
    WatchMask::CREATE
        | WatchMask::MODIFY
        | WatchMask::CLOSE_WRITE
        | WatchMask::MOVE
        | WatchMask::DELETE
        | WatchMask::DELETE_SELF
        | WatchMask::ATTRIB
        | WatchMask::DONT_FOLLOW
        | WatchMask::EXCL_UNLINK
}

#[derive(Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
enum Request {
    Subscribe {
        id: u64,
        subscription_id: String,
        root: PathBuf,
        globs: Vec<String>,
        #[serde(default = "default_kinds")]
        kinds: u32,
    },
    Unsubscribe {
        id: u64,
        subscription_id: String,
    },
    Shutdown {
        id: u64,
    },
}

fn default_kinds() -> u32 {
    KIND_ALL
}

#[derive(Serialize)]
#[serde(untagged)]
enum OutMessage {
    Response {
        id: u64,
        ok: bool,
        #[serde(skip_serializing_if = "Option::is_none")]
        err: Option<String>,
    },
    Event {
        event: &'static str,
        subscription_id: String,
        path: PathBuf,
        kind: &'static str,
    },
}

impl OutMessage {
    fn ok(id: u64) -> Self {
        OutMessage::Response {
            id,
            ok: true,
            err: None,
        }
    }
    fn err(id: u64, msg: String) -> Self {
        OutMessage::Response {
            id,
            ok: false,
            err: Some(msg),
        }
    }
}

struct Subscription {
    root: PathBuf,
    globs: GlobSet,
    kinds_mask: u32,
    dirs: HashSet<PathBuf>,
}

#[derive(Default)]
struct State {
    subscriptions: HashMap<String, Subscription>,
    wd_to_dir: HashMap<WatchDescriptor, PathBuf>,
    dir_to_wd: HashMap<PathBuf, (WatchDescriptor, u32)>,
}

type Tx = mpsc::UnboundedSender<OutMessage>;

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let inotify = Inotify::init().context("inotify init")?;
    let watches = inotify.watches();
    let buf = vec![0u8; 8192];
    let event_stream = inotify
        .into_event_stream(buf)
        .context("into_event_stream")?;

    let state: Arc<Mutex<State>> = Arc::new(Mutex::new(State::default()));
    let sem = Arc::new(Semaphore::new(SUBSCRIBE_CONCURRENCY));
    let (tx, mut rx) = mpsc::unbounded_channel::<OutMessage>();

    // Writer task: serializes OutMessages to stdout, one per line.
    let writer_handle = tokio::spawn(async move {
        let mut stdout = tokio::io::stdout();
        while let Some(msg) = rx.recv().await {
            let mut line = match serde_json::to_vec(&msg) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("fwatcher: serialize error: {e}");
                    continue;
                }
            };
            line.push(b'\n');
            if stdout.write_all(&line).await.is_err() {
                break;
            }
            if stdout.flush().await.is_err() {
                break;
            }
        }
    });

    // Event-stream task: converts inotify events into OutMessage::Event.
    let state_ev = state.clone();
    let tx_ev = tx.clone();
    let watches_ev = watches.clone();
    let event_handle = tokio::spawn(async move {
        handle_events(event_stream, state_ev, tx_ev, watches_ev).await;
    });

    // Reader loop: stdin → Request → spawn a task per request, tracked in JoinSet.
    let mut lines = BufReader::new(tokio::io::stdin()).lines();
    let mut tasks: JoinSet<()> = JoinSet::new();
    let ctrl_c = tokio::signal::ctrl_c();
    tokio::pin!(ctrl_c);

    loop {
        tokio::select! {
            next = lines.next_line() => {
                match next {
                    Ok(Some(line)) => {
                        if line.trim().is_empty() {
                            continue;
                        }
                        let req: Request = match serde_json::from_str(&line) {
                            Ok(r) => r,
                            Err(e) => {
                                eprintln!("fwatcher: bad request: {e}: {line}");
                                continue;
                            }
                        };
                        match req {
                            Request::Subscribe { id, subscription_id, root, globs, kinds } => {
                                let state = state.clone();
                                let watches = watches.clone();
                                let sem = sem.clone();
                                let tx = tx.clone();
                                tasks.spawn(async move {
                                    if let Err(e) = do_subscribe(
                                        state, watches, sem, id,
                                        subscription_id, root, globs, kinds, tx.clone(),
                                    ).await {
                                        let _ = tx.send(OutMessage::err(id, e.to_string()));
                                    }
                                });
                            }
                            Request::Unsubscribe { id, subscription_id } => {
                                let state = state.clone();
                                let watches = watches.clone();
                                let tx = tx.clone();
                                tasks.spawn(async move {
                                    if let Err(e) = do_unsubscribe(
                                        state, watches, id, subscription_id, tx.clone(),
                                    ).await {
                                        let _ = tx.send(OutMessage::err(id, e.to_string()));
                                    }
                                });
                            }
                            Request::Shutdown { id } => {
                                let _ = tx.send(OutMessage::ok(id));
                                break;
                            }
                        }
                    }
                    Ok(None) => break, // stdin EOF
                    Err(e) => {
                        eprintln!("fwatcher: stdin read error: {e}");
                        break;
                    }
                }
            }
            Some(res) = tasks.join_next() => {
                if let Err(e) = res {
                    if e.is_panic() {
                        eprintln!("fwatcher: task panicked: {e}");
                    }
                    // cancelled tasks are fine
                }
            }
            _ = &mut ctrl_c => {
                eprintln!("fwatcher: received SIGINT, shutting down");
                break;
            }
        }
    }

    // Graceful shutdown: abort in-flight subscribe/unsubscribe, close writer,
    // abort event task.
    tasks.abort_all();
    while tasks.join_next().await.is_some() {}
    drop(tx);
    let _ = writer_handle.await;
    event_handle.abort();
    let _ = event_handle.await;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
async fn do_subscribe(
    state: Arc<Mutex<State>>,
    watches: Watches,
    sem: Arc<Semaphore>,
    id: u64,
    subscription_id: String,
    root: PathBuf,
    globs: Vec<String>,
    kinds: u32,
    tx: Tx,
) -> Result<()> {
    let _permit = sem.acquire_owned().await?;

    let mut builder = GlobSetBuilder::new();
    for g in &globs {
        let glob = Glob::new(g).with_context(|| format!("bad glob {g:?}"))?;
        builder.add(glob);
    }
    let globset = builder.build().context("build globset")?;

    let root_canon = root
        .canonicalize()
        .with_context(|| format!("canonicalize {root:?}"))?;

    // Walk + add watches on the blocking pool.
    let state_walk = state.clone();
    let mut watches_walk = watches.clone();
    let root_walk = root_canon.clone();
    let dirs: HashSet<PathBuf> = tokio::task::spawn_blocking(move || -> HashSet<PathBuf> {
        let mut owned: HashSet<PathBuf> = HashSet::new();
        for entry in WalkDir::new(&root_walk).follow_links(false) {
            let entry = match entry {
                Ok(e) => e,
                Err(_) => continue,
            };
            if !entry.file_type().is_dir() {
                continue;
            }
            let dir = entry.into_path();
            incref_dir(&state_walk, &mut watches_walk, &dir);
            owned.insert(dir);
        }
        owned
    })
    .await
    .context("join walk task")?;

    let sub = Subscription {
        root: root_canon,
        globs: globset,
        kinds_mask: kinds,
        dirs,
    };

    {
        let mut st = state.lock().await;
        st.subscriptions.insert(subscription_id, sub);
    }

    let _ = tx.send(OutMessage::ok(id));
    Ok(())
}

async fn do_unsubscribe(
    state: Arc<Mutex<State>>,
    watches: Watches,
    id: u64,
    subscription_id: String,
    tx: Tx,
) -> Result<()> {
    let dirs: HashSet<PathBuf> = {
        let mut st = state.lock().await;
        match st.subscriptions.remove(&subscription_id) {
            Some(s) => s.dirs,
            None => {
                let _ = tx.send(OutMessage::err(
                    id,
                    format!("unknown subscription_id {subscription_id}"),
                ));
                return Ok(());
            }
        }
    };

    let state_del = state.clone();
    let mut watches_del = watches.clone();
    tokio::task::spawn_blocking(move || {
        let mut to_remove: Vec<WatchDescriptor> = Vec::new();
        {
            let mut st = state_del.blocking_lock();
            for dir in dirs {
                let wd_opt = match st.dir_to_wd.get_mut(&dir) {
                    Some((wd, count)) => {
                        *count = count.saturating_sub(1);
                        if *count == 0 {
                            Some(wd.clone())
                        } else {
                            None
                        }
                    }
                    None => None,
                };
                if let Some(wd) = wd_opt {
                    st.dir_to_wd.remove(&dir);
                    st.wd_to_dir.remove(&wd);
                    to_remove.push(wd);
                }
            }
        }
        for wd in to_remove {
            let _ = watches_del.remove(wd);
        }
    })
    .await
    .context("join unsubscribe task")?;

    let _ = tx.send(OutMessage::ok(id));
    Ok(())
}

/// Increment refcount for `dir`; on 0→1 transition, call inotify add_watch.
/// Must be called from a blocking context.
fn incref_dir(state: &Arc<Mutex<State>>, watches: &mut Watches, dir: &Path) {
    let existing_wd = {
        let st = state.blocking_lock();
        st.dir_to_wd.get(dir).map(|(wd, _)| wd.clone())
    };
    let wd = match existing_wd {
        Some(wd) => wd,
        None => match watches.add(dir, watch_mask()) {
            Ok(w) => w,
            Err(e) => {
                eprintln!("fwatcher: add_watch {dir:?}: {e}");
                return;
            }
        },
    };
    let mut st = state.blocking_lock();
    let wd_key = {
        let entry = st
            .dir_to_wd
            .entry(dir.to_path_buf())
            .or_insert_with(|| (wd.clone(), 0));
        entry.1 += 1;
        entry.0.clone()
    };
    st.wd_to_dir.entry(wd_key).or_insert_with(|| dir.to_path_buf());
}

async fn handle_events<S>(
    mut stream: inotify::EventStream<S>,
    state: Arc<Mutex<State>>,
    tx: Tx,
    watches: Watches,
) where
    S: AsMut<[u8]> + AsRef<[u8]> + Unpin,
{
    while let Some(next) = stream.next().await {
        let event = match next {
            Ok(e) => e,
            Err(e) => {
                eprintln!("fwatcher: event stream error: {e}");
                continue;
            }
        };

        let mask = event.mask;

        // Kernel auto-removed the watch (dir gone, etc).
        if mask.contains(EventMask::IGNORED) {
            let mut st = state.lock().await;
            if let Some(dir) = st.wd_to_dir.remove(&event.wd) {
                st.dir_to_wd.remove(&dir);
            }
            continue;
        }

        let dir = {
            let st = state.lock().await;
            match st.wd_to_dir.get(&event.wd) {
                Some(d) => d.clone(),
                None => continue, // orphan (late event after removal)
            }
        };

        let path = match event.name.as_deref() {
            Some(n) => dir.join(n),
            None => dir.clone(),
        };

        let is_dir = mask.contains(EventMask::ISDIR);

        if is_dir {
            if mask.intersects(EventMask::CREATE | EventMask::MOVED_TO) {
                let state_c = state.clone();
                let mut watches_c = watches.clone();
                let path_c = path.clone();
                tokio::task::spawn_blocking(move || {
                    add_subtree(&state_c, &mut watches_c, &path_c);
                });
            }
            // Dir delete / move-out will fan out as IN_IGNORED on descendant wds.
            continue;
        }

        let kind: &'static str =
            if mask.intersects(EventMask::CREATE | EventMask::MOVED_TO) {
                "create"
            } else if mask.intersects(
                EventMask::MODIFY | EventMask::CLOSE_WRITE | EventMask::ATTRIB,
            ) {
                "change"
            } else if mask.intersects(EventMask::DELETE | EventMask::MOVED_FROM) {
                "delete"
            } else {
                continue;
            };
        let kind_bit: u32 = match kind {
            "create" => KIND_CREATE,
            "change" => KIND_CHANGE,
            "delete" => KIND_DELETE,
            _ => continue,
        };

        let st = state.lock().await;
        for (sid, sub) in &st.subscriptions {
            if (sub.kinds_mask & kind_bit) == 0 {
                continue;
            }
            if !path.starts_with(&sub.root) {
                continue;
            }
            let rel = match path.strip_prefix(&sub.root) {
                Ok(r) => r,
                Err(_) => continue,
            };
            if !sub.globs.is_match(rel) {
                continue;
            }
            let _ = tx.send(OutMessage::Event {
                event: "fs",
                subscription_id: sid.clone(),
                path: path.clone(),
                kind,
            });
        }
    }
}

/// Walk a newly-created directory subtree and add watches for every dir that's
/// covered by at least one existing subscription.
fn add_subtree(state: &Arc<Mutex<State>>, watches: &mut Watches, start: &Path) {
    for entry in WalkDir::new(start).follow_links(false) {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };
        if !entry.file_type().is_dir() {
            continue;
        }
        let dir = entry.into_path();

        // Figure out which subscriptions cover this dir; if none, skip.
        let covering: Vec<String> = {
            let st = state.blocking_lock();
            st.subscriptions
                .iter()
                .filter(|(_, s)| dir.starts_with(&s.root))
                .map(|(k, _)| k.clone())
                .collect()
        };
        if covering.is_empty() {
            continue;
        }

        // Install the watch if missing.
        let was_new = {
            let st = state.blocking_lock();
            !st.dir_to_wd.contains_key(&dir)
        };
        let wd = if was_new {
            match watches.add(&dir, watch_mask()) {
                Ok(w) => Some(w),
                Err(e) => {
                    eprintln!("fwatcher: add_watch {dir:?}: {e}");
                    None
                }
            }
        } else {
            None
        };

        let mut st = state.blocking_lock();
        if let Some(wd) = wd {
            st.wd_to_dir.insert(wd.clone(), dir.clone());
            st.dir_to_wd.insert(dir.clone(), (wd, 0));
        }
        if let Some((_, count)) = st.dir_to_wd.get_mut(&dir) {
            *count += covering.len() as u32;
        }
        for sid in covering {
            if let Some(sub) = st.subscriptions.get_mut(&sid) {
                sub.dirs.insert(dir.clone());
            }
        }
    }
}
