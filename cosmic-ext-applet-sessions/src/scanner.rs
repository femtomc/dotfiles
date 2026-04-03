use serde::Deserialize;
use std::collections::HashSet;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

// ── Public types ──────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SessionKind {
    ClaudeCode,
    Codex,
    Ghostty,
}

impl SessionKind {
    pub fn label(self) -> &'static str {
        match self {
            Self::ClaudeCode => "Claude Code",
            Self::Codex => "Codex",
            Self::Ghostty => "Ghostty",
        }
    }

    pub fn spawn_args(self) -> (&'static str, &'static [&'static str]) {
        match self {
            Self::ClaudeCode => ("ghostty", &["-e", "claude"]),
            Self::Codex => ("ghostty", &["-e", "codex"]),
            Self::Ghostty => ("ghostty", &[]),
        }
    }

    pub const ALL: [SessionKind; 3] = [Self::ClaudeCode, Self::Codex, Self::Ghostty];
}

#[derive(Debug, Clone)]
pub struct SessionInfo {
    pub pid: Option<u32>,
    pub kind: SessionKind,
    pub cwd: Option<String>,
    pub activity: Activity,
    pub model: Option<String>,
    pub git_branch: Option<String>,
    pub summary: Option<String>,
    pub uptime: Option<Duration>,
    pub token_count: Option<u64>,
}

#[derive(Debug, Clone)]
pub enum Activity {
    /// Waiting for user input.
    Idle,
    /// Currently invoking a tool (Bash, Edit, Read, Agent, etc).
    Tool(String),
    /// Producing text output.
    Thinking,
    /// Status unknown or process just started.
    Unknown,
}

impl Activity {
    pub fn label(&self) -> String {
        match self {
            Self::Idle => "waiting for input".into(),
            Self::Tool(name) => format!("using {name}"),
            Self::Thinking => "thinking".into(),
            Self::Unknown => "starting".into(),
        }
    }
}

// ── Claude Code scanning ──────────────────────────────────────────────

/// Session registry entry from `~/.claude/sessions/<pid>.json`.
#[derive(Deserialize)]
struct ClaudeSessionFile {
    pid: u32,
    #[serde(rename = "sessionId")]
    session_id: String,
    cwd: String,
    #[serde(rename = "startedAt")]
    started_at: u64, // epoch ms
    #[allow(dead_code)]
    kind: Option<String>,
}

/// sessions-index.json entry.
#[derive(Deserialize)]
struct ClaudeSessionIndex {
    #[serde(rename = "sessionId")]
    session_id: String,
    summary: Option<String>,
    #[serde(rename = "gitBranch")]
    git_branch: Option<String>,
    #[serde(rename = "firstPrompt")]
    first_prompt: Option<String>,
}

#[derive(Deserialize)]
struct ClaudeSessionIndexFile {
    #[allow(dead_code)]
    version: Option<u32>,
    entries: Vec<ClaudeSessionIndex>,
}

fn claude_home() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".claude"))
}

/// Encode a cwd path the way Claude Code does for project directory names.
fn encode_project_dir(cwd: &str) -> String {
    cwd.replace('/', "-")
}

/// Read the last N lines of a file (cheaply, by seeking from the end).
fn tail_lines(path: &Path, n: usize) -> Vec<String> {
    let Ok(mut file) = std::fs::File::open(path) else {
        return vec![];
    };
    let Ok(len) = file.seek(SeekFrom::End(0)) else {
        return vec![];
    };
    // Read up to 64KB from the end — enough for recent activity.
    let start = len.saturating_sub(65536);
    let _ = file.seek(SeekFrom::Start(start));
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().map_while(Result::ok).collect();
    lines.into_iter().rev().take(n).collect()
}

/// Determine what the Claude Code session is currently doing by reading
/// the tail of its JSONL transcript.
fn claude_activity(session_id: &str, project_dir: &Path) -> (Activity, Option<String>) {
    let jsonl_path = project_dir.join(format!("{session_id}.jsonl"));
    let lines = tail_lines(&jsonl_path, 6);
    let mut model = None;

    // Walk backwards through recent entries to find the latest state.
    for line in &lines {
        let Ok(val) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        let msg = &val["message"];
        let role = msg["role"].as_str().unwrap_or("");

        if role == "assistant" {
            if model.is_none() {
                model = msg["model"].as_str().map(|s| s.to_string());
            }
            if let Some(content) = msg["content"].as_array() {
                // Check the *last* content block to see the most recent action.
                if let Some(last) = content.last() {
                    match last["type"].as_str() {
                        Some("tool_use") => {
                            let tool = last["name"]
                                .as_str()
                                .unwrap_or("tool")
                                .to_string();
                            return (Activity::Tool(tool), model);
                        }
                        Some("text") => {
                            return (Activity::Thinking, model);
                        }
                        _ => {}
                    }
                }
            }
        } else if role == "user" {
            // A user message (including tool_results) at the tail means
            // the assistant hasn't responded yet, or we're between turns.
            if let Some(content) = msg["content"].as_array() {
                let has_tool_result = content
                    .iter()
                    .any(|b| b["type"].as_str() == Some("tool_result"));
                if has_tool_result {
                    // Tool result just came back — assistant is about to respond.
                    return (Activity::Thinking, model);
                }
            }
            // Pure user text at the tail = waiting for assistant to start.
            return (Activity::Thinking, model);
        }
    }

    (Activity::Unknown, model)
}

pub fn scan_claude_sessions() -> Vec<SessionInfo> {
    let Some(claude_dir) = claude_home() else {
        return vec![];
    };
    let sessions_dir = claude_dir.join("sessions");
    let Ok(entries) = std::fs::read_dir(&sessions_dir) else {
        return vec![];
    };

    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;

    let mut results = vec![];

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let Ok(data) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok(sess) = serde_json::from_str::<ClaudeSessionFile>(&data) else {
            continue;
        };

        // Only include sessions whose process is still alive.
        if !Path::new(&format!("/proc/{}", sess.pid)).exists() {
            continue;
        }

        let project_key = encode_project_dir(&sess.cwd);
        let project_dir = claude_dir.join("projects").join(&project_key);

        // Look up summary + git branch from the sessions-index.
        let index_path = project_dir.join("sessions-index.json");
        let index_entry = std::fs::read_to_string(&index_path)
            .ok()
            .and_then(|s| serde_json::from_str::<ClaudeSessionIndexFile>(&s).ok())
            .and_then(|idx| {
                idx.entries
                    .into_iter()
                    .find(|e| e.session_id == sess.session_id)
            });

        let summary = index_entry
            .as_ref()
            .and_then(|e| e.summary.clone().or_else(|| e.first_prompt.clone()));
        let git_branch = index_entry.as_ref().and_then(|e| e.git_branch.clone());

        let (activity, model) = claude_activity(&sess.session_id, &project_dir);

        let uptime = if sess.started_at > 0 && now > sess.started_at {
            Some(Duration::from_millis(now - sess.started_at))
        } else {
            None
        };

        let cwd = shorten_home(&sess.cwd);

        results.push(SessionInfo {
            pid: Some(sess.pid),
            kind: SessionKind::ClaudeCode,
            cwd: Some(cwd),
            activity,
            model,
            git_branch,
            summary,
            uptime,
            token_count: None,
        });
    }

    results
}

// ── Codex scanning ────────────────────────────────────────────────────

pub fn scan_codex_sessions() -> Vec<SessionInfo> {
    let Some(home) = dirs::home_dir() else {
        return vec![];
    };
    let db_path = home.join(".codex/state_5.sqlite");
    if !db_path.exists() {
        return vec![];
    }

    let Ok(conn) = rusqlite::Connection::open_with_flags(
        &db_path,
        rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY | rusqlite::OpenFlags::SQLITE_OPEN_NO_MUTEX,
    ) else {
        return vec![];
    };

    let mut stmt = match conn.prepare(
        "SELECT id, title, cwd, model, tokens_used, git_branch, first_user_message, updated_at
         FROM threads
         WHERE archived = 0
         ORDER BY updated_at DESC
         LIMIT 20",
    ) {
        Ok(s) => s,
        Err(_) => return vec![],
    };

    let rows = stmt.query_map([], |row| {
        Ok((
            row.get::<_, String>(0)?,           // id
            row.get::<_, String>(1)?,           // title
            row.get::<_, String>(2)?,           // cwd
            row.get::<_, Option<String>>(3)?,   // model
            row.get::<_, i64>(4)?,              // tokens_used
            row.get::<_, Option<String>>(5)?,   // git_branch
            row.get::<_, String>(6)?,           // first_user_message
            row.get::<_, i64>(7)?,              // updated_at (epoch seconds)
        ))
    });

    let Ok(rows) = rows else {
        return vec![];
    };

    let mut results = vec![];

    // Find live codex processes.
    let live_codex_pids = find_procs_by_name("codex");

    for row in rows.flatten() {
        let (_id, title, cwd, model, tokens, git_branch, first_msg, updated_at) = row;

        // Only show threads updated in the last hour that have a live codex process
        // in the same cwd.
        let now_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64;

        if now_epoch - updated_at > 3600 {
            continue;
        }

        let has_live_proc = live_codex_pids
            .iter()
            .any(|&pid| proc_cwd_raw(pid).as_deref() == Some(cwd.as_str()));

        if !has_live_proc {
            continue;
        }

        let pid = live_codex_pids
            .iter()
            .find(|&&pid| proc_cwd_raw(pid).as_deref() == Some(cwd.as_str()))
            .copied();

        let summary = if !title.is_empty() {
            Some(title)
        } else if !first_msg.is_empty() {
            Some(truncate(&first_msg, 60))
        } else {
            None
        };

        // Try to determine activity from the latest rollout event.
        let activity = codex_activity_from_rollout(updated_at);

        results.push(SessionInfo {
            pid,
            kind: SessionKind::Codex,
            cwd: Some(shorten_home(&cwd)),
            activity,
            model,
            git_branch,
            summary,
            uptime: None,
            token_count: Some(tokens as u64),
        });
    }

    results
}

/// Try to infer current Codex activity from the most recent rollout file.
fn codex_activity_from_rollout(_updated_at: i64) -> Activity {
    let Some(home) = dirs::home_dir() else {
        return Activity::Unknown;
    };
    let sessions_dir = home.join(".codex/sessions");

    // Find the most recently modified rollout file.
    let latest = find_latest_file(&sessions_dir, "rollout-");
    let Some(path) = latest else {
        return Activity::Unknown;
    };

    let lines = tail_lines(&path, 4);
    for line in &lines {
        let Ok(val) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        let payload_type = val["payload"]["type"].as_str().unwrap_or("");
        match payload_type {
            "exec_command_end" => return Activity::Idle,
            "turn_aborted" => return Activity::Idle,
            "function_call" => {
                let name = val["payload"]["name"].as_str().unwrap_or("tool");
                return Activity::Tool(name.to_string());
            }
            "message" => {
                let role = val["payload"]["role"].as_str().unwrap_or("");
                if role == "assistant" {
                    return Activity::Thinking;
                } else {
                    return Activity::Idle;
                }
            }
            "token_count" => continue, // skip, look further back
            _ => continue,
        }
    }

    Activity::Unknown
}

fn find_latest_file(dir: &Path, prefix: &str) -> Option<PathBuf> {
    let mut latest: Option<(PathBuf, SystemTime)> = None;
    walk_files(dir, prefix, &mut |path, mtime| {
        if latest.as_ref().map_or(true, |(_, t)| mtime > *t) {
            latest = Some((path, mtime));
        }
    });
    latest.map(|(p, _)| p)
}

fn walk_files(dir: &Path, prefix: &str, cb: &mut dyn FnMut(PathBuf, SystemTime)) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let ft = entry.file_type().unwrap_or_else(|_| unreachable!());
        if ft.is_dir() {
            walk_files(&entry.path(), prefix, cb);
        } else if ft.is_file() {
            if let Some(name) = entry.file_name().to_str() {
                if name.starts_with(prefix) {
                    if let Ok(meta) = entry.metadata() {
                        if let Ok(mtime) = meta.modified() {
                            cb(entry.path(), mtime);
                        }
                    }
                }
            }
        }
    }
}

// ── Ghostty (standalone) ─────────────────────────────────────────────

pub fn scan_ghostty_sessions(
    claude_pids: &HashSet<u32>,
    codex_pids: &HashSet<u32>,
) -> Vec<SessionInfo> {
    let ghostty_pids = find_procs_by_name("ghostty");
    let mut parent_pids: HashSet<u32> = HashSet::new();

    // Collect parent/grandparent PIDs of all claude & codex processes.
    for &pid in claude_pids.iter().chain(codex_pids.iter()) {
        if let Some(ppid) = proc_ppid(pid) {
            parent_pids.insert(ppid);
            if let Some(gppid) = proc_ppid(ppid) {
                parent_pids.insert(gppid);
            }
        }
    }

    ghostty_pids
        .into_iter()
        .filter(|pid| !parent_pids.contains(pid))
        .map(|pid| SessionInfo {
            pid: Some(pid),
            kind: SessionKind::Ghostty,
            cwd: proc_cwd_raw(pid).map(|s| shorten_home(&s)),
            activity: Activity::Idle,
            model: None,
            git_branch: None,
            summary: None,
            uptime: proc_uptime(pid),
            token_count: None,
        })
        .collect()
}

// ── Full scan ────────────────────────────────────────────────────────

pub fn scan_all() -> Vec<SessionInfo> {
    let claude = scan_claude_sessions();
    let codex = scan_codex_sessions();

    let claude_pids: HashSet<u32> = claude.iter().filter_map(|s| s.pid).collect();
    let codex_pids: HashSet<u32> = codex.iter().filter_map(|s| s.pid).collect();
    let ghostty = scan_ghostty_sessions(&claude_pids, &codex_pids);

    let mut all = claude;
    all.extend(codex);
    all.extend(ghostty);
    all
}

// ── Helpers ──────────────────────────────────────────────────────────

fn find_procs_by_name(name: &str) -> Vec<u32> {
    let Ok(entries) = std::fs::read_dir("/proc") else {
        return vec![];
    };
    let mut pids = vec![];
    for entry in entries.flatten() {
        let fname = entry.file_name();
        let Some(pid) = fname.to_str().and_then(|s| s.parse::<u32>().ok()) else {
            continue;
        };
        if let Some(bin) = proc_bin_name(pid) {
            if bin == name {
                pids.push(pid);
            }
        }
    }
    pids
}

fn proc_bin_name(pid: u32) -> Option<String> {
    let raw = std::fs::read(format!("/proc/{pid}/cmdline")).ok()?;
    let first = raw.split(|&b| b == 0).next()?;
    if first.is_empty() {
        return None;
    }
    let s = String::from_utf8_lossy(first);
    Some(s.rsplit('/').next().unwrap_or(&s).to_string())
}

fn proc_cwd_raw(pid: u32) -> Option<String> {
    std::fs::read_link(format!("/proc/{pid}/cwd"))
        .ok()
        .map(|p| p.to_string_lossy().into_owned())
}

fn proc_ppid(pid: u32) -> Option<u32> {
    let stat = std::fs::read_to_string(format!("/proc/{pid}/stat")).ok()?;
    let after_comm = stat.rsplit_once(')')?.1;
    let mut fields = after_comm.split_whitespace();
    fields.next()?; // state
    fields.next()?.parse().ok()
}

fn proc_uptime(pid: u32) -> Option<Duration> {
    let meta = std::fs::metadata(format!("/proc/{pid}")).ok()?;
    meta.modified().ok()?.elapsed().ok()
}

fn shorten_home(path: &str) -> String {
    if let Ok(home) = std::env::var("HOME") {
        if let Some(rest) = path.strip_prefix(&home) {
            return format!("~{rest}");
        }
    }
    path.to_string()
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max.min(s.len())])
    }
}

// Public wrappers for app.rs to use for window matching.
pub fn proc_bin_name_pub(pid: u32) -> Option<String> {
    proc_bin_name(pid)
}

pub fn proc_ppid_pub(pid: u32) -> Option<u32> {
    proc_ppid(pid)
}

pub fn proc_cwd_str(pid: u32) -> Option<String> {
    proc_cwd_raw(pid)
}

pub fn format_duration(d: &Duration) -> String {
    let secs = d.as_secs();
    if secs < 60 {
        format!("{secs}s")
    } else if secs < 3600 {
        format!("{}m", secs / 60)
    } else {
        format!("{}h{}m", secs / 3600, (secs % 3600) / 60)
    }
}
