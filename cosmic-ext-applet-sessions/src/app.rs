use cosmic::iced::window::Id;
use cosmic::iced::{Alignment, Length, Limits, Subscription};
use cosmic::iced_winit::commands::popup::{destroy_popup, get_popup};
use cosmic::prelude::*;
use cosmic::widget;
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use std::collections::{HashMap, HashSet};
use std::process::Command;

const APP_ID: &str = "dev.femtomc.CosmicExtAppletSessions";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SessionKind {
    ClaudeCode,
    Codex,
    Ghostty,
}

impl SessionKind {
    fn label(self) -> &'static str {
        match self {
            Self::ClaudeCode => "Claude Code",
            Self::Codex => "Codex",
            Self::Ghostty => "Ghostty",
        }
    }

    fn spawn_args(self) -> (&'static str, &'static [&'static str]) {
        match self {
            Self::ClaudeCode => ("ghostty", &["-e", "claude"]),
            Self::Codex => ("ghostty", &["-e", "codex"]),
            Self::Ghostty => ("ghostty", &[]),
        }
    }

    const ALL: [SessionKind; 3] = [Self::ClaudeCode, Self::Codex, Self::Ghostty];
}

#[derive(Debug, Clone)]
pub struct SessionInfo {
    pub pid: u32,
    pub cwd: Option<String>,
}

pub struct Sessions {
    core: cosmic::Core,
    popup: Option<Id>,
    active_sessions: HashMap<SessionKind, Vec<SessionInfo>>,
}

impl Default for Sessions {
    fn default() -> Self {
        Self {
            core: cosmic::Core::default(),
            popup: None,
            active_sessions: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    TogglePopup,
    PopupClosed(Id),
    Refresh(HashMap<SessionKind, Vec<SessionInfo>>),
    Launch(SessionKind),
    Kill(SessionKind, u32),
}

/// Read the binary name from /proc/{pid}/cmdline (first arg, basename only).
fn proc_bin_name(pid: u32) -> Option<String> {
    let raw = std::fs::read(format!("/proc/{pid}/cmdline")).ok()?;
    let first = raw.split(|&b| b == 0).next()?;
    if first.is_empty() {
        return None;
    }
    let s = String::from_utf8_lossy(first);
    Some(s.rsplit('/').next().unwrap_or(&s).to_string())
}

/// Read the working directory of a process.
fn proc_cwd(pid: u32) -> Option<String> {
    let link = std::fs::read_link(format!("/proc/{pid}/cwd")).ok()?;
    let s = link.to_string_lossy().into_owned();
    // Shorten home directory to ~
    if let Ok(home) = std::env::var("HOME") {
        if let Some(rest) = s.strip_prefix(&home) {
            return Some(format!("~{rest}"));
        }
    }
    Some(s)
}

/// Read the parent PID from /proc/{pid}/stat.
fn proc_ppid(pid: u32) -> Option<u32> {
    let stat = std::fs::read_to_string(format!("/proc/{pid}/stat")).ok()?;
    // Format: pid (comm) state ppid ...
    // comm can contain spaces and parens, so find the last ')' first.
    let after_comm = stat.rsplit_once(')')?.1;
    let mut fields = after_comm.split_whitespace();
    fields.next()?; // state
    fields.next()?.parse().ok()
}

/// Scan /proc for sessions, using parent-child relationships to avoid double-counting.
///
/// Strategy:
/// - First pass: find all claude/codex PIDs and collect their parent PIDs.
/// - Second pass: ghostty processes whose PID is a parent of a claude/codex session
///   are excluded from the Ghostty count (they're accounted for under Claude/Codex).
fn scan_sessions() -> HashMap<SessionKind, Vec<SessionInfo>> {
    let mut map: HashMap<SessionKind, Vec<SessionInfo>> = HashMap::new();
    let Ok(entries) = std::fs::read_dir("/proc") else {
        return map;
    };

    // Collect all (pid, bin_name) pairs in one pass.
    let mut procs: Vec<(u32, String)> = Vec::new();
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Some(pid) = name.to_str().and_then(|s| s.parse::<u32>().ok()) else {
            continue;
        };
        if let Some(bin) = proc_bin_name(pid) {
            procs.push((pid, bin));
        }
    }

    // Find claude/codex sessions and record which ghostty PIDs are their parents.
    let mut ghostty_parents: HashSet<u32> = HashSet::new();

    for &(pid, ref bin) in &procs {
        let kind = match bin.as_str() {
            "claude" => SessionKind::ClaudeCode,
            "codex" => SessionKind::Codex,
            _ => continue,
        };
        // Walk up to find the ghostty parent so we can exclude it.
        if let Some(ppid) = proc_ppid(pid) {
            // The immediate parent might be a shell; check grandparent too.
            ghostty_parents.insert(ppid);
            if let Some(gppid) = proc_ppid(ppid) {
                ghostty_parents.insert(gppid);
            }
        }
        map.entry(kind).or_default().push(SessionInfo {
            pid,
            cwd: proc_cwd(pid),
        });
    }

    // Now collect standalone ghostty sessions (those NOT parenting a claude/codex).
    for &(pid, ref bin) in &procs {
        if bin == "ghostty" && !ghostty_parents.contains(&pid) {
            map.entry(SessionKind::Ghostty)
                .or_default()
                .push(SessionInfo {
                    pid,
                    cwd: proc_cwd(pid),
                });
        }
    }

    map
}

fn total_sessions(sessions: &HashMap<SessionKind, Vec<SessionInfo>>) -> usize {
    sessions.values().map(|v| v.len()).sum()
}

impl cosmic::Application for Sessions {
    type Executor = cosmic::executor::Default;
    type Flags = ();
    type Message = Message;
    const APP_ID: &'static str = APP_ID;

    fn core(&self) -> &cosmic::Core {
        &self.core
    }

    fn core_mut(&mut self) -> &mut cosmic::Core {
        &mut self.core
    }

    fn init(
        core: cosmic::Core,
        _flags: Self::Flags,
    ) -> (Self, Task<cosmic::Action<Self::Message>>) {
        let mut app = Self {
            core,
            ..Default::default()
        };
        app.active_sessions = scan_sessions();
        (app, Task::none())
    }

    fn on_close_requested(&self, id: Id) -> Option<Message> {
        Some(Message::PopupClosed(id))
    }

    fn style(&self) -> Option<cosmic::iced::theme::Style> {
        Some(cosmic::applet::style())
    }

    fn view(&self) -> Element<'_, Self::Message> {
        let total = total_sessions(&self.active_sessions);
        let icon = self
            .core
            .applet
            .icon_button("utilities-terminal-symbolic")
            .on_press(Message::TogglePopup);
        if total > 0 {
            widget::row![
                    icon,
                    widget::text::body(format!("{total}"))
                        .width(Length::Shrink)
                        .align_y(Alignment::Center),
                ]
                .align_y(Alignment::Center)
                .spacing(2)
                .into()
        } else {
            icon.into()
        }
    }

    fn view_window(&self, _id: Id) -> Element<'_, Self::Message> {
        let mut content = widget::list_column().padding(5).spacing(0);

        for kind in SessionKind::ALL {
            let sessions = self.active_sessions.get(&kind);
            let count = sessions.map_or(0, |v| v.len());

            // Section header with launch button
            content = content.add(widget::settings::item(
                format!("{} ({})", kind.label(), count),
                widget::button::icon(widget::icon::from_name("list-add-symbolic"))
                    .on_press(Message::Launch(kind)),
            ));

            // List active sessions with cwd
            if let Some(sessions) = sessions {
                for session in sessions {
                    let pid = session.pid;
                    let label = session
                        .cwd
                        .as_deref()
                        .unwrap_or("(unknown)");
                    content = content.add(widget::settings::item(
                        format!("  {label}"),
                        widget::button::icon(widget::icon::from_name("window-close-symbolic"))
                            .on_press(Message::Kill(kind, pid)),
                    ));
                }
            }
        }

        self.core.applet.popup_container(content).into()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::run(|| {
            cosmic::iced::stream::channel(
                4,
                move |mut channel: cosmic::iced::futures::channel::mpsc::Sender<Message>| async move {
                    use cosmic::iced::futures::SinkExt;
                    loop {
                        let sessions = scan_sessions();
                        _ = channel.send(Message::Refresh(sessions)).await;
                        tokio::time::sleep(std::time::Duration::from_secs(3)).await;
                    }
                },
            )
        })
    }

    fn update(&mut self, message: Self::Message) -> Task<cosmic::Action<Self::Message>> {
        match message {
            Message::Refresh(sessions) => {
                self.active_sessions = sessions;
            }
            Message::Launch(kind) => {
                let (cmd, args) = kind.spawn_args();
                let _ = Command::new(cmd).args(args).spawn();
            }
            Message::Kill(_kind, pid) => {
                let _ = signal::kill(Pid::from_raw(pid as i32), Signal::SIGTERM);
            }
            Message::TogglePopup => {
                return if let Some(p) = self.popup.take() {
                    destroy_popup(p)
                } else {
                    let new_id = Id::unique();
                    self.popup.replace(new_id);
                    let mut popup_settings = self.core.applet.get_popup_settings(
                        self.core.main_window_id().unwrap(),
                        new_id,
                        None,
                        None,
                        None,
                    );
                    popup_settings.positioner.size_limits = Limits::NONE
                        .max_width(400.0)
                        .min_width(320.0)
                        .min_height(100.0)
                        .max_height(600.0);
                    get_popup(popup_settings)
                }
            }
            Message::PopupClosed(id) => {
                if self.popup.as_ref() == Some(&id) {
                    self.popup = None;
                }
            }
        }
        Task::none()
    }
}
