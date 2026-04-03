use crate::scanner::{self, SessionInfo, SessionKind};
use crate::toplevel::{self, ToplevelAction, ToplevelEvent};
use cctk::wayland_client::Proxy;
use cctk::wayland_protocols::ext::foreign_toplevel_list::v1::client::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1;
use cosmic::iced::window::Id;
use cosmic::iced::{Alignment, Length, Limits, Subscription};
use cosmic::iced_winit::commands::popup::{destroy_popup, get_popup};
use cosmic::prelude::*;
use cosmic::widget;
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use std::process::Command;

const APP_ID: &str = "dev.femtomc.CosmicExtAppletSessions";

pub struct Sessions {
    core: cosmic::Core,
    popup: Option<Id>,
    sessions: Vec<SessionInfo>,
    /// Channel to send activation commands to the Wayland thread.
    toplevel_tx: Option<sctk::reexports::calloop::channel::Sender<ToplevelAction>>,
}

impl Default for Sessions {
    fn default() -> Self {
        Self {
            core: cosmic::Core::default(),
            popup: None,
            sessions: Vec::new(),
            toplevel_tx: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    TogglePopup,
    PopupClosed(Id),
    Refresh(Vec<SessionInfo>),
    Launch(SessionKind),
    Kill(u32),
    /// Focus the Ghostty window associated with this session (by pid).
    Focus(Option<u32>),
    /// Toplevel window list update from Wayland thread.
    ToplevelUpdate(Vec<ToplevelEventMsg>),
}

/// Serializable wrapper since ToplevelInfo isn't Clone-friendly for iced messages.
#[derive(Debug, Clone)]
pub enum ToplevelEventMsg {
    Update {
        app_id: String,
        title: String,
        proto_id: u32,
    },
    Remove(u32),
}

fn sessions_by_kind(sessions: &[SessionInfo], kind: SessionKind) -> Vec<&SessionInfo> {
    sessions.iter().filter(|s| s.kind == kind).collect()
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
        app.sessions = scanner::scan_all();
        (app, Task::none())
    }

    fn on_close_requested(&self, id: Id) -> Option<Message> {
        Some(Message::PopupClosed(id))
    }

    fn style(&self) -> Option<cosmic::iced::theme::Style> {
        Some(cosmic::applet::style())
    }

    // ── Panel icon ───────────────────────────────────────────────────

    fn view(&self) -> Element<'_, Self::Message> {
        let total = self.sessions.len();
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

    // ── Popup content ────────────────────────────────────────────────

    fn view_window(&self, _id: Id) -> Element<'_, Self::Message> {
        let mut content = widget::list_column().padding(5).spacing(0);

        for kind in SessionKind::ALL {
            let kind_sessions = sessions_by_kind(&self.sessions, kind);
            let count = kind_sessions.len();

            // Section header.
            content = content.add(widget::settings::item(
                format!("{} ({})", kind.label(), count),
                widget::button::icon(widget::icon::from_name("list-add-symbolic"))
                    .on_press(Message::Launch(kind)),
            ));

            // Session rows — clicking the row focuses the window.
            for session in kind_sessions {
                content = content.add(session_row(session));
            }
        }

        self.core.applet.popup_container(content).into()
    }

    // ── Subscriptions ────────────────────────────────────────────────

    fn subscription(&self) -> Subscription<Self::Message> {
        // 1. Process scanner (poll every 3s).
        let scanner_sub = Subscription::run(|| {
            cosmic::iced::stream::channel(
                4,
                move |mut channel: cosmic::iced::futures::channel::mpsc::Sender<Message>| async move {
                    use cosmic::iced::futures::SinkExt;
                    loop {
                        let sessions = scanner::scan_all();
                        _ = channel.send(Message::Refresh(sessions)).await;
                        tokio::time::sleep(std::time::Duration::from_secs(3)).await;
                    }
                },
            )
        });

        // 2. Toplevel window watcher.
        let toplevel_sub = Subscription::run(|| {
            cosmic::iced::stream::channel(
                8,
                move |mut channel: cosmic::iced::futures::channel::mpsc::Sender<Message>| async move {
                    use cosmic::iced::futures::SinkExt;
                    use futures::StreamExt;

                    let Some((mut event_rx, action_tx)) = toplevel::spawn() else {
                        // Can't connect to Wayland — just hang forever.
                        futures::future::pending::<()>().await;
                        unreachable!()
                    };

                    // Send the action_tx as part of the first message so the app
                    // can store it. We smuggle it via a dummy ToplevelUpdate.
                    // Actually, we can't send the Sender through iced messages easily.
                    // Instead, we'll use a static. (This is what cosmic-workspaces does.)
                    TOPLEVEL_TX.lock().unwrap().replace(action_tx);

                    while let Some(events) = event_rx.next().await {
                        let mut msgs = Vec::new();
                        // Update handle cache and build messages.
                        {
                            let mut hcache = HANDLE_CACHE.lock().unwrap();
                            for e in events {
                                match e {
                                    ToplevelEvent::Update(info) => {
                                        let proto_id = info.foreign_toplevel.id().protocol_id();
                                        let handle = info.foreign_toplevel.clone();
                                        // Upsert handle cache.
                                        if let Some(entry) = hcache.iter_mut().find(|(id, _)| *id == proto_id) {
                                            entry.1 = handle;
                                        } else {
                                            hcache.push((proto_id, handle));
                                        }
                                        msgs.push(ToplevelEventMsg::Update {
                                            app_id: info.app_id.clone(),
                                            title: info.title.clone(),
                                            proto_id,
                                        });
                                    }
                                    ToplevelEvent::Remove(handle) => {
                                        let proto_id = handle.id().protocol_id();
                                        hcache.retain(|(id, _)| *id != proto_id);
                                        msgs.push(ToplevelEventMsg::Remove(proto_id));
                                    }
                                }
                            }
                        }
                        _ = channel.send(Message::ToplevelUpdate(msgs)).await;
                    }
                },
            )
        });

        Subscription::batch(vec![scanner_sub, toplevel_sub])
    }

    // ── Update ───────────────────────────────────────────────────────

    fn update(&mut self, message: Self::Message) -> Task<cosmic::Action<Self::Message>> {
        match message {
            Message::Refresh(sessions) => {
                self.sessions = sessions;
                // Pick up the toplevel_tx if the background thread set it.
                if self.toplevel_tx.is_none() {
                    if let Ok(mut guard) = TOPLEVEL_TX.lock() {
                        self.toplevel_tx = guard.take();
                    }
                }
            }
            Message::ToplevelUpdate(events) => {
                for event in events {
                    match event {
                        ToplevelEventMsg::Update { app_id, title, proto_id } => {
                            update_window_cache(app_id, title, proto_id);
                        }
                        ToplevelEventMsg::Remove(proto_id) => {
                            if let Ok(mut cache) = WINDOW_CACHE.lock() {
                                cache.retain(|e| e.proto_id != proto_id);
                            }
                        }
                    }
                }
            }
            Message::Launch(kind) => {
                let (cmd, args) = kind.spawn_args();
                let _ = Command::new(cmd).args(args).spawn();
            }
            Message::Kill(pid) => {
                let _ = signal::kill(Pid::from_raw(pid as i32), Signal::SIGTERM);
            }
            Message::Focus(pid) => {
                if let Some(pid) = pid {
                    self.activate_window_for_pid(pid);
                }
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
                        .max_width(480.0)
                        .min_width(360.0)
                        .min_height(100.0)
                        .max_height(800.0);
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

// ── Toplevel matching & activation ───────────────────────────────────

use std::sync::Mutex;

/// Static channel sender so the subscription can hand it to the app.
static TOPLEVEL_TX: Mutex<Option<sctk::reexports::calloop::channel::Sender<ToplevelAction>>> =
    Mutex::new(None);

/// Lightweight window info cache (since we can't keep ToplevelInfo handles
/// across iced message boundaries easily).
#[derive(Debug, Clone)]
struct WindowEntry {
    app_id: String,
    title: String,
    proto_id: u32,
}

/// We actually store WindowEntry in a separate vec since ToplevelInfo isn't
/// easily cloneable across threads. Let's replace the toplevels field.
/// For now, we keep a simple Vec<WindowEntry> as our cache.

static WINDOW_CACHE: Mutex<Vec<WindowEntry>> = Mutex::new(Vec::new());

/// Also store the foreign toplevel handles so we can send activate commands.
static HANDLE_CACHE: Mutex<Vec<(u32, ExtForeignToplevelHandleV1)>> = Mutex::new(Vec::new());

fn update_window_cache(app_id: String, title: String, proto_id: u32) {
    if let Ok(mut cache) = WINDOW_CACHE.lock() {
        if let Some(entry) = cache.iter_mut().find(|e| e.proto_id == proto_id) {
            entry.app_id = app_id;
            entry.title = title;
        } else {
            cache.push(WindowEntry { app_id, title, proto_id });
        }
    }
}

impl Sessions {
    /// Find the Ghostty window for a given session PID and activate it.
    fn activate_window_for_pid(&self, session_pid: u32) {
        let Some(tx) = &self.toplevel_tx else {
            return;
        };

        // Find the Ghostty parent PID for this session.
        let _ghostty_pid = find_ghostty_ancestor(session_pid).unwrap_or(session_pid);

        // Match against window titles. Ghostty window titles often contain the
        // cwd or running command. We try multiple matching strategies:
        let cwd = scanner::proc_cwd_str(session_pid);

        let cache = WINDOW_CACHE.lock().unwrap_or_else(|e| e.into_inner());
        let handle_cache = HANDLE_CACHE.lock().unwrap_or_else(|e| e.into_inner());

        // Strategy 1: find a Ghostty window whose title contains the cwd.
        let matched = cache.iter().find(|w| {
            is_ghostty_app_id(&w.app_id)
                && cwd
                    .as_ref()
                    .map_or(false, |c| w.title.contains(c.as_str()))
        });

        // Strategy 2: match any Ghostty window if there's only one.
        let matched = matched.or_else(|| {
            let ghostty_windows: Vec<_> = cache
                .iter()
                .filter(|w| is_ghostty_app_id(&w.app_id))
                .collect();
            if ghostty_windows.len() == 1 {
                Some(ghostty_windows[0])
            } else {
                None
            }
        });

        // Strategy 3: find by title containing "claude" or "codex".
        let matched = matched.or_else(|| {
            let bin = scanner::proc_bin_name_pub(session_pid);
            bin.and_then(|name| {
                cache.iter().find(|w| {
                    is_ghostty_app_id(&w.app_id)
                        && w.title.to_lowercase().contains(&name.to_lowercase())
                })
            })
        });

        if let Some(window) = matched {
            if let Some((_, handle)) = handle_cache
                .iter()
                .find(|(id, _)| *id == window.proto_id)
            {
                let _ = tx.send(ToplevelAction::Activate(handle.clone()));
            }
        }
    }
}

fn is_ghostty_app_id(app_id: &str) -> bool {
    app_id.contains("ghostty") || app_id.contains("Ghostty")
}

/// Walk /proc ppid chain to find a ghostty ancestor.
fn find_ghostty_ancestor(pid: u32) -> Option<u32> {
    let mut current = pid;
    for _ in 0..10 {
        let ppid = scanner::proc_ppid_pub(current)?;
        if ppid <= 1 {
            return None;
        }
        if let Some(name) = scanner::proc_bin_name_pub(ppid) {
            if name == "ghostty" {
                return Some(ppid);
            }
        }
        current = ppid;
    }
    None
}

// ── Session row widget ───────────────────────────────────────────────

fn session_row<'a>(session: &'a SessionInfo) -> Element<'a, Message> {
    let cwd = session.cwd.as_deref().unwrap_or("?");
    let activity = session.activity.label();

    // Build the detail line: "activity · model · branch · uptime"
    let mut parts: Vec<String> = vec![activity];

    if let Some(model) = &session.model {
        parts.push(shorten_model(model));
    }

    if let Some(branch) = &session.git_branch {
        if !branch.is_empty() {
            parts.push(format!("⌥ {branch}"));
        }
    }

    if let Some(uptime) = &session.uptime {
        parts.push(scanner::format_duration(uptime));
    }

    if let Some(tokens) = session.token_count {
        if tokens > 0 {
            parts.push(format_tokens(tokens));
        }
    }

    let detail = parts.join(" · ");

    // Summary line (truncated).
    let summary_text = session
        .summary
        .as_deref()
        .map(|s| truncate(s, 50))
        .unwrap_or_default();

    // Compose: cwd on top, detail below, summary in between if present.
    let mut col = widget::column![widget::text::body(cwd),];

    if !summary_text.is_empty() {
        col = col.push(widget::text::caption(summary_text));
    }

    col = col.push(widget::text::caption(detail));
    col = col.spacing(2);

    let pid = session.pid;

    // Make the whole row clickable to focus the window.
    let focus_btn = widget::button::custom(col.width(Length::Fill))
        .on_press(Message::Focus(pid))
        .class(cosmic::theme::Button::Text);

    let row_content: Element<'_, Message> = focus_btn.into();

    // Kill button.
    let action: Element<'_, Message> = if let Some(pid) = session.pid {
        widget::button::icon(widget::icon::from_name("window-close-symbolic"))
            .on_press(Message::Kill(pid))
            .into()
    } else {
        widget::Space::new().into()
    };

    widget::settings::item_row(vec![row_content, action]).into()
}

// ── Helpers ──────────────────────────────────────────────────────────

fn shorten_model(model: &str) -> String {
    if let Some(rest) = model.strip_prefix("claude-") {
        rest.to_string()
    } else {
        model.to_string()
    }
}

fn format_tokens(tokens: u64) -> String {
    if tokens >= 1_000_000 {
        format!("{:.1}M tok", tokens as f64 / 1_000_000.0)
    } else if tokens >= 1_000 {
        format!("{:.0}K tok", tokens as f64 / 1_000.0)
    } else {
        format!("{tokens} tok")
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let truncated: String = s.chars().take(max).collect();
        format!("{truncated}...")
    }
}
