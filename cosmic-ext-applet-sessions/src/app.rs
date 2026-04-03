use crate::scanner::{self, SessionInfo, SessionKind};
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
}

impl Default for Sessions {
    fn default() -> Self {
        Self {
            core: cosmic::Core::default(),
            popup: None,
            sessions: Vec::new(),
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

            // Session rows.
            for session in kind_sessions {
                content = content.add(session_row(session));
            }
        }

        self.core.applet.popup_container(content).into()
    }

    // ── Subscription: poll every 3s ──────────────────────────────────

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::run(|| {
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
        })
    }

    // ── Update ───────────────────────────────────────────────────────

    fn update(&mut self, message: Self::Message) -> Task<cosmic::Action<Self::Message>> {
        match message {
            Message::Refresh(sessions) => {
                self.sessions = sessions;
            }
            Message::Launch(kind) => {
                let (cmd, args) = kind.spawn_args();
                let _ = Command::new(cmd).args(args).spawn();
            }
            Message::Kill(pid) => {
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

    let row_content: Element<'_, Message> = col.width(Length::Fill).into();

    // Kill button (only if we have a PID).
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
    // "claude-opus-4-6" -> "opus-4-6", "gpt-5.4" stays as is
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
