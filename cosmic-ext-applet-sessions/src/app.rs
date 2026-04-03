use cosmic::iced::window::Id;
use cosmic::iced::{Limits, Subscription};
use cosmic::iced_winit::commands::popup::{destroy_popup, get_popup};
use cosmic::prelude::*;
use cosmic::widget;
use std::collections::HashMap;
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

    fn process_match(self) -> &'static str {
        match self {
            Self::ClaudeCode => "claude",
            Self::Codex => "codex",
            Self::Ghostty => "ghostty",
        }
    }

    const ALL: [SessionKind; 3] = [Self::ClaudeCode, Self::Codex, Self::Ghostty];
}

#[derive(Debug, Clone)]
pub struct SessionInfo {
    pub pid: u32,
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

fn scan_sessions() -> HashMap<SessionKind, Vec<SessionInfo>> {
    let mut map: HashMap<SessionKind, Vec<SessionInfo>> = HashMap::new();
    let Ok(entries) = std::fs::read_dir("/proc") else {
        return map;
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Some(pid) = name.to_str().and_then(|s| s.parse::<u32>().ok()) else {
            continue;
        };
        let cmdline_path = format!("/proc/{pid}/cmdline");
        let Ok(raw) = std::fs::read(&cmdline_path) else {
            continue;
        };
        let cmdline: String = raw
            .split(|&b| b == 0)
            .filter(|s| !s.is_empty())
            .map(|s| String::from_utf8_lossy(s).into_owned())
            .collect::<Vec<_>>()
            .join(" ");

        for kind in SessionKind::ALL {
            let needle = kind.process_match();
            // Match on the binary name in the first argument
            let first_arg = cmdline.split_whitespace().next().unwrap_or("");
            let bin_name = first_arg.rsplit('/').next().unwrap_or(first_arg);
            if bin_name == needle {
                map.entry(kind).or_default().push(SessionInfo { pid });
            }
        }
    }
    map
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
        let total: usize = self.active_sessions.values().map(|v| v.len()).sum();
        if total > 0 {
            self.core
                .applet
                .icon_button_from_handle(
                    widget::icon::from_name("utilities-terminal-symbolic").handle(),
                )
                .on_press(Message::TogglePopup)
                .into()
        } else {
            self.core
                .applet
                .icon_button("utilities-terminal-symbolic")
                .on_press(Message::TogglePopup)
                .into()
        }
    }

    fn view_window(&self, _id: Id) -> Element<'_, Self::Message> {
        let mut content = widget::list_column().padding(5).spacing(0);

        for kind in SessionKind::ALL {
            let sessions = self.active_sessions.get(&kind);
            let count = sessions.map_or(0, |v| v.len());

            // Section header with launch button
            content = content.add(
                widget::settings::item(
                    format!("{} ({})", kind.label(), count),
                    widget::button::icon(widget::icon::from_name("list-add-symbolic"))
                        .on_press(Message::Launch(kind)),
                ),
            );

            // List active sessions
            if let Some(sessions) = sessions {
                for session in sessions {
                    let pid = session.pid;
                    content = content.add(
                        widget::settings::item(
                            format!("  PID {pid}"),
                            widget::button::icon(widget::icon::from_name("window-close-symbolic"))
                                .on_press(Message::Kill(kind, pid)),
                        ),
                    );
                }
            }
        }

        self.core.applet.popup_container(content).into()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::run(|| {
            cosmic::iced::stream::channel(4, move |mut channel: cosmic::iced::futures::channel::mpsc::Sender<Message>| async move {
                use cosmic::iced::futures::SinkExt;
                loop {
                    let sessions = scan_sessions();
                    _ = channel.send(Message::Refresh(sessions)).await;
                    tokio::time::sleep(std::time::Duration::from_secs(3)).await;
                }
            })
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
                unsafe {
                    libc::kill(pid as i32, libc::SIGTERM);
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
