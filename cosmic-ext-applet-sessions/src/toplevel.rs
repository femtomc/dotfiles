//! Background Wayland thread for toplevel window enumeration and activation.
//!
//! Uses zcosmic_toplevel_manager_v1 to list and activate Ghostty windows.
//! Pattern taken from pop-launcher's cosmic_toplevel plugin.

use std::collections::HashSet;

use cctk::{
    cosmic_protocols,
    toplevel_info::{ToplevelInfo, ToplevelInfoHandler, ToplevelInfoState},
    toplevel_management::{ToplevelManagerHandler, ToplevelManagerState},
    wayland_client::{self, WEnum},
    wayland_protocols::ext::foreign_toplevel_list::v1::client::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
};
use cosmic_protocols::{
    toplevel_info::v1::client::zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1,
    toplevel_management::v1::client::zcosmic_toplevel_manager_v1,
};
use futures::channel::mpsc;
use sctk::{
    reexports::{calloop, calloop_wayland_source::WaylandSource},
    registry::{ProvidesRegistryState, RegistryState},
    seat::{SeatHandler, SeatState},
};
use wayland_client::{
    Connection, QueueHandle,
    globals::registry_queue_init,
    protocol::wl_seat::WlSeat,
};

// ── Public types ─────────────────────────────────────────────────────

/// Commands we can send to the Wayland thread.
#[derive(Debug)]
pub enum ToplevelAction {
    Activate(ExtForeignToplevelHandleV1),
}

/// Updates from the Wayland thread.
pub enum ToplevelEvent {
    Update(ToplevelInfo),
    Remove(ExtForeignToplevelHandleV1),
}

// ── Wayland thread state ─────────────────────────────────────────────

struct AppData {
    exit: bool,
    tx: mpsc::UnboundedSender<Vec<ToplevelEvent>>,
    registry_state: RegistryState,
    toplevel_info_state: ToplevelInfoState,
    toplevel_manager_state: ToplevelManagerState,
    seat_state: SeatState,
    pending_update: HashSet<ExtForeignToplevelHandleV1>,
}

impl AppData {
    fn cosmic_toplevel_for_foreign(
        &self,
        foreign: &ExtForeignToplevelHandleV1,
    ) -> Option<&ZcosmicToplevelHandleV1> {
        self.toplevel_info_state
            .info(foreign)?
            .cosmic_toplevel
            .as_ref()
    }
}

impl ProvidesRegistryState for AppData {
    fn registry(&mut self) -> &mut RegistryState {
        &mut self.registry_state
    }
    sctk::registry_handlers!();
}

impl SeatHandler for AppData {
    fn seat_state(&mut self) -> &mut SeatState {
        &mut self.seat_state
    }
    fn new_seat(&mut self, _: &Connection, _: &QueueHandle<Self>, _: WlSeat) {}
    fn new_capability(&mut self, _: &Connection, _: &QueueHandle<Self>, _: WlSeat, _: sctk::seat::Capability) {}
    fn remove_capability(&mut self, _: &Connection, _: &QueueHandle<Self>, _: WlSeat, _: sctk::seat::Capability) {}
    fn remove_seat(&mut self, _: &Connection, _: &QueueHandle<Self>, _: WlSeat) {}
}

impl ToplevelManagerHandler for AppData {
    fn toplevel_manager_state(&mut self) -> &mut ToplevelManagerState {
        &mut self.toplevel_manager_state
    }
    fn capabilities(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        _: Vec<WEnum<zcosmic_toplevel_manager_v1::ZcosmicToplelevelManagementCapabilitiesV1>>,
    ) {}
}

impl ToplevelInfoHandler for AppData {
    fn toplevel_info_state(&mut self) -> &mut ToplevelInfoState {
        &mut self.toplevel_info_state
    }

    fn new_toplevel(&mut self, _: &Connection, _: &QueueHandle<Self>, toplevel: &ExtForeignToplevelHandleV1) {
        self.pending_update.insert(toplevel.clone());
    }

    fn update_toplevel(&mut self, _: &Connection, _: &QueueHandle<Self>, toplevel: &ExtForeignToplevelHandleV1) {
        self.pending_update.insert(toplevel.clone());
    }

    fn toplevel_closed(&mut self, _: &Connection, _: &QueueHandle<Self>, toplevel: &ExtForeignToplevelHandleV1) {
        self.pending_update.insert(toplevel.clone());
    }

    fn info_done(&mut self, _: &Connection, _: &QueueHandle<Self>) {
        let events: Vec<ToplevelEvent> = self
            .pending_update
            .drain()
            .map(|handle| match self.toplevel_info_state.info(&handle) {
                Some(info) => ToplevelEvent::Update(info.clone()),
                None => ToplevelEvent::Remove(handle),
            })
            .collect();
        let _ = self.tx.unbounded_send(events);
    }
}

// ── Public API ───────────────────────────────────────────────────────

/// Spawn the Wayland background thread. Returns:
/// - A receiver for toplevel updates
/// - A sender for activation commands
/// - The list of toplevel handles (maintained by the caller)
pub fn spawn() -> Option<(
    mpsc::UnboundedReceiver<Vec<ToplevelEvent>>,
    calloop::channel::Sender<ToplevelAction>,
)> {
    let (event_tx, event_rx) = mpsc::unbounded();
    let (action_tx, action_rx) = calloop::channel::channel();

    std::thread::spawn(move || {
        if let Err(e) = run_wayland_loop(event_tx, action_rx) {
            eprintln!("toplevel wayland thread exited: {e}");
        }
    });

    Some((event_rx, action_tx))
}

fn run_wayland_loop(
    tx: mpsc::UnboundedSender<Vec<ToplevelEvent>>,
    rx: calloop::channel::Channel<ToplevelAction>,
) -> anyhow::Result<()> {
    let conn = Connection::connect_to_env()?;
    let (globals, event_queue) = registry_queue_init(&conn)?;
    let mut event_loop = calloop::EventLoop::<AppData>::try_new()?;
    let qh = event_queue.handle();
    let wayland_source = WaylandSource::new(conn, event_queue);
    let handle = event_loop.handle();

    handle
        .insert_source(wayland_source, |_, q, state| q.dispatch_pending(state))
        .map_err(|e| anyhow::anyhow!("failed to insert wayland source: {}", e.error))?;

    let _ = handle.insert_source(rx, |event, _, state| match event {
        calloop::channel::Event::Msg(ToplevelAction::Activate(foreign)) => {
            if let Some(cosmic_toplevel) = state.cosmic_toplevel_for_foreign(&foreign) {
                for seat in state.seat_state.seats() {
                    state
                        .toplevel_manager_state
                        .manager
                        .activate(cosmic_toplevel, &seat);
                }
            }
        }
        calloop::channel::Event::Closed => {
            state.exit = true;
        }
    });

    let registry_state = RegistryState::new(&globals);
    let mut app_data = AppData {
        exit: false,
        tx,
        seat_state: SeatState::new(&globals, &qh),
        toplevel_info_state: ToplevelInfoState::new(&registry_state, &qh),
        toplevel_manager_state: ToplevelManagerState::new(&registry_state, &qh),
        registry_state,
        pending_update: HashSet::new(),
    };

    loop {
        if app_data.exit {
            break Ok(());
        }
        event_loop.dispatch(None, &mut app_data)?;
    }
}

sctk::delegate_seat!(AppData);
sctk::delegate_registry!(AppData);
cctk::delegate_toplevel_info!(AppData);
cctk::delegate_toplevel_manager!(AppData);
