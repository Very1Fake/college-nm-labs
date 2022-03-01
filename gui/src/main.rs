// Turn off terminal on Windows OS
#![windows_subsystem = "windows"]
#[cfg(not(target_arch = "wasm32"))]
fn main() {
    use eframe::{run_native, NativeOptions};

    run_native(
        Box::new(nm_gui::app::App::default()),
        NativeOptions::default(),
    )
}
