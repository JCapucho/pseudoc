use std::path::PathBuf;

use eframe::{egui, epi};

#[derive(Debug, PartialEq, Eq)]
enum Backend {
    Pseudo,
    Pascal,
    Dot,
}

impl Default for Backend {
    fn default() -> Self { Backend::Pseudo }
}

fn toolbar(ui: &mut egui::Ui, app: &mut App) {
    if ui.button("Load file").clicked() {
        if let Some(path) = rfd::FileDialog::new()
            .set_title("pseudoc file to load")
            .pick_file()
        {
            match std::fs::read_to_string(&path) {
                Ok(loaded) => app.source = loaded,
                Err(e) => {
                    eprintln!("Failed to read file at {:?}: {}", path, e)
                },
            }
        }
    }

    egui::ComboBox::from_id_source("Backend")
        .selected_text(format!("{:?}", app.backend))
        .show_ui(ui, |ui| {
            ui.selectable_value(&mut app.backend, Backend::Pseudo, "Pseudo");
            ui.selectable_value(&mut app.backend, Backend::Pascal, "Pascal");
            ui.selectable_value(&mut app.backend, Backend::Dot, "Dot");
        });

    if ui.button("Output").clicked() {
        if let Some(path) = rfd::FileDialog::new().set_title("Output file").save_file() {
            let mut options = std::fs::OpenOptions::new();
            options.write(true).create(true).truncate(true);
            match options.open(&path) {
                Ok(mut file) => {
                    let mut rodeo = pseudoc::new_rodeo();
                    let parse_result = pseudoc::build_parse(&app.source, &mut rodeo);
                    let resolver = rodeo.into_resolver();
                    let result = parse_result.and_then(|parse| match app.backend {
                        Backend::Pseudo => {
                            pseudoc::build_pseudo(&parse, &resolver, "editor", &mut file)
                        },
                        Backend::Pascal => {
                            pseudoc::build_pascal(&parse, &resolver, "editor", &mut file)
                        },
                        Backend::Dot => pseudoc::build_dot(&parse, &resolver, &mut file),
                    });
                },
                Err(e) => {
                    eprintln!("Failed to open file at {:?}: {}", path, e)
                },
            }
        }
    }

    if let Backend::Dot = app.backend {
        if ui.button("Dot executable").clicked() {
            if let Some(path) = rfd::FileDialog::new()
                .set_title("Dot executable")
                .pick_file()
            {
                app.dot_exe = Some(path);
            }
        }

        if ui
            .add_enabled(app.dot_exe.is_some(), egui::Button::new("Output png"))
            .clicked()
        {
            if let Some(path) = rfd::FileDialog::new().set_title("Output file").save_file() {
                let mut child = std::process::Command::new(app.dot_exe.as_ref().unwrap())
                    .stdin(std::process::Stdio::piped())
                    .args([
                        AsRef::<std::ffi::OsStr>::as_ref("-o"),
                        path.as_os_str(),
                        AsRef::<std::ffi::OsStr>::as_ref("-Tpng"),
                    ])
                    .spawn()
                    .expect("Failed to spawn child process");

                let mut rodeo = pseudoc::new_rodeo();
                let parse_result = pseudoc::build_parse(&app.source, &mut rodeo);
                let resolver = rodeo.into_resolver();
                let mut stdin = child.stdin.take().expect("Failed to open stdin");
                parse_result.and_then(|parse| pseudoc::build_dot(&parse, &resolver, &mut stdin));
            }
        }
    }
}

#[derive(Default)]
struct App {
    source: String,
    backend: Backend,
    dot_exe: Option<PathBuf>,
}

impl epi::App for App {
    fn name(&self) -> &str { "pseudoc" }

    fn update(&mut self, ctx: &egui::CtxRef, _: &mut epi::Frame<'_>) {
        egui::TopBottomPanel::top("toobar").show(ctx, |ui| {
            ui.horizontal(|ui| {
                toolbar(ui, self);
            })
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.add(
                    egui::TextEdit::multiline(&mut self.source)
                        .code_editor()
                        .interactive(false)
                        .desired_width(f32::INFINITY),
                )
            });
        });
    }
}

fn main() { eframe::run_native(Box::new(App::default()), Default::default()) }
