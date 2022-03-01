use std::{f64::consts::E, ops::RangeInclusive};

use eframe::{
    egui::{
        global_dark_light_mode_switch,
        plot::{Corner, Legend, Line, Plot, Text, Value, Values},
        CentralPanel, Context, DragValue, Label, RichText, ScrollArea, SidePanel, TextStyle,
        TopBottomPanel, Window,
    },
    emath::Align2,
    epaint::{Color32, Vec2},
    epi::{App as EApp, Frame},
};

use nm_math::{
    expression::{Expr, MathConst},
    method::MethodEquation,
    variable::OpType,
};

use crate::storage::Storage;

const GRAPH_PRECISION_RANGE: RangeInclusive<usize> = 10..=10000;

pub struct App {
    eq_storage: Storage<MethodEquation>,

    // UI
    full_width: bool,
    storage_viewer: bool,
    graph_viewer: bool,

    // Graph Viewer
    precision: usize,
    square_view: bool,
    proportional: bool,
}

impl Default for App {
    fn default() -> Self {
        Self {
            eq_storage: vec![
                MethodEquation::Internal(Box::new(|x: OpType| x + E.powf(x))),
                MethodEquation::Math(
                    Expr::var("x") + Expr::MathConst(MathConst::E).pow(Expr::var("x")),
                ),
                test(),
            ]
            .into(),
            full_width: false,
            storage_viewer: false,
            graph_viewer: false,
            precision: 500,
            square_view: false,
            proportional: false,
        }
    }
}

fn test() -> MethodEquation {
    MethodEquation::Math(Expr::MathConst(MathConst::E).pow(Expr::var("x")).sin())
}

impl EApp for App {
    fn update(&mut self, ctx: &Context, _frame: &Frame) {
        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            ui.horizontal_wrapped(|ui| {
                global_dark_light_mode_switch(ui);
                #[cfg(target_arch = "wasm32")]
                if ui
                    .selectable_label(self.full_width, "🖵")
                    .on_hover_text("Full width mode")
                    .clicked()
                {
                    self.full_width = !self.full_width;
                }
                ui.separator();
                if ui
                    .selectable_label(self.storage_viewer, "Equations Storage")
                    .clicked()
                {
                    self.storage_viewer = !self.storage_viewer;
                };
                if ui
                    .selectable_label(self.graph_viewer, "Graph Viewer")
                    .clicked()
                {
                    self.graph_viewer = !self.graph_viewer;
                };
            })
        });

        Window::new("Equations Storage")
            .open(&mut self.storage_viewer)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    if ui.button("Add").clicked() {
                        self.eq_storage.content.push(MethodEquation::Math(
                            Expr::var("x") + Expr::MathConst(MathConst::E).pow(Expr::var("x")),
                        ));
                    }
                });
                let row_height = ui.spacing().interact_size.y;
                ScrollArea::both()
                    .max_height(256.0)
                    .max_width(512.0)
                    .auto_shrink([false; 2])
                    .show_rows(
                        ui,
                        row_height,
                        self.eq_storage.content.len(),
                        |scroll, range| {
                            for i in range {
                                if let None = self.eq_storage.content.get(i) {
                                    continue;
                                }

                                let is_selected = {
                                    if let Some(id) = self.eq_storage.selected {
                                        if id == i {
                                            true
                                        } else {
                                            false
                                        }
                                    } else {
                                        false
                                    }
                                };

                                scroll.horizontal(|wrap| {
                                    if wrap
                                        .radio(
                                            is_selected,
                                            RichText::new(format!(
                                                "{}",
                                                self.eq_storage.content[i]
                                            ))
                                            .size(32.0),
                                        )
                                        .clicked()
                                    {
                                        self.eq_storage.selected = Some(i);
                                    }
                                    if wrap.button("🗑").clicked() {
                                        self.eq_storage.remove(i);
                                    }
                                });
                            }
                        },
                    )
            });

        Window::new("Graph Viewer")
            .open(&mut self.graph_viewer)
            .show(ctx, |ui| {
                SidePanel::show_inside(
                    SidePanel::left("graph_panel").resizable(false),
                    ui,
                    |panel| {
                        panel.horizontal(|wrap| {
                            wrap.label(RichText::new("Info").text_style(TextStyle::Heading))
                        });
                        panel.separator();
                        if let Some(id) = self.eq_storage.selected {
                            panel.label(&format!("Selected function: #{id}"));
                            ScrollArea::horizontal().show(panel, |scroll| {
                                scroll.add(
                                    Label::new(
                                        RichText::new(format!("{}", self.eq_storage.content[id]))
                                            .text_style(TextStyle::Monospace)
                                            .size(24.0),
                                    )
                                    .wrap(false),
                                )
                            });
                        } else {
                            panel.label("No function selected");
                        }
                        panel.add_space(16.0);
                        panel.horizontal(|wrap| {
                            wrap.label(RichText::new("Options").text_style(TextStyle::Heading))
                        });
                        panel.separator();
                        panel.add(
                            DragValue::new(&mut self.precision)
                                .prefix("Precision: ")
                                .suffix(" points")
                                .clamp_range(GRAPH_PRECISION_RANGE)
                                .speed(10.0),
                        );
                        panel.checkbox(&mut self.square_view, "Square View");
                        panel.checkbox(&mut self.proportional, "Proportional");
                    },
                );

                // ---------------------------------------------------------------------------------

                let mut plot = Plot::new("graph")
                    .legend(
                        Legend::default()
                            .position(Corner::RightBottom)
                            .text_style(TextStyle::Monospace),
                    )
                    .min_size(Vec2::new(256.0, 256.0));

                if self.square_view {
                    plot = plot.view_aspect(1.0);
                }
                if self.proportional {
                    plot = plot.data_aspect(1.0);
                }

                plot.show(ui, |plot| match self.eq_storage.get_selected() {
                    Some(eq) => {
                        let eqc = eq.clone();
                        plot.line(
                            Line::new(Values::from_explicit_callback(
                                move |x| eqc.eval(x).expect("Unreachable"),
                                f64::NEG_INFINITY..f64::INFINITY,
                                self.precision,
                            ))
                            .highlight(true)
                            .name("Equation"),
                        )
                    }
                    None => plot.text(
                        Text::new(Value::new(0.0, 0.0), RichText::new("Error").size(32.0))
                            .highlight(true)
                            .color(Color32::RED)
                            .anchor(Align2::CENTER_CENTER),
                    ),
                })
            });

        CentralPanel::default().show(ctx, |_| {});
    }

    fn name(&self) -> &str {
        "nm-labs"
    }

    fn max_size_points(&self) -> Vec2 {
        if self.full_width {
            Vec2::new(f32::INFINITY, f32::INFINITY)
        } else {
            Vec2::new(1024.0, 2048.0)
        }
    }
}
