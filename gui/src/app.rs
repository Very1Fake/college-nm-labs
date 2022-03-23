use core::{f64::consts::E, ops::RangeInclusive};

use eframe::{
    egui::{
        global_dark_light_mode_switch,
        plot::{Corner, Legend, Line, LineStyle, Plot, PlotUi, Points, VLine, Value, Values},
        CentralPanel, ComboBox, Context, DragValue, Label, RichText, ScrollArea, SidePanel,
        TextEdit, TextStyle, TopBottomPanel, Window,
    },
    epaint::{Color32, Vec2},
    epi::{App as EApp, Frame},
};

use nm_math::{
    expression::{Expr, MathConst},
    method::{CallStats, Interval, Method, MethodEquation, OutPut},
    parser::parse,
};

use crate::storage::Storage;

const INFINITE_INTERVAL: (f64, f64) = (f64::NEG_INFINITY, f64::INFINITY);
const METHOD_INTERVAL_LIMIT_RANGE: RangeInclusive<f64> = f64::NEG_INFINITY..=f64::INFINITY;
const METHOD_PRECISION_RANGE: RangeInclusive<i32> = 0..=24;
const METHOD_ITER_LIMIT_RANGE: RangeInclusive<usize> = 1..=usize::MAX;
const GRAPH_PRECISION_RANGE: RangeInclusive<usize> = 10..=10000;

#[derive(PartialEq, Clone)]
pub enum MethodKind {
    Simple,
    Derivative,
    Bisection,
}

impl MethodKind {
    const ALL: [Self; 3] = [Self::Simple, Self::Derivative, Self::Bisection];

    pub fn as_str(&self) -> &str {
        use MethodKind::*;

        match self {
            Simple => "Simple",
            Derivative => "Derivative",
            Bisection => "Bisection",
        }
    }
}

impl Default for MethodKind {
    fn default() -> Self {
        Self::Simple
    }
}

pub enum MethodResult {
    // Simple function. Stores: equation, interval
    Simple(MethodEquation),
    // Derivative of given function. Stores: equation, interval
    Derivative(MethodEquation),
    // Bisection method. Stores: equation, interval, root point
    Bisection(MethodEquation, Interval, (f64, f64)),
}

impl MethodResult {
    pub fn as_str(&self) -> &str {
        use MethodResult::*;

        match self {
            Simple(_) => "Simple",
            Derivative(_) => "Derivative",
            Bisection(..) => "Bisection",
        }
    }
}

// -------------------------------------------------------------------------------------------------

pub struct App {
    eq_storage: Storage<MethodEquation>,
    method_result: Option<MethodResult>,

    // UI
    full_width: bool,

    // Equations
    eq_viewer: bool,
    eq_input: String,
    eq_input_error: Option<String>,

    // Method Run
    m_viewer: bool,
    m_kind: MethodKind,
    m_interval: Interval,
    m_precision: i32,
    m_limit: usize,
    m_verbose: bool,
    m_stats: Option<CallStats>,
    m_error: Option<String>,

    // Graph Viewer
    graph_viewer: bool,
    precision: usize,
    square_view: bool,
    proportional: bool,

    // Log Viewer
    log_viewer: bool,
    log: Vec<String>,
}

impl Default for App {
    fn default() -> Self {
        Self {
            eq_storage: vec![
                MethodEquation::Internal(Box::new(|x: f64| x + E.powf(x))),
                MethodEquation::Math(
                    Expr::var("x") + Expr::MathConst(MathConst::E).pow(Expr::var("x")),
                ),
                test(),
            ]
            .into(),
            method_result: None,
            full_width: false,
            eq_viewer: false,
            eq_input: String::new(),
            eq_input_error: None,
            m_viewer: false,
            m_kind: Default::default(),
            m_interval: (-1.0, 1.0),
            m_precision: 3,
            m_limit: 1000,
            m_verbose: false,
            m_stats: None,
            m_error: None,
            graph_viewer: false,
            precision: 500,
            square_view: false,
            proportional: false,
            log_viewer: false,
            log: Vec::new(),
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
                if ui.selectable_label(self.eq_viewer, "Equations").clicked() {
                    self.eq_viewer = !self.eq_viewer;
                };
                if ui.selectable_label(self.m_viewer, "Method").clicked() {
                    self.m_viewer = !self.m_viewer;
                };
                if ui.selectable_label(self.graph_viewer, "Graph").clicked() {
                    self.graph_viewer = !self.graph_viewer;
                };
                if ui.selectable_label(self.log_viewer, "Log").clicked() {
                    self.log_viewer = !self.log_viewer;
                };
            })
        });

        Window::new("Equations")
            .open(&mut self.eq_viewer)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    if ui.button(RichText::new("Clear").size(20.0)).clicked() {
                        self.eq_storage.clear();
                    }
                    ui.collapsing(RichText::new("Add new ").size(20.0), |col| {
                        if let Some(err) = &self.eq_input_error {
                            col.label(RichText::new(err).size(16.0).color(Color32::RED));
                        }
                        col.add(
                            TextEdit::singleline(&mut self.eq_input)
                                .font(TextStyle::Heading)
                                .hint_text("example: 'x^2'"),
                        );
                        if col.button("Add").clicked() {
                            self.eq_input_error = None;
                            match parse(&self.eq_input) {
                                Ok(expr) => {
                                    self.eq_storage.content.push(MethodEquation::Math(expr));
                                    self.eq_input.clear();
                                    self.eq_input.shrink_to_fit();
                                }
                                Err(err) => self.eq_input_error = Some(err.to_string()),
                            }
                        }
                    });
                });
                ui.separator();
                ScrollArea::both()
                    .max_height(256.0)
                    .max_width(512.0)
                    .auto_shrink([false; 2])
                    .show_rows(
                        ui,
                        ui.spacing().interact_size.y,
                        self.eq_storage.content.len(),
                        |scroll, range| {
                            for i in range {
                                if self.eq_storage.content.get(i).is_none() {
                                    continue;
                                }

                                let is_selected = {
                                    if let Some(id) = self.eq_storage.selected {
                                        id == i
                                    } else {
                                        false
                                    }
                                };

                                scroll.horizontal(|wrap| {
                                    let eq_label = wrap.radio(
                                        is_selected,
                                        RichText::new(format!("{}", self.eq_storage.content[i]))
                                            .size(28.0),
                                    );
                                    if eq_label.clicked() {
                                        self.eq_storage.selected = Some(i);
                                    }
                                    if eq_label.middle_clicked() {
                                        self.eq_storage.remove(i);
                                    }
                                });
                            }
                        },
                    )
            });

        Window::new("Method")
            .open(&mut self.m_viewer)
            .show(ctx, |ui| {
                let equation = self.eq_storage.get_selected();

                if let Some((id, eq)) = equation {
                    let eq = eq.clone();
                    ui.horizontal(|hor| {
                        hor.vertical(|vert| {
                            vert.label(format!("Selected equation #{id}"));

                            ScrollArea::horizontal()
                                .max_width(256.0)
                                .show(vert, |scroll| {
                                    scroll.add(
                                        Label::new(
                                            RichText::new(format!("{}", eq))
                                                .text_style(TextStyle::Monospace)
                                                .size(20.0),
                                        )
                                        .wrap(false),
                                    )
                                });

                            vert.add_space(8.0);

                            if let Some(err) = &self.m_error {
                                vert.label(RichText::new(err).size(16.0).color(Color32::RED));
                                vert.add_space(8.0);
                            }

                            vert.checkbox(&mut self.m_verbose, "Verbose output");
                            if vert.button("Calculate").clicked() {
                                self.m_stats = None;
                                self.m_error = None;
                                self.method_result = match self.m_kind {
                                    MethodKind::Simple => Some(MethodResult::Simple(eq.clone())),
                                    MethodKind::Derivative => if let Some(eq) = eq.math() {
                                            match eq.derivative().fix().optimize() {
                                                Ok(result) => Some(MethodResult::Derivative(MethodEquation::Math(result))),
                                                Err(err) => {
                                                    self.log.push(format!("Error while optimizing derivative: '{err}'"));
                                                    self.m_error = Some(err.to_string());
                                                    None
                                                }
                                            }
                                        } else {
                                            self.log.push(format!("Can't differentiate internal rust function"));
                                            self.m_error = Some(String::from("Can't differentiate internal rust function"));
                                            None
                                        },
                                    MethodKind::Bisection => {
                                        let precision =
                                            1.0 / 10.0_f64.powi(self.m_precision as i32);
                                        self.log.extend([
                                            String::new(),
                                            format!("> Bisection ({})", eq),
                                            format!("Precision: {precision}"),
                                            format!("Interval: {:?}", self.m_interval),
                                        ]);

                                        match Method::new(
                                            self.m_limit as u128,
                                            OutPut::Vec(&mut self.log),
                                        )
                                        .verbose(if self.m_verbose { 1 } else { 0 })
                                        .bisection(
                                            eq.clone(),
                                            self.m_interval,
                                            precision,
                                        ) {
                                            Ok(result) => {
                                                self.log.push(format!(
                                                    "Root: x={} (f(x)={})",
                                                    result.root.0, result.root.1
                                                ));
                                                self.m_stats = Some(result.stats);
                                                Some(MethodResult::Bisection(
                                                    eq.clone(),
                                                    self.m_interval,
                                                    result.root,
                                                ))
                                            }
                                            Err(err) => {
                                                self.log.push(format!("Error: '{err}'"));
                                                self.m_error = Some(err.to_string());
                                                None // FIX: check boundaries
                                            }
                                        }
                                    }
                                };
                            }
                        });

                        hor.separator();

                        hor.vertical(|vert| {
                            ComboBox::from_label("Choose method")
                                .selected_text(self.m_kind.as_str())
                                .show_ui(vert, |combo| {
                                    MethodKind::ALL.iter().for_each(|kind| {
                                        combo.selectable_value(
                                            &mut self.m_kind,
                                            kind.clone(),
                                            kind.as_str(),
                                        );
                                    })
                                });

                            vert.collapsing("Options", |col| {
                                col.vertical_centered_justified(|center| {
                                    if matches!(self.m_kind, MethodKind::Bisection) {
                                        center.label("Interval");
                                        center.add(
                                            DragValue::new(&mut self.m_interval.0)
                                                .prefix("a: ")
                                                .clamp_range(METHOD_INTERVAL_LIMIT_RANGE)
                                                .speed(0.1),
                                        );
                                        center.add(
                                            DragValue::new(&mut self.m_interval.1)
                                                .prefix("b: ")
                                                .clamp_range(METHOD_INTERVAL_LIMIT_RANGE)
                                                .speed(0.1),
                                        );
                                    }

                                    if matches!(self.m_kind, MethodKind::Bisection) {
                                        center.label("Method precision");
                                        center.add(
                                            DragValue::new(&mut self.m_precision)
                                                .prefix("Precision: ")
                                                .clamp_range(METHOD_PRECISION_RANGE)
                                                .speed(0.1),
                                        );
                                    }

                                    if matches!(self.m_kind, MethodKind::Bisection) {
                                        center.label("Iterations limit");
                                        center.add(
                                            DragValue::new(&mut self.m_limit)
                                                .prefix("Limit: ")
                                                .suffix(" iters")
                                                .clamp_range(METHOD_ITER_LIMIT_RANGE)
                                                .speed(10),
                                        );
                                    }
                                })
                            });

                            if let Some(stats) = &self.m_stats {
                                vert.collapsing("Last method results", |col| {
                                    col.label(format!("Time elapsed: {:?}", stats.elapsed));
                                    col.label(format!("Iterations: {}", stats.iterations));
                                });
                            }
                        });
                    });

                    if let Some(result) = &self.method_result {
                        if let MethodResult::Simple(..) = &result {
                        } else {
                            ui.separator();
                            ui.label(RichText::new("Result").text_style(TextStyle::Heading));

                            match result {
                                MethodResult::Bisection(_, _, (x, y)) => {
                                    ui.label(
                                        RichText::new(format!("x = {x}; y = {y}"))
                                            .text_style(TextStyle::Monospace),
                                    );
                                }
                                _ => (),
                            }
                        }
                    }
                } else {
                    ui.label("No equation selected");
                }
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
                        if let Some(result) = &self.method_result {
                            panel.label("Method:");
                            panel.label(
                                RichText::new(result.as_str()).text_style(TextStyle::Monospace),
                            );
                            panel.label("Method equation:");
                            ScrollArea::horizontal().show(panel, |scroll| {
                                scroll.add(match result {
                                    MethodResult::Simple(eq)
                                    | MethodResult::Derivative(eq)
                                    | MethodResult::Bisection(eq, ..) => Label::new(
                                        RichText::new(format!("{}", eq))
                                            .text_style(TextStyle::Monospace)
                                            .size(24.0),
                                    )
                                    .wrap(false),
                                })
                            });
                        } else {
                            panel.label("No method has been run");
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

                // DRY
                #[inline]
                fn show_line(
                    plot: &mut PlotUi,
                    eq: MethodEquation,
                    interval: Interval,
                    precision: usize,
                ) {
                    plot.line(
                        Line::new(Values::from_explicit_callback(
                            move |x| eq.eval(x).expect("Unreachable"),
                            interval.0..=interval.1,
                            precision,
                        ))
                        .highlight(true)
                        .name("Equation"),
                    )
                }

                plot.show(ui, |plot| match &self.method_result {
                    Some(result) => match result {
                        MethodResult::Simple(eq) | MethodResult::Derivative(eq) => {
                            show_line(plot, eq.clone(), INFINITE_INTERVAL, self.precision)
                        }
                        MethodResult::Bisection(eq, (a, b), (x, y)) => {
                            show_line(plot, eq.clone(), (a - 1.0, b + 1.0), self.precision);
                            plot.vline(
                                VLine::new(*a)
                                    .style(LineStyle::dashed_loose())
                                    .name("Interval"),
                            );
                            plot.vline(
                                VLine::new(*b)
                                    .style(LineStyle::dashed_loose())
                                    .name("Interval"),
                            );
                            plot.points(
                                Points::new(Values::from_values(vec![Value::new(*x, *y)]))
                                    .radius(5.0)
                                    .name("Root"),
                            )
                        }
                    },
                    None => (),
                })
            });

        Window::new("Log Viewer")
            .open(&mut self.log_viewer)
            .show(ctx, |ui| {
                ui.vertical_centered_justified(|vert| {
                    if vert.button("Clear").clicked() {
                        self.log = Vec::new();
                        self.log.push(String::from("--- Log cleared ---"));
                    }
                });
                ui.separator();
                // BUG: Memory leak
                ScrollArea::both()
                    .auto_shrink([false; 2])
                    .stick_to_bottom()
                    .max_height(512.0)
                    .show_rows(
                        ui,
                        ui.text_style_height(&TextStyle::Monospace),
                        self.log.len(),
                        |scroll, range| {
                            range.for_each(|i| {
                                scroll.add(
                                    Label::new(
                                        RichText::new(&self.log[i])
                                            .text_style(TextStyle::Monospace),
                                    )
                                    .wrap(false),
                                );
                            })
                        },
                    );
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
