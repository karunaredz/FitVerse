# ============================================================
# FitVerse Shiny Application
# ============================================================
# Description : Interactive web interface for the FitVerse R
#               package.  Users upload data, choose estimation
#               settings, and instantly receive:
#                 - Interactive density + Q-Q diagnostic plot
#                 - Full ranked comparison table with GoF stats
#                 - Descriptive statistics panel
#                 - Optional bootstrap confidence intervals
#                 - Downloadable HTML report and high-res plot
#
# Authors:
#   Karuna G. Reddy <karuna.reddy@auckland.ac.nz>
#   University of Auckland, New Zealand
#
#   M. G. M. Khan <mgm.khan@usp.ac.fj>
#   University of the South Pacific, Fiji
#
# Requires : FitVerse package + shiny, bslib, DT, plotly
# Run with : shiny::runApp("FitVerseApp")
# ============================================================

source("global.R")   # loads FitVerse, bslib, DT, plotly, constants


# +===========================================================+
# |  UI                                                       |
# +===========================================================+

ui <- bslib::page_navbar(

  title = tags$img(
    src    = "logo.svg",
    height = "112px",
    style  = "vertical-align:middle; margin-top:-10px; margin-bottom:-10px; display:block;",
    onerror = "this.style.display='none'"
  ),

  theme = bslib::bs_theme(
    version      = 5,
    bg           = "#f9f8ff",
    fg           = "#0f172a",
    primary      = "#4f46e5",
    secondary    = "#64748b",
    base_font    = bslib::font_google("Plus Jakarta Sans"),
    heading_font = bslib::font_google("Plus Jakarta Sans"),
    `navbar-bg`  = "#1e1b4b"
  ),

  # Include custom CSS
  header = tags$head(
    tags$link(rel = "stylesheet", href = "fitverse.css"),
    # Resize window to 90% of screen on launch
    tags$script(HTML("
      (function() {
        try {
          var w = Math.round(screen.availWidth  * 0.90);
          var h = Math.round(screen.availHeight * 0.90);
          var x = Math.round((screen.availWidth  - w) / 2);
          var y = Math.round((screen.availHeight - h) / 2);
          window.resizeTo(w, h);
          window.moveTo(x, y);
        } catch(e) {}
      })();
    ")),
    # Fixed border overlay — sits above all content, blocks no clicks
    tags$div(
      id = "fv-border-frame",
      style = paste0(
        "position:fixed; top:0; left:0; right:0; bottom:0; ",
        "border:4px solid #1e1b4b; ",
        "box-shadow: inset 0 0 0 1px rgba(109,40,217,0.35); ",
        "pointer-events:none; z-index:99999;"
      )
    ),
    tags$style(HTML("
      /* ============================================================
         GLOBAL -- professional slate background
      ============================================================ */
      body {
        background: linear-gradient(145deg, #f5f3ff 0%, #ede9fe 50%, #faf5ff 100%) !important;
        background-attachment: fixed !important;
        color: #0f172a !important;
        font-family: 'Plus Jakarta Sans', 'Inter', sans-serif !important;
      }

      /* ============================================================
         NAVBAR -- deep midnight, tall, logo-first layout
      ============================================================ */
      .navbar {
        min-height: 106px !important;
        padding: 0 28px !important;
        background: linear-gradient(180deg, #180f42 0%, #1e1b4b 100%) !important;
        box-shadow: 0 2px 24px rgba(0,0,0,0.40) !important;
        border-bottom: 2px solid rgba(124,58,237,0.4) !important;
      }
      .navbar-brand {
        padding: 0 !important;
        margin-right: 32px !important;
        display: flex !important;
        align-items: center !important;
      }
      .navbar-collapse {
        align-items: center !important;
      }
      .navbar-nav {
        gap: 4px !important;
        align-items: center !important;
      }

      /* -- Navbar menu items ----------------------------------- */
      .navbar-nav .nav-link {
        font-family: 'Inter', sans-serif !important;
        font-weight: 600 !important;
        font-size: 0.84rem !important;
        letter-spacing: 0.06em !important;
        text-transform: uppercase !important;
        padding: 0.52rem 1.1rem !important;
        border-radius: 8px !important;
        color: rgba(255,255,255,0.70) !important;
        transition: all 0.18s ease !important;
        white-space: nowrap !important;
      }
      .navbar-nav .nav-link:hover {
        background: rgba(255,255,255,0.10) !important;
        color: #ffffff !important;
        transform: translateY(-1px) !important;
      }
      .navbar-nav .nav-link.active {
        background: linear-gradient(135deg, #6d28d9 0%, #4f46e5 100%) !important;
        color: #ffffff !important;
        font-weight: 700 !important;
        box-shadow: 0 4px 14px rgba(109,40,217,0.50),
                    inset 0 1px 0 rgba(255,255,255,0.20) !important;
        transform: translateY(-1px) !important;
      }

      /* -- Help tab --------------------------------------------- */
      .navbar-nav .nav-link[data-value='Help'] {
        color: rgba(148,191,255,0.75) !important;
        border: 1px solid rgba(148,191,255,0.25) !important;
      }
      .navbar-nav .nav-link[data-value='Help']:hover {
        background: rgba(148,191,255,0.12) !important;
        color: #ffffff !important;
        border-color: rgba(148,191,255,0.50) !important;
      }
      .navbar-nav .nav-link[data-value='Help'].active {
        background: linear-gradient(135deg, #0ea5e9 0%, #0369a1 100%) !important;
        color: #ffffff !important;
        border-color: transparent !important;
      }

      /* -- Version badge ---------------------------------------- */
      .navbar .nav-item span {
        color: rgba(148,191,255,0.55) !important;
        font-size: 0.72rem !important;
      }

      /* ============================================================
         PAGE CONTENT -- light card surfaces on slate background
      ============================================================ */
      .bslib-page-navbar > .tab-content {
        background: transparent !important;
        padding: 20px 24px !important;
      }

      /* ============================================================
         CARDS -- white surface, subtle shadow, clean radius
      ============================================================ */
      .card {
        background: #ffffff !important;
        border: 1px solid #e2e8f0 !important;
        border-radius: 12px !important;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06),
                    0 4px 16px rgba(0,0,0,0.06) !important;
      }
      .card-header {
        background: #ffffff !important;
        border-bottom: 1px solid #e2e8f0 !important;
        border-radius: 12px 12px 0 0 !important;
        font-weight: 700 !important;
        color: #0f172a !important;
        padding: 14px 20px !important;
      }
      .card-body {
        padding: 18px 20px !important;
      }

      /* ============================================================
         SIDEBAR -- soft blue-white, clean border
      ============================================================ */
      .sidebar {
        background: #f5f3ff !important;
        border-right: 1px solid #ddd6fe !important;
        border-radius: 0 !important;
      }
      .sidebar .sidebar-title {
        color: #1e1b4b !important;
        font-weight: 700 !important;
        font-size: 0.75rem !important;
        text-transform: uppercase !important;
        letter-spacing: 0.08em !important;
      }

      /* ============================================================
         INNER CARD TABS (Plot / Rankings / etc.)
      ============================================================ */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0 !important;
        gap: 2px !important;
      }
      .nav-tabs .nav-link {
        font-weight: 600 !important;
        font-size: 0.86rem !important;
        color: #64748b !important;
        border: none !important;
        border-radius: 8px 8px 0 0 !important;
        padding: 0.55rem 1.2rem !important;
        letter-spacing: 0.02em !important;
        transition: all 0.15s ease !important;
        background: transparent !important;
      }
      .nav-tabs .nav-link.active {
        font-weight: 700 !important;
        color: #4f46e5 !important;
        background: #ede9fe !important;
        border-bottom: 3px solid #4f46e5 !important;
        box-shadow: none !important;
      }
      .nav-tabs .nav-link:not(.active):hover {
        color: #4f46e5 !important;
        background: #f0eeff !important;
        border-bottom: 3px solid #c4b5fd !important;
      }

      /* ============================================================
         BUTTONS -- rounded, clean, modern
      ============================================================ */
      .btn {
        border-radius: 8px !important;
        font-weight: 600 !important;
        font-size: 0.84rem !important;
        letter-spacing: 0.02em !important;
        transition: all 0.18s ease !important;
      }
      .btn-primary {
        background: linear-gradient(135deg, #6d28d9 0%, #4f46e5 100%) !important;
        border-color: #4f46e5 !important;
        color: #fff !important;
        box-shadow: 0 2px 8px rgba(109,40,217,0.35) !important;
      }
      .btn-primary:hover {
        filter: brightness(1.08) !important;
        box-shadow: 0 4px 16px rgba(109,40,217,0.50) !important;
        transform: translateY(-1px) !important;
      }
      .btn-outline-primary {
        color: #4f46e5 !important;
        border-color: #4f46e5 !important;
      }
      .btn-outline-primary:hover {
        background: #4f46e5 !important;
        color: #fff !important;
      }
      .btn-success {
        background: linear-gradient(135deg, #22c55e 0%, #15803d 100%) !important;
        border-color: #15803d !important;
        box-shadow: 0 2px 6px rgba(21,128,61,0.30) !important;
      }
      .btn-success:hover { filter: brightness(1.08) !important; transform: translateY(-1px) !important; }
      .btn-outline-success {
        color: #16a34a !important;
        border-color: #16a34a !important;
      }
      .btn-outline-success:hover { background: #16a34a !important; color: #fff !important; }
      .btn-secondary {
        background: linear-gradient(135deg, #64748b 0%, #475569 100%) !important;
        border-color: #475569 !important;
        color: #fff !important;
      }
      .btn-secondary:hover { filter: brightness(1.08) !important; transform: translateY(-1px) !important; }
      .btn-warning {
        background: linear-gradient(135deg, #f59e0b 0%, #b45309 100%) !important;
        border-color: #b45309 !important;
        color: #fff !important;
        box-shadow: 0 2px 6px rgba(180,83,9,0.30) !important;
      }
      .btn-warning:hover { filter: brightness(1.08) !important; transform: translateY(-1px) !important; }
      .btn-danger {
        background: linear-gradient(135deg, #ef4444 0%, #b91c1c 100%) !important;
        border-color: #b91c1c !important;
        box-shadow: 0 2px 6px rgba(185,28,28,0.28) !important;
      }

      /* ============================================================
         INPUTS -- cleaner, modern look
      ============================================================ */
      .form-control, .form-select {
        border: 1px solid #cbd5e1 !important;
        border-radius: 8px !important;
        font-size: 0.85rem !important;
        background: #ffffff !important;
        transition: border-color 0.15s, box-shadow 0.15s !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #2563eb !important;
        box-shadow: 0 0 0 3px rgba(37,99,235,0.12) !important;
      }

      /* ============================================================
         CODE DISPLAY -- dark editor
      ============================================================ */
      #rcode_display {
        background: #0d1117 !important;
        color: #c9d1d9 !important;
        border-radius: 10px !important;
        font-size: 0.82rem !important;
        font-family: 'Fira Code', 'Cascadia Code', 'Courier New', monospace !important;
        border: 1px solid #30363d !important;
        padding: 20px 24px !important;
        max-height: 62vh !important;
        overflow-y: auto !important;
        line-height: 1.7 !important;
        white-space: pre !important;
      }

      /* ============================================================
         DIVIDERS & MISC
      ============================================================ */
      hr, .sidebar-divider {
        border-color: #e2e8f0 !important;
        opacity: 1 !important;
      }
      .fv-status-box {
        background: #f1f5f9 !important;
        border-left: 3px solid #2563eb !important;
        border-radius: 0 8px 8px 0 !important;
        padding: 8px 12px !important;
        font-size: 0.84rem !important;
      }
      .fv-status-box.success {
        background: #f0fdf4 !important;
        border-left-color: #22c55e !important;
      }
    ")),
    # Load MathJax at app startup so it is fully initialised before the first
    # fit result arrives.  withMathJax() uses singleton() internally, so this
    # one call is enough -- putting it here in the static header means the CDN
    # script starts downloading the moment the page loads, not after the first
    # fit completes.
    withMathJax(),
    # Inline JS for a few UI interactions
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fv_disable_btn', function(msg) {
        var btn = document.getElementById('run_btn');
        if (btn) btn.disabled = msg.disabled;
      });

      // Backup retypeset -- called from server observeEvent after fit_result()
      // updates.  Retries every 300 ms (up to 15 times = 4.5 s) so the first
      // fit works even if MathJax CDN hasn't finished loading yet.
      Shiny.addCustomMessageHandler('mathjax_typeset_eq', function(msg) {
        var n = 0;
        function tryMJ() {
          if (window.MathJax && MathJax.Hub) {
            MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
          } else if (n++ < 15) {
            setTimeout(tryMJ, 300);
          }
        }
        tryMJ();
      });
    "))
  ),

  # -- Nav 1: Fit Distributions ---------------------------------------------
  bslib::nav_panel(
    title = "Fit Distributions",
    icon  = shiny::icon("chart-line"),

    bslib::layout_sidebar(

      # -- SIDEBAR -----------------------------------------------------------
      sidebar = bslib::sidebar(
        width = 320,
        open  = TRUE,

        # -- Section: Data Input -------------------------------------------
        tags$div(class = "sidebar-title",
          tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;flex-shrink:0;", "1"),
          "Data Input"
        ),

        bslib::navset_tab(
          id = "data_input_tab",

          bslib::nav_panel(
            title = "Upload CSV",
            shiny::fileInput(
              "file_upload", NULL,
              accept  = c(".csv", ".txt", ".tsv"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            tags$div(
              style = paste0(
                "background:#f0f5ff; border-left:3px solid #3d7ebf; ",
                "border-radius:0 5px 5px 0; padding:7px 10px; ",
                "font-size:0.78rem; color:#3a5175; margin-bottom:6px;"
              ),
              tags$strong("CSV format: "),
              "Numeric values in one or more columns with a header row. ",
              "For multi-column files, select the target column below.",
              tags$br(),
              tags$span(style = "color:#6c757d;",
                        "Separators: comma (default), semicolon, tab, or space.")
            ),
            shiny::uiOutput("col_picker"),
            shiny::checkboxInput("header", "File has header row", value = TRUE),
            shiny::selectInput("sep", "Separator",
                               choices = c(Comma = ",", Semicolon = ";",
                                           Tab = "\t", Space = " "),
                               selected = ",")
          ),

          bslib::nav_panel(
            title = "Paste Data",
            shiny::textAreaInput(
              "paste_data", NULL,
              placeholder = "Paste one number per line, or comma-separated...",
              rows = 7
            )
          ),

          bslib::nav_panel(
            title = tags$span(
              shiny::icon("database", style = "font-size:0.78rem; margin-right:3px;"),
              "Demo"
            ),
            tags$p(
              "Try FitVerse with a built-in dataset, no upload needed.",
              style = "font-size:0.8rem; color:#6c757d; margin-bottom:8px;"
            ),
            shiny::selectInput(
              "demo_choice", NULL,
              choices = c(
                "-- choose a dataset --"             = "",
                "US city annual precipitation"     = "precip",
                "Port Pirie annual max sea levels" = "portpirie",
                "North American river lengths"     = "rivers",
                "New York daily ozone levels"      = "ozone"
              ),
              selected = ""
            ),
            shiny::uiOutput("demo_description_ui"),
            shiny::actionButton(
              "load_demo", "Load this dataset",
              icon  = shiny::icon("play"),
              class = "btn btn-outline-primary btn-sm w-100",
              style = "margin-top:6px;"
            )
          ),

        ),

        shiny::textInput("xlab_input", "Variable label", placeholder = "Auto: column name  -  Override: e.g. Annual rainfall (mm)"),

        tags$hr(class = "sidebar-divider"),

        # -- Section: Fit Settings -----------------------------------------
        tags$div(class = "sidebar-title",
          tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;flex-shrink:0;", "2"),
          "Fit Settings"
        ),

        shiny::radioButtons(
          "dist_mode", "Distribution selection",
          choices = c("Auto: find best fit" = "auto",
                      "Manual: choose distributions" = "manual"),
          selected = "auto"
        ),

        shiny::conditionalPanel(
          condition = "input.dist_mode == 'manual'",
          shiny::checkboxGroupInput(
            "manual_dists", "Distributions to try",
            choiceNames  = lapply(names(unlist(FV_DIST_CHOICES)), function(n)
              tags$span(n, style = "font-size:0.83rem;")),
            choiceValues = unname(unlist(FV_DIST_CHOICES)),
            selected     = c("gamma", "lognormal", "weibull", "normal",
                             "exponential")
          )
        ),

        shiny::selectInput(
          "method", "Estimation method",
          choices  = FV_METHOD_CHOICES,
          selected = "ALL"
        ),

        shiny::radioButtons(
          "criterion", "Ranking criterion",
          choices  = FV_CRITERION_CHOICES,
          selected = "AIC",
          inline   = TRUE
        ),

        shiny::sliderInput(
          "top_n", "Overlay top N distributions on plot",
          min = 1, max = 5, value = 3, step = 1
        ),

        tags$hr(class = "sidebar-divider"),

        # -- Section: Run -------------------------------------------------
        tags$div(class = "sidebar-title",
          tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:50%;background:#1e1b4b;color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;flex-shrink:0;", "3"),
          "Run"
        ),

        tags$div(
          style = "display:flex; justify-content:center; width:100%;",
          shiny::actionButton(
            "run_btn", "Fit Distributions",
            icon  = shiny::icon("play"),
            class = "btn btn-primary"
          )
        ),

        tags$br(), tags$br(),
        shiny::uiOutput("sidebar_status")
      ),

      # -- MAIN CONTENT ------------------------------------------------------
      tagList(

        # -- Instructions (modal) -----------------------------------------
        tags$div(
          class = "mb-2",
          tags$button(
            class = "btn btn-outline-secondary btn-sm",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = "#fit_instructions_modal",
            shiny::icon("circle-info", class = "me-1"),
            "Instructions"
          ),
          tags$div(
            class = "modal fade", id = "fit_instructions_modal", tabindex = "-1",
            tags$div(
              class = "modal-dialog modal-xl modal-dialog-scrollable",
              tags$div(
                class = "modal-content",
                tags$div(
                  class = "modal-header",
                  style = "background:#f5f9ff; border-bottom:2px solid #2d6bc4;",
                  tags$h5(class = "modal-title",
                          shiny::icon("chart-line", class = "me-2"),
                          "Fit Distributions -- Instructions"),
                  tags$button(type = "button", class = "btn-close",
                              `data-bs-dismiss` = "modal", `aria-label` = "Close")
                ),
                tags$div(
                  class = "modal-body",
                tags$div(
                  style = "display:flex; align-items:flex-start; gap:14px;",
                  tags$div(
                    shiny::icon("chart-line",
                                style = "font-size:2rem; color:#2d6bc4; margin-top:2px; flex-shrink:0;")
                  ),
                  tags$div(
                    style = "width:100%;",
                    tags$h5(style = "margin:0 0 6px; color:#1e3a5f; font-weight:800;",
                            "Fit Distributions"),
                    tags$p(
                      style = "margin:0 0 10px; font-size:0.89rem; color:#2c3e50;",
                      "Upload or paste your data, then let FitVerse rank ",
                      tags$strong("up to 52 distribution families"),
                      " by AIC, BIC, and goodness-of-fit tests -- or pick specific distributions manually."
                    ),
                    tags$hr(style = "margin:8px 0; border-color:#d0e2f7;"),

                    # Step-by-step
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("list-ol", class="me-1"), "How to use"),
                    tags$div(
                      style = "display:grid; grid-template-columns:repeat(3,1fr); gap:8px; font-size:0.83rem; margin-bottom:10px;",
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;",
                                    tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:16px;height:16px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.6rem;font-weight:800;margin-right:5px;", "1"),
                                    "Load data"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Upload a CSV, paste numbers, or pick a built-in demo dataset.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;",
                                    tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:16px;height:16px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.6rem;font-weight:800;margin-right:5px;", "2"),
                                    "Configure"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Choose Auto (all distributions) or Manual. Set estimation method and ranking criterion.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;",
                                    tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:16px;height:16px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.6rem;font-weight:800;margin-right:5px;", "3"),
                                    "Fit & explore"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Click Fit Distributions and explore the results across the tabs below.")
                      )
                    ),

                    tags$hr(style = "margin:8px 0; border-color:#d0e2f7;"),

                    # Data format
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("file-csv", class="me-1"), "Data format"),
                    tags$div(
                      style = "display:grid; grid-template-columns:1fr 1fr; gap:8px; font-size:0.83rem; margin-bottom:10px;",
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#22863a;", shiny::icon("check", class="me-1"), "What to include"),
                        tags$ul(style="margin:5px 0 0; padding-left:16px;",
                          tags$li("A ", tags$strong("header row"), " with column names"),
                          tags$li(tags$strong("Numeric, continuous"), " positive values"),
                          tags$li("File types: ", tags$code(".csv"), ", ", tags$code(".txt"), ", ", tags$code(".tsv")),
                          tags$li("At least ", tags$strong("10 observations"), " for reliable fitting")
                        )
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #f5ccd0; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#cb2431;", shiny::icon("xmark", class="me-1"), "What to avoid"),
                        tags$ul(style="margin:5px 0 0; padding-left:16px;",
                          tags$li("Negative values (use ", tags$strong("Censored Fit"), " or a location-shifted distribution)"),
                          tags$li("Integer counts or proportions (these are discrete / bounded data)"),
                          tags$li("Missing values -- remove NAs before uploading"),
                          tags$li("All-identical values (zero variance)")
                        )
                      )
                    ),

                    tags$hr(style = "margin:8px 0; border-color:#d0e2f7;"),

                    # Output tabs
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("table-columns", class="me-1"), "Result tabs"),
                    tags$div(
                      style = "display:grid; grid-template-columns:repeat(4,1fr); gap:7px; font-size:0.81rem;",
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("Plot"), tags$br(),
                        tags$span(style="color:#555;", "Histogram + density overlay, Q-Q plot, boxplot & outliers.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("Rankings"), tags$br(),
                        tags$span(style="color:#555;", "AIC / BIC / GOF table for all fitted distributions.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("Parameters"), tags$br(),
                        tags$span(style="color:#555;", "Estimated parameters with confidence intervals.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("GOF Tests"), tags$br(),
                        tags$span(style="color:#555;", "KS, Anderson-Darling, and Cramer-von Mises p-values.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("Bootstrap"), tags$br(),
                        tags$span(style="color:#555;", "Percentile CIs for parameters via resampling.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("L-Moments"), tags$br(),
                        tags$span(style="color:#555;", "L-moment ratio diagram for distribution identification.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("Extreme Value"), tags$br(),
                        tags$span(style="color:#555;", "POT threshold selection and GPD/GEV return levels.")
                      ),
                      tags$div(style="background:#fff; border:1px solid #cce0f5; border-radius:5px; padding:7px;",
                        tags$strong("Download"), tags$br(),
                        tags$span(style="color:#555;", "Export results as HTML report, plot PNG, or JSON.")
                      )
                    ),

                    tags$div(
                      style = "margin-top:10px; padding:7px 12px; background:#fffbe6;
                               border:1px solid #ffe066; border-radius:5px; font-size:0.82rem; color:#6b4f00;",
                      shiny::icon("lightbulb", class="me-1"),
                      tags$strong("Tip: "),
                      "Start with ", tags$strong("Auto"), " mode and ", tags$strong("MLE"),
                      " to get a quick ranking. Then switch to ",
                      tags$strong("LMOM"), " for small samples or hydrological data, ",
                      "or select distributions manually for domain-specific fitting."
                    )
                  )
                )
              )
            )
          )
        )
        ),

        bslib::navset_card_tab(
          id     = "results_tabs",
          height = "auto",

          # -- Tab: Welcome / Overview --------------------------------------
          bslib::nav_panel(
            title = "Overview",
            icon  = shiny::icon("house"),
            shiny::uiOutput("overview_panel")
          ),

        # -- Tab: Diagnostic Plot -----------------------------------------
        bslib::nav_panel(
          title = "Plot",
          icon  = shiny::icon("chart-area"),
          shiny::uiOutput("plot_header"),
          plotly::plotlyOutput("interactive_plot",
                               height = "460px") |>
            shinycssloaders_wrapper("Fitting distributions..."),
          # -- Q-Q and P-P plots side by side ----------------------------------
          tags$div(
            style = "margin-top:18px; margin-bottom:6px; border-top:1px solid #e0e6ef; padding-top:14px;",
            tags$span(
              shiny::icon("chart-line", style = "color:#3d7ebf; margin-right:6px;"),
              tags$strong("Diagnostic Plots", style = "font-size:0.95rem; color:#1a2b3c;"),
              tags$span(
                " — Q-Q (quantile vs quantile, sensitive to tails) and P-P (probability vs probability, sensitive to body). Updates when you click a row in Rankings.",
                style = "font-size:0.83rem; color:#6c7a8d;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(6,
              plotly::plotlyOutput("qq_plot", height = "380px") |>
                shinycssloaders_wrapper("Building Q-Q plot...")
            ),
            shiny::column(6,
              plotly::plotlyOutput("pp_plot", height = "380px") |>
                shinycssloaders_wrapper("Building P-P plot...")
            )
          ),

          # -- CDF / Survival / Hazard / Cumulative Hazard ----------------------
          tags$div(
            style = "margin-top:22px; margin-bottom:6px; border-top:1px solid #e0e6ef; padding-top:14px;",
            tags$span(
              shiny::icon("wave-square", style = "color:#3d7ebf; margin-right:6px;"),
              tags$strong("Distribution Curves", style = "font-size:0.95rem; color:#1a2b3c;"),
              tags$span(
                " — CDF, Survival S(x)=1−F(x), Hazard h(x)=f(x)/S(x), Cumulative Hazard H(x)=−log S(x).",
                style = "font-size:0.83rem; color:#6c7a8d;"
              )
            )
          ),
          shiny::radioButtons(
            "curve_type",
            label   = NULL,
            choices = c("CDF", "Survival", "Hazard", "Cumulative Hazard"),
            selected = "Survival",
            inline  = TRUE
          ),
          plotly::plotlyOutput("curve_plot", height = "380px") |>
            shinycssloaders_wrapper("Building curve..."),

          # -- Boxplot & Outlier Detection ----------------------------------
          tags$div(
            style = "margin-top:18px; margin-bottom:6px; border-top:1px solid #e0e6ef; padding-top:14px;",
            tags$span(
              shiny::icon("chart-simple", style = "color:#3d7ebf; margin-right:6px;"),
              tags$strong("Boxplot & Outlier Screening", style = "font-size:0.95rem; color:#1a2b3c;"),
              tags$span(", with potential outliers flagged via Modified Z-score (Iglewicz & Hoaglin, 1993)",
                        style = "font-size:0.83rem; color:#6c7a8d;")
            )
          ),
          plotly::plotlyOutput("outlier_boxplot", height = "220px") |>
            shinycssloaders_wrapper("Building boxplot..."),
          shiny::uiOutput("outlier_footer"),

          # -- L-Moment Ratio Diagram --------------------------------------
          tags$div(
            style = "margin-top:18px; margin-bottom:6px; border-top:1px solid #e0e6ef; padding-top:14px;",
            tags$span(
              shiny::icon("circle-dot", style = "color:#3d7ebf; margin-right:6px;"),
              tags$strong("L-Moment Ratio Diagram",
                          style = "font-size:0.95rem; color:#1a2b3c;"),
              tags$span(
                ", identifying candidate distribution families from sample tau3 and tau4",
                style = "font-size:0.83rem; color:#6c7a8d;"
              )
            )
          ),
          plotly::plotlyOutput("lmrd_plot", height = "520px") |>
            shinycssloaders_wrapper("Building L-moment ratio diagram..."),
          tags$div(
            style = "margin-top:10px; padding:10px 14px; background:#f8fafc; border-left:3px solid #3d7ebf; border-radius:0 6px 6px 0;",
            tags$p(
              style = "font-size:0.82rem; color:#4a5568; margin:0 0 6px 0;",
              tags$strong("How to read this diagram: "),
              "The ", tags$strong("curves"), " (GLO, GEV, GNO, PE3, GPA) are distribution ",
              tags$em("families"), ", each parameterised by a shape parameter that traces the curve. ",
              "The ", tags$strong("labelled dots"), " (Normal, Logistic, Gumbel, Exponential, Uniform) are ",
              "well-known special cases that fall ", tags$em("on"), " a particular family's curve: ",
              "Logistic lies on GLO, Gumbel on GEV, Normal on GNO, and Exponential/Uniform on GPA. ",
              "The ", tags$strong("red point"), " is your sample (tau3, tau4); the family whose curve passes ",
              "closest to it is most consistent with your data's L-moment structure."
            ),
            tags$p(
              style = "font-size:0.78rem; color:#718096; margin:0;",
              tags$em("Exact curves: GLO, GEV, GPA (Hosking, 1990). Polynomial approximations: GNO, PE3 (Hosking & Wallis, 1997)."),
              tags$br(),
              "Hosking, J. R. M. (1990). L-moments. ",
              tags$em("Journal of the Royal Statistical Society B"), ", 52(1), 105-124.  .  ",
              "Hosking, J. R. M. & Wallis, J. R. (1997). ",
              tags$em("Regional Frequency Analysis."),
              " Cambridge University Press."
            )
          )
        ),

        # -- Tab: Rankings ------------------------------------------------
        bslib::nav_panel(
          title = "Rankings",
          icon  = shiny::icon("table"),
          shiny::uiOutput("ranking_header"),
          shiny::uiOutput("aic_note"),
          DT::DTOutput("ranking_table") |>
            shinycssloaders_wrapper("Building ranking table..."),
          shiny::tags$p(
            shiny::icon("circle-check"),
            "Only converged fits appear in rankings, so every result is statistically trustworthy.",
            style = "font-size:0.82rem; color:#6c757d; margin-top:6px; margin-bottom:0;"
          )
        ),

        # -- Tab: Data Summary ---------------------------------------------
        bslib::nav_panel(
          title = "Data Summary",
          icon  = shiny::icon("magnifying-glass-chart"),
          shiny::uiOutput("data_summary_panel")
        ),

        # -- Tab: Calculator ----------------------------------------------
        bslib::nav_panel(
          title = "Calculator",
          icon  = shiny::icon("calculator"),
          bslib::card(
            bslib::card_header(
              shiny::icon("calculator"), " Probability & Quantile Calculator"
            ),
            bslib::card_body(
              tags$p(
                class = "text-muted small",
                "Compute tail probabilities and quantiles from the currently displayed",
                " distribution (click a row in Rankings to change it)."
              ),
              bslib::layout_columns(
                col_widths = c(5, 5, 2),
                shiny::numericInput(
                  "calc_x_val", "Value  x",
                  value = NA_real_, step = NA
                ),
                shiny::numericInput(
                  "calc_p_val", "Probability  p  (0 – 1)",
                  value = NA_real_, min = 0, max = 1, step = 0.01
                ),
                tags$div(
                  style = "padding-top:24px;",
                  shiny::actionButton(
                    "calc_run", "Calculate",
                    icon  = shiny::icon("play"),
                    class = "btn btn-primary w-100"
                  )
                )
              ),
              tags$hr(),
              shiny::uiOutput("calc_result_ui")
            )
          )
        ),

        # -- Tab: Simulate from fit ----------------------------------------
        bslib::nav_panel(
          title = "Simulate",
          icon  = shiny::icon("shuffle"),
          bslib::card(
            bslib::card_header(
              shiny::icon("shuffle"), " Simulate from Fitted Distribution"
            ),
            bslib::card_body(
              tags$p(
                class = "text-muted small",
                "Draw random samples from the currently displayed fitted distribution",
                " (click a row in Rankings to change it). Download the simulated data as CSV."
              ),
              bslib::layout_columns(
                col_widths = c(4, 3, 3, 2),
                shiny::numericInput(
                  "postfit_sim_n", "Sample size  n",
                  value = 1000L, min = 1L, max = 1000000L, step = 100L
                ),
                shiny::numericInput(
                  "postfit_sim_seed", "Random seed",
                  value = 42L, min = 1L, step = 1L
                ),
                shiny::checkboxInput(
                  "postfit_sim_use_seed", "Fix seed",
                  value = TRUE
                ),
                tags$div(
                  style = "padding-top:24px;",
                  shiny::actionButton(
                    "postfit_sim_run", "Generate",
                    icon  = shiny::icon("dice"),
                    class = "btn btn-primary w-100"
                  )
                )
              ),
              tags$hr(),
              shiny::uiOutput("postfit_sim_result_ui")
            )
          )
        ),

        # -- Tab: Bootstrap CI ---------------------------------------------
        bslib::nav_panel(
          title = "Bootstrap CI",
          icon  = shiny::icon("arrows-rotate"),
          bslib::card(
            bslib::card_header("Bootstrap Confidence Intervals"),
            bslib::card_body(
              bslib::layout_columns(
                col_widths = c(3, 2, 2, 5),
                shiny::numericInput("boot_B", "Replicates (B)",
                                    value = 499, min = 99, max = 9999, step = 100),
                shiny::numericInput("boot_conf", "Conf. level",
                                    value = 0.95, min = 0.80, max = 0.99, step = 0.01),
                shiny::numericInput("boot_seed", "Random seed",
                                    value = 42, min = 1),
                shiny::textInput(
                  "boot_periods",
                  "Return periods for CI (optional, comma-sep.)",
                  value       = "",
                  placeholder = "e.g. 10, 50, 100, 200"
                )
              ),
              tags$div(
                style = "margin-bottom:12px;",
                shiny::actionButton("boot_run", "Run Bootstrap",
                                    icon  = shiny::icon("arrows-rotate"),
                                    class = "btn btn-secondary")
              ),
              tags$hr(),
              shiny::uiOutput("boot_result_panel")
            )
          )
        ),

        # -- Tab: Threshold Selection --------------------------------------
        bslib::nav_panel(
          title = "Threshold",
          icon  = shiny::icon("sliders"),
          bslib::card(
            bslib::card_header("Extreme Value Threshold Selection"),
            bslib::card_body(
              tags$p(class = "text-muted small",
                "Use these three diagnostic plots to identify the lowest threshold above which a ",
                "Generalised Pareto Distribution (GPD) fits exceedances. ",
                "Look for (1) linearity in the MRL plot, and (2) approximate constancy in the shape and modified-scale plots."),
              bslib::layout_columns(
                col_widths = c(3, 2, 2, 5),
                shiny::numericInput("thr_n", "# candidate thresholds",
                                    value = 40L, min = 10L, max = 120L, step = 5L),
                shiny::numericInput("thr_min_exc", "Min. exceedances",
                                    value = 10L, min = 5L, max = 50L, step = 1L),
                shiny::numericInput("thr_conf", "Conf. level",
                                    value = 0.95, min = 0.80, max = 0.99, step = 0.01),
                tags$div(
                  style = "padding-top:24px;",
                  shiny::actionButton(
                    "thr_run", "Run Threshold Diagnostics",
                    icon  = shiny::icon("sliders"),
                    class = "btn btn-primary w-100"
                  )
                )
              ),
              tags$hr(),
              shiny::uiOutput("thr_result_panel")
            )
          )
        ),

        # -- Tab: Return Levels --------------------------------------------
        bslib::nav_panel(
          title = "Return Levels",
          icon  = shiny::icon("water"),
          bslib::card(
            bslib::card_header("Return Level Estimation"),
            bslib::card_body(
              tags$p(class = "text-muted small",
                     "The T-year return level is the value expected to be exceeded
                      with probability 1/T in any single period.  Enter the return
                      periods you want to evaluate, then click Compute."),
              bslib::layout_columns(
                col_widths = c(8, 4),
                shiny::textInput(
                  "rl_periods",
                  "Return periods (comma-separated years)",
                  value       = "2, 5, 10, 25, 50, 100, 200, 500",
                  placeholder = "e.g. 2, 5, 10, 25, 50, 100"
                ),
                tags$div(
                  style = "padding-top:24px;",
                  shiny::actionButton(
                    "rl_run", "Compute Return Levels",
                    icon  = shiny::icon("calculator"),
                    class = "btn btn-primary w-100"
                  )
                )
              ),
              tags$hr(),
              shiny::uiOutput("rl_result_panel")
            )
          )
        ),

        # -- Tab: Manual Fit -----------------------------------------------
        bslib::nav_panel(
          title = "Manual Fit",
          icon  = shiny::icon("pen-ruler"),
          bslib::card(
            bslib::card_header(
              shiny::icon("pen-ruler"), " Manual Parameter Specification"
            ),
            bslib::card_body(
              tags$p(
                class = "text-muted small",
                "Enter parameter values from domain knowledge or literature.",
                " FitVerse will compute the log-likelihood, AIC, BIC, and GoF tests",
                " for your specified model and overlay it on the density plot."
              ),
              bslib::layout_columns(
                col_widths = c(5, 7),
                shiny::selectInput(
                  "manual_fit_dist",
                  "Distribution",
                  choices  = FV_DIST_CHOICES,
                  selected = "normal"
                ),
                shiny::uiOutput("manual_fit_param_inputs")
              ),
              tags$div(
                style = "margin-top:6px;",
                shiny::actionButton(
                  "manual_fit_run", "Evaluate Manual Fit",
                  icon  = shiny::icon("play"),
                  class = "btn btn-primary"
                )
              ),
              tags$hr(),
              shiny::uiOutput("manual_fit_result_ui")
            )
          )
        ),

        # -- Tab: Download -------------------------------------------------
        bslib::nav_panel(
          title = "Download",
          icon  = shiny::icon("download"),
          shiny::uiOutput("download_panel")
        )
        ) # navset_card_tab
      ) # tagList
    ) # layout_sidebar
  ), # nav_panel Fit Distributions

  # -- Nav 2: Batch Fit ------------------------------------------------------
  bslib::nav_panel(
    title = "Batch Fit",
    icon  = shiny::icon("layer-group"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        open  = TRUE,

        tags$div(class = "sidebar-title",
          tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;flex-shrink:0;", "1"),
          "Data"
        ),
        shiny::fileInput(
          "batch_file", NULL,
          accept      = c(".csv", ".txt", ".tsv"),
          buttonLabel = "Browse CSV...",
          placeholder = "No file selected"
        ),
        tags$div(
          style = paste0(
            "background:#f0f5ff; border-left:3px solid #3d7ebf; ",
            "border-radius:0 5px 5px 0; padding:7px 10px; ",
            "font-size:0.78rem; color:#3a5175; margin-bottom:6px;"
          ),
          tags$strong("CSV format: "),
          "Each numeric column is fitted independently. Include a header row; ",
          "column names become row labels in the results table. ",
          "Non-numeric columns are ignored automatically."
        ),
        shiny::checkboxInput("batch_header", "File has header row", value = TRUE),
        shiny::uiOutput("batch_col_picker"),

        tags$hr(class = "sidebar-divider"),

        tags$div(class = "sidebar-title",
          tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;flex-shrink:0;", "2"),
          "Settings"
        ),
        shiny::radioButtons(
          "batch_method", "Estimation method",
          choices  = c("MLE" = "MLE", "MOM" = "MOM",
                       "L-Moments" = "LMOM", "All" = "ALL"),
          selected = "MLE"
        ),
        shiny::radioButtons(
          "batch_criterion", "Ranking criterion",
          choices = FV_CRITERION_CHOICES, selected = "AIC", inline = TRUE
        ),

        tags$hr(class = "sidebar-divider"),

        tags$div(class = "sidebar-title",
          tags$span(style="display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:50%;background:#1e3a5f;color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;flex-shrink:0;", "3"),
          "Run"
        ),
        shiny::actionButton(
          "batch_run", "Run Batch Fit",
          icon  = shiny::icon("play"),
          class = "btn btn-primary"
        )
      ),

      tagList(

        # -- Instructions (modal) ---------------------------------------------
        tags$div(
          class = "mb-2",
          tags$button(
            class = "btn btn-outline-secondary btn-sm",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = "#batch_instructions_modal",
            shiny::icon("circle-info", class = "me-1"),
            "Instructions"
          ),
          tags$div(
            class = "modal fade", id = "batch_instructions_modal", tabindex = "-1",
            tags$div(
              class = "modal-dialog modal-xl modal-dialog-scrollable",
              tags$div(
                class = "modal-content",
                tags$div(
                  class = "modal-header",
                  style = "background:#f5f9ff; border-bottom:2px solid #2d6bc4;",
                  tags$h5(class = "modal-title",
                          shiny::icon("layer-group", class = "me-2"),
                          "Batch Fit -- Instructions"),
                  tags$button(type = "button", class = "btn-close",
                              `data-bs-dismiss` = "modal", `aria-label` = "Close")
                ),
                tags$div(
                  class = "modal-body",
                tags$div(
                  style = "display:flex; align-items:flex-start; gap:14px;",
                  tags$div(
                    shiny::icon("layer-group",
                                style = "font-size:2rem; color:#2d6bc4; margin-top:2px; flex-shrink:0;")
                  ),
                  tags$div(
                    tags$h5(style = "margin:0 0 6px; color:#1e3a5f; font-weight:800;",
                            "Batch Distribution Fitting"),
                    tags$p(
                      style = "margin:0 0 10px; font-size:0.89rem; color:#2c3e50;",
                      "Batch Fit runs the full FitVerse fitting pipeline on ",
                      tags$strong("every numeric column in your file simultaneously"),
                      ", returning the single best-fit distribution and its parameters",
                      " for each variable in one summary table."
                    ),
                    tags$hr(style = "margin:8px 0; border-color:#d0e2f7;"),
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("file-csv", class="me-1"), "Required data format"),
                    tags$div(
                      style = "display:grid; grid-template-columns:1fr 1fr; gap:10px; font-size:0.84rem;",
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:10px;",
                        tags$strong(style="color:#22863a;", shiny::icon("check", class="me-1"), "What to include"),
                        tags$ul(
                          style = "margin:6px 0 0; padding-left:16px;",
                          tags$li("A ", tags$strong("header row"), " with column names (one name per variable)"),
                          tags$li("One ", tags$strong("numeric column per variable"), " (fitted independently)"),
                          tags$li("File type: ", tags$code(".csv"), ", ", tags$code(".txt"), ", or ", tags$code(".tsv")),
                          tags$li("Columns may have ", tags$strong("different lengths"), " (trailing NAs are dropped per column)")
                        )
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #f5ccd0; border-radius:6px; padding:10px;",
                        tags$strong(style="color:#cb2431;", shiny::icon("xmark", class="me-1"), "What to avoid"),
                        tags$ul(
                          style = "margin:6px 0 0; padding-left:16px;",
                          tags$li("Non-numeric columns (text, dates, IDs) are ", tags$strong("silently ignored"), "."),
                          tags$li("Columns with fewer than ", tags$strong("10 observations"), ", too few to fit reliably"),
                          tags$li("Columns where all values are identical (zero variance)"),
                          tags$li("Row-index columns in the first position; uncheck ", tags$em("Header"), " if the file has no names")
                        )
                      )
                    ),
                    tags$div(
                      style = "margin-top:10px; padding:7px 12px; background:#fffbe6;
                               border:1px solid #ffe066; border-radius:5px; font-size:0.82rem; color:#6b4f00;",
                      shiny::icon("lightbulb", class="me-1"),
                      tags$strong("Example: "), "A file with columns ",
                      tags$code("Rainfall"), ", ", tags$code("Windspeed"), ", ", tags$code("Temperature"),
                      " will produce three rows: one best-fit result per variable."
                    )
                  )
                )
              )
            )
          )
        )
        ),

        # -- Batch results -----------------------------------------------------
        bslib::card(
          bslib::card_header("Batch Results: best-fit distribution per column"),
          bslib::card_body(
            shiny::uiOutput("batch_status"),
            tags$br(),
            DT::DTOutput("batch_table") |>
              shinycssloaders_wrapper("Running batch fit..."),
            tags$br(),
            shiny::uiOutput("batch_download_ui")
          )
        )

      )
    )
  ),

  # -- Nav 3: Censored Fit ---------------------------------------------------
  bslib::nav_panel(
    title = "Censored Fit",
    icon  = shiny::icon("hourglass-half"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 290,
        open  = TRUE,

        # Step 1: Data
        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "1"
          ),
          "Data"
        ),
        shiny::fileInput(
          "cens_file", NULL,
          accept      = c(".csv", ".txt", ".tsv"),
          buttonLabel = "Browse CSV...",
          placeholder = "No file selected"
        ),
        tags$div(
          style = paste0(
            "background:#f0f5ff; border-left:3px solid #3d7ebf; ",
            "border-radius:0 5px 5px 0; padding:7px 10px; ",
            "font-size:0.78rem; color:#3a5175; margin-bottom:6px;"
          ),
          tags$strong("Required column: "),
          tags$code("x"), " (numeric). ",
          "Optional: ", tags$code("status"), " (0/1/2/3) and ", tags$code("x_upper"),
          " for interval-censored data. ",
          tags$span(
            style = "color:#1e3a5f; font-weight:600;",
            shiny::icon("circle-info", style = "font-size:0.75rem;"),
            " See the ", tags$strong("How to use"), " tab for full format details."
          )
        ),
        # Auto-detected column summary
        shiny::uiOutput("cens_detect_ui"),

        tags$hr(class = "sidebar-divider"),

        # Step 2: Truncation (optional)
        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "2"
          ),
          "Truncation",
          tags$span(class = "text-muted", style = "font-weight:400; font-size:0.8rem;",
                    " (optional)")
        ),
        shiny::checkboxInput("cens_trunc", "Apply truncation bounds", value = FALSE),
        shiny::conditionalPanel(
          condition = "input.cens_trunc",
          shiny::numericInput("cens_tl", "Left truncation bound",  value = NA),
          shiny::numericInput("cens_tu", "Right truncation bound", value = NA)
        ),

        tags$hr(class = "sidebar-divider"),

        # Step 3: Models
        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "3"
          ),
          "Models"
        ),
        shiny::checkboxGroupInput(
          "cens_dists", "Distributions to fit",
          choices = c(
            "Normal"       = "normal",
            "Log-Normal"   = "lognormal",
            "Weibull"      = "weibull",
            "Exponential"  = "exponential",
            "Gamma"        = "gamma",
            "Log-Logistic" = "loglogistic",
            "Gumbel"       = "gumbel"
          ),
          selected = c("normal", "lognormal", "weibull", "exponential",
                       "gamma", "loglogistic")
        ),
        shiny::radioButtons(
          "cens_criterion", "Ranking criterion",
          choices = c("AIC", "BIC"), selected = "AIC", inline = TRUE
        ),
        shiny::numericInput(
          "cens_starts", "Optimisation starts",
          value = 3L, min = 1L, max = 20L, step = 1L
        ),

        tags$hr(class = "sidebar-divider"),

        shiny::actionButton(
          "cens_run", "Fit Censored Data",
          icon  = shiny::icon("play"),
          class = "btn btn-primary w-100"
        )
      ),

      # Main results panel
      tagList(

        # -- Instructions (modal) ---------------------------------------------
        tags$div(
          class = "mb-2",
          tags$button(
            class = "btn btn-outline-secondary btn-sm",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = "#cens_instructions_modal",
            shiny::icon("circle-info", class = "me-1"),
            "Instructions"
          ),
          tags$div(
            class = "modal fade", id = "cens_instructions_modal", tabindex = "-1",
            tags$div(
              class = "modal-dialog modal-xl modal-dialog-scrollable",
              tags$div(
                class = "modal-content",
                tags$div(
                  class = "modal-header",
                  style = "background:#f5f9ff; border-bottom:2px solid #2d6bc4;",
                  tags$h5(class = "modal-title",
                          shiny::icon("hourglass-half", class = "me-2"),
                          "Censored Fit -- Instructions"),
                  tags$button(type = "button", class = "btn-close",
                              `data-bs-dismiss` = "modal", `aria-label` = "Close")
                ),
                tags$div(
                  class = "modal-body",
                tags$div(
                  style = "display:flex; align-items:flex-start; gap:14px;",
                  tags$div(
                    shiny::icon("hourglass-half",
                                style = "font-size:2rem; color:#2d6bc4; margin-top:2px; flex-shrink:0;")
                  ),
                  tags$div(
                    style = "width:100%;",
                    tags$h5(style = "margin:0 0 6px; color:#1e3a5f; font-weight:800;",
                            "Censored Distribution Fitting"),
                    tags$p(
                      style = "margin:0 0 10px; font-size:0.89rem; color:#2c3e50;",
                      "Upload a CSV with one or more censoring columns. The app detects ",
                      "the censoring structure automatically and fits distributions using ",
                      tags$strong("maximum likelihood for censored/truncated data"), "."
                    ),
                    tags$hr(style = "margin:8px 0; border-color:#d0e2f7;"),
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("file-csv", class="me-1"), "CSV column reference"),
                    tags$table(
                      class = "table table-sm table-bordered",
                      style = "font-size:0.83rem; background:#fff;",
                      tags$thead(
                        tags$tr(
                          tags$th("Column"), tags$th("Required"), tags$th("Description")
                        )
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td(tags$code("x")),
                          tags$td(tags$strong("Yes")),
                          tags$td("Your measurement, time, or concentration value.")
                        ),
                        tags$tr(
                          tags$td(tags$code("status")),
                          tags$td("No"),
                          tags$td(
                            tags$code("1"), " = observed, ",
                            tags$code("0"), " = right-censored, ",
                            tags$code("2"), " = left-censored, ",
                            tags$code("3"), " = interval-censored. ",
                            "Omit column to treat all rows as fully observed."
                          )
                        ),
                        tags$tr(
                          tags$td(tags$code("x_upper")),
                          tags$td("No"),
                          tags$td(
                            "Upper bound for interval-censored rows (",
                            tags$code("status = 3"), "). Only needed when interval-censored rows are present."
                          )
                        )
                      )
                    ),
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin:10px 0 6px;",
                            shiny::icon("code", class="me-1"), "Minimal examples"),
                    tags$div(
                      style = "display:grid; grid-template-columns:1fr 1fr; gap:8px; font-size:0.82rem;",
                      tags$div(
                        tags$strong("Right-censored:"),
                        tags$pre(style = "background:#f4f7fb; padding:7px; border-radius:4px; margin:4px 0 0;",
                                 "x,status\n34.2,1\n50.0,0\n21.7,1")
                      ),
                      tags$div(
                        tags$strong("Interval-censored:"),
                        tags$pre(style = "background:#f4f7fb; padding:7px; border-radius:4px; margin:4px 0 0;",
                                 "x,x_upper,status\n10,20,3\n25,30,3\n18,18,1")
                      )
                    ),
                    tags$hr(style = "margin:10px 0; border-color:#d0e2f7;"),
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("book", class="me-1"), "Key concepts"),
                    tags$div(
                      style = "display:grid; grid-template-columns:1fr 1fr; gap:8px; font-size:0.83rem;",
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;", "Right-censored"),
                        tags$p(style="margin:4px 0 0;",
                               "Event has not yet occurred (e.g., patient still alive at study end). ",
                               "The recorded value is the last observation time.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;", "Left-censored"),
                        tags$p(style="margin:4px 0 0;",
                               "True value is below a detection limit. Record the DL as ",
                               tags$code("x"), " with ", tags$code("status = 2"), ".")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;", "Interval-censored"),
                        tags$p(style="margin:4px 0 0;",
                               "True value is known to lie in ", tags$code("[x, x_upper]"),
                               " (e.g., failure occurred between two inspection visits).")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px;",
                        tags$strong(style="color:#1e3a5f;", "Truncation"),
                        tags$p(style="margin:4px 0 0;",
                               "Observations outside a window are never recorded. ",
                               "Enter left/right bounds in the Truncation section of the sidebar.")
                      )
                    ),
                    tags$p(
                      style = "margin-top:8px; font-size:0.78rem; color:#6b7280;",
                      tags$em("Reference: Meeker & Escobar (1998). Statistical Methods for Reliability Data. Wiley.")
                    )
                  )
                )
              )
            )
          )
        )
        ),

        bslib::navset_card_tab(
          id = "cens_tabs",

          bslib::nav_panel(
            title = "Plot",
            icon  = shiny::icon("chart-line"),
            shiny::uiOutput("cens_empty_plot"),
            shiny::plotOutput("cens_plot", height = "520px") |>
              shinycssloaders_wrapper("Fitting distributions...")
          ),

          bslib::nav_panel(
            title = "Rankings",
            icon  = shiny::icon("trophy"),
            shiny::uiOutput("cens_empty_table"),
            DT::DTOutput("cens_table") |>
              shinycssloaders_wrapper("Computing rankings..."),
            tags$br(),
            shiny::uiOutput("cens_params_ui")
          )
        )
      )
    )
  ),

  # -- Nav 4: Grouped Fit ---------------------------------------------------
  bslib::nav_panel(
    title = "Grouped Fit",
    icon  = shiny::icon("table"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 290,
        open  = TRUE,

        # Step 1: Data -------------------------------------------------------
        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "1"
          ),
          "Data"
        ),
        shiny::fileInput(
          "grouped_upload", NULL,
          accept      = c(".csv", ".txt"),
          buttonLabel = "Browse CSV...",
          placeholder = "No file selected"
        ),
        tags$div(
          style = paste0(
            "background:#f0f5ff; border-left:3px solid #3d7ebf; ",
            "border-radius:0 5px 5px 0; padding:7px 10px; ",
            "font-size:0.78rem; color:#3a5175; margin-bottom:8px;"
          ),
          tags$strong("Frequency table format:"),
          tags$br(),
          HTML("&bull; 2 cols: <code>midpoint, count</code>"),
          tags$br(),
          HTML("&bull; 3 cols: <code>lower, upper, count</code>"),
          tags$br(),
          tags$span(style = "color:#6c757d;", "Comment lines (#) are ignored.")
        ),
        shiny::uiOutput("grouped_preview_ui"),
        shiny::textInput(
          "grouped_xlab", "Variable label",
          placeholder = "e.g. Annual rainfall (mm)"
        ),

        tags$hr(class = "sidebar-divider"),

        # Step 2: Distributions ----------------------------------------------
        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "2"
          ),
          "Distributions"
        ),
        shiny::radioButtons(
          "grouped_dist_mode", "Selection",
          choices  = c("Auto: try all" = "auto",
                       "Manual: choose" = "manual"),
          selected = "auto"
        ),
        shiny::conditionalPanel(
          condition = "input.grouped_dist_mode == 'manual'",
          shiny::checkboxGroupInput(
            "grouped_dists", NULL,
            choiceNames  = lapply(names(unlist(FV_DIST_CHOICES)), function(n)
              tags$span(style = "font-size:0.82rem;", n)),
            choiceValues = unname(unlist(FV_DIST_CHOICES)),
            selected     = c("normal","lognormal","weibull","gamma","gev","gumbel")
          )
        ),

        tags$hr(class = "sidebar-divider"),

        # Step 3: Method -----------------------------------------------------
        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "3"
          ),
          "Method"
        ),
        shiny::radioButtons(
          "grouped_method", NULL,
          choices  = c("All methods" = "All",
                       "MLE (grouped likelihood)" = "MLE",
                       "L-Moments" = "L-Moments",
                       "MOM" = "MOM"),
          selected = "All"
        ),
        tags$small(
          class = "text-muted",
          shiny::icon("circle-info", style = "font-size:0.75rem;"),
          " MLE uses the grouped likelihood ",
          HTML("&sum; n<sub>i</sub> log[F(b<sub>i</sub>) &minus; F(a<sub>i</sub>)]"),
          " which is more accurate than treating midpoints as raw values."
        ),

        tags$hr(class = "sidebar-divider"),

        shiny::actionButton(
          "run_grouped", "Fit Grouped Data",
          icon  = shiny::icon("chart-line"),
          class = "btn btn-primary w-100"
        )
      ),

      # -- Main results area --------------------------------------------------
      tagList(
        # Instructions button
        tags$div(
          class = "mb-2",
          tags$button(
            class = "btn btn-outline-secondary btn-sm",
            type  = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = "#grouped_instructions_modal",
            shiny::icon("circle-info", class = "me-1"),
            "Instructions"
          ),
          tags$div(
            class = "modal fade", id = "grouped_instructions_modal", tabindex = "-1",
            tags$div(
              class = "modal-dialog modal-lg modal-dialog-scrollable",
              tags$div(
                class = "modal-content",
                tags$div(
                  class = "modal-header",
                  style = "background:#f5f9ff; border-bottom:2px solid #2d6bc4;",
                  tags$h5(class = "modal-title",
                          shiny::icon("table", class = "me-2"),
                          "Grouped Fit -- Instructions"),
                  tags$button(type = "button", class = "btn-close",
                              `data-bs-dismiss` = "modal", `aria-label` = "Close")
                ),
                tags$div(
                  class = "modal-body",
                  tags$p(
                    "Many real-world datasets arrive as frequency tables — annual rainfall records,",
                    "insurance loss summaries, published engineering data — where only class counts",
                    "are available, not individual raw values.",
                    style = "font-size:0.89rem;"
                  ),
                  tags$h6("Two CSV formats are accepted:", style="color:#1e3a5f;font-weight:700;"),
                  tags$div(
                    style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;font-size:0.83rem;",
                    tags$div(
                      tags$strong("Midpoint format (2 columns):"),
                      tags$pre(style="background:#f4f7fb;padding:7px;border-radius:4px;margin:4px 0 0;",
                               "midpoint,count
30,3
50,17
70,32
90,14")
                    ),
                    tags$div(
                      tags$strong("Interval format (3 columns):"),
                      tags$pre(style="background:#f4f7fb;padding:7px;border-radius:4px;margin:4px 0 0;",
                               "lower,upper,count
0,100,3
100,200,17
200,350,21")
                    )
                  ),
                  tags$hr(style="margin:12px 0;"),
                  tags$h6("Fitting method:", style="color:#1e3a5f;font-weight:700;"),
                  tags$p(
                    tags$strong("MLE"), " uses the grouped likelihood: maximises",
                    tags$span(style="font-family:monospace;",
                              HTML("&sum; n<sub>i</sub> log[F(b<sub>i</sub>) &minus; F(a<sub>i</sub>)]")),
                    ". This is the statistically correct method for grouped data — more accurate",
                    "than treating midpoints as if they were exact observations.",
                    tags$br(),
                    tags$strong("L-Moments"), " and ", tags$strong("MOM"),
                    " use weighted pseudo-observations (each midpoint replicated by its count).",
                    style = "font-size:0.85rem;"
                  ),
                  tags$hr(style="margin:12px 0;"),
                  tags$h6("Trial datasets (bundled):", style="color:#1e3a5f;font-weight:700;"),
                  tags$ul(
                    style = "font-size:0.85rem;",
                    tags$li(tags$strong("rainfall_grouped.csv"), " — 80 station-years of annual max daily rainfall (GEV/Gumbel expected)"),
                    tags$li(tags$strong("failure_times_grouped.csv"), " — 120 component failure times (Weibull/Gamma expected)"),
                    tags$li(tags$strong("insurance_losses_grouped.csv"), " — 250 property insurance claims (LogNormal/Pareto expected)")
                  ),
                  tags$p(
                    "These are in ", tags$code("system.file('extdata', package='FitVerse')"),
                    ".", style = "font-size:0.83rem; color:#6c757d;"
                  )
                )
              )
            )
          )
        ),

        # Results
        shiny::uiOutput("grouped_fit_main_ui")
      )
    )
  ),  # nav_panel Grouped Fit

  # -- Nav 5: Simulate Data ----------------------------------------------------
  bslib::nav_panel(
    title = "Simulate Data",
    icon  = shiny::icon("dice"),

    bslib::layout_sidebar(

      sidebar = bslib::sidebar(
        width = 310,
        open  = TRUE,

        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "1"
          ),
          "Distribution"
        ),
        shiny::selectInput(
          "sim_dist", NULL,
          choices  = FV_DIST_CHOICES,
          selected = "normal"
        ),

        tags$hr(class = "sidebar-divider"),

        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "2"
          ),
          "Parameters"
        ),
        shiny::uiOutput("sim_params_ui"),

        tags$hr(class = "sidebar-divider"),

        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "3"
          ),
          "Sample size & seed"
        ),
        shiny::numericInput("sim_n", "Sample size (n)",
                            value = 200, min = 1, max = 100000, step = 50),
        shiny::checkboxInput("sim_use_seed", "Use fixed random seed", value = TRUE),
        shiny::conditionalPanel(
          condition = "input.sim_use_seed",
          shiny::numericInput("sim_seed", "Seed", value = 42, min = 1, step = 1)
        ),

        tags$hr(class = "sidebar-divider"),

        shiny::actionButton(
          "sim_generate", "Generate Data",
          icon  = shiny::icon("dice"),
          class = "btn btn-primary w-100"
        )
      ),

      tagList(

        # -- Instructions (modal) ---------------------------------------------
        tags$div(
          class = "mb-2",
          tags$button(
            class = "btn btn-outline-secondary btn-sm",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = "#sim_instructions_modal",
            shiny::icon("circle-info", class = "me-1"),
            "Instructions"
          ),
          tags$div(
            class = "modal fade", id = "sim_instructions_modal", tabindex = "-1",
            tags$div(
              class = "modal-dialog modal-xl modal-dialog-scrollable",
              tags$div(
                class = "modal-content",
                tags$div(
                  class = "modal-header",
                  style = "background:#f5f9ff; border-bottom:2px solid #2d6bc4;",
                  tags$h5(class = "modal-title",
                          shiny::icon("dice", class = "me-2"),
                          "Simulate Data -- Instructions"),
                  tags$button(type = "button", class = "btn-close",
                              `data-bs-dismiss` = "modal", `aria-label` = "Close")
                ),
                tags$div(
                  class = "modal-body",
                tags$div(
                  style = "display:flex; align-items:flex-start; gap:14px;",
                  tags$div(
                    shiny::icon("dice",
                                style = "font-size:2rem; color:#2d6bc4; margin-top:2px; flex-shrink:0;")
                  ),
                  tags$div(
                    style = "width:100%;",
                    tags$h5(style = "margin:0 0 6px; color:#1e3a5f; font-weight:800;",
                            "Simulate Data"),
                    tags$p(
                      style = "margin:0 0 10px; font-size:0.89rem; color:#2c3e50;",
                      "Generate random samples from any of FitVerse's ",
                      tags$strong("52 distribution families"),
                      ". Use simulated data to explore distributional shapes, ",
                      "validate fitting workflows, or produce datasets for teaching and presentations."
                    ),
                    tags$hr(style = "margin:8px 0; border-color:#d0e2f7;"),
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("list-ol", class="me-1"), "How to use"),
                    tags$div(
                      style = "display:grid; grid-template-columns:repeat(4,1fr); gap:8px; font-size:0.83rem;",
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px; text-align:center;",
                        tags$div(style="font-size:1.3rem; color:#2d6bc4; margin-bottom:4px;",
                                 shiny::icon("list")),
                        tags$strong("1. Distribution"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Choose a distribution family from the sidebar dropdown.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px; text-align:center;",
                        tags$div(style="font-size:1.3rem; color:#2d6bc4; margin-bottom:4px;",
                                 shiny::icon("sliders")),
                        tags$strong("2. Parameters"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Set the distribution parameters. Controls update automatically per family.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px; text-align:center;",
                        tags$div(style="font-size:1.3rem; color:#2d6bc4; margin-bottom:4px;",
                                 shiny::icon("hashtag")),
                        tags$strong("3. Sample size"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Set n and optionally fix a random seed for reproducibility.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:9px; text-align:center;",
                        tags$div(style="font-size:1.3rem; color:#2d6bc4; margin-bottom:4px;",
                                 shiny::icon("play")),
                        tags$strong("4. Generate"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Click Generate Data. View the histogram, summary stats, and data preview.")
                      )
                    ),
                    tags$hr(style = "margin:10px 0; border-color:#d0e2f7;"),
                    tags$h6(style = "color:#1e3a5f; font-weight:700; margin-bottom:6px;",
                            shiny::icon("download", class="me-1"), "Output tabs"),
                    tags$div(
                      style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px; font-size:0.83rem;",
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:8px;",
                        tags$strong(shiny::icon("chart-area", class="me-1"), "Plot"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Histogram with the theoretical density overlaid. Download as CSV.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:8px;",
                        tags$strong(shiny::icon("magnifying-glass-chart", class="me-1"), "Summary"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "Descriptive statistics: mean, SD, skewness, kurtosis, min/max, quantiles.")
                      ),
                      tags$div(
                        style = "background:#fff; border:1px solid #cce0f5; border-radius:6px; padding:8px;",
                        tags$strong(shiny::icon("table", class="me-1"), "Data Preview"),
                        tags$p(style="margin:4px 0 0; color:#555;",
                               "First 500 rows of the simulated sample. Download the full dataset as CSV.")
                      )
                    ),
                    tags$div(
                      style = "margin-top:10px; padding:7px 12px; background:#fffbe6;
                               border:1px solid #ffe066; border-radius:5px; font-size:0.82rem; color:#6b4f00;",
                      shiny::icon("lightbulb", class="me-1"),
                      tags$strong("Tip: "),
                      "Generate data, download the CSV, then upload it to the ",
                      tags$strong("Fit Distributions"), " tab to test how well FitVerse recovers the true parameters."
                    )
                  )
                )
              )
            )
          )
        )
        ),

        bslib::navset_card_tab(
          id = "sim_tabs",

          bslib::nav_panel(
            title = "Plot",
            icon  = shiny::icon("chart-area"),
          shiny::uiOutput("sim_dist_info"),
          shiny::plotOutput("sim_hist", height = "420px") |>
            shinycssloaders_wrapper("Generating..."),
          tags$div(
            style = "margin-top:10px;",
            shiny::downloadButton("sim_download_plot", "Download CSV",
                                  class = "btn btn-outline-success btn-sm",
                                  icon  = shiny::icon("download"))
          )
        ),

        bslib::nav_panel(
          title = "Summary",
          icon  = shiny::icon("magnifying-glass-chart"),
          shiny::uiOutput("sim_summary_ui")
        ),

        bslib::nav_panel(
          title = "Data Preview",
          icon  = shiny::icon("table"),
          tags$p(class = "text-muted small mt-2",
                 "Showing up to 500 rows. Download for the full dataset."),
          DT::DTOutput("sim_table") |>
            shinycssloaders_wrapper("Loading..."),
          tags$div(
            style = "margin-top:10px;",
            shiny::downloadButton("sim_download_table", "Download CSV",
                                  class = "btn btn-outline-success btn-sm",
                                  icon  = shiny::icon("download"))
          )
        )
      )
    ) # tagList
  ) # layout_sidebar
  ), # nav_panel Simulate Data

  # -- Nav 6: Method Guide ---------------------------------------------------
  bslib::nav_panel(
    title = "Method Guide",
    icon  = shiny::icon("compass"),

    bslib::layout_columns(
      col_widths = c(12),

      bslib::card(
        bslib::card_header(
          shiny::icon("lightbulb"), " Which estimation method should I use?"
        ),
        bslib::card_body(
          tags$p(
            "FitVerse offers three estimation methods: ",
            tags$strong("Maximum Likelihood (MLE)"),
            ", ",
            tags$strong("Method of Moments (MOM)"),
            ", and ",
            tags$strong("L-Moments (LMOM)"),
            ". The choice matters, especially for small samples, ",
            "extreme-value distributions, and hydrological applications."
          ),

          bslib::layout_columns(
            col_widths = c(4, 4, 4),

            bslib::card(
              class = "border-primary",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::icon("chart-line"), " MLE"
              ),
              bslib::card_body(
                tags$p(tags$strong("Best for: "), "large samples (n > 100), ",
                       "all distribution families, exploratory analysis."),
                tags$p(
                  "Asymptotically efficient (achieves Cramer-Rao bound). ",
                  "Use MLE as the default starting point. Available for all 52 distributions."
                ),
                tags$p(
                  tags$em("Caution: "),
                  "can be sensitive to outliers; less reliable for ",
                  "extreme-value tails at small n."
                )
              )
            ),

            bslib::card(
              class = "border-warning",
              bslib::card_header(
                class = "bg-warning",
                shiny::icon("calculator"), " MOM"
              ),
              bslib::card_body(
                tags$p(tags$strong("Best for: "), "quick checks, closed-form initialisation."),
                tags$p(
                  "Closed-form estimators are fast and transparent. ",
                  "However, higher-order moments (skewness, kurtosis) converge slowly ",
                  "and are highly sensitive to outliers."
                ),
                tags$p(
                  tags$em("Caution: "),
                  "not available for distributions without finite moments ",
                  "(e.g., Cauchy). Generally not preferred over MLE or LMOM."
                )
              )
            ),

            bslib::card(
              class = "border-success",
              bslib::card_header(
                class = "bg-success text-white",
                shiny::icon("water"), " L-Moments"
              ),
              bslib::card_body(
                tags$p(tags$strong("Best for: "), "hydrology, extreme-value analysis, ",
                       "small samples (n < 100), data with outliers."),
                tags$p(
                  "L-moments always exist, are unbiased, and are far more robust ",
                  "to outliers than product moments. Mandated by UK WINFAP and ",
                  "recommended by US Bulletin 17C for flood frequency analysis."
                ),
                tags$p(
                  tags$em("Reference: "),
                  "Hosking (1990, JRSS-B); Hosking & Wallis (1997)."
                )
              )
            )
          ),

          tags$hr(),
          tags$h5(shiny::icon("table"), " Distribution-Specific Recommendations"),
          tags$p(
            tags$strong("Bold"), " = strongly recommended; ",
            tags$em("italics"), " = good alternative; -- = not available in FitVerse."
          ),

          DT::datatable(
            data.frame(
              Distribution = c(
                "Normal", "Logistic", "Skew-Normal", "Cauchy", "Laplace", "Student-t (loc-scale)",
                "Log-Normal", "Gamma", "Exponential",
                "Weibull (2-Param.)", "Weibull (3-Param.)",
                "Log-Logistic", "Inverse Gamma", "Inverse Weibull", "Paralogistic",
                "Pearson Type III", "Log-Pearson Type III",
                "Pareto", "Burr XII",
                "Inverse Gaussian", "Lomax (Pareto II)", "Generalised Gamma",
                "Gompertz", "Birnbaum-Saunders",
                "Gumbel", "Gen. Extreme Value (GEV)", "Gen. Pareto (GPD)", "Gen. Logistic (GLO)",
                "Beta", "Uniform", "Triangular", "Right Triangular", "PERT", "Kumaraswamy",
                "Rayleigh", "Half-Normal", "Exponentiated Weibull",
                "Gamma (3-Parameter)", "Log-Normal (3-Parameter)"
              ),
              Family = c(
                "Symmetric","Symmetric","Symmetric","Symmetric","Symmetric","Symmetric",
                "Right-skewed","Right-skewed","Right-skewed",
                "Right-skewed","Right-skewed",
                "Right-skewed","Right-skewed","Right-skewed","Right-skewed",
                "Right-skewed","Right-skewed",
                "Heavy-tailed","Heavy-tailed",
                "Right-skewed","Heavy-tailed","Right-skewed",
                "Right-skewed","Right-skewed",
                "Extreme-value","Extreme-value","Extreme-value","Extreme-value",
                "Bounded","Bounded","Bounded","Bounded","Bounded","Bounded",
                "Right-skewed","Right-skewed","Right-skewed",
                "Right-skewed","Right-skewed"
              ),
              MLE = c(
                "Y","Y","Y","Y","Y","Y",
                "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
                "Y","Y",
                "Y","Y","Y","Y","Y",
                "Y","Y","Y","Y",
                "Y","Y","Y","Y","Y","Y",
                "Y","Y","Y","Y","Y"
              ),
              MOM = c(
                "Y","Y","--","--","Y","Y",
                "Y","Y","Y","Y","Y","Y","Y","--","--","Y","Y",
                "Y","--",
                "Y","Y","Y","--","Y",
                "Y","--","Y","--",
                "Y","Y","Y","Y","--","Y",
                "Y","Y","--","Y","--"
              ),
              LMOM = c(
                "Y","Y","--","--","--","--",
                "Y","Y","Y","Y","Y","--","--","--","--","Y","Y",
                "--","--",
                "--","--","--","--","--",
                "Y","Y","Y","Y",
                "--","--","--","--","--","--",
                "--","--","--","--","--"
              ),
              Recommended = c(
                "LMOM (n<100); MLE (n>100)",     # Normal
                "LMOM (n<100); MLE (n>100)",     # Logistic
                "MLE only",                       # Skew-Normal
                "MLE only (moments undefined)",   # Cauchy
                "MLE (robust to outliers)",       # Laplace
                "MLE (n>100); MOM (quick check)", # Student-t
                "MLE (n>100); LMOM (n<100)",     # Log-Normal
                "MLE (n>50); LMOM (n<50)",       # Gamma
                "MLE or LMOM",                   # Exponential
                "MLE (n>50); LMOM (hydrology)",  # Weibull 2P
                "MLE; LMOM (hydrology)",          # Weibull 3P
                "MLE",                            # Log-Logistic
                "MLE",                            # Inverse Gamma
                "MLE only",                       # Inverse Weibull
                "MLE only",                       # Paralogistic
                "LMOM (hydrology standard)",      # Pearson III
                "LMOM (Bulletin 17C standard)",   # Log-Pearson III
                "MLE (n>100)",                    # Pareto
                "MLE only",                       # Burr XII
                "MLE (closed-form; UMVUE)",       # Inverse Gaussian
                "MLE (n>100); MOM (quick check)", # Lomax
                "MLE (profile likelihood)",       # Generalised Gamma
                "MLE",                            # Gompertz
                "MLE (closed-form)",              # Birnbaum-Saunders
                "LMOM (lower bias in tails)",     # Gumbel
                "LMOM (n<100); MLE (n>100)",     # GEV
                "LMOM (peaks-over-threshold)",    # GPD
                "LMOM (UK WINFAP standard)",      # GLO
                "MLE",                            # Beta
                "MLE or MOM",                     # Uniform
                "MLE (caution: support bias)",    # Triangular
                "MLE (caution: support bias)",    # Right Triangular
                "MLE only",                       # PERT
                "MLE (closed-form CDF)",          # Kumaraswamy
                "MLE (closed-form)",              # Rayleigh
                "MLE (closed-form)",              # Half-Normal
                "MLE (n>100)",                    # Exponentiated Weibull
                "MLE (profile likelihood)",       # Gamma 3P
                "MLE (profile likelihood)"        # Log-Normal 3P
              ),
              stringsAsFactors = FALSE
            ),
            rownames = FALSE,
            options = list(
              pageLength = 52,
              dom = "ftp",
              columnDefs = list(list(className = "dt-center", targets = 2:4))
            ),
            class = "stripe hover compact"
          ),

          tags$hr(),
          tags$h5(shiny::icon("book-open"), " Key References"),

          tags$p(tags$strong("Estimation Methods")),
          tags$ul(
            tags$li(
              tags$strong("Pearson, K. (1894)."),
              " Contributions to the mathematical theory of evolution. ",
              tags$em("Philosophical Transactions of the Royal Society A"), ", 185, 71-110.",
              tags$span(" [Foundational MOM paper]", style="color:#6c757d;font-size:0.85em;"),
              " ", tags$a("doi:10.1098/rsta.1894.0003", href="https://doi.org/10.1098/rsta.1894.0003", target="_blank")
            ),
            tags$li(
              tags$strong("Fisher, R.A. (1922)."),
              " On the mathematical foundations of theoretical statistics. ",
              tags$em("Philosophical Transactions of the Royal Society A"), ", 222, 309-368.",
              tags$span(" [Foundational MLE paper]", style="color:#6c757d;font-size:0.85em;"),
              " ", tags$a("doi:10.1098/rsta.1922.0009", href="https://doi.org/10.1098/rsta.1922.0009", target="_blank")
            ),
            tags$li(
              tags$strong("Hosking, J.R.M. (1990)."),
              " L-Moments: Analysis and Estimation of Distributions Using Linear
               Combinations of Order Statistics. ",
              tags$em("Journal of the Royal Statistical Society B"), ", 52(1), 105-124.",
              " ", tags$a("doi:10.1111/j.2517-6161.1990.tb01775.x",
                          href="https://doi.org/10.1111/j.2517-6161.1990.tb01775.x", target="_blank")
            ),
            tags$li(
              tags$strong("Hosking, J.R.M. & Wallis, J.R. (1997)."),
              " Regional Frequency Analysis: An Approach Based on L-Moments. ",
              tags$em("Cambridge University Press.")
            ),
            tags$li(
              tags$strong("Delicado, P. & Goria, M.N. (2008)."),
              " A Small Sample Comparison of Maximum Likelihood, Moments and L-Moments Methods. ",
              tags$em("Computational Statistics & Data Analysis"), ", 52(3), 1661-1673.",
              " ", tags$a("doi:10.1016/j.csda.2007.05.012",
                          href="https://doi.org/10.1016/j.csda.2007.05.012", target="_blank")
            )
          ),

          tags$p(tags$strong("Model Selection")),
          tags$ul(
            tags$li(
              tags$strong("Akaike, H. (1974)."),
              " A new look at the statistical model identification. ",
              tags$em("IEEE Transactions on Automatic Control"), ", 19(6), 716-723.",
              tags$span(" [AIC criterion used in FitVerse rankings]", style="color:#6c757d;font-size:0.85em;"),
              " ", tags$a("doi:10.1109/TAC.1974.1100705",
                          href="https://doi.org/10.1109/TAC.1974.1100705", target="_blank")
            ),
            tags$li(
              tags$strong("Schwarz, G. (1978)."),
              " Estimating the dimension of a model. ",
              tags$em("Annals of Statistics"), ", 6(2), 461-464.",
              tags$span(" [BIC criterion]", style="color:#6c757d;font-size:0.85em;"),
              " ", tags$a("doi:10.1214/aos/1176344136",
                          href="https://doi.org/10.1214/aos/1176344136", target="_blank")
            ),
            tags$li(
              tags$strong("Burnham, K.P. & Anderson, D.R. (2002)."),
              " Model Selection and Multimodel Inference: A Practical Information-Theoretic Approach",
              " (2nd ed.). Springer.",
              tags$span(" [DeltaAIC < 2 rule applied in ranking table]", style="color:#6c757d;font-size:0.85em;")
            )
          ),

          tags$p(tags$strong("Goodness-of-Fit Tests")),
          tags$ul(
            tags$li(
              tags$strong("Kolmogorov, A.N. (1933)."),
              " Sulla determinazione empirica di una legge di distribuzione. ",
              tags$em("Giornale dell'Istituto Italiano degli Attuari"), ", 4, 83-91.",
              tags$span(" [KS test]", style="color:#6c757d;font-size:0.85em;")
            ),
            tags$li(
              tags$strong("Anderson, T.W. & Darling, D.A. (1952)."),
              " Asymptotic theory of certain goodness-of-fit criteria based on stochastic processes. ",
              tags$em("Annals of Mathematical Statistics"), ", 23(2), 193-212.",
              tags$span(" [AD test]", style="color:#6c757d;font-size:0.85em;"),
              " ", tags$a("doi:10.1214/aoms/1177729437",
                          href="https://doi.org/10.1214/aoms/1177729437", target="_blank")
            ),
            tags$li(
              tags$strong("Cramer, H. (1928)."),
              " On the composition of elementary errors. ",
              tags$em("Skandinavisk Aktuarietidskrift"), ", 11, 13-74.",
              tags$span(" [CvM test]", style="color:#6c757d;font-size:0.85em;")
            )
          ),

          tags$p(tags$strong("Distributions & Applications")),
          tags$ul(
            tags$li(
              tags$strong("Weibull, W. (1951)."),
              " A statistical distribution function of wide applicability. ",
              tags$em("Journal of Applied Mechanics"), ", 18(3), 293-297.",
              " ", tags$a("doi:10.1115/1.4010337",
                          href="https://doi.org/10.1115/1.4010337", target="_blank")
            ),
            tags$li(
              tags$strong("Stedinger, J.R., Vogel, R.M. & Foufoula-Georgiou, E. (1993)."),
              " Frequency Analysis of Extreme Events. In: Maidment (Ed.), ",
              tags$em("Handbook of Hydrology"), ", Ch. 18. McGraw-Hill."
            ),
            tags$li(
              tags$strong("England et al. (2019)."),
              " Guidelines for Determining Flood Flow Frequency, Bulletin 17C. ",
              tags$em("USGS Techniques and Methods 4-B5."),
              " ", tags$a("doi:10.3133/tm4B5", href="https://doi.org/10.3133/tm4B5", target="_blank")
            )
          ),

          tags$p(tags$strong("R Packages & Software")),
          tags$ul(
            tags$li(
              tags$strong("R Core Team (2024)."),
              " R: A Language and Environment for Statistical Computing. ",
              "R Foundation for Statistical Computing, Vienna, Austria.",
              " ", tags$a("https://www.R-project.org/", href="https://www.R-project.org/", target="_blank")
            ),
            tags$li(
              tags$strong("Delignette-Muller, M.L. & Dutang, C. (2015)."),
              " fitdistrplus: An R Package for Fitting Distributions. ",
              tags$em("Journal of Statistical Software"), ", 64(4), 1-34.",
              " ", tags$a("doi:10.18637/jss.v064.i04",
                          href="https://doi.org/10.18637/jss.v064.i04", target="_blank")
            ),
            tags$li(
              tags$strong("Dutang, C., Goulet, V. & Pigeon, M. (2008)."),
              " actuar: An R Package for Actuarial Science. ",
              tags$em("Journal of Statistical Software"), ", 25(7), 1-37.",
              " ", tags$a("doi:10.18637/jss.v025.i07",
                          href="https://doi.org/10.18637/jss.v025.i07", target="_blank")
            ),
            tags$li(
              tags$strong("Asquith, W.H. (2024)."),
              " lmomco: L-Moments, Censored L-Moments, Trimmed L-Moments, L-Comoments,",
              " and Many Parameter Estimations. R package.",
              " ", tags$a("CRAN:lmomco", href="https://CRAN.R-project.org/package=lmomco", target="_blank")
            )
          ),

          tags$div(
            class = "alert alert-info mt-3",
            shiny::icon("circle-info"), " ",
            tags$strong("Tip: "),
            "When in doubt, select ",
            tags$strong("All Methods"),
            " in the Fit Distributions panel. FitVerse will fit every available
             method for each distribution and rank them together by AIC or BIC,
             letting the data guide the choice."
          )
        )
      )
    )
  ),

  # -- Nav 7: R Code ------------------------------------------------------------
  bslib::nav_panel(
    title = tags$span(shiny::icon("code", class = "me-1"), "R Code"),
    value = "R Code",

    bslib::layout_columns(
      col_widths = c(12),

      bslib::card(
        bslib::card_header(
          style = paste0(
            "background:linear-gradient(135deg,#0f172a 0%,#1e3a5f 100%); ",
            "color:#fff; display:flex; align-items:center; gap:10px; padding:12px 18px;"
          ),
          tags$span(shiny::icon("code",
                                style = "color:#F59E0B; font-size:1.1rem;")),
          tags$span("Generated R Code",
                    style = "font-weight:700; font-size:1rem;"),
          tags$span(
            "LIVE",
            style = paste0(
              "background:#22c55e; color:#fff; font-size:0.62rem; font-weight:800; ",
              "padding:2px 9px; border-radius:20px; letter-spacing:0.10em; margin-left:2px;"
            )
          ),
          tags$span(
            shiny::icon("arrows-rotate",
                        style = "font-size:0.75rem; color:#aecbea; margin-right:4px;"),
            "Updates as you interact with the app",
            style = "margin-left:auto; font-size:0.78rem; color:#aecbea; font-weight:400;"
          )
        ),
        bslib::card_body(
          tags$div(
            style = paste0(
              "background:#f0f7ff; border-left:4px solid #F59E0B; ",
              "border-radius:0 8px 8px 0; padding:10px 14px; ",
              "margin-bottom:14px; font-size:0.86rem; color:#374151;"
            ),
            shiny::icon("circle-info",
                        style = "color:#F59E0B; margin-right:6px;"),
            "This panel tracks every action across all tabs and builds the equivalent R code. ",
            "Download the script to reproduce your analysis outside the app."
          ),
          tags$div(
            style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:14px;",
            shiny::downloadButton(
              "download_rcode",
              label = " Download .R script",
              icon  = shiny::icon("download"),
              class = "btn btn-warning"
            ),
            tags$button(
              id    = "copy_rcode_btn",
              class = "btn btn-outline-secondary btn-sm",
              style = "font-size:0.82rem;",
              onclick = paste0(
                "var el = document.getElementById('rcode_display');",
                "var txt = el ? el.innerText : '';",
                "navigator.clipboard.writeText(txt).then(function(){",
                "  var b = document.getElementById('copy_rcode_btn');",
                "  var orig = b.innerHTML;",
                "  b.innerHTML = '<i class=\"fa fa-check\"></i> Copied!';",
                "  setTimeout(function(){ b.innerHTML = orig; }, 2000);",
                "});"
              ),
              shiny::icon("copy", class = "me-1"),
              "Copy to clipboard"
            )
          ),
          tags$hr(style = "margin:4px 0 14px;"),
          shiny::verbatimTextOutput("rcode_display")
        )
      )
    )
  ),

  # -- Nav 8: Help (rightmost -- utility/about panel) -------------------------
  bslib::nav_panel(
    title = "Help",
    icon  = shiny::icon("circle-question"),

    bslib::layout_columns(
      col_widths = c(7, 5),

      bslib::card(
        bslib::card_header("User Guide"),
        bslib::card_body(
          tags$h5("Getting started"),
          tags$ol(
            tags$li("Upload a CSV file or paste numbers in the ", tags$strong("Fit Distributions"), " sidebar."),
            tags$li("Enter a descriptive variable label (e.g. 'Annual rainfall (mm)')."),
            tags$li("Choose ", tags$strong("Auto"), " to try all compatible distributions, or ", tags$strong("Manual"), " to pick specific families."),
            tags$li("Select an estimation method: MLE (default), MOM, L-Moments, or all three simultaneously."),
            tags$li("Choose the ranking criterion (AIC or BIC) and the number of overlay curves."),
            tags$li("Click ", tags$strong("Fit Distributions"), "."),
            tags$li("Explore results across the Plot, Rankings, Data Summary, Return Levels, Bootstrap CI, and Download tabs.")
          ),
          tags$h5("Tabs explained"),
          tags$dl(
            tags$dt("Plot"),
            tags$dd("Interactive density curve overlaid on a histogram of your data.
                     Hover for exact values; click legend entries to toggle curves."),
            tags$dt("Rankings"),
            tags$dd("All fitted distributions ranked by AIC (or BIC). Columns include
                     Delta AIC, log-likelihood, and KS / AD goodness-of-fit p-values.
                     The best-fit row is highlighted."),
            tags$dt("Data Summary"),
            tags$dd("Descriptive statistics (mean, SD, skewness, kurtosis, quartiles)
                     and a mini histogram of your data."),
            tags$dt("Return Levels"),
            tags$dd("T-year return levels for any return periods you specify.
                     Useful for flood frequency, extreme rainfall, and reliability analysis."),
            tags$dt("Bootstrap CI"),
            tags$dd("Percentile bootstrap confidence intervals for the best-fit parameters.
                     Optionally include CI for return levels by entering return periods."),
            tags$dt("Download"),
            tags$dd("Export a self-contained HTML report, a high-resolution PNG plot,
                     or the cleaned data as CSV.")
          ),
          tags$h5("Batch Fit"),
          tags$p("Upload a multi-column CSV in the ", tags$strong("Batch Fit"), " tab
                  to fit the best distribution to every numeric column simultaneously.
                  The results table can be downloaded as CSV.")
        )
      ),

      tagList(
        bslib::card(
          bslib::card_header("About FitVerse"),
          bslib::card_body(
            tags$p(
              "FitVerse fits ", tags$strong("52 parametric probability distributions"),
              " to continuous univariate data using three estimation methods:",
              " Maximum Likelihood (MLE), Method of Moments (MOM), and",
              " L-Moments (LMOM), and picks the best model by AIC or BIC.",
              " It covers symmetric, right-skewed, heavy-tailed, extreme-value,",
              " hydrological, and bounded families - useful across a wide",
              " range of settings from engineering and hydrology to finance."
            ),
            tags$hr(class = "my-2"),
            tags$h6(shiny::icon("users", class = "me-1"), "Authors"),
            tags$table(
              class = "table table-borderless table-sm mb-2",
              style = "font-size:0.85rem;",
              tags$tbody(
                tags$tr(
                  tags$td(tags$strong("Karuna G. Reddy")),
                  tags$td(tags$span(class = "text-muted", "University of Auckland, New Zealand"))
                ),
                tags$tr(
                  tags$td(tags$strong("M. G. M. Khan")),
                  tags$td(tags$span(class = "text-muted", "University of the South Pacific, Fiji"))
                )
              )
            ),
            tags$h6(shiny::icon("quote-left", class = "me-1"), "Citation"),
            tags$blockquote(
              class = "blockquote",
              style = "font-size:0.82rem; border-left:3px solid #dee2e6; padding-left:10px; color:#555;",
              tags$p(
                class = "mb-0",
                "Reddy, K.G. & Khan, M.G.M. (2026). ",
                tags$em("FitVerse: Parametric Distribution Fitting and Analysis."),
                " R package version 1.0-2."
              )
            ),
            tags$h6(shiny::icon("link", class = "me-1"), "Links"),
            tags$p(
              class = "mb-0",
              tags$a(shiny::icon("box"), " CRAN",
                     href   = "https://cran.r-project.org/package=FitVerse",
                     target = "_blank")
            )
          )
        ),

        bslib::card(
          bslib::card_header("Supported Distributions"),
          bslib::card_body(
            tags$ul(
              lapply(names(FV_DIST_CHOICES), function(fam)
                tags$li(tags$strong(fam), ": ",
                        paste(names(FV_DIST_CHOICES[[fam]]), collapse = ", ")))
            )
          )
        )
      )
    )
  ),

  # -- Nav 9: Explore Distributions (StatAssist) --------------------------------
  bslib::nav_panel(
    title = "Explore Distributions",
    icon  = shiny::icon("magnifying-glass"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        open  = TRUE,

        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "1"
          ),
          "Distribution"
        ),
        shiny::selectInput(
          "explore_dist", NULL,
          choices  = FV_DIST_CHOICES,
          selected = "normal"
        ),

        tags$hr(class = "sidebar-divider"),

        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "2"
          ),
          "Parameters"
        ),
        shiny::uiOutput("explore_param_ui"),

        tags$hr(class = "sidebar-divider"),

        tags$div(class = "sidebar-title",
          tags$span(
            style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                           "width:17px;height:17px;border-radius:50%;background:#1e3a5f;",
                           "color:#fff;font-size:0.62rem;font-weight:800;margin-right:7px;",
                           "flex-shrink:0;"),
            "3"
          ),
          "Curve to display"
        ),
        shiny::radioButtons(
          "explore_curve", NULL,
          choices = c("PDF", "CDF", "Survival", "Hazard", "Cumulative Hazard"),
          selected = "PDF",
          inline  = FALSE
        )
      ),

      tagList(
        bslib::navset_card_tab(
          id = "explore_tabs",

          bslib::nav_panel(
            title = "Plot",
            icon  = shiny::icon("chart-area"),
            shiny::uiOutput("explore_dist_banner"),
            plotly::plotlyOutput("explore_plot", height = "400px") |>
              shinycssloaders_wrapper("Building curve...")
          ),

          bslib::nav_panel(
            title = "Statistics",
            icon  = shiny::icon("table"),
            shiny::uiOutput("explore_stats_ui")
          ),

          bslib::nav_panel(
            title = "About",
            icon  = shiny::icon("circle-info"),
            shiny::uiOutput("explore_about_ui")
          )
        )
      )
    )
  ),

  # Trailing nav spacer + version badge
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$span("v1.0-3",
              style = "color:#aecbea; font-size:0.75rem; padding:8px 4px;")
  )
)


# Helper functions (shinycssloaders_wrapper, stat_card_html, fmt_pval)
# are defined in global.R so they are available before the UI is evaluated.


# +===========================================================+
# |  Server                                                   |
# +===========================================================+

server <- function(input, output, session) {

  # -- Demo data --------------------------------------------------------------
  # Stores list(x = numeric_vector, xlab = "label") when a demo is active.
  demo_rv <- shiny::reactiveVal(NULL)

  # Description blurb for the selected demo
  output$demo_description_ui <- shiny::renderUI({
    key <- input$demo_choice
    if (is.null(key) || !nzchar(key)) return(NULL)
    demo <- FV_DEMOS[[key]]
    if (is.null(demo)) return(NULL)
    tags$div(
      style = paste0(
        "background:#f0f5ff; border-left:3px solid #3d7ebf; ",
        "border-radius:0 5px 5px 0; padding:7px 10px; ",
        "font-size:0.78rem; color:#3a5175; margin-top:6px; margin-bottom:2px;"
      ),
      demo$desc
    )
  })

  # Load demo dataset when button is clicked
  shiny::observeEvent(input$load_demo, {
    key <- input$demo_choice
    if (is.null(key) || !nzchar(key)) {
      shiny::showNotification("Please choose a dataset first.",
                              type = "warning", duration = 3)
      return()
    }
    demo <- FV_DEMOS[[key]]
    x <- tryCatch(demo$load(), error = function(e) NULL)
    if (is.null(x) || length(x) < 4) {
      shiny::showNotification("Failed to load demo dataset.",
                              type = "error", duration = 5)
      return()
    }
    demo_rv(list(x = x, xlab = demo$xlab))
    shiny::updateTextInput(session, "xlab_input", value = demo$xlab)
    shiny::showNotification(
      paste0("OK Loaded '", demo$label, "' (n?=?", length(x), "). ",
             "Click 'Fit Distributions' to continue."),
      type = "message", duration = 4
    )
  })

  # Clear demo when the user uploads a real file or pastes data
  shiny::observeEvent(input$file_upload,
    { demo_rv(NULL) }, ignoreNULL = TRUE)
  shiny::observeEvent(input$paste_data, {
    if (nzchar(trimws(input$paste_data %||% ""))) demo_rv(NULL)
  })

  # -- Reactive: raw data vector ----------------------------------------------
  raw_data <- shiny::reactive({
    # Priority 1: demo data (loaded via Demo tab)
    demo <- demo_rv()
    if (!is.null(demo)) return(data.frame(value = demo$x))

    # Priority 2: uploaded file
    if (!is.null(input$file_upload)) {
      df <- tryCatch(
        utils::read.csv(input$file_upload$datapath,
                        header = input$header,
                        sep    = input$sep,
                        stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df)) return(df)
    }

    txt <- shiny::req(input$paste_data)
    txt <- trimws(txt)
    if (!nzchar(txt)) return(NULL)

    # Try comma-separated first, then newline-separated
    vals <- tryCatch({
      nums <- suppressWarnings(as.numeric(strsplit(txt, "[,\n\r\t ]+")[[1]]))
      nums[!is.na(nums)]
    }, error = function(e) NULL)

    if (length(vals) == 0) return(NULL)
    data.frame(value = vals)
  })

  # -- UI: column picker (only for CSV upload) --------------------------------
  output$col_picker <- shiny::renderUI({
    df <- raw_data()
    shiny::req(df, input$file_upload)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(num_cols) == 0) return(
      tags$p("No numeric columns found.", style = "color:#e74c3c; font-size:0.83rem;")
    )
    shiny::selectInput("selected_col", "Select column", choices = num_cols)
  })

  # -- Reactive: clean numeric vector ----------------------------------------
  clean_vec <- shiny::reactive({
    # When a grouped fit has been run, use its pseudo-observations so that
    # all downstream plots (CDF, Q-Q, Boxplot, LMRD, etc.) render correctly
    # without requiring a raw data upload in the regular sidebar.
    gfv <- grouped_fv_val()
    if (shiny::isTruthy(gfv)) return(gfv$data)

    df <- raw_data()
    shiny::req(df)

    if (!is.null(input$file_upload) && !is.null(input$selected_col) &&
        input$selected_col %in% names(df)) {
      x <- df[[input$selected_col]]
    } else {
      x <- df[[1]]
    }

    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) < 4) return(NULL)
    x
  })

  # -- Reactive: xlab --------------------------------------------------------
  # Priority: grouped-fit label > user-typed label > selected column name > "x"
  xlab_val <- shiny::reactive({
    gfv <- grouped_fv_val()
    if (shiny::isTruthy(gfv) && !is.null(gfv$data_summary$xlab) &&
        nzchar(gfv$data_summary$xlab)) return(gfv$data_summary$xlab)
    lbl <- trimws(input$xlab_input)
    if (nzchar(lbl)) return(lbl)
    col <- input$selected_col
    if (!is.null(col) && nzchar(col)) {
      # Replace underscores / dots with spaces for a readable axis label
      gsub("[_.]", " ", col)
    } else {
      "x"
    }
  })

  # -- Sidebar status --------------------------------------------------------
  output$sidebar_status <- shiny::renderUI({
    x <- clean_vec()
    if (is.null(x)) {
      tags$div(class = "fv-status-box",
               shiny::icon("circle-info"), " Upload data or paste values to begin.")
    } else {
      tags$div(class = "fv-status-box success",
               shiny::icon("circle-check"),
               sprintf(" %d observations ready.", length(x)))
    }
  })

  # -- Main reactive: run FitVerse -------------------------------------------
  # Grouped-data override (NULL until user clicks 'Fit Grouped Data')
  grouped_fv_val <- shiny::reactiveVal(NULL)

  fit_result_regular <- shiny::eventReactive(input$run_btn, {
    x <- clean_vec()
    shiny::validate(
      shiny::need(!is.null(x) && length(x) >= 4,
                  "Please provide at least 4 valid numeric observations.")
    )

    # Resolve distributions
    dists <- if (input$dist_mode == "auto") {
      "all"
    } else {
      shiny::validate(
        shiny::need(length(input$manual_dists) >= 1,
                    "Please select at least one distribution.")
      )
      input$manual_dists
    }

    # Resolve methods
    method <- if (input$method == "ALL") c("MLE","MOM","LMOM") else input$method

    # Disable run button during fitting
    session$sendCustomMessage("fv_disable_btn", list(disabled = TRUE))
    on.exit(session$sendCustomMessage("fv_disable_btn", list(disabled = FALSE)))

    # Progress
    shiny::withProgress(
      message = "Fitting distributions...",
      detail  = "This may take a few seconds.",
      value   = 0, {

        prog_fn <- function(p) shiny::setProgress(p)

        fv <- withCallingHandlers(
          tryCatch(
            fitverse(
              x         = x,
              dists     = dists,
              method    = method,
              criterion = input$criterion,
              xlab      = xlab_val(),
              top_n     = input$top_n,
              plot      = FALSE,
              verbose   = FALSE,
              progress  = prog_fn
            ),
            error = function(e) {
              shiny::showNotification(
                paste("Fitting error:", conditionMessage(e)),
                type     = "error",
                duration = 8
              )
              NULL
            }
          ),
          message = function(m) {
            txt <- conditionMessage(m)
            if (grepl("[FV_NOTIFY]", txt, fixed = TRUE)) {
              clean <- trimws(sub("^\\s*\\[FV_NOTIFY\\]\\s*", "", txt))
              shiny::showNotification(
                clean,
                type     = "warning",
                duration = 10
              )
            }
            invokeRestart("muffleMessage")
          }
        )

        shiny::setProgress(1)
        fv
      }
    )
  })

  # Effective fit_result: returns grouped fit when available, else regular fit.
  # All 20+ downstream call sites automatically use grouped data when set.
  fit_result <- shiny::reactive({
    if (shiny::isTruthy(grouped_fv_val())) return(grouped_fv_val())
    fit_result_regular()
  })

  # -- Grouped data: preview uploaded frequency table ------------------------
  output$grouped_preview_ui <- shiny::renderUI({
    req_file <- input$grouped_upload
    if (is.null(req_file)) return(NULL)
    df <- tryCatch(
      read_grouped_csv(req_file$datapath),
      error = function(e) NULL
    )
    if (is.null(df)) {
      return(tags$p(style = "color:#e74c3c; font-size:0.8rem;",
                    "Could not parse file. Check format (see hint above)."))
    }
    n_obs <- if (!is.null(df$counts)) sum(df$counts) else NA
    tbl <- if (!is.null(df$midpoints)) {
      data.frame(Midpoint = df$midpoints, Count = df$counts)
    } else {
      data.frame(Lower = df$lower, Upper = df$upper, Count = df$counts)
    }
    tagList(
      tags$p(
        shiny::icon("circle-check", style = "color:#27ae60;"),
        sprintf(" %d classes, n = %d", nrow(tbl), n_obs),
        style = "font-size:0.8rem; margin-bottom:6px; color:#27ae60;"
      ),
      DT::datatable(
        tbl,
        options = list(dom = "t", pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class    = "compact stripe"
      )
    )
  })

  # -- Grouped Fit: main results panel --------------------------------------
  output$grouped_fit_main_ui <- shiny::renderUI({
    gfv <- grouped_fv_val()

    if (is.null(gfv)) {
      # Empty state
      tags$div(
        class = "fv-empty-state",
        style = paste0(
          "display:flex;flex-direction:column;align-items:center;",
          "justify-content:center;min-height:320px;color:#7a8fa6;text-align:center;"
        ),
        shiny::icon("table", style = "font-size:2.8rem;color:#c5d3e0;margin-bottom:14px;"),
        tags$h5(style = "color:#7a8fa6;font-weight:600;margin:0 0 8px;",
                "No grouped fit yet"),
        tags$p(style = "font-size:0.87rem;max-width:320px;",
               "Upload a frequency table CSV, choose distributions and method, then click ",
               tags$strong("Fit Grouped Data"), "."),
        tags$p(
          style = "font-size:0.82rem;color:#aab9c9;margin-top:6px;",
          shiny::icon("circle-info", style = "font-size:0.8rem;"),
          " Full plots (Q-Q, P-P, CDF, density) appear automatically in the ",
          tags$strong("Fit Distributions"), " tab once fitting is done."
        )
      )
    } else {
      # Results: best-fit card + top-5 rankings + redirect hint
      bf        <- gfv$best_fit
      entry     <- tryCatch(get_dist_entry(bf$dist_key), error = function(e) NULL)
      dname     <- if (!is.null(entry)) entry$display_name else bf$dist_key
      rnk       <- gfv$ranking
      top5      <- head(rnk, 5L)

      # Format parameter table
      param_rows <- lapply(seq_along(bf$params), function(i) {
        tags$tr(
          tags$td(style = "padding:3px 10px;font-family:monospace;font-size:0.82rem;color:#3a5175;",
                  names(bf$params)[i]),
          tags$td(style = "padding:3px 10px;font-size:0.82rem;",
                  format(round(bf$params[i], 5), scientific = FALSE))
        )
      })

      # Format ranking rows
      rank_rows <- lapply(seq_len(nrow(top5)), function(i) {
        bg <- if (i == 1) "#eaf6ec" else if (i == 2) "#eef4ff" else "transparent"
        rank_entry <- tryCatch(get_dist_entry(top5$dist_key[i]), error = function(e) NULL)
        dn2 <- if (!is.null(rank_entry)) rank_entry$display_name else top5$dist_key[i]
        tags$tr(
          style = paste0("background:", bg, ";"),
          tags$td(style = "padding:4px 10px;font-weight:700;text-align:center;", i),
          tags$td(style = "padding:4px 10px;", dn2),
          tags$td(style = "padding:4px 10px;", top5$Method[i]),
          tags$td(style = "padding:4px 10px;font-family:monospace;font-size:0.82rem;",
                  if (!is.null(top5$AIC)) round(top5$AIC[i], 2) else "—"),
          tags$td(style = "padding:4px 10px;font-family:monospace;font-size:0.82rem;",
                  if (!is.null(top5$BIC)) round(top5$BIC[i], 2) else "—")
        )
      })

      tagList(
        # Best fit card
        tags$div(
          style = paste0(
            "background:linear-gradient(135deg,#eaf6ec,#f7fff8);",
            "border:1.5px solid #4caf50;border-radius:10px;padding:16px 20px;",
            "margin-bottom:18px;"
          ),
          tags$div(
            style = "display:flex;align-items:center;gap:12px;margin-bottom:10px;",
            shiny::icon("trophy", style = "font-size:1.4rem;color:#2e7d32;"),
            tags$div(
              tags$div(style = "font-size:0.74rem;color:#6a9f6b;font-weight:600;letter-spacing:.05em;",
                       "BEST FIT  —  GROUPED MLE"),
              tags$div(style = "font-size:1.15rem;font-weight:700;color:#2e7d32;", dname)
            ),
            tags$div(
              style = "margin-left:auto;text-align:right;",
              tags$div(style = "font-size:0.74rem;color:#6a9f6b;", "Method"),
              tags$div(style = "font-size:0.88rem;font-weight:600;", bf$method)
            )
          ),
          tags$div(
            style = "display:grid;grid-template-columns:repeat(3,1fr);gap:8px;margin-bottom:10px;",
            tags$div(
              style = "background:#fff;border-radius:6px;padding:6px 10px;text-align:center;border:1px solid #c8e6c9;",
              tags$div(style = "font-size:0.72rem;color:#6a9f6b;font-weight:600;", "AIC"),
              tags$div(style = "font-size:0.95rem;font-weight:700;", round(bf$aic, 2))
            ),
            tags$div(
              style = "background:#fff;border-radius:6px;padding:6px 10px;text-align:center;border:1px solid #c8e6c9;",
              tags$div(style = "font-size:0.72rem;color:#6a9f6b;font-weight:600;", "BIC"),
              tags$div(style = "font-size:0.95rem;font-weight:700;", round(bf$bic, 2))
            ),
            tags$div(
              style = "background:#fff;border-radius:6px;padding:6px 10px;text-align:center;border:1px solid #c8e6c9;",
              tags$div(style = "font-size:0.72rem;color:#6a9f6b;font-weight:600;", "Log-Lik"),
              tags$div(style = "font-size:0.95rem;font-weight:700;", round(bf$loglik, 2))
            )
          ),
          # Parameter table
          tags$div(
            style = "font-size:0.8rem;font-weight:600;color:#4a7c4e;margin-bottom:4px;",
            "Parameters"
          ),
          tags$table(
            style = paste0(
              "width:100%;border-collapse:collapse;background:#fff;",
              "border-radius:6px;overflow:hidden;border:1px solid #c8e6c9;"
            ),
            tags$tbody(!!!param_rows)
          )
        ),

        # Top-5 rankings
        tags$div(
          style = "font-size:0.88rem;font-weight:700;color:#1e3a5f;margin-bottom:8px;",
          "Top 5 Distributions"
        ),
        tags$div(
          style = "overflow-x:auto;",
          tags$table(
            style = paste0(
              "width:100%;border-collapse:collapse;font-size:0.83rem;",
              "border:1px solid #dce6f0;border-radius:8px;overflow:hidden;"
            ),
            tags$thead(
              tags$tr(
                style = "background:#1e3a5f;color:#fff;",
                tags$th(style="padding:6px 10px;text-align:center;", "#"),
                tags$th(style="padding:6px 10px;", "Distribution"),
                tags$th(style="padding:6px 10px;", "Method"),
                tags$th(style="padding:6px 10px;", "AIC"),
                tags$th(style="padding:6px 10px;", "BIC")
              )
            ),
            tags$tbody(!!!rank_rows)
          )
        ),

        # Redirect hint
        tags$div(
          style = paste0(
            "margin-top:16px;padding:10px 14px;background:#f0f5ff;",
            "border-left:4px solid #3d7ebf;border-radius:0 6px 6px 0;",
            "font-size:0.83rem;color:#3a5175;"
          ),
          shiny::icon("circle-arrow-right", style = "margin-right:6px;color:#3d7ebf;"),
          "Full diagnostic plots (Q-Q, P-P, CDF overlay, density) are in the ",
          tags$strong("Fit Distributions"), " tab — the grouped fit result is loaded there automatically."
        )
      )
    }
  })

  # -- Grouped data: run fitting on button click -----------------------------
  shiny::observeEvent(input$run_grouped, {
    req_file <- input$grouped_upload
    shiny::req(!is.null(req_file))

    gdata <- tryCatch(
      read_grouped_csv(req_file$datapath),
      error = function(e) {
        shiny::showNotification(
          paste("Could not read grouped file:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      }
    )
    shiny::req(!is.null(gdata))

    xlab_g <- trimws(input$grouped_xlab)
    if (!nzchar(xlab_g)) xlab_g <- tools::file_path_sans_ext(req_file$name)

    dists_g <- if (input$grouped_dist_mode == "auto") "auto" else input$grouped_dists
    if (length(dists_g) == 0) dists_g <- "auto"

    session$sendCustomMessage("fv_disable_btn", list(disabled = TRUE))
    on.exit(session$sendCustomMessage("fv_disable_btn", list(disabled = FALSE)))

    shiny::withProgress(
      message = "Fitting grouped distributions...",
      detail  = "Using grouped likelihood for MLE...",
      value   = 0, {

        fv <- tryCatch({
          shiny::setProgress(0.1)
          args <- c(gdata, list(
            dists   = dists_g,
            method  = if (input$method == "ALL") "All" else input$method,
            xlab    = xlab_g,
            verbose = FALSE
          ))
          do.call(fitverse_grouped, args)
        }, error = function(e) {
          shiny::showNotification(
            paste("Grouped fitting failed:", conditionMessage(e)),
            type = "error", duration = 10
          )
          NULL
        })

        shiny::setProgress(1)
        if (!is.null(fv)) {
          grouped_fv_val(fv)
          shiny::showNotification(
            sprintf(
              "Grouped fit complete. Best: %s (%s)",
              get_dist_entry(fv$best_fit$dist_key)$display_name,
              fv$best_fit$method
            ),
            type = "message", duration = 5
          )
        }
      }
    )
  })

  # Clear grouped override when regular fit is run
  shiny::observeEvent(input$run_btn, {
    grouped_fv_val(NULL)
  }, priority = 100)

  # -- Reactive: which fit to display (clicked row, or rank-1 default) ------
  # Reads the single-row selection from the ranking DT and looks up the
  # corresponding fit in fv$all_fits.  Falls back to fv$best_fit (rank 1)
  # when no row is selected or the selection is out of range.
  selected_fit_rv <- shiny::reactive({
    fv <- fit_result()
    shiny::req(fv)
    row_idx <- input$ranking_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) return(fv$best_fit)
    ranking <- fv$ranking
    if (row_idx > nrow(ranking)) return(fv$best_fit)
    sel      <- ranking[row_idx, ]
    fit_key  <- paste0(sel$dist_key, "_", sel$Method)
    f        <- fv$all_fits[[fit_key]]
    if (is.null(f)) fv$best_fit else f
  })

  # -- Reactive: fv object with best_fit swapped to the selected row --------
  # Overview, Plot header, and Q-Q plot all read from display_fv() so they
  # update when the user clicks a different row in the ranking table.
  # The density overlay (interactive_plot) continues to show top-N and is
  # NOT modified -- it is a comparison view, not a per-distribution view.
  display_fv <- shiny::reactive({
    fv  <- fit_result()
    shiny::req(fv)
    sel <- selected_fit_rv()
    fv$best_fit <- sel
    fv
  })

  # -- Backup MathJax trigger ------------------------------------------------
  # withMathJax() inside renderUI fires a typeset script each time it runs,
  # but on the very first fit the CDN script may not yet be evaluated when
  # that inline script runs.  This observeEvent fires 300 ms later as a
  # safety net -- by then MathJax is definitely loaded.
  shiny::observeEvent(fit_result(), {
    session$sendCustomMessage("mathjax_typeset_eq", list())
  }, ignoreNULL = TRUE)

  # -- Overview panel --------------------------------------------------------
  output$overview_panel <- shiny::renderUI({
    fv <- display_fv()
    x  <- clean_vec()

    if (is.null(fv) || is.null(x)) {
      return(tags$div(
        style = paste0(
          "display:flex; flex-direction:column; align-items:center; ",
          "justify-content:center; padding:72px 24px; text-align:center;"
        ),
        tags$div(
          style = paste0(
            "width:72px; height:72px; border-radius:50%; ",
            "background:linear-gradient(135deg,#e8f1fa,#dbeafe); ",
            "display:flex; align-items:center; justify-content:center; ",
            "margin-bottom:20px; box-shadow:0 4px 16px rgba(59,130,246,0.12);"
          ),
          tags$span(style = "font-size:2rem; color:#3b82f6; opacity:0.85;",
                    shiny::icon("chart-simple"))
        ),
        tags$div(
          style = "font-size:1.05rem; font-weight:700; color:#1e3a5f; margin-bottom:8px;",
          "Ready to fit"
        ),
        tags$div(
          style = "font-size:0.85rem; color:#64748b; max-width:280px; line-height:1.6;",
          "Upload a CSV or paste your data in the sidebar, then click ",
          tags$strong(style="color:#1e3a5f;", "Fit Distributions"),
          " to get started."
        )
      ))
    }

    bf    <- fv$best_fit
    entry <- get_dist_entry(bf$dist_key)
    ds    <- fv$data_summary
    gof_ok <- !is.null(bf$gof) && nrow(bf$gof) > 0

    # Is this the rank-1 best fit, or a user-selected row?
    sel_row  <- input$ranking_table_rows_selected
    is_selected <- !is.null(sel_row) && length(sel_row) > 0 && sel_row > 1L
    hero_label  <- if (is_selected)
      paste0("Selected distribution (Rank ", sel_row, ")")
    else
      "Best-fit distribution (Rank 1)"

    # helpers
    metric_pill <- function(label, value, icon_name, accent = "#1e3a5f") {
      tags$div(
        style = paste0(
          "display:flex; align-items:center; gap:10px; ",
          "background:#fff; border:1px solid #e9ecef; border-radius:10px; ",
          "padding:10px 16px; flex:1; min-width:110px;"
        ),
        tags$div(
          style = paste0(
            "width:34px; height:34px; border-radius:8px; ",
            "background:", accent, "15; color:", accent, "; ",
            "display:flex; align-items:center; justify-content:center; ",
            "font-size:0.95rem; flex-shrink:0;"
          ),
          shiny::icon(icon_name)
        ),
        tags$div(
          tags$div(style = "font-size:0.7rem; color:#6c757d; font-weight:600;
                            text-transform:uppercase; letter-spacing:0.04em;
                            line-height:1;",
                   label),
          tags$div(style = "font-size:0.95rem; font-weight:700; color:#1a1a2e;
                            margin-top:3px; line-height:1.2;",
                   value)
        )
      )
    }

    tagList(
      # -- Hero banner ------------------------------------------------------
      tags$div(
        style = paste0(
          "background: linear-gradient(135deg, #1e3a5f 0%, #2d5a9e 100%); ",
          "border-radius:12px; padding:20px 24px; margin-bottom:16px; ",
          "display:flex; align-items:center; justify-content:space-between; ",
          "flex-wrap:wrap; gap:12px;"
        ),
        tags$div(
          tags$div(style = "font-size:0.72rem; color:#aecbea; font-weight:600;
                            text-transform:uppercase; letter-spacing:0.08em;",
                   hero_label),
          tags$div(style = "font-size:1.6rem; font-weight:700; color:#ffffff;
                            line-height:1.2; margin-top:2px;",
                   entry$display_name),
          tags$div(
            style = "margin-top:8px; display:flex; gap:8px; flex-wrap:wrap;",
            tags$span(
              style = "background:rgba(255,255,255,0.15); color:#fff;
                       border-radius:20px; padding:3px 10px; font-size:0.78rem;
                       font-weight:500;",
              shiny::icon("gear"), " ", bf$method
            ),
            tags$span(
              style = "background:rgba(255,255,255,0.15); color:#fff;
                       border-radius:20px; padding:3px 10px; font-size:0.78rem;
                       font-weight:500;",
              "AIC = ", round(bf$aic, 2)
            ),
            tags$span(
              style = "background:rgba(255,255,255,0.15); color:#fff;
                       border-radius:20px; padding:3px 10px; font-size:0.78rem;
                       font-weight:500;",
              "BIC = ", round(bf$bic, 2)
            ),
            tags$span(
              style = "background:rgba(255,255,255,0.15); color:#fff;
                       border-radius:20px; padding:3px 10px; font-size:0.78rem;
                       font-weight:500;",
              "n = ", format(fv$n, big.mark = ",")
            )
          )
        ),
        tags$div(
          style = "font-size:3.5rem; color:rgba(255,255,255,0.12);
                   line-height:1; user-select:none;",
          shiny::icon("award")
        )
      ),

      # -- Metric pills row -------------------------------------------------
      tags$div(
        style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:16px;",
        metric_pill("Log-likelihood", round(bf$loglik, 2),
                    "wave-square", "#1e3a5f"),
        metric_pill("Parameters",    bf$n_params,
                    "sliders",      "#2d5a9e"),
        metric_pill("Sample size",   format(fv$n, big.mark = ","),
                    "database",     "#3d7ebf"),
        metric_pill("Converged",
                    if (isTRUE(bf$converged)) "Yes" else "No",
                    if (isTRUE(bf$converged)) "circle-check" else "circle-xmark",
                    if (isTRUE(bf$converged)) "#28a745" else "#dc3545")
      ),

      # -- PDF Equation box (full width) -------------------------------------
      {
        dist_key <- bf$dist_key
        eq_latex <- FV_PDF_EQUATIONS[[dist_key]]
        if (!is.null(eq_latex)) {
          # withMathJax() is ALSO in the static header (early CDN load), but we
          # keep it here too because its non-singleton trailing <script> tag
          # fires MathJax.Hub.Queue(["Typeset", MathJax.Hub]) every time this
          # renderUI block re-executes -- that is the reliable per-fit trigger.
          shiny::withMathJax(
            tags$div(
              id = "fv-pdf-eq",
              style = paste0(
                "background: linear-gradient(135deg, #f0f4ff 0%, #e8f0fb 100%); ",
                "border: 2px solid #3d7ebf; border-radius: 12px; ",
                "padding: 14px 20px; margin-bottom: 14px; ",
                "text-align: center;"
              ),
              tags$div(
                style = "font-size:0.72rem; font-weight:700; color:#2c5f9e;
                         text-transform:uppercase; letter-spacing:0.07em;
                         margin-bottom:6px;",
                shiny::icon("square-root-variable", class = "me-1"),
                "Probability Density Function"
              ),
              tags$div(
                style = "font-size:1.1rem; color:#1a1a2e; line-height:1.8;
                         overflow-x:auto;",
                HTML(eq_latex)
              )
            )
          )
        }
      },

      # -- Parameters + GoF side by side ------------------------------------
      bslib::layout_columns(
        col_widths = c(5, 7),

        # Parameters card
        tags$div(
          style = paste0(
            "background:#fff; border:1px solid #e9ecef; border-radius:12px; ",
            "overflow:hidden; height:100%;"
          ),
          tags$div(
            style = paste0(
              "background:#f8f9fa; border-bottom:1px solid #e9ecef; ",
              "padding:10px 16px; display:flex; align-items:center; ",
              "justify-content:space-between;"
            ),
            tags$span(
              style = "font-size:0.82rem; font-weight:700; color:#1a1a2e;
                       text-transform:uppercase; letter-spacing:0.05em;",
              shiny::icon("sliders"), " Parameter Estimates"
            ),
            tags$span(
              style = paste0(
                "background:#1e3a5f; color:#fff; border-radius:20px; ",
                "padding:2px 10px; font-size:0.72rem; font-weight:600;"
              ),
              bf$method
            )
          ),
          tags$div(
            style = "padding:0;",
            tags$table(
              style = "width:100%; border-collapse:collapse; font-size:0.88rem;",
              tags$tbody(
                lapply(seq_along(bf$params), function(i) {
                  pname    <- names(bf$params)[i]
                  sym_map  <- FV_PARAM_SYMBOLS[[bf$dist_key]]
                  sym_label <- if (!is.null(sym_map) && pname %in% names(sym_map))
                    sym_map[[pname]] else pname
                  is_even <- i %% 2 == 0
                  tags$tr(
                    style = paste0(
                      "border-bottom:1px solid #f0f0f0;",
                      if (is_even) "background:#fafbfc;" else "background:#fff;"
                    ),
                    tags$td(
                      style = "padding:9px 16px; color:#495057; font-weight:500;",
                      tags$span(
                        style = paste0(
                          "display:inline-block; width:7px; height:7px; ",
                          "border-radius:50%; background:#3d7ebf; ",
                          "margin-right:8px; vertical-align:middle;"
                        )
                      ),
                      sym_label
                    ),
                    tags$td(
                      style = "padding:9px 16px; text-align:right;
                               font-family:'Courier New', monospace;
                               font-weight:700; color:#1e3a5f; font-size:0.87rem;",
                      formatC(bf$params[i], format = "f", digits = 5)
                    )
                  )
                })
              )
            )
          )
        ),

        # GoF card
        if (gof_ok) {
          tags$div(
            style = paste0(
              "background:#fff; border:1px solid #e9ecef; border-radius:12px; ",
              "overflow:hidden; height:100%;"
            ),
            tags$div(
              style = paste0(
                "background:#f8f9fa; border-bottom:1px solid #e9ecef; ",
                "padding:10px 16px; display:flex; align-items:center; ",
                "justify-content:space-between;"
              ),
              tags$span(
                style = "font-size:0.82rem; font-weight:700; color:#1a1a2e;
                         text-transform:uppercase; letter-spacing:0.05em;",
                shiny::icon("circle-check"), " Goodness-of-Fit Tests"
              ),
              tags$span(
                style = "font-size:0.72rem; color:#6c757d;",
                tags$span(style="color:#28a745;font-size:0.7rem;","*"),
                " p>=.05 ",
                tags$span(style="color:#fd7e14;font-size:0.7rem;","*"),
                " p>=.01 ",
                tags$span(style="color:#dc3545;font-size:0.7rem;","*"),
                " p<.01"
              )
            ),
            tags$div(
              style = "padding:0;",
              tags$table(
                style = "width:100%; border-collapse:collapse; font-size:0.88rem;",
                tags$thead(
                  tags$tr(
                    style = "background:#fafbfc;",
                    tags$th(style = "padding:8px 16px; text-align:left;
                                     color:#6c757d; font-weight:600;
                                     font-size:0.78rem; text-transform:uppercase;
                                     letter-spacing:0.04em; border-bottom:1px solid #e9ecef;",
                            "Test"),
                    tags$th(style = "padding:8px 16px; text-align:right;
                                     color:#6c757d; font-weight:600;
                                     font-size:0.78rem; text-transform:uppercase;
                                     letter-spacing:0.04em; border-bottom:1px solid #e9ecef;",
                            "Statistic"),
                    tags$th(style = "padding:8px 16px; text-align:right;
                                     color:#6c757d; font-weight:600;
                                     font-size:0.78rem; text-transform:uppercase;
                                     letter-spacing:0.04em; border-bottom:1px solid #e9ecef;",
                            "p-value"),
                    tags$th(style = "padding:8px 10px; text-align:center;
                                     border-bottom:1px solid #e9ecef; width:36px;",
                            "")
                  )
                ),
                tags$tbody(
                  lapply(seq_len(nrow(bf$gof)), function(i) {
                    row  <- bf$gof[i, ]
                    pval <- as.numeric(row[["P_value"]])
                    stat <- as.numeric(row[["Statistic"]])
                    is_even <- i %% 2 == 0
                    gof_na  <- is.na(pval)
                    dot_col <- if (gof_na)                         "#adb5bd"
                               else if (pval >= 0.05)             "#28a745"
                               else if (pval >= 0.01)             "#fd7e14"
                               else                               "#dc3545"
                    verdict <- if (gof_na)                         "N/A"
                               else if (pval >= 0.05)             "Good fit"
                               else if (pval >= 0.01)             "Marginal"
                               else                               "Poor fit"
                    stat_txt <- if (is.na(stat)) "--"
                                else formatC(stat, format = "f", digits = 4)
                    pval_txt <- if (gof_na) "--"
                                else sprintf("%.4f", pval)
                    tags$tr(
                      style = paste0(
                        "border-bottom:1px solid #f0f0f0;",
                        if (is_even) "background:#fafbfc;" else "background:#fff;"
                      ),
                      tags$td(
                        style = "padding:10px 16px; color:#343a40; font-weight:500;",
                        as.character(row[["Test"]])
                      ),
                      tags$td(
                        style = "padding:10px 16px; text-align:right;
                                 font-family:'Courier New',monospace;
                                 color:#495057; font-size:0.85rem;",
                        stat_txt
                      ),
                      tags$td(
                        style = "padding:10px 16px; text-align:right;
                                 font-family:'Courier New',monospace;
                                 font-weight:700; font-size:0.85rem;",
                        tags$span(
                          style = paste0("color:", dot_col, ";"),
                          pval_txt
                        )
                      ),
                      tags$td(
                        style = "padding:10px; text-align:center;",
                        tags$span(
                          style = paste0(
                            "display:inline-block; padding:2px 7px; ",
                            "border-radius:20px; font-size:0.68rem; font-weight:600; ",
                            "background:", dot_col, "20; color:", dot_col, ";"
                          ),
                          verdict
                        )
                      )
                    )
                  })
                )
              )
            )
          )
        } else {
          tags$div(
            style = "background:#f8f9fa; border-radius:12px; padding:20px;",
            tags$p(style = "color:#6c757d; margin:0;",
                   "No goodness-of-fit results available.")
          )
        }
      )
    )
  })

  # -- Interactive plot ------------------------------------------------------
  output$plot_header <- shiny::renderUI({
    fv  <- display_fv()
    shiny::req(fv)
    entry   <- get_dist_entry(fv$best_fit$dist_key)
    sel_row <- input$ranking_table_rows_selected
    is_sel  <- !is.null(sel_row) && length(sel_row) > 0 && sel_row > 1L
    label   <- if (is_sel)
      sprintf(" Selected (Rank %d): %s  |  %s  |  AIC = %.2f  |  n = %d",
              sel_row, entry$display_name, fv$best_fit$method,
              fv$best_fit$aic, fv$n)
    else
      sprintf(" Best fit: %s  |  %s  |  AIC = %.2f  |  n = %d",
              entry$display_name, fv$best_fit$method,
              fv$best_fit$aic, fv$n)
    tags$div(
      class = "fv-status-box success mb-3",
      shiny::icon(if (is_sel) "circle-dot" else "circle-check"),
      label
    )
  })

  output$interactive_plot <- plotly::renderPlotly({
    fv <- fit_result()
    shiny::req(fv)

    # If the user has clicked a non-rank-1 row in the ranking table, inject
    # the selected distribution into the density overlay so it appears
    # highlighted with the solid/thick line.  We do this by:
    #   (a) swapping best_fit → selected fit
    #   (b) moving the selected row to the top of ranking so .plot_density_panel
    #       includes it within the top_n curves (it reads head(ranking, top_n))
    # This local modification is only for the density plot; it does NOT affect
    # the ranking table output, which reads from fit_result() directly.
    sel_row <- input$ranking_table_rows_selected
    if (!is.null(sel_row) && length(sel_row) > 0 &&
        sel_row > 1L && sel_row <= nrow(fv$ranking)) {
      sel_fit <- selected_fit_rv()
      if (!is.null(sel_fit)) {
        fv$best_fit <- sel_fit
        ranking     <- fv$ranking
        other_idx   <- setdiff(seq_len(nrow(ranking)), as.integer(sel_row))
        fv$ranking  <- ranking[c(as.integer(sel_row), other_idx), ]
        rownames(fv$ranking) <- NULL
      }
    }

    # Build density panel as ggplot then convert to plotly.
    # draw = FALSE suppresses the internal print() call so it does not
    # attempt to write to a graphics device inside renderPlotly.
    p <- tryCatch(
      plot(fv, which = "density",
           top_n = input$top_n,
           xlab  = xlab_val(),
           draw  = FALSE),
      error = function(e) {
        shiny::showNotification(paste("Plot error:", conditionMessage(e)),
                                type = "warning", duration = 5)
        NULL
      }
    )

    shiny::req(!is.null(p))

    plotly::ggplotly(p, tooltip = c("colour", "x", "y")) |>
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",   # matches panel.background in plot_fitverse.R
        legend = list(
          orientation = "h",
          y           = -0.18,
          x           = 0.5,
          xanchor     = "center",
          font        = list(size = 11, color = "#1A2B3C"),
          bgcolor     = "rgba(255,255,255,0)",
          bordercolor = "rgba(0,0,0,0)"
        ),
        margin = list(t = 50, b = 90)
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0("fitverse_", gsub(" ", "_", xlab_val())),
          width    = 1200,
          height   = 600,
          scale    = 2
        )
      )
  })

  # -- Q-Q plot --------------------------------------------------------------
  output$qq_plot <- plotly::renderPlotly({
    fv <- display_fv()   # uses selected distribution, not always rank 1
    shiny::req(fv)

    p <- tryCatch(
      plot(fv, which = "qq", xlab = xlab_val(), draw = FALSE),
      error = function(e) NULL
    )
    shiny::req(!is.null(p))

    plotly::ggplotly(p, tooltip = c("x", "y")) |>
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",
        margin        = list(t = 50, b = 60)
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0("fitverse_qq_", gsub(" ", "_", xlab_val())),
          width    = 900,
          height   = 600,
          scale    = 2
        )
      )
  })

  # -- P-P plot ---------------------------------------------------------------
  output$pp_plot <- plotly::renderPlotly({
    fv <- display_fv()
    shiny::req(fv)

    x    <- sort(clean_vec())
    shiny::req(!is.null(x) && length(x) >= 4)

    bf   <- fv$best_fit
    p_fn <- tryCatch(.lookup_dpqr("p", bf$dist_key), error = function(e) NULL)
    shiny::req(!is.null(p_fn))

    n       <- length(x)
    # Hazen plotting positions: (i - 0.5) / n
    emp_cdf <- (seq_len(n) - 0.5) / n
    theo_cdf <- tryCatch(
      do.call(p_fn, c(list(q = x), as.list(bf$params))),
      error = function(e) NULL
    )
    shiny::req(!is.null(theo_cdf))
    theo_cdf <- pmax(0, pmin(1, theo_cdf))

    entry    <- get_dist_entry(bf$dist_key)
    dist_lbl <- entry$display_name

    df_pp <- data.frame(
      Theoretical = theo_cdf,
      Empirical   = emp_cdf,
      x_val       = x
    )

    p <- ggplot2::ggplot(df_pp,
           ggplot2::aes(x = Theoretical, y = Empirical,
                        text = paste0("x = ", round(x_val, 4),
                                      "<br>Theoretical CDF = ", round(Theoretical, 4),
                                      "<br>Empirical CDF = ", round(Empirical, 4)))) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           colour = "#888", linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_point(colour = "#3d7ebf", size = 2.2, alpha = 0.75) +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0.02, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0.02, 0)) +
      ggplot2::labs(
        title = paste0("P-P Plot: ", dist_lbl),
        x     = "Theoretical CDF  F(x)",
        y     = "Empirical CDF"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
        plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = ggplot2::element_text(size = 13, colour = "#1a2b3c", face = "bold"),
        axis.title = ggplot2::element_text(size = 11, colour = "#1a2b3c")
      )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",
        margin        = list(t = 50, b = 60)
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0("fitverse_pp_", gsub(" ", "_", xlab_val())),
          width    = 900,
          height   = 600,
          scale    = 2
        )
      )
  })

  # -- CDF / Survival / Hazard / Cumulative Hazard plot ----------------------
  output$curve_plot <- plotly::renderPlotly({
    fv   <- display_fv()
    shiny::req(fv)

    x    <- clean_vec()
    shiny::req(!is.null(x) && length(x) >= 4)

    bf   <- fv$best_fit
    p_fn <- tryCatch(.lookup_dpqr("p", bf$dist_key), error = function(e) NULL)
    d_fn <- tryCatch(.lookup_dpqr("d", bf$dist_key), error = function(e) NULL)
    shiny::req(!is.null(p_fn))

    entry    <- get_dist_entry(bf$dist_key)
    dist_lbl <- entry$display_name
    ctype    <- input$curve_type %||% "Survival"

    # Build evaluation grid over the data range (with 5% padding)
    r    <- range(x, na.rm = TRUE)
    pad  <- diff(r) * 0.05
    xs   <- seq(r[1] - pad, r[2] + pad, length.out = 512)

    Fx <- tryCatch(
      pmax(0, pmin(1, do.call(p_fn, c(list(q = xs), as.list(bf$params))))),
      error = function(e) NULL
    )
    shiny::req(!is.null(Fx))

    if (ctype == "CDF") {
      ys     <- Fx
      y_lbl  <- "F(x) = P(X ≤ x)"
      colour <- "#2d6bc4"
    } else if (ctype == "Survival") {
      ys     <- 1 - Fx
      y_lbl  <- "S(x) = 1 − F(x)"
      colour <- "#27ae60"
    } else if (ctype == "Hazard") {
      # h(x) = f(x) / S(x); need density
      shiny::req(!is.null(d_fn))
      fx <- tryCatch(
        pmax(0, do.call(d_fn, c(list(x = xs), as.list(bf$params)))),
        error = function(e) NULL
      )
      shiny::req(!is.null(fx))
      Sx <- pmax(1 - Fx, 1e-10)
      ys     <- fx / Sx
      y_lbl  <- "h(x) = f(x) / S(x)"
      colour <- "#c0392b"
    } else {   # Cumulative Hazard
      Sx <- pmax(1 - Fx, 1e-10)
      ys     <- -log(Sx)
      y_lbl  <- "H(x) = −log S(x)"
      colour <- "#8e44ad"
    }

    # Empirical points (Hazen plotting positions) for CDF, Survival, CumHaz.
    # Hazard has no natural empirical analogue so we skip it there.
    n_x     <- length(x)
    x_ord   <- sort(x)
    p_hazen <- (seq_len(n_x) - 0.5) / n_x
    emp_y <- switch(ctype,
      "CDF"               = p_hazen,
      "Survival"          = 1 - p_hazen,
      "Cumulative Hazard" = -log(pmax(1 - p_hazen, 1e-10)),
      NULL
    )
    emp_lbl <- switch(ctype,
      "CDF"               = "Empirical F(x)",
      "Survival"          = "Empirical S(x)",
      "Cumulative Hazard" = "Empirical H(x)",
      NULL
    )

    # Use plot_ly directly (not ggplotly) to avoid the ggplot2 `text` aesthetic
    # grouping bug: aes(text = lbl) in a global ggplot mapping causes ggplotly
    # to create one trace per unique label (512 single-point traces), making
    # the curve appear as disconnected dots instead of a smooth line.
    lbl <- paste0(
      "<b>", xlab_val(), "</b>: ", round(xs, 4),
      "<br><b>", ctype, "(x)</b>: ", round(ys, 5)
    )

    plt <- plotly::plot_ly()

    # Layer 1: empirical points (CDF / Survival / CumHaz only)
    if (!is.null(emp_y)) {
      emp_hover <- paste0(
        "<b>", xlab_val(), "</b>: ", round(x_ord, 4),
        "<br><b>", emp_lbl, "</b>: ", round(emp_y, 5)
      )
      plt <- plt |>
        plotly::add_trace(
          x         = x_ord,
          y         = emp_y,
          text      = emp_hover,
          type      = "scatter",
          mode      = "markers",
          marker    = list(color = "#888888", size = 4, opacity = 0.5),
          name      = emp_lbl,
          hoverinfo = "text"
        )
    }

    # Layer 2: theoretical curve
    plt <- plt |>
      plotly::add_trace(
        x         = xs,
        y         = ys,
        text      = lbl,
        type      = "scatter",
        mode      = "lines",
        line      = list(color = colour, width = 2.5),
        name      = dist_lbl,
        hoverinfo = "text"
      )

    plt |>
      plotly::layout(
        title = list(
          text = paste0(ctype, " — ", dist_lbl),
          font = list(size = 13, color = "#1a2b3c", family = "Arial, sans-serif"),
          x    = 0.05
        ),
        xaxis = list(
          title     = list(text = xlab_val(),
                           font = list(size = 11, color = "#1a2b3c")),
          gridcolor = "#c5cfe0",
          zeroline  = FALSE
        ),
        yaxis = list(
          title     = list(text = y_lbl,
                           font = list(size = 11, color = "#1a2b3c")),
          gridcolor = "#c5cfe0",
          zeroline  = FALSE
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",
        margin        = list(t = 50, b = 60)
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0("fitverse_", tolower(gsub(" ", "_", ctype)), "_",
                            gsub(" ", "_", xlab_val())),
          width    = 1200,
          height   = 600,
          scale    = 2
        )
      )
  })

  # -- Rankings table --------------------------------------------------------
  output$ranking_header <- shiny::renderUI({
    fv <- fit_result()
    shiny::req(fv)
    n_dists <- nrow(fv$ranking)

    tags$div(
      class = "mb-2",
      tags$p(
        class = "text-muted small mb-1",
        sprintf("%d distributions fitted and ranked.", n_dists)
      ),
      # Row colour legend
      tags$div(
        style = "display:flex; gap:12px; flex-wrap:wrap; font-size:0.78rem; margin-bottom:6px;",
        tags$span(
          tags$span(style="display:inline-block;width:12px;height:12px;background:#a8ccf0;border:1px solid #6090c0;border-radius:2px;margin-right:4px;vertical-align:middle;"),
          tags$strong("Rank 1"), " — best fit"
        ),
        tags$span(
          tags$span(style="display:inline-block;width:12px;height:12px;background:#e6f7ee;border:1px solid #40a060;border-radius:2px;margin-right:4px;vertical-align:middle;"),
          "GoF adequate"
        ),
        tags$span(
          tags$span(style="display:inline-block;width:12px;height:12px;background:#ffd060;border:1px solid #c09020;border-radius:2px;margin-right:4px;vertical-align:middle;"),
          "GoF partial (one test)"
        ),
        tags$span(
          tags$span(style="display:inline-block;width:12px;height:12px;background:#f5f5f5;border:1px solid #aaa;border-radius:2px;margin-right:4px;vertical-align:middle;"),
          "GoF inadequate / no test"
        )
      ),
      # Click hint
      tags$div(
        style = paste0(
          "background:#f0f4ff; border-left:3px solid #4f46e5; border-radius:0 6px 6px 0; ",
          "padding:6px 12px; font-size:0.79rem; color:#3730a3;"
        ),
        shiny::icon("hand-pointer", class = "me-1"),
        tags$strong("Click any row"), " to explore that distribution in the Overview and Plot tabs."
      )
    )
  })

  output$aic_note <- shiny::renderUI({
    fv <- fit_result()
    shiny::req(fv)
    crit <- fv$criterion
    tags$details(
      class = "mb-3",
      style = "font-size:0.81rem; color:#2c3e6b;",
      tags$summary(
        style = paste0(
          "cursor:pointer; list-style:none; display:flex; align-items:center; ",
          "gap:6px; color:#2c5f9e; font-size:0.81rem; user-select:none;"
        ),
        tags$span(style="font-size:0.9rem;", "▶"),
        shiny::icon("circle-info", class = "me-1"),
        tags$span(
          class = "fw-semibold",
          "Ranking: GoF tier → ", crit, " → GoF composite score"
        ),
        tags$span(style="color:#888; font-weight:normal;", "(click to expand)")
      ),
      tags$div(
        style = paste0(
          "margin-top:8px; background:#eef3fb; border-left:4px solid #2c5f9e; ",
          "border-radius:0 6px 6px 0; padding:10px 14px; line-height:1.8;"
        ),
        tags$ul(
          class = "mb-0 ps-3",
          tags$li(
            tags$strong("GoF tier"),
            " — ", tags$em("adequate"), " (both KS & AD available, ≥1 passes) →",
            " ", tags$em("partial"), " (only one test, passes) →",
            " ", tags$em("inadequate / no test"), " (all available tests fail, or no tests",
            " could be computed). Distributions showing ", tags$strong("--"),
            " for both KS and AD have no GoF information; they fall into the",
            " inadequate tier and are ranked by AIC only — they cannot displace",
            " distributions that actually pass a test.",
            " Exception: if the best adequate distribution is >10 AIC units worse than",
            " the best overall, tier advantages are suppressed and pure AIC ranking applies."
          ),
          tags$li(
            tags$strong(crit), " — primary within-tier sort. Penalises extra parameters",
            " so flexible distributions (e.g. Wakeby, Kappa-4) do not displace more",
            " parsimonious models. Delta", crit, " is relative to Rank 1."
          ),
          tags$li(
            tags$strong("GoF composite (KS rank + AD rank)"), " — tiebreaker only",
            " when two fits have identical ", crit, ". Lower = better p-value profile.",
            " A ", tags$strong("--"), " receives a neutral mid-rank (neither rewarded nor penalised)."
          ),
          tags$li(
            tags$strong("Large samples"), " — KS, AD, and χ² tests become extremely powerful",
            " as sample size grows. With several hundred or more observations, even the true",
            " underlying distribution can be rejected, because the tests detect tiny deviations",
            " that have no practical significance. When all GoF p-values are near zero,",
            " ", tags$strong("AIC/BIC is the recommended selection criterion."),
            " GoF p-values still carry relative information even in this situation:",
            " a distribution with p = 0.04 fits better than one with p = 0.0001,",
            " even though both are formally rejected. Use the p-values for",
            tags$em(" comparison"), ", not as a binary pass/fail gate."
          ),
          tags$li(
            tags$strong("Multiple methods (MLE + MOM + LMOM)"), " — only the best method",
            " per distribution is shown. Re-run with a specific method to compare directly."
          )
        )
      )
    )
  })

  output$ranking_table <- DT::renderDT({
    fv <- fit_result()
    shiny::req(fv)

    tbl      <- fv$ranking
    gof_grp  <- tbl$GoF_group   # save before subsetting columns

    show_cols <- c("Rank","Distribution","Method","AIC","Delta_AIC",
                   "BIC","KS_p","AD_p","ChiSq_p","Converged")
    show_cols <- show_cols[show_cols %in% names(tbl)]
    tbl <- tbl[, show_cols]

    # Round numeric columns
    num_cols <- c("AIC","Delta_AIC","BIC","KS_p","AD_p","ChiSq_p")
    for (col in intersect(num_cols, names(tbl))) {
      tbl[[col]] <- round(tbl[[col]], 4)
    }

    # Replace NA GoF p-values with "--" so users see intentional unavailability
    for (col in c("KS_p", "AD_p", "ChiSq_p")) {
      if (col %in% names(tbl)) {
        tbl[[col]] <- ifelse(is.na(tbl[[col]]), "--", as.character(tbl[[col]]))
      }
    }

    # Row colour based on GoF tier (method-agnostic, matches new ranking logic):
    #   best      = Rank 1 (blue)
    #   adequate  = GoF tier 0, rank > 1 (light green: KS & AD adequate)
    #   partial   = GoF tier 1 (amber: only one test available)
    #   poor      = GoF tier 2 (light grey: KS fails)
    tbl$.colour_group <- ifelse(tbl$Rank == 1L,    "best",
                         ifelse(gof_grp   == 0L,   "adequate",
                         ifelse(gof_grp   == 1L,   "partial",
                                                    "poor")))

    # Hidden colour-group column is always the last column
    hidden_col_idx <- ncol(tbl) - 1L

    dt <- DT::datatable(
      tbl,
      rownames  = FALSE,
      class     = "compact hover",
      selection = "single",
      options   = list(
        pageLength = 15,
        dom        = "tip",
        columnDefs = list(
          list(className = "dt-center", targets = c(0, 2, 8)),
          list(visible = FALSE, targets = hidden_col_idx)
        )
      )
    ) |>
      DT::formatStyle(
        ".colour_group",
        target          = "row",
        backgroundColor = DT::styleEqual(
          c("best",    "adequate", "partial", "poor"),
          c("#a8ccf0", "#e6f7ee",  "#ffd060", "#f5f5f5")
        ),
        fontWeight = DT::styleEqual(
          c("best",  "adequate", "partial", "poor"),
          c("bold",  "normal",   "normal",  "normal")
        )
      ) |>
      DT::formatStyle(
        "KS_p",
        backgroundColor = DT::styleInterval(
          c(0.05),
          c("#fde8e8", "#e6f7ee")   # red < 0.05, green >= 0.05
        ),
        color = DT::styleInterval(
          c(0.05),
          c("#a93226", "#1e8449")
        ),
        fontWeight = "600"
      ) |>
      DT::formatStyle(
        "AD_p",
        backgroundColor = DT::styleInterval(
          c(0.05),
          c("#fde8e8", "#e6f7ee")
        ),
        color = DT::styleInterval(
          c(0.05),
          c("#a93226", "#1e8449")
        ),
        fontWeight = "600"
      ) |>
      DT::formatStyle(
        "ChiSq_p",
        backgroundColor = DT::styleInterval(
          c(0.05),
          c("#fde8e8", "#e6f7ee")
        ),
        color = DT::styleInterval(
          c(0.05),
          c("#a93226", "#1e8449")
        ),
        fontWeight = "600"
      )

    dt
  })

  # -- Calculator: tail probability / quantile --------------------------------
  calc_result_rv <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$calc_run, {
    fv <- display_fv()
    if (is.null(fv)) {
      shiny::showNotification("Please fit distributions first.", type = "warning")
      return()
    }

    bf    <- fv$best_fit
    entry <- get_dist_entry(bf$dist_key)

    p_fn  <- tryCatch(.lookup_dpqr("p", bf$dist_key), error = function(e) NULL)
    q_fn  <- tryCatch(.lookup_dpqr("q", bf$dist_key), error = function(e) NULL)

    x_val <- input$calc_x_val
    p_val <- input$calc_p_val

    results <- list()

    # x → probabilities
    if (!is.null(x_val) && is.finite(x_val) && !is.null(p_fn)) {
      cdf_val <- tryCatch(
        do.call(p_fn, c(list(q = x_val), as.list(bf$params))),
        error = function(e) NA_real_
      )
      results$x_to_p <- list(x = x_val, cdf = cdf_val, surv = 1 - cdf_val)
    }

    # p → quantile
    if (!is.null(p_val) && is.finite(p_val) && p_val > 0 && p_val < 1 && !is.null(q_fn)) {
      quant <- tryCatch(
        do.call(q_fn, c(list(p = p_val), as.list(bf$params))),
        error = function(e) NA_real_
      )
      results$p_to_x <- list(p = p_val, quantile = quant)
    }

    calc_result_rv(list(entry = entry, bf = bf, results = results))
  })

  output$calc_result_ui <- shiny::renderUI({
    res <- calc_result_rv()
    if (is.null(res)) {
      return(tags$p(class = "text-muted small",
                    "Enter a value or probability above and click Calculate."))
    }
    entry <- res$entry
    bf    <- res$bf
    r     <- res$results

    param_str <- paste0(
      names(bf$params), " = ",
      round(unlist(bf$params), 5),
      collapse = ", "
    )

    items <- list()
    items[[1]] <- tags$div(
      style = "font-size:0.83rem; color:#555; margin-bottom:10px;",
      tags$strong("Distribution: "), entry$display_name,
      tags$br(),
      tags$strong("Parameters: "), param_str
    )

    if (!is.null(r$x_to_p)) {
      v <- r$x_to_p
      items[[2]] <- tags$div(
        style = paste0(
          "background:#f0f7ff; border-left:4px solid #2d6bc4; ",
          "border-radius:0 8px 8px 0; padding:12px 16px; margin-bottom:10px;"
        ),
        tags$div(style = "font-weight:700; color:#1e3a5f; margin-bottom:6px;",
                 shiny::icon("arrow-right", class = "me-1"),
                 sprintf("P(X ≤ %.4g) =", v$x)),
        tags$div(
          style = "font-size:1.15rem; font-family:monospace; color:#1a1a2e; font-weight:800;",
          sprintf("%.6f", v$cdf)
        ),
        tags$div(style = "margin-top:6px; font-size:0.88rem; color:#555;",
                 sprintf("P(X > %.4g) = %.6f  (survival probability)", v$x, v$surv))
      )
    }

    if (!is.null(r$p_to_x)) {
      v <- r$p_to_x
      items[[3]] <- tags$div(
        style = paste0(
          "background:#f0fff5; border-left:4px solid #27ae60; ",
          "border-radius:0 8px 8px 0; padding:12px 16px; margin-bottom:10px;"
        ),
        tags$div(style = "font-weight:700; color:#155724; margin-bottom:6px;",
                 shiny::icon("arrow-right", class = "me-1"),
                 sprintf("Q(%.4g) =  %.6g-th quantile", v$p, v$p)),
        tags$div(
          style = "font-size:1.15rem; font-family:monospace; color:#1a1a2e; font-weight:800;",
          sprintf("%.6g", v$quantile)
        )
      )
    }

    if (length(items) <= 1) {
      items[[2]] <- tags$p(class = "text-muted small",
                           "Enter a valid value or probability (not both are required).")
    }

    tagList(items)
  })


  # -- Simulate from fitted distribution -------------------------------------
  postfit_sim_rv <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$postfit_sim_run, {
    fv <- display_fv()
    if (is.null(fv)) {
      shiny::showNotification("Please fit distributions first.", type = "warning")
      return()
    }

    bf    <- fv$best_fit
    entry <- get_dist_entry(bf$dist_key)
    r_fn  <- tryCatch(.lookup_dpqr("r", bf$dist_key), error = function(e) NULL)

    if (is.null(r_fn)) {
      shiny::showNotification(
        paste0("Random generation not available for ", entry$display_name, "."),
        type = "warning"
      )
      return()
    }

    n   <- as.integer(input$postfit_sim_n)
    if (is.na(n) || n < 1L) {
      shiny::showNotification("Enter a valid sample size.", type = "warning")
      return()
    }

    if (isTRUE(input$postfit_sim_use_seed)) {
      set.seed(as.integer(input$postfit_sim_seed))
    }

    x_sim <- tryCatch(
      do.call(r_fn, c(list(n = n), as.list(bf$params))),
      error = function(e) {
        shiny::showNotification(paste("Simulation error:", conditionMessage(e)),
                                type = "error")
        NULL
      }
    )
    if (is.null(x_sim)) return()

    postfit_sim_rv(list(
      x     = x_sim,
      n     = n,
      entry = entry,
      bf    = bf
    ))
  })

  output$postfit_sim_result_ui <- shiny::renderUI({
    sim <- postfit_sim_rv()
    if (is.null(sim)) {
      return(tags$p(class = "text-muted small",
                    "Click Generate to produce random samples from the fitted distribution."))
    }
    tagList(
      tags$div(
        style = "font-size:0.83rem; color:#555; margin-bottom:8px;",
        tags$strong("Distribution: "), sim$entry$display_name,
        " | ",
        tags$strong("n = "), sim$n,
        " | ",
        tags$strong("Mean: "), round(mean(sim$x, na.rm = TRUE), 4),
        " | ",
        tags$strong("SD: "), round(stats::sd(sim$x, na.rm = TRUE), 4)
      ),
      shiny::downloadButton(
        "postfit_sim_download", "Download CSV",
        class = "btn btn-outline-success btn-sm",
        icon  = shiny::icon("download")
      )
    )
  })

  output$postfit_sim_download <- shiny::downloadHandler(
    filename = function() {
      sim <- postfit_sim_rv()
      nm  <- if (!is.null(sim))
        gsub("[^A-Za-z0-9]", "_", sim$entry$display_name)
      else "simulated"
      paste0("FitVerse_sim_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      sim <- shiny::req(postfit_sim_rv())
      utils::write.csv(data.frame(x = sim$x), file, row.names = FALSE)
    }
  )


  # -- Manual Fit: dynamic parameter inputs ------------------------------------
  output$manual_fit_param_inputs <- shiny::renderUI({
    key   <- input$manual_fit_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))

    params <- entry$params
    lower  <- entry$lower
    upper  <- entry$upper

    # Sensible starting values: use the registry start_fun on the current data,
    # or fall back to midpoints of the bounds.
    x <- clean_vec()
    starts <- if (!is.null(x) && length(x) >= 4) {
      tryCatch(entry$start_fun(x), error = function(e) NULL)
    } else NULL

    inputs <- lapply(seq_along(params), function(i) {
      pname <- params[i]
      lo    <- if (!is.null(lower) && pname %in% names(lower)) lower[[pname]] else -Inf
      hi    <- if (!is.null(upper) && pname %in% names(upper)) upper[[pname]] else  Inf

      default_val <- if (!is.null(starts) && pname %in% names(starts))
        round(starts[[pname]], 5)
      else if (is.finite(lo) && is.finite(hi)) (lo + hi) / 2
      else if (is.finite(lo)) lo + 1
      else 1

      shiny::numericInput(
        paste0("mfp_", pname),
        label = pname,
        value = default_val,
        step  = NA
      )
    })

    do.call(bslib::layout_columns,
            c(list(col_widths = rep(ceiling(12 / length(params)), length(params))),
              inputs))
  })


  # -- Manual Fit: evaluate on button click -----------------------------------
  manual_fit_rv <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$manual_fit_run, {
    key   <- input$manual_fit_dist
    x     <- clean_vec()

    if (is.null(x) || length(x) < 4) {
      shiny::showNotification("Please load data first (Fit Distributions tab).",
                              type = "warning")
      return()
    }

    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    if (is.null(entry)) return()

    params <- entry$params

    # Collect user-entered parameter values
    param_vals <- setNames(
      vapply(params, function(pname) {
        val <- input[[paste0("mfp_", pname)]]
        if (is.null(val) || !is.finite(val)) NA_real_ else val
      }, numeric(1L)),
      params
    )

    if (anyNA(param_vals)) {
      shiny::showNotification("All parameter values must be finite numbers.",
                              type = "warning")
      return()
    }

    # Compute log-likelihood
    d_fn <- tryCatch(.lookup_dpqr("d", key), error = function(e) NULL)
    p_fn <- tryCatch(.lookup_dpqr("p", key), error = function(e) NULL)

    if (is.null(d_fn)) {
      shiny::showNotification(
        paste0("Density function not found for ", entry$display_name, "."),
        type = "warning"
      )
      return()
    }

    loglik <- tryCatch({
      ll_vec <- do.call(d_fn, c(list(x = x, log = TRUE), as.list(param_vals)))
      if (any(!is.finite(ll_vec))) NA_real_ else sum(ll_vec)
    }, error = function(e) NA_real_)

    n_params <- length(param_vals)
    aic_val  <- if (is.finite(loglik)) 2 * n_params - 2 * loglik else NA_real_
    bic_val  <- if (is.finite(loglik)) n_params * log(length(x)) - 2 * loglik else NA_real_

    # GoF tests
    gof_res <- if (!is.null(p_fn)) {
      pcdf <- function(q) do.call(p_fn, c(list(q = q), as.list(param_vals)))
      tryCatch(compute_gof(x, key, param_vals), error = function(e) NULL)
    } else NULL

    manual_fit_rv(list(
      dist_key   = key,
      entry      = entry,
      params     = param_vals,
      loglik     = loglik,
      aic        = aic_val,
      bic        = bic_val,
      gof        = gof_res,
      n          = length(x),
      n_params   = n_params
    ))
  })


  output$manual_fit_result_ui <- shiny::renderUI({
    mf <- manual_fit_rv()
    if (is.null(mf)) {
      return(tags$p(class = "text-muted small",
                    "Click 'Evaluate Manual Fit' to see results."))
    }

    entry <- mf$entry

    param_str <- paste0(
      names(mf$params), " = ",
      round(unlist(mf$params), 5),
      collapse = ", "
    )

    ll_str  <- if (is.finite(mf$loglik)) sprintf("%.4f", mf$loglik) else "N/A"
    aic_str <- if (is.finite(mf$aic))    sprintf("%.4f", mf$aic)    else "N/A"
    bic_str <- if (is.finite(mf$bic))    sprintf("%.4f", mf$bic)    else "N/A"

    gof_rows <- if (!is.null(mf$gof) && nrow(mf$gof) > 0) {
      lapply(seq_len(nrow(mf$gof)), function(i) {
        row  <- mf$gof[i, ]
        pval <- as.numeric(row[["P_value"]])
        dot_col <- if (is.na(pval)) "#adb5bd"
                   else if (pval >= 0.05) "#28a745"
                   else if (pval >= 0.01) "#fd7e14"
                   else "#dc3545"
        verdict <- if (is.na(pval)) "N/A"
                   else if (pval >= 0.05) "Good fit"
                   else if (pval >= 0.01) "Marginal"
                   else "Poor fit"
        tags$tr(
          tags$td(style="padding:6px 12px;", as.character(row[["Test"]])),
          tags$td(style="padding:6px 12px; font-family:monospace; text-align:right;",
                  if (is.na(row[["Statistic"]])) "--" else sprintf("%.4f", as.numeric(row[["Statistic"]]))),
          tags$td(style=paste0("padding:6px 12px; font-family:monospace; text-align:right; color:", dot_col, "; font-weight:700;"),
                  if (is.na(pval)) "--" else sprintf("%.4f", pval)),
          tags$td(style=paste0("padding:6px 10px; text-align:center;"),
                  tags$span(
                    style=paste0("display:inline-block; padding:2px 7px; border-radius:20px; font-size:0.68rem; font-weight:600; background:", dot_col, "20; color:", dot_col, ";"),
                    verdict
                  ))
        )
      })
    } else list(tags$tr(tags$td(colspan = "4", "GoF tests unavailable.")))

    tagList(
      tags$div(
        style = paste0(
          "background:#f8f9ff; border:1px solid #c8d6f0; border-radius:10px; ",
          "padding:14px 18px; margin-bottom:12px;"
        ),
        tags$div(
          style = "font-size:0.88rem; margin-bottom:8px; color:#1a2b3c;",
          tags$strong("Distribution: "), entry$display_name, tags$br(),
          tags$strong("Parameters: "), param_str
        ),
        tags$div(
          style = "display:flex; gap:24px; flex-wrap:wrap;",
          tags$div(
            tags$span(style="font-size:0.75rem; color:#888; text-transform:uppercase; letter-spacing:0.05em;", "Log-Lik"),
            tags$div(style="font-size:1.05rem; font-family:monospace; font-weight:700; color:#1a1a2e;", ll_str)
          ),
          tags$div(
            tags$span(style="font-size:0.75rem; color:#888; text-transform:uppercase; letter-spacing:0.05em;", "AIC"),
            tags$div(style="font-size:1.05rem; font-family:monospace; font-weight:700; color:#1a1a2e;", aic_str)
          ),
          tags$div(
            tags$span(style="font-size:0.75rem; color:#888; text-transform:uppercase; letter-spacing:0.05em;", "BIC"),
            tags$div(style="font-size:1.05rem; font-family:monospace; font-weight:700; color:#1a1a2e;", bic_str)
          )
        )
      ),
      tags$table(
        style = "width:100%; border-collapse:collapse; font-size:0.87rem; margin-bottom:8px;",
        tags$thead(
          tags$tr(
            style = "background:#f0f4fb; border-bottom:1px solid #cdd8f0;",
            tags$th(style="padding:6px 12px; text-align:left;", "Test"),
            tags$th(style="padding:6px 12px; text-align:right;", "Statistic"),
            tags$th(style="padding:6px 12px; text-align:right;", "p-value"),
            tags$th(style="padding:6px 10px;", "")
          )
        ),
        tags$tbody(gof_rows)
      ),
      tags$p(
        class = "text-muted",
        style = "font-size:0.78rem; margin-top:6px;",
        "AIC and BIC assume the parameters were estimated by MLE. ",
        "If parameters were specified from external knowledge, AIC/BIC are indicative only. ",
        "df = n_params for AIC; Pearson chi-squared df adjusted accordingly."
      )
    )
  })


  # -- Data summary panel ----------------------------------------------------
  output$data_summary_panel <- shiny::renderUI({
    fv <- fit_result()
    shiny::req(fv)
    ds <- fv$data_summary

    tagList(
      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_header("Location & Spread"),
          bslib::card_body(
            tags$div(
              class = "fv-stat-grid",
              stat_card_html("n",         format(ds$n, big.mark = ",")),
              stat_card_html("Missing",   ds$n_missing),
              stat_card_html("Mean",      round(ds$mean, 4)),
              stat_card_html("Std Dev",   round(ds$sd,   4)),
              stat_card_html("CV",        if(is.na(ds$cv)) "--" else round(ds$cv, 4)),
              stat_card_html("Median",    round(ds$median, 4)),
              stat_card_html("IQR",       round(ds$iqr,    4)),
              stat_card_html("Min",       round(ds$min,    4)),
              stat_card_html("Max",       round(ds$max,    4)),
              stat_card_html("Range",     round(ds$range,  4))
            )
          )
        ),

        bslib::card(
          bslib::card_header("Shape & Quartiles"),
          bslib::card_body(
            tags$div(
              class = "fv-stat-grid",
              stat_card_html("Skewness",        round(ds$skewness, 4)),
              stat_card_html("Excess Kurtosis", round(ds$kurtosis, 4)),
              stat_card_html("Q1 (25%)",        round(ds$q25, 4)),
              stat_card_html("Q3 (75%)",        round(ds$q75, 4))
            ),
            tags$hr(),
            # Mini histogram
            shiny::plotOutput("mini_hist", height = "200px")
          )
        )
      )
    )
  })

  output$mini_hist <- shiny::renderPlot({
    x <- clean_vec()
    shiny::req(x)
    fv <- fit_result()
    shiny::req(fv)

    df <- data.frame(value = x)
    p  <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins   = max(10, ceiling(log2(length(x)) + 1)),
        fill   = "#3d7ebf", colour = "#1e3a5f", alpha = 0.7
      ) +
      ggplot2::geom_density(colour = "#e74c3c", linewidth = 0.8) +
      ggplot2::labs(x = xlab_val(), y = "Density",
                    title = "Data distribution") +
      ggplot2::theme_classic(base_size = 10) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 9,
                                                         colour = "#1e3a5f"))
    print(p)
  }, bg = "transparent")

  # -- Outlier boxplot -------------------------------------------------------
  output$outlier_boxplot <- plotly::renderPlotly({
    x <- clean_vec()
    shiny::req(x)

    # Modified Z-score (Iglewicz & Hoaglin 1993)
    med     <- median(x, na.rm = TRUE)
    MAD_val <- median(abs(x - med), na.rm = TRUE)
    if (MAD_val < 1e-10) MAD_val <- mean(abs(x - med), na.rm = TRUE) * 1.253314

    Mz         <- if (MAD_val > 1e-10) 0.6745 * (x - med) / MAD_val else rep(0, length(x))
    is_outlier <- abs(Mz) > 3.5
    typical    <- x[!is_outlier]
    outliers   <- x[is_outlier]
    Mz_out     <- round(Mz[is_outlier], 3)
    xlab       <- xlab_val()

    set.seed(42)  # reproducible jitter

    # Native plotly: box at y=0 (numeric group), scatter overlaid at y ~ 0
    # Using numeric y for both means they share the same axis and overlay correctly
    fig <- plotly::plot_ly() |>

      # Box + whiskers + median line -- y=rep(0,...) places box at y=0 on numeric axis
      plotly::add_trace(
        x           = x,
        y           = rep(0, length(x)),
        type        = "box",
        orientation = "h",
        boxpoints   = FALSE,
        fillcolor   = "#AED6F1",
        line        = list(color = "#0D3B66", width = 2),
        name        = "Box",
        showlegend  = FALSE,
        hoverinfo   = "none"
      ) |>

      # Typical points -- jittered around y=0
      plotly::add_trace(
        x             = typical,
        y             = runif(length(typical), -0.30, 0.30),
        type          = "scatter",
        mode          = "markers",
        name          = "Typical",
        marker        = list(color = "#0D3B66", opacity = 0.28, size = 6,
                             symbol = "circle"),
        hovertemplate = paste0(xlab, ": %{x:.4f}<extra></extra>"),
        showlegend    = TRUE
      ) |>

      # Potential outliers -- red dots, tighter jitter so they stay visible
      plotly::add_trace(
        x             = outliers,
        y             = runif(length(outliers), -0.12, 0.12),
        type          = "scatter",
        mode          = "markers",
        name          = "Potential outlier",
        marker        = list(color = "#E63946", opacity = 0.92, size = 6,
                             symbol = "circle",
                             line = list(color = "#9B1C1C", width = 1.0)),
        text          = paste0(xlab, ": ", outliers,
                               "<br>Modified Z-score: ", Mz_out),
        hovertemplate = "%{text}<extra>Potential outlier</extra>",
        showlegend    = TRUE
      ) |>

      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",
        xaxis = list(title = xlab, gridcolor = "#D5DCE8", zeroline = FALSE),
        yaxis = list(visible = FALSE, range = c(-0.55, 0.55), zeroline = FALSE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center",
                      y = 1.12, font = list(size = 11, color = "#1A2B3C")),
        margin = list(t = 30, b = 50, l = 10, r = 10)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # -- Outlier summary (alert + table) --------------------------------------
  output$outlier_footer <- shiny::renderUI({
    x <- clean_vec()
    shiny::req(x)

    med     <- median(x, na.rm = TRUE)
    MAD_val <- median(abs(x - med), na.rm = TRUE)
    if (MAD_val < 1e-10) MAD_val <- mean(abs(x - med), na.rm = TRUE) * 1.253314

    if (MAD_val < 1e-10) {
      return(tags$div(
        class = "alert alert-warning mt-3",
        style = "font-size:0.84rem; border-radius:6px;",
        shiny::icon("circle-exclamation"),
        " Modified Z-score undefined; all values are identical."
      ))
    }

    Mz      <- 0.6745 * (x - med) / MAD_val
    out_idx <- which(abs(Mz) > 3.5)
    n_out   <- length(out_idx)

    if (n_out == 0) {
      return(tags$div(
        class = "alert alert-success",
        style = "font-size:0.84rem; padding:8px 14px; margin-top:12px; border-radius:6px;",
        shiny::icon("circle-check"),
        " No potential outliers detected (Modified Z-score threshold = 3.5).",
        tags$br(),
        tags$small(
          class = "text-muted",
          "Reference: Iglewicz, B. & Hoaglin, D. C. (1993). ",
          tags$em("How to Detect and Handle Outliers."),
          " ASQC Quality Press."
        )
      ))
    }

    out_df  <- data.frame(
      Index           = out_idx,
      Value           = round(x[out_idx], 4),
      Modified_Zscore = round(Mz[out_idx], 3),
      Direction       = ifelse(Mz[out_idx] > 0, "Up High", "Down Low"),
      stringsAsFactors = FALSE
    )
    out_df <- out_df[order(abs(out_df$Modified_Zscore), decreasing = TRUE), ]

    # Build HTML table rows
    row_list <- lapply(seq_len(nrow(out_df)), function(i) {
      r     <- out_df[i, ]
      col   <- if (r$Direction == "Up High") "#C0392B" else "#E67E22"
      tags$tr(
        tags$td(r$Index),
        tags$td(r$Value),
        tags$td(style = paste0("color:", col, "; font-weight:600;"), r$Modified_Zscore),
        tags$td(r$Direction)
      )
    })

    tagList(
      tags$div(
        class = "alert alert-warning",
        style = "font-size:0.84rem; padding:8px 14px; margin-top:12px; border-radius:6px;",
        shiny::icon("triangle-exclamation"),
        sprintf(" %d potential outlier%s detected (|Modified Z-score| > 3.5). Review before interpreting fit results.",
                n_out, if (n_out > 1) "s" else "")
      ),
      tags$div(
        style = "overflow-x:auto; margin-top:8px;",
        tags$table(
          class = "table table-sm table-striped",
          style = "font-size:0.84rem; width:auto; max-width:460px;",
          tags$thead(
            tags$tr(
              tags$th("Index"), tags$th("Value"),
              tags$th("Modified Z-score"), tags$th("Direction")
            )
          ),
          tags$tbody(row_list)
        )
      ),
      tags$p(
        class = "text-muted",
        style = "font-size:0.75rem; margin-top:6px;",
        "Reference: Iglewicz, B. & Hoaglin, D. C. (1993). ",
        tags$em("How to Detect and Handle Outliers."),
        " ASQC Quality Press, Milwaukee."
      )
    )
  })

  # -- L-Moment Ratio Diagram ------------------------------------------------
  output$lmrd_plot <- plotly::renderPlotly({
    x <- clean_vec()
    shiny::req(x, length(x) >= 4)

    fv   <- fit_result()
    xlab <- xlab_val()

    # Build ggplot via lmrd() -- use fitverse object if available, else raw vec
    p <- tryCatch(
      if (!is.null(fv)) lmrd(fv, xlab = xlab, draw = FALSE)
      else               lmrd(x,  xlab = xlab, draw = FALSE),
      error = function(e) NULL
    )
    shiny::req(!is.null(p))

    # Rename short dist codes to full names in the plotly legend
    .lmrd_full <- c(
      glo = "GLO: Generalised Logistic",
      gev = "GEV: Generalised Extreme Value",
      gno = "GNO: Generalised Normal",
      pe3 = "PE3: Pearson Type III",
      gpa = "GPA: Generalised Pareto"
    )
    pl <- plotly::ggplotly(p, tooltip = c("x", "y"))
    for (i in seq_along(pl$x$data)) {
      nm <- pl$x$data[[i]]$name
      if (!is.null(nm) && nm %in% names(.lmrd_full))
        pl$x$data[[i]]$name <- .lmrd_full[[nm]]
    }
    pl |>
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center", y = -0.22,
          font = list(size = 11, color = "#1A2B3C"),
          bgcolor      = "rgba(255,255,255,0)",
          bordercolor  = "rgba(0,0,0,0)"
        ),
        margin = list(t = 55, b = 115, l = 65, r = 20),
        xaxis  = list(title = list(text = "L-skewness (tau3)", standoff = 20)),
        yaxis  = list(title = "L-kurtosis (tau4)")
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0("fitverse_lmrd_", gsub(" ", "_", xlab)),
          width    = 1000, height = 650, scale = 2
        )
      )
  })

  # -- Download panel --------------------------------------------------------
  output$download_panel <- shiny::renderUI({
    fv <- fit_result()
    if (is.null(fv)) {
      return(tags$div(
        class = "fv-empty",
        tags$div(class = "fv-empty-icon", shiny::icon("download")),
        tags$p("Run a fit first to enable downloads.")
      ))
    }

    tagList(
      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_header("HTML Report"),
          bslib::card_body(
            tags$p("A self-contained HTML report with data summary, ranking table,
                    goodness-of-fit tests, and the diagnostic plot."),
            shiny::downloadButton(
              "dl_report", "Download Report (.html)",
              class = "fv-download-btn"
            )
          )
        ),

        bslib::card(
          bslib::card_header("Diagnostic Plot"),
          bslib::card_body(
            tags$p("High-resolution PNG of the density + Q-Q diagnostic plot
                    (300 dpi, 12 ? 5 in, suitable for publication)."),
            shiny::downloadButton(
              "dl_plot", "Download Plot (.png)",
              class = "fv-download-btn secondary"
            ),
            tags$br(), tags$br(),
            shiny::downloadButton(
              "dl_data", "Download Cleaned Data (.csv)",
              class = "fv-download-btn secondary"
            )
          )
        )
      ),

      tags$br(),

      bslib::card(
        bslib::card_header(
          tags$span(shiny::icon("code", style = "margin-right:6px;"),
                    "Export R Code")
        ),
        bslib::card_body(
          tags$p(
            "Generate the exact ", tags$code("fitverse()"), " call that reproduces",
            " this fit; copy it into R to continue your analysis,",
            " share results with colleagues, or build on top of the package."
          ),
          shiny::actionButton(
            "show_rcode", "Show R Code",
            icon  = shiny::icon("code"),
            class = "fv-download-btn secondary"
          )
        )
      )
    )
  })

  output$dl_report <- shiny::downloadHandler(
    filename = function() {
      paste0("fitverse_report_",
             format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      fv <- fit_result()
      shiny::req(fv)
      shiny::withProgress(message = "Generating report...", value = 0.5, {
        tryCatch(
          generate_report(fv, output_file = file,
                          title  = paste("FitVerse Report:", xlab_val()),
                          author = "FitVerse Shiny App",
                          open   = FALSE),
          error = function(e) {
            shiny::showNotification(paste("Report error:", conditionMessage(e)),
                                    type = "error", duration = 8)
          }
        )
        shiny::setProgress(1)
      })
    }
  )

  output$dl_plot <- shiny::downloadHandler(
    filename = function() {
      paste0("fitverse_plot_",
             format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      fv <- fit_result()
      shiny::req(fv)
      tryCatch(
        save_plot(fv, file = file,
                  which = "both", top_n = input$top_n,
                  xlab  = xlab_val(),
                  width = 12, height = 5, dpi = 300),
        error = function(e) {
          shiny::showNotification(paste("Plot save error:", conditionMessage(e)),
                                  type = "error", duration = 8)
        }
      )
    }
  )

  output$dl_data <- shiny::downloadHandler(
    filename = function() {
      paste0("fitverse_data_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      fv <- fit_result()
      shiny::req(fv)
      df_out <- data.frame(value = fv$data)
      names(df_out) <- xlab_val()
      utils::write.csv(df_out, file, row.names = FALSE)
    }
  )

  # -- Export R Code ---------------------------------------------------------
  shiny::observeEvent(input$show_rcode, {
    fv <- fit_result()
    shiny::req(fv)

    # Build dists argument
    dists_arg <- if (input$dist_mode == "auto") {
      '  dists     = "all",'
    } else {
      dv <- input$manual_dists
      paste0('  dists     = c("', paste(dv, collapse = '", "'), '"),')
    }

    # Build method argument
    method_str <- if (input$method == "ALL") {
      'c("MLE", "MOM", "LMOM")'
    } else {
      paste0('"', input$method, '"')
    }

    # First few data values as a hint (full data too large to inline)
    n    <- length(fv$data)
    peek <- paste(round(head(fv$data, 6), 4), collapse = ", ")
    data_comment <- if (n > 6) paste0(peek, ",\n#        ...  # ", n, " observations total") else peek

    rcode <- paste0(
      "# -- Reproduce this FitVerse fit in R --------------------------------------\n",
      "# Run once if FitVerse is not yet installed:\n",
      "# install.packages(\"FitVerse\")\n",
      "library(FitVerse)\n\n",
      "# Supply your data (replace with read.csv or your own vector):\n",
      "# x <- read.csv(\"your_file.csv\")$", gsub(" ", "_", xlab_val()), "\n",
      "x <- c(", data_comment, ")\n\n",
      "fit <- fitverse(\n",
      "  x         = x,\n",
      dists_arg, "\n",
      "  method    = ", method_str, ",\n",
      "  criterion = \"", input$criterion, "\",\n",
      "  xlab      = \"", xlab_val(), "\",\n",
      "  top_n     = ", input$top_n, "\n",
      ")\n\n",
      "print(fit)   # best-fit summary\n",
      "plot(fit)    # density + Q-Q diagnostic plot\n\n",
      "# Optional: bootstrap CIs for parameters and return levels\n",
      "# ci <- bootstrap_ci(fit, B = 499, conf = 0.95, seed = 42)\n",
      "# print(ci)\n\n",
      "# Optional: T-year return levels\n",
      "# return_level(fit, return_periods = c(10, 50, 100))\n"
    )

    shiny::showModal(shiny::modalDialog(
      title     = tags$span(shiny::icon("code"), " Reproduce this fit in R"),
      size      = "l",
      easyClose = TRUE,
      tags$div(
        style = "position:relative;",
        tags$button(
          "Copy to clipboard",
          id      = "fv_copy_btn",
          onclick = paste0(
            "navigator.clipboard.writeText(",
            "document.getElementById('fv_rcode_pre').innerText",
            ").then(function(){",
            "  var b=document.getElementById('fv_copy_btn');",
            "  b.textContent='OK Copied!';",
            "  setTimeout(function(){ b.textContent='Copy to clipboard'; },2000);",
            "});"
          ),
          class = "btn btn-sm btn-outline-secondary",
          style = "position:absolute;top:8px;right:8px;z-index:10;"
        ),
        tags$pre(
          id    = "fv_rcode_pre",
          style = paste0(
            "background:#f8fafc; border:1px solid #e0e6ef; border-radius:6px;",
            "padding:40px 16px 16px 16px; font-size:0.82rem;",
            "overflow-x:auto; white-space:pre; font-family:monospace;"
          ),
          rcode
        )
      ),
      footer = shiny::modalButton("Close")
    ))
  })

  # -- Threshold Selection ----------------------------------------------------
  thr_result <- shiny::eventReactive(input$thr_run, {
    fv <- fit_result()
    shiny::validate(
      shiny::need(!is.null(fv), "Run Fit Distributions first."),
      shiny::need(length(fv$data) >= 2 * input$thr_min_exc,
                  sprintf("Need at least %d observations (have %d).",
                          2L * input$thr_min_exc, length(fv$data)))
    )
    ev_threshold(
      x               = fv$data,
      n_thresholds    = as.integer(input$thr_n),
      min_exceedances = as.integer(input$thr_min_exc),
      conf            = input$thr_conf,
      xlab            = xlab_val(),
      draw            = FALSE
    )
  })

  output$thr_mrl_plot <- plotly::renderPlotly({
    res <- thr_result()
    shiny::validate(shiny::need(!is.null(res$p_mrl), "MRL plot unavailable."))
    plotly::ggplotly(res$p_mrl, tooltip = c("x", "y")) |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = -0.18),
        margin = list(t = 45, b = 60)
      )
  })

  output$thr_shape_plot <- plotly::renderPlotly({
    res <- thr_result()
    shiny::validate(shiny::need(!is.null(res$p_shape),
                                "GPD shape plot unavailable; install the evd package."))
    plotly::ggplotly(res$p_shape, tooltip = c("x", "y")) |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = -0.18),
        margin = list(t = 45, b = 60)
      )
  })

  output$thr_scale_plot <- plotly::renderPlotly({
    res <- thr_result()
    shiny::validate(shiny::need(!is.null(res$p_scale),
                                "GPD modified-scale plot unavailable; install the evd package."))
    plotly::ggplotly(res$p_scale, tooltip = c("x", "y")) |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = -0.18),
        margin = list(t = 45, b = 60)
      )
  })

  output$thr_result_panel <- shiny::renderUI({
    res <- thr_result()

    mrl_section <- bslib::card(
      bslib::card_header(
        shiny::icon("chart-line"), " Mean Residual Life Plot",
        class = "bg-light fw-semibold"
      ),
      bslib::card_body(
        shinycssloaders_wrapper(
          plotly::plotlyOutput("thr_mrl_plot", height = "340px"),
          "Building MRL plot..."
        ),
        tags$div(
          style = "margin-top:8px; padding:8px 12px; background:#f8fafc;
                   border-left:3px solid #3d7ebf; border-radius:0 6px 6px 0; font-size:0.80rem; color:#4a5568;",
          tags$strong("How to read: "),
          "If GPD holds above threshold u0, the mean excess e(u) = E[X ? u | X > u] is ",
          tags$em("linear"), " in u for u > u0. Choose the lowest u where linearity begins."
        )
      )
    )

    gpd_section <- if (!is.null(res$p_shape)) {
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(
            shiny::icon("circle-dot"), " GPD Shape (xi) Stability",
            class = "bg-light fw-semibold"
          ),
          bslib::card_body(
            shinycssloaders_wrapper(
              plotly::plotlyOutput("thr_shape_plot", height = "300px"),
              "Fitting GPD models..."
            ),
            tags$div(
              style = "margin-top:8px; padding:8px 12px; background:#fff8f5;
                       border-left:3px solid #D85A30; border-radius:0 6px 6px 0; font-size:0.80rem; color:#4a5568;",
              tags$strong("xi > 0: "), "heavy tail (Frechet-type).  ",
              tags$strong("xi = 0: "), "exponential tail.  ",
              tags$strong("xi < 0: "), "bounded tail.  ",
              "Should be approximately constant above the true threshold."
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            shiny::icon("arrows-left-right-to-line"), " GPD Modified Scale (sigma*) Stability",
            class = "bg-light fw-semibold"
          ),
          bslib::card_body(
            shinycssloaders_wrapper(
              plotly::plotlyOutput("thr_scale_plot", height = "300px"),
              "Fitting GPD models..."
            ),
            tags$div(
              style = "margin-top:8px; padding:8px 12px; background:#f0faf6;
                       border-left:3px solid #0F6E56; border-radius:0 6px 6px 0; font-size:0.80rem; color:#4a5568;",
              tags$strong("sigma* = sigma ? xiu"), " is threshold-invariant when GPD holds ",
              "(Coles 2001, p. 78).  Should be approximately constant above the true threshold."
            )
          )
        )
      )
    } else {
      tags$p(class = "text-muted small mt-3",
        "GPD stability plots could not be computed. Ensure the ",
        tags$code("evd"), " package is installed alongside FitVerse."
      )
    }

    tags$div(
      mrl_section,
      tags$div(style = "margin-top:16px;", gpd_section),
      tags$div(
        style = "margin-top:12px; font-size:0.76rem; color:#718096;",
        "Ref: Davison & Smith (1990). J. R. Stat. Soc. B, 52(3), 393-442. . ",
        "Coles, S. (2001). An Introduction to Statistical Modeling of Extreme Values. Springer."
      )
    )
  })

  # -- Return Levels ----------------------------------------------------------
  rl_result <- shiny::eventReactive(input$rl_run, {
    fv <- fit_result()
    shiny::validate(
      shiny::need(!is.null(fv), "Run Fit Distributions first.")
    )

    # Parse return periods from text input
    raw <- trimws(input$rl_periods)
    shiny::validate(
      shiny::need(nzchar(raw), "Please enter at least one return period.")
    )
    periods <- suppressWarnings(
      as.numeric(strsplit(raw, "[,;\\s]+")[[1]])
    )
    periods <- periods[!is.na(periods) & periods > 1]
    shiny::validate(
      shiny::need(length(periods) > 0,
                  "Please enter valid return periods greater than 1.")
    )

    tryCatch(
      return_level(fv, return_periods = sort(periods)),
      error = function(e) {
        shiny::showNotification(
          paste("Return level error:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      }
    )
  })

  output$rl_result_panel <- shiny::renderUI({
    rl <- rl_result()
    if (is.null(rl)) {
      return(tags$p(class = "text-muted",
                    "Enter return periods above and click Compute Return Levels."))
    }

    fv    <- fit_result()
    entry <- get_dist_entry(fv$best_fit$dist_key)

    tagList(
      tags$p(
        class = "fv-status-box success",
        shiny::icon("circle-check"),
        sprintf(" Return levels: %s (%s)",
                entry$display_name, fv$best_fit$method)
      ),
      DT::renderDT(
        DT::datatable(
          rl,
          rownames  = FALSE,
          class     = "compact stripe hover",
          options   = list(dom = "t", pageLength = 25)
        ) |>
          DT::formatRound(c("return_level"), digits = 4) |>
          DT::formatStyle(
            "return_period",
            fontWeight = "bold"
          )
      ),
      tags$br(),
      shiny::downloadButton("dl_rl", "Download Return Levels (.csv)",
                            class = "fv-download-btn secondary")
    )
  })

  output$dl_rl <- shiny::downloadHandler(
    filename = function()
      paste0("fitverse_return_levels_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      rl <- rl_result()
      shiny::req(rl)
      utils::write.csv(rl, file, row.names = FALSE)
    }
  )

  # -- Bootstrap -- pass return_periods if provided ---------------------------
  # (Override boot_result to include return_periods)
  boot_result <- shiny::eventReactive(input$boot_run, {
    fv <- fit_result()
    shiny::validate(
      shiny::need(!is.null(fv), "Run Fit Distributions first."),
      shiny::need(input$boot_B  >= 99,  "B must be at least 99."),
      shiny::need(input$boot_conf > 0 && input$boot_conf < 1,
                  "Confidence level must be between 0 and 1.")
    )

    # Parse optional return periods
    rp_raw <- trimws(input$boot_periods)
    rp <- if (nzchar(rp_raw)) {
      v <- suppressWarnings(as.numeric(strsplit(rp_raw, "[,;\\s]+")[[1]]))
      v <- v[!is.na(v) & v > 1]
      if (length(v) > 0) sort(v) else NULL
    } else NULL

    shiny::withProgress(
      message = sprintf("Running %d bootstrap replicates...", input$boot_B),
      value   = 0, {
        prog_fn <- function(p) shiny::setProgress(p)
        ci <- tryCatch(
          bootstrap_ci(fv,
                       B              = as.integer(input$boot_B),
                       conf           = input$boot_conf,
                       return_periods = rp,
                       seed           = as.integer(input$boot_seed),
                       progress       = prog_fn,
                       verbose        = FALSE),
          error = function(e) {
            shiny::showNotification(paste("Bootstrap error:", conditionMessage(e)),
                                    type = "error", duration = 8)
            NULL
          }
        )
        shiny::setProgress(1)
        ci
      }
    )
  })

  output$boot_result_panel <- shiny::renderUI({
    ci <- boot_result()
    if (is.null(ci)) {
      return(tags$p(class = "text-muted",
                    "Configure settings above and click Run Bootstrap."))
    }

    entry <- get_dist_entry(ci$dist_key)
    out <- tagList(
      tags$p(
        class = "fv-status-box success",
        shiny::icon("circle-check"),
        sprintf(" %d%% CI  |  B = %d  |  %s (%s)",
                round(ci$conf * 100), ci$B,
                entry$display_name, ci$method)
      ),
      tags$h6("Parameter confidence intervals"),
      DT::renderDT(
        DT::datatable(ci$params_ci, rownames = FALSE,
                      class = "compact stripe",
                      options = list(dom = "t")) |>
          DT::formatRound(c("estimate","lower","upper"), digits = 5)
      )
    )

    # Return-level CI table if available
    if (!is.null(ci$return_level_ci)) {
      out <- tagList(
        out,
        tags$h6("Return-level confidence intervals", class = "mt-3"),
        DT::renderDT(
          DT::datatable(ci$return_level_ci, rownames = FALSE,
                        class = "compact stripe",
                        options = list(dom = "t")) |>
            DT::formatRound(c("return_level","lower","upper"), digits = 4)
        )
      )
    }

    out
  })

  # -- Batch Fit -------------------------------------------------------------
  batch_data <- shiny::reactive({
    shiny::req(input$batch_file)
    tryCatch(
      utils::read.csv(input$batch_file$datapath,
                      header = input$batch_header,
                      stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })

  output$batch_col_picker <- shiny::renderUI({
    df <- batch_data()
    shiny::req(df)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(num_cols) == 0)
      return(tags$p("No numeric columns found.",
                    style = "color:#e74c3c; font-size:0.83rem;"))

    shiny::checkboxGroupInput(
      "batch_cols", "Columns to fit",
      choices  = num_cols,
      selected = num_cols
    )
  })

  batch_result <- shiny::eventReactive(input$batch_run, {
    df <- batch_data()
    shiny::validate(
      shiny::need(!is.null(df), "Upload a CSV file first."),
      shiny::need(length(input$batch_cols) >= 1,
                  "Select at least one column.")
    )

    method <- if (input$batch_method == "ALL") c("MLE","MOM","LMOM") else input$batch_method

    # Keep only selected columns
    df_sel <- df[, input$batch_cols, drop = FALSE]

    shiny::withProgress(
      message = "Running batch fit...",
      detail  = sprintf("Fitting %d columns...", ncol(df_sel)),
      value   = 0, {
        res <- tryCatch(
          fitverse_batch(
            df_sel,
            method    = method,
            criterion = input$batch_criterion,
            plot      = FALSE,
            verbose   = FALSE
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Batch error:", conditionMessage(e)),
              type = "error", duration = 8
            )
            NULL
          }
        )
        shiny::setProgress(1)
        res
      }
    )
  })

  output$batch_status <- shiny::renderUI({
    br <- batch_result()
    if (is.null(br)) {
      df <- batch_data()
      if (is.null(df))
        return(tags$div(class = "fv-status-box",
                        shiny::icon("circle-info"),
                        " Upload a CSV file and click Run Batch Fit."))
      return(NULL)
    }
    tags$div(
      class = "fv-status-box success",
      shiny::icon("circle-check"),
      sprintf(" Batch complete: %d columns fitted.", length(br$fits))
    )
  })

  output$batch_table <- DT::renderDT({
    br <- batch_result()
    shiny::req(br, nrow(br$summary) > 0)

    tbl <- br$summary

    DT::datatable(
      tbl,
      rownames  = FALSE,
      class     = "compact hover stripe",
      options   = list(pageLength = 25, dom = "tip",
                       scrollX = TRUE)
    ) |>
      DT::formatStyle(
        "Distribution",
        fontWeight = "bold"
      ) |>
      DT::formatStyle(
        "AIC",
        background = DT::styleColorBar(range(tbl$AIC, na.rm = TRUE), "#d0e8f5"),
        backgroundSize   = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$batch_download_ui <- shiny::renderUI({
    br <- batch_result()
    shiny::req(br)
    shiny::downloadButton("dl_batch", "Download Batch Results (.csv)",
                          class = "fv-download-btn secondary")
  })

  output$dl_batch <- shiny::downloadHandler(
    filename = function()
      paste0("fitverse_batch_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      br <- batch_result()
      shiny::req(br)
      utils::write.csv(br$summary, file, row.names = FALSE)
    }
  )


  # +===========================================================+
  # |  Censored Fit -- Server                                    |
  # +===========================================================+

  # -- Reactive: read censored CSV ----------------------------------------------
  cens_raw <- shiny::reactive({
    shiny::req(input$cens_file)
    tryCatch(
      utils::read.csv(input$cens_file$datapath,
                      header           = TRUE,
                      stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })

  # -- UI: column pickers --------------------------------------------------------
  # -- Auto-detect columns by name -----------------------------------------------
  cens_cols <- shiny::reactive({
    df <- cens_raw(); shiny::req(df)
    nms <- names(df)
    list(
      has_x       = "x"       %in% nms,
      has_status  = "status"  %in% nms,
      has_x_upper = "x_upper" %in% nms
    )
  })

  output$cens_detect_ui <- shiny::renderUI({
    df <- cens_raw()
    if (is.null(df)) return(NULL)
    cols  <- cens_cols()
    nms   <- names(df)

    check <- function(found, label)
      tags$div(
        style = "display:flex; align-items:center; gap:6px; margin-bottom:3px; font-size:0.83rem;",
        if (found)
          tags$span(style = "color:#0F6E56; font-weight:700;", "OK")
        else
          tags$span(style = "color:#c0392b; font-weight:700;", "X"),
        tags$code(label),
        if (!found) tags$span(class = "text-muted", "(not found)")
      )

    tagList(
      tags$div(
        style = paste0("background:#f4f7fb; border:1px solid #d0dce8;",
                       "border-radius:6px; padding:10px 12px; margin-top:4px;"),
        tags$p(style = "font-size:0.78rem; font-weight:600; color:#4A6278; margin-bottom:6px;",
               "Detected columns:"),
        check(cols$has_x,       "x"),
        check(cols$has_status,  "status"),
        check(cols$has_x_upper, "x_upper"),
        if (!cols$has_x)
          tags$p(class = "text-danger small mt-2 mb-0",
                 shiny::icon("triangle-exclamation"),
                 " Rename your value column to ", tags$code("x"), " and re-upload.")
        else if (!cols$has_status)
          tags$p(class = "text-muted small mt-2 mb-0",
                 shiny::icon("circle-info"),
                 " No ", tags$code("status"), " column; all rows treated as fully observed.")
        else
          tags$p(class = "text-muted small mt-2 mb-0",
                 sprintf("n = %d  |  observed: %d  |  censored: %d",
                         nrow(df),
                         sum(df$status == 1L, na.rm = TRUE),
                         sum(df$status != 1L, na.rm = TRUE)))
      )
    )
  })

  # -- Reactive: error message store (avoids duplicating via shiny::validate) ---
  cens_error_rv <- shiny::reactiveVal(NULL)

  # -- Reactive: run fitverse_censored() ----------------------------------------
  cens_result <- shiny::eventReactive(input$cens_run, {
    cens_error_rv(NULL)   # reset any previous error
    df   <- cens_raw()
    cols <- cens_cols()
    shiny::req(df)
    shiny::validate(
      shiny::need(cols$has_x,
                  "Column 'x' not found. Rename your value column to x and re-upload.")
    )

    x    <- as.numeric(df[["x"]])
    xlab <- "x"

    # Status: read from 'status' column if present, else all observed
    status <- if (cols$has_status) as.integer(df[["status"]]) else rep(1L, length(x))

    # Upper bounds: read from 'x_upper' if present and needed
    x_upper <- NULL
    if (any(status == 3L, na.rm = TRUE)) {
      shiny::validate(
        shiny::need(cols$has_x_upper,
                    "Interval-censored rows (status=3) found but no 'x_upper' column. Add it and re-upload.")
      )
      x_upper <- as.numeric(df[["x_upper"]])
    }

    # Truncation
    tl <- if (isTRUE(input$cens_trunc) && is.finite(input$cens_tl)) input$cens_tl else -Inf
    tu <- if (isTRUE(input$cens_trunc) && is.finite(input$cens_tu)) input$cens_tu else  Inf

    dists <- input$cens_dists
    shiny::validate(shiny::need(length(dists) > 0L, "Select at least one distribution."))

    tryCatch(
      fitverse_censored(
        x           = x,
        status      = status,
        x_upper     = x_upper,
        trunc_lower = tl,
        trunc_upper = tu,
        dists       = dists,
        criterion   = input$cens_criterion,
        n_starts    = as.integer(input$cens_starts),
        xlab        = xlab,
        draw        = FALSE
      ),
      error = function(e) {
        cens_error_rv(conditionMessage(e))
        NULL
      }
    )
  })

  # -- Output: survival / CDF plot -----------------------------------------------
  output$cens_empty_plot <- shiny::renderUI({
    err <- cens_error_rv()
    if (!is.null(err)) {
      # Show the error once here; all other outputs use shiny::req() and stay silent
      tags$div(
        class = "fv-empty-state",
        shiny::icon("circle-exclamation",
                    style = "font-size:2rem;color:#c0392b;margin-bottom:10px;"),
        tags$p(err, class = "text-danger small", style = "white-space:pre-wrap;")
      )
    } else if (is.null(cens_result())) {
      tags$div(
        class = "fv-empty-state",
        shiny::icon("hourglass-half",
                    style = "font-size:2rem;color:#b0bec5;margin-bottom:10px;"),
        tags$p("Upload a CSV and click ",
               tags$strong("Fit Censored Data"), " to see results.",
               class = "text-muted")
      )
    }
  })

  output$cens_plot <- shiny::renderPlot({
    res <- cens_result()
    shiny::req(res)
    plot(res, top_n = min(3L, length(res$fits)))
  }, bg = "white")

  # -- Output: rankings table ----------------------------------------------------
  output$cens_empty_table <- shiny::renderUI({
    # Errors are shown in the Plot tab; here just show "no results" when NULL
    if (is.null(cens_result()) && is.null(cens_error_rv()))
      tags$p("No results yet.", class = "text-muted small")
  })

  output$cens_table <- DT::renderDT({
    res <- cens_result(); shiny::req(res)
    tbl <- res$rankings
    DT::datatable(
      tbl,
      selection = "single",
      rownames  = FALSE,
      class     = "compact hover stripe",
      options   = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    ) |>
      DT::formatRound(c("LogLik", "AIC", "BIC"), digits = 3) |>
      DT::formatStyle(
        "Rank",
        target = "row",
        backgroundColor = DT::styleEqual(1, "#e8f4fc")
      ) |>
      DT::formatStyle(
        "AIC",
        background = DT::styleColorBar(range(tbl$AIC, na.rm = TRUE), "#d0e8f5"),
        backgroundSize     = "100% 80%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # -- Output: parameter estimates for selected row ------------------------------
  output$cens_params_ui <- shiny::renderUI({
    res <- cens_result(); shiny::req(res)
    sel <- input$cens_table_rows_selected
    if (is.null(sel)) sel <- 1L
    fit <- res$fits[[sel]]
    shiny::req(fit)

    est <- fit$par
    par_rows <- lapply(seq_along(est), function(i)
      tags$tr(
        tags$td(tags$strong(names(est)[i])),
        tags$td(formatC(est[i], format = "f", digits = 6))
      )
    )

    tags$div(
      class = "mt-3",
      tags$h6(
        shiny::icon("sliders"),
        paste(" Parameter estimates:", fit$display)
      ),
      tags$table(
        class = "table table-sm table-bordered",
        style = "font-size:0.87rem; width:auto;",
        tags$thead(tags$tr(tags$th("Parameter"), tags$th("Estimate"))),
        tags$tbody(par_rows)
      ),
      tags$p(
        class = "text-muted small",
        sprintf("Log-likelihood: %.4f  |  AIC: %.4f  |  BIC: %.4f",
                fit$log_lik, fit$aic, fit$bic)
      )
    )
  })

  # +===========================================================+
  # |  Simulate Data                                            |
  # +===========================================================+

  # -- Dynamic parameter inputs ------------------------------------------------
  output$sim_params_ui <- shiny::renderUI({
    key  <- input$sim_dist
    shiny::req(key, key %in% names(FITVERSE_DIST_REGISTRY))

    entry    <- FITVERSE_DIST_REGISTRY[[key]]
    defaults <- FV_SIM_DEFAULTS[[key]]
    symbols  <- FV_PARAM_SYMBOLS[[key]]

    inputs <- lapply(entry$params, function(pname) {
      label <- if (!is.null(symbols) && pname %in% names(symbols))
        symbols[[pname]] else pname

      def <- if (!is.null(defaults) && pname %in% names(defaults))
        unname(defaults[[pname]]) else 1

      step <- if (abs(def) >= 100) 10 else if (abs(def) >= 10) 1 else 0.1

      shiny::numericInput(
        inputId = paste0("sim_par_", pname),
        label   = label,
        value   = def,
        step    = step
      )
    })

    do.call(shiny::tagList, inputs)
  })

  # -- Collect current parameter values ----------------------------------------
  sim_params_rv <- shiny::reactive({
    key   <- shiny::req(input$sim_dist)
    entry <- FITVERSE_DIST_REGISTRY[[key]]
    defaults <- FV_SIM_DEFAULTS[[key]]

    vapply(entry$params, function(pname) {
      val <- input[[paste0("sim_par_", pname)]]
      if (is.null(val) || !is.finite(val)) {
        unname(defaults[[pname]])
      } else {
        as.numeric(val)
      }
    }, numeric(1L))
  })

  # -- Generate data on button click --------------------------------------------
  sim_data_rv <- shiny::eventReactive(input$sim_generate, {
    key    <- shiny::req(input$sim_dist)
    n      <- as.integer(shiny::req(input$sim_n))
    params <- sim_params_rv()
    seed   <- if (isTRUE(input$sim_use_seed)) as.integer(input$sim_seed) else NULL

    shiny::validate(
      shiny::need(n >= 1L && n <= 100000L,
                  "n must be between 1 and 100,000.")
    )

    result <- tryCatch({
      x <- fv_simulate(key, params, n, seed)
      x_clean <- x[is.finite(x)]
      shiny::validate(
        shiny::need(length(x_clean) > 0,
                    "Simulation produced no finite values; check your parameter values.")
      )
      list(x = x_clean, dist_key = key, params = params,
           n_req = n, n_got = length(x_clean))
    }, error = function(e) {
      shiny::validate(shiny::need(FALSE, paste("Simulation error:", conditionMessage(e))))
    })

    result
  })

  # -- Output: distribution info banner + PDF equation --------------------------
  output$sim_dist_info <- shiny::renderUI({
    sim <- sim_data_rv()

    if (is.null(sim)) {
      return(tags$div(
        class = "fv-empty-state",
        shiny::icon("dice", style = "font-size:2rem;color:#b0bec5;margin-bottom:10px;"),
        tags$p("Choose a distribution, set parameters, then click ",
               tags$strong("Generate Data"), ".", class = "text-muted")
      ))
    }

    entry <- FITVERSE_DIST_REGISTRY[[sim$dist_key]]
    eq    <- FV_PDF_EQUATIONS[[sim$dist_key]]

    tags$div(
      style = paste0("background:#f8fafc;border-left:3px solid #3d7ebf;",
                     "border-radius:0 6px 6px 0;padding:8px 14px;",
                     "margin-bottom:12px;font-size:0.83rem;color:#3a5175;"),
      tags$strong(entry$display_name), ": ", entry$description,
      if (!is.null(eq)) tagList(tags$br(), withMathJax(HTML(eq)))
    )
  })

  # -- Output: histogram with theoretical density overlay -----------------------
  output$sim_hist <- shiny::renderPlot({
    sim <- shiny::req(sim_data_rv())
    x   <- sim$x
    entry <- FITVERSE_DIST_REGISTRY[[sim$dist_key]]

    # Compute histogram bins
    h <- hist(x, plot = FALSE, breaks = "Sturges")

    # Theoretical density curve
    rng  <- range(x)
    span <- diff(rng)
    xs   <- seq(rng[1] - 0.05 * span, rng[2] + 0.05 * span, length.out = 500)

    dfun <- tryCatch(
      match.fun(paste0("d", entry$r_name)),
      error = function(e) NULL
    )
    y_dens <- if (!is.null(dfun)) {
      tryCatch(
        do.call(dfun, c(list(xs), as.list(sim$params))),
        error = function(e) NULL
      )
    } else NULL

    ylim_top <- max(
      max(h$density, na.rm = TRUE),
      if (!is.null(y_dens) && any(is.finite(y_dens))) max(y_dens[is.finite(y_dens)]) else 0
    ) * 1.15

    par_txt <- paste(
      mapply(function(nm, v) paste0(nm, " = ", signif(v, 4)),
             names(sim$params), sim$params),
      collapse = ",  "
    )

    op <- par(mar = c(4.5, 4.5, 3.5, 1.5), bg = "white")
    on.exit(par(op))

    plot(h, freq = FALSE,
         col    = "#d1e4f5",
         border = "#3d7ebf",
         main   = paste0(entry$display_name, "  (n = ", sim$n_got, ")"),
         xlab   = "x",
         ylab   = "Density",
         ylim   = c(0, ylim_top),
         las    = 1,
         cex.axis = 0.9, cex.lab = 0.95)

    if (!is.null(y_dens) && any(is.finite(y_dens))) {
      # Only plot where density is finite and non-negative
      ok <- is.finite(y_dens) & y_dens >= 0
      lines(xs[ok], y_dens[ok], col = "#1e3a5f", lwd = 2.5)
    }

    mtext(par_txt, side = 3, line = 0.2, cex = 0.77, col = "#555555")
    grid(lty = "dotted", col = "#e0e8f0", lwd = 0.8)
    box()
  }, bg = "white")

  # -- Output: summary statistics table -----------------------------------------
  output$sim_summary_ui <- shiny::renderUI({
    sim <- shiny::req(sim_data_rv())
    x   <- sim$x

    xbar <- mean(x);      sdx  <- sd(x)
    med  <- median(x);    q1   <- unname(quantile(x, 0.25))
    q3   <- unname(quantile(x, 0.75))
    skw  <- if (sdx > 0) mean(((x - xbar) / sdx)^3) else 0
    krt  <- if (sdx > 0) mean(((x - xbar) / sdx)^4) - 3 else 0

    row2 <- function(lbl, val)
      tags$tr(tags$td(tags$strong(lbl)), tags$td(val))

    fmt <- function(v, d = 6) formatC(v, digits = d, format = "g")

    tags$div(
      class = "mt-2",
      tags$h5(shiny::icon("magnifying-glass-chart"), " Descriptive Statistics"),
      tags$table(
        class = "table table-sm table-hover table-bordered",
        style = "font-size:0.88rem; max-width:400px;",
        tags$tbody(
          row2("n (generated)",    formatC(length(x), format = "d", big.mark = ",")),
          row2("Mean",             fmt(xbar)),
          row2("Median",           fmt(med)),
          row2("Std Deviation",    fmt(sdx)),
          row2("Skewness",         fmt(skw, 4)),
          row2("Excess Kurtosis",  fmt(krt, 4)),
          row2("Min",              fmt(min(x))),
          row2("Q1 (25th pct.)",   fmt(q1)),
          row2("Q3 (75th pct.)",   fmt(q3)),
          row2("Max",              fmt(max(x))),
          row2("IQR",              fmt(q3 - q1))
        )
      ),
      tags$p(
        class = "text-muted small",
        shiny::icon("circle-info", style = "margin-right:4px;"),
        "Tip: Load this data into Fit Distributions by downloading as CSV and uploading."
      )
    )
  })

  # -- Output: data preview (DT table) ------------------------------------------
  output$sim_table <- DT::renderDT({
    sim <- shiny::req(sim_data_rv())
    df  <- data.frame(
      Row   = seq_along(head(sim$x, 500)),
      Value = head(sim$x, 500)
    )
    DT::datatable(
      df,
      rownames  = FALSE,
      colnames  = c("#", "Simulated value"),
      class     = "compact hover stripe",
      options   = list(
        pageLength = 25,
        dom        = "tip",
        scrollY    = "380px"
      )
    ) |>
      DT::formatRound("Value", digits = 8)
  })

  # -- Download handlers (shared filename logic) ---------------------------------
  .sim_filename <- function() {
    sim <- sim_data_rv()
    if (is.null(sim)) return("simulated_data.csv")
    nm <- gsub("[^A-Za-z0-9]", "_", FITVERSE_DIST_REGISTRY[[sim$dist_key]]$display_name)
    paste0("FitVerse_Simulate_", nm, "_n", sim$n_got, ".csv")
  }
  .sim_content <- function(file) {
    sim <- shiny::req(sim_data_rv())
    utils::write.csv(data.frame(x = sim$x), file, row.names = FALSE)
  }

  output$sim_download_plot  <- shiny::downloadHandler(
    filename = .sim_filename, content = .sim_content
  )
  output$sim_download_table <- shiny::downloadHandler(
    filename = .sim_filename, content = .sim_content
  )

  # -- R Code generation -----------------------------------------------------
  r_code_text <- shiny::reactive({

    hdr <- c(
      "# =============================================================",
      "# FitVerse -- R Code Export",
      paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "# =============================================================",
      "",
      "library(FitVerse)",
      ""
    )

    body  <- character(0)
    has_any <- FALSE

    # -- 1. Fit Distributions ---------------------------------------------
    fv <- tryCatch(fit_result(), error = function(e) NULL)
    if (!is.null(fv)) {
      has_any <- TRUE
      col <- input$selected_col
      data_lines <- if (!is.null(col) && nzchar(col)) {
        c('df <- read.csv("your_data.csv")', paste0("x  <- df$", col))
      } else {
        c('x <- c( /* paste your numbers here */ )',
          '# or: x <- read.csv("your_data.csv")$column_name')
      }

      dists_arg <- if (input$dist_mode == "auto") {
        '"all"'
      } else {
        paste0('c(', paste0('"', input$manual_dists, '"', collapse = ", "), ')')
      }
      method_arg <- if (input$method == "ALL") {
        'c("MLE", "MOM", "LMOM")'
      } else {
        paste0('"', input$method, '"')
      }

      body <- c(body,
        "# --- 1. Fit Distributions -------------------------------------",
        "",
        data_lines,
        "",
        "fit <- fitverse(",
        paste0('  x         = x,'),
        paste0('  dists     = ', dists_arg, ','),
        paste0('  method    = ', method_arg, ','),
        paste0('  criterion = "', input$criterion, '",'),
        paste0('  xlab      = "', xlab_val(), '",'),
        paste0('  top_n     = ', input$top_n),
        ")",
        "",
        "print(fit)    # ranked table",
        "summary(fit)  # best-fit parameters + GOF tests",
        "plot(fit)     # diagnostic plot",
        ""
      )

      # Bootstrap CI (only if run)
      ci <- tryCatch(boot_result(), error = function(e) NULL)
      if (!is.null(ci)) {
        rp_raw  <- trimws(input$boot_periods %||% "")
        rp_part <- if (nzchar(rp_raw))
          paste0(",\n  return_periods = c(", rp_raw, ")")
        else ""
        body <- c(body,
          "# Bootstrap confidence intervals",
          "boot <- bootstrap_ci(fit,",
          paste0('  B    = ', input$boot_B, ','),
          paste0('  conf = ', input$boot_conf, ','),
          paste0('  seed = ', input$boot_seed, rp_part),
          ")",
          "print(boot)",
          ""
        )
      }

      # Return levels (only if run)
      rl <- tryCatch(rl_result(), error = function(e) NULL)
      if (!is.null(rl)) {
        body <- c(body,
          "# Return levels",
          paste0('rl <- return_level(fit, return_periods = c(', input$rl_periods, '))'),
          "print(rl)",
          ""
        )
      }

      body <- c(body,
        "# HTML report",
        'generate_report(fit, output_file = "FitVerse_report.html", open = TRUE)',
        ""
      )
    }

    # -- 2. Batch Fit -----------------------------------------------------
    br <- tryCatch(batch_result(), error = function(e) NULL)
    if (!is.null(br)) {
      has_any <- TRUE
      method_arg <- if (input$batch_method == "ALL") {
        'c("MLE", "MOM", "LMOM")'
      } else {
        paste0('"', input$batch_method, '"')
      }
      cols_sel <- input$batch_cols %||% character(0)

      body <- c(body,
        "# --- 2. Batch Fit ---------------------------------------------",
        "",
        'df_batch <- read.csv("your_batch_data.csv")'
      )
      if (length(cols_sel) > 0) {
        body <- c(body,
          paste0('df_batch <- df_batch[, c(', paste0('"', cols_sel, '"', collapse = ", "), ')]')
        )
      }
      body <- c(body,
        "",
        "batch_fit <- fitverse_batch(",
        "  df_batch,",
        paste0('  method    = ', method_arg, ','),
        paste0('  criterion = "', input$batch_criterion, '"'),
        ")",
        "",
        "print(batch_fit)",
        "summary(batch_fit)",
        ""
      )
    }

    # -- 3. Censored Fit ---------------------------------------------------
    cr <- tryCatch(cens_result(), error = function(e) NULL)
    if (!is.null(cr)) {
      has_any <- TRUE
      dists_arg <- paste0('c(', paste0('"', input$cens_dists, '"', collapse = ", "), ')')
      tl_arg <- if (isTRUE(input$cens_trunc) && is.finite(input$cens_tl)) input$cens_tl else -Inf
      tu_arg <- if (isTRUE(input$cens_trunc) && is.finite(input$cens_tu)) input$cens_tu else  Inf

      body <- c(body,
        "# --- 3. Censored Fit ------------------------------------------",
        "",
        'df_cens <- read.csv("your_censored_data.csv")',
        "# Required column: x (numeric value or time)",
        "# Optional: status  (0 = right-censored, 1 = observed,",
        "#                    2 = left-censored,  3 = interval-censored)",
        "# Optional: x_upper (upper bound for status = 3 rows)",
        "",
        "cens_fit <- fitverse_censored(",
        "  x           = df_cens$x,",
        "  status      = df_cens$status,",
        paste0('  dists       = ', dists_arg, ','),
        paste0('  criterion   = "', input$cens_criterion, '",'),
        paste0('  n_starts    = ', input$cens_starts, ','),
        paste0('  trunc_lower = ', tl_arg, ','),
        paste0('  trunc_upper = ', tu_arg),
        ")",
        "",
        "print(cens_fit)",
        ""
      )
    }

    # -- 4. Simulate Data -------------------------------------------------
    sim <- tryCatch(sim_data_rv(), error = function(e) NULL)
    if (!is.null(sim)) {
      has_any <- TRUE
      params  <- sim$params
      p_str   <- paste0(names(params), " = ",
                        round(unlist(params), 6), collapse = ", ")
      seed_val <- if (isTRUE(input$sim_use_seed)) input$sim_seed else "NULL"

      body <- c(body,
        "# --- 4. Simulate Data -----------------------------------------",
        "",
        paste0("# Distribution : ", sim$dist_key, "  |  n = ", sim$n_req),
        if (isTRUE(input$sim_use_seed)) paste0("set.seed(", input$sim_seed, ")") else character(0),
        paste0('sim_data <- FitVerse:::fv_simulate("', sim$dist_key, '",'),
        paste0('               params = list(', p_str, '),'),
        paste0('               n      = ', sim$n_req, ','),
        paste0('               seed   = ', seed_val, ")"),
        "",
        "# Fit to verify parameter recovery:",
        paste0('fit_sim <- fitverse(sim_data, xlab = "Simulated ', sim$dist_key, '")'),
        "print(fit_sim)",
        ""
      )
    }

    if (!has_any) {
      body <- c(body,
        "# No actions recorded yet.",
        "#",
        "# Use Fit Distributions, Batch Fit, Censored Fit, or",
        "# Simulate Data -- code will appear here automatically.",
        ""
      )
    }

    ftr <- c(
      "# =============================================================",
      "# End of FitVerse R Code export",
      "# ============================================================="
    )

    paste(c(hdr, body, ftr), collapse = "\n")
  })

  output$rcode_display <- shiny::renderText({
    r_code_text()
  })

  output$download_rcode <- shiny::downloadHandler(
    filename = function() {
      paste0("FitVerse_export_", format(Sys.Date(), "%Y%m%d"), ".R")
    },
    content = function(file) {
      writeLines(r_code_text(), file)
    }
  )


  # ===========================================================================
  # EXPLORE DISTRIBUTIONS (StatAssist)
  # ===========================================================================

  # -- Dynamic parameter sliders/inputs ---------------------------------------
  output$explore_param_ui <- shiny::renderUI({
    key   <- input$explore_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))

    params  <- entry$params
    lower_b <- entry$lower
    upper_b <- entry$upper
    # Default starting values from data if present, else generic
    starts  <- tryCatch(entry$start_fun(c(1, 2, 3, 4, 5)),
                        error = function(e) NULL)

    lapply(seq_along(params), function(i) {
      pname <- params[i]
      lo    <- if (!is.null(lower_b) && pname %in% names(lower_b)) lower_b[[pname]] else -Inf
      hi    <- if (!is.null(upper_b) && pname %in% names(upper_b)) upper_b[[pname]] else  Inf
      def   <- if (!is.null(starts) && pname %in% names(starts))
        round(as.numeric(starts[[pname]]), 4)
      else if (is.finite(lo) && is.finite(hi)) round((lo + hi) / 2, 4)
      else if (is.finite(lo)) lo + 1
      else 1

      step_val <- if (is.finite(lo) && is.finite(hi)) max(1e-3, (hi - lo) / 100) else 0.1

      shiny::numericInput(
        inputId = paste0("ep_", pname),
        label   = pname,
        value   = def,
        step    = step_val,
        min     = if (is.finite(lo)) lo else NA,
        max     = if (is.finite(hi)) hi else NA
      )
    })
  })

  # -- Helper: collect current parameter list ---------------------------------
  explore_params <- shiny::reactive({
    key   <- input$explore_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))

    pnames <- entry$params
    vals <- setNames(
      vapply(pnames, function(pname) {
        v <- input[[paste0("ep_", pname)]]
        if (is.null(v) || !is.finite(v)) NA_real_ else as.numeric(v)
      }, numeric(1L)),
      pnames
    )
    vals
  })

  # -- Distribution banner ----------------------------------------------------
  output$explore_dist_banner <- shiny::renderUI({
    key   <- input$explore_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))
    pv    <- explore_params()
    if (anyNA(pv)) return(NULL)

    param_str <- paste0(names(pv), " = ", round(pv, 5), collapse = ", ")
    tags$div(
      class = "fv-status-box success mb-2",
      shiny::icon("circle-info"),
      sprintf(" %s  |  %s", entry$display_name, param_str)
    )
  })

  # -- Main explore plot -------------------------------------------------------
  output$explore_plot <- plotly::renderPlotly({
    key   <- input$explore_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))

    pv <- explore_params()
    shiny::req(!anyNA(pv))

    d_fn <- tryCatch(.lookup_dpqr("d", key), error = function(e) NULL)
    p_fn <- tryCatch(.lookup_dpqr("p", key), error = function(e) NULL)
    q_fn <- tryCatch(.lookup_dpqr("q", key), error = function(e) NULL)

    # Build x-grid from 0.5th to 99.5th quantile of the distribution
    xs <- tryCatch({
      if (!is.null(q_fn)) {
        lo_q <- do.call(q_fn, c(list(p = 0.005), as.list(pv)))
        hi_q <- do.call(q_fn, c(list(p = 0.995), as.list(pv)))
        if (!is.finite(lo_q) || !is.finite(hi_q)) stop("infinite quantile")
        seq(lo_q, hi_q, length.out = 512)
      } else {
        seq(-5, 5, length.out = 512)
      }
    }, error = function(e) seq(-5, 5, length.out = 512))

    ctype <- input$explore_curve %||% "PDF"

    ys <- tryCatch({
      if (ctype == "PDF") {
        shiny::req(!is.null(d_fn))
        do.call(d_fn, c(list(x = xs), as.list(pv)))
      } else {
        shiny::req(!is.null(p_fn))
        Fx <- pmax(0, pmin(1, do.call(p_fn, c(list(q = xs), as.list(pv)))))
        if      (ctype == "CDF")               Fx
        else if (ctype == "Survival")          1 - Fx
        else if (ctype == "Hazard") {
          shiny::req(!is.null(d_fn))
          fx <- pmax(0, do.call(d_fn, c(list(x = xs), as.list(pv))))
          Sx <- pmax(1 - Fx, 1e-10)
          fx / Sx
        } else {   # Cumulative Hazard
          -log(pmax(1 - Fx, 1e-10))
        }
      }
    }, error = function(e) NULL)
    shiny::req(!is.null(ys))

    ys[!is.finite(ys)] <- NA_real_

    colour <- switch(ctype,
      "PDF"               = "#4f46e5",
      "CDF"               = "#2d6bc4",
      "Survival"          = "#27ae60",
      "Hazard"            = "#c0392b",
      "Cumulative Hazard" = "#8e44ad"
    )
    y_lbl <- switch(ctype,
      "PDF"               = "Density  f(x)",
      "CDF"               = "F(x) = P(X ≤ x)",
      "Survival"          = "S(x) = 1 − F(x)",
      "Hazard"            = "h(x) = f(x) / S(x)",
      "Cumulative Hazard" = "H(x) = −log S(x)"
    )

    df_e <- data.frame(x = xs, y = ys,
                       tip = paste0("x = ", round(xs, 4), "<br>",
                                    ctype, " = ", round(ys, 6)))

    p <- ggplot2::ggplot(df_e, ggplot2::aes(x = x, y = y, text = tip)) +
      ggplot2::geom_line(colour = colour, linewidth = 1.2, na.rm = TRUE) +
      ggplot2::geom_area(fill = colour, alpha = 0.08, na.rm = TRUE) +
      ggplot2::labs(
        title = paste0(ctype, " — ", entry$display_name),
        x     = "x",
        y     = y_lbl
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
        plot.background  = ggplot2::element_rect(fill = "white",   colour = NA),
        plot.title = ggplot2::element_text(size = 13, colour = "#1a2b3c", face = "bold"),
        axis.title = ggplot2::element_text(size = 11, colour = "#1a2b3c")
      )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#EDF2F7",
        margin        = list(t = 50, b = 50)
      ) |>
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0("explore_", key, "_", tolower(ctype)),
          width    = 1000, height = 550, scale = 2
        )
      )
  })

  # -- Statistics panel -------------------------------------------------------
  output$explore_stats_ui <- shiny::renderUI({
    key   <- input$explore_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))

    pv <- explore_params()
    shiny::req(!anyNA(pv))

    d_fn <- tryCatch(.lookup_dpqr("d", key), error = function(e) NULL)
    p_fn <- tryCatch(.lookup_dpqr("p", key), error = function(e) NULL)
    q_fn <- tryCatch(.lookup_dpqr("q", key), error = function(e) NULL)

    # Numerically compute mean, variance, skewness, kurtosis via density on a grid
    stats_num <- tryCatch({
      if (is.null(d_fn) || is.null(q_fn)) stop("no d/q functions")
      lo_q <- do.call(q_fn, c(list(p = 1e-5), as.list(pv)))
      hi_q <- do.call(q_fn, c(list(p = 1 - 1e-5), as.list(pv)))
      if (!is.finite(lo_q) || !is.finite(hi_q)) stop("infinite")
      xs <- seq(lo_q, hi_q, length.out = 2000)
      dx <- xs[2] - xs[1]
      fx <- pmax(0, do.call(d_fn, c(list(x = xs), as.list(pv))))
      mn  <- sum(xs * fx) * dx
      vr  <- sum((xs - mn)^2 * fx) * dx
      sd_ <- sqrt(max(vr, 0))
      sk  <- if (sd_ > 0) sum(((xs - mn) / sd_)^3 * fx) * dx else 0
      kt  <- if (sd_ > 0) sum(((xs - mn) / sd_)^4 * fx) * dx else 0
      list(mean = mn, var = vr, sd = sd_, skewness = sk, kurtosis = kt)
    }, error = function(e) NULL)

    # Quantiles
    quantiles_list <- if (!is.null(q_fn)) {
      probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
      qvals <- tryCatch(
        do.call(q_fn, c(list(p = probs), as.list(pv))),
        error = function(e) rep(NA_real_, length(probs))
      )
      setNames(qvals, paste0("Q", probs * 100, "%"))
    } else NULL

    # Tail probabilities at mean ± 1 SD
    tail_ps <- if (!is.null(p_fn) && !is.null(stats_num)) {
      thr_lo <- stats_num$mean - stats_num$sd
      thr_hi <- stats_num$mean + stats_num$sd
      cdf_lo <- tryCatch(do.call(p_fn, c(list(q = thr_lo), as.list(pv))), error = function(e) NA_real_)
      cdf_hi <- tryCatch(do.call(p_fn, c(list(q = thr_hi), as.list(pv))), error = function(e) NA_real_)
      list(lo = thr_lo, hi = thr_hi, plo = cdf_lo, phi = cdf_hi)
    } else NULL

    .stat_row <- function(label, value, fmt = "%.5g") {
      tags$tr(
        tags$td(style = "padding:5px 12px; color:#555; font-size:0.87rem;", label),
        tags$td(style = "padding:5px 12px; font-family:monospace; font-weight:600; font-size:0.87rem;",
                if (is.null(value) || !is.finite(value)) "N/A"
                else sprintf(fmt, value))
      )
    }

    tagList(
      # Moments
      tags$div(
        style = paste0(
          "background:#f8f9ff; border:1px solid #c8d6f0; border-radius:10px; ",
          "padding:12px 16px; margin-bottom:12px;"
        ),
        tags$div(
          style = "font-weight:700; color:#1e3a5f; font-size:0.88rem; margin-bottom:8px;",
          shiny::icon("sigma", class="me-1"), " Moments (numerical)"
        ),
        tags$table(
          style = "width:100%;",
          if (!is.null(stats_num)) tagList(
            .stat_row("Mean",             stats_num$mean),
            .stat_row("Variance",         stats_num$var),
            .stat_row("Std. Deviation",   stats_num$sd),
            .stat_row("Skewness",         stats_num$skewness),
            .stat_row("Excess Kurtosis",  stats_num$kurtosis - 3)
          ) else tags$tr(tags$td("Could not compute moments for these parameters."))
        )
      ),
      # Quantiles
      if (!is.null(quantiles_list)) tags$div(
        style = paste0(
          "background:#f8fff8; border:1px solid #b2dfbc; border-radius:10px; ",
          "padding:12px 16px; margin-bottom:12px;"
        ),
        tags$div(
          style = "font-weight:700; color:#155724; font-size:0.88rem; margin-bottom:8px;",
          shiny::icon("list-ol", class="me-1"), " Quantiles"
        ),
        tags$table(
          style = "width:100%;",
          lapply(seq_along(quantiles_list), function(i)
            .stat_row(names(quantiles_list)[i], quantiles_list[[i]]))
        )
      ) else NULL,
      # Tail probability note
      if (!is.null(tail_ps)) tags$div(
        style = "font-size:0.82rem; color:#555; margin-top:4px;",
        sprintf(
          "P(X ≤ Mean − 1 SD) = P(X ≤ %.4g) = %.4f  |  P(X ≤ Mean + 1 SD) = P(X ≤ %.4g) = %.4f",
          tail_ps$lo, tail_ps$plo, tail_ps$hi, tail_ps$phi
        )
      )
    )
  })

  # -- About panel ------------------------------------------------------------
  output$explore_about_ui <- shiny::renderUI({
    key   <- input$explore_dist
    shiny::req(nzchar(key %||% ""))
    entry <- tryCatch(get_dist_entry(key), error = function(e) NULL)
    shiny::req(!is.null(entry))

    pv    <- explore_params()
    param_str <- if (!anyNA(pv))
      paste0(names(pv), " = ", round(pv, 5), collapse = ", ")
    else "—"

    tagList(
      tags$div(
        style = paste0(
          "background:#fff; border:1px solid #dde5f0; border-radius:10px; ",
          "padding:16px 20px; margin-bottom:12px;"
        ),
        tags$h5(style = "color:#1e3a5f; font-weight:800; margin-bottom:6px;",
                entry$display_name),
        tags$p(style = "font-size:0.87rem; color:#444; margin-bottom:8px;",
               entry$description %||% ""),
        tags$hr(style = "margin:8px 0;"),
        tags$table(
          style = "width:100%; font-size:0.87rem;",
          tags$tr(
            tags$td(style="padding:4px 0; color:#888;", "Family"),
            tags$td(style="padding:4px 8px; font-weight:600;", entry$family %||% "—")
          ),
          tags$tr(
            tags$td(style="padding:4px 0; color:#888;", "Parameters"),
            tags$td(style="padding:4px 8px; font-family:monospace;", param_str)
          ),
          tags$tr(
            tags$td(style="padding:4px 0; color:#888;", "Support"),
            tags$td(style="padding:4px 8px;", entry$support %||% "—")
          ),
          tags$tr(
            tags$td(style="padding:4px 0; color:#888;", "MLE / MOM / LMOM"),
            tags$td(style="padding:4px 8px;",
                    paste(
                      c(if (isTRUE(entry$supports_mle))  "MLE",
                        if (isTRUE(entry$supports_mom))  "MOM",
                        if (isTRUE(entry$supports_lmom)) "LMOM"),
                      collapse = " / "
                    ))
          )
        ),
        if (!is.null(entry$formula_text) && nzchar(entry$formula_text)) tagList(
          tags$hr(style="margin:8px 0;"),
          tags$div(
            style = "font-size:0.84rem; color:#333; font-family:monospace; background:#f5f7fb; padding:6px 10px; border-radius:6px;",
            entry$formula_text
          )
        )
      )
    )
  })

}


# +===========================================================+
# |  Launch                                                   |
# +===========================================================+

shiny::shinyApp(ui = ui, server = server)
