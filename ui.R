# ── Shaw Industries CEO Strategy Portal ──────────────────────────────────────
# ui.R  |  Login gate + 6-module tabbed portal
# ─────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = shaw_theme,
  useShinyjs(),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=Playfair+Display:wght@400;600&",
                            "family=Barlow:wght@300;400;500&display=swap")),
    tags$link(rel = "stylesheet", href = "styles.css"),
    # Shaw blue overrides for accent color elements
    tags$style(HTML("
      .card-header-accent, .card-header-sm { color: #2b7bd6 !important; }
      .divider-accent { background: linear-gradient(90deg, rgba(43,123,214,0.6), transparent) !important; }
      .card-accent-border { border-left: 3px solid #2b7bd6 !important; }
      .badge-accent { background: rgba(43,123,214,0.15) !important; color: #2b7bd6 !important;
                      border: 1px solid rgba(43,123,214,0.35) !important; }
      .kpi-sub { border-top: 1px solid #192840 !important; }
      .btn-accent, .btn-accent:hover, .btn-accent:focus, .btn-accent:active {
        background: #2b7bd6 !important; color: #ffffff !important; }
      .progress-bar { background: #2b7bd6 !important; }
      .shaw-logo { filter: brightness(1.1); }
      .login-box { border-top: 3px solid #2b7bd6 !important; }
      .topbar { border-bottom: 2px solid #1a3a64 !important; }
      .shaw-wordmark { font-family: 'Playfair Display', serif; font-size: 18px;
                       letter-spacing: 0.25em; color: #4aa3e8; font-weight: 400; }
    "))
  ),
  
  # ── LOGIN SCREEN ───────────────────────────────────────────────────────────
  div(id = "login_screen",
      div(class = "login-wrapper",
          div(class = "login-box",
              
              # Shaw logo + wordmark
              div(style = "text-align:center; margin-bottom:28px;",
                  tags$div(style = "margin-bottom:12px;",
                           tags$img(
                             src    = SHAW_LOGO_URL,
                             height = "38px",
                             class  = "shaw-logo",
                             onerror = "this.style.display='none'; document.getElementById('shaw-fallback').style.display='block';"
                           ),
                           # Fallback wordmark if logo fails to load
                           tags$div(id = "shaw-fallback",
                                    style = "display:none;",
                                    tags$span(class = "shaw-wordmark", "SHAW")
                           )
                  ),
                  tags$p(style = paste0("font-size:9px; color:", PAL$muted,
                                        "; letter-spacing:0.3em; text-transform:uppercase; margin:0 0 8px;"),
                         "INDUSTRIES GROUP"),
                  tags$h1(style = paste0("font-size:20px; margin:0 0 6px; font-weight:normal;",
                                         " font-family:'Playfair Display',serif; color:", PAL$text, ";"),
                          "Strategy Intelligence Portal"),
                  tags$hr(style = paste0("border-color:", PAL$border, "; margin:16px 0 0;"))
              ),
              
              div(class = "mb-3",
                  tags$label("Username", class = "login-label"),
                  textInput("login_user", label = NULL, placeholder = "Username", width = "100%")
              ),
              div(class = "mb-4",
                  tags$label("Password", class = "login-label"),
                  passwordInput("login_pass", label = NULL, placeholder = "Password", width = "100%")
              ),
              
              div(id    = "login_error",
                  style = "display:none; color:#d95f5f; font-size:12px;",
                  class = "text-center mb-3",
                  "Invalid credentials."),
              
              actionButton("login_btn", "Access Portal", width = "100%", class = "btn btn-accent"),
              
              tags$hr(style = paste0("border-color:", PAL$border, "; margin:18px 0 12px;")),
              
              # Data source status
              uiOutput("login_api_status"),
              
              # Limited use notice
              div(style = paste0("margin-top:12px; padding:10px 14px; background:rgba(43,123,214,0.07);",
                                 " border:1px solid rgba(43,123,214,0.2); border-radius:3px;"),
                  tags$p(style = paste0("font-size:10px; color:", PAL$muted, "; margin:0; line-height:1.6;"),
                         tags$span(style = paste0("color:", PAL$blue, ";"), "\u2139 Limited demo access. "),
                         "This portal is provided for exploratory and analytical use only. ",
                         "All Shaw revenue, margin, and financial estimates are analyst-derived — not reported figures. ",
                         "Not for distribution."
                  )
              ),
              
              tags$p(style = paste0("font-size:10px; text-align:center; color:", PAL$muted,
                                    "; margin-top:10px;"),
                     "Access: demo / demo")
          )
      )
  ),
  
  # ── MAIN PORTAL ────────────────────────────────────────────────────────────
  hidden(div(id = "main_portal",
             
             # ── Top Bar ──────────────────────────────────────────────────────────────
             div(class = "topbar",
                 div(style = "display:flex; align-items:center; justify-content:space-between;",
                     
                     div(style = "display:flex; align-items:center; gap:16px;",
                         # Shaw logo in topbar
                         div(style = "display:flex; align-items:center; gap:10px;",
                             tags$img(
                               src    = SHAW_LOGO_URL,
                               height = "24px",
                               class  = "shaw-logo",
                               onerror = "this.style.display='none'; document.getElementById('topbar-fallback').style.display='inline';"
                             ),
                             tags$span(id = "topbar-fallback",
                                       style = "display:none;",
                                       tags$span(class = "shaw-wordmark", style = "font-size:15px;", "SHAW")
                             ),
                             div(
                               tags$p(style = paste0("font-size:8px; color:", PAL$muted,
                                                     "; letter-spacing:0.25em; text-transform:uppercase; margin:0;"),
                                      "INDUSTRIES GROUP"),
                               tags$p(style = paste0("font-size:12px; font-family:'Playfair Display',serif;",
                                                     " margin:0; color:", PAL$blue, ";"),
                                      "Strategy Intelligence Portal")
                             )
                         ),
                         tags$div(style = paste0("height:28px; width:1px; background:", PAL$border, ";")),
                         # Division toggles
                         div(style = "display:flex; gap:4px;",
                             lapply(c("Residential","Commercial","Turf"), function(d) {
                               tags$button(d,
                                           style  = "background:transparent; border:1px solid transparent;
                          border-radius:2px; padding:4px 12px; cursor:pointer;
                          font-size:11px; letter-spacing:0.06em; color:#52637a;",
                                           class  = "division-btn",
                                           id     = paste0("div_btn_", tolower(d)),
                                           onclick = paste0("Shiny.setInputValue('active_division','", d,
                                                            "',{priority:'event'})")
                               )
                             })
                         )
                     ),
                     
                     div(style = "display:flex; align-items:center; gap:16px;",
                         # Q1 2026 badge
                         tags$span(style = paste0("font-size:10px; color:", PAL$muted,
                                                  "; border:1px solid ", PAL$border,
                                                  "; border-radius:2px; padding:3px 8px;"),
                                   "Q1 2026"),
                         uiOutput("topbar_status"),
                         actionButton("signout_btn", "Sign Out",
                                      style = paste0("background:transparent; border:1px solid ",
                                                     PAL$border, "; color:", PAL$muted,
                                                     "; font-size:10px; padding:4px 12px;",
                                                     " letter-spacing:0.1em; text-transform:uppercase;"))
                     )
                 )
             ),
             
             # ── Division Context Banner ───────────────────────────────────────────────
             div(class = "division-banner",
                 uiOutput("division_banner")
             ),
             
             # ── Navigation & Content ──────────────────────────────────────────────────
             div(style = "padding:0 24px;",
                 navset_tab(
                   id = "main_tabs",
                   
                   # ── 1. Executive Summary ──────────────────────────────────────────────
                   nav_panel("Executive Summary",
                             div(class = "tab-content-pad",
                                 uiOutput("exec_header"),
                                 uiOutput("exec_kpi_row"),
                                 tags$br(),
                                 layout_columns(col_widths = c(8, 4),
                                                card(
                                                  card_header(uiOutput("exec_primary_chart_title"), class = "card-header-sm"),
                                                  plotlyOutput("exec_rev_housing", height = "240px"),
                                                  uiOutput("exec_rev_insight"),
                                                  uiOutput("exec_rev_note")
                                                ),
                                                card(
                                                  card_header(uiOutput("exec_secondary_chart_title"), class = "card-header-sm"),
                                                  plotlyOutput("exec_input_costs", height = "240px"),
                                                  uiOutput("exec_cost_insight")
                                                ),
                                                # ── Add this card block in the Executive tab, after the two line charts ──────
                                                card(
                                                  card_header(
                                                    div(
                                                      style = "display:flex; align-items:center; gap:10px;",
                                                      "U.S. Flooring Market Structure \u2014 Marimekko (2026E)",
                                                      tags$span(class = "badge-accent", "Mekko"),
                                                      tags$span(
                                                        style = "font-size:10px; color:#5a6070; font-weight:normal;",
                                                        "Width = market size ($B) \u2022 Height = competitive share within category"
                                                      )
                                                    ),
                                                    class = "card-header-sm"
                                                  ),
                                                  plotlyOutput("exec_mekko", height = "320px"),
                                                  uiOutput("exec_mekko_note")
                                                )
                                 ),
                                 tags$br(),
                                 layout_columns(col_widths = c(6, 6),
                                                card(
                                                  card_header("Competitor Gross Margin Benchmarking — Latest Available 10-K",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("exec_margins", height = "200px"),
                                                  uiOutput("exec_margins_note")
                                                ),
                                                card(
                                                  card_header("Floor Category Share Shift — Floor Covering Weekly Estimates",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("exec_cat_share", height = "200px")
                                                )
                                 ),
                                 tags$br(),
                                 card(class = "card-accent-border",
                                      card_header("Strategic Narrative — Q1 2026", class = "card-header-accent"),
                                      uiOutput("exec_narrative")
                                 )
                             )
                   ),
                   
                   # ── 2. Porter's Five Forces ───────────────────────────────────────────
                   nav_panel("Porter's Five Forces",
                             div(class = "tab-content-pad",
                                 uiOutput("porter_header"),
                                 uiOutput("porter_division_note"),
                                 layout_columns(col_widths = c(4, 8),
                                                card(
                                                  card_header("Force Intensity — Analyst Scoring (0 = no pressure \u2192 100 = extreme)",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("porter_radar", height = "300px"),
                                                  tags$p(style = paste0("font-size:10px; color:", PAL$muted,
                                                                        "; margin:8px 0 0; line-height:1.5;"),
                                                         tags$strong(style = paste0("color:", PAL$accent, ";"), "Blue = Shaw. "),
                                                         "Higher score = more competitive pressure (unfavorable for Shaw). ",
                                                         "Score derivation in 'Score Methodology' panel below."
                                                  )
                                                ),
                                                card(
                                                  card_header("Select Force for Deep Analysis", class = "card-header-sm"),
                                                  uiOutput("porter_force_selector")
                                                )
                                 ),
                                 tags$br(),
                                 card(
                                   card_header(uiOutput("porter_active_header"), class = "card-header-sm"),
                                   uiOutput("porter_deep_dive")
                                 ),
                                 tags$br(),
                                 card(
                                   card_header(
                                     div(style = "display:flex; align-items:center; gap:10px;",
                                         tags$span(class = "card-header-sm",
                                                   "Score Methodology — Full Derivation for Selected Force"),
                                         tags$span(class = "badge-muted", "How is each score calculated?")
                                     ),
                                     class = "card-header-sm"
                                   ),
                                   div(style = paste0("padding:12px 0 16px; border-bottom:1px solid ", PAL$border,
                                                      "; margin-bottom:16px;"),
                                       tags$p(style = paste0("font-size:12px; color:", PAL$muted, "; line-height:1.7; margin:0;"),
                                              tags$strong(style = paste0("color:", PAL$text, ";"), "Scoring system: "),
                                              "All Porter's scores are on a 0–100 scale: 0 = no competitive pressure, ",
                                              "100 = maximum pressure (worst for Shaw). ",
                                              "Scores are analyst qualitative judgements — not statistical outputs. ",
                                              "Each score uses a documented baseline + explicit adjustments, each tied to a verifiable data source. ",
                                              tags$strong(style = paste0("color:", PAL$amber, ";"),
                                                          "Scores should be reviewed when the listed Review Trigger conditions are met.")
                                       )
                                   ),
                                   uiOutput("porter_methodology")
                                 )
                             )
                   ),
                   
                   # ── 3. SWOT Engine ────────────────────────────────────────────────────
                   nav_panel("SWOT Engine",
                             div(class = "tab-content-pad",
                                 uiOutput("swot_header"),
                                 uiOutput("swot_quadrant_row"),
                                 tags$br(),
                                 uiOutput("swot_items")
                             )
                   ),
                   
                   # ── 4. Assumption Monitor ─────────────────────────────────────────────
                   nav_panel("Assumption Monitor",
                             div(class = "tab-content-pad",
                                 uiOutput("assump_header"),
                                 uiOutput("assump_summary_cards"),
                                 tags$br(),
                                 div(style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:14px;",
                                     uiOutput("assump_cat_filters")
                                 ),
                                 card(DTOutput("assump_table")),
                                 tags$br(),
                                 card(class = "card-accent-border", uiOutput("assump_directive"))
                             )
                   ),
                   
                   # ── 5. Scenario Modeling ──────────────────────────────────────────────
                   nav_panel("Scenario Modeling",
                             div(class = "tab-content-pad",
                                 uiOutput("scenario_header"),
                                 uiOutput("scenario_selector"),
                                 tags$br(),
                                 uiOutput("scenario_desc_box"),
                                 tags$br(),
                                 layout_columns(col_widths = c(8, 4),
                                                card(
                                                  card_header("Revenue Trajectory ($M) — Analyst Estimate", class = "card-header-sm"),
                                                  plotlyOutput("scenario_rev_chart", height = "250px"),
                                                  uiOutput("scenario_rev_note")
                                                ),
                                                card(
                                                  card_header("EBITDA ($M) — Analyst Estimate", class = "card-header-sm"),
                                                  plotlyOutput("scenario_ebitda_chart", height = "250px")
                                                )
                                 ),
                                 tags$br(),
                                 card(
                                   card_header("2024A–2028E Financial Model — All figures analyst estimates",
                                               class = "card-header-sm"),
                                   DTOutput("scenario_table")
                                 ),
                                 tags$br(),
                                 card(
                                   card_header("Leading Indicators — Variables That Move 6–18 Months Before Revenue",
                                               class = "card-header-sm"),
                                   uiOutput("leading_indicators")
                                 )
                             )
                   ),
                   
                   # ── 6. Fact Base ──────────────────────────────────────────────────────
                   nav_panel("Fact Base",
                             div(class = "tab-content-pad",
                                 uiOutput("factbase_header"),
                                 uiOutput("source_card_row"),
                                 tags$br(),
                                 div(style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:14px;",
                                     uiOutput("factbase_cat_filters")
                                 ),
                                 card(DTOutput("factbase_table")),
                                 tags$br(),
                                 layout_columns(col_widths = c(6, 6),
                                                card(
                                                  card_header("Macro Signal Heatmap — Q1 2026", class = "card-header-sm"),
                                                  uiOutput("macro_heatmap")
                                                ),
                                                card(
                                                  card_header("Competitive Intelligence — Public Filings & Trade Press",
                                                              class = "card-header-sm"),
                                                  uiOutput("competitive_intel")
                                                )
                                 )
                             )
                   )
                 )
             ),
             
             # ── Footer ────────────────────────────────────────────────────────────────
             div(class = "portal-footer",
                 div(style = "display:flex; justify-content:space-between; align-items:center;",
                     # Shaw logo small in footer
                     div(style = "display:flex; align-items:center; gap:8px;",
                         tags$img(src = SHAW_LOGO_URL, height = "16px", class = "shaw-logo",
                                  onerror = "this.style.display='none'"),
                         tags$p(class = "mb-0",
                                style = "font-size:10px; color:#2a3a50; letter-spacing:0.1em;",
                                "SHAW INDUSTRIES GROUP, INC. \u2014 CONFIDENTIAL \u2014 LIMITED DEMO ACCESS")
                     ),
                     tags$p(class = "mb-0", style = "font-size:10px; color:#2a3a50;",
                            "FRED \u00b7 tidyquant/Yahoo Finance \u00b7 U.S. Census \u00b7 SEC Filings \u00b7 Trade Press \u00b7 Analyst Estimates")
                 )
             )
  ))
)