# ── Shaw Industries CEO Strategy Portal ──────────────────────────────────────
# ui.R  |  Login gate + 6-module tabbed portal
#
# CHANGES vs. original:
#  ADD: "Score Methodology" expandable card in Porter's Five Forces tab.
#       Shows derivation, data sources, and review triggers for the selected force.
# ─────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = shaw_theme,
  useShinyjs(),
  tags$head(
    tags$link(rel  = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel  = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = NA),
    tags$link(rel  = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=Playfair+Display:wght@400;600&",
                            "family=Barlow:wght@300;400;500&display=swap")),
    tags$link(rel  = "stylesheet", href = "styles.css")
  ),
  
  # ── LOGIN SCREEN ───────────────────────────────────────────────────────────
  div(id = "login_screen",
      div(class = "login-wrapper",
          div(class = "login-box",
              
              div(style = "text-align:center; margin-bottom:32px;",
                  tags$p(style = paste0("font-size:9px; color:", PAL$muted,
                                        "; letter-spacing:0.3em; text-transform:uppercase; margin-bottom:12px;"),
                         "SHAW INDUSTRIES GROUP"),
                  tags$h1(style = "font-size:22px; margin:0 0 6px; font-weight:normal;",
                          "CEO Strategy Portal"),
                  tags$p(style = paste0("font-size:12px; color:", PAL$muted, "; margin:0;"),
                         "Restricted Access — ELT Only"),
                  tags$hr(style = "border-color:#1e2530; margin:20px 0 0;")
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
              
              tags$hr(style = "border-color:#1e2530; margin:20px 0;"),
              uiOutput("login_api_status"),
              tags$p(style = paste0("font-size:10px; text-align:center; color:", PAL$muted,
                                    "; margin-top:8px;"),
                     markdown("username: *demo* "),
                     markdown("password: *demo* "),
                     markdown("**NOTE: This password is for temporary acccess only and will expire soon.**"))
          )
      )
  ),
  
  # ── MAIN PORTAL ────────────────────────────────────────────────────────────
  hidden(div(id = "main_portal",
             
             # ── Top Bar ──────────────────────────────────────────────────────────────
             div(class = "topbar",
                 div(style = "display:flex; align-items:center; justify-content:space-between;",
                     
                     div(style = "display:flex; align-items:center; gap:20px;",
                         div(
                           tags$p(style = paste0("font-size:9px; color:", PAL$muted,
                                                 "; letter-spacing:0.25em; text-transform:uppercase; margin:0;"),
                                  "SHAW INDUSTRIES GROUP"),
                           tags$p(style = "font-size:14px; font-family:'Playfair Display',serif; margin:0;",
                                  "CEO Strategy Portal")
                         ),
                         tags$div(style = "height:30px; width:1px; background:#1e2530;"),
                         div(style = "display:flex; gap:4px;",
                             lapply(c("Residential","Commercial","Turf"), function(d) {
                               tags$button(d,
                                           style  = "background:transparent; border:1px solid transparent;
                          border-radius:2px; padding:4px 12px; cursor:pointer;
                          font-size:11px; letter-spacing:0.06em;",
                                           class  = "division-btn",
                                           id     = paste0("div_btn_", tolower(d)),
                                           onclick = paste0("Shiny.setInputValue('active_division','", d,
                                                            "',{priority:'event'})")
                               )
                             })
                         )
                     ),
                     
                     div(style = "display:flex; align-items:center; gap:16px;",
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
                                                  card_header("Revenue Proxy vs. Housing Starts (r ≈ 0.87 — modeled correlation)",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("exec_rev_housing", height = "240px"),
                                                  uiOutput("exec_rev_insight"),
                                                  uiOutput("exec_rev_note")
                                                ),
                                                card(
                                                  card_header("Input Costs & Rate Environment", class = "card-header-sm"),
                                                  plotlyOutput("exec_input_costs", height = "240px"),
                                                  uiOutput("exec_cost_insight")
                                                )
                                 ),
                                 tags$br(),
                                 layout_columns(col_widths = c(6, 6),
                                                card(
                                                  card_header("Competitor Revenue Benchmarking — Public Filings (2023)",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("exec_margins", height = "200px"),
                                                  uiOutput("exec_margins_note")
                                                ),
                                                card(
                                                  card_header("Floor Category Share Shift — Trade Press Estimates",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("exec_cat_share", height = "200px")
                                                )
                                 ),
                                 tags$br(),
                                 card(class = "card-accent-border",
                                      card_header("Strategic Narrative — Q1 2025", class = "card-header-accent"),
                                      uiOutput("exec_narrative")
                                 )
                             )
                   ),
                   
                   # ── 2. Porter's Five Forces ───────────────────────────────────────────
                   nav_panel("Porter's Five Forces",
                             div(class = "tab-content-pad",
                                 uiOutput("porter_header"),
                                 layout_columns(col_widths = c(4, 8),
                                                card(
                                                  card_header("Force Intensity — Analyst Scoring (0 = no pressure → 100 = extreme)",
                                                              class = "card-header-sm"),
                                                  plotlyOutput("porter_radar", height = "300px"),
                                                  # ADD: inline legend note
                                                  tags$p(style = paste0("font-size:10px; color:", PAL$muted,
                                                                        "; margin:8px 0 0; line-height:1.5;"),
                                                         tags$strong(style = paste0("color:", PAL$accent, ";"), "Gold = Shaw. "),
                                                         tags$strong(style = paste0("color:", PAL$blue, ";"), "Blue dashed = mature U.S. mfg. benchmark. "),
                                                         "Higher score = more competitive pressure (unfavorable). ",
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
                                 # ADD: Scoring methodology card — full transparency panel
                                 card(
                                   card_header(
                                     div(style = "display:flex; align-items:center; gap:10px;",
                                         tags$span(class = "card-header-sm",
                                                   "Score Methodology — Full Derivation for Selected Force"),
                                         tags$span(class = "badge-muted", "How is this score calculated?")
                                     ),
                                     class = "card-header-sm"
                                   ),
                                   # Intro explanation of the scoring system
                                   div(style = paste0("padding:12px 0 16px; border-bottom:1px solid ", PAL$border,
                                                      "; margin-bottom:16px;"),
                                       tags$p(style = paste0("font-size:12px; color:", PAL$muted, "; line-height:1.7; margin:0;"),
                                              tags$strong(style = paste0("color:", PAL$text, ";"), "Scoring system: "),
                                              "All Porter's scores are on a 0–100 scale where 0 = no competitive pressure and ",
                                              "100 = maximum competitive pressure (worst for Shaw). ",
                                              "Scores are analyst qualitative judgements — not statistical outputs. ",
                                              "Each score is built from a baseline starting point with explicit additive and subtractive ",
                                              "adjustments, each tied to a verifiable data source. The final score is the sum of the base ",
                                              "and all adjustments. Shaw's scores are compared against a mature U.S. industrial ",
                                              "manufacturing benchmark (composite of analyst estimates for comparable sectors). ",
                                              tags$strong(style = paste0("color:", PAL$amber, ";"),
                                                          "Scores should be reviewed when the Review Trigger conditions are met.")
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
                                   card_header("3-Year Financial Model — Analyst Estimates (Shaw does not disclose financials)",
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
                                                  card_header("Macro Signal Heatmap", class = "card-header-sm"),
                                                  uiOutput("macro_heatmap")
                                                ),
                                                card(
                                                  card_header(
                                                    div(style = "display:flex; align-items:center; gap:8px;",
                                                        "Competitive Intelligence — Public Filings & Trade Press",
                                                        tags$span(class = "badge-amber", "Shaw \u2260 Engineered Floors")
                                                    ),
                                                    class = "card-header-sm"
                                                  ),
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
                     tags$p(class = "mb-0",
                            style = "font-size:10px; color:#3a3830; letter-spacing:0.1em;",
                            "SHAW INDUSTRIES GROUP, INC. — CONFIDENTIAL ELT DOCUMENT"),
                     tags$p(class = "mb-0", style = "font-size:10px; color:#3a3830;",
                            "Sources: FRED · tidyquant/Yahoo Finance · U.S. Census · Public SEC Filings · Trade Press · Analyst Estimates")
                 )
             )
  ))
)