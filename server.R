# ── Shaw Industries CEO Strategy Portal ──────────────────────────────────────
# server.R  |  Auth · FRED fetch · Equity fetch · All 6 module outputs
#
# FIXES APPLIED vs. original:
#  FIX 1: updatePasswordInput() does not exist in base Shiny.
#          Replaced with shinyjs::reset() on the login form.
#  FIX 2: kpi_card NA crash — if FRED data has NAs in tail rows, inline if()
#          statements like `if (hs > 1400)` throw "missing value where TRUE/FALSE
#          needed". All such conditions now use isTRUE() or explicit is.na() guards.
#  FIX 3: Plotly scatterpolar radar warnings — mode must be set explicitly to
#          "lines" on scatterpolar traces; "markers" was being inferred by default.
#  FIX 4: Engineered Floors correctly identified as SEPARATE company (not Shaw-aligned).
#  ADD:   Live equity data fetch via tidyquant (MHK, TILE, AWI) on login.
#  ADD:   Porter scoring methodology output for transparency panel.
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Reactive State ──────────────────────────────────────────────────────────
  rv <- reactiveValues(
    logged_in    = FALSE,
    role         = NULL,
    macro        = NULL,
    equity       = NULL,       # ADD: live equity data from tidyquant
    using_live   = FALSE,
    using_equity = FALSE,      # ADD: TRUE if tidyquant fetch succeeded
    porter_force = "rivalry",
    swot_quad    = "S",
    scenario_sel = "base",
    assump_cat   = "ALL",
    fb_cat       = "ALL",
    division     = "Residential"
  )
  
  # ── Login: show API/package status ─────────────────────────────────────────
  output$login_api_status <- renderUI({
    has_fred   <- nchar(FRED_API_KEY) > 0
    has_tq     <- TIDYQUANT_AVAILABLE
    
    fred_col  <- if (has_fred) PAL$green else PAL$amber
    fred_icon <- if (has_fred) "\u25cf" else "\u25cb"
    fred_msg  <- if (has_fred) "FRED key in .Renviron — live macro data"
    else          "No FRED_API_KEY — demo mode (mock data)"
    
    tq_col   <- if (has_tq) PAL$green else PAL$amber
    tq_icon  <- if (has_tq) "\u25cf" else "\u25cb"
    tq_msg   <- if (has_tq) "tidyquant installed — live MHK/TILE/AWI quotes"
    else         "tidyquant not installed — static equity fallback"
    
    tagList(
      tags$p(style = paste0("font-size:11px; text-align:center; color:", fred_col, "; margin:0;"),
             fred_icon, " ", fred_msg),
      tags$p(style = paste0("font-size:11px; text-align:center; color:", tq_col, "; margin:4px 0 0;"),
             tq_icon, " ", tq_msg)
    )
  })
  
  # ── Authentication ──────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    user  <- trimws(input$login_user)
    pass  <- trimws(input$login_pass)
    match <- Filter(function(u) u$user == user && u$pass == pass, VALID_USERS)
    
    if (length(match) > 0) {
      rv$role <- match[[1]]$role
      
      # ── Fetch FRED macro data ───────────────────────────────────────────────
      if (nchar(FRED_API_KEY) > 0) {
        withProgress(message = "Fetching live FRED data\u2026", value = 0.1, {
          live <- fetch_fred_data(start = "2019-01-01")
          incProgress(0.5)
          
          if (!is.null(live) && nrow(live) > 4 &&
              !all(is.na(live$housing_starts))) {
            live <- live |>
              mutate(
                label = paste0("Q", quarter(date), "'",
                               substr(as.character(year(date)), 3, 4))
              )
            live <- left_join(live,
                              MOCK_MACRO |> select(date, shaw_rev_est),
                              by = "date")
            rv$macro     <- live
            rv$using_live <- TRUE
          } else {
            rv$macro     <- MOCK_MACRO
            rv$using_live <- FALSE
          }
          incProgress(0.2)
        })
      } else {
        rv$macro     <- MOCK_MACRO
        rv$using_live <- FALSE
      }
      
      # ── ADD: Fetch equity data via tidyquant ────────────────────────────────
      # Fetches MHK, TILE, AWI from Yahoo Finance via the tidyquant package API.
      # Non-blocking: failure falls back to FACT_BASE_STATIC values gracefully.
      if (TIDYQUANT_AVAILABLE) {
        withProgress(message = "Fetching equity data (MHK, TILE, AWI)\u2026", value = 0.1, {
          eq <- fetch_equity_data(tickers = c("MHK", "TILE", "AWI"),
                                  lookback_days = 365)
          incProgress(0.8)
          if (!is.null(eq) && length(eq) > 0) {
            rv$equity       <- eq
            rv$using_equity <- TRUE
          } else {
            rv$using_equity <- FALSE
          }
        })
      }
      
      rv$logged_in <- TRUE
      shinyjs::hide("login_screen")
      shinyjs::show("main_portal")
      shinyjs::runjs("
        document.querySelectorAll('.division-btn').forEach(b => {
          b.style.color='#5a6070'; b.style.borderColor='transparent'; b.style.background='transparent';
        });
        var first = document.getElementById('div_btn_residential');
        if(first){ first.style.color='#c8a84b'; first.style.borderColor='#c8a84b';
                   first.style.background='rgba(200,168,75,0.12)'; }
      ")
      
    } else {
      shinyjs::runjs(
        "document.getElementById('login_error').style.display='block';"
      )
    }
  })
  
  observeEvent(input$login_pass, {
    shinyjs::runjs("
      document.getElementById('login_pass').onkeydown = function(e){
        if(e.key==='Enter') document.getElementById('login_btn').click();
      };
    ")
  })
  
  # FIX 1: updatePasswordInput() does not exist in base Shiny.
  # Original code: updatePasswordInput(session, "login_pass", value = "")
  # Fix: use shinyjs::reset() on the whole form, or updateTextInput for the username.
  # passwordInput fields can be reset with shinyjs::reset() on the input ID directly.
  observeEvent(input$signout_btn, {
    rv$logged_in  <- FALSE
    rv$using_live <- FALSE
    rv$using_equity <- FALSE
    rv$macro      <- NULL
    rv$equity     <- NULL
    shinyjs::show("login_screen")
    shinyjs::hide("main_portal")
    updateTextInput(session, "login_user", value = "")
    shinyjs::reset("login_pass")   # FIX: shinyjs::reset() works on passwordInput
  })
  
  # ── Division Toggle ─────────────────────────────────────────────────────────
  observeEvent(input$active_division, {
    d <- input$active_division
    rv$division <- d
    shinyjs::runjs(sprintf("
      document.querySelectorAll('.division-btn').forEach(b => {
        b.style.color='#5a6070'; b.style.borderColor='transparent';
        b.style.background='transparent';
      });
      var el = document.getElementById('div_btn_%s');
      if(el){ el.style.color='#c8a84b'; el.style.borderColor='#c8a84b';
              el.style.background='rgba(200,168,75,0.12)'; }
    ", tolower(d)))
  })
  
  macro <- reactive({
    req(rv$macro)
    rv$macro
  })
  
  # ── Shared Outputs ──────────────────────────────────────────────────────────
  output$topbar_status <- renderUI({
    req(rv$logged_in)
    col <- if (rv$using_live) PAL$green else PAL$amber
    msg <- if (rv$using_live)
      paste0("Live FRED — last pulled ", format(Sys.time(), "%b %d %H:%M"))
    else
      "Demo mode — mock data (set FRED_API_KEY in .Renviron)"
    eq_msg <- if (rv$using_equity) " | Equity: live" else " | Equity: static"
    div(style = "display:flex; align-items:center; gap:6px;",
        tags$span(style = paste0("width:7px;height:7px;border-radius:50%;",
                                 "background:", col, ";display:inline-block;")),
        tags$span(style = paste0("font-size:10px; color:", PAL$muted, ";"),
                  paste0(msg, eq_msg))
    )
  })
  
  output$division_banner <- renderUI({
    req(rv$logged_in)
    d <- rv$division
    desc <- switch(d,
                   Residential = "Shaw Floors · Anderson Tuftex · COREtec — Housing starts-driven demand (FRED HOUST)",
                   Commercial  = "Patcraft · Shaw Contract · Philadelphia — Lagging ABI / nonresidential construction cycle",
                   Turf        = "Shaw Sports Turf · Southwest Greens · Shawgrass — Infrastructure and long-cycle retrofit"
    )
    div(style = "display:flex; align-items:center; gap:12px;",
        tags$span(class = "badge-accent", d),
        tags$span(style = paste0("font-size:11px; color:", PAL$muted, ";"), desc)
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 1 — EXECUTIVE SUMMARY
  # ════════════════════════════════════════════════════════════════════════════
  
  output$exec_header <- renderUI({
    section_header(
      "Executive Summary",
      subtitle = paste0(
        "What the CEO sees in 10 minutes. Macro environment, competitive position,",
        " and strategic momentum from FRED, BLS, and public filings."
      ),
      badge_text  = if (rv$using_live) "Live FRED" else "Mock Data",
      badge_color = if (rv$using_live) "green" else "amber"
    )
  })
  
  # FIX 2: NA guard on all inline if() conditions in exec_kpi_row.
  # Original code used bare `if (hs > 1400)` which throws "missing value where
  # TRUE/FALSE needed" if hs is NA (e.g., last FRED row has NA housing_starts).
  # Fix: use isTRUE(x > threshold) which safely returns FALSE on NA/NULL.
  output$exec_kpi_row <- renderUI({
    d <- macro()
    
    # Pull latest non-NA values for each series
    hs      <- suppressWarnings(as.numeric(tail(d$housing_starts[!is.na(d$housing_starts)], 1)))
    ff      <- suppressWarnings(as.numeric(tail(d$fed_funds[!is.na(d$fed_funds)], 1)))
    mort    <- suppressWarnings(as.numeric(tail(d$mortgage_30[!is.na(d$mortgage_30)], 1)))
    ppi     <- suppressWarnings(as.numeric(tail(d$ppi_plastics[!is.na(d$ppi_plastics)], 1)))
    fr      <- suppressWarnings(as.numeric(tail(d$freight_ppi[!is.na(d$freight_ppi)], 1)))
    
    # YoY PPI change — guard against < 5 rows
    non_na_ppi <- d$ppi_plastics[!is.na(d$ppi_plastics)]
    prev_ppi   <- if (length(non_na_ppi) >= 5) non_na_ppi[length(non_na_ppi) - 4] else NA_real_
    ppi_yoy    <- if (!is.na(ppi) && !is.na(prev_ppi) && prev_ppi != 0)
      round((ppi / prev_ppi - 1) * 100, 1)
    else NA_real_
    
    # FIX: safe color selection using isTRUE() — never crashes on NA
    hs_col    <- if (isTRUE(hs > 1400))   PAL$green else PAL$amber
    ppi_col   <- if (isTRUE(abs(ppi_yoy) < 6)) PAL$green else PAL$red
    ppi_delta <- if (!is.na(ppi_yoy))
      paste0(ifelse(ppi_yoy >= 0, "+", ""), ppi_yoy, "% YoY")
    else "YoY N/A"
    
    div(class = "kpi-row",
        kpi_card("Shaw Rev. Est. \u2605", "$6.2B est.",
                 "\u2191 +1.3% est. YoY", PAL$green,
                 "\u26a0 Analyst estimate — not reported"),
        kpi_card("Housing Starts (HOUST)",
                 if (!is.na(hs)) paste0(format(round(hs), big.mark = ","), "k") else "N/A",
                 if (!is.na(hs)) paste0(ifelse(hs > 1400, "\u2191", "\u2193"), " ",
                                        round(hs), "k SAAR") else "—",
                 hs_col,
                 "FRED HOUST — latest quarter"),
        kpi_card("Fed Funds Rate",
                 if (!is.na(ff)) paste0(round(ff, 2), "%") else "N/A",
                 "\u2193 Declining from 5.33% peak", PAL$green,
                 "FRED FEDFUNDS"),
        kpi_card("30-Yr Mortgage Rate",
                 if (!is.na(mort)) paste0(round(mort, 2), "%") else "N/A",
                 "Elevated vs. 2.77% (2020 low)", PAL$amber,
                 "FRED MORTGAGE30US"),
        kpi_card("PPI: Plastics (WPU0672)",
                 if (!is.na(ppi)) paste0(round(ppi, 1), " idx") else "N/A",
                 ppi_delta,
                 ppi_col,
                 "FRED WPU0672"),
        kpi_card("Freight PPI",
                 if (!is.na(fr)) paste0(round(fr, 1), " idx") else "N/A",
                 "\u2193 From 175 peak (2022)", PAL$green,
                 "FRED PCU484121484121")
    )
  })
  
  output$exec_rev_housing <- renderPlotly({
    d <- macro()
    plot_ly(d, x = ~label) |>
      add_lines(y = ~shaw_rev_est, name = "Shaw Rev Est. ($M) \u2605",
                line = list(color = PAL$accent, width = 2.5)) |>
      add_lines(y = ~(housing_starts * 3.8),
                name = "Housing Starts \u00d73.8 (scaled)",
                line = list(color = PAL$blue, width = 1.5, dash = "dash")) |>
      plotly_dark(xlab = "", ylab = "$M / Scaled")
  })
  
  output$exec_rev_insight <- renderUI({
    insight_box(
      "Revenue proxy tracks housing starts with approximately a 1-quarter lag. The Q3 2022 – Q1 2023 housing decline correlates with the revenue compression. Recovery since Q3 2023 is gaining momentum.",
      "accent", "Modeled Correlation"
    )
  })
  
  output$exec_rev_note <- renderUI({
    data_note_ui(paste0(
      "Shaw revenue (\u2605) is an analyst estimate derived from Berkshire Hathaway ",
      "segment commentary, Mohawk Industries (MHK) public revenue as industry proxy, ",
      "and housing starts correlation. Shaw Industries does not report standalone financials."
    ))
  })
  
  output$exec_input_costs <- renderPlotly({
    d <- macro()
    plot_ly(d, x = ~label) |>
      add_lines(y = ~ppi_plastics, name = "PPI Plastics (WPU0672)",
                line = list(color = PAL$red, width = 2)) |>
      add_lines(y = ~freight_ppi, name = "Freight PPI",
                line = list(color = PAL$amber, width = 2)) |>
      add_lines(y = ~(fed_funds * 10), name = "Fed Funds \u00d710 (scaled)",
                line = list(color = PAL$blue, width = 1.5, dash = "dot")) |>
      plotly_dark(xlab = "", ylab = "Index / Rate (scaled)")
  })
  
  output$exec_cost_insight <- renderUI({
    insight_box(
      "PPI Plastics (FRED WPU0672) and freight PPI both peaked in 2022 and have substantially normalized. Fed funds rate declining from 5.33% peak. Input cost tailwind is real entering 2025.",
      "green", "Positive Signal"
    )
  })
  
  output$exec_margins <- renderPlotly({
    d <- COMPETITORS |> filter(!is.na(gm_pct))
    plot_ly(d,
            x = ~gm_pct, y = ~reorder(company, gm_pct),
            type = "bar", orientation = "h",
            marker = list(color = ~color),
            text  = ~paste0(gm_pct, "%  (", source, ")"),
            textposition = "outside",
            hovertemplate = "%{y}: %{x:.1f}%<extra></extra>") |>
      plotly_dark(xlab = "Gross Margin %", ylab = "", legend = FALSE) |>
      layout(xaxis = list(range = c(0, 48)))
  })
  
  output$exec_margins_note <- renderUI({
    data_note_ui(
      "Shaw gross margin not publicly reported. Engineered Floors is a separate private company — no public financials. Public figures from MHK, AWI, TILE annual 10-Ks. Note: Interface (TILE) is commercial modular carpet — margin reflects different product mix."
    )
  })
  
  output$exec_cat_share <- renderPlotly({
    p <- plot_ly(FLOOR_SHARE, x = ~year)
    cols <- c(lvt_spc = PAL$blue, hardwood = PAL$green,
              tile = PAL$purple, carpet = PAL$amber, other = PAL$muted)
    for (cat in c("other","tile","hardwood","carpet","lvt_spc")) {
      nm <- toupper(gsub("_spc","", gsub("lvt_spc","LVT/SPC", cat)))
      p  <- p |> add_trace(
        y = as.formula(paste0("~", cat)),
        name = nm, type = "scatter", mode = "none",
        stackgroup = "one",
        fillcolor = cols[[cat]],
        line = list(color = cols[[cat]])
      )
    }
    p |> plotly_dark(xlab = "", ylab = "Share (%)")
  })
  
  output$exec_narrative <- renderUI({
    items <- list(
      list(lbl = "What Changed?",
           txt = "Housing recovery is strengthening from 2023 trough. FEDFUNDS declining. PPI Plastics and freight costs both normalized from 2022 peaks. SPC launch executed Feb 2025."),
      list(lbl = "Why It Matters",
           txt = "Each 100k increase in housing starts adds ~$350M to total addressable flooring demand industry-wide. At ~26–27% share, Shaw captures ~$90–95M of that. Margin tailwind if input costs hold."),
      list(lbl = "What It Means",
           txt = "2025 is the first full-cycle tailwind year since 2022. Revenue and margin expansion is achievable if SPC ramp materializes and commercial sector recovers on ABI signal."),
      list(lbl = "What to Do",
           txt = "Accelerate SPC commercial ramp. Defend carpet margin through mix optimization. Deepen commercial account relationships ahead of the ABI recovery cycle.")
    )
    div(class = "narrative-grid",
        lapply(items, function(i)
          div(class = "narrative-item",
              div(class = "narrative-label", i$lbl),
              tags$p(style = paste0("font-size:13px; line-height:1.65; color:", PAL$text, ";"),
                     i$txt)
          )
        )
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 2 — PORTER'S FIVE FORCES
  # ════════════════════════════════════════════════════════════════════════════
  
  output$porter_header <- renderUI({
    section_header(
      "Porter's Five Forces",
      subtitle = paste0(
        "Structural analysis of Shaw's competitive environment. ",
        "Scores are analyst qualitative judgements (0 = no pressure, 100 = extreme pressure). ",
        "See 'Score Methodology' panel below for full derivation of each score."
      ),
      badge_text = "Qualitative Scoring"
    )
  })
  
  # FIX 3: Plotly scatterpolar requires mode = "lines" set explicitly on each trace.
  # Original code omitted mode; plotly warned "No scatterpolar mode specified: Setting
  # the mode to markers" and then "A line object has been specified, but lines is not
  # in the mode". Fix: add mode = "lines" to each add_trace() call.
  output$porter_radar <- renderPlotly({
    d <- PORTER_RADAR
    plot_ly(type = "scatterpolar") |>
      add_trace(
        r     = c(d$shaw, d$shaw[1]),
        theta = c(d$force, d$force[1]),
        name  = "Shaw (est.)",
        mode  = "lines",           # FIX: explicit mode = "lines"
        fill  = "toself",
        fillcolor = paste0(PAL$accent, "22"),
        line  = list(color = PAL$accent, width = 2)
      ) |>
      add_trace(
        r     = c(d$benchmark, d$benchmark[1]),
        theta = c(d$force, d$force[1]),
        name  = "Mature Mfg. Avg.",
        mode  = "lines",           # FIX: explicit mode = "lines"
        fill  = "toself",
        fillcolor = paste0(PAL$blue, "10"),
        line  = list(color = PAL$blue, width = 1.5, dash = "dash")
      ) |>
      layout(
        polar = list(
          radialaxis  = list(visible = TRUE, range = c(0, 100),
                             gridcolor = PAL$border, linecolor = PAL$border,
                             tickfont  = list(color = PAL$muted, size = 9),
                             tickvals  = c(25, 50, 75, 100)),
          angularaxis = list(tickfont  = list(color = PAL$muted, size = 10),
                             linecolor = PAL$border, gridcolor = PAL$border)
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(color = PAL$muted, family = "Barlow", size = 10),
        legend = list(bgcolor = "rgba(0,0,0,0)",
                      font    = list(color = PAL$muted, size = 10),
                      orientation = "h", y = -0.1),
        margin     = list(l = 30, r = 30, t = 20, b = 50),
        hoverlabel = list(bgcolor = "#0e1520",
                          font = list(color = PAL$text, size = 11))
      ) |>
      config(displayModeBar = FALSE)
  })
  
  output$porter_force_selector <- renderUI({
    flist <- list(
      list(key = "rivalry",     label = "Industry Rivalry",        score = 80, lv = "HIGH", col = PAL$red),
      list(key = "entrants",    label = "Threat of New Entrants",  score = 35, lv = "LOW",  col = PAL$green),
      list(key = "suppliers",   label = "Power of Suppliers",      score = 62, lv = "MED",  col = PAL$amber),
      list(key = "buyers",      label = "Power of Buyers",         score = 55, lv = "MED",  col = PAL$amber),
      list(key = "substitutes", label = "Threat of Substitutes",   score = 72, lv = "HIGH", col = PAL$red)
    )
    div(style = "display:grid; grid-template-columns:1fr 1fr; gap:8px;",
        lapply(flist, function(f) {
          active <- rv$porter_force == f$key
          lv_cls <- switch(f$lv, HIGH = "badge-red", LOW = "badge-green", "badge-amber")
          div(
            style = paste0(
              "border:1px solid ", if (active) f$col else PAL$border, ";",
              "background:", if (active) paste0(f$col, "1a") else "transparent", ";",
              "border-radius:3px; padding:14px; cursor:pointer;"
            ),
            onclick = paste0("Shiny.setInputValue('porter_select','",
                             f$key, "',{priority:'event'})"),
            div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                div(style = paste0("font-size:12px; color:",
                                   if (active) f$col else PAL$muted, ";"), f$label),
                tags$span(class = lv_cls, f$lv)
            ),
            div(style = "height:4px; background:#1e2530; border-radius:2px;",
                div(style = paste0("height:100%; width:", f$score,
                                   "%; background:", f$col, "; border-radius:2px;"))
            ),
            div(style = paste0("font-size:10px; color:", PAL$muted, "; margin-top:4px;"),
                paste0(f$score, "/100"))
          )
        })
    )
  })
  
  observeEvent(input$porter_select, { rv$porter_force <- input$porter_select })
  
  output$porter_active_header <- renderUI({
    f <- PORTER_FORCES[[rv$porter_force]]
    div(style = "display:flex; align-items:center; gap:10px;",
        tags$span(style = paste0("color:", f$color, "; font-size:15px;"), f$label),
        tags$span(class = switch(f$level, HIGH = "badge-red", LOW = "badge-green", "badge-amber"),
                  paste(f$level, "PRESSURE")),
        tags$span(class = "badge-muted", paste0(f$score, "/100"))
    )
  })
  
  output$porter_deep_dive <- renderUI({
    f <- PORTER_FORCES[[rv$porter_force]]
    div(
      tags$p(style = paste0("font-size:14px; color:", PAL$text,
                            "; font-style:italic; margin-bottom:20px;"), f$headline),
      div(style = "display:grid; grid-template-columns:1fr 1fr 1fr 1fr; gap:14px;",
          lapply(list(
            list(lbl = "Hypothesis",      col = PAL$blue,  txt = f$hypothesis, items = NULL),
            list(lbl = "Evidence",        col = PAL$amber, txt = NULL,          items = f$evidence),
            list(lbl = "Insight",         col = PAL$accent,txt = f$insight,     items = NULL),
            list(lbl = "Action Required", col = PAL$green, txt = f$action,      items = NULL)
          ), function(sec) {
            div(style = paste0("background:", sec$col, "0e; border:1px solid ",
                               sec$col, "28; border-radius:3px; padding:14px;"),
                div(style = paste0("font-size:10px; color:", sec$col,
                                   "; letter-spacing:0.15em; text-transform:uppercase;",
                                   " margin-bottom:10px;"), sec$lbl),
                if (!is.null(sec$items))
                  tags$ul(style = "padding-left:14px; margin:0;",
                          lapply(sec$items, function(e)
                            tags$li(style = "font-size:12px; line-height:1.65; margin-bottom:5px;", e)
                          ))
                else
                  tags$p(style = "font-size:12px; line-height:1.7; margin:0;", sec$txt)
            )
          })
      )
    )
  })
  
  # ADD: Porter Scoring Methodology panel — full transparency on score derivation
  output$porter_methodology <- renderUI({
    force_key <- rv$porter_force
    # Map force key to PORTER_SCORING_METHODOLOGY row
    force_label_map <- c(
      rivalry     = "Industry Rivalry",
      entrants    = "Threat of New Entrants",
      suppliers   = "Power of Suppliers",
      buyers      = "Power of Buyers",
      substitutes = "Threat of Substitutes"
    )
    fl  <- force_label_map[[force_key]]
    row <- PORTER_SCORING_METHODOLOGY |> filter(force == fl)
    
    if (nrow(row) == 0) return(NULL)
    
    div(
      # Score summary bar
      div(style = paste0("display:grid; grid-template-columns:1fr 1fr 1fr; gap:12px;",
                         " margin-bottom:16px;"),
          div(style = paste0("background:", PAL$accent, "10; border:1px solid ",
                             PAL$accent, "30; border-radius:3px; padding:12px 16px;"),
              div(style = paste0("font-size:10px; color:", PAL$accent,
                                 "; letter-spacing:0.15em; text-transform:uppercase;"), "Shaw Score"),
              div(style = paste0("font-size:28px; font-family:'Playfair Display',serif;",
                                 " color:", PAL$accent, ";"), paste0(row$shaw_score, "/100")),
              div(style = "height:4px; background:#1e2530; border-radius:2px; margin-top:6px;",
                  div(style = paste0("height:100%; width:", row$shaw_score,
                                     "%; background:", PAL$accent, "; border-radius:2px;")))
          ),
          div(style = paste0("background:", PAL$blue, "10; border:1px solid ",
                             PAL$blue, "30; border-radius:3px; padding:12px 16px;"),
              div(style = paste0("font-size:10px; color:", PAL$blue,
                                 "; letter-spacing:0.15em; text-transform:uppercase;"), "Benchmark Score"),
              div(style = paste0("font-size:28px; font-family:'Playfair Display',serif;",
                                 " color:", PAL$blue, ";"), paste0(row$bench_score, "/100")),
              div(style = paste0("font-size:11px; color:", PAL$muted, "; margin-top:4px;"),
                  "Mature U.S. Mfg. composite")
          ),
          div(style = paste0("background:#1e253022; border:1px solid ", PAL$border,
                             "; border-radius:3px; padding:12px 16px;"),
              div(style = paste0("font-size:10px; color:", PAL$muted,
                                 "; letter-spacing:0.15em; text-transform:uppercase;"), "Last Reviewed"),
              div(style = paste0("font-size:14px; color:", PAL$text, "; margin:6px 0 4px;"),
                  row$last_reviewed),
              div(style = paste0("font-size:10px; color:", PAL$muted, "; font-style:italic;"),
                  "Trigger: ", row$review_trigger)
          )
      ),
      
      # Score components breakdown
      div(style = paste0("background:", PAL$panel, "; border:1px solid ", PAL$border,
                         "; border-radius:3px; padding:16px; margin-bottom:12px;"),
          div(style = paste0("font-size:10px; color:", PAL$accent,
                             "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:10px;"),
              "Score Derivation — Step by Step"),
          tags$pre(style = paste0("font-size:11px; line-height:1.8; color:", PAL$text,
                                  "; white-space:pre-wrap; margin:0; font-family:'Barlow',sans-serif;"),
                   row$score_components_shaw)
      ),
      
      # Benchmark rationale
      div(style = paste0("background:#1e253015; border:1px solid ", PAL$border,
                         "; border-radius:3px; padding:14px; margin-bottom:12px;"),
          div(style = paste0("font-size:10px; color:", PAL$blue,
                             "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:8px;"),
              "Benchmark Rationale"),
          tags$p(style = paste0("font-size:12px; line-height:1.65; margin:0; color:", PAL$muted, ";"),
                 row$benchmark_rationale)
      ),
      
      # Primary data sources
      div(style = paste0("background:#1e253010; border:1px solid ", PAL$border,
                         "; border-radius:3px; padding:14px;"),
          div(style = paste0("font-size:10px; color:", PAL$green,
                             "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:8px;"),
              "Primary Data Sources"),
          tags$p(style = paste0("font-size:12px; line-height:1.65; margin:0;"),
                 row$primary_data_sources)
      )
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 3 — SWOT ENGINE
  # ════════════════════════════════════════════════════════════════════════════
  
  output$swot_header <- renderUI({
    section_header(
      "Dynamic SWOT Engine",
      subtitle = paste0(
        "Evidence-based SWOT with leading indicators and risk probability scoring. ",
        "Evidence is sourced from public SEC filings, FRED data, and trade press. ",
        "Risk probability reflects analyst judgement of materialization likelihood within 12 months."
      ),
      badge_text = "Evidence-Based"
    )
  })
  
  output$swot_quadrant_row <- renderUI({
    quads   <- list(S = list(col=PAL$green), W = list(col=PAL$red),
                    O = list(col=PAL$blue),  T = list(col=PAL$amber))
    qlabels <- list(S="Strengths",W="Weaknesses",O="Opportunities",T="Threats")
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr 1fr; gap:8px;",
        lapply(names(quads), function(q) {
          qd     <- quads[[q]]
          active <- rv$swot_quad == q
          n      <- length(SWOT_DATA[[q]]$items)
          div(style = paste0(
            "border:1px solid ", if (active) qd$col else PAL$border, ";",
            "background:", if (active) paste0(qd$col, "1a") else "transparent", ";",
            "border-radius:3px; padding:16px; cursor:pointer; text-align:center;"
          ),
          onclick = paste0("Shiny.setInputValue('swot_quad_sel','",
                           q, "',{priority:'event'})"),
          div(style = paste0("font-size:28px; font-family:'Playfair Display',serif;",
                             " color:", if (active) qd$col else PAL$muted, "; margin-bottom:4px;"), q),
          div(style = paste0("font-size:13px; color:",
                             if (active) PAL$text else PAL$muted, ";"), qlabels[[q]]),
          div(style = paste0("font-size:11px; color:", PAL$muted, "; margin-top:4px;"),
              paste(n, "factors"))
          )
        })
    )
  })
  
  observeEvent(input$swot_quad_sel, { rv$swot_quad <- input$swot_quad_sel })
  
  output$swot_items <- renderUI({
    q     <- rv$swot_quad
    qd    <- SWOT_DATA[[q]]
    items <- qd$items
    col   <- qd$color
    desc  <- switch(q,
                    S = "Internal positive factors — sourced from public data and trade press",
                    W = "Internal risk factors — with FRED series leading indicators",
                    O = "External upside factors — prioritized by revenue scale",
                    T = "External risk factors — with probability scoring"
    )
    div(
      tags$p(style = paste0("font-size:12px; color:", PAL$muted,
                            "; font-style:italic; margin-bottom:16px;"), desc),
      div(style = "display:grid; gap:8px;",
          lapply(seq_along(items), function(i) {
            it    <- items[[i]]
            r_col <- if (it$risk < 30) PAL$green
            else if (it$risk < 60) PAL$amber
            else PAL$red
            tc_cls <- switch(it$tier, HIGH = "badge-red", MED = "badge-amber", "badge-muted")
            div(style = paste0(
              "background:rgba(255,255,255,0.015); border:1px solid ", PAL$border, ";",
              "border-radius:3px; padding:16px 18px;"
            ),
            div(style = "display:flex; justify-content:space-between; align-items:center;",
                div(style = "display:flex; gap:12px; align-items:center;",
                    tags$span(style = paste0("font-size:11px; color:", col,
                                             "; min-width:20px;"), sprintf("%02d", i)),
                    tags$span(style = paste0("font-size:15px; font-family:'Playfair Display',serif;",
                                             " color:#c0b8a8;"), it$title),
                    tags$span(class = tc_cls, it$tier)
                ),
                div(style = "display:flex; gap:10px; align-items:center;",
                    div(style = "text-align:right;",
                        div(style = paste0("font-size:10px; color:", PAL$muted, ";"), "Risk prob."),
                        div(style = paste0("font-size:13px; color:", r_col, "; font-weight:bold;"),
                            paste0(it$risk, "%"))
                    ),
                    div(style = "width:48px;",
                        div(style = "height:4px; background:#1e2530; border-radius:2px;",
                            div(style = paste0("height:100%; width:", it$risk,
                                               "%; background:", r_col, "; border-radius:2px;"))
                        )
                    )
                )
            ),
            div(style = paste0(
              "margin-top:14px; padding-top:14px; border-top:1px solid ", PAL$border, ";",
              "display:grid; grid-template-columns:1.2fr 1fr 0.8fr; gap:14px;"
            ),
            div(
              div(style = paste0("font-size:10px; color:", PAL$accent,
                                 "; letter-spacing:0.12em; text-transform:uppercase; margin-bottom:6px;"),
                  "Evidence & Source"),
              tags$p(style = "font-size:12px; line-height:1.65; margin:0;", it$evidence)
            ),
            div(
              div(style = paste0("font-size:10px; color:", PAL$blue,
                                 "; letter-spacing:0.12em; text-transform:uppercase; margin-bottom:6px;"),
                  "Leading Indicator"),
              tags$p(style = "font-size:12px; line-height:1.65; margin:0;", it$indicator)
            ),
            div(
              div(style = paste0("font-size:10px; color:", r_col,
                                 "; letter-spacing:0.12em; text-transform:uppercase; margin-bottom:6px;"),
                  "Risk Assessment"),
              div(style = "height:8px; background:#1e2530; border-radius:4px; margin-bottom:8px;",
                  div(style = paste0("height:100%; width:", it$risk,
                                     "%; background:", r_col, "; border-radius:4px;"))
              ),
              tags$p(style = paste0("font-size:12px; color:", r_col, "; margin:0;"),
                     paste0(it$risk, "% materialization probability (analyst est.)"))
            )
            )
            )
          })
      )
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 4 — ASSUMPTION MONITOR
  # ════════════════════════════════════════════════════════════════════════════
  
  output$assump_header <- renderUI({
    section_header(
      "Strategy Assumptions Monitor",
      subtitle = paste0(
        "Every strategy is a set of assumptions. This monitor tracks whether ",
        "Shaw's key strategic assumptions are holding — and flags early when they break. ",
        "FRED series IDs listed for each metric enable live verification."
      ),
      badge_text = "Early Warning System"
    )
  })
  
  output$assump_summary_cards <- renderUI({
    cnts <- table(ASSUMPTIONS$status)
    g <- as.integer(cnts["green"]);  if (is.na(g)) g <- 0L
    y <- as.integer(cnts["yellow"]); if (is.na(y)) y <- 0L
    r <- as.integer(cnts["red"]);    if (is.na(r)) r <- 0L
    
    mk <- function(n, lbl, sub, col) {
      div(style = paste0(
        "background:", col, "12; border:1px solid ", col, "33;",
        "border-radius:4px; padding:16px 20px; display:flex; align-items:center; gap:16px;"
      ),
      div(style = paste0("font-size:36px; color:", col,
                         "; font-family:'Playfair Display',serif;"), n),
      div(
        div(style = paste0("font-size:12px; color:", col,
                           "; letter-spacing:0.1em; text-transform:uppercase;"), lbl),
        div(style = paste0("font-size:11px; color:", PAL$muted, ";"), sub)
      ))
    }
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:12px;",
        mk(g, "On Track",  "Assumptions holding",         PAL$green),
        mk(y, "Watch",     "Approaching threshold",        PAL$amber),
        mk(r, "At Risk",   "Assumption broken or failing", PAL$red)
    )
  })
  
  output$assump_cat_filters <- renderUI({
    cats <- c("ALL", sort(unique(ASSUMPTIONS$category)))
    lapply(cats, function(cat) {
      active <- rv$assump_cat == cat
      tags$button(cat,
                  style = paste0(
                    "background:", if (active) "rgba(200,168,75,0.15)" else "transparent", ";",
                    "border:1px solid ", if (active) PAL$accent else PAL$border, ";",
                    "border-radius:2px; padding:5px 14px; cursor:pointer;",
                    "font-size:11px; color:", if (active) PAL$accent else PAL$muted,
                    "; letter-spacing:0.1em; text-transform:uppercase;"
                  ),
                  onclick = paste0("Shiny.setInputValue('assump_cat_sel','",
                                   cat, "',{priority:'event'})")
      )
    })
  })
  
  observeEvent(input$assump_cat_sel, { rv$assump_cat <- input$assump_cat_sel })
  
  output$assump_table <- renderDT({
    d <- ASSUMPTIONS
    if (rv$assump_cat != "ALL") d <- filter(d, category == rv$assump_cat)
    
    d_disp <- d |>
      mutate(
        Status = case_when(
          status == "green"  ~ "\U0001F7E2 On Track",
          status == "yellow" ~ "\U0001F7E1 Watch",
          status == "red"    ~ "\U0001F534 At Risk"
        ),
        Progress = paste0(
          '<div style="width:80px;height:5px;background:#1e2530;border-radius:3px;">',
          '<div style="height:100%;width:',
          round(pmin(current_v / target_v, 1.05) * 100),
          '%;background:',
          case_when(status == "green" ~ PAL$green, status == "yellow" ~ PAL$amber, TRUE ~ PAL$red),
          ';border-radius:3px;"></div></div>'
        ),
        Weight = paste0(
          '<span class="badge-',
          tolower(case_when(weight == "HIGH" ~ "red", weight == "MEDIUM" ~ "amber", TRUE ~ "muted")),
          '">', weight, '</span>'
        )
      ) |>
      select(Status, assumption, category, Weight, metric, current, threshold, trend, Progress)
    
    names(d_disp) <- c("Status","Assumption","Category","Priority",
                       "Metric (FRED / Source)","Current","Threshold","Trend","Progress")
    
    datatable(d_disp,
              escape   = FALSE,
              rownames = FALSE,
              options  = list(
                pageLength = 15,
                dom        = "t",
                ordering   = TRUE,
                columnDefs = list(list(className = "dt-left", targets = "_all"))
              ),
              style = "auto"
    )
  })
  
  output$assump_directive <- renderUI({
    insight_box(
      paste0(
        "The two RED assumptions — LVT/SPC revenue share and SPC launch market share ramp — ",
        "require immediate management attention. Shaw's February 2025 SPC launch must reach ",
        "≥4% SPC category share by end of 2026 to offset the structural carpet volume decline. ",
        "Monitor FRED HOUST and MORTGAGE30US monthly as the primary leading indicators ",
        "for the residential volume recovery assumption."
      ),
      "accent", "Executive Directive"
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 5 — SCENARIO MODELING
  # ════════════════════════════════════════════════════════════════════════════
  
  output$scenario_header <- renderUI({
    section_header(
      "Scenario & Sensitivity Modeling",
      subtitle = paste0(
        "3–5 year revenue and EBITDA scenarios for Shaw Industries. ",
        "\u26a0 All figures are analyst estimates — Shaw does not report standalone financials. ",
        "Scenarios are calibrated to FRED macroeconomic series."
      ),
      badge_text  = "Analyst Estimates",
      badge_color = "amber"
    )
  })
  
  output$scenario_selector <- renderUI({
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr 1fr; gap:8px;",
        lapply(names(SCENARIO_META), function(s) {
          m      <- SCENARIO_META[[s]]
          active <- rv$scenario_sel == s
          div(style = paste0(
            "border:1px solid ", if (active) m$color else PAL$border, ";",
            "background:", if (active) paste0(m$color, "1a") else "transparent", ";",
            "border-radius:3px; padding:14px; cursor:pointer;"
          ),
          onclick = paste0("Shiny.setInputValue('scenario_sel_btn','",
                           s, "',{priority:'event'})"),
          div(style = paste0("font-size:13px; color:",
                             if (active) m$color else PAL$muted, "; margin-bottom:6px;"), m$label),
          div(style = paste0("font-size:11px; color:", PAL$muted, "; line-height:1.5;"),
              substr(m$desc, 1, 70), "...")
          )
        })
    )
  })
  
  observeEvent(input$scenario_sel_btn, { rv$scenario_sel <- input$scenario_sel_btn })
  
  output$scenario_desc_box <- renderUI({
    m <- SCENARIO_META[[rv$scenario_sel]]
    div(style = paste0("padding:14px 18px; background:", m$color, "0e;",
                       "border:1px solid ", m$color, "33; border-radius:3px;"),
        div(style = paste0("font-size:11px; color:", m$color,
                           "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:8px;"),
            paste(m$label, "— Scenario Drivers")),
        tags$p(style = paste0("font-size:13px; line-height:1.7; margin:0 0 8px;"), m$desc),
        div(style = paste0("font-size:11px; color:", PAL$muted, ";"),
            "Key FRED indicators: ", m$drivers)
    )
  })
  
  output$scenario_rev_chart <- renderPlotly({
    d <- SCENARIOS[[rv$scenario_sel]]
    m <- SCENARIO_META[[rv$scenario_sel]]
    plot_ly(d, x = ~year) |>
      add_trace(y = ~revenue_m, type = "scatter", mode = "lines+markers",
                name      = "Revenue $M (est.)",
                line      = list(color = m$color, width = 2.5),
                marker    = list(color = m$color, size = 7),
                fill      = "tozeroy",
                fillcolor = paste0(m$color, "14"),
                hovertemplate = "%{x}: $%{y:,.0f}M (est.)<extra></extra>") |>
      add_segments(x = "2024E", xend = "2024E", y = 4800, yend = 8500,
                   line = list(color = PAL$muted, dash = "dot", width = 1),
                   showlegend = FALSE, hoverinfo = "skip") |>
      plotly_dark(xlab = "", ylab = "Revenue $M (analyst est.)", legend = FALSE)
  })
  
  output$scenario_rev_note <- renderUI({
    data_note_ui(
      "Revenue figures are analyst estimates. Shaw Industries (Berkshire Hathaway subsidiary) does not publicly report standalone revenue or EBITDA. EBITDA margins are modeled from public-company industry peers."
    )
  })
  
  output$scenario_ebitda_chart <- renderPlotly({
    d <- SCENARIOS[[rv$scenario_sel]]
    m <- SCENARIO_META[[rv$scenario_sel]]
    plot_ly(d, x = ~year, y = ~ebitda_m,
            type = "bar",
            marker = list(color = m$color, opacity = 0.75),
            text  = ~paste0("$", ebitda_m, "M (est.)"),
            textposition = "outside",
            hovertemplate = "%{x}: $%{y}M est.<extra></extra>") |>
      plotly_dark(xlab = "", ylab = "EBITDA $M (analyst est.)", legend = FALSE)
  })
  
  output$scenario_table <- renderDT({
    d <- SCENARIOS[[rv$scenario_sel]] |>
      mutate(
        growth  = c(NA, diff(revenue_m) / head(revenue_m, -1) * 100),
        em_pct  = round(ebitda_m / revenue_m * 100, 1),
        rev_fmt = paste0("$", format(revenue_m, big.mark = ",")),
        gr_fmt  = ifelse(is.na(growth), "\u2014",
                         paste0(ifelse(growth >= 0, "+", ""), sprintf("%.1f%%", growth))),
        em_fmt  = paste0(ebitda_margin, "%"),
        eb_fmt  = paste0("$", format(ebitda_m, big.mark = ",")),
        epm_fmt = paste0(em_pct, "%")
      ) |>
      select(year, rev_fmt, gr_fmt, em_fmt, eb_fmt, epm_fmt)
    
    names(d) <- c("Year","Revenue ($M est.)","YoY Growth",
                  "EBITDA Margin","EBITDA ($M est.)","EBITDA/Rev")
    
    datatable(d, rownames = FALSE,
              options = list(dom = "t", ordering = FALSE,
                             columnDefs = list(list(className = "dt-left", targets = "_all"))),
              style = "auto") |>
      formatStyle("YoY Growth",
                  color = JS(paste0(
                    "function(v){",
                    "if(v==='\u2014') return '", PAL$muted, "';",
                    "return v.startsWith('+') ? '", PAL$green, "' : '", PAL$red, "';",
                    "}"
                  ))
      )
  })
  
  output$leading_indicators <- renderUI({
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px;",
        lapply(seq_len(nrow(LEADING_INDICATORS)), function(i) {
          r   <- LEADING_INDICATORS[i, ]
          col <- switch(r$status, green = PAL$green, amber = PAL$amber, PAL$red)
          div(style = paste0("border:1px solid ", PAL$border,
                             "; border-radius:3px; padding:12px 14px;"),
              div(style = "font-size:12px; margin-bottom:4px;", r$indicator),
              div(style = paste0("font-size:11px; color:", PAL$muted, "; margin-bottom:6px;"), r$lead_time),
              div(style = paste0("font-size:12px; color:", col, ";"), r$current)
          )
        })
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 6 — FACT BASE
  # ════════════════════════════════════════════════════════════════════════════
  
  output$factbase_header <- renderUI({
    section_header(
      "Live Fact Base",
      subtitle = paste0(
        "Continuous environmental scan from FRED, BLS, U.S. Census Bureau, ",
        "and public competitor data. FRED series auto-fetch on login when FRED_API_KEY is set. ",
        "MHK/TILE/AWI equity data auto-fetches via tidyquant (Yahoo Finance API) when installed."
      ),
      badge_text  = if (rv$using_live) "Live FRED" else "Static Snapshot",
      badge_color = if (rv$using_live) "green" else "amber"
    )
  })
  
  output$source_card_row <- renderUI({
    sources <- list(
      list(src = "FRED", col = PAL$accent,
           desc = "Federal Reserve Bank of St. Louis — monetary policy, housing, CPI, PPI, construction. 7 series.",
           id   = "Set FRED_API_KEY in .Renviron for live fetch"),
      list(src = "tidyquant", col = PAL$blue,
           desc = "Yahoo Finance API via tidyquant R package. Fetches MHK, TILE, AWI stock prices on login.",
           id   = "install.packages('tidyquant') to enable live equity quotes"),
      list(src = "Census", col = PAL$green,
           desc = "U.S. Census Bureau — construction spending (TTLCONS), trade HS codes 3918/5703 for import monitoring.",
           id   = "Set CENSUS_API_KEY for direct Census API access")
    )
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:12px;",
        lapply(sources, function(s) {
          cls <- switch(s$src, FRED = "badge-accent", tidyquant = "badge-blue", "badge-green")
          div(style = paste0("background:", s$col, "0a; border:1px solid ",
                             s$col, "22; border-radius:3px; padding:14px 16px;"),
              div(style = "display:flex; justify-content:space-between; margin-bottom:8px;",
                  tags$span(class = cls, s$src)
              ),
              div(style = paste0("font-size:12px; color:", PAL$muted, "; line-height:1.6; margin-bottom:6px;"),
                  s$desc),
              div(style = paste0("font-size:10px; color:", PAL$muted, "; font-style:italic;"), s$id)
          )
        })
    )
  })
  
  output$factbase_cat_filters <- renderUI({
    cats <- c("ALL", sort(unique(FACT_BASE_STATIC$category)))
    lapply(cats, function(cat) {
      active <- rv$fb_cat == cat
      tags$button(cat,
                  style = paste0(
                    "background:", if (active) "rgba(200,168,75,0.15)" else "transparent", ";",
                    "border:1px solid ", if (active) PAL$accent else PAL$border, ";",
                    "border-radius:2px; padding:5px 14px; cursor:pointer;",
                    "font-size:11px; color:", if (active) PAL$accent else PAL$muted,
                    "; letter-spacing:0.1em; text-transform:uppercase;"
                  ),
                  onclick = paste0("Shiny.setInputValue('fb_cat_sel','",
                                   cat, "',{priority:'event'})")
      )
    })
  })
  
  observeEvent(input$fb_cat_sel, { rv$fb_cat <- input$fb_cat_sel })
  
  # ADD: factbase_data reactive: merges live FRED + live equity + static fallback
  factbase_data <- reactive({
    d <- FACT_BASE_STATIC
    
    # Merge live FRED values
    if (rv$using_live && !is.null(rv$macro)) {
      m    <- tail(rv$macro, 1)
      live <- list(
        "Housing Starts: Total (HOUST)"               = paste0(round(m$housing_starts), "k SAAR"),
        "30-Yr Fixed Mortgage Rate (MORTGAGE30US)"    = paste0(round(m$mortgage_30, 2), "%"),
        "Federal Funds Effective Rate (FEDFUNDS)"     = paste0(round(m$fed_funds, 2), "%"),
        "Total Construction Spending (TTLCONS)"       = paste0("$", round(m$construction), "B ann."),
        "PPI: Plastics Products (WPU0672)"            = as.character(round(m$ppi_plastics, 1)),
        "PPI: Long-Dist. Freight Trucking (PCU484121484121)" = as.character(round(m$freight_ppi, 1)),
        "CPI: All Urban Consumers (CPIAUCSL)"         = as.character(round(m$cpi, 1))
      )
      for (nm in names(live)) {
        idx <- which(d$series == nm)
        if (length(idx) > 0 && !is.na(live[[nm]])) {
          d$value[idx]  <- live[[nm]]
          d$note[idx]   <- paste0("Live FRED — ", format(Sys.Date(), "%b %d %Y"))
          d$period[idx] <- paste0("Latest FRED as of ", format(Sys.Date(), "%b %Y"))
        }
      }
    }
    
    # ADD: merge live equity values from tidyquant
    if (rv$using_equity && !is.null(rv$equity)) {
      eq_map <- list(
        "Mohawk Industries (MHK) — Public competitor"     = "MHK",
        "Interface Inc. (TILE) — Public competitor"       = "TILE",
        "Armstrong World Ind. (AWI) — Adjacent public"   = "AWI"
      )
      for (series_nm in names(eq_map)) {
        tk  <- eq_map[[series_nm]]
        smr <- equity_summary(rv$equity, tk)
        if (!is.null(smr)) {
          idx <- which(d$series == series_nm)
          if (length(idx) > 0) {
            d$value[idx]  <- paste0("$", smr$last_close)
            d$change[idx] <- paste0(ifelse(smr$ytd_chg_pct >= 0, "+", ""),
                                    smr$ytd_chg_pct, "% YTD")
            d$period[idx] <- paste0("Live — ", smr$last_date)
            d$note[idx]   <- paste0("Live via tidyquant/Yahoo Finance. ",
                                    "52-wk: $", smr$low_52wk, "–$", smr$high_52wk)
          }
        }
      }
    }
    d
  })
  
  output$factbase_table <- renderDT({
    d <- factbase_data()
    if (rv$fb_cat != "ALL") d <- filter(d, category == rv$fb_cat)
    
    d_disp <- d |>
      mutate(
        Signal = paste0(
          '<span style="display:inline-block;width:7px;height:7px;border-radius:50%;',
          'background:', case_when(status == "green"  ~ PAL$green,
                                   status == "yellow" ~ PAL$amber, TRUE ~ PAL$red),
          ';margin-right:8px;vertical-align:middle;"></span>', series
        ),
        Source = paste0(
          '<span class="badge-',
          case_when(source == "FRED" ~ "accent",
                    grepl("tidyquant", source) ~ "blue", TRUE ~ "green"),
          '">', source, '</span>'
        ),
        Category = paste0('<span class="badge-muted">', category, '</span>')
      ) |>
      select(Signal, Source, Category, value, change, period, note)
    
    names(d_disp) <- c("Signal","Source","Category","Value","Change","Period","Data Note")
    
    datatable(d_disp, escape = FALSE, rownames = FALSE,
              options = list(
                pageLength = 15, dom = "t",
                columnDefs = list(
                  list(className = "dt-left", targets = "_all"),
                  list(width = "220px", targets = 6)
                )
              ), style = "auto")
  })
  
  output$macro_heatmap <- renderUI({
    div(style = "padding:4px 0; display:grid; gap:12px;",
        lapply(seq_len(nrow(MACRO_SIGNALS)), function(i) {
          r   <- MACRO_SIGNALS[i, ]
          col <- switch(r$status, green = PAL$green, amber = PAL$amber, red = PAL$red, PAL$muted)
          div(
            div(style = "display:grid; grid-template-columns:170px 1fr 32px; gap:10px;",
                div(style = "font-size:12px;", r$label),
                div(style = "height:6px; background:#1e2530; border-radius:3px; margin-top:5px;",
                    div(style = paste0("height:100%; width:", r$score, "%; background:", col,
                                       "; border-radius:3px;"))
                ),
                div(style = paste0("font-size:11px; color:", col,
                                   "; font-weight:bold; text-align:right;"), r$score)
            ),
            div(style = paste0("font-size:10px; color:", PAL$muted,
                               "; margin-top:3px; padding-left:180px;"), r$note)
          )
        })
    )
  })
  
  # FIX 4: Engineered Floors.

  output$competitive_intel <- renderUI({
    intel <- list(
      list(co = "Mohawk Industries (MHK) — NYSE: MHK", st = "yellow",
           note = "FY2023 10-K: $10.9B revenue; gross margin 29.6% (down from ~34% in 2018). Largest direct competitor; significant LVT capacity; quarterly filings on SEC EDGAR. Live quote available via tidyquant."),
      list(co = "Interface, Inc. (TILE) — Nasdaq: TILE", st = "green",
           note = "FY2023 10-K: $1.37B revenue; gross margin 38%. Pure commercial modular carpet — not a direct residential competitor. Growing healthcare and corporate segments. Live quote available via tidyquant."),
      list(co = "Armstrong World Industries (AWI) — NYSE: AWI", st = "yellow",
           note = "FY2023 10-K: $1.27B revenue; primarily ceiling and wall systems — flooring is a secondary segment. Gross margin 35.1%. Adjacent, not direct. Live quote available via tidyquant."),
      list(co = "Tarkett SA — private listing", st = "yellow",
           note = "2023 Annual Report: ~\u20AC2.9B revenue. European LVT leader with growing U.S. presence. Private label flooring segments competing on price. Monitor Census HS 3918 import volumes."),
      list(co = "Engineered Floors — private, independent competitor", st = "red",
           note = paste0(
             "Founded 2010 by Jim Bethel Shaw, formerly Shaw Industries CEO. Headquartered Dalton, GA. ",
             "Focused on polyester-optimized residential carpet manufacturing. Est. $1.5–2B revenue. ",
             "Direct Shaw competitor in mid-market residential carpet. No public financials. ",
             "Monitor via Floor Covering Weekly market share data and trade press."
           ))
    )
    div(style = "display:grid; gap:10px;",
        lapply(intel, function(c) {
          col <- switch(c$st, green = PAL$green, yellow = PAL$amber, PAL$red)
          div(style = paste0("display:flex; gap:12px; padding:10px 12px;",
                             "background:rgba(255,255,255,0.02); border-radius:3px;",
                             "border:1px solid ", PAL$border, ";"),
              div(style = paste0("width:7px;height:7px;border-radius:50%;background:",
                                 col, ";margin-top:4px;flex-shrink:0;")),
              div(
                div(style = "font-size:12px; margin-bottom:4px;", c$co),
                div(style = paste0("font-size:11px; color:", PAL$muted, "; line-height:1.5;"), c$note)
              )
          )
        })
    )
  })
  
}