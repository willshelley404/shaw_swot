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
#  FIX 5: exec_rev_housing — switched x = ~label (categorical, causes jumbled ticks)
#          to x = ~date with type="date", dtick="M3", tickformat="%b '%y".
#          Removed debug print(d). Removed unused monthly_ticks() helper.
#          Shaw Rev Est. forward projection rendered as dashed gold trace with
#          "(E)" suffix so the legend distinguishes actual FRED vs. estimated tail.
#  FIX 6: exec_input_costs already used x = ~date correctly — preserved as-is.
#  ADD:   Live equity data fetch via tidyquant (MHK, TILE, AWI) on login.
#  ADD:   Porter scoring methodology output for transparency panel.
#  ADD:   output$exec_mekko + output$exec_mekko_note (Marimekko market structure).
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Reactive State ──────────────────────────────────────────────────────────
  rv <- reactiveValues(
    logged_in    = FALSE,
    role         = NULL,
    macro        = NULL,
    equity       = NULL,
    using_live   = FALSE,
    using_equity = FALSE,
    porter_force = "rivalry",
    swot_quad    = "S",
    scenario_sel = "base",
    assump_cat   = "ALL",
    fb_cat       = "ALL",
    division     = "Residential"
  )
  
  # ── Login: show API/package status ─────────────────────────────────────────
  output$login_api_status <- renderUI({
    has_fred <- nchar(FRED_API_KEY) > 0
    has_tq   <- TIDYQUANT_AVAILABLE
    
    fred_col  <- if (has_fred) PAL$green else PAL$amber
    fred_icon <- if (has_fred) "\u25cf" else "\u25cb"
    fred_msg  <- if (has_fred) "FRED key in .Renviron — live macro data"
    else          "No FRED_API_KEY — demo mode (mock data)"
    
    tq_col  <- if (has_tq) PAL$green else PAL$amber
    tq_icon <- if (has_tq) "\u25cf" else "\u25cb"
    tq_msg  <- if (has_tq) "tidyquant installed — live MHK/TILE/AWI quotes"
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
        live <- tryCatch({
          withProgress(message = "Connecting to FRED\u2026", value = 0.3, {
            result <- fetch_fred_data(start = "2019-01-01")
            incProgress(0.6)
            result
          })
        }, error = function(e) NULL)
        
        if (!is.null(live) && nrow(live) > 4 && !all(is.na(live$housing_starts))) {
          # FIX 5: keep date column intact; label column retained for any legacy use
          live <- live |> mutate(label = format(date, "%b '%y"))
          live <- left_join(live, MOCK_MACRO |> select(date, shaw_rev_est), by = "date")
          rv$macro      <- live
          rv$using_live <- TRUE
        } else {
          rv$macro      <- MOCK_MACRO
          rv$using_live <- FALSE
        }
      } else {
        rv$macro      <- MOCK_MACRO
        rv$using_live <- FALSE
      }
      
      # ── Fetch equity data via tidyquant ────────────────────────────────────
      if (TIDYQUANT_AVAILABLE) {
        withProgress(message = "Fetching equity data (MHK, TILE, AWI)\u2026", value = 0.1, {
          eq <- fetch_equity_data(tickers = c("MHK", "TILE", "AWI"), lookback_days = 365)
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
        if(first){ first.style.color='#2b7bd6'; first.style.borderColor='#2b7bd6';
                   first.style.background='rgba(43,123,214,0.12)'; }
      ")
      
    } else {
      shinyjs::runjs("document.getElementById('login_error').style.display='block';")
    }
  })
  
  observeEvent(input$login_pass, {
    shinyjs::runjs("
      document.getElementById('login_pass').onkeydown = function(e){
        if(e.key==='Enter') document.getElementById('login_btn').click();
      };
    ")
  })
  
  observeEvent(input$signout_btn, {
    rv$logged_in    <- FALSE
    rv$using_live   <- FALSE
    rv$using_equity <- FALSE
    rv$macro        <- NULL
    rv$equity       <- NULL
    shinyjs::show("login_screen")
    shinyjs::hide("main_portal")
    updateTextInput(session, "login_user", value = "")
    shinyjs::reset("login_pass")
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
      if(el){ el.style.color='#2b7bd6'; el.style.borderColor='#2b7bd6';
              el.style.background='rgba(43,123,214,0.12)'; }
    ", tolower(d)))
  })
  
  macro <- reactive({ req(rv$macro); rv$macro })
  
  # ── Shared Outputs ──────────────────────────────────────────────────────────
  output$topbar_status <- renderUI({
    req(rv$logged_in)
    col    <- if (rv$using_live) PAL$green else PAL$blue
    msg    <- if (rv$using_live) paste0("Live FRED data — ", format(Sys.time(), "%b %d %H:%M"))
    else "Demo mode — estimates current Q1 2026"
    eq_msg <- if (rv$using_equity) " | MHK/TILE/AWI live" else ""
    div(style = "display:flex; align-items:center; gap:6px;",
        tags$span(style = paste0("width:7px;height:7px;border-radius:50%;",
                                 "background:", col, ";display:inline-block;")),
        tags$span(style = paste0("font-size:10px; color:", PAL$muted, ";"),
                  paste0(msg, eq_msg))
    )
  })
  
  output$division_banner <- renderUI({
    req(rv$logged_in)
    d   <- rv$division
    cfg <- DIVISION_CONFIG[[d]]
    col <- cfg$color
    div(style = "display:flex; align-items:flex-start; gap:20px; flex-wrap:wrap;",
        div(style = "display:flex; align-items:center; gap:10px; flex-shrink:0;",
            tags$span(class = "badge-accent",
                      style = paste0("background:rgba(43,123,214,0.18); color:", col, ";",
                                     "border-color:", col, "55;"), d),
            tags$span(style = paste0("font-size:10px; color:", PAL$muted,
                                     "; letter-spacing:0.15em; text-transform:uppercase;"),
                      cfg$rev_share)
        ),
        tags$span(style = paste0("font-size:11px; color:", PAL$muted,
                                 "; border-left:1px solid ", PAL$border, "; padding-left:16px;"),
                  cfg$brands),
        tags$span(style = paste0("font-size:11px; color:", PAL$muted,
                                 "; border-left:1px solid ", PAL$border, "; padding-left:16px;"),
                  "\uD83D\uDCCA Key cycle: ", cfg$cycle),
        tags$span(style = paste0("font-size:11px; color:", PAL$muted,
                                 "; border-left:1px solid ", PAL$border,
                                 "; padding-left:16px; font-style:italic;"), cfg$why)
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 1 — EXECUTIVE SUMMARY
  # ════════════════════════════════════════════════════════════════════════════
  
  output$exec_header <- renderUI({
    d   <- rv$division
    sub <- switch(d,
                  Residential = "Macro environment and competitive position for the Residential division (Shaw Floors, Anderson Tuftex, COREtec, SPC). Driven by HOUST, mortgage rates, and SPC category share shift.",
                  Commercial  = "Macro environment and competitive position for the Commercial division (Patcraft, Philadelphia, Shaw Contract). Driven by AIA Architecture Billings Index and total construction spending.",
                  Turf        = "Macro environment for the Turf & Specialty division (Shaw Sports Turf, Southwest Greens, Shawgrass). Driven by infrastructure spending, stadium replacement cycles, and municipal budgets."
    )
    section_header(
      paste0("Executive Summary — ", d, " Division"),
      subtitle    = sub,
      badge_text  = if (rv$using_live) "Live FRED" else "Q1 2026 Estimates",
      badge_color = if (rv$using_live) "green" else "amber"
    )
  })
  
  output$exec_kpi_row <- renderUI({
    d   <- macro()
    cfg <- DIVISION_CONFIG[[rv$division]]
    
    kpis <- mapply(function(series, lbl, sub) {
      vals <- d[[series]]
      vals <- vals[!is.na(vals)]
      v    <- if (length(vals) > 0) tail(vals, 1) else NA_real_
      prev <- if (length(vals) >= 13) vals[length(vals) - 12] else NA_real_
      yoy  <- if (!is.na(v) && !is.na(prev) && prev != 0)
        round((v / prev - 1) * 100, 1) else NA_real_
      
      val_fmt <- if (is.na(v)) "N/A"
      else if (series == "housing_starts") paste0(format(round(v), big.mark=","), "k")
      else if (series %in% c("fed_funds","mortgage_30")) paste0(round(v, 2), "%")
      else if (series == "construction") paste0("$", round(v/1000, 1), "T")
      else paste0(round(v, 1))
      
      delta_txt <- if (!is.na(yoy))
        paste0(ifelse(yoy >= 0, "\u2191 +", "\u2193 "), yoy, "% YoY")
      else if (!is.na(v)) as.character(round(v, 2))
      else "\u2014"
      
      good_high <- series %in% c("housing_starts","construction")
      delta_col <- if (is.na(yoy)) PAL$muted
      else if (good_high) ifelse(yoy > 0, PAL$green, PAL$amber)
      else ifelse(yoy < 0, PAL$green, PAL$amber)
      
      kpi_card(lbl, val_fmt, delta_txt, delta_col, sub)
    }, cfg$kpi_series, cfg$kpi_labels, cfg$kpi_subs, SIMPLIFY = FALSE)
    
    rev_card <- kpi_card(
      "Shaw Rev. Est. \u2605", "$6.2B est.", "\u2191 +1.3% est. YoY", PAL$green,
      "\u26a0 Analyst estimate — not reported"
    )
    div(class = "kpi-row", rev_card, kpis[[1]], kpis[[2]], kpis[[3]], kpis[[4]], kpis[[5]])
  })
  
  output$exec_primary_chart_title <- renderUI({
    switch(rv$division,
           Residential = tags$span("Residential Demand — Housing Starts & Shaw Rev Est. (YoY %) \u2022 30-Yr Mortgage actual + FOMC implied fwd (level %, right axis)"),
           Commercial  = tags$span("Commercial Demand — Construction & Shaw Rev Est. (YoY %) \u2022 Fed Funds actual + FOMC 1-yr dot plot (level %, right axis)"),
           Turf        = tags$span("Turf Demand — Construction & CPI (YoY %) \u2022 Fed Funds actual + FOMC 1-yr dot plot (level %, right axis)")
    )
  })
  
  output$exec_secondary_chart_title <- renderUI({
    switch(rv$division,
           Residential = tags$span("Input Costs — PPI Resins, Fibers & Freight (YoY %) \u2022 Fed Funds level (right axis)"),
           Commercial  = tags$span("Input & Rate Environment — Construction & Freight (YoY %) \u2022 Fed Funds level (right axis)"),
           Turf        = tags$span("Turf Cost Drivers — Construction, CPI & Freight (YoY %) \u2022 Fed Funds level (right axis)")
    )
  })
  
  # Helper: YoY % change (12-month lag on monthly data)
  yoy_pct <- function(x, lag = 12) {
    n   <- length(x)
    out <- rep(NA_real_, n)
    for (i in (lag + 1):n) {
      prev <- x[i - lag]
      if (!is.na(prev) && prev != 0) out[i] <- round((x[i] / prev - 1) * 100, 1)
    }
    out
  }
  
  # ── Lag correlation: 30-yr mortgage (lead) vs housing starts (follow) ───────
  # 9-month lead chosen from academic literature on mortgage rate → housing permit
  # → start transmission lag (Kearl 1979; Glaeser et al. 2008; FRBSF 2022).
  # Trailing 48-month window captures a full rate cycle without over-anchoring to
  # the 2020 COVID distortion. Used in exec_rev_insight callout and chart note.
  lag_cor_res <- reactive({
    req(rv$macro)
    d     <- macro() |> arrange(date) |> filter(!is.na(housing_starts), !is.na(mortgage_30))
    lag_n <- 9L
    if (nrow(d) < lag_n + 13L) return(list(r = NA_real_, r_sq = NA_real_))
    win  <- min(nrow(d) - lag_n, 48L)
    x_hs <- tail(d$housing_starts, win)
    y_m  <- d$mortgage_30[(nrow(d) - win - lag_n + 1L):(nrow(d) - lag_n)]
    r_val <- suppressWarnings(cor(x_hs, y_m, use = "complete.obs"))
    if (is.na(r_val)) return(list(r = NA_real_, r_sq = NA_real_))
    list(r = round(r_val, 2), r_sq = round(r_val^2 * 100, 0))
  })
  
  # ── exec_rev_housing ────────────────────────────────────────────────────────
  # Dual y-axis layout:
  #   Left  (y1) — YoY % change: Shaw Rev Est., Housing Starts, Construction, CPI
  #   Right (y2) — Rate level %:
  #     Residential: 30-Yr Mortgage actual (solid) + implied fwd from FOMC (dashed)
  #                  Implied = FEDTARC1 (1-yr dot plot) + 250 bps historical spread
  #     Commercial/Turf: Fed Funds actual (solid) + FEDTARC1 1-yr projection (dashed)
  # FEDTARC1 is quarterly (SEP meetings); step-held to monthly in global.R via fill().
  # Guard: if FEDTARC1 not available (no API key / series returns NULL), projection
  # traces are silently skipped — chart still renders with actual data only.
  output$exec_rev_housing <- renderPlotly({
    d <- macro() |> arrange(date)
    
    d <- d |> mutate(
      shaw_yoy   = yoy_pct(shaw_rev_est),
      houst_yoy  = yoy_pct(housing_starts),
      constr_yoy = yoy_pct(construction),
      cpi_yoy    = yoy_pct(cpi),
      implied_mort_fwd = if ("fed_proj_1yr" %in% names(d) && any(!is.na(d$fed_proj_1yr)))
        fed_proj_1yr + 2.5 else NA_real_
    )
    
    d <- d |> filter(!is.na(shaw_yoy))
    
    has_proj     <- "fed_proj_1yr" %in% names(d) && any(!is.na(d$fed_proj_1yr))
    has_mort_fwd <- "implied_mort_fwd" %in% names(d) && any(!is.na(d$implied_mort_fwd))
    
    # Last confirmed FRED observation for the primary demand series
    last_fred_date <- suppressWarnings(
      max(d$date[!is.na(d$housing_starts)], na.rm = TRUE)
    )
    if (is.infinite(last_fred_date)) last_fred_date <- max(d$date)
    
    # Actual data up to last FRED date
    d_actual <- d |> filter(date <= last_fred_date)
    
    # ── Forward projection — explicitly generated, not sliced from existing data ──
    # Problem with slicing: last_fred_date is usually the most recent row, so
    # filter(date >= last_fred_date) returns 0–1 rows — Plotly renders nothing.
    # Fix: generate 12 months of future dates and taper shaw_yoy toward the
    # base scenario annual growth rate (~5% for 2026E per SCENARIOS$base).
    last_actual_yoy <- tail(d_actual$shaw_yoy[!is.na(d_actual$shaw_yoy)], 1)
    target_yoy      <- 5.0   # base case ~5% — matches SCENARIOS$base 2026E growth
    proj_dates      <- seq.Date(last_fred_date, by = "month", length.out = 13)
    proj_yoy        <- c(last_actual_yoy,   # overlap point for visual continuity
                         seq(last_actual_yoy, target_yoy, length.out = 12))
    
    d_proj <- tibble(date = proj_dates, shaw_yoy = proj_yoy)
    # ─────────────────────────────────────────────────────────────────────────────
    
    # Shared axis definitions
    date_xaxis <- list(
      type = "date", dtick = "M3", tickformat = "%b '%y",
      tickangle = -30, gridcolor = PAL$border, zeroline = FALSE,
      tickfont = list(color = PAL$muted, size = 10)
    )
    yax_left <- list(
      title      = list(text = "YoY % Change", font = list(size = 9, color = PAL$muted)),
      ticksuffix = "%", gridcolor = PAL$border, zeroline = TRUE,
      zerolinecolor = PAL$border, zerolinewidth = 1,
      tickfont   = list(color = PAL$muted, size = 10)
    )
    zero_shape <- list(list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
      line = list(color = PAL$border, width = 1, dash = "dot")
    ))
    legend_cfg <- list(bgcolor = "rgba(0,0,0,0)", font = list(color = PAL$muted, size = 10),
                       orientation = "h", y = -0.28)
    
    if (rv$division == "Residential") {
      
      yax_right_mort <- list(
        title      = list(
          text = if (has_mort_fwd)
            "Mortgage Rate %: actual (solid) / FOMC implied fwd (dashed) \u2192"
          else "30-Yr Mortgage Rate (%) \u2192",
          font = list(size = 9, color = PAL$amber)
        ),
        overlaying = "y", side = "right",
        ticksuffix = "%", showgrid = FALSE, zeroline = FALSE,
        tickfont   = list(color = PAL$amber, size = 10), rangemode = "normal"
      )
      
      p <- plot_ly() |>
        add_lines(data = d_actual, x = ~date, y = ~shaw_yoy,
                  name = "Shaw Rev Est. YoY \u2605",
                  line = list(color = PAL$accent, width = 2.5),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>Shaw Rev Est. YoY</extra>") |>
        add_lines(data = d_proj, x = ~date, y = ~shaw_yoy,
                  name = "Shaw Rev Est. YoY (E \u2014 fwd proj.)",
                  line = list(color = PAL$accent, width = 1.8, dash = "dot"),
                  hovertemplate = paste0(
                    "%{x|%b '%y}: %{y:+.1f}% (proj.)<br>",
                    "<i>Tapered to base case ~5% \u2014 not a FRED series</i>",
                    "<extra>Shaw Rev Est. (E)</extra>"
                  )) |>
        add_lines(data = d, x = ~date, y = ~houst_yoy,
                  name = "Housing Starts YoY (HOUST)",
                  line = list(color = PAL$blue, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>HOUST YoY</extra>") |>
        add_lines(data = d, x = ~date, y = ~mortgage_30,
                  name = "30-Yr Mortgage % (level \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$amber, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:.2f}%<extra>MORTGAGE30US level</extra>")
      
      if (has_mort_fwd)
        p <- p |>
        add_lines(data = d, x = ~date, y = ~implied_mort_fwd,
                  name = "Implied Fwd Mortgage (FOMC +250\u00a0bps \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$amber, width = 1.4, dash = "dash"),
                  hovertemplate = paste0(
                    "%{x|%b '%y}: %{y:.2f}% (implied)<br>",
                    "<i>FEDTARC1 + 250 bps spread</i>",
                    "<extra>FOMC implied fwd mortgage</extra>"
                  ))
      
      p |> plotly_dark(xlab = "", ylab = "YoY % Change") |>
        layout(xaxis = date_xaxis, yaxis = yax_left, yaxis2 = yax_right_mort,
               shapes = zero_shape, legend = legend_cfg)
      
    } else if (rv$division == "Commercial") {
      
      yax_right_ff <- list(
        title      = list(
          text = if (has_proj) "Fed Funds %: actual (solid) / FOMC 1-yr proj. (dashed) \u2192"
          else "Fed Funds Rate (%) \u2192",
          font = list(size = 9, color = PAL$green)
        ),
        overlaying = "y", side = "right",
        ticksuffix = "%", showgrid = FALSE, zeroline = FALSE,
        tickfont   = list(color = PAL$green, size = 10), rangemode = "normal"
      )
      
      p <- plot_ly() |>
        add_lines(data = d_actual, x = ~date, y = ~shaw_yoy,
                  name = "Shaw Rev Est. YoY \u2605",
                  line = list(color = PAL$accent, width = 2.5),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>Shaw Rev Est. YoY</extra>") |>
        add_lines(data = d_proj, x = ~date, y = ~shaw_yoy,
                  name = "Shaw Rev Est. YoY (E \u2014 fwd proj.)",
                  line = list(color = PAL$accent, width = 1.8, dash = "dot"),
                  hovertemplate = paste0(
                    "%{x|%b '%y}: %{y:+.1f}% (proj.)<br>",
                    "<i>Tapered to base case ~5% \u2014 not a FRED series</i>",
                    "<extra>Shaw Rev Est. (E)</extra>"
                  )) |>
        add_lines(data = d, x = ~date, y = ~constr_yoy,
                  name = "Construction Spend YoY (TTLCONS)",
                  line = list(color = PAL$blue, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>TTLCONS YoY</extra>") |>
        add_lines(data = d, x = ~date, y = ~fed_funds,
                  name = "Fed Funds % (level \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$green, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:.2f}%<extra>FEDFUNDS level</extra>")
      
      if (has_proj)
        p <- p |>
        add_lines(data = d, x = ~date, y = ~fed_proj_1yr,
                  name = "FOMC 1-Yr Projection (dot plot \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$green, width = 1.4, dash = "dash"),
                  hovertemplate = paste0(
                    "%{x|%b '%y}: %{y:.2f}% (FOMC proj.)<br>",
                    "<i>SEP median, step-held monthly</i>",
                    "<extra>FEDTARC1</extra>"
                  ))
      
      p |> plotly_dark(xlab = "", ylab = "YoY % Change") |>
        layout(xaxis = date_xaxis, yaxis = yax_left, yaxis2 = yax_right_ff,
               shapes = zero_shape, legend = legend_cfg)
      
    } else {   # Turf
      
      yax_right_turf <- list(
        title      = list(
          text = if (has_proj) "Fed Funds %: actual (solid) / FOMC 1-yr proj. (dashed) \u2192"
          else "Fed Funds Rate (%) \u2192",
          font = list(size = 9, color = PAL$amber)
        ),
        overlaying = "y", side = "right",
        ticksuffix = "%", showgrid = FALSE, zeroline = FALSE,
        tickfont   = list(color = PAL$amber, size = 10), rangemode = "normal"
      )
      
      p <- plot_ly() |>
        add_lines(data = d_actual, x = ~date, y = ~shaw_yoy,
                  name = "Shaw Rev Est. YoY \u2605",
                  line = list(color = PAL$accent, width = 2.5),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>Shaw Rev Est. YoY</extra>") |>
        add_lines(data = d_proj, x = ~date, y = ~shaw_yoy,
                  name = "Shaw Rev Est. YoY (E \u2014 fwd proj.)",
                  line = list(color = PAL$accent, width = 1.8, dash = "dot"),
                  hovertemplate = paste0(
                    "%{x|%b '%y}: %{y:+.1f}% (proj.)<br>",
                    "<i>Tapered to base case ~5% \u2014 not a FRED series</i>",
                    "<extra>Shaw Rev Est. (E)</extra>"
                  )) |>
        add_lines(data = d, x = ~date, y = ~constr_yoy,
                  name = "Construction Spend YoY (TTLCONS)",
                  line = list(color = PAL$green, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>TTLCONS YoY</extra>") |>
        add_lines(data = d, x = ~date, y = ~cpi_yoy,
                  name = "CPI YoY (CPIAUCSL)",
                  line = list(color = PAL$muted, width = 1.5, dash = "dash"),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>CPI YoY</extra>") |>
        add_lines(data = d, x = ~date, y = ~fed_funds,
                  name = "Fed Funds % (level \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$amber, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:.2f}%<extra>FEDFUNDS level</extra>")
      
      if (has_proj)
        p <- p |>
        add_lines(data = d, x = ~date, y = ~fed_proj_1yr,
                  name = "FOMC 1-Yr Projection (dot plot \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$amber, width = 1.4, dash = "dash"),
                  hovertemplate = paste0(
                    "%{x|%b '%y}: %{y:.2f}% (FOMC proj.)<br>",
                    "<i>SEP median, step-held monthly</i>",
                    "<extra>FEDTARC1</extra>"
                  ))
      
      p |> plotly_dark(xlab = "", ylab = "YoY % Change") |>
        layout(xaxis = date_xaxis, yaxis = yax_left, yaxis2 = yax_right_turf,
               shapes = zero_shape, legend = legend_cfg)
    }
  })
  
  output$exec_rev_insight <- renderUI({
    # ── Lag correlation callout (Residential only) ─────────────────────────
    # Shows rolling 48-month Pearson r between 30-yr mortgage (9-month lead)
    # and housing starts. Gives the CEO a single statistical anchor for why
    # the mortgage level trace belongs on the chart.
    cor_callout <- if (rv$division == "Residential") {
      lc <- lag_cor_res()
      if (!is.na(lc$r))
        div(style = paste0("margin-top:10px; padding:10px 14px; ",
                           "border-top:1px solid ", PAL$border, "; ",
                           "border-left:3px solid ", PAL$gold, ";"),
            tags$span(style = paste0("font-size:10px; color:", PAL$gold,
                                     "; font-weight:bold; letter-spacing:0.1em;",
                                     " text-transform:uppercase;"),
                      "Lag Correlation \u2014 trailing 48\u00a0mo. | "),
            tags$span(style = paste0("font-size:11px; color:", PAL$text, ";"),
                      paste0("HOUST vs. 30-yr mortgage lagged 9\u00a0months: ",
                             "r\u00a0=\u00a0", lc$r, "  |  ",
                             "R\u00b2\u00a0\u2248\u00a0", lc$r_sq, "% \u2014 ",
                             "mortgage rate level explains ~", lc$r_sq,
                             "% of housing starts variance in this window. ",
                             "Implied fwd mortgage (FOMC dot plot +\u00a0250\u00a0bps) ",
                             "gives a forward read on when that drag lifts. ",
                             "At ~5.5% the refi/purchase threshold historically unlocks."
                      )
            )
        )
    } else NULL
    
    base_txt <- switch(rv$division,
                       Residential = paste0(
                         "Housing starts and Shaw revenue track closely with a ~1-quarter lag (r\u00a0\u2248\u00a00.87 modeled). ",
                         "The 2022\u201323 decline drove revenue compression; the 2024\u201325 recovery is visible in both series. ",
                         "Right axis: 30-yr mortgage actual level (solid amber) and implied forward rate from ",
                         "FOMC 1-yr dot-plot + 250\u00a0bps historical spread (dashed). ",
                         "Watch implied fwd for the \u22645.5% unlock threshold."
                       ),
                       Commercial = paste0(
                         "Construction spending YoY is the best forward proxy for Shaw's commercial division. ",
                         "The 2025 recovery in both series is visible; ABI crossing 50 in late 2025 implies ",
                         "continued growth through H1\u2013H2 2026. ",
                         "Right axis: Fed Funds actual (solid) and FOMC 1-yr dot-plot projection (dashed) \u2014 ",
                         "the gap between them signals whether market pricing is ahead of or behind Fed guidance. ",
                         "A downward-stepping projection directly reduces commercial project hurdle rates."
                       ),
                       Turf = paste0(
                         "Turf demand is less correlated with CPI or construction spend than residential \u2014 ",
                         "primary driver is infrastructure budget cycles and turf replacement schedules (~8\u201310\u00a0yr). ",
                         "Right axis: Fed Funds actual and FOMC 1-yr projection \u2014 relevant for muni ",
                         "project financing cost and bond-funded stadium/parks capital programs."
                       )
    )
    
    tagList(
      insight_box(base_txt, "accent", "Chart Note"),
      cor_callout
    )
  })
  
  output$exec_rev_note <- renderUI({
    data_note_ui(paste0(
      "Shaw revenue (\u2605) is an analyst estimate \u2014 Shaw Industries does not report standalone financials. ",
      "YoY % = (current / prior year \u2212 1) \u00d7 100. First 12 months excluded (no prior-year base). ",
      "Dashed line (E) = forward projection: tapers from last actual YoY toward base-case ~5% ",
      "over 12 months \u2014 not a FRED series, for directional context only. ",
      "Live FRED auto-updates through most recent available month when FRED_API_KEY is set."
    ))
  })
  
  # ── exec_input_costs ────────────────────────────────────────────────────────
  # Dual y-axis layout:
  #   Left  (y1) — YoY % change: PPI Resins, PPI Fibers, Freight PPI, Construction, CPI
  #   Right (y2) — Rate level %: Fed Funds (the absolute rate level drives margin & cost
  #                of capital; its YoY delta is less actionable than knowing "we're at 3.6%")
  output$exec_input_costs <- renderPlotly({
    
    d <- macro() |>
      arrange(date) |>
      mutate(
        resins_yoy  = yoy_pct(ppi_plastics),
        fiber_yoy   = yoy_pct(ppi_fiber),
        freight_yoy = yoy_pct(freight_ppi),
        constr_yoy  = yoy_pct(construction),
        cpi_yoy     = yoy_pct(cpi)
        # fed_funds used as level directly — no yoy_pct
      ) |>
      filter(!is.na(resins_yoy) | !is.na(constr_yoy))
    
    # Shared x-axis
    date_xaxis <- list(
      type = "date", dtick = "M3", tickformat = "%b '%y",
      tickangle = -30, gridcolor = PAL$border, zeroline = FALSE,
      tickfont = list(color = PAL$muted, size = 10)
    )
    # Left y — YoY %
    yax_left <- list(
      title      = list(text = "YoY % Change", font = list(size = 9, color = PAL$muted)),
      ticksuffix = "%", gridcolor = PAL$border, zeroline = TRUE,
      zerolinecolor = PAL$border, zerolinewidth = 1,
      tickfont   = list(color = PAL$muted, size = 10)
    )
    # Right y — Fed Funds level; color matches its trace
    yax_right_ff <- list(
      title      = list(text = "Fed Funds Rate (%)", font = list(size = 9, color = PAL$blue)),
      overlaying = "y", side = "right",
      ticksuffix = "%", showgrid = FALSE, zeroline = FALSE,
      tickfont   = list(color = PAL$blue, size = 10),
      rangemode  = "normal"
    )
    zero_shape <- list(list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
      line = list(color = PAL$border, width = 1, dash = "dot")
    ))
    legend_cfg <- list(bgcolor = "rgba(0,0,0,0)", font = list(color = PAL$muted, size = 10),
                       orientation = "h", y = -0.22)
    
    if (rv$division == "Residential") {
      # Left:  PPI Resins YoY, PPI Fibers YoY, Freight YoY
      # Right: Fed Funds level — drives cost of debt for Shaw's working capital & customers
      plot_ly(d, x = ~date) |>
        add_lines(y = ~resins_yoy,
                  name = "PPI Resins YoY (WPU0911)",
                  line = list(color = PAL$red, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>WPU0911 YoY</extra>") |>
        add_lines(y = ~fiber_yoy,
                  name = "PPI Fibers YoY (WPU0713)",
                  line = list(color = PAL$purple, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>WPU0713 YoY</extra>") |>
        add_lines(y = ~freight_yoy,
                  name = "Freight PPI YoY",
                  line = list(color = PAL$amber, width = 1.5),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>Freight YoY</extra>") |>
        add_lines(y = ~fed_funds,
                  name = "Fed Funds % (level \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$blue, width = 1.8, dash = "dot"),
                  hovertemplate = "%{x|%b '%y}: %{y:.2f}%<extra>FEDFUNDS level</extra>") |>
        plotly_dark(xlab = "", ylab = "YoY % Change") |>
        layout(xaxis = date_xaxis, yaxis = yax_left, yaxis2 = yax_right_ff,
               shapes = zero_shape, legend = legend_cfg)
      
    } else if (rv$division == "Commercial") {
      # Left:  Construction Spend YoY, Freight YoY
      # Right: Fed Funds level — primary hurdle rate for commercial project finance
      plot_ly(d, x = ~date) |>
        add_lines(y = ~constr_yoy,
                  name = "Construction YoY (TTLCONS)",
                  line = list(color = PAL$blue, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>TTLCONS YoY</extra>") |>
        add_lines(y = ~freight_yoy,
                  name = "Freight PPI YoY",
                  line = list(color = PAL$amber, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>Freight YoY</extra>") |>
        add_lines(y = ~fed_funds,
                  name = "Fed Funds % (level \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$blue, width = 1.8, dash = "dot"),
                  hovertemplate = "%{x|%b '%y}: %{y:.2f}%<extra>FEDFUNDS level</extra>") |>
        plotly_dark(xlab = "", ylab = "YoY % Change") |>
        layout(xaxis = date_xaxis, yaxis = yax_left, yaxis2 = yax_right_ff,
               shapes = zero_shape, legend = legend_cfg)
      
    } else {   # Turf
      # Left:  Construction Spend YoY, CPI YoY, Freight YoY
      # Right: Fed Funds level — muni project financing costs
      plot_ly(d, x = ~date) |>
        add_lines(y = ~constr_yoy,
                  name = "Construction YoY (TTLCONS)",
                  line = list(color = PAL$green, width = 2),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>TTLCONS YoY</extra>") |>
        add_lines(y = ~cpi_yoy,
                  name = "CPI YoY (CPIAUCSL)",
                  line = list(color = PAL$muted, width = 1.5),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>CPI YoY</extra>") |>
        add_lines(y = ~freight_yoy,
                  name = "Freight PPI YoY",
                  line = list(color = PAL$amber, width = 1.5),
                  hovertemplate = "%{x|%b '%y}: %{y:+.1f}%<extra>Freight YoY</extra>") |>
        add_lines(y = ~fed_funds,
                  name = "Fed Funds % (level \u2192)",
                  yaxis = "y2",
                  line = list(color = PAL$blue, width = 1.8, dash = "dot"),
                  hovertemplate = "%{x|%b '%y}: %{y:.2f}%<extra>FEDFUNDS level</extra>") |>
        plotly_dark(xlab = "", ylab = "YoY % Change") |>
        layout(xaxis = date_xaxis, yaxis = yax_left, yaxis2 = yax_right_ff,
               shapes = zero_shape, legend = legend_cfg)
    }
  })
  
  output$exec_cost_insight <- renderUI({
    txt <- switch(rv$division,
                  Residential = paste0(
                    "The 2021\u201322 input cost spike (WPU0911 +36% YoY peak, WPU0713 +30% YoY peak) is clearly visible and has fully reversed. ",
                    "Both series are now running +3\u20134% YoY \u2014 well within normal range. ",
                    "Fed Funds level at 3.58% (down from 5.33%) is a meaningful margin tailwind as freight contract rates reprice."
                  ),
                  Commercial = paste0(
                    "Construction spend YoY is positive and accelerating \u2014 consistent with ABI crossing 50 in late 2025. ",
                    "Freight PPI YoY is rising modestly (+8% YoY) as freight demand recovers with construction activity. ",
                    "Fed Funds level at 3.58% reduces commercial project hurdle rates and improves pipeline conversion."
                  ),
                  Turf = paste0(
                    "Construction spend YoY is positive, supporting municipal turf procurement budgets. ",
                    "CPI YoY at ~+2.5% is near target \u2014 cost basis for turf projects is stable. ",
                    "Freight PPI YoY rising modestly but within tolerable range for turf installation economics."
                  )
    )
    insight_box(txt, "green", "Input Signal")
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
    p    <- plot_ly(FLOOR_SHARE, x = ~year)
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
  
  # ── ADD: Mekko / Marimekko Market Structure ─────────────────────────────────
  # MEKKO_RECTS is built once at startup in global.R from MEKKO_DATA.
  # Each cell is a closed scatter polygon (BL→BR→TR→TL→BL→NA) so Plotly fills
  # variable-width columns correctly — there is no native marimekko geom in plotly R.
  output$exec_mekko <- renderPlotly({
    d        <- MEKKO_RECTS
    players  <- c("Shaw (est.)", "Mohawk (est.)", "Others")
    pal_fill <- c("Shaw (est.)"   = paste0(PAL$accent, "d0"),
                  "Mohawk (est.)" = paste0(PAL$blue,   "d0"),
                  "Others"        = paste0(PAL$muted,  "55"))
    pal_line <- c("Shaw (est.)"   = PAL$accent,
                  "Mohawk (est.)" = PAL$blue,
                  "Others"        = PAL$border)
    
    p <- plot_ly()
    for (pl in players) {
      rows <- d[d$player == pl, ]
      xs <- ys <- txt <- c()
      for (i in seq_len(nrow(rows))) {
        r   <- rows[i, ]
        xs  <- c(xs, r$x0, r$x1, r$x1, r$x0, r$x0, NA)
        ys  <- c(ys, r$y0, r$y0, r$y1, r$y1, r$y0, NA)
        tip <- paste0("<b>", r$category, "</b><br>",
                      r$player, ": <b>", r$pct, "%</b><br>",
                      "Market: <b>$", r$mkt_b, "B</b><br>",
                      "Trend: <i>", r$trend, "</i><br>", r$note)
        txt <- c(txt, tip, tip, tip, tip, tip, NA)
      }
      p <- p |> add_trace(
        type = "scatter", mode = "lines", fill = "toself",
        x = xs, y = ys, name = pl,
        fillcolor = pal_fill[[pl]],
        line      = list(color = pal_line[[pl]], width = 1.2),
        text = txt, hoverinfo = "text", hoveron = "fills+points"
      )
    }
    
    # Category header annotations (above each column)
    cats <- d |>
      group_by(category) |>
      summarise(x_mid = first(x_mid), mkt_b = first(mkt_b),
                trend = first(trend), .groups = "drop")
    
    annots <- lapply(seq_len(nrow(cats)), function(i) {
      r     <- cats[i, ]
      t_col <- switch(r$trend,
                      shrinking = PAL$amber,
                      growing   = PAL$green,
                      PAL$muted)
      arrow <- switch(r$trend, shrinking = " \u2193", growing = " \u2191", "")
      list(x = r$x_mid, y = 104,
           text = paste0("<b>", r$category, arrow, "</b><br>$", r$mkt_b, "B"),
           showarrow = FALSE,
           font = list(size = 10, color = t_col, family = "Barlow"),
           xanchor = "center", yanchor = "bottom")
    })
    
    # Inline share % labels inside Shaw cells (only when cell is tall enough to read)
    shaw_rows <- d[d$player == "Shaw (est.)", ]
    for (i in seq_len(nrow(shaw_rows))) {
      r <- shaw_rows[i, ]
      if ((r$y1 - r$y0) >= 10) {
        annots[[length(annots) + 1]] <- list(
          x = r$x_mid, y = r$y_mid, text = paste0(r$pct, "%"),
          showarrow = FALSE,
          font = list(size = 9, color = "#ffffff", family = "Barlow"),
          xanchor = "center", yanchor = "middle"
        )
      }
    }
    
    p |> layout(
      plot_bgcolor  = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
      font          = list(color = PAL$muted, family = "Barlow", size = 10),
      xaxis = list(
        tickvals = cats$x_mid,
        ticktext = cats$category,
        tickfont = list(color = PAL$muted, size = 10),
        showgrid = FALSE, zeroline = FALSE, range = c(-0.01, 1.01),
        title    = list(
          text = "Column width proportional to market size ($B, 2026E est.)",
          font = list(size = 9, color = PAL$muted)
        )
      ),
      yaxis = list(
        title     = list(text = "Share within category (%)",
                         font = list(size = 9, color = PAL$muted)),
        range     = c(0, 116), ticksuffix = "%",
        gridcolor = PAL$border,
        tickfont  = list(color = PAL$muted, size = 10),
        zeroline  = FALSE
      ),
      legend      = list(bgcolor = "rgba(0,0,0,0)",
                         font = list(color = PAL$muted, size = 10),
                         orientation = "h", x = 0, y = -0.14),
      margin      = list(l = 50, r = 20, t = 40, b = 65),
      annotations = annots,
      hoverlabel  = list(bgcolor = "#0d1828",
                         font = list(color = PAL$text, size = 11),
                         bordercolor = PAL$border)
    ) |> config(displayModeBar = FALSE)
  })
  
  output$exec_mekko_note <- renderUI({
    data_note_ui(paste0(
      "Market size: Floor Covering Weekly + Catalina Research (~$31B U.S. retail, 2026E). ",
      "Mohawk share from MHK FY2023 10-K. Shaw share is analyst estimate (private company). ",
      "\u2193 = category shrinking as % of total flooring market; \u2191 = growing."
    ))
  })
  
  output$exec_narrative <- renderUI({
    cfg <- DIVISION_CONFIG[[rv$division]]
    n   <- cfg$narrative
    items <- list(
      list(lbl = "What Changed?",  txt = n$what_changed),
      list(lbl = "Why It Matters", txt = n$why_matters),
      list(lbl = "What It Means",  txt = n$what_means),
      list(lbl = "What to Do",     txt = n$what_to_do)
    )
    div(class = "narrative-grid",
        lapply(items, function(i)
          div(class = "narrative-item",
              div(class = "narrative-label", i$lbl),
              tags$p(style = paste0("font-size:13px; line-height:1.65; color:", PAL$text, ";"), i$txt)
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
        "Structural analysis of Shaw's competitive environment — viewed through the ",
        rv$division, " division lens. ",
        "Scores: 0 = no competitive pressure, 100 = extreme (unfavorable). ",
        "See 'Score Methodology' panel for full derivation."
      ),
      badge_text = "Qualitative Scoring"
    )
  })
  
  output$porter_division_note <- renderUI({
    cfg <- DIVISION_CONFIG[[rv$division]]
    div(style = paste0("margin-bottom:16px; padding:12px 16px; background:rgba(43,123,214,0.07);",
                       "border:1px solid rgba(43,123,214,0.2); border-radius:3px;"),
        div(style = paste0("font-size:10px; color:", PAL$accent,
                           "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:6px;"),
            rv$division, " Division — Most Relevant Forces"),
        tags$p(style = paste0("font-size:12px; color:", PAL$text, "; margin:0 0 6px; line-height:1.6;"),
               cfg$porter_note),
        div(style = paste0("font-size:11px; color:", PAL$muted, ";"),
            "Highlighted forces: ",
            paste(sapply(cfg$porter_focus, function(k) PORTER_FORCES[[k]]$label), collapse = " \u00b7 ")
        )
    )
  })
  
  output$porter_radar <- renderPlotly({
    d <- PORTER_RADAR
    plot_ly(type = "scatterpolar") |>
      add_trace(
        r     = c(d$shaw, d$shaw[1]),
        theta = c(d$force, d$force[1]),
        name  = "Shaw (est.)",
        mode  = "lines",
        fill  = "toself",
        fillcolor = paste0(PAL$accent, "22"),
        line  = list(color = PAL$accent, width = 2)
      ) |>
      add_trace(
        r     = c(d$benchmark, d$benchmark[1]),
        theta = c(d$force, d$force[1]),
        name  = "Mature Mfg. Avg.",
        mode  = "lines",
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
      list(key = "rivalry",     label = "Industry Rivalry",       score = 80, lv = "HIGH", col = PAL$red),
      list(key = "entrants",    label = "Threat of New Entrants", score = 35, lv = "LOW",  col = PAL$green),
      list(key = "suppliers",   label = "Power of Suppliers",     score = 62, lv = "MED",  col = PAL$amber),
      list(key = "buyers",      label = "Power of Buyers",        score = 55, lv = "MED",  col = PAL$amber),
      list(key = "substitutes", label = "Threat of Substitutes",  score = 72, lv = "HIGH", col = PAL$red)
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
            onclick = paste0("Shiny.setInputValue('porter_select','", f$key, "',{priority:'event'})"),
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
            list(lbl = "Hypothesis",      col = PAL$blue,   txt = f$hypothesis, items = NULL),
            list(lbl = "Evidence",        col = PAL$amber,  txt = NULL,          items = f$evidence),
            list(lbl = "Insight",         col = PAL$accent, txt = f$insight,     items = NULL),
            list(lbl = "Action Required", col = PAL$green,  txt = f$action,      items = NULL)
          ), function(sec) {
            div(style = paste0("background:", sec$col, "0e; border:1px solid ",
                               sec$col, "28; border-radius:3px; padding:14px;"),
                div(style = paste0("font-size:10px; color:", sec$col,
                                   "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:10px;"),
                    sec$lbl),
                if (!is.null(sec$items))
                  tags$ul(style = "padding-left:14px; margin:0;",
                          lapply(sec$items, function(e)
                            tags$li(style = "font-size:12px; line-height:1.65; margin-bottom:5px;", e)))
                else
                  tags$p(style = "font-size:12px; line-height:1.7; margin:0;", sec$txt)
            )
          })
      )
    )
  })
  
  output$porter_methodology <- renderUI({
    force_label_map <- c(
      rivalry     = "Industry Rivalry",
      entrants    = "Threat of New Entrants",
      suppliers   = "Power of Suppliers",
      buyers      = "Power of Buyers",
      substitutes = "Threat of Substitutes"
    )
    fl  <- force_label_map[[rv$porter_force]]
    row <- PORTER_SCORING_METHODOLOGY |> filter(force == fl)
    if (nrow(row) == 0) return(NULL)
    
    div(
      div(style = paste0("display:grid; grid-template-columns:1fr 1fr 1fr; gap:12px; margin-bottom:16px;"),
          div(style = paste0("background:", PAL$accent, "10; border:1px solid ", PAL$accent,
                             "30; border-radius:3px; padding:12px 16px;"),
              div(style = paste0("font-size:10px; color:", PAL$accent,
                                 "; letter-spacing:0.15em; text-transform:uppercase;"), "Shaw Score"),
              div(style = paste0("font-size:28px; font-family:'Playfair Display',serif; color:", PAL$accent, ";"),
                  paste0(row$shaw_score, "/100")),
              div(style = "height:4px; background:#1e2530; border-radius:2px; margin-top:6px;",
                  div(style = paste0("height:100%; width:", row$shaw_score,
                                     "%; background:", PAL$accent, "; border-radius:2px;")))
          ),
          div(style = paste0("background:", PAL$blue, "10; border:1px solid ", PAL$blue,
                             "30; border-radius:3px; padding:12px 16px;"),
              div(style = paste0("font-size:10px; color:", PAL$blue,
                                 "; letter-spacing:0.15em; text-transform:uppercase;"), "Benchmark Score"),
              div(style = paste0("font-size:28px; font-family:'Playfair Display',serif; color:", PAL$blue, ";"),
                  paste0(row$bench_score, "/100")),
              div(style = paste0("font-size:11px; color:", PAL$muted, "; margin-top:4px;"), "Mature U.S. Mfg. composite")
          ),
          div(style = paste0("background:#1e253022; border:1px solid ", PAL$border,
                             "; border-radius:3px; padding:12px 16px;"),
              div(style = paste0("font-size:10px; color:", PAL$muted,
                                 "; letter-spacing:0.15em; text-transform:uppercase;"), "Last Reviewed"),
              div(style = paste0("font-size:14px; color:", PAL$text, "; margin:6px 0 4px;"), row$last_reviewed),
              div(style = paste0("font-size:10px; color:", PAL$muted, "; font-style:italic;"),
                  "Trigger: ", row$review_trigger)
          )
      ),
      div(style = paste0("background:", PAL$panel, "; border:1px solid ", PAL$border,
                         "; border-radius:3px; padding:16px; margin-bottom:12px;"),
          div(style = paste0("font-size:10px; color:", PAL$accent,
                             "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:10px;"),
              "Score Derivation — Step by Step"),
          tags$pre(style = paste0("font-size:11px; line-height:1.8; color:", PAL$text,
                                  "; white-space:pre-wrap; margin:0; font-family:'Barlow',sans-serif;"),
                   row$score_components_shaw)
      ),
      div(style = paste0("background:#1e253015; border:1px solid ", PAL$border,
                         "; border-radius:3px; padding:14px; margin-bottom:12px;"),
          div(style = paste0("font-size:10px; color:", PAL$blue,
                             "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:8px;"),
              "Benchmark Rationale"),
          tags$p(style = paste0("font-size:12px; line-height:1.65; margin:0; color:", PAL$muted, ";"),
                 row$benchmark_rationale)
      ),
      div(style = paste0("background:#1e253010; border:1px solid ", PAL$border,
                         "; border-radius:3px; padding:14px;"),
          div(style = paste0("font-size:10px; color:", PAL$green,
                             "; letter-spacing:0.15em; text-transform:uppercase; margin-bottom:8px;"),
              "Primary Data Sources"),
          tags$p(style = "font-size:12px; line-height:1.65; margin:0;", row$primary_data_sources)
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
        "Risk probability reflects analyst judgement of materialization likelihood within 12 months. ",
        "Note: Shaw and Engineered Floors are separate, independent companies."
      ),
      badge_text = "Evidence-Based"
    )
  })
  
  output$swot_quadrant_row <- renderUI({
    quads   <- list(S = list(col=PAL$green), W = list(col=PAL$red),
                    O = list(col=PAL$blue),  T = list(col=PAL$amber))
    qlabels <- list(S="Strengths", W="Weaknesses", O="Opportunities", T="Threats")
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
          onclick = paste0("Shiny.setInputValue('swot_quad_sel','", q, "',{priority:'event'})"),
          div(style = paste0("font-size:28px; font-family:'Playfair Display',serif; color:",
                             if (active) qd$col else PAL$muted, "; margin-bottom:4px;"), q),
          div(style = paste0("font-size:13px; color:", if (active) PAL$text else PAL$muted, ";"), qlabels[[q]]),
          div(style = paste0("font-size:11px; color:", PAL$muted, "; margin-top:4px;"), paste(n, "factors"))
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
            r_col <- if (it$risk < 30) PAL$green else if (it$risk < 60) PAL$amber else PAL$red
            tc_cls <- switch(it$tier, HIGH = "badge-red", MED = "badge-amber", "badge-muted")
            div(style = paste0("background:rgba(255,255,255,0.015); border:1px solid ",
                               PAL$border, "; border-radius:3px; padding:16px 18px;"),
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
                                                   "%; background:", r_col, "; border-radius:2px;")))
                        )
                    )
                ),
                div(style = paste0("margin-top:14px; padding-top:14px; border-top:1px solid ",
                                   PAL$border, "; display:grid; grid-template-columns:1.2fr 1fr 0.8fr; gap:14px;"),
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
  
  observeEvent(rv$division, {
    cfg           <- DIVISION_CONFIG[[rv$division]]
    rv$assump_cat <- cfg$assump_cats[1]
  }, ignoreInit = TRUE)
  
  output$assump_header <- renderUI({
    cfg <- DIVISION_CONFIG[[rv$division]]
    section_header(
      paste0("Assumption Monitor — ", rv$division, " Division"),
      subtitle = paste0(
        "Tracking whether Shaw's key strategic assumptions are holding as of Q1 2026. ",
        "Filtered to ", rv$division, " division's most relevant categories: ",
        paste(cfg$assump_cats, collapse = ", "), ". ",
        "Use category filters to see all assumptions."
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
      div(style = paste0("background:", col, "12; border:1px solid ", col, "33;",
                         "border-radius:4px; padding:16px 20px; display:flex; align-items:center; gap:16px;"),
          div(style = paste0("font-size:36px; color:", col, "; font-family:'Playfair Display',serif;"), n),
          div(
            div(style = paste0("font-size:12px; color:", col, "; letter-spacing:0.1em; text-transform:uppercase;"), lbl),
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
    cats   <- c("ALL", sort(unique(ASSUMPTIONS$category)))
    active <- rv$assump_cat
    lapply(cats, function(cat) {
      is_act <- active == cat
      tags$button(cat,
                  style = paste0(
                    "background:", if (is_act) "rgba(43,123,214,0.15)" else "transparent", ";",
                    "border:1px solid ", if (is_act) PAL$accent else PAL$border, ";",
                    "border-radius:2px; padding:5px 14px; cursor:pointer;",
                    "font-size:11px; color:", if (is_act) PAL$accent else PAL$muted,
                    "; letter-spacing:0.1em; text-transform:uppercase;"
                  ),
                  onclick = paste0("Shiny.setInputValue('assump_cat_sel','", cat, "',{priority:'event'})")
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
          '<div style="height:100%;width:', round(pmin(current_v / target_v, 1.05) * 100),
          '%;background:', case_when(status=="green" ~ PAL$green, status=="yellow" ~ PAL$amber, TRUE ~ PAL$red),
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
    datatable(d_disp, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 15, dom = "t", ordering = TRUE,
                             columnDefs = list(list(className = "dt-left", targets = "_all"))),
              style = "auto")
  })
  
  output$assump_directive <- renderUI({
    txt <- switch(rv$division,
                  Residential = paste0(
                    "Residential's two watch items — LVT/SPC revenue share (est. 21% vs. 23% target) and SPC launch category share (est. 3.5% vs. 4.0% target) — both require near-term management focus. ",
                    "HOUST recovery toward 1.6M is on track but mortgage rate stickiness at ~6.34% remains the primary headwind. ",
                    "Monitor FRED HOUST and MORTGAGE30US monthly. SPC channel execution in the production builder segment is the most important near-term variable."
                  ),
                  Commercial = paste0(
                    "Commercial's key assumption — +3%+ YoY segment recovery — has been achieved in 2025. The ABI signal is now positive for H1\u2013H2 2026 demand at Patcraft and Philadelphia. ",
                    "Priority: convert the ABI signal into multi-year preferred-vendor agreements before the demand wave fully arrives. ",
                    "Monitor AIA ABI monthly and FRED TTLCONS nonresidential subcategory."
                  ),
                  Turf = paste0(
                    "Turf & Specialty assumptions are the most stable of the three divisions \u2014 minimal direct exposure to HOUST or mortgage cycle. ",
                    "Primary risks are Infrastructure Act funding pace and competitive positioning against FieldTurf (Tarkett). ",
                    "Monitor federal infrastructure grant disbursement schedules and municipal parks capital budgets."
                  )
    )
    insight_box(txt, "accent", "Division Directive")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # MODULE 5 — SCENARIO MODELING
  # ════════════════════════════════════════════════════════════════════════════
  
  output$scenario_header <- renderUI({
    section_header(
      "Scenario & Sensitivity Modeling",
      subtitle = paste0(
        "3\u20135 year revenue and EBITDA scenarios for Shaw Industries. ",
        "\u26a0 All figures are analyst estimates \u2014 Shaw does not report standalone financials. ",
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
          onclick = paste0("Shiny.setInputValue('scenario_sel_btn','", s, "',{priority:'event'})"),
          div(style = paste0("font-size:13px; color:", if (active) m$color else PAL$muted,
                             "; margin-bottom:6px;"), m$label),
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
        tags$p(style = "font-size:13px; line-height:1.7; margin:0 0 8px;", m$desc),
        div(style = paste0("font-size:11px; color:", PAL$muted, ";"), "Key FRED indicators: ", m$drivers)
    )
  })
  
  output$scenario_rev_chart <- renderPlotly({
    d <- SCENARIOS[[rv$scenario_sel]]
    m <- SCENARIO_META[[rv$scenario_sel]]
    plot_ly(d, x = ~year) |>
      add_trace(y = ~revenue_m, type = "scatter", mode = "lines+markers",
                name = "Revenue $M (est.)",
                line   = list(color = m$color, width = 2.5),
                marker = list(color = m$color, size = 7),
                fill   = "tozeroy", fillcolor = paste0(m$color, "14"),
                hovertemplate = "%{x}: $%{y:,.0f}M (est.)<extra></extra>") |>
      add_segments(x = "2024E", xend = "2024E", y = 4800, yend = 8500,
                   line = list(color = PAL$muted, dash = "dot", width = 1),
                   showlegend = FALSE, hoverinfo = "skip") |>
      plotly_dark(xlab = "", ylab = "Revenue $M (analyst est.)", legend = FALSE)
  })
  
  output$scenario_rev_note <- renderUI({
    data_note_ui(paste0(
      "2024A and 2025A* = analyst estimates of closed fiscal years. ",
      "Shaw Industries (Berkshire Hathaway subsidiary) does not publicly report ",
      "standalone revenue or EBITDA. 2026E\u20132028E = forward projections. ",
      "EBITDA margins modeled from public-company industry peers (MHK, AWI, TILE)."
    ))
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
                         paste0(ifelse(growth >= 0, "+", ""),
                                sprintf("%.1f%%", growth))),
        em_fmt  = paste0(ebitda_margin, "%"),
        eb_fmt  = paste0("$", format(ebitda_m, big.mark = ",")),
        epm_fmt = paste0(em_pct, "%")
      )
    
    # Save color vector BEFORE building display tibble — never put it in d_disp.
    # Column names starting with _ cause dplyr select() to silently malform the
    # tibble, which DT then rejects entirely ("No matching records found").
    gr_colors <- case_when(
      is.na(d$growth) ~ PAL$muted,
      d$growth >= 0   ~ PAL$green,
      TRUE            ~ PAL$red
    )
    
    d_disp <- d |>
      select(year, rev_fmt, gr_fmt, em_fmt, eb_fmt, epm_fmt)
    
    names(d_disp) <- c(
      "Year", "Revenue ($M est.)", "YoY Growth",
      "EBITDA Margin", "EBITDA ($M est.)", "EBITDA/Rev"
    )
    
    datatable(
      d_disp,
      rownames = FALSE,
      options  = list(
        dom        = "t",
        ordering   = FALSE,
        columnDefs = list(
          list(className = "dt-left", targets = "_all")
        )
      )
    ) |>
      formatStyle(
        columns = "YoY Growth",
        color   = styleEqual(
          levels = d_disp[["YoY Growth"]],
          values = gr_colors
        )
      ) |>
      formatStyle(
        columns    = names(d_disp),
        color      = PAL$text,
        background = "transparent"
      )
  })
  
  output$leading_indicators <- renderUI({
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px;",
        lapply(seq_len(nrow(LEADING_INDICATORS)), function(i) {
          r   <- LEADING_INDICATORS[i, ]
          col <- switch(r$status, green = PAL$green, amber = PAL$amber, PAL$red)
          div(style = paste0("border:1px solid ", PAL$border, "; border-radius:3px; padding:12px 14px;"),
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
                  tags$span(class = cls, s$src)),
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
                  onclick = paste0("Shiny.setInputValue('fb_cat_sel','", cat, "',{priority:'event'})")
      )
    })
  })
  
  observeEvent(input$fb_cat_sel, { rv$fb_cat <- input$fb_cat_sel })
  
  factbase_data <- reactive({
    d <- FACT_BASE_STATIC
    
    if (rv$using_live && !is.null(rv$macro)) {
      m <- rv$macro |> arrange(date)
      
      # BUG FIX: tail(rv$macro, 1) often has NAs because FRED series publish on
      # different lags. Pull the last non-NA observation for each column individually.
      last_val <- function(col) {
        v <- m[[col]]
        v <- v[!is.na(v)]
        if (length(v) == 0) NA_real_ else tail(v, 1)
      }
      last_date_for <- function(col) {
        rows <- m[!is.na(m[[col]]), ]
        if (nrow(rows) == 0) Sys.Date() else tail(rows$date, 1)
      }
      
      # Series name → list(value_string, date_of_last_obs)
      # Keys must EXACTLY match FACT_BASE_STATIC$series column
      live <- list(
        "Housing Starts: Total (HOUST)" = list(
          val  = paste0(format(round(last_val("housing_starts")), big.mark = ","), "k SAAR"),
          dt   = last_date_for("housing_starts")
        ),
        "30-Yr Fixed Mortgage Rate (MORTGAGE30US)" = list(
          val  = paste0(round(last_val("mortgage_30"), 2), "%"),
          dt   = last_date_for("mortgage_30")
        ),
        "Federal Funds Effective Rate (FEDFUNDS)" = list(
          val  = paste0(round(last_val("fed_funds"), 2), "%"),
          dt   = last_date_for("fed_funds")
        ),
        "Total Construction Spending (TTLCONS)" = list(
          val  = paste0("$", round(last_val("construction") / 1000, 1), "T ann."),
          dt   = last_date_for("construction")
        ),
        "PPI: Plastics Mat. & Resins (WPU0911)" = list(
          val  = as.character(round(last_val("ppi_plastics"), 1)),
          dt   = last_date_for("ppi_plastics")
        ),
        "PPI: Long-Dist. Freight Trucking (PCU484121484121)" = list(
          val  = as.character(round(last_val("freight_ppi"), 1)),
          dt   = last_date_for("freight_ppi")
        ),
        "CPI: All Urban Consumers (CPIAUCSL)" = list(
          val  = as.character(round(last_val("cpi"), 1)),
          dt   = last_date_for("cpi")
        )
      )
      
      for (nm in names(live)) {
        idx <- which(d$series == nm)
        v   <- live[[nm]]$val
        dt  <- live[[nm]]$dt
        if (length(idx) > 0 && !is.na(v) && v != "NA" && v != "NA%") {
          d$value[idx]  <- v
          d$note[idx]   <- paste0("Live FRED \u2014 as of ", format(dt, "%b %d %Y"))
          d$period[idx] <- format(dt, "%b %Y")
        }
      }
    }
    
    if (rv$using_equity && !is.null(rv$equity)) {
      # BUG FIX: keys must exactly match FACT_BASE_STATIC$series — no extra suffixes.
      # Previous keys had " — Public competitor" / " — Adjacent public" appended,
      # causing which(d$series == series_nm) to always return integer(0).
      eq_map <- list(
        "Mohawk Industries (MHK)"    = "MHK",
        "Interface Inc. (TILE)"      = "TILE",
        "Armstrong World Ind. (AWI)" = "AWI"
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
            d$period[idx] <- paste0("Live \u2014 ", smr$last_date)
            d$note[idx]   <- paste0("Live via tidyquant/Yahoo Finance. ",
                                    "52-wk: $", smr$low_52wk, "\u2013$", smr$high_52wk)
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
          'background:', case_when(status == "green" ~ PAL$green, status == "yellow" ~ PAL$amber, TRUE ~ PAL$red),
          ';margin-right:8px;vertical-align:middle;"></span>', series
        ),
        Source = paste0(
          '<span class="badge-',
          case_when(source == "FRED" ~ "accent", grepl("tidyquant", source) ~ "blue", TRUE ~ "green"),
          '">', source, '</span>'
        ),
        Category = paste0('<span class="badge-muted">', category, '</span>')
      ) |>
      select(Signal, Source, Category, value, change, period, note)
    names(d_disp) <- c("Signal","Source","Category","Value","Change","Period","Data Note")
    datatable(d_disp, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 15, dom = "t",
                             columnDefs = list(
                               list(className = "dt-left", targets = "_all"),
                               list(width = "220px", targets = 6)
                             )), style = "auto")
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
                    div(style = paste0("height:100%; width:", r$score, "%; background:", col, "; border-radius:3px;"))
                ),
                div(style = paste0("font-size:11px; color:", col, "; font-weight:bold; text-align:right;"), r$score)
            ),
            div(style = paste0("font-size:10px; color:", PAL$muted, "; margin-top:3px; padding-left:180px;"), r$note)
          )
        })
    )
  })
  
  # FIX 4: Engineered Floors is a SEPARATE, INDEPENDENT company (not Shaw-aligned).
  output$competitive_intel <- renderUI({
    intel <- list(
      list(co = "Mohawk Industries (MHK) — NYSE", st = "yellow",
           note = "Est. $10.6B revenue (FY2024). Gross margin ~30.1% (down from ~34% in FY2018). Largest direct competitor. Significant LVT/SPC capacity. Quarterly filings on SEC EDGAR."),
      list(co = "Interface, Inc. (TILE) — Nasdaq", st = "green",
           note = "Est. $1.41B revenue (FY2024). Gross margin ~38.5%. Commercial modular carpet specialist — not a direct residential competitor. Growing healthcare and corporate verticals."),
      list(co = "Armstrong World Industries (AWI) — NYSE", st = "yellow",
           note = "Est. $1.31B revenue (FY2024). Primarily ceiling and wall systems; flooring is secondary. Gross margin ~35.8%. Adjacent, not direct."),
      list(co = "Tarkett SA — private listing", st = "yellow",
           note = "~\u20AC2.9B revenue (2024 est.). European LVT leader with growing U.S. commercial presence. Monitor Census HS 3918 import volumes for competitive signal."),
      list(co = "Engineered Floors — private", st = "red",
           note = "Est. $1.5\u20132.0B revenue. Focused polyester residential carpet manufacturer based in Dalton, GA. Direct competitor in mid-market residential carpet. No public financials.")
    )
    div(style = "display:grid; gap:10px;",
        lapply(intel, function(c) {
          col <- switch(c$st, green = PAL$green, yellow = PAL$amber, PAL$red)
          div(style = paste0("display:flex; gap:12px; padding:10px 12px; background:rgba(255,255,255,0.02);",
                             "border-radius:3px; border:1px solid ", PAL$border, ";"),
              div(style = paste0("width:7px;height:7px;border-radius:50%;background:", col,
                                 ";margin-top:4px;flex-shrink:0;")),
              div(
                div(style = "font-size:12px; margin-bottom:4px;", c$co),
                div(style = paste0("font-size:11px; color:", PAL$muted, "; line-height:1.5;"), c$note)
              )
          )
        })
    )
  })
  
}