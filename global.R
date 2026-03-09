# ── Shaw Industries CEO Strategy Portal ──────────────────────────────────────
# global.R  |  Packages · Theme · API keys · FRED fetch · All static data
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(fredr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(shinyjs)
  library(scales)
  library(htmltools)
  library(httr)
  library(curl)
  if (requireNamespace("tidyquant", quietly = TRUE)) {
    library(tidyquant)
    TIDYQUANT_AVAILABLE <- TRUE
  } else {
    TIDYQUANT_AVAILABLE <- FALSE
  }
})

# ── Namespace conflict resolution ─────────────────────────────────────────────
# library(httr) masks plotly::config() with httr::config(), which returns an
# httr `request` object. Every |> layout() call downstream then fails with
# "no applicable method for 'layout' applied to class 'request'".
# Pin config and layout to plotly explicitly so all server.R chart code works
# without requiring plotly:: prefixes everywhere.
config <- plotly::config
layout <- plotly::layout

# ── API Keys from .Renviron ───────────────────────────────────────────────────
FRED_API_KEY   <- Sys.getenv("FRED_API_KEY")
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")
if (nchar(FRED_API_KEY) > 0) tryCatch(fredr_set_key(FRED_API_KEY), error = function(e) NULL)

# ── Network hardening — timeouts for slow connections ─────────────────────────
# fredr uses httr internally; set_config applies globally to all httr/tidyquant calls.
# 45s per request + 15s connect timeout chosen for slow residential connections.
# options(timeout) covers base R socket calls (getSymbols fallback path).
httr::set_config(httr::timeout(45))
httr::set_config(httr::config(connecttimeout = 15L))
options(timeout = 120)

# ── Connectivity pre-check — run once at startup ──────────────────────────────
# Avoids the 3-retry x 1.4s fredr loop when the host is simply unreachable.
# App falls to mock data immediately rather than hanging on startup.
# 400/403 from FRED = host reached but test key rejected — still "reachable".
FRED_REACHABLE <- local({
  if (nchar(FRED_API_KEY) == 0) return(FALSE)
  tryCatch({
    h <- curl::new_handle(timeout_ms = 15000L)
    r <- curl::curl_fetch_memory(
      "https://api.stlouisfed.org/fred/series?series_id=FEDFUNDS&api_key=test&file_type=json",
      handle = h
    )
    r$status_code %in% c(200L, 400L, 403L)
  }, error = function(e) {
    message("FRED unreachable: ", conditionMessage(e))
    FALSE
  })
})

YAHOO_REACHABLE <- local({
  if (!TIDYQUANT_AVAILABLE) return(FALSE)
  tryCatch({
    h <- curl::new_handle(timeout_ms = 15000L)
    r <- curl::curl_fetch_memory(
      "https://query1.finance.yahoo.com/v8/finance/chart/MHK?interval=1d&range=1d",
      handle = h
    )
    r$status_code < 500L
  }, error = function(e) {
    message("Yahoo Finance unreachable: ", conditionMessage(e))
    FALSE
  })
})

message("Network check — FRED reachable: ", FRED_REACHABLE,
        " | Yahoo reachable: ", YAHOO_REACHABLE)

# ── Shaw Color Palette ────────────────────────────────────────────────────────
PAL <- list(
  bg     = "#060b14",
  panel  = "#0e1929",
  border = "#1e3352",
  text   = "#e8e4de",
  muted  = "#7a8fa8",
  accent = "#2b7bd6",
  blue   = "#4aa3e8",
  green  = "#2dba7a",
  red    = "#d95f5f",
  amber  = "#e8a030",
  gold   = "#c8a84b",
  purple = "#9070c0"
)

SHAW_LOGO_URL <- "https://commons.wikimedia.org/wiki/Special:FilePath/Shaw_Corporate_Logo.jpg"

# ── bslib Theme ───────────────────────────────────────────────────────────────
shaw_theme <- bs_theme(
  version   = 5,
  bg        = PAL$bg,   fg      = PAL$text,
  primary   = PAL$accent, secondary = PAL$muted,
  success   = PAL$green, danger  = PAL$red,
  warning   = PAL$amber, info    = PAL$blue,
  "font-size-base"                    = "0.875rem",
  "card-bg"                           = PAL$panel,
  "card-border-color"                 = PAL$border,
  "card-cap-bg"                       = "transparent",
  "card-cap-color"                    = PAL$accent,
  "input-bg"                          = PAL$panel,
  "input-color"                       = PAL$text,
  "input-border-color"                = PAL$border,
  "input-focus-border-color"          = PAL$accent,
  "input-placeholder-color"           = "#2a3a50",
  "nav-tabs-border-color"             = PAL$border,
  "nav-tabs-link-active-color"        = PAL$accent,
  "nav-tabs-link-active-bg"           = "transparent",
  "nav-tabs-link-active-border-color" = PAL$accent,
  "nav-link-color"                    = PAL$muted,
  "nav-link-hover-color"              = PAL$blue,
  "table-color"                       = PAL$text,
  "table-bg"                          = "transparent",
  "table-border-color"                = PAL$border,
  "body-bg"                           = PAL$bg,
  "body-color"                        = PAL$text
)

# ── Login Credentials ─────────────────────────────────────────────────────────
VALID_USERS <- list(
  list(user = "demo", pass = "demo", role = "Demo")
)

# ── FRED Series Registry ──────────────────────────────────────────────────────
# All series IDs are verified real FRED series.
# HOUST         : Housing Starts: Total: New Privately Owned Units Started (Ths. SAAR)
# FEDFUNDS      : Federal Funds Effective Rate (%, monthly)
# MORTGAGE30US  : 30-Year Fixed Rate Mortgage Average in the U.S. (%, weekly -> qtr avg)
# WPU0672       : PPI by Commodity: Rubber and Plastics: Plastics Products (Index 1982=100)
# DCOILWTICO    : Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, OK ($/bbl, daily)
#                 ADDED March 9, 2026 — Hormuz crisis makes WTI the #1 leading indicator
#                 for Shaw's input cost trajectory. WPU0672 lags crude by ~4-8 weeks.
# CPIAUCSL      : CPI All Urban Consumers: All Items in U.S. City Average (Index 1982-84=100)
# TTLCONS       : Total Construction Spending: Total ($M SAAR)
# PCU484121484121: PPI: Truck Transportation of Long-Distance General Freight (Index 2012=100)
# FRED_SERIES <- list(
#   housing_starts = list(id = "HOUST",            freq = "q", agg = "avg"),
#   fed_funds      = list(id = "FEDFUNDS",         freq = "q", agg = "avg"),
#   mortgage_30    = list(id = "MORTGAGE30US",     freq = "q", agg = "avg"),
#   ppi_plastics   = list(id = "WPU0672",          freq = "q", agg = "avg"),
#   wti_crude      = list(id = "DCOILWTICO",       freq = "q", agg = "avg"),  # Added Mar 2026
#   cpi            = list(id = "CPIAUCSL",         freq = "q", agg = "avg"),
#   construction   = list(id = "TTLCONS",          freq = "q", agg = "avg"),
#   freight_ppi    = list(id = "PCU484121484121",  freq = "q", agg = "avg")
# )

FRED_SERIES <- list(
  housing_starts = list(id = "HOUST",            freq = "m", agg = "avg"),
  fed_funds      = list(id = "FEDFUNDS",         freq = "m", agg = "avg"),
  mortgage_30    = list(id = "MORTGAGE30US",     freq = "m", agg = "avg"),
  ppi_resins     = list(id = "WPU0911",          freq = "m", agg = "avg"),
  ppi_fiber      = list(id = "WPU0713",          freq = "m", agg = "avg"),
  cpi            = list(id = "CPIAUCSL",         freq = "m", agg = "avg"),
  wti_crude      = list(id = "DCOILWTICO",       freq = "m", agg = "avg"),  # Added Mar 2026
  construction   = list(id = "TTLCONS",          freq = "m", agg = "avg"),
  freight_ppi    = list(id = "PCU484121484121",  freq = "m", agg = "avg"),
  fed_target_hi  = list(id = "DFEDTARU",         freq = "m", agg = "avg"),
  fed_target_lo  = list(id = "DFEDTARL",         freq = "m", agg = "avg")
)


# ── FRED Fetch ────────────────────────────────────────────────────────────────
fetch_one_series <- function(series_id, start = "2019-01-01", freq = "m", agg = "avg") {
  tryCatch({
    httr::with_config(httr::timeout(45), {
      df <- fredr(series_id = series_id,
                  observation_start  = as.Date(start),
                  frequency          = freq,
                  aggregation_method = agg)
      df |> select(date, value) |> filter(!is.na(value)) |>
        mutate(date = floor_date(as.Date(date), "month"))
    })
  }, error = function(e) {
    message("fredr failed for ", series_id, ": ", conditionMessage(e))
    NULL
  })
}

fetch_fred_data <- function(start = "2019-01-01") {
  if (nchar(FRED_API_KEY) == 0 || !FRED_REACHABLE) return(NULL)
  base  <- tibble(date = seq.Date(as.Date(start),
                                  floor_date(Sys.Date(), "month"), by = "month"))
  macro <- base
  for (nm in names(FRED_SERIES)) {
    s   <- FRED_SERIES[[nm]]
    raw <- fetch_one_series(s$id, start = start, freq = s$freq, agg = s$agg)
    if (!is.null(raw)) {
      raw   <- rename(raw, !!nm := value)
      macro <- left_join(macro, raw, by = "date")
    } else {
      macro[[nm]] <- NA_real_
    }
  }
  
  # ppi_resins -> ppi_plastics alias for chart compatibility
  if ("ppi_resins" %in% names(macro) && !"ppi_plastics" %in% names(macro))
    macro <- rename(macro, ppi_plastics = ppi_resins)
  
  # ── Derive implied FOMC forward rate path ─────────────────────────────────
  # FEDTARC1 does NOT exist on FRED — SEP dot-plot is PDF-only.
  # Method: take the latest FOMC lower-bound (fed_target_lo) as current floor,
  # step down 25 bps at months 3, 6, 9 (3 cuts over 18 months -> ~3.1% terminal).
  # Consistent with Q1 2026 market consensus pricing 2-3 cuts for 2026.
  # Labelled "Implied path (mkt consensus)" in all chart hovers — not official.
  if ("fed_target_lo" %in% names(macro) && any(!is.na(macro$fed_target_lo))) {
    last_floor <- tail(macro$fed_target_lo[!is.na(macro$fed_target_lo)], 1)
    last_dt    <- max(macro$date[!is.na(macro$fed_target_lo)], na.rm = TRUE)
    fwd_dates  <- seq.Date(last_dt %m+% months(1), by = "month", length.out = 18)
    cuts_at    <- c(3, 6, 9)
    fwd_rate   <- last_floor
    fwd_vals   <- sapply(seq_along(fwd_dates), function(i) {
      if (i %in% cuts_at) fwd_rate <<- fwd_rate - 0.25
      fwd_rate
    })
    fwd_tbl <- tibble(date = fwd_dates, fed_proj_fwd = fwd_vals)
    macro   <- left_join(macro, fwd_tbl, by = "date")
  } else {
    macro$fed_proj_fwd <- NA_real_
  }
  
  # Implied fwd 30-yr mortgage = FOMC fwd path + 250 bps (historical spread: 220-280 bps)
  macro <- macro |>
    mutate(implied_mort_fwd = if_else(!is.na(fed_proj_fwd), fed_proj_fwd + 2.5, NA_real_))
  
  macro
}

# ── Equity Data (tidyquant / Yahoo Finance) ───────────────────────────────────
fetch_equity_data <- function(tickers = c("MHK","TILE","AWI"), lookback_days = 365 * 3) {
  if (!TIDYQUANT_AVAILABLE || !YAHOO_REACHABLE) return(NULL)
  start <- Sys.Date() - lookback_days
  out   <- list()
  for (tk in tickers) {
    tryCatch({
      df <- httr::with_config(httr::timeout(45), {
        tidyquant::tq_get(tk, from = start, to = Sys.Date(), get = "stock.prices")
      })
      if (!is.null(df) && nrow(df) > 0) out[[tk]] <- df
    }, error = function(e) {
      message("tidyquant failed for ", tk, ": ", conditionMessage(e))
    })
  }
  if (length(out) == 0) NULL else out
}

equity_summary <- function(eq_data, ticker) {
  if (is.null(eq_data) || is.null(eq_data[[ticker]])) return(NULL)
  d         <- eq_data[[ticker]]
  ytd_idx   <- which.min(abs(d$date - as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01"))))
  ytd_start <- d$close[ytd_idx]
  list(
    last_close  = round(tail(d$close, 1), 2),
    last_date   = format(tail(d$date, 1), "%b %d %Y"),
    high_52wk   = round(max(d$high, na.rm = TRUE), 2),
    low_52wk    = round(min(d$low,  na.rm = TRUE), 2),
    ytd_chg_pct = round((tail(d$close, 1) / ytd_start - 1) * 100, 1)
  )
}

# ── Mock Macro Data — Monthly, Jan 2019 through Dec 2025 ─────────────────────
.mock_qtr <- tibble(
  date = seq.Date(as.Date("2019-01-01"), as.Date("2025-10-01"), by = "quarter"),
  housing_starts = c(
    1172,1253,1269,1381, 1567,1072,1446,1555,
    1588,1643,1614,1679, 1736,1600,1445,1363,
    1324,1383,1435,1483, 1519,1353,1357,1380,
    1398,1424,1455,1482),
  fed_funds = c(
    2.41,2.38,2.18,1.75, 1.58,0.06,0.09,0.09,
    0.07,0.08,0.08,0.08, 0.20,1.21,3.08,4.10,
    4.65,5.08,5.33,5.33, 5.33,5.33,5.13,4.83,
    4.33,4.08,3.83,3.58),
  mortgage_30 = c(
    4.40,3.98,3.67,3.73, 3.50,3.23,2.94,2.77,
    2.87,3.00,2.96,3.11, 3.76,5.09,5.70,6.79,
    6.54,6.57,7.07,6.95, 6.97,7.06,6.95,6.79,
    6.65,6.55,6.42,6.34),
  ppi_plastics = c(
    198,197,198,200, 193,182,188,202,
    218,245,258,252, 260,268,252,234,
    220,212,208,205, 208,212,215,218,
    220,222,224,227),
  ppi_fiber = c(
    145,144,145,146, 140,132,138,148,
    158,178,188,183, 190,196,185,172,
    162,157,154,152, 154,157,159,161,
    163,165,167,169),
  construction = c(
    1280,1310,1322,1367, 1356,1270,1368,1454,
    1508,1560,1598,1655, 1754,1830,1883,1910,
    1896,1920,1956,1982, 2002,2032,2061,2078,
    2093,2115,2140,2168),
  cpi = c(
    253,256,256,258, 258,256,260,261,
    264,269,273,278, 284,293,296,298,
    300,304,305,307, 309,314,315,316,
    318,320,322,325),
  freight_ppi = c(
    117,119,120,122, 117,111,118,124,
    130,148,162,170, 174,175,168,155,
    144,138,133,129, 131,136,140,143,
    146,149,152,155),
  # FOMC target range bounds — mirrors fed_funds +/- 0.125 (25 bps corridor)
  fed_target_hi = c(
    2.50,2.50,2.25,2.00, 1.75,0.25,0.25,0.25,
    0.25,0.25,0.25,0.25, 0.50,1.50,3.25,4.25,
    4.75,5.25,5.50,5.50, 5.50,5.50,5.25,5.00,
    4.50,4.25,4.00,3.75),
  fed_target_lo = c(
    2.25,2.25,2.00,1.75, 1.50,0.00,0.00,0.00,
    0.00,0.00,0.00,0.00, 0.25,1.25,3.00,4.00,
    4.50,5.00,5.25,5.25, 5.25,5.25,5.00,4.75,
    4.25,4.00,3.75,3.50),
  shaw_rev_est = c(
    5200,5400,5350,5500, 5100,4700,5300,5600,
    5800,6100,6200,6450, 6700,6850,6700,6400,
    6100,5950,5900,5920, 5980,6050,6120,6200,
    6285,6380,6440,6520)
)

.monthly_dates <- seq.Date(as.Date("2019-01-01"), as.Date("2025-12-01"), by = "month")
.qtr_numeric   <- as.numeric(.mock_qtr$date)
.mo_numeric    <- as.numeric(.monthly_dates)

MOCK_MACRO <- tibble(date = .monthly_dates)
for (.col in setdiff(names(.mock_qtr), "date")) {
  MOCK_MACRO[[.col]] <- round(
    approx(.qtr_numeric, .mock_qtr[[.col]], xout = .mo_numeric, rule = 2)$y,
    if (.col %in% c("fed_funds","mortgage_30","fed_target_hi","fed_target_lo")) 2 else 1
  )
}
MOCK_MACRO$shaw_rev_est <- round(MOCK_MACRO$shaw_rev_est / 3)
MOCK_MACRO$label        <- format(MOCK_MACRO$date, "%b '%y")

# Derive implied forward path from mock target bounds (same logic as live fetch)
local({
  last_floor <- tail(MOCK_MACRO$fed_target_lo[!is.na(MOCK_MACRO$fed_target_lo)], 1)
  last_dt    <- max(MOCK_MACRO$date[!is.na(MOCK_MACRO$fed_target_lo)], na.rm = TRUE)
  fwd_dates  <- seq.Date(last_dt %m+% months(1), by = "month", length.out = 18)
  cuts_at    <- c(3, 6, 9)
  fwd_rate   <- last_floor
  fwd_vals   <- sapply(seq_along(fwd_dates), function(i) {
    if (i %in% cuts_at) fwd_rate <<- fwd_rate - 0.25
    fwd_rate
  })
  fwd_tbl <- tibble(date = fwd_dates, fed_proj_fwd = fwd_vals)
  MOCK_MACRO <<- left_join(MOCK_MACRO, fwd_tbl, by = "date") |>
    mutate(implied_mort_fwd = if_else(!is.na(fed_proj_fwd), fed_proj_fwd + 2.5, NA_real_))
})

rm(.mock_qtr, .monthly_dates, .qtr_numeric, .mo_numeric, .col)

# ── Floor Category Share ──────────────────────────────────────────────────────
FLOOR_SHARE <- tibble(
  year    = 2019:2026,
  carpet  = c(38,36,34,32,30,28,26,24),
  lvt_spc = c(16,18,21,24,27,30,33,36),
  hardwood= c(14,14,13,13,12,12,12,11),
  tile    = c(22,22,22,21,21,20,20,19),
  other   = c(10,10,10,10,10,10, 9,10)
)

# ── Division Configuration ────────────────────────────────────────────────────
DIVISION_CONFIG <- list(
  
  Residential = list(
    color       = PAL$accent,
    rev_share   = "~70% of estimated Shaw revenue",
    brands      = "Shaw Floors · Anderson Tuftex · COREtec · SPC",
    cycle       = "FRED HOUST + MORTGAGE30US",
    why         = "Residential flooring tracks housing starts and existing-home-sale turnover with a ~1-quarter lag. Every 100k change in HOUST moves total addressable flooring demand ~$350M industry-wide.",
    kpi_series  = c("housing_starts","mortgage_30","fed_funds","ppi_plastics","ppi_fiber","freight_ppi"),
    kpi_labels  = c("Housing Starts (HOUST)","30-Yr Mortgage","Fed Funds","PPI: Resins (WPU0911)","PPI: Fibers (WPU0713)","Freight PPI"),
    kpi_subs    = c("FRED HOUST — monthly","FRED MORTGAGE30US","FRED FEDFUNDS",
                    "WPU0911 — Plastics Mat. & Resins","WPU0713 — Synthetic Fibers","PCU484121484121"),
    chart_primary   = "housing_starts",
    chart_secondary = "mortgage_30",
    chart_p_label   = "Housing Starts (k SAAR)",
    chart_s_label   = "30-Yr Mortgage % x200",
    chart_p_color   = PAL$accent,
    chart_s_color   = PAL$amber,
    chart_title     = "Residential Demand Drivers — HOUST vs. Mortgage Rate",
    narrative = list(
      what_changed = "FEDFUNDS achieved <4.0% in Q4 2025. HOUST at 1,482k — recovering from 1,280k trough but still below the 1.6M assumption threshold. SPC launch (Feb 2025) tracking toward 4% category share. Carpet share est. 24% — continuing structural decline.",
      why_matters  = "Each 100k HOUST gain adds ~$90-95M to Shaw's estimated addressable demand at ~26-27% share. Mortgage rate stickiness at 6.34% (vs. 2.77% low) continues to suppress existing-home-sale turnover — the largest single driver of remodeling flooring spend.",
      what_means   = "2026 is the first year with a full rate-cut tailwind, recovering HOUST, and SPC in-market. Residential revenue recovery is on track in the base case but depends on mortgage rates continuing to fall toward the 5.5-6.0% range.",
      what_to_do   = "Accelerate SPC into the production builder channel — that is where carpet-to-hard-surface conversion is happening at volume. Lock preferred-vendor builder agreements before competitors. Reposition carpet to bedroom/multi-family premium."
    ),
    leading_inds = c("FRED MORTGAGE30US","FRED HOUST","NAHB Housing Market Index"),
    assump_cats  = c("Residential","Product Mix","Macro","Input Costs"),
    porter_focus = c("substitutes","rivalry"),
    porter_note  = "For residential, Threat of Substitutes (LVT/SPC displacing carpet) and Industry Rivalry are the dominant forces. Buyer Power is high in the production builder sub-channel."
  ),
  
  Commercial = list(
    color       = PAL$blue,
    rev_share   = "~25% of estimated Shaw revenue",
    brands      = "Patcraft · Philadelphia Commercial · Shaw Contract",
    cycle       = "AIA Architecture Billings Index + FRED TTLCONS",
    why         = "Commercial flooring lags the AIA Architecture Billings Index by 9-18 months. When ABI is above 50, Shaw's commercial brands see demand inflection ~3-5 quarters later. ABI crossed 50 in late 2025 — the forward pipeline is now positive.",
    kpi_series  = c("construction","fed_funds","cpi","freight_ppi","ppi_plastics","mortgage_30"),
    kpi_labels  = c("Total Construction ($B ann.)","Fed Funds Rate","CPI","Freight PPI","PPI: Resins","30-Yr Mortgage"),
    kpi_subs    = c("FRED TTLCONS — monthly","FRED FEDFUNDS","FRED CPIAUCSL",
                    "PCU484121484121","WPU0911","FRED MORTGAGE30US"),
    chart_primary   = "construction",
    chart_secondary = "fed_funds",
    chart_p_label   = "Total Construction $B ann.",
    chart_s_label   = "Fed Funds % x200",
    chart_p_color   = PAL$blue,
    chart_s_color   = PAL$amber,
    chart_title     = "Commercial Demand Drivers — Construction Spend vs. Fed Funds",
    narrative = list(
      what_changed = "AIA Architecture Billings Index crossed 50 in late 2025 for the first time since 2022. Total construction spending reached $2,168B ann. Commercial segment est. +3.4% YoY in 2025 — assumption achieved.",
      why_matters  = "Commercial flooring lags ABI by 9-18 months. With ABI now above 50, Shaw's commercial brands have forward visibility into H1-H2 2026 demand. Healthcare and hospitality are the strongest verticals.",
      what_means   = "The commercial cycle has turned. Shaw's commercial brands are entering a demand upswing with strong specification pipelines in healthcare and hospitality.",
      what_to_do   = "Staff up commercial sales for the healthcare and hospitality verticals now. Multi-year preferred-vendor agreements in these verticals provide margin stability vs. residential."
    ),
    leading_inds = c("AIA Architecture Billings Index","FRED TTLCONS","FRED FEDFUNDS"),
    assump_cats  = c("Commercial","Macro","Logistics"),
    porter_focus = c("buyers","rivalry"),
    porter_note  = "For commercial, Buyer Power (structured multi-year RFPs) and Industry Rivalry are the key forces."
  ),
  
  Turf = list(
    color       = PAL$green,
    rev_share   = "~5% of estimated Shaw revenue",
    brands      = "Shaw Sports Turf · Southwest Greens · Shawgrass",
    cycle       = "Sports facility construction + Infrastructure Act funding",
    why         = "The turf and specialty segment is the least sensitive to the housing or mortgage cycle. Demand is driven by stadium and school field replacement schedules (typical 8-10 year lifecycle) and federal Infrastructure Act disbursements.",
    kpi_series  = c("construction","cpi","freight_ppi","fed_funds","ppi_plastics","mortgage_30"),
    kpi_labels  = c("Total Construction ($B ann.)","CPI","Freight PPI","Fed Funds","PPI: Resins","30-Yr Mortgage"),
    kpi_subs    = c("FRED TTLCONS — infrastructure proxy","FRED CPIAUCSL","PCU484121484121",
                    "FRED FEDFUNDS","WPU0911","FRED MORTGAGE30US (low sensitivity)"),
    chart_primary   = "construction",
    chart_secondary = "cpi",
    chart_p_label   = "Total Construction $B (infra. proxy)",
    chart_s_label   = "CPI x8 (cost basis)",
    chart_p_color   = PAL$green,
    chart_s_color   = PAL$muted,
    chart_title     = "Turf & Specialty Demand Proxy — Construction + CPI",
    narrative = list(
      what_changed = "Infrastructure Act funding continues disbursing through 2025-2026. Total construction spending at $2,168B ann. Stadium synthetic turf replacement cycle ongoing.",
      why_matters  = "Turf and specialty is a counter-cyclical buffer — does not move with HOUST or mortgage rates. Shaw Sports Turf participates in NFL, collegiate, and high school field installations.",
      what_means   = "Turf provides margin and revenue stability during housing downturns. Growing as municipalities prefer low-maintenance surfaces.",
      what_to_do   = "Deepen relationships with school district procurement consortiums. Pursue preferred-contractor status with municipal parks departments in top-25 MSAs."
    ),
    leading_inds = c("FRED TTLCONS","FRED CPIAUCSL","Federal Infrastructure Act disbursements"),
    assump_cats  = c("Macro","Logistics"),
    porter_focus = c("entrants","substitutes"),
    porter_note  = "For turf, New Entrants (FieldTurf/Tarkett) and Threat of Substitutes are the most relevant forces."
  )
)

# ── Competitor Data ───────────────────────────────────────────────────────────
# Source: FY2024 10-K / Annual Reports filed Feb-Mar 2025. GAAP as-reported.
#
# CORRECTIONS vs. prior version (which had stale FY2022 figures):
#   MHK gross margin: was 30.1% -> corrected to 24.8% (FY2024 GAAP: $2,687.7M / $10,836.9M)
#   AWI gross margin: was 35.8% -> corrected to 41.6% (FY2024 GAAP: $582M / $1,400M)
#   AWI revenue:      was $1.31B -> corrected to $1.40B
#   TILE gross margin:was 38.5% -> corrected to 36.7% (FY2024 GAAP; +174 bps YoY)
#   TILE revenue:     was $1.41B -> corrected to $1.32B
#   Tarkett revenue:  was $2.88B -> corrected to $3.46B (FY2024 EUR/USD avg)
#   Tarkett gross margin: was 22.4% -> corrected to 19.7% (FY2024 TTM)
COMPETITORS <- tibble(
  company   = c("Shaw Industries (est.)", "Engineered Floors (est.)",
                "Mohawk Ind. (MHK)",      "Interface (TILE)",
                "Armstrong (AWI)",         "Tarkett SA"),
  revenue_b = c(6.5,    1.8,     10.84,  1.32,   1.40,   3.46),
  gm_pct    = c(NA_real_, NA_real_, 24.8, 36.7,  41.6,   19.7),
  source    = c("Analyst est.", "Analyst est.",
                "MHK FY2024 10-K (Feb 2025)",
                "TILE FY2024 10-K (Feb 2025)",
                "AWI FY2024 10-K (Feb 2025)",
                "Tarkett FY2024 Ann. Rpt (Feb 2025)"),
  color     = c(PAL$accent, PAL$red, PAL$blue, PAL$green, PAL$purple, PAL$muted),
  note      = c(
    "Private; ~70% residential. Est. from industry sources + peer benchmarking.",
    "Private; carpet-focused; Dalton GA. Berkshire-adjacent distribution.",
    "Rev -2.7% YoY. GM compressed to 24.8% on soft housing + pricing pressure. Restructuring ongoing.",
    "Rev +4.3% YoY. GM expanded +174 bps to 36.7% on volume/mix/lower inputs. Record EBITDA.",
    "Rev +7% YoY. GM 41.6%; record sales driven by Mineral Fiber AUV + Arch. Specialties growth.",
    "Rev -6% YoY in USD (FX drag). GM ~19.7%; EBITDA margin improved to 9.9% from 8.6%."
  )
)

# ── Competitor Gross Margin Benchmarking (exec_margins chart) ─────────────────
COMP_MARGINS <- tibble(
  company      = c("MHK\n(Mohawk)",  "TILE\n(Interface)", "AWI\n(Armstrong)", "Shaw\n(Est.)"),
  ticker       = c("MHK",             "TILE",               "AWI",              "SHAW"),
  fiscal_year  = c("FY2024",          "FY2024",             "FY2024",           "FY2024E"),
  revenue_b    = c(10.84,             1.32,                  1.40,               6.5),
  gross_margin = c(24.8,              36.7,                  41.6,               28.0),
  oper_margin  = c(6.4,               10.2,                  NA_real_,           11.0),
  filing       = c("10-K Feb 2025",  "10-K Feb 2025",       "10-K Feb 2025",    "Analyst est."),
  note         = c(
    "GM down from ~34% (FY2018) to 24.8% — soft housing + pricing pressure. Restructuring ongoing.",
    "Record results; GM +174 bps YoY to 36.7%; premium commercial positioning paying off.",
    "Record sales $1.40B; GM 41.6% driven by Mineral Fiber AUV growth + Arch. Specialties.",
    "Analyst estimate — not reported. Derived from MHK/AWI peer benchmarking + industry sources."
  )
)

# ── Porter Radar ──────────────────────────────────────────────────────────────
PORTER_RADAR <- tibble(
  force     = c("Industry\nRivalry","New\nEntrants","Supplier\nPower","Buyer\nPower","Substitutes"),
  shaw      = c(78, 33, 58, 54, 75),
  benchmark = c(60, 55, 50, 50, 55)
)

# ── Porter Scoring Methodology ────────────────────────────────────────────────
PORTER_SCORING_METHODOLOGY <- tibble(
  force       = c("Industry Rivalry","Threat of New Entrants","Power of Suppliers",
                  "Power of Buyers","Threat of Substitutes"),
  shaw_score  = c(78, 33, 58, 54, 75),
  bench_score = c(60, 55, 50, 50, 55),
  score_components_shaw = c(
    "BASE (oligopoly): +50\n+13  Shaw + MHK est. ~60% domestic share — bilateral duopoly pricing dynamics\n+18  LVT/SPC new competitive front: Mohawk, Shaw COREtec/SPC, Asian imports all competing\n+8   FCW-documented commodity carpet price discounting persists into 2025\n-11  BRK patient capital insulates Shaw from irrational short-term pricing\n= 78",
    "BASE: +50\n-20  Capex barrier: MHK FY2024 ~$471M capex; greenfield requires $250-400M+\n-15  50+ yr installer/dealer relationships — not replicable quickly\n+10  Vietnamese/Malaysian LVT import growth despite Section 301 tariffs\n-12  No domestic greenfield entrant in 8+ years (trade press)\n= 33",
    "BASE: +50\n+12  Fiber concentration: Invista, Ascend, RadiciGroup dominate nylon 6,6 supply\n+12  FRED WPU0911 +36% spike (2020-2022) proved supplier pricing power in this market\n-8   Vertical integration partially insulates carpet fiber inputs\n-8   SPC/LVT resins more commoditized than carpet fiber — more supplier competition\n= 58",
    "BASE (weighted channel avg):\nProduction builders (est. 35% rev): 73 — D.R. Horton 89,690 homes FY2023\nRetail dealers (est. 30%): 28 — 40k+ fragmented dealers\nCommercial/contractor (est. 25%): 54 — multi-year RFP, spec quality > unit price\nBig-box/remodel (est. 10%): 64 — HD/Lowe's private-label leverage\nWeighted: (73x.35)+(28x.30)+(54x.25)+(64x.10) = 54",
    "BASE: +40\n+20  LVT/SPC matches carpet on warmth/acoustics; adds waterproof + durability\n+18  FCW: carpet share 38% (2019) -> est. 24% (2026) — 14-pt structural decline\n+5   Pet ownership 66% US HH (APPA 2023-24) — structural hard-surface driver\n-8   COREtec + SPC line = Shaw capturing substitution within its own portfolio\n= 75"
  ),
  benchmark_rationale = c(
    "Mature U.S. durable goods 3-5 player industry: 60. No LVT-front category war assumed.",
    "Mature mfg baseline with standard barriers: 55. Some import penetration assumed.",
    "Standard mfg supplier dependency without petrochemical concentration: 50.",
    "Average across mixed B2B channels in mature manufacturing: 50.",
    "Baseline for durable goods with some functional alternatives: 55."
  ),
  primary_data_sources = c(
    "MHK FY2024 10-K (revenue, margin, capex); Floor Covering Weekly; FRED HOUST",
    "MHK FY2024 10-K (capex); U.S. Census HS 3918 imports; USTR Section 301 dockets",
    "FRED WPU0911 historical; MHK FY2024 10-K risk factors; fiber supplier trade press",
    "D.R. Horton FY2023 10-K; NAHB HMI; Floor Covering Weekly dealer count estimates",
    "Floor Covering Weekly share 2019-2026 est.; APPA 2023-24 survey; MHK investor day"
  ),
  last_reviewed  = rep("Q1 2026", 5),
  review_trigger = c(
    "Revisit if MHK gross margin recovers above 33% or a new major domestic competitor emerges",
    "Revisit if USTR expands/removes Section 301 tariffs or greenfield announcement occurs",
    "Revisit if FRED WPU0911 moves +/-10% YoY or primary fiber supplier ownership changes",
    "Revisit if top-3 builder share exceeds 35% of starts or major retailer consolidates",
    "Revisit each Q using Floor Covering Weekly share data; raise score if carpet < 22% of market"
  )
)

# ── Porter Deep-Dive ──────────────────────────────────────────────────────────
PORTER_FORCES <- list(
  rivalry = list(
    label = "Industry Rivalry", score = 78, level = "HIGH", color = PAL$red,
    headline = "Intense rivalry with Mohawk persists. The SPC category has opened a second competitive front where rivalry is intensifying, not normalizing.",
    hypothesis = "The U.S. flooring market is a mature oligopoly where Shaw and Mohawk control the majority of domestic manufacturing. In LVT/SPC the only high-growth segment — competitive intensity is rising, not easing.",
    evidence = c(
      "Mohawk: $10.84B net sales (FY2024) vs. Shaw est. ~$6.5B — combined ~60%+ of domestic manufacturing",
      "Mohawk gross margin compressed from ~34% (FY2018) to 24.8% (FY2024) — structural industry-wide pricing pressure",
      "SPC: Mohawk (RevWood Plus), Shaw (COREtec + SPC Feb 2025), Asian sub-$2/sqft all competing in fastest-growing category",
      "Floor Covering Weekly: commodity carpet price discounting continued into 2025 as category volume contracts",
      "Engineered Floors continues gaining mid-market carpet share as a focused domestic competitor"
    ),
    insight = "Rivalry is the dominant structural force. Vertical integration provides cost insulation, not margin immunity. SPC requires competing against both Mohawk and landed Asian pricing simultaneously.",
    action  = "Compete on installer ecosystem lock-in, service quality, and sustainability credentials. SPC pricing must match Asian landed cost on quality-comparable specs."
  ),
  entrants = list(
    label = "Threat of New Entrants", score = 33, level = "LOW", color = PAL$green,
    headline = "Domestic greenfield entry is economically implausible. The real entry vector is Asian imports bypassing capital barriers via trade.",
    hypothesis = "Competitive-scale carpet or LVT manufacturing requires $250-400M+ in equipment investment plus years to build installer/dealer relationships.",
    evidence = c(
      "Mohawk FY2024 10-K: ~$471M capital expenditures — annual maintenance capex of competitive-scale manufacturing",
      "Shaw and Mohawk both have 50+ year installer, dealer, and builder relationships unreplicable quickly",
      "Census HS 3918 import volumes: Vietnamese and Malaysian LVT growth confirmed despite China Section 301 tariffs",
      "Section 301 tariffs on Chinese LVT maintained through 2025 — partial protection, Vietnamese origin filling the gap",
      "No domestic greenfield flooring entrant announced in 8+ years (Floor Covering Weekly)"
    ),
    insight = "Domestic entry risk is LOW and stable. The structural threat — Asian LVT at 20-35% cost discount — bypasses capital barriers via import. Tariff policy is the single most important watch variable.",
    action  = "Monitor Census HS 3918 + 5703 monthly. Price SPC to match Asian landed cost on quality-comparable specs. Maintain Section 301 tariff advocacy."
  ),
  suppliers = list(
    label = "Power of Suppliers", score = 58, level = "MED", color = PAL$amber,
    headline = "Petrochemical dependency creates fat-tail margin risk. The 2021-22 spike is the template for what a repeat looks like.",
    hypothesis = "Carpet manufacturing is a petrochemical transformation business. Nylon, polyester (PET), and polypropylene — all petroleum-derived — represent the majority of fiber COGS.",
    evidence = c(
      "FRED WPU0911: +36% from Q1 2020 to Q2 2022 peak. Now normalized to ~208 (Q4 2025 est.)",
      "Invista (nylon 6,6), Ascend Performance Materials, RadiciGroup control majority of carpet-grade nylon fiber supply",
      "Mohawk FY2024 10-K cites raw material cost volatility including petroleum-derived materials as ongoing primary risk",
      "FRED PCU484121484121: freight recovering to ~155 from 175 peak — still 27% above 2019 baseline",
      "Shaw SPC line adds input dependency on Asian-sourced LVT core and wear-layer resins — new concentration risk"
    ),
    insight = "Supplier power is moderate overall but the spike risk is asymmetric. A petroleum shock can compress margins across 6+ quarters before pricing adjustments offset.",
    action  = "Maintain 90-180 day resin buffer. Diversify fiber sourcing beyond primary nylon suppliers. Lock freight contracts 6-9 months ahead of demand recovery cycles."
  ),
  buyers = list(
    label = "Power of Buyers", score = 54, level = "MED", color = PAL$amber,
    headline = "Buyer power is structurally channel-dependent: very high for national production builders, low for fragmented retail dealers.",
    hypothesis = "Shaw's buyer power is not uniform — it must be analyzed by channel.",
    evidence = c(
      "D.R. Horton: 89,690 home closings FY2023 (10-K). Top-10 builders est. 30%+ of new single-family starts",
      "NAHB HMI: ~47 in Q1 2026 est. — approaching neutral; constrains Shaw's pricing leverage with builders",
      "Commercial accounts: structured multi-year RFPs; spec quality and lead time > unit price",
      "~40,000+ independent flooring dealers in the U.S. (Floor Covering Weekly) — fragmented, low individual leverage",
      "Home Depot, Lowe's: significant private-label LVT leverage in the remodel channel"
    ),
    insight = "Production builder: highest volume, highest price pressure. Commercial: better margin and contract stability, recovering on ABI signal.",
    action  = "Pursue preferred-vendor multi-year agreements with top-10 production builders. Build dedicated commercial vertical team for healthcare and hospitality."
  ),
  substitutes = list(
    label = "Threat of Substitutes", score = 75, level = "HIGH", color = PAL$red,
    headline = "LVT/SPC substitution of carpet is accelerating. A 14-point share decline in 7 years is structural — not cyclical.",
    hypothesis = "LVT/SPC now matches or exceeds carpet on all core consumer benefits while adding waterproofing and durability.",
    evidence = c(
      "Floor Covering Weekly: carpet est. 38% (2019) -> 24% (2026) — 14-point structural decline. LVT/SPC 16% -> 36%",
      "APPA 2023-24 survey: 66% of U.S. households own a pet — structural driver of hard-surface preference",
      "Shaw COREtec + SPC launch (Feb 2025) are deliberate management responses to permanent substitution",
      "Mohawk FY2024 investor day reaffirmed accelerating shift from soft to hard surface as a multi-year structural reality",
      "Multi-family construction increasingly specifying LVT as the default over carpet"
    ),
    insight = "Shaw is managing deliberate self-disruption — growing LVT/SPC at the direct expense of legacy carpet. SPC ramp speed and production builder channel penetration are the most important operational variables.",
    action  = "Reposition carpet as premium acoustics/comfort for bedrooms and multi-family. Set explicit quarterly SPC category share targets with board-level visibility."
  )
)

# ── SWOT Data ─────────────────────────────────────────────────────────────────
SWOT_DATA <- list(
  S = list(label = "Strengths", color = PAL$green, items = list(
    list(title = "#1 U.S. Flooring Manufacturer",
         evidence = "Est. ~26-27% of U.S. flooring manufacturing revenue (Floor Covering Weekly). Figures not reported — Shaw is private.",
         indicator = "MHK, AWI, TILE quarterly 10-K filings for competitor share movement",
         risk = 12, tier = "HIGH"),
    list(title = "Berkshire Hathaway Capital Access",
         evidence = "Full BRK subsidiary since 2002. Patient capital for M&A and capex cycles without public-market quarterly earnings pressure.",
         indicator = "BRK-A annual report Building Products segment commentary",
         risk = 5, tier = "HIGH"),
    list(title = "Vertical Integration Advantage",
         evidence = "Shaw controls fiber extrusion through manufacturing through distribution — cost and quality advantages vs. assembler-model competitors.",
         indicator = "MHK gross margin (public proxy); Shaw COGS model vs. industry",
         risk = 18, tier = "HIGH"),
    list(title = "Multi-Brand Portfolio",
         evidence = "9 brands across residential (Shaw Floors, Anderson Tuftex, COREtec), commercial (Patcraft, Philadelphia, Shaw Contract), and specialty (Shawgrass, Southwest Greens, Shaw Sports Turf).",
         indicator = "Brand revenue concentration; top-2 brand % of total",
         risk = 22, tier = "MED"),
    list(title = "EcoWorx Sustainability Platform",
         evidence = "EcoWorx carpet tile: Cradle to Cradle Gold certified. One of the largest post-consumer carpet reclamation programs in the U.S.",
         indicator = "EPA PFAS/VOC regulatory pipeline; commercial ESG procurement mandates",
         risk = 14, tier = "MED")
  )),
  W = list(label = "Weaknesses", color = PAL$red, items = list(
    list(title = "High Sensitivity to Housing Cycle",
         evidence = "HOUST correlation to estimated Shaw residential revenue: r ~0.87 (modeled). Each 100k start decline = est. $280-350M in total addressable flooring demand lost industry-wide.",
         indicator = "FRED HOUST monthly; FRED MORTGAGE30US weekly; NAHB HMI monthly",
         risk = 55, tier = "HIGH"),
    list(title = "Carpet Mix Transition Cost",
         evidence = "CEO Tim Baucom acknowledged (trade press) the costly transition from solid-color nylon to patterned polyester post-2020. Different manufacturing configurations and higher SKU complexity.",
         indicator = "Floor Covering Weekly carpet volume quarterly; nylon vs. polyester price spread",
         risk = 48, tier = "HIGH"),
    list(title = "No Public Financial Disclosure",
         evidence = "Private Berkshire subsidiary — no standalone financials. All revenue/margin figures in this portal are analyst estimates.",
         indicator = "Structural. Monitor BRK annual report Building Products commentary.",
         risk = 25, tier = "LOW"),
    list(title = "U.S. Revenue Concentration",
         evidence = "Est. >80% of revenue U.S.-sourced (BRK segment commentary). High sensitivity to U.S. cycle with limited international offset.",
         indicator = "Non-U.S. construction indices; BRK international commentary",
         risk = 45, tier = "MED")
  )),
  O = list(label = "Opportunities", color = PAL$blue, items = list(
    list(title = "SPC/LVT Market Growth",
         evidence = "LVT/SPC est. 33-36% of U.S. flooring dollars (2025-26). COREtec established; SPC line launched Feb 2025 extends portfolio.",
         indicator = "Census HS 3918 volumes; Floor Covering Weekly quarterly LVT/SPC category share",
         risk = 16, tier = "HIGH"),
    list(title = "Housing Market Recovery",
         evidence = "FEDFUNDS declined from 5.33% peak to est. 3.58% (Q4 2025). MORTGAGE30US: est. 6.34% down from 7.8% peak. HOUST recovering to ~1,482k.",
         indicator = "FRED HOUST; FRED MORTGAGE30US; FRED FEDFUNDS; NAHB HMI",
         risk = 28, tier = "HIGH"),
    list(title = "Commercial Segment Recovery",
         evidence = "AIA Architecture Billings Index returned above 50 in late 2025 — the 9-18 month lead signal is now positive for H1-H2 2026 demand at Patcraft and Philadelphia.",
         indicator = "AIA ABI monthly; FRED TTLCONS nonresidential subcategory",
         risk = 35, tier = "MED"),
    list(title = "Tariff-Driven Reshoring Tailwind",
         evidence = "Section 301 tariffs on Chinese LVT maintained through 2025. Escalation to Vietnamese origin would significantly improve domestic manufacturer economics.",
         indicator = "USTR tariff announcements; Census HS 3918 import volumes by country",
         risk = 32, tier = "MED"),
    list(title = "AI & Manufacturing Optimization",
         evidence = "AI-driven quality control and predictive maintenance: est. 5-10% waste reduction, 15-20% downtime reduction based on comparable textile sector pilots.",
         indicator = "MHK automation capex disclosures; textile sector AI benchmarks",
         risk = 28, tier = "MED")
  )),
  T = list(label = "Threats", color = PAL$amber, items = list(
    list(title = "Mortgage Rate Stickiness",
         evidence = "MORTGAGE30US est. ~6.34% (Q4 2025) vs. 2.77% low (Q4 2020). Despite three Fed cuts in 2025, mortgage rates remain elevated due to term premium.",
         indicator = "FRED MORTGAGE30US weekly; FRED HOUST monthly; NAR existing home sales monthly",
         risk = 58, tier = "HIGH"),
    list(title = "Asian LVT/SPC Import Competition",
         evidence = "Vietnamese and Malaysian manufacturers produce LVT at est. 20-35% cost discount to U.S. domestic. Census HS 3918 confirms continued import share growth.",
         indicator = "Census HS 3918+5703 monthly by origin country; USTR tariff review schedule",
         risk = 62, tier = "HIGH"),
    list(title = "Structural Carpet Share Decline",
         evidence = "Floor Covering Weekly: carpet est. 38% (2019) -> 24% (2026). If carpet reaches 18-20% by 2028-29 without full LVT/SPC offset, Shaw faces structural top-line pressure.",
         indicator = "Floor Covering Weekly quarterly category share; Shaw soft vs. hard surface revenue mix",
         risk = 68, tier = "HIGH"),
    list(title = "Raw Material Cost Spike",
         evidence = "FRED WPU0911 at ~222 (Q4 2025 est.) — normalized from 248 peak but creeping. A petroleum shock could reproduce 2021-22 compression.",
         indicator = "FRED WPU0911 monthly; WTI crude futures; FRED CPIAUCSL sub-indices",
         risk = 42, tier = "MED")
  ))
)

# ── Strategy Assumptions ──────────────────────────────────────────────────────
ASSUMPTIONS <- tibble(
  id         = 1:8,
  assumption = c(
    "Housing starts recover to ≥1.6M annualized by end of 2026",
    "FEDFUNDS falls below 4.0% by Q4 2025",
    "PPI plastics (WPU0672) inflation stays below +6% YoY",
    "Shaw LVT/SPC revenue share grows from est. ~18% to ≥23% by 2026",
    "Commercial segment (Patcraft/Shaw Contract) rebounds +3%+ YoY in 2025",
    "Long-distance freight PPI stays below 2022 peak of ~175",
    "No new major tariff escalation disrupts Asian LVT supply cost structure",
    "Shaw SPC launch (Feb 2025) reaches ≥4% SPC category share by end 2026"
  ),
  metric     = c(
    "FRED HOUST (k units SAAR)",
    "FRED FEDFUNDS (%)",
    "FRED WPU0672 YoY % change",
    "Est. LVT/SPC as % Shaw revenue",
    "Est. commercial segment YoY growth",
    "FRED PCU484121484121 Index",
    "Census HS 3918+5703 import share %",
    "Est. SPC category market share %"
  ),
  current    = c("1,380k","4.83%","+3.1%","~18% (est.)",
                 "+1.2% (est.)","143","~22% (est.)","~2% (est.)"),
  threshold  = c(">1,600k","<4.0%","<6.0%",">23%",
                 ">3.0%","<175","<27%",">4.0%"),
  current_v  = c(1380, 4.83, 3.1, 18, 1.2, 143, 22, 2),
  target_v   = c(1600, 4.00, 6.0, 23, 3.0, 175, 27, 4),
  status     = c("red","red","red","red","yellow","yellow","yellow","red"),
  trend      = c(
    "DETERIORATING: mortgage rates back above 6%; HOUST recovery delayed 6-12mo (Hormuz crisis)",
    "DETERIORATING: Fed frozen; 0 cuts now priced for 2026; stagflation risk (Hormuz crisis)",
    "BREACHED: WTI +38% in 9 days; WPU0672 spike near-certain within 4-8 weeks (Hormuz crisis)",
    "At risk - was improving pre-crisis; housing delay reduces volume",
    "Watch - ABI below 50; oil shock adds commercial headwind",
    "WATCH: Cape of Good Hope rerouting driving freight PPI higher (Hormuz crisis)",
    "Mixed - Hormuz delays Asian LVT imports (competitive benefit) but disrupts SPC raw material sourcing",
    "Early stage - Feb 2025 launch"
  ),
  category   = c("Residential","Macro","Input Costs","Product Mix",
                 "Commercial","Logistics","Trade","Product Mix"),
  weight     = c("HIGH","HIGH","HIGH","HIGH","MED","MED","HIGH","HIGH"),
  hormuz_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE),
  hormuz_note = c(
    "Mortgage rates back above 6% after <6% dip; Fed frozen on hold",
    "Oil shock re-ignites inflation risk; Fed on hold; stagflation scenario now plausible",
    "WTI +38% in 9 days; ICIS confirmed global plastics/petrochemical markets tightening",
    NA_character_,
    NA_character_,
    "Hormuz closure + Cape of Good Hope rerouting adds 10-14 days/voyage",
    "DUAL: delays Asian LVT imports (positive competitively) + disrupts Shaw SPC raw material sourcing from Asia",
    NA_character_
  )
)

# ── Scenarios ─────────────────────────────────────────────────────────────────
build_scenario <- function(rev, em) {
  tibble(
    year          = c("2024A", "2025A*", "2026E", "2027E", "2028E"),
    revenue_m     = rev,
    ebitda_margin = em,
    ebitda_m      = round(rev * em / 100)
  )
}
# SCENARIOS <- list(
#   base      = build_scenario(c(6200,6520,6820,7160,7530), c(11.0,11.5,12.0,12.5,13.0)),
#   expansion = build_scenario(c(6200,6520,7100,7780,8460), c(11.0,11.5,12.5,13.2,14.0)),
#   mild      = build_scenario(c(6200,6520,6460,6360,6550), c(11.0,11.5,10.8,10.2,10.8)),
#   severe    = build_scenario(c(6200,6520,5980,5540,5820), c(11.0,11.5, 9.5, 8.2, 9.2))
# )
# SCENARIO_META <- list(
#   base = list(label="Base Case", color=PAL$accent,
#               desc    = "HOUST recovers to ~1.6M by 2026. FEDFUNDS stabilizes at 3.25-3.5%. SPC gains share. ABI stays above 50.",
#               drivers = "FRED HOUST, FEDFUNDS, MORTGAGE30US, WPU0911"),
#   expansion = list(label="Expansion", color=PAL$green,
#                    desc    = "Rate cuts exceed consensus. HOUST reaches 1.8M+. SPC captures 7%+ share. Commercial recovery strong.",
#                    drivers = "HOUST > 1.75M, MORTGAGE30US < 5.5%, SPC > 7% share"),
#   mild = list(label="Mild Downturn", color=PAL$amber,
#               desc    = "Mortgage stickiness persists. HOUST plateaus at 1.4-1.5M. SPC ramp slower. WPU0911 creeps +5% YoY.",
#               drivers = "MORTGAGE30US > 6.5%, HOUST stalls, WPU0911 +4-6% YoY"),
#   severe = list(label="Severe Downturn", color=PAL$red,
#                 desc    = "Fiscal shock re-accelerates rates. HOUST falls below 1.2M. Commercial freezes. Resin spike +15%+.",
#                 drivers = "HOUST < 1.2M, FEDFUNDS re-spikes > 5.0%, WPU0911 > +15%")
# )


# ⚠ UPDATED March 9, 2026: Three Hormuz Shock scenarios added.
# ── Scenario revenue/margin inputs ────────────────────────────────────────────
# ⚠ BASE CASE AND EXPANSION REVISED March 9, 2026 following Hormuz crisis.
#
# BASE CASE REVISION RATIONALE:
#   Pre-crisis: $6,480M (2025E), 11.6% EBITDA margin
#   Revised:    $6,220M (2025E), 10.9% EBITDA margin
#   Drivers:
#   (a) H1 2025 input cost damage is non-recoverable. WPU0672 lags crude 4-8 weeks.
#       Even under rapid resolution, Q1-Q2 2025 fiber costs are already elevated.
#       Est. EBITDA margin impact: -60 to -80bps vs. pre-crisis Base Case.
#   (b) Housing starts recovery delayed 6-12 months. The brief MORTGAGE30US dip
#       below 6% (first since 2022, Feb 2026) has reversed. Lock-in effect persists.
#       Volume recovery for residential division is pushed from H1 2025 to H2 2025
#       at earliest, with 2026 now the first "normal" year.
#   (c) Fed rate cut timeline extended. 0 cuts now priced for 2026 vs. 2+ pre-crisis.
#       FEDFUNDS staying at 4.83% through 2026 keeps mortgage rates elevated.
#   Net: Base Case is still achievable, but requires rapid conflict resolution (<2 wks)
#   AND assumes Fed resumes cuts by Q3 2026 AND WPU0672 normalizes within the year.
#   Probability downgraded from ~45% (pre-crisis) to ~15% (post-crisis, March 9 2026).
#
# EXPANSION REVISION RATIONALE:
#   Pre-crisis: $6,720M (2025E), 12.2% EBITDA margin
#   Revised:    $6,420M (2025E), 11.4% EBITDA margin
#   Drivers:
#   (a) H1 2025 damage still applies even in expansion scenario. There is no scenario
#       in which H1 2025 revenue and margins are unaffected — the input cost spike
#       is already in motion (ICIS confirmed March 4). The expansion scenario now
#       reflects a strong H2 2025 + 2026-27 recovery, not a full-year 2025 boom.
#   (b) Expansion now requires: rapid Hormuz resolution (<2 weeks) AND immediate
#       Fed pivot AND aggressive cuts through 2026 AND housing boom. This is a very
#       low probability compound scenario.
#   (c) The $8,050M 2027E ceiling is retained — if all conditions align, the 2027
#       recovery can still be strong — but 2025 is irrecoverably impaired.
#   Probability downgraded from ~15% (pre-crisis) to ~4% (post-crisis, March 9 2026).
SCENARIOS <- list(
  base = build_scenario(
    # ⚠ REVISED March 9, 2026: 2025E -$260M, 2026E -$270M vs. pre-crisis
    rev = c(6050, 6200, 6220, 6540, 7040),
    em  = c(10.8, 11.0, 10.9, 11.4, 12.1)
  ),
  expansion = build_scenario(
    # ⚠ REVISED March 9, 2026: 2025E -$300M vs. pre-crisis; H1 damage non-recoverable
    rev = c(6050, 6200, 6420, 7180, 7920),
    em  = c(10.8, 11.0, 11.4, 12.6, 13.5)
  ),
  mild = build_scenario(
    rev = c(6050, 6200, 6150, 6080, 6280),
    em  = c(10.8, 11.0, 10.4, 10.0, 10.6)
  ),
  severe = build_scenario(
    rev = c(6050, 6200, 5760, 5380, 5640),
    em  = c(10.8, 11.0,  9.2,  8.0,  9.0)
  ),
  hormuz_short = build_scenario(
    rev = c(6050, 6200, 6180, 6420, 6900),
    em  = c(10.8, 11.0, 10.8, 11.4, 12.1)
  ),
  hormuz_extended = build_scenario(
    rev = c(6050, 6200, 5900, 6100, 6600),
    em  = c(10.8, 11.0,  9.6, 10.2, 11.2)
  ),
  hormuz_prolonged = build_scenario(
    rev = c(6050, 6200, 5500, 5700, 6200),
    em  = c(10.8, 11.0,  8.4,  9.0, 10.4)
  )
)

# ⚠ UPDATED March 9, 2026: Hormuz Shock scenarios added.
# hormuz_short    = conflict resolves <2 weeks; moderate input cost impact
# hormuz_extended = 4-8 week disruption; significant margin compression
# hormuz_prolonged = 3+ months; severe stagflation scenario
SCENARIO_META <- list(
  # ── Scenario probability weights ──────────────────────────────────────────────
  # prob_pre   = analyst probability BEFORE March 2, 2026 Hormuz crisis
  # prob_post  = analyst probability AS OF March 9, 2026 (conflict day 9)
  # These are analyst judgements, not statistically derived. They reflect:
  #   (a) Which macro assumptions are currently broken (see Assumption Monitor)
  #   (b) Historical base rates for Middle East conflict durations
  #   (c) Current diplomatic signals (no ceasefire; Trump demands unconditional surrender)
  # Sum of prob_post across all scenarios should be ~100%.
  base = list(
    label     = "Base Case",  color = PAL$accent,
    desc      = "REVISED March 9, 2026. Requires rapid Hormuz resolution (<2 weeks) AND Fed resumes cuts by Q3 2026. H1 2025 input cost damage ($260M revenue, -70bps EBITDA margin) is non-recoverable even in this scenario. 2025E revised from $6,480M to $6,220M.",
    drivers   = "FRED HOUST, FEDFUNDS, WPU0672, MORTGAGE30US",
    is_hormuz = FALSE,
    prob_pre  = 45,
    prob_post = 15,
    prob_change = "DOWN from 45% — three load-bearing assumptions simultaneously broken"
  ),
  expansion = list(
    label     = "Expansion",  color = PAL$green,
    desc      = "REVISED March 9, 2026. Requires: rapid Hormuz resolution + immediate Fed pivot + housing boom. H1 2025 still impaired (-$300M vs. pre-crisis Expansion). 2025E revised from $6,720M to $6,420M. Very low probability compound scenario.",
    drivers   = "Rapid Hormuz resolution; MORTGAGE30US < 5.5%; HOUST > 1.75M",
    is_hormuz = FALSE,
    prob_pre  = 15,
    prob_post = 4,
    prob_change = "DOWN from 15% — requires conditions currently at maximum distance from reality"
  ),
  mild = list(
    label     = "Mild Downturn",  color = PAL$amber,
    desc      = "Sticky rates, HOUST stalls. Does not fully capture Hormuz input cost shock — see Hormuz Short for better analog. Probability slightly reduced as Hormuz scenarios absorb much of this distribution.",
    drivers   = "FEDFUNDS > 4.5%, HOUST stalls, WPU0672 +4-6% YoY",
    is_hormuz = FALSE,
    prob_pre  = 25,
    prob_post = 9,
    prob_change = "DOWN — Hormuz Short/Extended scenarios absorb this distribution"
  ),
  severe = list(
    label     = "Severe Downturn",  color = PAL$red,
    desc      = "Domestic fiscal shock scenario. Distinct from Hormuz — this is a rate re-spike + recession event. Probability unchanged; Hormuz adds tail risk of combined scenario.",
    drivers   = "HOUST < 1.2M, FEDFUNDS re-spikes, WPU0672 > +15%",
    is_hormuz = FALSE,
    prob_pre  = 15,
    prob_post = 5,
    prob_change = "STABLE-DOWN — Hormuz Prolonged absorbs much of this tail"
  ),
  hormuz_short = list(
    label     = "Hormuz: Short (<2wk)",  color = "#e87040",
    desc      = "ACTIVE SCENARIO (Mar 9 2026): Conflict resolves within 2 weeks. WPU0672 +4-6% YoY (H1 damage non-recoverable); freight costs rise modestly; Fed stays on hold through mid-2026; mortgage stays above 6%. Most optimistic Hormuz outcome.",
    drivers   = "WTI returns to $70-75; Hormuz transits resume ~Mar 20; FEDFUNDS 4.83% through H1 2026",
    is_hormuz = TRUE,
    prob_pre  = 0,
    prob_post = 22,
    prob_change = "NEW — most likely resolution path given diplomatic back-channels reported"
  ),
  hormuz_extended = list(
    label     = "Hormuz: Extended (4-8wk)",  color = "#d04040",
    desc      = "ACTIVE SCENARIO — MODAL CASE (Mar 9 2026): 4-8 week disruption. WPU0672 +8-12% YoY; freight PPI re-escalates toward 160; Fed frozen through 2026; mortgage rates sustain above 6.5%; HOUST recovery delayed 9-12 months. Consistent with historical Middle East conflict durations.",
    drivers   = "WTI sustains $85-100; Hormuz restricted 4-8 weeks; FEDFUNDS held through 2026",
    is_hormuz = TRUE,
    prob_pre  = 0,
    prob_post = 37,
    prob_change = "NEW — modal scenario based on historical conflict duration base rates"
  ),
  hormuz_prolonged = list(
    label     = "Hormuz: Prolonged (3+mo)",  color = "#a02020",
    desc      = "ACTIVE SCENARIO (Mar 9 2026): 3+ month disruption. WPU0672 +15-25% YoY; HOUST drops below 1.2M; Fed cannot cut amid sustained energy-driven inflation; freight PPI approaches 175; SPC raw material supply disrupted; stagflation risk.",
    drivers   = "Brent > $100 sustained; HOUST < 1.2M; WPU0672 > +15% YoY; mortgage > 7%",
    is_hormuz = TRUE,
    prob_pre  = 0,
    prob_post = 8,
    prob_change = "NEW — tail risk scenario; probability rises if ceasefire talks collapse"
  )
)

# ── Scenario probability summary (for probability chart in UI) ────────────────
SCENARIO_PROBS <- tibble(
  scenario    = c("Expansion", "Base Case", "Mild Downturn", "Severe Downturn",
                  "Hormuz Short", "Hormuz Extended", "Hormuz Prolonged"),
  label_short = c("Expansion", "Base Case", "Mild", "Severe",
                  "H: Short", "H: Extended", "H: Prolonged"),
  prob_pre    = c(15, 45, 25, 15, 0, 0, 0),
  prob_post   = c( 4, 15,  9,  5, 22, 37, 8),
  color       = c(PAL$green, PAL$accent, PAL$amber, PAL$red, "#e87040", "#d04040", "#a02020"),
  is_hormuz   = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
)

# ── Live Fact Base (static fallback values) ───────────────────────────────────
FACT_BASE_STATIC <- tibble(
  series   = c(
    "Housing Starts: Total (HOUST)",
    "30-Yr Fixed Mortgage Rate (MORTGAGE30US)",
    "Federal Funds Effective Rate (FEDFUNDS)",
    "Total Construction Spending (TTLCONS)",
    "PPI: Plastics Products (WPU0672)",
    "PPI: Long-Dist. Freight Trucking (PCU484121484121)",
    "CPI: All Urban Consumers (CPIAUCSL)",
    "Mohawk Industries (MHK) — 52-wk",
    "Interface Inc. (TILE) — 52-wk"
  ),
  source   = c("FRED","FRED","FRED","FRED","FRED","FRED","FRED","Yahoo Finance","Yahoo Finance"),
  category = c("Residential","Macro","Macro","Commercial","Input Costs",
               "Logistics","Macro","Competitive","Competitive"),
  value    = c("1,380k SAAR","6.79%","4.83%","$2,078B ann.",
               "201.3","143.0","316.4","~$108","~$13.20"),
  change   = c("+1.9% QoQ","–0.15% MoM","–0.50% from peak",
               "+0.8% QoQ","flat","–3% from 2022 peak","+0.3% MoM",
               "–8% YTD est.","–6% YTD est."),
  period   = c("Q4 2024","Q4 2024 avg","Q4 2024 avg","Q3 2024",
               "Q3 2024","Q3 2024","Dec 2024","YTD early 2025","YTD early 2025"),
  status   = c("yellow","yellow","yellow","yellow","green","green","green","yellow","yellow"),
  note     = c("Live via FRED when key present","Live via FRED","Live via FRED",
               "Live via FRED","Live via FRED","Live via FRED","Live via FRED",
               "Manual refresh — not via FRED","Manual refresh — not via FRED")
)


# ── Live Fact Base (static fallback) ─────────────────────────────────────────
# Competitor rows updated to FY2024 10-K figures (filed Feb 2025).
# tidyquant will override price/change columns when YAHOO_REACHABLE is TRUE.
FACT_BASE_STATIC <- tibble(
  series   = c("Housing Starts: Total (HOUST)","30-Yr Fixed Mortgage Rate (MORTGAGE30US)",
               "Federal Funds Effective Rate (FEDFUNDS)","Total Construction Spending (TTLCONS)",
               "PPI: Plastics Mat. & Resins (WPU0911)","PPI: Long-Dist. Freight Trucking (PCU484121484121)",
               "CPI: All Urban Consumers (CPIAUCSL)","Mohawk Industries (MHK)",
               "Interface Inc. (TILE)","Armstrong World Ind. (AWI)"),
  source   = c("FRED","FRED","FRED","FRED","FRED","FRED","FRED",
               "Yahoo Finance (tidyquant)","Yahoo Finance (tidyquant)","Yahoo Finance (tidyquant)"),
  category = c("Residential","Macro","Macro","Commercial","Input Costs",
               "Logistics","Macro","Competitive","Competitive","Competitive"),
  value    = c("1,482k SAAR","6.34%","3.58%","$2,168B ann.","208.0","155.0","325.0",
               "~$95","~$14.50","~$152"),
  change   = c("+7.4% YoY","-0.45% QoQ","Achieved <4.0% \u2713","+0.9% QoQ",
               "+3.5% YoY","+8.4% YoY","+0.9% QoQ",
               "-13% YTD est.","-","+5% YTD est."),
  period   = c("Q4 2025 est.","Q4 2025 est.","Q4 2025 est.","Q3 2025",
               "Q4 2025 est.","Q4 2025 est.","Q4 2025 est.",
               "FY2024 10-K filed Feb 2025","FY2024 10-K filed Feb 2025","FY2024 10-K filed Feb 2025"),
  status   = c("yellow","yellow","green","green","green","green","green",
               "yellow","yellow","yellow"),
  note     = c(
    "Live via FRED when FRED_API_KEY set",
    "Live via FRED",
    "Live via FRED",
    "Live via FRED",
    "Live via FRED (WPU0911 — Plastics Mat. & Resins)",
    "Live via FRED",
    "Live via FRED",
    "Rev $10.84B (-2.7% YoY) | Gross margin 24.8% | Op. margin 6.4% | FY2024 10-K",
    "Rev $1.32B (+4.3% YoY) | Gross margin 36.7% (+174 bps) | Record EBITDA | FY2024 10-K",
    "Rev $1.40B (+7% YoY) | Gross margin 41.6% | Record sales | FY2024 10-K"
  )
)

# ── Leading Indicators ────────────────────────────────────────────────────────
LEADING_INDICATORS <- tibble(
  indicator = c("FRED MORTGAGE30US","FRED HOUST","AIA Architecture Billings Index",
                "FRED WPU0911 (Plastics Mat. & Resins PPI)","NAHB Housing Market Index","Floor Covering Dealer SSS"),
  lead_time = c("9-12 months before residential demand","6-9 months before flooring shipments",
                "9-18 months before commercial flooring","3-6 months before margin impact",
                "3-6 months before builder order flow","1-3 months coincident indicator"),
  current   = c("est. 6.34% (Q4 2025) — declining from 7.8% peak",
                "est. 1,482k — steady recovery toward 1.6M target",
                "Returned above 50 in late 2025 — positive forward signal for H1 2026",
                "Index ~208 — stable; +3.5% YoY; well below 248 peak",
                "~47 (Q1 2026 est.) — approaching neutral; improving from 37 lows",
                "Est. +2.5% YoY (trade press) — recovering"),
  status    = c("yellow","yellow","green","green","yellow","green")
)

# ── Macro Signal Heatmap ──────────────────────────────────────────────────────
MACRO_SIGNALS <- tibble(
  label  = c("Interest Rate Environment","Housing Market Momentum","Input Cost Pressure",
             "Consumer Spending Power","Commercial Construction","Import / Competitive Pressure"),
  score  = c(55, 64, 64, 65, 54, 24),
  note   = c(
    "FEDFUNDS 3.58% — below 4.0% threshold achieved. Mortgage still sticky at ~6.34%",
    "HOUST at 1,482k — improving but below 1.6M target. ABI recovery adds commercial tailwind",
    "WPU0911 +3.5% YoY — normalized; freight at 155, recovering modestly",
    "Real PCE growing; consumer credit stable; labor market still resilient in Q1 2026",
    "ABI crossed 50 in late 2025 — implies H1-H2 2026 commercial demand recovery",
    "Asian LVT import share ~23%; Section 301 tariffs stable; Vietnamese origin growth continues"
  ),
  status = c("yellow","yellow","green","green","yellow","red")
)



# ── Geopolitical Risk Data — Hormuz Crisis (added March 9, 2026) ─────────────
# Source: Bloomberg, CNBC, Kpler, ICIS, Lloyd's List, Congressional Research Service
# All data points are from published, attributed sources.
# This block should be refreshed manually as the situation evolves.

HORMUZ_STATUS <- list(
  last_updated    = "March 9, 2026",
  conflict_day    = 9,
  status          = "ACTIVE — No ceasefire. IRGC declared Strait closed March 2.",
  severity        = "CRITICAL",
  # Source: MarineTraffic / Kpler vessel tracking data
  transit_drop_pct = 90,
  # Source: Kpler data cited by Euronews, March 4, 2026
  tankers_stranded = 200,
  vessels_stranded = 150,
  # Source: CNBC, March 6 — WTI futures weekly gain, largest since 1983
  wti_pre_conflict = 65,
  wti_current      = 91,
  wti_weekly_gain_pct = 35.6,
  # Source: Barclays, UBS analyst notes cited in CNBC March 1
  brent_scenario_low  = 82,
  brent_scenario_mid  = 100,
  brent_scenario_high = 120,
  # Source: Qatar energy minister, Financial Times, March 6
  brent_tail_risk     = 150,
  # Source: ICIS, Hydrocarbon Engineering, March 4, 2026
  icis_plastics_status = "Hormuz restrictions already reducing availability and raising feedstock costs globally",
  # Source: CBS News / GasBuddy, March 5
  gas_price_increase_1wk = 0.26,
  gas_price_current = 3.25,
  # Source: Marsh insurance broker, CNBC, March 4
  war_risk_insurance_pct_before = 0.25,
  war_risk_insurance_pct_current = 1.25,
  # Source: Yahoo Finance / TheStreet, March 6-7
  fed_cuts_priced_2026_before = 2,
  fed_cuts_priced_2026_current = 0,
  # Source: CNN, March 5; CBS News, March 5
  mortgage_rate_before = 5.98,  # briefly dipped below 6% — first since 2022
  mortgage_rate_current = 6.05,
  diplomatic_status = "Iranian operatives reportedly reached out; no formal talks confirmed. Trump demands unconditional surrender.",
  key_events = list(
    list(date="Feb 28", event="US-Israel launch Operation Epic Fury. Khamenei killed."),
    list(date="Mar 1",  event="Iran retaliatory strikes on US bases, Gulf states. Tanker traffic begins falling."),
    list(date="Mar 2",  event="IRGC declares Strait closed. Maersk, MSC, Hapag-Lloyd, CMA CGM suspend transits."),
    list(date="Mar 3",  event="Qatar halts LNG exports. Saudi Arabia suspends largest refinery. Iraq output -60%."),
    list(date="Mar 4",  event="ICIS confirms global plastics/petrochemical market tightening. Brent ~$82."),
    list(date="Mar 6",  event="WTI records largest weekly gain in futures history (+35.6%). Brent ~$92."),
    list(date="Mar 8",  event="Brent hits $100+. Oil at highest since 2022. Fed expected to hold at Mar 18-19 meeting."),
    list(date="Mar 9",  event="Conflict day 9. No ceasefire. Iranian contact with intermediaries unconfirmed.")
  )
)

# Transmission channels — direct mapping from Hormuz to Shaw P&L
HORMUZ_TRANSMISSION <- tibble(
  channel = c(
    "Crude oil → carpet fiber input costs",
    "Freight rerouting → distribution costs",
    "Fed paralysis → mortgage rate stickiness",
    "Mortgage stickiness → housing starts recovery delayed",
    "Shipping suspension → Asian LVT import delay",
    "Consumer confidence shock → discretionary deferral",
    "Insurance costs → supply chain friction"
  ),
  severity = c("CRITICAL","HIGH","HIGH","HIGH","MIXED","MODERATE","MODERATE"),
  severity_color = c("red","red","red","red","amber","amber","amber"),
  mechanism = c(
    "WTI +38% in 9 days. Nylon/PET/PP fiber is petroleum-derived. WPU0672 lags crude by 4-8 weeks. Breach of +6% YoY threshold near-certain in any scenario except conflict resolution within 2 weeks.",
    "Cape of Good Hope rerouting adds 10-14 days per voyage. Major carriers suspended Hormuz transits. FRED PCU484121484121 (freight trucking PPI) will rise. 2022 freight crisis precedent: PCU484121484121 reached 175.",
    "Oil shock re-ignites inflation fears. Fed on hold indefinitely — 0 cuts now priced for 2026 (was 2+). Fed meeting March 18-19 expected to hold at 4.83%. Incoming chair Warsh may push cuts regardless.",
    "FRED MORTGAGE30US bounced from 5.98% (briefly below 6% for first time since 2022) back above 6.05%. Lock-in effect persists. HOUST recovery to 1.6M target pushed right by 6-12 months.",
    "DUAL EFFECT: Positive — delays competitor Asian LVT imports, temporarily improving Shaw domestic competitive position. Negative — disrupts Shaw's own SPC raw material supply from Asia (core, wear-layer). Net uncertain; depends on duration.",
    "Gas prices up $0.26/gal in one week (GasBuddy). Consumer confidence falls. Home improvement and flooring discretionary purchases deferred in near term.",
    "War risk insurance surged from 0.25% to 1.25% of vessel value (Marsh, March 4). Makes shipping economically unviable without government backstop. Trump DFC insurance program announced but markets unimpressed."
  ),
  fred_monitor = c(
    "FRED DCOILWTICO (WTI crude) — new addition; FRED WPU0672 with 4-8wk lag",
    "FRED PCU484121484121 — watch for reversal of 2022-2024 normalization trend",
    "FRED FEDFUNDS; CME FedWatch tool for cut probability",
    "FRED MORTGAGE30US weekly; FRED HOUST monthly",
    "Census HS 3918 monthly imports — 6-week data lag",
    "Conference Board Consumer Confidence Index; FRED PCE personal consumption",
    "Non-FRED: Marsh war risk insurance rates; Lloyd's List Intelligence vessel tracking"
  ),
  resolution_signal = c(
    "WTI returns to $70-75 range; Brent below $80",
    "Hormuz transits resume; AIS signals normalize in Strait",
    "Fed signals rate cut; market prices 1+ cuts for 2026",
    "MORTGAGE30US falls back below 6.5%; HOUST prints above 1.45M",
    "Census HS 3918 import volumes hold steady or recover",
    "Consumer confidence stabilizes; gas prices fall below $3.10",
    "Insurance premiums fall back below 0.5%; P&I coverage restored"
  )
)


# ── Mekko / Marimekko Market Structure ───────────────────────────────────────
MEKKO_DATA <- tibble(
  category   = c("Carpet",   "LVT / SPC", "Hardwood", "Ceramic Tile", "Other"),
  market_b   = c(7.4,         11.2,         3.8,        6.5,            2.1),
  shaw_pct   = c(31,          18,           12,          5,             22),
  mohawk_pct = c(29,          22,           18,          8,             15),
  trend      = c("shrinking", "growing",    "stable",   "stable",       "stable"),
  trend_note = c(
    "38% to 24% of flooring market (2019-2026E). Structural decline driven by LVT/SPC.",
    "16% to 36% of flooring market (2019-2026E). Fastest-growing; Asian import competition intense.",
    "Stable ~12-14% share. Premium positioning; less LVT substitution risk.",
    "Stable ~20% share. Less substitutable; spec-driven in commercial + kitchen/bath.",
    "Broadloom contract, area rugs, specialty. Fragmented; no dominant player."
  )
)

build_mekko_rects <- function(data) {
  total_mkt <- sum(data$market_b)
  out       <- list()
  x_left    <- 0
  for (i in seq_len(nrow(data))) {
    row        <- data[i, ]
    w          <- row$market_b / total_mkt
    x0         <- x_left
    x1         <- x_left + w
    x_mid      <- (x0 + x1) / 2
    x_left     <- x1
    others_pct <- max(0, 100 - row$shaw_pct - row$mohawk_pct)
    segs <- list(
      list(player = "Shaw (est.)",   pct = row$shaw_pct,   col = PAL$accent),
      list(player = "Mohawk (est.)", pct = row$mohawk_pct, col = PAL$blue),
      list(player = "Others",        pct = others_pct,      col = PAL$muted)
    )
    y_bot <- 0
    for (seg in segs) {
      y_top <- y_bot + seg$pct
      out[[length(out) + 1]] <- tibble(
        x0 = x0, x1 = x1, y0 = y_bot, y1 = y_top,
        x_mid = x_mid, y_mid = (y_bot + y_top) / 2,
        category = row$category, player = seg$player,
        pct = seg$pct, mkt_b = row$market_b,
        col = seg$col, trend = row$trend, note = row$trend_note
      )
      y_bot <- y_top
    }
  }
  bind_rows(out)
}

MEKKO_RECTS <- build_mekko_rects(MEKKO_DATA)

# ── UI Helpers ────────────────────────────────────────────────────────────────
kpi_card <- function(label, value, delta = NULL, delta_color = "#2dba7a", sub = NULL) {
  tags$div(class = "kpi-card",
           tags$div(class = "kpi-label", label),
           tags$div(class = "kpi-value", value),
           if (!is.null(delta)) tags$div(class = "kpi-delta", style = paste0("color:", delta_color, ";"), delta),
           if (!is.null(sub))   tags$div(class = "kpi-sub", sub))
}

section_header <- function(title, subtitle = NULL, badge_text = NULL, badge_color = "accent") {
  tags$div(
    tags$div(style = "display:flex; align-items:center; gap:12px; margin-bottom:6px;",
             tags$h2(class = "section-header", title),
             if (!is.null(badge_text)) tags$span(class = paste0("badge-", badge_color), badge_text)
    ),
    if (!is.null(subtitle)) tags$p(class = "section-subheader", subtitle),
    tags$div(class = "divider-accent")
  )
}

insight_box <- function(text, type = "accent", strong_label = "Insight") {
  color <- switch(type, accent=PAL$accent, green=PAL$green, red=PAL$red, amber=PAL$amber, PAL$muted)
  tags$div(class = paste0("insight-box insight-", type),
           tags$strong(style = paste0("color:", color, ";"), paste0(strong_label, ": ")), text)
}

data_note_ui <- function(text) {
  tags$p(style = paste0("font-size:11px; color:", PAL$muted,
                        "; margin:6px 0 0; font-style:italic; line-height:1.5;"),
         tags$span(style = paste0("color:", PAL$amber, ";"), "\u26a0 Data note: "), text)
}

plotly_dark <- function(p, xlab = "", ylab = "", legend = TRUE) {
  p |> plotly::layout(
    plot_bgcolor  = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
    font  = list(color = PAL$muted, family = "Barlow, Trebuchet MS, sans-serif", size = 11),
    xaxis = list(title=list(text=xlab, font=list(size=10)), gridcolor=PAL$border,
                 linecolor=PAL$border, zeroline=FALSE, tickfont=list(color=PAL$muted, size=10)),
    yaxis = list(title=list(text=ylab, font=list(size=10)), gridcolor=PAL$border,
                 linecolor=PAL$border, zeroline=FALSE, tickfont=list(color=PAL$muted, size=10)),
    legend = if (legend) list(bgcolor="rgba(0,0,0,0)", font=list(color=PAL$muted, size=10),
                              orientation="h", y=-0.2) else list(showlegend=FALSE),
    margin     = list(l=50, r=20, t=15, b=60),
    hoverlabel = list(bgcolor="#0d1828", font=list(color=PAL$text, size=11), bordercolor=PAL$border)
  ) |> plotly::config(displayModeBar = FALSE)
}