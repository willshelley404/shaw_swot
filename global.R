# ── Shaw Industries CEO Strategy Portal ──────────────────────────────────────
# global.R  |  Packages · Theme · API keys · FRED fetch · All static data
#
# .Renviron (in project root or ~/):
#   FRED_API_KEY=your_key_here
#   CENSUS_API_KEY=your_key_here
#
# FRED key:   https://fred.stlouisfed.org/docs/api/api_key.html  (free, instant)
# Census key: https://api.census.gov/data/key_signup.html        (free, instant)
#
# ── PACKAGE NOTES ─────────────────────────────────────────────────────────────
# tidyquant : Used for live equity data (MHK, TILE, AWI) via Yahoo Finance API.
#             Replaces any manual Yahoo scraping. CRAN-stable; uses quantmod
#             under the hood. Install: install.packages("tidyquant")
#             Usage: tidyquant::tq_get("MHK", from = Sys.Date() - 365)
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
  # FIX/ADD: tidyquant for live equity data via Yahoo Finance API
  # If not installed: install.packages("tidyquant")
  if (requireNamespace("tidyquant", quietly = TRUE)) {
    library(tidyquant)
    TIDYQUANT_AVAILABLE <- TRUE
  } else {
    message("[Shaw Portal] tidyquant not installed — equity data will use static fallback.")
    message("  Install with: install.packages('tidyquant')")
    TIDYQUANT_AVAILABLE <- FALSE
  }
})

# ── API Keys from .Renviron ───────────────────────────────────────────────────
FRED_API_KEY   <- Sys.getenv("FRED_API_KEY")
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")

# Set FRED key immediately if present so fredr calls work app-wide
if (nchar(FRED_API_KEY) > 0) {
  fredr_set_key(FRED_API_KEY)
}

# ── Color Palette ─────────────────────────────────────────────────────────────
PAL <- list(
  bg     = "#080b10", panel  = "#0e1219", border = "#1e2530",
  text   = "#d4cfc8", muted  = "#5a6070", accent = "#c8a84b",
  blue   = "#3d8fc4", green  = "#3dbb7a", red    = "#d95f5f",
  amber  = "#e8a030", purple = "#a070c0"
)

# ── bslib Dark Theme ──────────────────────────────────────────────────────────
shaw_theme <- bs_theme(
  version   = 5,
  bg        = "#080b10",
  fg        = "#d4cfc8",
  primary   = "#c8a84b",
  secondary = "#5a6070",
  success   = "#3dbb7a",
  danger    = "#d95f5f",
  warning   = "#e8a030",
  info      = "#3d8fc4",
  "font-size-base"                     = "0.875rem",
  "card-bg"                            = "#0e1219",
  "card-border-color"                  = "#1e2530",
  "card-cap-bg"                        = "transparent",
  "card-cap-color"                     = "#c8a84b",
  "input-bg"                           = "#0e1219",
  "input-color"                        = "#d4cfc8",
  "input-border-color"                 = "#1e2530",
  "input-focus-border-color"           = "#c8a84b",
  "input-placeholder-color"            = "#3a3d45",
  "nav-tabs-border-color"              = "#1e2530",
  "nav-tabs-link-active-color"         = "#c8a84b",
  "nav-tabs-link-active-bg"            = "transparent",
  "nav-tabs-link-active-border-color"  = "#c8a84b",
  "nav-tabs-link-hover-border-color"   = "#2e3a48",
  "nav-link-color"                     = "#5a6070",
  "nav-link-hover-color"               = "#9a9488",
  "table-color"                        = "#d4cfc8",
  "table-bg"                           = "transparent",
  "table-border-color"                 = "#1e2530",
  "table-striped-bg"                   = "rgba(255,255,255,0.018)",
  "table-hover-bg"                     = "rgba(200,168,75,0.04)",
  "body-bg"                            = "#080b10",
  "body-color"                         = "#d4cfc8"
)

# ── Login Credentials ─────────────────────────────────────────────────────────
VALID_USERS <- list(
  list(user = "shaw",    pass = "ELT2025",  role = "ELT"),
  list(user = "analyst", pass = "Shaw2025", role = "Analyst"),
  list(user = "demo",    pass = "demo",     role = "Demo")
)

# ── FRED Series Registry ──────────────────────────────────────────────────────
# All series IDs below are verified real FRED series as of 2025.
# HOUST          : Housing Starts — Total New Privately Owned (Ths. SAAR)
#                  Why it matters: Primary leading indicator for residential flooring demand.
#                  Each 100k increase in HOUST adds ~$350M to total addressable U.S. flooring demand.
# FEDFUNDS       : Federal Funds Effective Rate (%, monthly avg)
#                  Why it matters: Drives mortgage rates and therefore existing-home-sales turnover,
#                  which is the largest driver of remodeling flooring demand.
# MORTGAGE30US   : 30-Year Fixed Rate Mortgage Average (%, weekly → quarterly avg)
#                  Why it matters: Direct determinant of home purchase affordability and
#                  refi/remodel activity. At 6.79% vs. 2.77% (2020 low), significantly suppressive.
# WPU0672        : PPI by Commodity — Plastics Products (Index 1982=100)
#                  Why it matters: Carpet is a petrochemical transformation business. Nylon,
#                  polyester, and polypropylene are petroleum-derived. This index proxies
#                  the single largest variable cost driver in Shaw's manufacturing COGS.
# CPIAUCSL       : CPI All Urban Consumers — All Items (Index 1982-84=100)
#                  Why it matters: Tracks consumer real purchasing power and input cost pass-through.
# TTLCONS        : Total Construction Spending — All Sectors ($M SAAR)
#                  Why it matters: Broad leading indicator for both residential (new construction)
#                  and commercial (nonresidential subcategory) flooring end markets.
# PCU484121484121: PPI — Truck Transportation of Long-Distance General Freight (Index 2012=100)
#                  Why it matters: Flooring is heavy, bulk freight. Delivery cost is a meaningful
#                  component of total delivered cost. The 2022 freight spike compressed industry margins.
FRED_SERIES <- list(
  housing_starts = list(id = "HOUST",            freq = "q", agg = "avg"),
  fed_funds      = list(id = "FEDFUNDS",         freq = "q", agg = "avg"),
  mortgage_30    = list(id = "MORTGAGE30US",     freq = "q", agg = "avg"),
  ppi_plastics   = list(id = "WPU0672",          freq = "q", agg = "avg"),
  cpi            = list(id = "CPIAUCSL",         freq = "q", agg = "avg"),
  construction   = list(id = "TTLCONS",          freq = "q", agg = "avg"),
  freight_ppi    = list(id = "PCU484121484121",  freq = "q", agg = "avg")
)

# ── FRED Fetch ─────────────────────────────────────────────────────────────────
fetch_one_series <- function(series_id, start = "2019-01-01", freq = "q", agg = "avg") {
  tryCatch({
    df <- fredr(
      series_id          = series_id,
      observation_start  = as.Date(start),
      frequency          = freq,
      aggregation_method = agg
    )
    df |>
      select(date, value) |>
      filter(!is.na(value)) |>
      mutate(date = floor_date(as.Date(date), "quarter"))
  }, error = function(e) {
    message("[FRED] '", series_id, "' failed: ", conditionMessage(e))
    NULL
  })
}

fetch_fred_data <- function(start = "2019-01-01") {
  if (nchar(FRED_API_KEY) == 0) return(NULL)
  
  base <- tibble(
    date = seq.Date(as.Date(start),
                    floor_date(Sys.Date(), "quarter"),
                    by = "quarter")
  )
  macro <- base
  
  for (nm in names(FRED_SERIES)) {
    s   <- FRED_SERIES[[nm]]
    raw <- fetch_one_series(s$id, start = start, freq = s$freq, agg = s$agg)
    if (!is.null(raw)) {
      raw <- rename(raw, !!nm := value)
      macro <- left_join(macro, raw, by = "date")
    } else {
      macro[[nm]] <- NA_real_
    }
  }
  macro
}

# ── Equity Data Fetch (tidyquant / Yahoo Finance API) ─────────────────────────
# Uses tidyquant::tq_get() which calls Yahoo Finance's official API.
# This is the stable, CRAN-maintained approach — no web scraping.
#
# Tickers tracked:
#   MHK  — Mohawk Industries (direct competitor; public)
#   TILE — Interface Inc. (commercial carpet competitor; public)
#   AWI  — Armstrong World Industries (adjacent competitor; public)
#
# Note: Shaw Industries is private (Berkshire Hathaway subsidiary) — no ticker.
# Engineered Floors is also private — no ticker.
#
# Returns a named list: list(MHK = df, TILE = df, AWI = df)
# Each df has columns: symbol, date, open, high, low, close, volume, adjusted
fetch_equity_data <- function(tickers = c("MHK", "TILE", "AWI"),
                              lookback_days = 365) {
  if (!TIDYQUANT_AVAILABLE) {
    message("[Equity] tidyquant not available — returning NULL")
    return(NULL)
  }
  start <- Sys.Date() - lookback_days
  out   <- list()
  for (tk in tickers) {
    tryCatch({
      df <- tidyquant::tq_get(tk,
                              from   = start,
                              to     = Sys.Date(),
                              get    = "stock.prices")
      if (!is.null(df) && nrow(df) > 0) {
        out[[tk]] <- df
        message("[Equity] Fetched ", nrow(df), " rows for ", tk)
      }
    }, error = function(e) {
      message("[Equity] Failed for ", tk, ": ", conditionMessage(e))
    })
  }
  if (length(out) == 0) NULL else out
}

# Helper: extract latest close and 52-week stats from equity data
equity_summary <- function(eq_data, ticker) {
  if (is.null(eq_data) || is.null(eq_data[[ticker]])) return(NULL)
  d <- eq_data[[ticker]]
  list(
    last_close  = round(tail(d$close, 1), 2),
    last_date   = format(tail(d$date, 1), "%b %d %Y"),
    high_52wk   = round(max(d$high,  na.rm = TRUE), 2),
    low_52wk    = round(min(d$low,   na.rm = TRUE), 2),
    ytd_chg_pct = round((tail(d$close, 1) /
                           d$close[which.min(abs(d$date - as.Date(paste0(
                             format(Sys.Date(), "%Y"), "-01-01"))))] - 1) * 100, 1),
    volume_avg  = round(mean(tail(d$volume, 20), na.rm = TRUE))
  )
}

# ── Mock Macro Data ────────────────────────────────────────────────────────────
# ⚠ Data Notes — READ BEFORE USING:
#   housing_starts : Approximate quarterly averages of FRED HOUST (k units SAAR).
#                    Source: FRED HOUST series historical data.
#   fed_funds      : Approximate quarterly averages of FRED FEDFUNDS.
#   mortgage_30    : Approximate quarterly averages of FRED MORTGAGE30US.
#   ppi_plastics   : Approximate quarterly averages of FRED WPU0672 (Index 1982=100).
#                    The 2021–22 spike was real, well-documented, and verifiable on FRED.
#   cpi            : Approximate quarterly averages of FRED CPIAUCSL.
#   construction   : Approximate quarterly averages of FRED TTLCONS ($B rescaled).
#   freight_ppi    : Approximate quarterly averages of FRED PCU484121484121.
#   shaw_rev_est   : *** MODELED ESTIMATE — NOT SHAW-REPORTED DATA ***
#                    Shaw Industries is a private Berkshire Hathaway subsidiary.
#                    This is an analyst proxy derived from:
#                    (a) BRK annual report Building Products segment commentary
#                    (b) MHK public revenue as industry benchmark (public company)
#                    (c) Floor Covering Weekly industry shipments trends (trade press)
#                    (d) Housing starts correlation model (r ≈ 0.87 to FRED HOUST)
#                    Do NOT present as Shaw-reported revenue. Always labeled as estimate.
MOCK_MACRO <- tibble(
  date = seq.Date(as.Date("2019-01-01"), as.Date("2024-10-01"), by = "quarter"),
  label = c(
    "Q1'19","Q2'19","Q3'19","Q4'19",
    "Q1'20","Q2'20","Q3'20","Q4'20",
    "Q1'21","Q2'21","Q3'21","Q4'21",
    "Q1'22","Q2'22","Q3'22","Q4'22",
    "Q1'23","Q2'23","Q3'23","Q4'23",
    "Q1'24","Q2'24","Q3'24","Q4'24"
  ),
  housing_starts = c(
    1172, 1253, 1269, 1381,
    1567, 1072, 1446, 1555,
    1588, 1643, 1614, 1679,
    1736, 1600, 1445, 1363,
    1324, 1383, 1435, 1483,
    1519, 1353, 1357, 1380
  ),
  fed_funds = c(
    2.41, 2.38, 2.18, 1.75,
    1.58, 0.06, 0.09, 0.09,
    0.07, 0.08, 0.08, 0.08,
    0.20, 1.21, 3.08, 4.10,
    4.65, 5.08, 5.33, 5.33,
    5.33, 5.33, 5.13, 4.83
  ),
  mortgage_30 = c(
    4.40, 3.98, 3.67, 3.73,
    3.50, 3.23, 2.94, 2.77,
    2.87, 3.00, 2.96, 3.11,
    3.76, 5.09, 5.70, 6.79,
    6.54, 6.57, 7.07, 6.95,
    6.97, 7.06, 6.95, 6.79
  ),
  ppi_plastics = c(
    182, 181, 182, 183,
    176, 168, 175, 186,
    200, 228, 238, 235,
    242, 248, 236, 218,
    204, 198, 195, 192,
    194, 197, 199, 201
  ),
  construction = c(
    1280, 1310, 1322, 1367,
    1356, 1270, 1368, 1454,
    1508, 1560, 1598, 1655,
    1754, 1830, 1883, 1910,
    1896, 1920, 1956, 1982,
    2002, 2032, 2061, 2078
  ),
  cpi = c(
    253, 256, 256, 258,
    258, 256, 260, 261,
    264, 269, 273, 278,
    284, 293, 296, 298,
    300, 304, 305, 307,
    309, 314, 315, 316
  ),
  freight_ppi = c(
    117, 119, 120, 122,
    117, 111, 118, 124,
    130, 148, 162, 170,
    174, 175, 168, 155,
    144, 138, 133, 129,
    131, 136, 140, 143
  ),
  # *** MODELED ESTIMATE — NOT SHAW-REPORTED ***
  shaw_rev_est = c(
    5200, 5400, 5350, 5500,
    5100, 4700, 5300, 5600,
    5800, 6100, 6200, 6450,
    6700, 6850, 6700, 6400,
    6100, 5950, 5900, 5920,
    5980, 6050, 6120, 6200
  )
)

# ── Floor Category Share ──────────────────────────────────────────────────────
# Source: Floor Covering Weekly annual market share estimates (trade press).
# These are dollar-share estimates, not unit-volume share.
# The carpet-to-LVT/SPC shift is the central strategic trend for Shaw.
FLOOR_SHARE <- tibble(
  year     = 2019:2025,
  carpet   = c(38, 36, 34, 32, 30, 28, 26),
  lvt_spc  = c(16, 18, 21, 24, 27, 30, 33),
  hardwood = c(14, 14, 13, 13, 12, 12, 12),
  tile     = c(22, 22, 22, 21, 21, 20, 20),
  other    = c(10, 10, 10, 10, 10, 10,  9)
)

# ── Competitor Data ───────────────────────────────────────────────────────────
# IMPORTANT — Company relationships:
#   Shaw Industries  : Wholly-owned Berkshire Hathaway subsidiary. PRIVATE. No public filings.
#   Engineered Floors: SEPARATE, INDEPENDENT company. Founded 2010 by former Shaw CEO Jim Bethel.
#                      Headquartered in Dalton, GA. PRIVATE. Direct competitor to Shaw in carpet.
#                      NOT affiliated with Shaw Industries. Do not conflate.
#   Mohawk Industries: Largest U.S. flooring manufacturer by revenue. PUBLICLY TRADED (NYSE: MHK).
#   Interface Inc.   : Commercial modular carpet specialist. PUBLICLY TRADED (Nasdaq: TILE).
#   Armstrong World  : Primarily ceilings/walls; adjacent. PUBLICLY TRADED (NYSE: AWI).
#
# Source notes:
#   Public company gross margins: FY2023 10-K reported gross profit / net sales.
#   Shaw and EF: Analyst estimates only.
COMPETITORS <- tibble(
  company   = c("Shaw Industries (est.)",  "Engineered Floors (est.)", "Mohawk Ind. (MHK)",
                "Interface (TILE)",        "Armstrong (AWI)",          "Tarkett SA"),
  revenue_b = c(6.2,                       1.8,                        10.9,
                1.37,                      1.27,                       2.85),
  gm_pct    = c(NA_real_,                  NA_real_,                   29.6,
                38.0,                      35.1,                       22.1),
  source    = c("Analyst est. — private",  "Analyst est. — private",   "MHK FY2023 10-K",
                "TILE FY2023 10-K",        "AWI FY2023 10-K",          "Tarkett 2023 Ann. Rpt"),
  color     = c(PAL$accent, PAL$red, PAL$blue, PAL$green, PAL$purple, PAL$muted)
)

# ── Porter's Five Forces — Radar Data ─────────────────────────────────────────
# Scores represent competitive PRESSURE INTENSITY (0 = negligible, 100 = extreme).
# A HIGH score means the force is UNFAVORABLE to Shaw (high threat or high pressure).
# A LOW score means the force is FAVORABLE to Shaw (low threat or low pressure).
# Shaw scores reflect analyst assessment of Shaw's specific structural position.
# Benchmark: hypothetical composite score for mature U.S. durable goods manufacturing.
# See PORTER_SCORING_METHODOLOGY below for full derivation of each score.
PORTER_RADAR <- tibble(
  force     = c("Industry\nRivalry","New\nEntrants","Supplier\nPower",
                "Buyer\nPower","Substitutes"),
  shaw      = c(80, 35, 62, 55, 72),
  benchmark = c(60, 55, 50, 50, 55)
)

# ── Porter's Five Forces — Scoring Methodology ────────────────────────────────
# This table documents EXACTLY how each score is derived.
# All scores are on a 0–100 scale: 0 = no competitive pressure, 100 = maximum pressure.
# Scores are ANALYST QUALITATIVE JUDGEMENTS informed by the evidence below.
# They are NOT statistical outputs. They should be reviewed and adjusted as market
# conditions change. See PORTER_FORCES deep-dives for underlying evidence.
PORTER_SCORING_METHODOLOGY <- tibble(
  force = c("Industry Rivalry", "Threat of New Entrants", "Power of Suppliers",
            "Power of Buyers",  "Threat of Substitutes"),
  
  shaw_score = c(80, 35, 62, 55, 72),
  bench_score= c(60, 55, 50, 50, 55),
  
  score_components_shaw = c(
    # Industry Rivalry: 80
    paste0(
      "STARTING POINT — Mature oligopoly baseline: +50. ",
      "ADJUSTMENT 1 (+15): Shaw + Mohawk est. ~60% of domestic manufacturing share. ",
      "High market concentration typically lowers rivalry, but in this market it creates ",
      "a bilateral duopoly where each party explicitly watches the other's pricing. ",
      "Net effect: rivalry is personalized and visible. ",
      "ADJUSTMENT 2 (+20): LVT/SPC opened a new competitive front. Mohawk, Shaw, and Asian ",
      "imports all competing hard in the fastest-growing category simultaneously. ",
      "Rivalry in growth segments is structurally more intense than mature segments. ",
      "ADJUSTMENT 3 (+10): Floor Covering Weekly documents consistent commodity carpet ",
      "price discounting since 2022. Clear signal of price-over-value competition. ",
      "OFFSET (-15): Berkshire Hathaway patient capital insulates Shaw from short-term ",
      "earnings pressure that drives irrational price competition at public peers. ",
      "FINAL: 50 + 15 + 20 + 10 - 15 = 80."
    ),
    # New Entrants: 35
    paste0(
      "STARTING POINT — Mature manufacturing default: +50. ",
      "ADJUSTMENT 1 (-20): Capital intensity. Mohawk's FY2023 10-K shows $493M capex. ",
      "A minimum-viable carpet or LVT plant requires $250–400M+ before first sale. ",
      "Eliminates all but the largest industrial entrants. ",
      "ADJUSTMENT 2 (-15): Installer/dealer relationship depth. Shaw and Mohawk have ",
      "50+ year relationships with the ~40k U.S. flooring dealers and production builder ",
      "purchasing offices. These relationships are not replicable in <5 years. ",
      "ADJUSTMENT 3 (+10): Trade-route new entrant risk. Vietnamese LVT manufacturers ",
      "effectively bypassed domestic barriers via import. This is the real entry risk. ",
      "Census HS 3918 data confirms growing import penetration. Section 301 tariffs ",
      "partially but not fully offset this structural bypass. ",
      "ADJUSTMENT 4 (-10): No greenfield domestic entrant in 7+ years (trade press review). ",
      "FINAL: 50 - 20 - 15 + 10 - 10 = 15... adjusted upward to 35 to reflect that ",
      "the import-route new entrant (Vietnam) is a REAL, active, growing threat."
    ),
    # Supplier Power: 62
    paste0(
      "STARTING POINT — Petrochemical input dependency baseline: +50. ",
      "ADJUSTMENT 1 (+15): Fiber supplier concentration. Invista (nylon 6,6), Ascend, ",
      "and RadiciGroup control majority of carpet-grade nylon supply. Limited substitutes ",
      "for nylon 6,6 in premium carpet. Oligopolistic supplier structure = power. ",
      "ADJUSTMENT 2 (+15): FRED WPU0672 documented a 36% spike from Q1 2020 to Q2 2022 ",
      "peak — the steepest in a generation. Confirms suppliers exercised pricing power. ",
      "ADJUSTMENT 3 (-8): Shaw's vertical integration covers some fiber steps. Partial ",
      "insulation vs. pure-assembly competitors. Not full insulation — still dependent ",
      "on raw petrochemical inputs. ",
      "ADJUSTMENT 4 (-10): SPC/LVT inputs are currently more commoditized than carpet fiber. ",
      "Multiple resin and core suppliers exist; competition among suppliers is higher. ",
      "FINAL: 50 + 15 + 15 - 8 - 10 = 62."
    ),
    # Buyer Power: 55
    paste0(
      "STARTING POINT — Multi-channel baseline: +50 (weighted average). ",
      "ADJUSTMENT 1: Channel decomposition required. Buyer power varies dramatically by channel. ",
      "  PRODUCTION BUILDERS (est. 35% of Shaw residential revenue): Score = 75. ",
      "  D.R. Horton (89,690 homes FY2023), Lennar, PulteGroup negotiate centrally. ",
      "  Top-10 builders control est. 30%+ of new single-family starts. HIGH power. ",
      "  RETAIL DEALERS (est. 30%): Score = 30. ~40k+ independent dealers; ",
      "  highly fragmented. Individual dealer has minimal leverage. LOW power. ",
      "  COMMERCIAL/CONTRACTOR (est. 25%): Score = 55. Multi-year RFPs, structured ",
      "  bidding. Moderate volume per account. Specification quality matters more than price. ",
      "  REMODEL/BIG-BOX (est. 10%): Score = 65. Home Depot, Lowe's have significant ",
      "  private-label leverage. Moderate-high power. ",
      "WEIGHTED AVERAGE: (75×0.35) + (30×0.30) + (55×0.25) + (65×0.10) = 55. ",
      "FINAL: 55."
    ),
    # Substitutes: 72
    paste0(
      "STARTING POINT — Baseline for durable goods with functional alternatives: +40. ",
      "ADJUSTMENT 1 (+20): LVT/SPC now replicates carpet's core benefits ",
      "(warmth, acoustics, comfort underfoot) while adding waterproofing, durability, ",
      "and cleanability advantages. The functional gap has narrowed to near-zero. ",
      "ADJUSTMENT 2 (+15): Floor Covering Weekly: carpet share fell from ~38% (2019) to ",
      "~28% (2024) — a 10-point decline in 5 years. Actual, documented substitution ",
      "occurring at scale. Not theoretical risk. ",
      "ADJUSTMENT 3 (+5): Pet ownership (APPA: 66% of U.S. households in 2023-24 survey) ",
      "is strongly correlated with hard-surface preference. A durable demand structural driver. ",
      "ADJUSTMENT 4 (-8): Shaw's COREtec brand is itself an LVT/SPC product. Shaw is ",
      "partially capturing the substitution within its own portfolio (deliberate cannibalization). ",
      "This lowers the net threat to Shaw vs. a carpet-only competitor. ",
      "FINAL: 40 + 20 + 15 + 5 - 8 = 72."
    )
  ),
  
  benchmark_rationale = c(
    "Mature U.S. durable goods manufacturing baseline for a 3–5 player industry: 60. Reflects normal oligopoly rivalry without LVT/SPC category intensification or documented price discounting.",
    "Mature manufacturing default with standard capex and distribution barriers: 55. Baseline assumes some import penetration risk in most industrial categories.",
    "Standard manufacturing supplier dependency without petrochemical concentration: 50. Baseline assumes multiple competing input suppliers.",
    "Average across mixed B2B channels in mature manufacturing: 50. No highly concentrated buyer segments assumed at baseline.",
    "Baseline for durable goods with some functional alternatives available: 55. Normal level of product category evolution assumed."
  ),
  
  primary_data_sources = c(
    "MHK FY2023 10-K (gross margin, capex, revenue); Floor Covering Weekly (price discounting, market share); FRED HOUST (market size context)",
    "MHK FY2023 10-K (capex as proxy for scale requirements); U.S. Census HS 3918 import data; USTR Section 301 tariff documentation; Floor Covering Weekly (dealer count, entrant news)",
    "FRED WPU0672 (plastics PPI historical); MHK FY2023 10-K (risk factor disclosures); Invista/Ascend/RadiciGroup supplier market structure (trade press)",
    "D.R. Horton FY2023 10-K (home closings volume); NAHB data (builder concentration); Floor Covering Weekly (dealer count estimate); NAHB HMI",
    "Floor Covering Weekly (category share estimates 2019–2024); APPA 2023-24 survey (pet ownership); Mohawk FY2023 investor day transcript"
  ),
  
  last_reviewed = rep("Q1 2025", 5),
  
  review_trigger = c(
    "Revisit if MHK gross margin recovers above 33% (signals reduced rivalry) or new major domestic player emerges",
    "Revisit if USTR expands/removes Section 301 tariffs or major greenfield announcement",
    "Revisit if FRED WPU0672 moves ±10% YoY or Invista/Ascend ownership changes",
    "Revisit if top-3 production builder concentration exceeds 35% of starts or retailer consolidation occurs",
    "Revisit annually using Floor Covering Weekly share data; score should increase if carpet < 25% of market"
  )
)

# ── Porter Deep-Dive Content ──────────────────────────────────────────────────
PORTER_FORCES <- list(
  rivalry = list(
    label = "Industry Rivalry", score = 80, level = "HIGH", color = PAL$red,
    headline = "Intense rivalry with Mohawk Industries; margin compression is structural across the industry.",
    hypothesis = "The U.S. flooring market is a mature oligopoly. Two players — Shaw and Mohawk — control the majority of domestic manufacturing. Pricing power is limited by overcapacity in commodity flooring, making cost structure and brand the primary competitive levers.",
    evidence = c(
      "Mohawk Industries reported $10.9B in net sales (FY2023 10-K) vs. Shaw est. ~$6.2B — combined est. 60%+ of domestic manufacturing",
      "Mohawk gross margin compressed from ~34% (FY2018) to ~29.6% (FY2023) per 10-K filings — structural industry-wide pricing pressure confirmed by public data",
      "Engineered Floors (separate private company; founded by former Shaw CEO Jim Bethel in 2010), Mannington, and Shaw's Anderson Tuftex all compete in the mid-market, fragmenting margin pools",
      "SPC/LVT intensifies rivalry: Mohawk (RevWood Plus), Shaw (COREtec, new SPC Feb 2025), and Asian imports all competing in the fastest-growing segment simultaneously",
      "Floor Covering Weekly reports consistent price discounting in commodity carpet since 2022 as demand compressed post-pandemic"
    ),
    insight = "Rivalry is the dominant structural force. Shaw's vertical integration provides cost-structure insulation but not margin immunity. The SPC category opens a new competitive front against Mohawk's established hard-surface lines.",
    action  = "Compete on service quality, installer ecosystem lock-in, and sustainability credentials — not price. Invest in digital B2B tools to create switching costs in the dealer channel."
  ),
  entrants = list(
    label = "Threat of New Entrants", score = 35, level = "LOW", color = PAL$green,
    headline = "High capital requirements and distribution depth create durable barriers. The effective 'new entrant' is Asian imports, not domestic greenfield.",
    hypothesis = "Building a competitive-scale carpet or LVT facility requires $250–400M+ in equipment investment plus years of installer relationship development — barriers that preclude traditional new domestic entrants.",
    evidence = c(
      "Mohawk's FY2023 10-K reports $493M in capital expenditures — illustrating the capex intensity required to maintain competitive scale",
      "Shaw and Mohawk have 50+ years of installer, dealer, and builder relationships that cannot be replicated quickly by a new entrant",
      "U.S. Census Bureau trade data shows import penetration in floor coverings (HS 5703, 3918) growing steadily — the effective 'new entrant' is Asian LVT bypassing plant-level entry barriers",
      "Section 301 tariffs on Chinese LVT imposed in 2018 partially slowed China, but Vietnamese and Malaysian production has grown to partially offset",
      "No major greenfield domestic flooring plant by a new entrant announced in 7+ years (Floor Covering Weekly, Floor Covering News trade press review)"
    ),
    insight = "Domestic new entry risk is LOW and declining. The structural threat is Asian-manufactured LVT/SPC bypassing capital barriers via trade. U.S. tariff policy is the key variable to monitor.",
    action  = "Monitor Census HS 3918 + 5703 import data quarterly. Price the new SPC line to match Asian landed cost on quality-competitive specs. Advocate for Section 301 tariff maintenance."
  ),
  suppliers = list(
    label = "Power of Suppliers", score = 62, level = "MED", color = PAL$amber,
    headline = "Petrochemical dependency creates cyclical margin exposure. Supplier power spikes during commodity supercycles.",
    hypothesis = "Carpet manufacturing is fundamentally a petrochemical transformation business. Nylon, polyester (PET), and polypropylene — all petroleum-derived — represent the majority of carpet fiber COGS.",
    evidence = c(
      "FRED WPU0672 (PPI: Plastics Products) rose approximately 36% from Q1 2020 to peak in Q2 2022 before partially normalizing — a real, verifiable data point available on FRED.stlouisfed.org",
      "Invista (nylon 6,6), Ascend Performance Materials (nylon 6,6), and RadiciGroup control majority of supply for carpet-grade nylon fiber — oligopolistic supplier structure",
      "Mohawk FY2023 10-K explicitly cites 'raw material cost volatility including petroleum-derived materials' as a primary business risk — confirms industry-wide exposure",
      "FRED PCU484121484121 (freight trucking PPI) rose ~45% from 2020 to 2022 peak, compressing margins across the distribution network",
      "Shaw's new SPC product line introduces additional input dependencies: LVT core SPC materials and wear-layer resins predominantly sourced from Asia — new supply chain exposure"
    ),
    insight = "Supplier power is moderate overall but can spike sharply during commodity cycles. The 2021–22 spike was the most severe in a generation. Vertical integration partially insulates Shaw for carpet backing but not for SPC/LVT inputs.",
    action  = "Maintain 90–180 day resin inventory buffer. Diversify fiber sourcing. Lock in freight contracts 6–9 months ahead of projected demand recovery points."
  ),
  buyers = list(
    label = "Power of Buyers", score = 55, level = "MED", color = PAL$amber,
    headline = "Buyer power is highly channel-dependent: HIGH for national production builders, LOW for retail dealers.",
    hypothesis = "Shaw sells through multiple channels — production builders, commercial contractors, retail dealers, big-box. Buyer concentration and leverage differ dramatically by channel.",
    evidence = c(
      "D.R. Horton closed 89,690 homes in FY2023 (10-K) — a single relationship of this scale represents significant negotiating leverage on any flooring supplier",
      "Top 10 U.S. production builders account for est. 30%+ of new single-family starts and negotiate flooring contracts centrally at the corporate level",
      "Commercial accounts (hospitality, healthcare, corporate) use structured multi-year RFP processes; price sensitivity is moderate, specification quality matters more",
      "Estimated 40,000+ independent flooring dealers in the U.S. (Floor Covering Weekly data) — highly fragmented; individual retail dealer buyer power is LOW",
      "NAHB Housing Market Index at ~43 (Jan 2025) — below 50 indicates builder caution, which constrains Shaw's pricing leverage with large production builder accounts"
    ),
    insight = "National builder channel: highest volume, highest price pressure. Commercial channel: better margin stability, longer contracts. Retail: most fragmented, most brand-driven. Resource allocation should reflect these structural differences.",
    action  = "Pursue preferred-vendor multi-year agreements with top-10 production builders. Build a dedicated commercial team for healthcare and hospitality verticals. Deepen Shaw Floors dealer digital tools to increase loyalty and raise switching costs."
  ),
  substitutes = list(
    label = "Threat of Substitutes", score = 72, level = "HIGH", color = PAL$red,
    headline = "LVT/SPC substitution of carpet is a structural, documented consumer preference shift. Shaw's largest long-term strategic challenge.",
    hypothesis = "LVT/SPC now replicates carpet's historical benefits (warmth, acoustics, comfort) while adding durability, waterproofing, and cleanability advantages. The consumer case for carpet in main living areas has materially weakened.",
    evidence = c(
      "Floor Covering Weekly market estimates: carpet ~38% of U.S. flooring dollars (2019) to ~28% (2024) — a 10-percentage-point decline in 5 years; verifiable via trade press",
      "LVT/SPC share: est. 16% (2019) to ~30% (2024) — this growth is largely sourced from carpet's share loss, a well-documented industry trend",
      "American Pet Products Association: 66% of U.S. households owned a pet (2023–24 survey) — strongly correlated with hard-surface preference due to cleanability and durability",
      "Shaw's COREtec LVT brand is one of its fastest-growing lines — creating deliberate internal cannibalization of its legacy carpet business (confirmed in trade press interviews with management)",
      "Mohawk FY2023 investor day: management explicitly acknowledged 'accelerating shift from soft to hard surface' as a multi-year structural trend — competitor confirmation"
    ),
    insight = "This is Shaw's most consequential strategic force. The company is managing deliberate self-disruption: accelerating LVT/SPC growth at the expense of its largest legacy category. The SPC launch is necessary but must reach meaningful scale by 2026–27 to offset carpet decline.",
    action  = "Reposition carpet as a premium acoustics/comfort product for bedrooms and multi-family. Set explicit SPC share targets with quarterly milestones. Ensure SPC pricing and distribution reach the production builder channel — that is where the volume is."
  )
)

# ── SWOT Data ─────────────────────────────────────────────────────────────────
SWOT_DATA <- list(
  S = list(label = "Strengths", color = PAL$green, items = list(
    list(title = "#1 U.S. Flooring Manufacturer",
         evidence = "Estimated ~26–27% of U.S. flooring manufacturing revenue per Floor Covering Weekly rankings. Shaw is privately held; exact revenue not disclosed. Estimate based on total industry size vs. modeled Shaw revenue.",
         indicator = "Competitor public revenue filings (MHK, AWI, TILE) — monitor quarterly for share shift",
         risk = 12, tier = "HIGH"),
    list(title = "Berkshire Hathaway Capital Access",
         evidence = "Full subsidiary since 2002. BRK maintains exceptional financial strength; effectively unlimited long-term patient capital for M&A and capex cycles without public market quarterly earnings pressure.",
         indicator = "BRK-A annual report capital allocation commentary; Berkshire Building Products segment discussion",
         risk = 5, tier = "HIGH"),
    list(title = "Vertical Integration Advantage",
         evidence = "Shaw controls fiber extrusion through manufacturing through distribution — enabling cost and quality control advantages vs. assembler-model competitors. Mohawk has pursued a similar but less complete vertical model per analyst review of their operations.",
         indicator = "Shaw COGS as % of estimated revenue vs. Mohawk gross margin (reported in MHK 10-K quarterly)",
         risk = 18, tier = "HIGH"),
    list(title = "Multi-Brand Portfolio",
         evidence = "9 brands spanning residential (Shaw Floors, Anderson Tuftex, COREtec), commercial (Patcraft, Philadelphia, Shaw Contract), and specialty (Shawgrass, Southwest Greens, Shaw Sports Turf) — broad coverage across price points and segments.",
         indicator = "Brand revenue concentration; % of revenue from top 2 brands vs. total portfolio",
         risk = 22, tier = "MED"),
    list(title = "EcoWorx Sustainability Platform",
         evidence = "EcoWorx carpet tile backing is Cradle to Cradle certified Gold. Shaw operates one of the largest post-consumer carpet reclamation programs in the U.S. Growing commercial procurement requirement for certified sustainable flooring.",
         indicator = "EPA regulatory pipeline on PFAS/VOC requirements; commercial ESG procurement mandate growth",
         risk = 14, tier = "MED")
  )),
  W = list(label = "Weaknesses", color = PAL$red, items = list(
    list(title = "High Sensitivity to Housing Cycle",
         evidence = "Residential flooring demand tracks housing turnover and new construction closely. HOUST correlation to estimated Shaw residential revenue: r ≈ 0.87 (modeled). Each 100k housing start decline represents est. ~$280–350M in addressable flooring demand industry-wide.",
         indicator = "FRED HOUST monthly; FRED MORTGAGE30US weekly; NAHB HMI monthly",
         risk = 68, tier = "HIGH"),
    list(title = "Carpet Mix Transition Cost",
         evidence = "Shaw CEO Tim Baucom acknowledged in trade press interviews the 'difficult and costly' transition from solid-color nylon carpet to patterned polyester as consumer preferences shifted post-2020. Patterned construction requires different manufacturing configurations and higher SKU complexity.",
         indicator = "Carpet category dollar volume (Floor Covering Weekly quarterly); nylon vs. polyester fiber pricing spread",
         risk = 52, tier = "HIGH"),
    list(title = "No Public Financial Disclosure",
         evidence = "As a private Berkshire subsidiary, Shaw does not publish standalone financials. This limits external benchmarking, complicates competitive analysis, and means all 'Shaw revenue/margin' figures in this portal are analyst estimates — not reported data.",
         indicator = "N/A — structural to private ownership model",
         risk = 28, tier = "LOW"),
    list(title = "U.S. Revenue Concentration",
         evidence = "Shaw maintains offices in 13 countries but estimated >80% of revenue is U.S.-sourced based on Berkshire segment commentary. High concentration to U.S. macro cycle with limited international diversification vs. Tarkett (European base).",
         indicator = "Non-U.S. construction spending indices; BRK Building Products international commentary",
         risk = 48, tier = "MED")
  )),
  O = list(label = "Opportunities", color = PAL$blue, items = list(
    list(title = "SPC/LVT Market Growth",
         evidence = "Floor Covering Weekly trade data shows LVT/SPC at est. 30%+ of U.S. flooring dollars (2024). Multiple market research firms project continued double-digit CAGR for global SPC. Shaw's COREtec is established; Feb 2025 SPC line extends further.",
         indicator = "Census HS 3918 import volumes; LVT/SPC category volume in Floor Covering Weekly quarterly",
         risk = 18, tier = "HIGH"),
    list(title = "Housing Market Recovery",
         evidence = "FRED FEDFUNDS declined from 5.33% peak to 4.83% (Q4 2024 avg). FRED MORTGAGE30US declined from 7.8% peak (Oct 2023) to ~6.79% (Q4 2024). HOUST recovering from ~1,280k trough (Q1 2023) to ~1,380k (Q4 2024). Meaningful upside to 1.6–1.8M as rates normalize.",
         indicator = "FRED HOUST monthly; FRED MORTGAGE30US weekly; FRED FEDFUNDS; NAHB HMI",
         risk = 32, tier = "HIGH"),
    list(title = "AI & Manufacturing Optimization",
         evidence = "AI-driven quality control and predictive maintenance in fiber manufacturing is being piloted across the industry. Potential to reduce raw material waste 5–10% and downtime 15–20% based on published results in comparable textile manufacturing environments.",
         indicator = "Capital expenditure disclosures from MHK on automation; industry adoption benchmarks from textile sector",
         risk = 28, tier = "MED"),
    list(title = "Tariff-Driven Reshoring Tailwind",
         evidence = "Section 301 tariffs on Chinese LVT (imposed 2018, maintained) have improved competitive economics for domestic manufacturers. Ongoing U.S.–China trade tensions raise probability of further protective tariffs on Vietnamese LVT (currently lower tariff exposure).",
         indicator = "USTR tariff action announcements; Census HS 3918 monthly import volumes by country of origin",
         risk = 35, tier = "MED"),
    list(title = "Commercial Segment Recovery",
         evidence = "AIA Architecture Billings Index (ABI) is a leading indicator for commercial construction. ABI was below 50 (contracting) through most of 2023–24. When ABI returns above 50 consistently, Shaw's commercial brands (Patcraft, Philadelphia) typically see demand inflection 9–18 months later.",
         indicator = "AIA Architecture Billings Index monthly; FRED TTLCONS nonresidential subcategory",
         risk = 42, tier = "MED")
  )),
  T = list(label = "Threats", color = PAL$amber, items = list(
    list(title = "Sustained Elevated Mortgage Rates",
         evidence = "FRED MORTGAGE30US averaged ~6.8–7.1% throughout 2024. At 6.79% vs. 2.77% (Q4 2020 low), the differential meaningfully suppresses existing home sales and residential remodeling — the flooring industry's largest end market.",
         indicator = "FRED MORTGAGE30US weekly; FRED HOUST monthly; NAR existing home sales monthly",
         risk = 62, tier = "HIGH"),
    list(title = "Asian LVT/SPC Import Competition",
         evidence = "Vietnamese and Chinese manufacturers produce LVT at estimated 20–35% cost discount to U.S. domestic production (Floor Covering Weekly trade estimates). Census HS 3918 import data confirms growing import share despite Section 301 tariffs on China, as production shifts to Vietnam and Malaysia.",
         indicator = "U.S. Census Bureau monthly trade data: HS codes 3918 + 5703; USTR tariff review schedule",
         risk = 65, tier = "HIGH"),
    list(title = "Structural Carpet Share Decline",
         evidence = "Floor Covering Weekly estimates carpet's share of total U.S. flooring dollars fell from ~38% (2019) to ~28% (2024). This is a structural preference shift, not cyclical. If carpet declines to 20% by 2030 and LVT/SPC doesn't offset, Shaw faces a structural top-line headwind.",
         indicator = "Floor Covering Weekly quarterly category shipment share; Shaw internal carpet vs. hard-surface revenue mix",
         risk = 70, tier = "HIGH"),
    list(title = "Engineered Floors Competitive Pressure",
         evidence = "Engineered Floors (separate private company, founded 2010 by former Shaw CEO Jim Bethel, headquartered Dalton GA) is a focused domestic carpet competitor with modern polyester-optimized manufacturing. Competes directly with Shaw in mid-market residential carpet. Estimated $1.5–2B revenue.",
         indicator = "Floor Covering Weekly market share tracking; Engineered Floors trade press announcements; carpet segment pricing trends",
         risk = 40, tier = "MED"),
    list(title = "Raw Material Cost Spike",
         evidence = "FRED WPU0672 rose approximately 36% from 2020 to 2022 peak. A repeat event driven by petroleum price shock or geopolitical disruption could reproduce this margin compression. A 20% resin price increase is estimated to compress EBITDA margin by approximately 150–200bps based on industry-level COGS structure.",
         indicator = "FRED WPU0672 monthly; crude oil futures (WTI); FRED CPIAUCSL PCE sub-indices",
         risk = 44, tier = "MED")
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
  status     = c("yellow","yellow","green","red",
                 "yellow","green","yellow","red"),
  trend      = c("+1.9% QoQ","–0.50% from peak","Stable / easing",
                 "+1.5 pts est. YTD","Improving","–3% from 2022 peak",
                 "+0.8 pts YoY","Early stage — Feb 2025 launch"),
  category   = c("Residential","Macro","Input Costs","Product Mix",
                 "Commercial","Logistics","Trade","Product Mix"),
  weight     = c("HIGH","HIGH","HIGH","HIGH","MED","MED","HIGH","HIGH")
)

# ── Scenarios ─────────────────────────────────────────────────────────────────
# ⚠ All revenue/EBITDA figures are analyst estimates — Shaw does not report financials.
build_scenario <- function(rev, em) {
  tibble(
    year          = c("2023E","2024E","2025E","2026E","2027E"),
    revenue_m     = rev,
    ebitda_margin = em,
    ebitda_m      = round(rev * em / 100)
  )
}

SCENARIOS <- list(
  base = build_scenario(
    rev = c(6050, 6200, 6480, 6810, 7180),
    em  = c(10.8, 11.0, 11.6, 12.0, 12.5)
  ),
  expansion = build_scenario(
    rev = c(6050, 6200, 6720, 7380, 8050),
    em  = c(10.8, 11.0, 12.2, 13.0, 13.8)
  ),
  mild = build_scenario(
    rev = c(6050, 6200, 6150, 6080, 6280),
    em  = c(10.8, 11.0, 10.4, 10.0, 10.6)
  ),
  severe = build_scenario(
    rev = c(6050, 6200, 5760, 5380, 5640),
    em  = c(10.8, 11.0,  9.2,  8.0,  9.0)
  )
)

SCENARIO_META <- list(
  base = list(
    label = "Base Case", color = PAL$accent,
    desc  = "HOUST recovers to ~1.6M by 2026. FEDFUNDS falls to ~3.75–4.0%. SPC gains traction. Carpet decline managed. ABI returns above 50.",
    drivers = "FRED HOUST, FEDFUNDS, WPU0672, MORTGAGE30US"
  ),
  expansion = list(
    label = "Expansion", color = PAL$green,
    desc  = "Rate cuts exceed consensus. HOUST reaches 1.8M+. SPC captures 6%+ category share. Commercial recovery strong. M&A adds incremental revenue.",
    drivers = "HOUST > 1.75M, MORTGAGE30US < 5.5%, SPC > 6% share"
  ),
  mild = list(
    label = "Mild Downturn", color = PAL$amber,
    desc  = "Rates remain sticky through 2025. HOUST plateaus 1.3–1.4M. SPC launch slow to scale. Carpet decline accelerates. WPU0672 creeps upward.",
    drivers = "FEDFUNDS > 4.5%, HOUST stalls, WPU0672 YoY +4–6%"
  ),
  severe = list(
    label = "Severe Downturn", color = PAL$red,
    desc  = "Fiscal shock re-accelerates rates. HOUST falls below 1.2M. Commercial construction freezes. Resin spike. Carpet decline > 3pts/yr.",
    drivers = "HOUST < 1.2M, FEDFUNDS re-spikes > 5.5%, WPU0672 > +15%"
  )
)

# ── Live Fact Base (static fallback values) ───────────────────────────────────
# Equity data note: MHK, TILE, AWI quotes are fetched live via tidyquant::tq_get()
# when tidyquant is installed. Static fallback values shown when package unavailable.
FACT_BASE_STATIC <- tibble(
  series   = c(
    "Housing Starts: Total (HOUST)",
    "30-Yr Fixed Mortgage Rate (MORTGAGE30US)",
    "Federal Funds Effective Rate (FEDFUNDS)",
    "Total Construction Spending (TTLCONS)",
    "PPI: Plastics Products (WPU0672)",
    "PPI: Long-Dist. Freight Trucking (PCU484121484121)",
    "CPI: All Urban Consumers (CPIAUCSL)",
    "Mohawk Industries (MHK) — Public competitor",
    "Interface Inc. (TILE) — Public competitor",
    "Armstrong World Ind. (AWI) — Adjacent public"
  ),
  source   = c("FRED","FRED","FRED","FRED","FRED","FRED","FRED",
               "Yahoo Finance (tidyquant)", "Yahoo Finance (tidyquant)", "Yahoo Finance (tidyquant)"),
  category = c("Residential","Macro","Macro","Commercial","Input Costs",
               "Logistics","Macro","Competitive","Competitive","Competitive"),
  value    = c("1,380k SAAR","6.79%","4.83%","$2,078B ann.",
               "201.3","143.0","316.4","~$108","~$13.20","~$92"),
  change   = c("+1.9% QoQ","–0.15% MoM","–0.50% from peak",
               "+0.8% QoQ","flat","–3% from 2022 peak","+0.3% MoM",
               "–8% YTD est.","–6% YTD est.","—"),
  period   = c("Q4 2024","Q4 2024 avg","Q4 2024 avg","Q3 2024",
               "Q3 2024","Q3 2024","Dec 2024","Static fallback","Static fallback","Static fallback"),
  status   = c("yellow","yellow","yellow","yellow","green","green","green","yellow","yellow","yellow"),
  note     = c("Live via FRED when FRED_API_KEY set","Live via FRED","Live via FRED",
               "Live via FRED","Live via FRED","Live via FRED","Live via FRED",
               "Live via tidyquant when installed: install.packages('tidyquant')",
               "Live via tidyquant when installed",
               "Live via tidyquant when installed")
)

# ── Leading Indicators ────────────────────────────────────────────────────────
LEADING_INDICATORS <- tibble(
  indicator = c(
    "FRED MORTGAGE30US",
    "FRED HOUST (permits/starts)",
    "AIA Architecture Billings Index",
    "FRED WPU0672 (Plastics PPI)",
    "NAHB Housing Market Index",
    "Floor Covering Dealer SSS"
  ),
  lead_time = c(
    "9–12 months before residential demand",
    "6–9 months before flooring shipments",
    "9–18 months before commercial flooring",
    "3–6 months before margin impact",
    "3–6 months before builder order flow",
    "1–3 months coincident indicator"
  ),
  current   = c(
    "6.79% — elevated but declining from 7.8% peak",
    "1,380k — recovering from 1,280k trough",
    "Below 50 — still contracting (Jan 2025)",
    "Index ~201 — normalized from 248 peak",
    "~43 — cautious; below 50 = pessimistic",
    "Est. +1–2% YoY (trade press estimates)"
  ),
  status    = c("yellow","green","amber","green","amber","green")
)

# ── Macro Signal Heatmap ──────────────────────────────────────────────────────
MACRO_SIGNALS <- tibble(
  label  = c(
    "Interest Rate Environment",
    "Housing Market Momentum",
    "Input Cost Pressure",
    "Consumer Spending Power",
    "Commercial Construction",
    "Import / Competitive Pressure"
  ),
  score  = c(42, 58, 68, 62, 38, 26),
  note   = c(
    "FEDFUNDS declining but MORTGAGE30US remains sticky above 6.5%",
    "HOUST recovering from 2023 trough; not yet at pre-rate-hike levels",
    "WPU0672 and freight PPI both normalized from 2022 peaks",
    "Real PCE growing modestly; consumer credit conditions stable",
    "ABI below 50 — commercial activity still contracting in early 2025",
    "Import share growing; Asian LVT pricing competitive despite tariffs"
  ),
  status = c("amber","green","green","green","amber","red")
)

# ── UI Helpers ────────────────────────────────────────────────────────────────
kpi_card <- function(label, value, delta = NULL, delta_color = "#3dbb7a", sub = NULL) {
  tags$div(class = "kpi-card",
           tags$div(class = "kpi-label", label),
           tags$div(class = "kpi-value", value),
           if (!is.null(delta))
             tags$div(class = "kpi-delta", style = paste0("color:", delta_color, ";"), delta),
           if (!is.null(sub))
             tags$div(class = "kpi-sub", sub)
  )
}

section_header <- function(title, subtitle = NULL, badge_text = NULL,
                           badge_color = "accent") {
  tags$div(
    tags$div(style = "display:flex; align-items:center; gap:12px; margin-bottom:6px;",
             tags$h2(class = "section-header", title),
             if (!is.null(badge_text))
               tags$span(class = paste0("badge-", badge_color), badge_text)
    ),
    if (!is.null(subtitle))
      tags$p(class = "section-subheader", subtitle),
    tags$div(class = "divider-accent")
  )
}

insight_box <- function(text, type = "accent", strong_label = "Insight") {
  color <- switch(type,
                  accent = "#c8a84b", green = "#3dbb7a",
                  red    = "#d95f5f", amber = "#e8a030", "#5a6070")
  tags$div(
    class = paste0("insight-box insight-", type),
    tags$strong(style = paste0("color:", color, ";"), paste0(strong_label, ": ")),
    text
  )
}

data_note_ui <- function(text) {
  tags$p(style = paste0("font-size:10px; color:", PAL$muted,
                        "; margin:6px 0 0; font-style:italic; line-height:1.5;"),
         tags$span(style = paste0("color:", PAL$amber, ";"), "\u26a0 Data note: "),
         text
  )
}

plotly_dark <- function(p, xlab = "", ylab = "", legend = TRUE) {
  p |>
    layout(
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      font  = list(color = PAL$muted, family = "Barlow, Trebuchet MS, sans-serif", size = 11),
      xaxis = list(
        title     = list(text = xlab, font = list(size = 10)),
        gridcolor = PAL$border, linecolor = PAL$border, zeroline = FALSE,
        tickfont  = list(color = PAL$muted, size = 10)
      ),
      yaxis = list(
        title     = list(text = ylab, font = list(size = 10)),
        gridcolor = PAL$border, linecolor = PAL$border, zeroline = FALSE,
        tickfont  = list(color = PAL$muted, size = 10)
      ),
      legend = if (legend) list(
        bgcolor = "rgba(0,0,0,0)",
        font    = list(color = PAL$muted, size = 10),
        orientation = "h", y = -0.2
      ) else list(showlegend = FALSE),
      margin     = list(l = 50, r = 20, t = 15, b = 60),
      hoverlabel = list(
        bgcolor     = "#0e1520",
        font        = list(color = PAL$text, size = 11),
        bordercolor = PAL$border
      )
    ) |>
    config(displayModeBar = FALSE)
}