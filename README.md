# Shaw Industries CEO Strategy Portal
### R Shiny | ELT-Ready Strategic Intelligence Platform

---

## Quick Start

```r
# 1. Install required packages
install.packages(c(
  "shiny", "bslib", "plotly", "DT", "fredr",
  "dplyr", "tidyr", "lubridate", "shinyjs", "scales", "htmltools"
))

# 2. Set your API keys (optional — app works in demo mode without them)
# FRED key:    https://fred.stlouisfed.org/docs/api/api_key.html
# Census key:  https://api.census.gov/data/key_signup.html

# 3. Run the app
shiny::runApp("shaw_portal/")
```

---

## Login Credentials

| Username  | Password   | Role     |
|-----------|------------|----------|
| shaw      | ELT2025    | ELT      |
| analyst   | Shaw2025   | Analyst  |
| demo      | demo       | Demo     |

Add your FRED API key at the login screen for live data.  
Leave blank to run in **demo mode** with mock data.

---

## File Structure

```
shaw_portal/
├── global.R          # Packages, palette, theme, data fetching, static data
├── ui.R              # Full portal UI (login + 6 tabs)
├── server.R          # All reactive logic and outputs
├── www/
│   └── styles.css    # Dark theme CSS
└── README.md
```

---

## Modules

### 1. Executive Summary
- KPI row: Revenue proxy, housing starts, fed rate, resin index, freight, market share
- Revenue vs. housing starts correlation chart (r = 0.87)
- Input cost & rate environment overlay
- Competitor gross margin benchmarking
- Floor category share shift (carpet erosion → LVT growth)
- "What Changed / Why / What It Means / What to Do" strategic narrative

### 2. Porter's Five Forces
- Radar chart: Shaw vs. industry average across all 5 forces
- Click-to-explore deep dive for each force
- Full Hypothesis → Evidence → Insight → Action logic for each

### 3. Dynamic SWOT Engine
- 18 factors across 4 quadrants, all evidence-backed
- Risk probability scoring per factor
- Leading indicator for each item
- Expandable — no static text

### 4. Strategy Assumptions Monitor
- 8 strategic assumptions tracked against real thresholds
- RAG status (🟢 🟡 🔴), category filter, priority weighting
- Progress bars showing distance to threshold
- Executive directive callout on RED items

### 5. Scenario & Sensitivity Modeling
- Base / Expansion / Mild Downturn / Severe Downturn scenarios
- 3-year revenue, margin, EBITDA projections per scenario
- 6 leading indicators with historical lead times

### 6. Live Fact Base
- 12 live data series from FRED, BLS, and Census
- Macro signal heatmap
- Competitive intelligence panel

---

## FRED Series Used

| Series ID   | Description                      | Module Used              |
|-------------|----------------------------------|--------------------------|
| HOUST       | Housing Starts (k units)         | Exec Summary, Scenarios  |
| FEDFUNDS    | Federal Funds Rate               | Exec Summary, Assumptions|
| MORTGAGE30US| 30-Year Fixed Mortgage Rate      | Fact Base                |
| WPU0911     | PPI: Plastics Materials & Resins | Exec Summary, SWOT       |
| CPIAUCSL    | CPI: All Urban Consumers         | Fact Base                |
| PCE         | Personal Consumption Expenditures| Fact Base                |
| TTLCONS     | Total Construction Spending      | Exec Summary             |

---

## Adding Live Data Refresh

For production with Posit Connect / shinyapps.io:

```r
# In server.R, add a timer for weekly auto-refresh:
auto_refresh <- reactiveTimer(intervalMs = 7 * 24 * 60 * 60 * 1000)

observe({
  auto_refresh()
  if (rv$logged_in && nchar(rv$fred_key) > 0) {
    rv$macro_data <- fetch_fred_data(rv$fred_key)
  }
})
```

---

## Production Security Options

| Option              | Notes                                      |
|---------------------|--------------------------------------------|
| **Posit Connect**   | Native SSO, role-based access, audit logs  |
| **shinymanager**    | `install.packages("shinymanager")` — drop-in auth |
| **Auth0 + shiny**   | Enterprise SSO, MFA, directory integration |
| **AWS + nginx**     | Basic auth or Cognito for fully custom hosting |

---

## Customization

- **Credentials**: Edit `VALID_USERS` list in `global.R`
- **Color palette**: Edit `PAL` list in `global.R`  
- **Assumptions**: Edit `ASSUMPTIONS` tibble in `global.R`
- **SWOT items**: Edit `SWOT_DATA` list in `global.R`
- **Scenarios**: Edit `SCENARIOS` list in `global.R`

---

*Shaw Industries Group, Inc. — Confidential ELT Document*  
*Built with R Shiny + bslib + plotly + fredr*
