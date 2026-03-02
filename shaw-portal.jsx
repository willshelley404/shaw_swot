import { useState, useEffect, useRef } from "react";
import {
  LineChart, Line, BarChart, Bar, AreaChart, Area, ScatterChart, Scatter,
  RadarChart, Radar, PolarGrid, PolarAngleAxis, PolarRadiusAxis,
  XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer,
  ReferenceLine, Cell
} from "recharts";

// ─── DESIGN TOKENS ────────────────────────────────────────────────────────────
const C = {
  bg: "#080b10",
  panel: "#0e1219",
  border: "#1e2530",
  borderBright: "#2e3a48",
  text: "#d4cfc8",
  muted: "#5a6070",
  accent: "#c8a84b",
  accentDim: "rgba(200,168,75,0.15)",
  accentBorder: "rgba(200,168,75,0.35)",
  blue: "#3d8fc4",
  blueDim: "rgba(61,143,196,0.15)",
  green: "#3dbb7a",
  greenDim: "rgba(61,187,122,0.12)",
  red: "#d95f5f",
  redDim: "rgba(217,95,95,0.12)",
  amber: "#e8a030",
  amberDim: "rgba(232,160,48,0.12)",
};

const FONT = { heading: "'Palatino Linotype', Palatino, 'Book Antiqua', serif", body: "'Trebuchet MS', 'Lucida Grande', sans-serif" };

// ─── MOCK DATA ─────────────────────────────────────────────────────────────────
const macroTimeSeries = [
  { q: "Q1'21", housingStarts: 1580, commercialConstruction: 840, resinIndex: 95, freight: 82, cpi: 102, fed: 0.25, shawRevProxy: 5800 },
  { q: "Q2'21", housingStarts: 1620, commercialConstruction: 855, resinIndex: 118, freight: 104, cpi: 104, fed: 0.25, shawRevProxy: 6100 },
  { q: "Q3'21", housingStarts: 1590, commercialConstruction: 870, resinIndex: 130, freight: 130, cpi: 106, fed: 0.25, shawRevProxy: 6250 },
  { q: "Q4'21", housingStarts: 1700, commercialConstruction: 895, resinIndex: 128, freight: 148, cpi: 108, fed: 0.25, shawRevProxy: 6500 },
  { q: "Q1'22", housingStarts: 1740, commercialConstruction: 910, resinIndex: 135, freight: 145, cpi: 113, fed: 0.5,  shawRevProxy: 6700 },
  { q: "Q2'22", housingStarts: 1680, commercialConstruction: 930, resinIndex: 140, freight: 138, cpi: 117, fed: 1.75, shawRevProxy: 6850 },
  { q: "Q3'22", housingStarts: 1540, commercialConstruction: 945, resinIndex: 130, freight: 120, cpi: 118, fed: 3.0,  shawRevProxy: 6700 },
  { q: "Q4'22", housingStarts: 1360, commercialConstruction: 960, resinIndex: 115, freight: 100, cpi: 119, fed: 4.25, shawRevProxy: 6400 },
  { q: "Q1'23", housingStarts: 1310, commercialConstruction: 970, resinIndex: 104, freight: 88,  cpi: 120, fed: 4.75, shawRevProxy: 6100 },
  { q: "Q2'23", housingStarts: 1370, commercialConstruction: 980, resinIndex: 100, freight: 82,  cpi: 121, fed: 5.0,  shawRevProxy: 5950 },
  { q: "Q3'23", housingStarts: 1390, commercialConstruction: 985, resinIndex: 98,  freight: 79,  cpi: 122, fed: 5.25, shawRevProxy: 5900 },
  { q: "Q4'23", housingStarts: 1460, commercialConstruction: 975, resinIndex: 96,  freight: 77,  cpi: 122, fed: 5.25, shawRevProxy: 5920 },
  { q: "Q1'24", housingStarts: 1490, commercialConstruction: 968, resinIndex: 97,  freight: 81,  cpi: 123, fed: 5.25, shawRevProxy: 5980 },
  { q: "Q2'24", housingStarts: 1530, commercialConstruction: 960, resinIndex: 99,  freight: 86,  cpi: 124, fed: 5.0,  shawRevProxy: 6060 },
  { q: "Q3'24", housingStarts: 1560, commercialConstruction: 955, resinIndex: 101, freight: 90,  cpi: 124, fed: 4.75, shawRevProxy: 6140 },
  { q: "Q4'24", housingStarts: 1590, commercialConstruction: 958, resinIndex: 103, freight: 94,  cpi: 125, fed: 4.5,  shawRevProxy: 6230 },
];

const competitorData = [
  { company: "Shaw (Est.)", revenue: 6230, margin: 11.8, revGrowth: 1.5, color: C.accent },
  { company: "Mohawk", revenue: 10380, margin: 7.2, revGrowth: -2.1, color: C.blue },
  { company: "Interface", revenue: 1290, margin: 14.1, revGrowth: 3.8, color: C.green },
  { company: "Armstrong", revenue: 1240, margin: 9.4, revGrowth: -1.0, color: "#a070c0" },
  { company: "Tarkett", revenue: 3100, margin: 6.8, revGrowth: 0.9, color: C.muted },
];

const floorCatShare = [
  { year: "2019", carpet: 38, lvt: 16, hardwood: 14, tile: 22, other: 10 },
  { year: "2020", carpet: 36, lvt: 18, hardwood: 14, tile: 22, other: 10 },
  { year: "2021", carpet: 34, lvt: 21, hardwood: 13, tile: 22, other: 10 },
  { year: "2022", carpet: 32, lvt: 24, hardwood: 13, tile: 21, other: 10 },
  { year: "2023", carpet: 30, lvt: 27, hardwood: 12, tile: 21, other: 10 },
  { year: "2024", carpet: 28, lvt: 30, hardwood: 12, tile: 20, other: 10 },
  { year: "2025E", carpet: 26, lvt: 33, hardwood: 12, tile: 20, other: 9 },
];

const porterRadarData = [
  { force: "Industry Rivalry", score: 82, benchmark: 60 },
  { force: "New Entrants", score: 38, benchmark: 55 },
  { force: "Supplier Power", score: 64, benchmark: 50 },
  { force: "Buyer Power", score: 58, benchmark: 50 },
  { force: "Substitutes", score: 72, benchmark: 55 },
];

const scenarioData = {
  base: [
    { year: "2024A", revenue: 6230, margin: 11.8, ebitda: 735 },
    { year: "2025E", revenue: 6540, margin: 12.2, ebitda: 798 },
    { year: "2026E", revenue: 6920, margin: 12.6, ebitda: 872 },
    { year: "2027E", revenue: 7350, margin: 13.0, ebitda: 956 },
  ],
  mild: [
    { year: "2024A", revenue: 6230, margin: 11.8, ebitda: 735 },
    { year: "2025E", revenue: 6180, margin: 11.2, ebitda: 692 },
    { year: "2026E", revenue: 6090, margin: 10.8, ebitda: 658 },
    { year: "2027E", revenue: 6350, margin: 11.4, ebitda: 724 },
  ],
  severe: [
    { year: "2024A", revenue: 6230, margin: 11.8, ebitda: 735 },
    { year: "2025E", revenue: 5790, margin: 9.8, ebitda: 567 },
    { year: "2026E", revenue: 5420, margin: 8.4, ebitda: 455 },
    { year: "2027E", revenue: 5780, margin: 9.6, ebitda: 555 },
  ],
  expansion: [
    { year: "2024A", revenue: 6230, margin: 11.8, ebitda: 735 },
    { year: "2025E", revenue: 6810, margin: 13.0, ebitda: 885 },
    { year: "2026E", revenue: 7520, margin: 13.8, ebitda: 1038 },
    { year: "2027E", revenue: 8180, margin: 14.5, ebitda: 1186 },
  ],
};

const assumptions = [
  { id: 1, assumption: "Housing starts normalize ≥1.8M by 2027", metric: "Housing Starts (k units)", current: "1,590", threshold: ">1,800", value: 1590, target: 1800, status: "yellow", trend: "+2.0% QoQ", category: "Residential", weight: "HIGH" },
  { id: 2, assumption: "Fed funds rate drops below 4.0% by Q4 2025", metric: "Fed Funds Rate", current: "4.50%", threshold: "<4.0%", value: 4.5, target: 4.0, status: "yellow", trend: "–0.75% YTD", category: "Macro", weight: "HIGH" },
  { id: 3, assumption: "Resin/polymer inflation stays below +5% YoY", metric: "Resin Price Index YoY", current: "+3.2%", threshold: "<5.0%", value: 3.2, target: 5.0, status: "green", trend: "Stable", category: "Input Costs", weight: "HIGH" },
  { id: 4, assumption: "LVT market share growth offset by vol. gains", metric: "Shaw LVT Revenue Share", current: "18%", threshold: ">22%", value: 18, target: 22, status: "red", trend: "+1.5% YTD", category: "Product Mix", weight: "MEDIUM" },
  { id: 5, assumption: "Commercial segment rebounds 3%+ in 2025", metric: "Commercial Rev YoY", current: "+1.2%", threshold: ">3.0%", value: 1.2, target: 3.0, status: "yellow", trend: "Improving", category: "Commercial", weight: "MEDIUM" },
  { id: 6, assumption: "Freight costs remain within 2022 baseline", metric: "Freight Cost Index", current: "94", threshold: "<110", value: 94, target: 110, status: "green", trend: "–3% QoQ", category: "Logistics", weight: "MEDIUM" },
  { id: 7, assumption: "No major tariff escalation on Asian flooring imports", metric: "Import Penetration %", current: "22%", threshold: "<30%", value: 22, target: 30, status: "yellow", trend: "+0.8% YoY", category: "Trade", weight: "HIGH" },
  { id: 8, assumption: "SPC launch captures ≥5% category share by 2026", metric: "SPC Market Share Est.", current: "1.8%", threshold: ">5.0%", value: 1.8, target: 5.0, status: "red", trend: "Early stage", category: "Product Mix", weight: "HIGH" },
];

const swotItems = {
  S: [
    { title: "#1 Market Position", evidence: "~26.8% share of U.S. flooring manufacturing revenue", indicator: "Market share trend", risk: 15, tier: "HIGH" },
    { title: "Berkshire Capital Access", evidence: "Unlimited long-term capital for M&A and capex cycles", indicator: "BRK credit rating", risk: 5, tier: "HIGH" },
    { title: "Vertical Integration", evidence: "Controls manufacturing → distribution → install ecosystem", indicator: "COGS / revenue ratio", risk: 20, tier: "HIGH" },
    { title: "Brand Portfolio Depth", evidence: "9 brands across residential, commercial, and turf", indicator: "Brand revenue diversification", risk: 25, tier: "MED" },
    { title: "Sustainability Credentials", evidence: "Cradle-to-cradle certified, EcoWorx backing leadership", indicator: "ESG regulation pipeline", risk: 15, tier: "MED" },
  ],
  W: [
    { title: "Housing Cycle Exposure", evidence: "Residential revenue corr. to housing starts: r = 0.87", indicator: "Housing starts YoY", risk: 70, tier: "HIGH" },
    { title: "Carpet Mix Transition", evidence: "CEO-acknowledged high cost of nylon → polyester shift", indicator: "Carpet COGS / revenue", risk: 55, tier: "HIGH" },
    { title: "Private Co. Opacity", evidence: "No public financials — limits benchmarking and M&A signaling", indicator: "N/A", risk: 30, tier: "LOW" },
    { title: "US Revenue Concentration", evidence: "Est. >80% of revenue from U.S. market", indicator: "International revenue %", risk: 50, tier: "MED" },
  ],
  O: [
    { title: "SPC/LVT Market Growth", evidence: "Global SPC market growing at 13.5% CAGR to $28.4B by 2034", indicator: "SPC shipment index", risk: 20, tier: "HIGH" },
    { title: "2025 Housing Recovery", evidence: "Rate cuts + post-election stability → rebound in starts", indicator: "Mortgage application index", risk: 35, tier: "HIGH" },
    { title: "AI Manufacturing Optimization", evidence: "AI-driven quality control could reduce waste 8–12%", indicator: "AI adoption index in mfg", risk: 25, tier: "MED" },
    { title: "Reshoring Tailwinds", evidence: "Tariff escalation increases domestic mfg. competitiveness", indicator: "Import penetration trend", risk: 30, tier: "MED" },
    { title: "Commercial Recovery", evidence: "Nonresidential construction backlog +4.2% YoY", indicator: "ABI (Architecture Billings Index)", risk: 40, tier: "MED" },
  ],
  T: [
    { title: "Prolonged High Rates", evidence: "Each 100bps increase suppresses housing starts ~80k units", indicator: "Fed funds rate", risk: 60, tier: "HIGH" },
    { title: "Asian Import Competition", evidence: "Vietnam/China LVT at 20–35% price discount to domestic", indicator: "Trade policy / tariff watch", risk: 65, tier: "HIGH" },
    { title: "Hard Surface Share Shift", evidence: "Carpet share -10pp since 2019; accelerating trend", indicator: "Category ship. share quarterly", risk: 70, tier: "HIGH" },
    { title: "Raw Material Spike", evidence: "20% resin price increase compresses EBITDA ~180bps", indicator: "Petroleum / resin futures", risk: 45, tier: "MED" },
  ],
};

const factBase = [
  { source: "FRED", series: "Housing Starts (HOUST)", value: "1,590k", change: "+2.0%", period: "Q4 2024", status: "green", category: "Residential" },
  { source: "FRED", series: "30-Yr Mortgage Rate", value: "6.84%", change: "–0.42%", period: "Mar 2025", status: "yellow", category: "Macro" },
  { source: "FRED", series: "Fed Funds Rate", value: "4.50%", change: "–0.75%", period: "Mar 2025", status: "yellow", category: "Macro" },
  { source: "Census", series: "Nonresidential Construction Spend", value: "$958B (ann.)", change: "+1.2%", period: "Q4 2024", status: "yellow", category: "Commercial" },
  { source: "Census", series: "Residential Remodeling (LIRA)", value: "$447B", change: "–1.8%", period: "Q4 2024", status: "yellow", category: "Residential" },
  { source: "BLS", series: "CPI: Floor Covering", value: "+3.1% YoY", change: "+0.4% QoQ", period: "Jan 2025", status: "yellow", category: "Input Costs" },
  { source: "BLS", series: "PPI: Plastic Resins", value: "+3.2% YoY", change: "+0.8% QoQ", period: "Feb 2025", status: "green", category: "Input Costs" },
  { source: "BLS", series: "PPI: Freight Transportation", value: "94.1", change: "–3.0%", period: "Jan 2025", status: "green", category: "Logistics" },
  { source: "FRED", series: "Consumer Confidence Index", value: "105.3", change: "+2.1", period: "Feb 2025", status: "green", category: "Macro" },
  { source: "FRED", series: "PCE Disposable Income", value: "+2.4% YoY", change: "+0.2%", period: "Jan 2025", status: "green", category: "Macro" },
  { source: "Census", series: "Import Penetration (HS 5703)", value: "21.8%", change: "+0.8%", period: "2024", status: "yellow", category: "Trade" },
  { source: "Public", series: "Mohawk Industries Stock YTD", value: "$111.2", change: "–4.2%", period: "Mar 2025", status: "yellow", category: "Competitive" },
];

// ─── COMPONENTS ───────────────────────────────────────────────────────────────
const Badge = ({ children, color = C.muted }) => (
  <span style={{
    display: "inline-block", padding: "2px 8px", borderRadius: 2,
    fontSize: 10, letterSpacing: "0.15em", fontFamily: FONT.body,
    background: color + "22", color: color, border: `1px solid ${color}44`,
    textTransform: "uppercase", whiteSpace: "nowrap",
  }}>{children}</span>
);

const StatusDot = ({ status }) => {
  const map = { green: C.green, yellow: C.amber, red: C.red };
  return <span style={{ display: "inline-block", width: 8, height: 8, borderRadius: "50%", background: map[status] || C.muted, flexShrink: 0 }} />;
};

const KPICard = ({ label, value, delta, unit = "", status = "neutral", sub = "" }) => {
  const statusColor = status === "up" ? C.green : status === "down" ? C.red : C.muted;
  return (
    <div style={{ background: C.panel, border: `1px solid ${C.border}`, borderRadius: 4, padding: "18px 20px" }}>
      <div style={{ fontSize: 10, color: C.muted, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 6 }}>{label}</div>
      <div style={{ fontSize: 26, fontFamily: FONT.heading, color: C.text, lineHeight: 1 }}>{value}<span style={{ fontSize: 13, color: C.muted, marginLeft: 4 }}>{unit}</span></div>
      {delta && <div style={{ fontSize: 12, color: statusColor, fontFamily: FONT.body, marginTop: 4 }}>{delta}</div>}
      {sub && <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body, marginTop: 4, borderTop: `1px solid ${C.border}`, paddingTop: 6 }}>{sub}</div>}
    </div>
  );
};

const SectionHeader = ({ title, subtitle, badge }) => (
  <div style={{ marginBottom: 24 }}>
    <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 6 }}>
      <h2 style={{ margin: 0, fontSize: 20, fontFamily: FONT.heading, fontWeight: "normal", color: C.text }}>{title}</h2>
      {badge && <Badge color={C.accent}>{badge}</Badge>}
    </div>
    {subtitle && <p style={{ margin: 0, fontSize: 13, color: C.muted, fontFamily: FONT.body, lineHeight: 1.6 }}>{subtitle}</p>}
    <div style={{ height: 1, background: `linear-gradient(90deg, ${C.accent}55, transparent)`, marginTop: 12 }} />
  </div>
);

const Panel = ({ children, style = {} }) => (
  <div style={{ background: C.panel, border: `1px solid ${C.border}`, borderRadius: 4, padding: "24px", ...style }}>{children}</div>
);

const CustomTooltip = ({ active, payload, label }) => {
  if (!active || !payload?.length) return null;
  return (
    <div style={{ background: "#0e1520", border: `1px solid ${C.border}`, borderRadius: 3, padding: "10px 14px", fontFamily: FONT.body, fontSize: 12 }}>
      <div style={{ color: C.accent, marginBottom: 6, fontWeight: "bold" }}>{label}</div>
      {payload.map((p, i) => (
        <div key={i} style={{ color: p.color || C.text, marginBottom: 2 }}>
          {p.name}: <strong>{typeof p.value === "number" ? p.value.toLocaleString() : p.value}</strong>
        </div>
      ))}
    </div>
  );
};

// ─── MODULES ──────────────────────────────────────────────────────────────────
function ExecSummary() {
  const latest = macroTimeSeries[macroTimeSeries.length - 1];
  const prev = macroTimeSeries[macroTimeSeries.length - 5];

  return (
    <div>
      <SectionHeader
        title="Executive Summary Dashboard"
        subtitle="What the CEO needs in 10 minutes. Synthesizing macro environment, competitive position, and strategic momentum."
        badge="Live Refresh: Weekly"
      />

      {/* KPI Row */}
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))", gap: 12, marginBottom: 24 }}>
        <KPICard label="Shaw Rev. Proxy (Est.)" value="$6.23B" delta="↑ +1.5% YoY" status="up" sub="Q4 2024 estimate" />
        <KPICard label="Housing Starts" value="1,590" unit="k" delta="↑ +2.0% QoQ" status="up" sub="FRED HOUST, Q4'24" />
        <KPICard label="Fed Funds Rate" value="4.50" unit="%" delta="↓ –0.75% YTD" status="up" sub="FRED, Mar 2025" />
        <KPICard label="Resin Price Index" value="+3.2" unit="%" delta="Within tolerance" status="up" sub="BLS PPI, Feb 2025" />
        <KPICard label="Freight Cost Index" value="94.1" delta="↓ –3.0% QoQ" status="up" sub="BLS, Jan 2025" />
        <KPICard label="Mkt. Share Est." value="~26.8" unit="%" delta="Stable" status="neutral" sub="Modeled proxy" />
      </div>

      {/* Revenue + Housing Correlation */}
      <div style={{ display: "grid", gridTemplateColumns: "1.5fr 1fr", gap: 16, marginBottom: 16 }}>
        <Panel>
          <div style={{ fontSize: 12, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase", marginBottom: 14 }}>Revenue Proxy vs. Housing Starts — Cyclicality Correlation (r = 0.87)</div>
          <ResponsiveContainer width="100%" height={240}>
            <LineChart data={macroTimeSeries} margin={{ top: 5, right: 10, bottom: 0, left: 0 }}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} />
              <XAxis dataKey="q" tick={{ fill: C.muted, fontSize: 10 }} />
              <YAxis yAxisId="l" tick={{ fill: C.muted, fontSize: 10 }} domain={[5400, 7000]} />
              <YAxis yAxisId="r" orientation="right" tick={{ fill: C.muted, fontSize: 10 }} domain={[1200, 1900]} />
              <Tooltip content={<CustomTooltip />} />
              <Legend wrapperStyle={{ fontSize: 11, color: C.muted }} />
              <Line yAxisId="l" type="monotone" dataKey="shawRevProxy" stroke={C.accent} strokeWidth={2.5} dot={false} name="Shaw Rev ($M est.)" />
              <Line yAxisId="r" type="monotone" dataKey="housingStarts" stroke={C.blue} strokeWidth={1.5} strokeDasharray="5 3" dot={false} name="Housing Starts (k)" />
            </LineChart>
          </ResponsiveContainer>
          <div style={{ marginTop: 10, padding: "10px 14px", background: C.accentDim, border: `1px solid ${C.accentBorder}`, borderRadius: 3, fontSize: 12, color: C.text, fontFamily: FONT.body }}>
            <strong style={{ color: C.accent }}>Insight:</strong> Revenue tracks housing starts with ~1 quarter lag. The Q3'22–Q1'23 housing decline explains the revenue compression. Recovery since Q4'23 is gaining momentum.
          </div>
        </Panel>

        <Panel>
          <div style={{ fontSize: 12, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase", marginBottom: 14 }}>Input Cost & Rate Environment</div>
          <ResponsiveContainer width="100%" height={240}>
            <LineChart data={macroTimeSeries} margin={{ top: 5, right: 10, bottom: 0, left: 0 }}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} />
              <XAxis dataKey="q" tick={{ fill: C.muted, fontSize: 10 }} />
              <YAxis tick={{ fill: C.muted, fontSize: 10 }} />
              <Tooltip content={<CustomTooltip />} />
              <Legend wrapperStyle={{ fontSize: 11, color: C.muted }} />
              <Line type="monotone" dataKey="resinIndex" stroke={C.red} strokeWidth={2} dot={false} name="Resin Index" />
              <Line type="monotone" dataKey="freight" stroke={C.amber} strokeWidth={2} dot={false} name="Freight Index" />
              <Line type="monotone" dataKey="fed" stroke={C.blue} strokeWidth={1.5} strokeDasharray="4 2" dot={false} name="Fed Rate (%)" />
            </LineChart>
          </ResponsiveContainer>
          <div style={{ marginTop: 10, padding: "10px 14px", background: C.greenDim, border: `1px solid ${C.green}33`, borderRadius: 3, fontSize: 12, color: C.text, fontFamily: FONT.body }}>
            <strong style={{ color: C.green }}>Positive Signal:</strong> Resin and freight costs peaked in 2021–22 and are now normalizing. Rate cuts underway. Input cost tailwind is real in 2025.
          </div>
        </Panel>
      </div>

      {/* Competitor Margin Benchmarking */}
      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16, marginBottom: 16 }}>
        <Panel>
          <div style={{ fontSize: 12, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase", marginBottom: 14 }}>Competitive Gross Margin Benchmarking (2024)</div>
          <ResponsiveContainer width="100%" height={200}>
            <BarChart data={competitorData} layout="vertical" margin={{ left: 20, right: 20, top: 5, bottom: 5 }}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} />
              <XAxis type="number" tick={{ fill: C.muted, fontSize: 10 }} unit="%" />
              <YAxis dataKey="company" type="category" tick={{ fill: C.muted, fontSize: 10 }} width={80} />
              <Tooltip content={<CustomTooltip />} />
              <Bar dataKey="margin" name="Gross Margin %" radius={[0, 2, 2, 0]}>
                {competitorData.map((d, i) => <Cell key={i} fill={d.color} />)}
              </Bar>
            </BarChart>
          </ResponsiveContainer>
          <div style={{ marginTop: 8, fontSize: 11, color: C.muted, fontFamily: FONT.body }}>Shaw estimated margin leads Mohawk (+4.6pp) — reflecting vertical integration advantage.</div>
        </Panel>

        <Panel>
          <div style={{ fontSize: 12, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase", marginBottom: 14 }}>Floor Category Share Shift — Carpet Erosion vs LVT Growth</div>
          <ResponsiveContainer width="100%" height={200}>
            <AreaChart data={floorCatShare} margin={{ top: 5, right: 10, bottom: 0, left: 0 }}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} />
              <XAxis dataKey="year" tick={{ fill: C.muted, fontSize: 10 }} />
              <YAxis tick={{ fill: C.muted, fontSize: 10 }} unit="%" />
              <Tooltip content={<CustomTooltip />} />
              <Area type="monotone" dataKey="lvt" stackId="1" stroke={C.blue} fill={C.blueDim} name="LVT/SPC" />
              <Area type="monotone" dataKey="hardwood" stackId="1" stroke={C.green} fill={C.greenDim} name="Hardwood" />
              <Area type="monotone" dataKey="tile" stackId="1" stroke="#a070c0" fill="rgba(160,112,192,0.12)" name="Tile" />
              <Area type="monotone" dataKey="carpet" stackId="1" stroke={C.amber} fill={C.amberDim} name="Carpet" />
            </AreaChart>
          </ResponsiveContainer>
          <div style={{ marginTop: 8, fontSize: 11, color: C.muted, fontFamily: FONT.body }}>Carpet share –12pp since 2019. LVT is the structural winner. Shaw SPC launch (Feb 2025) is a must-win bet.</div>
        </Panel>
      </div>

      {/* Strategic Narrative */}
      <Panel style={{ borderLeft: `3px solid ${C.accent}` }}>
        <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.2em", textTransform: "uppercase", marginBottom: 12 }}>Strategic Narrative — Q1 2025</div>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr 1fr", gap: 16 }}>
          {[
            { label: "What Changed?", text: "Housing recovery is strengthening. Fed rate cuts are underway. Resin and freight costs are normalizing after 2021–22 spike." },
            { label: "Why It Matters", text: "Each 100k increase in housing starts adds ~$350M in total flooring demand. Shaw captures ~27% — that's ~$95M incremental revenue." },
            { label: "What It Means", text: "2025 represents the first full-cycle tailwind since 2022. Margin expansion is achievable if SPC mix shift and commercial recovery materialize." },
            { label: "What to Do", text: "Accelerate SPC ramp, defend carpet margins through mix optimization, deepen commercial relationships ahead of recovery cycle." },
          ].map(({ label, text }) => (
            <div key={label}>
              <div style={{ fontSize: 10, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 6 }}>{label}</div>
              <div style={{ fontSize: 13, color: C.text, fontFamily: FONT.body, lineHeight: 1.65 }}>{text}</div>
            </div>
          ))}
        </div>
      </Panel>
    </div>
  );
}

function PorterForces() {
  const [activeForce, setActiveForce] = useState("rivalry");

  const forces = {
    rivalry: {
      label: "Industry Rivalry",
      score: 82,
      level: "HIGH",
      color: C.red,
      headline: "Intense rivalry with Mohawk; margin compression pressure is structural.",
      hypothesis: "Flooring is a mature oligopoly with 2–3 dominant players. Pricing power is limited and market share gains require significant investment.",
      evidence: [
        "Mohawk Industries ($10.4B rev) and Shaw (~$6.2B) control est. 60%+ of U.S. flooring market",
        "HHI approximation: ~2,100 (moderately concentrated; rivalry still intense)",
        "Mohawk gross margin compressed from 30.2% (2021) to 7.2% (2024) — industry-wide pressure",
        "Private equity entrants (Engineered Floors, Mannington) are price-competitive",
        "Product innovation cycle time shrinking — design differentiation window is ~18 months",
      ],
      insight: "Rivalry is the dominant force. Shaw's vertical integration provides cost insulation but not immunity. SPC category is the next battleground and Mohawk has equal scale.",
      action: "Invest in brand loyalty, installer programs, and digital platform lock-in. Compete on service, sustainability credentials, and product innovation, not price.",
      data: competitorData,
    },
    entrants: {
      label: "Threat of New Entrants",
      score: 38,
      level: "LOW",
      color: C.green,
      headline: "High capex and brand requirements create durable barriers. Threat is LOW.",
      hypothesis: "Large-scale flooring manufacturing requires $300M+ capex investment, established distribution, and brand credibility. These barriers are high for traditional entrants.",
      evidence: [
        "New carpet/LVT manufacturing plant requires $250–400M in equipment investment",
        "Shaw and Mohawk have 50+ years of established installer and dealer relationships",
        "Import penetration (HS 5703) from Asia is the real 'new entrant' threat at 21.8% and growing",
        "PE activity: Engineered Floors recapitalized under Shaw in 2022, reducing new independent competition",
        "No greenfield domestic flooring plant opened by a new entrant in 7+ years",
      ],
      insight: "Traditional new entry risk is LOW. The real entrant threat is Asian imports — particularly Vietnamese LVT — which bypass capital barriers via trade.",
      action: "Monitor import tariff policy closely. Lobby for Section 301 extensions. Price domestic SPC competitively enough to neutralize import advantage on quality-sensitive segments.",
      data: null,
    },
    suppliers: {
      label: "Power of Suppliers",
      score: 64,
      level: "MED",
      color: C.amber,
      headline: "Petrochemical dependency creates margin volatility; moderately high supplier power.",
      hypothesis: "Nylon, polyester, and polypropylene are derived from petroleum. Shaw has limited ability to substitute these inputs, and supplier concentration is meaningful.",
      evidence: [
        "Nylon/polyester fiber accounts for est. 35–45% of carpet COGS",
        "Resin index peaked at 140 in Q2'22 — a 47% increase vs. 2020 baseline",
        "Top 3 fiber suppliers (Invista, RadiciGroup, Ascend) control the bulk of nylon supply",
        "China/Vietnam import dependency on LVT core and wear-layer resins is growing",
        "Freight costs doubled during 2021–22 supply chain crisis, amplifying supplier power",
        "Current resin index at 103 — within historical norms but vulnerable to crude oil shocks",
      ],
      insight: "Supplier power is moderate and cyclical. It spikes during commodity supercycles and supply chain crises. Shaw's vertical integration partially mitigates this for carpet backing.",
      action: "Maintain 6-month resin inventory buffer. Diversify fiber sourcing to 3+ suppliers. Lock in freight contracts ahead of demand recovery cycle.",
      data: macroTimeSeries.slice(-8).map(d => ({ q: d.q, resin: d.resinIndex, freight: d.freight })),
    },
    buyers: {
      label: "Power of Buyers",
      score: 58,
      level: "MED",
      color: C.amber,
      headline: "Buyer power varies by channel: HIGH for big builders, LOW for retail.",
      hypothesis: "Shaw sells through multiple channels — national builders, commercial contractors, retail dealers, and direct. Buyer concentration varies significantly by channel.",
      evidence: [
        "Top 10 national builders (D.R. Horton, Lennar, PulteGroup) collectively build 30%+ of new homes",
        "These builders negotiate flooring contracts centrally — significant pricing leverage",
        "Commercial: large corporate accounts (hospitality, healthcare) use RFP-driven procurement",
        "Retail channel (big box + dealers) is fragmented — buyer power is LOW",
        "Consumer sentiment and credit conditions impact end-demand elasticity",
        "Builder Sentiment Index at 44 (below 50 = pessimistic) — constraining top-line leverage",
      ],
      insight: "The big builder channel represents concentrated buyer power but also volume certainty. The sweet spot is mid-market builders and commercial accounts where service quality matters more than price.",
      action: "Develop long-term preferred vendor agreements with top-10 builders. Invest in commercial account management. Use digital tools to create switching costs in dealer channel.",
      data: null,
    },
    substitutes: {
      label: "Threat of Substitutes",
      score: 72,
      level: "HIGH",
      color: C.red,
      headline: "LVT substitution of carpet is structural and accelerating. Existential for carpet.",
      hypothesis: "LVT/SPC provides carpet's core benefits (softness, warmth, acoustic insulation) with additional advantages: waterproofing, durability, and cleanability.",
      evidence: [
        "Carpet market share: 38% (2019) → 28% (2024) → est. 26% (2025) — structural decline",
        "LVT/SPC market share: 16% (2019) → 30% (2024) — fastest-growing hard surface category",
        "SPC global CAGR: 13.5% through 2034 — vs. carpet market declining in absolute terms",
        "Consumer preference surveys: 68% prefer hard surface for main living areas (2024)",
        "Health/wellness trend: LVT perceived as more sanitary than carpet for allergy sufferers",
        "Pet ownership boom (pandemic era) accelerated hard surface preference permanently",
      ],
      insight: "This is Shaw's most critical strategic challenge. The primary substitute for carpet is Shaw's own LVT/SPC product line — a cannibalization dynamic that management must actively manage.",
      action: "Reposition carpet as a premium bedroom/acoustics product. Aggressively grow LVT/SPC to offset carpet volume declines. Ensure SPC launch reaches 5%+ category share by 2026.",
      data: floorCatShare,
    },
  };

  const activeData = forces[activeForce];

  return (
    <div>
      <SectionHeader
        title="Porter's Five Forces"
        subtitle="Structural industry analysis calibrated to Shaw's competitive environment. Each force scored 0–100 (100 = maximum competitive pressure)."
        badge="Strategic Framework"
      />

      {/* Radar Overview */}
      <div style={{ display: "grid", gridTemplateColumns: "1fr 2fr", gap: 16, marginBottom: 24 }}>
        <Panel>
          <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 14 }}>Force Intensity Map</div>
          <ResponsiveContainer width="100%" height={260}>
            <RadarChart data={porterRadarData}>
              <PolarGrid stroke={C.border} />
              <PolarAngleAxis dataKey="force" tick={{ fill: C.muted, fontSize: 10, fontFamily: FONT.body }} />
              <PolarRadiusAxis angle={30} domain={[0, 100]} tick={{ fill: C.muted, fontSize: 8 }} />
              <Radar name="Shaw" dataKey="score" stroke={C.accent} fill={C.accent} fillOpacity={0.15} />
              <Radar name="Industry Avg" dataKey="benchmark" stroke={C.blue} fill={C.blue} fillOpacity={0.08} strokeDasharray="4 2" />
              <Legend wrapperStyle={{ fontSize: 11, color: C.muted }} />
            </RadarChart>
          </ResponsiveContainer>
        </Panel>

        <Panel>
          <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 14 }}>Select Force for Deep Analysis</div>
          <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 8 }}>
            {Object.entries(forces).map(([key, f]) => (
              <button
                key={key}
                onClick={() => setActiveForce(key)}
                style={{
                  background: activeForce === key ? f.color + "22" : "transparent",
                  border: `1px solid ${activeForce === key ? f.color : C.border}`,
                  borderRadius: 3, padding: "14px 16px", cursor: "pointer", textAlign: "left",
                  transition: "all 0.2s",
                }}
              >
                <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 8 }}>
                  <div style={{ fontSize: 12, color: activeForce === key ? f.color : C.text, fontFamily: FONT.body }}>{f.label}</div>
                  <Badge color={f.color}>{f.level}</Badge>
                </div>
                <div style={{ height: 4, background: C.border, borderRadius: 2 }}>
                  <div style={{ height: "100%", width: `${f.score}%`, background: f.color, borderRadius: 2 }} />
                </div>
                <div style={{ fontSize: 10, color: C.muted, marginTop: 4, fontFamily: FONT.body }}>{f.score}/100</div>
              </button>
            ))}
          </div>
        </Panel>
      </div>

      {/* Deep Dive */}
      <Panel style={{ borderLeft: `3px solid ${activeData.color}` }}>
        <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", marginBottom: 20 }}>
          <div>
            <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 6 }}>
              <h3 style={{ margin: 0, fontSize: 18, fontFamily: FONT.heading, fontWeight: "normal", color: activeData.color }}>{activeData.label}</h3>
              <Badge color={activeData.color}>{activeData.level} PRESSURE</Badge>
              <Badge color={C.muted}>{activeData.score}/100</Badge>
            </div>
            <div style={{ fontSize: 14, color: C.text, fontFamily: FONT.body, fontStyle: "italic" }}>{activeData.headline}</div>
          </div>
        </div>

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr 1fr", gap: 16, marginBottom: 20 }}>
          {[
            { label: "Hypothesis", text: activeData.hypothesis, color: C.blue },
            { label: "Evidence", text: null, items: activeData.evidence, color: C.amber },
            { label: "Insight", text: activeData.insight, color: C.accent },
            { label: "Action Required", text: activeData.action, color: C.green },
          ].map(({ label, text, items, color }) => (
            <div key={label} style={{ background: color + "0e", border: `1px solid ${color}28`, borderRadius: 3, padding: "14px" }}>
              <div style={{ fontSize: 10, color: color, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 10 }}>{label}</div>
              {text && <div style={{ fontSize: 12, color: C.text, fontFamily: FONT.body, lineHeight: 1.7 }}>{text}</div>}
              {items && <ul style={{ margin: 0, padding: "0 0 0 14px" }}>
                {items.map((item, i) => (
                  <li key={i} style={{ fontSize: 12, color: C.text, fontFamily: FONT.body, lineHeight: 1.6, marginBottom: 4 }}>{item}</li>
                ))}
              </ul>}
            </div>
          ))}
        </div>
      </Panel>
    </div>
  );
}

function SWOTEngine() {
  const [activeQ, setActiveQ] = useState("S");
  const [expandedItem, setExpandedItem] = useState(null);

  const quadrants = {
    S: { label: "Strengths", color: C.green, desc: "Internal positive factors — quantified where possible" },
    W: { label: "Weaknesses", color: C.red, desc: "Internal risk factors — with leading indicator signals" },
    O: { label: "Opportunities", color: C.blue, desc: "External upside factors — prioritized by scale" },
    T: { label: "Threats", color: C.amber, desc: "External risk factors — with probability weighting" },
  };

  const items = swotItems[activeQ];
  const q = quadrants[activeQ];

  return (
    <div>
      <SectionHeader
        title="Dynamic SWOT Engine"
        subtitle="Evidence-based SWOT with leading indicators and risk probability scoring for each factor. Not static text — a living strategic framework."
        badge="Evidence-Based"
      />

      {/* Quadrant Selector */}
      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr 1fr", gap: 8, marginBottom: 24 }}>
        {Object.entries(quadrants).map(([key, val]) => (
          <button
            key={key}
            onClick={() => { setActiveQ(key); setExpandedItem(null); }}
            style={{
              background: activeQ === key ? val.color + "1a" : "transparent",
              border: `1px solid ${activeQ === key ? val.color : C.border}`,
              borderRadius: 3, padding: "16px", cursor: "pointer", textAlign: "center",
              transition: "all 0.2s",
            }}
          >
            <div style={{ fontSize: 28, fontFamily: FONT.heading, color: activeQ === key ? val.color : C.muted, marginBottom: 4 }}>{key}</div>
            <div style={{ fontSize: 13, color: activeQ === key ? C.text : C.muted, fontFamily: FONT.body }}>{val.label}</div>
            <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body, marginTop: 4 }}>{swotItems[key].length} factors</div>
          </button>
        ))}
      </div>

      {/* Items */}
      <div style={{ marginBottom: 16 }}>
        <div style={{ fontSize: 12, color: C.muted, fontFamily: FONT.body, marginBottom: 16, fontStyle: "italic" }}>{q.desc}</div>
        <div style={{ display: "grid", gap: 8 }}>
          {items.map((item, i) => {
            const isOpen = expandedItem === i;
            const riskColor = item.risk < 30 ? C.green : item.risk < 60 ? C.amber : C.red;
            return (
              <button
                key={i}
                onClick={() => setExpandedItem(isOpen ? null : i)}
                style={{
                  background: isOpen ? q.color + "0e" : "transparent",
                  border: `1px solid ${isOpen ? q.color + "44" : C.border}`,
                  borderRadius: 3, padding: "16px 18px", cursor: "pointer", textAlign: "left",
                  transition: "all 0.2s",
                }}
              >
                <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center" }}>
                  <div style={{ display: "flex", gap: 14, alignItems: "center" }}>
                    <span style={{ fontSize: 11, color: q.color, fontFamily: FONT.body, minWidth: 20 }}>{String(i + 1).padStart(2, "0")}</span>
                    <span style={{ fontSize: 15, color: isOpen ? C.text : "#9a9488", fontFamily: FONT.heading }}>{item.title}</span>
                    <Badge color={item.tier === "HIGH" ? q.color : C.muted}>{item.tier}</Badge>
                  </div>
                  <div style={{ display: "flex", gap: 12, alignItems: "center" }}>
                    <div style={{ textAlign: "right" }}>
                      <div style={{ fontSize: 10, color: C.muted, fontFamily: FONT.body }}>Risk Probability</div>
                      <div style={{ fontSize: 13, color: riskColor, fontFamily: FONT.body, fontWeight: "bold" }}>{item.risk}%</div>
                    </div>
                    <div style={{ width: 48, height: 4, background: C.border, borderRadius: 2 }}>
                      <div style={{ height: "100%", width: `${item.risk}%`, background: riskColor, borderRadius: 2 }} />
                    </div>
                    <span style={{ color: isOpen ? q.color : C.muted, fontSize: 18, fontFamily: FONT.body, transform: isOpen ? "rotate(45deg)" : "none", display: "inline-block", transition: "transform 0.2s" }}>+</span>
                  </div>
                </div>
                {isOpen && (
                  <div style={{ marginTop: 16, display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 12, borderTop: `1px solid ${C.border}`, paddingTop: 16 }}>
                    <div>
                      <div style={{ fontSize: 10, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.12em", textTransform: "uppercase", marginBottom: 6 }}>Evidence Base</div>
                      <div style={{ fontSize: 12, color: C.text, fontFamily: FONT.body, lineHeight: 1.7 }}>{item.evidence}</div>
                    </div>
                    <div>
                      <div style={{ fontSize: 10, color: C.blue, fontFamily: FONT.body, letterSpacing: "0.12em", textTransform: "uppercase", marginBottom: 6 }}>Leading Indicator</div>
                      <div style={{ fontSize: 12, color: C.text, fontFamily: FONT.body, lineHeight: 1.7 }}>{item.indicator}</div>
                    </div>
                    <div>
                      <div style={{ fontSize: 10, color: riskColor, fontFamily: FONT.body, letterSpacing: "0.12em", textTransform: "uppercase", marginBottom: 6 }}>Risk Assessment</div>
                      <div style={{ height: 8, background: C.border, borderRadius: 4, marginBottom: 8 }}>
                        <div style={{ height: "100%", width: `${item.risk}%`, background: riskColor, borderRadius: 4, transition: "width 0.5s" }} />
                      </div>
                      <div style={{ fontSize: 12, color: riskColor, fontFamily: FONT.body }}>{item.risk}% probability / materiality</div>
                    </div>
                  </div>
                )}
              </button>
            );
          })}
        </div>
      </div>
    </div>
  );
}

function AssumptionTracker() {
  const [filter, setFilter] = useState("ALL");
  const categories = ["ALL", ...new Set(assumptions.map(a => a.category))];
  const filtered = filter === "ALL" ? assumptions : assumptions.filter(a => a.category === filter);

  const counts = { green: assumptions.filter(a => a.status === "green").length, yellow: assumptions.filter(a => a.status === "yellow").length, red: assumptions.filter(a => a.status === "red").length };

  return (
    <div>
      <SectionHeader
        title="Strategy Assumptions Monitor"
        subtitle="Every strategy is a set of assumptions. This monitor tracks whether those assumptions are holding — and flags early when they're breaking. Review cycle: monthly."
        badge="Early Warning System"
      />

      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 12, marginBottom: 24 }}>
        <div style={{ background: C.greenDim, border: `1px solid ${C.green}33`, borderRadius: 4, padding: "16px 20px", display: "flex", alignItems: "center", gap: 16 }}>
          <div style={{ fontSize: 36, color: C.green, fontFamily: FONT.heading }}>{counts.green}</div>
          <div>
            <div style={{ fontSize: 12, color: C.green, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase" }}>On Track</div>
            <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body }}>Assumptions holding</div>
          </div>
        </div>
        <div style={{ background: C.amberDim, border: `1px solid ${C.amber}33`, borderRadius: 4, padding: "16px 20px", display: "flex", alignItems: "center", gap: 16 }}>
          <div style={{ fontSize: 36, color: C.amber, fontFamily: FONT.heading }}>{counts.yellow}</div>
          <div>
            <div style={{ fontSize: 12, color: C.amber, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase" }}>Watch</div>
            <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body }}>Approaching threshold</div>
          </div>
        </div>
        <div style={{ background: C.redDim, border: `1px solid ${C.red}33`, borderRadius: 4, padding: "16px 20px", display: "flex", alignItems: "center", gap: 16 }}>
          <div style={{ fontSize: 36, color: C.red, fontFamily: FONT.heading }}>{counts.red}</div>
          <div>
            <div style={{ fontSize: 12, color: C.red, fontFamily: FONT.body, letterSpacing: "0.1em", textTransform: "uppercase" }}>At Risk</div>
            <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body }}>Assumption broken or failing</div>
          </div>
        </div>
      </div>

      {/* Category filter */}
      <div style={{ display: "flex", gap: 8, marginBottom: 16, flexWrap: "wrap" }}>
        {categories.map(cat => (
          <button key={cat} onClick={() => setFilter(cat)} style={{
            background: filter === cat ? C.accentDim : "transparent",
            border: `1px solid ${filter === cat ? C.accent : C.border}`,
            borderRadius: 2, padding: "5px 14px", cursor: "pointer",
            fontSize: 11, color: filter === cat ? C.accent : C.muted, fontFamily: FONT.body,
            letterSpacing: "0.1em", textTransform: "uppercase",
          }}>{cat}</button>
        ))}
      </div>

      {/* Table */}
      <Panel style={{ padding: 0 }}>
        <table style={{ width: "100%", borderCollapse: "collapse" }}>
          <thead>
            <tr style={{ borderBottom: `1px solid ${C.border}` }}>
              {["Status", "Assumption", "Category", "Priority", "Metric", "Current Value", "Threshold", "Trend"].map(h => (
                <th key={h} style={{ padding: "12px 16px", textAlign: "left", fontSize: 10, color: C.muted, fontFamily: FONT.body, letterSpacing: "0.12em", textTransform: "uppercase", background: C.panel }}>{h}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {filtered.map((a, i) => (
              <tr key={a.id} style={{ borderBottom: `1px solid ${C.border}`, background: i % 2 === 0 ? "transparent" : "rgba(255,255,255,0.01)" }}>
                <td style={{ padding: "14px 16px" }}>
                  <StatusDot status={a.status} />
                </td>
                <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body, maxWidth: 280 }}>{a.assumption}</td>
                <td style={{ padding: "14px 16px" }}><Badge color={C.muted}>{a.category}</Badge></td>
                <td style={{ padding: "14px 16px" }}>
                  <Badge color={a.weight === "HIGH" ? C.red : a.weight === "MEDIUM" ? C.amber : C.muted}>{a.weight}</Badge>
                </td>
                <td style={{ padding: "14px 16px", fontSize: 12, color: C.muted, fontFamily: FONT.body }}>{a.metric}</td>
                <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body, fontWeight: "bold" }}>{a.current}</td>
                <td style={{ padding: "14px 16px", fontSize: 12, color: C.muted, fontFamily: FONT.body }}>{a.threshold}</td>
                <td style={{ padding: "14px 16px", fontSize: 12, fontFamily: FONT.body, color: a.trend.includes("+") ? C.green : a.trend.includes("–") ? C.amber : C.muted }}>{a.trend}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </Panel>

      <div style={{ marginTop: 16, padding: "14px 18px", background: C.accentDim, border: `1px solid ${C.accentBorder}`, borderRadius: 3, fontSize: 12, color: C.text, fontFamily: FONT.body, lineHeight: 1.7 }}>
        <strong style={{ color: C.accent }}>Executive Directive:</strong> The two RED items — LVT/SPC market share and SPC launch ramp — require immediate attention. The Shaw SPC launch (February 2025) must reach 5%+ category share by 2026 or the product mix headwind from carpet erosion will outpace top-line recovery. Assign dedicated SPC commercialization resources.
      </div>
    </div>
  );
}

function ScenarioModule() {
  const [scenario, setScenario] = useState("base");
  const scenarios = {
    base: { label: "Base Case", color: C.accent, desc: "Housing starts reach 1.8M by 2027. Rates fall to 3.75%. SPC gains traction. Carpet manages orderly transition." },
    expansion: { label: "Expansion", color: C.green, desc: "Aggressive rate cuts, housing boom, SPC takes 8%+ share. M&A opportunity materializes. Margin expansion accelerates." },
    mild: { label: "Mild Downturn", color: C.amber, desc: "Rates stay elevated through 2026. Housing starts plateau at 1.5M. SPC launch slow. Margin pressure from input costs." },
    severe: { label: "Severe Downturn", color: C.red, desc: "Housing crash to sub-1.2M. Rate spike from fiscal crisis. Resin costs surge. Commercial construction freezes. Carpet accelerates decline." },
  };

  const data = scenarioData[scenario];
  const s = scenarios[scenario];

  return (
    <div>
      <SectionHeader
        title="Scenario & Sensitivity Modeling"
        subtitle="3–5 year revenue, margin, and EBITDA modeling across four macro scenarios. Each scenario is calibrated to real input variables."
        badge="3–5 Year Horizon"
      />

      {/* Scenario Selector */}
      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr 1fr", gap: 8, marginBottom: 24 }}>
        {Object.entries(scenarios).map(([key, s]) => (
          <button key={key} onClick={() => setScenario(key)} style={{
            background: scenario === key ? s.color + "1a" : "transparent",
            border: `1px solid ${scenario === key ? s.color : C.border}`,
            borderRadius: 3, padding: "14px 16px", cursor: "pointer", textAlign: "left", transition: "all 0.2s",
          }}>
            <div style={{ fontSize: 13, color: scenario === key ? s.color : C.muted, fontFamily: FONT.body, marginBottom: 6 }}>{s.label}</div>
            <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body, lineHeight: 1.5 }}>{s.desc.substring(0, 60)}...</div>
          </button>
        ))}
      </div>

      <div style={{ marginBottom: 20, padding: "14px 18px", background: s.color + "0e", border: `1px solid ${s.color}33`, borderRadius: 3 }}>
        <div style={{ fontSize: 11, color: s.color, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 6 }}>{s.label} — Scenario Description</div>
        <div style={{ fontSize: 13, color: C.text, fontFamily: FONT.body, lineHeight: 1.7 }}>{s.desc}</div>
      </div>

      <div style={{ display: "grid", gridTemplateColumns: "1.5fr 1fr", gap: 16, marginBottom: 16 }}>
        <Panel>
          <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 14 }}>Revenue Trajectory ($M)</div>
          <ResponsiveContainer width="100%" height={240}>
            <AreaChart data={data}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} />
              <XAxis dataKey="year" tick={{ fill: C.muted, fontSize: 10 }} />
              <YAxis tick={{ fill: C.muted, fontSize: 10 }} domain={["auto", "auto"]} />
              <Tooltip content={<CustomTooltip />} />
              <Area type="monotone" dataKey="revenue" stroke={s.color} fill={s.color + "18"} strokeWidth={2.5} name="Revenue ($M)" />
              <ReferenceLine x="2024A" stroke={C.muted} strokeDasharray="4 2" label={{ value: "Actual", fill: C.muted, fontSize: 10 }} />
            </AreaChart>
          </ResponsiveContainer>
        </Panel>

        <Panel>
          <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 14 }}>Margin & EBITDA ($M)</div>
          <ResponsiveContainer width="100%" height={240}>
            <BarChart data={data}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} />
              <XAxis dataKey="year" tick={{ fill: C.muted, fontSize: 10 }} />
              <YAxis tick={{ fill: C.muted, fontSize: 10 }} />
              <Tooltip content={<CustomTooltip />} />
              <Bar dataKey="ebitda" name="EBITDA ($M)" fill={s.color} fillOpacity={0.7} radius={[2, 2, 0, 0]} />
            </BarChart>
          </ResponsiveContainer>
        </Panel>
      </div>

      {/* Scenario Table */}
      <Panel style={{ padding: 0 }}>
        <table style={{ width: "100%", borderCollapse: "collapse" }}>
          <thead>
            <tr style={{ borderBottom: `1px solid ${C.border}` }}>
              {["Year", "Revenue ($M)", "YoY Growth", "Gross Margin (%)", "EBITDA ($M)", "EBITDA Margin"].map(h => (
                <th key={h} style={{ padding: "12px 16px", textAlign: "left", fontSize: 10, color: C.muted, fontFamily: FONT.body, letterSpacing: "0.12em", textTransform: "uppercase", background: C.panel }}>{h}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {data.map((row, i) => {
              const prev = i > 0 ? data[i - 1] : null;
              const growth = prev ? (((row.revenue - prev.revenue) / prev.revenue) * 100).toFixed(1) : "—";
              const ebitdaMargin = ((row.ebitda / row.revenue) * 100).toFixed(1);
              const growthColor = !prev ? C.muted : parseFloat(growth) > 0 ? C.green : C.red;
              return (
                <tr key={i} style={{ borderBottom: `1px solid ${C.border}` }}>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: row.year.includes("A") ? C.muted : C.text, fontFamily: FONT.body }}>
                    {row.year} {row.year.includes("A") && <span style={{ fontSize: 10, color: C.muted }}>(Actual)</span>}
                  </td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body, fontWeight: "bold" }}>${row.revenue.toLocaleString()}</td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: growthColor, fontFamily: FONT.body }}>{prev ? (parseFloat(growth) > 0 ? "+" : "") + growth + "%" : "—"}</td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body }}>{row.margin}%</td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body }}>${row.ebitda}</td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body }}>{ebitdaMargin}%</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </Panel>

      {/* Leading Indicators */}
      <div style={{ marginTop: 20 }}>
        <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 12 }}>Leading Indicators — Variables That Move 6–12 Months Before Revenue Shifts</div>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 8 }}>
          {[
            { indicator: "Mortgage Application Index", lead: "9–12 months", current: "Trending Up ↑", color: C.green },
            { indicator: "Housing Permit Issuances", lead: "6–9 months", current: "Stable / Recovery ↑", color: C.green },
            { indicator: "Architecture Billings Index (ABI)", lead: "9–12 months", current: "Below 50 (Contracting)", color: C.amber },
            { indicator: "Petroleum / Resin Futures", lead: "3–6 months", current: "Neutral ↔", color: C.green },
            { indicator: "Builder Sentiment Index (NAHB)", lead: "3–6 months", current: "44 — Cautious", color: C.amber },
            { indicator: "Retail Floor Covering SSS", lead: "2–4 months", current: "Recovering +1.8%", color: C.green },
          ].map(({ indicator, lead, current, color }) => (
            <div key={indicator} style={{ background: "transparent", border: `1px solid ${C.border}`, borderRadius: 3, padding: "12px 14px" }}>
              <div style={{ fontSize: 12, color: C.text, fontFamily: FONT.body, marginBottom: 4 }}>{indicator}</div>
              <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body, marginBottom: 6 }}>Lead time: {lead}</div>
              <div style={{ fontSize: 12, color: color, fontFamily: FONT.body }}>{current}</div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}

function FactBaseModule() {
  const [catFilter, setCatFilter] = useState("ALL");
  const categories = ["ALL", ...new Set(factBase.map(f => f.category))];
  const filtered = catFilter === "ALL" ? factBase : factBase.filter(f => f.category === catFilter);

  return (
    <div>
      <SectionHeader
        title="Live Fact Base"
        subtitle="Continuous environmental scan from FRED, BLS, Census Bureau, and public competitor data. This is the foundation every strategic assumption is built on."
        badge="Auto-Refresh: Weekly"
      />

      {/* Source Key */}
      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 12, marginBottom: 24 }}>
        {[
          { source: "FRED", desc: "Federal Reserve Bank of St. Louis — Monetary policy, housing, and consumer data", color: C.accent, series: "6 series active" },
          { source: "BLS", desc: "Bureau of Labor Statistics — PPI, CPI, labor market, and freight indexes", color: C.blue, series: "3 series active" },
          { source: "Census", desc: "U.S. Census Bureau — Construction spending, trade, and demographic data", color: C.green, series: "3 series active" },
        ].map(({ source, desc, color, series }) => (
          <div key={source} style={{ background: color + "0a", border: `1px solid ${color}22`, borderRadius: 3, padding: "14px 16px" }}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 6 }}>
              <Badge color={color}>{source}</Badge>
              <div style={{ fontSize: 10, color: C.muted, fontFamily: FONT.body }}>{series}</div>
            </div>
            <div style={{ fontSize: 12, color: C.muted, fontFamily: FONT.body, lineHeight: 1.6 }}>{desc}</div>
          </div>
        ))}
      </div>

      {/* Filter */}
      <div style={{ display: "flex", gap: 8, marginBottom: 16, flexWrap: "wrap" }}>
        {categories.map(cat => (
          <button key={cat} onClick={() => setCatFilter(cat)} style={{
            background: catFilter === cat ? C.accentDim : "transparent",
            border: `1px solid ${catFilter === cat ? C.accent : C.border}`,
            borderRadius: 2, padding: "5px 14px", cursor: "pointer",
            fontSize: 11, color: catFilter === cat ? C.accent : C.muted, fontFamily: FONT.body,
            letterSpacing: "0.1em", textTransform: "uppercase",
          }}>{cat}</button>
        ))}
      </div>

      {/* Data Table */}
      <Panel style={{ padding: 0 }}>
        <table style={{ width: "100%", borderCollapse: "collapse" }}>
          <thead>
            <tr style={{ borderBottom: `1px solid ${C.border}` }}>
              {["Signal", "Source", "Category", "Current Value", "Change", "Period", "Status"].map(h => (
                <th key={h} style={{ padding: "12px 16px", textAlign: "left", fontSize: 10, color: C.muted, fontFamily: FONT.body, letterSpacing: "0.12em", textTransform: "uppercase", background: C.panel }}>{h}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {filtered.map((f, i) => {
              const changeColor = f.change.startsWith("–") || f.change.startsWith("-") ? C.amber : C.green;
              return (
                <tr key={i} style={{ borderBottom: `1px solid ${C.border}`, background: i % 2 === 0 ? "transparent" : "rgba(255,255,255,0.01)" }}>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body }}>{f.series}</td>
                  <td style={{ padding: "14px 16px" }}><Badge color={f.source === "FRED" ? C.accent : f.source === "BLS" ? C.blue : C.green}>{f.source}</Badge></td>
                  <td style={{ padding: "14px 16px" }}><Badge color={C.muted}>{f.category}</Badge></td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: C.text, fontFamily: FONT.body, fontWeight: "bold" }}>{f.value}</td>
                  <td style={{ padding: "14px 16px", fontSize: 13, color: changeColor, fontFamily: FONT.body }}>{f.change}</td>
                  <td style={{ padding: "14px 16px", fontSize: 12, color: C.muted, fontFamily: FONT.body }}>{f.period}</td>
                  <td style={{ padding: "14px 16px" }}><StatusDot status={f.status} /></td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </Panel>

      <div style={{ marginTop: 16, display: "grid", gridTemplateColumns: "1fr 1fr", gap: 12 }}>
        <Panel>
          <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 14 }}>Macro Signal Heatmap</div>
          <div style={{ display: "grid", gap: 6 }}>
            {[
              { label: "Interest Rate Environment", score: 45, color: C.amber, note: "Improving — cuts underway but still restrictive" },
              { label: "Housing Market Momentum", score: 60, color: C.green, note: "Recovering from 2023 trough, sustained growth" },
              { label: "Input Cost Pressure", score: 72, color: C.green, note: "Normalized — within historical range" },
              { label: "Consumer Spending Power", score: 65, color: C.green, note: "PCE income growing, credit conditions stable" },
              { label: "Commercial Construction", score: 40, color: C.amber, note: "ABI below 50 — soft but stabilizing" },
              { label: "Competitive Intensity", score: 28, color: C.red, note: "High rivalry, import pressure escalating" },
            ].map(({ label, score, color, note }) => (
              <div key={label} style={{ display: "grid", gridTemplateColumns: "180px 1fr 40px", gap: 10, alignItems: "center" }}>
                <div style={{ fontSize: 12, color: C.text, fontFamily: FONT.body }}>{label}</div>
                <div style={{ height: 6, background: C.border, borderRadius: 3 }}>
                  <div style={{ height: "100%", width: `${score}%`, background: color, borderRadius: 3 }} />
                </div>
                <div style={{ fontSize: 11, color: color, fontFamily: FONT.body, textAlign: "right" }}>{score}</div>
              </div>
            ))}
          </div>
        </Panel>

        <Panel>
          <div style={{ fontSize: 11, color: C.accent, fontFamily: FONT.body, letterSpacing: "0.15em", textTransform: "uppercase", marginBottom: 14 }}>Competitive Intelligence</div>
          <div style={{ display: "grid", gap: 10 }}>
            {[
              { company: "Mohawk Industries (MHK)", note: "Gross margin under pressure at 7.2% — structural weakness in commoditized segments. CEO cited residential softness in Q3'24.", status: "yellow" },
              { company: "Interface, Inc. (TILE)", note: "Highest margin at 14.1%. Pure commercial modular carpet play. Growing in Asia and healthcare. Not a direct residential competitor.", status: "green" },
              { company: "Tarkett SA", note: "European flooring leader entering U.S. LVT market aggressively. Price competitive. Watch import share.", status: "yellow" },
              { company: "Engineered Floors", note: "Shaw-recapitalized private competitor. Domestic capacity, polyester-focused. Competitive in lower price tiers.", status: "green" },
            ].map(({ company, note, status }) => (
              <div key={company} style={{ display: "flex", gap: 12, padding: "10px 12px", background: "rgba(255,255,255,0.02)", borderRadius: 3, border: `1px solid ${C.border}` }}>
                <StatusDot status={status} />
                <div>
                  <div style={{ fontSize: 12, color: C.text, fontFamily: FONT.body, marginBottom: 3 }}>{company}</div>
                  <div style={{ fontSize: 11, color: C.muted, fontFamily: FONT.body, lineHeight: 1.5 }}>{note}</div>
                </div>
              </div>
            ))}
          </div>
        </Panel>
      </div>
    </div>
  );
}

// ─── LOGIN SCREEN ──────────────────────────────────────────────────────────────
function Login({ onLogin }) {
  const [user, setUser] = useState("");
  const [pass, setPass] = useState("");
  const [err, setErr] = useState("");
  const [loading, setLoading] = useState(false);

  const attempt = () => {
    setLoading(true);
    setTimeout(() => {
      if (user === "shaw" && pass === "ELT2025") {
        onLogin();
      } else {
        setErr("Invalid credentials. Contact IT for access.");
      }
      setLoading(false);
    }, 600);
  };

  return (
    <div style={{
      minHeight: "100vh", background: C.bg, display: "flex", alignItems: "center", justifyContent: "center",
      fontFamily: FONT.body,
      backgroundImage: "radial-gradient(ellipse at 20% 50%, rgba(200,168,75,0.04) 0%, transparent 60%), radial-gradient(ellipse at 80% 20%, rgba(61,143,196,0.04) 0%, transparent 60%)",
    }}>
      <div style={{ width: 360, padding: "48px 40px", background: C.panel, border: `1px solid ${C.border}`, borderRadius: 6 }}>
        <div style={{ textAlign: "center", marginBottom: 36 }}>
          <div style={{ fontSize: 10, letterSpacing: "0.3em", textTransform: "uppercase", color: C.muted, marginBottom: 12 }}>SHAW INDUSTRIES GROUP</div>
          <div style={{ fontSize: 22, fontFamily: FONT.heading, color: C.text, marginBottom: 6 }}>CEO Strategy Portal</div>
          <div style={{ fontSize: 12, color: C.muted }}>Restricted Access — ELT Only</div>
          <div style={{ height: 1, background: `linear-gradient(90deg, transparent, ${C.accent}44, transparent)`, marginTop: 20 }} />
        </div>

        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 10, color: C.muted, letterSpacing: "0.12em", textTransform: "uppercase", marginBottom: 6 }}>Username</div>
          <input
            value={user} onChange={e => setUser(e.target.value)}
            placeholder="Enter username"
            onKeyDown={e => e.key === "Enter" && attempt()}
            style={{
              width: "100%", background: "#080b10", border: `1px solid ${C.border}`, borderRadius: 3,
              padding: "10px 14px", color: C.text, fontSize: 13, fontFamily: FONT.body,
              outline: "none", boxSizing: "border-box",
            }}
          />
        </div>
        <div style={{ marginBottom: 24 }}>
          <div style={{ fontSize: 10, color: C.muted, letterSpacing: "0.12em", textTransform: "uppercase", marginBottom: 6 }}>Password</div>
          <input
            type="password" value={pass} onChange={e => setPass(e.target.value)}
            placeholder="Enter password"
            onKeyDown={e => e.key === "Enter" && attempt()}
            style={{
              width: "100%", background: "#080b10", border: `1px solid ${C.border}`, borderRadius: 3,
              padding: "10px 14px", color: C.text, fontSize: 13, fontFamily: FONT.body,
              outline: "none", boxSizing: "border-box",
            }}
          />
        </div>

        {err && <div style={{ marginBottom: 16, fontSize: 12, color: C.red, textAlign: "center" }}>{err}</div>}

        <button
          onClick={attempt}
          style={{
            width: "100%", padding: "12px", background: loading ? C.muted : C.accent,
            border: "none", borderRadius: 3, color: "#080b10", fontSize: 13, fontFamily: FONT.body,
            fontWeight: "bold", cursor: "pointer", letterSpacing: "0.1em", textTransform: "uppercase",
            transition: "background 0.2s",
          }}
        >
          {loading ? "Authenticating..." : "Access Portal"}
        </button>

        <div style={{ marginTop: 20, fontSize: 11, color: C.muted, textAlign: "center", lineHeight: 1.6 }}>
          Demo credentials: <span style={{ color: C.accent }}>shaw / ELT2025</span><br />
          <span style={{ fontSize: 10 }}>Expandable to SSO / Auth0 for production</span>
        </div>
      </div>
    </div>
  );
}

// ─── MAIN APP ─────────────────────────────────────────────────────────────────
export default function ShawPortal() {
  const [auth, setAuth] = useState(false);
  const [tab, setTab] = useState("exec");
  const [division, setDivision] = useState("Residential");
  const [lastRefresh] = useState("Mar 1, 2025 06:00 UTC");

  if (!auth) return <Login onLogin={() => setAuth(true)} />;

  const tabs = [
    { key: "exec", label: "Executive Summary" },
    { key: "porter", label: "Porter's Five Forces" },
    { key: "swot", label: "SWOT Engine" },
    { key: "assumptions", label: "Assumption Monitor" },
    { key: "scenarios", label: "Scenario Modeling" },
    { key: "factbase", label: "Fact Base" },
  ];

  const divisions = ["Residential", "Commercial", "Turf"];

  return (
    <div style={{ minHeight: "100vh", background: C.bg, fontFamily: FONT.body, color: C.text }}>
      {/* Top Bar */}
      <div style={{
        background: C.panel, borderBottom: `1px solid ${C.border}`,
        padding: "0 32px", display: "flex", alignItems: "center", justifyContent: "space-between",
        position: "sticky", top: 0, zIndex: 100,
      }}>
        <div style={{ display: "flex", alignItems: "center", gap: 24 }}>
          <div style={{ padding: "14px 0" }}>
            <div style={{ fontSize: 9, color: C.muted, letterSpacing: "0.25em", textTransform: "uppercase" }}>SHAW INDUSTRIES</div>
            <div style={{ fontSize: 14, color: C.text, fontFamily: FONT.heading }}>CEO Strategy Portal</div>
          </div>
          <div style={{ height: 32, width: 1, background: C.border }} />
          {/* Division Toggle */}
          <div style={{ display: "flex", gap: 4 }}>
            {divisions.map(d => (
              <button key={d} onClick={() => setDivision(d)} style={{
                background: division === d ? C.accentDim : "transparent",
                border: `1px solid ${division === d ? C.accent : "transparent"}`,
                borderRadius: 2, padding: "4px 12px", cursor: "pointer",
                fontSize: 11, color: division === d ? C.accent : C.muted,
                letterSpacing: "0.08em",
              }}>{d}</button>
            ))}
          </div>
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
          <div style={{ fontSize: 10, color: C.muted }}>
            <span style={{ width: 6, height: 6, borderRadius: "50%", background: C.green, display: "inline-block", marginRight: 6 }} />
            Refreshed: {lastRefresh}
          </div>
          <button onClick={() => setAuth(false)} style={{
            background: "transparent", border: `1px solid ${C.border}`, borderRadius: 2,
            padding: "5px 12px", cursor: "pointer", fontSize: 10, color: C.muted,
            letterSpacing: "0.1em", textTransform: "uppercase",
          }}>Sign Out</button>
        </div>
      </div>

      {/* Nav Tabs */}
      <div style={{ background: C.panel, borderBottom: `1px solid ${C.border}`, padding: "0 32px", display: "flex", gap: 4, overflowX: "auto" }}>
        {tabs.map(t => (
          <button key={t.key} onClick={() => setTab(t.key)} style={{
            background: "transparent",
            borderBottom: `2px solid ${tab === t.key ? C.accent : "transparent"}`,
            border: "none", borderBottom: `2px solid ${tab === t.key ? C.accent : "transparent"}`,
            padding: "12px 18px", cursor: "pointer",
            fontSize: 12, color: tab === t.key ? C.accent : C.muted,
            letterSpacing: "0.06em", whiteSpace: "nowrap",
            transition: "color 0.2s",
          }}>{t.label}</button>
        ))}
      </div>

      {/* Division Context Banner */}
      <div style={{
        background: `linear-gradient(90deg, ${C.accent}0a, transparent)`,
        borderBottom: `1px solid ${C.border}`,
        padding: "8px 32px", display: "flex", alignItems: "center", gap: 12,
      }}>
        <Badge color={C.accent}>{division} Division</Badge>
        <div style={{ fontSize: 11, color: C.muted }}>
          {division === "Residential" ? "Shaw Floors, Anderson Tuftex, COREtec — Housing starts-driven demand cycle" :
           division === "Commercial" ? "Patcraft, Shaw Contract, Philadelphia — Lagging commercial construction indicator" :
           "Shaw Sports Turf, Southwest Greens — Long-term infrastructure and retrofit cycle"}
        </div>
      </div>

      {/* Content */}
      <div style={{ padding: "32px", maxWidth: 1280, margin: "0 auto" }}>
        {tab === "exec" && <ExecSummary />}
        {tab === "porter" && <PorterForces />}
        {tab === "swot" && <SWOTEngine />}
        {tab === "assumptions" && <AssumptionTracker />}
        {tab === "scenarios" && <ScenarioModule />}
        {tab === "factbase" && <FactBaseModule />}
      </div>

      {/* Footer */}
      <div style={{ padding: "20px 32px", borderTop: `1px solid ${C.border}`, display: "flex", justifyContent: "space-between", alignItems: "center" }}>
        <div style={{ fontSize: 10, color: C.muted, letterSpacing: "0.1em" }}>SHAW INDUSTRIES GROUP, INC. — CONFIDENTIAL ELT DOCUMENT</div>
        <div style={{ fontSize: 10, color: C.muted }}>Data sources: FRED · BLS · U.S. Census · Public filings · Modeled estimates · Q4 2024</div>
      </div>
    </div>
  );
}
