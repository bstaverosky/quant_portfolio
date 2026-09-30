# ==============================================================================
# JOSH MULTI-ASSET RESEARCH: MACRO DATA HARVESTER
# ==============================================================================
#
# METHODOLOGY NOTE -------------------------------------------------------------
#
# PURPOSE
#   Pull a deliberately small set of public macro/market series from FRED so
#   macro information can be tested as an independent overlay on the existing
#   return-based multi-asset momentum strategy.
#
# RESEARCH PHILOSOPHY
#   - Do NOT fit a giant macro model.
#   - Keep signals economically interpretable.
#   - First test macro variables as family-level confirmation / veto rules.
#   - Keep momentum, family concentration rules, ERC, and leverage as separate
#     layers so any improvement can be attributed cleanly.
#
# SERIES -----------------------------------------------------------------------
#
# Rates / curve:
#   DGS2      2-Year Treasury constant maturity yield
#   DGS10     10-Year Treasury constant maturity yield
#
# Policy:
#   DFF       Effective federal funds rate
#
# Credit:
#   BAA10Y    Moody's Baa yield minus 10-Year Treasury yield
#
# Real rates / inflation expectations (available from 2003):
#   DFII10    10-Year TIPS real yield
#   T10YIE    10-Year breakeven inflation rate
#   T5YIFR    5-year, 5-year forward inflation expectation rate
#
# Dollar:
#   DTWEXB    Legacy broad nominal dollar index, through 2019
#   DTWEXBGS  Current broad nominal dollar index, from 2006
#
# DOLLAR STITCH
#   DTWEXB and DTWEXBGS use different index bases but overlap for many years.
#   The script scales DTWEXBGS to the legacy series using the median ratio over
#   their overlap, then uses legacy observations through 2019-12-31 and the
#   scaled current series afterward. Since subsequent research will primarily
#   use percentage/log changes, the arbitrary index base itself is immaterial.
#
# LOOKAHEAD / TIMING
#   These are observation-date series, not publication-vintage data.
#   For daily market series (Treasury yields, breakevens, dollar, spreads), the
#   intended backtest will use values known by the month-end signal date and
#   trade no earlier than the next strategy trading day.
#
#   Do NOT use revised monthly macroeconomic releases (GDP, CPI, unemployment,
#   etc.) without explicit publication lags or vintage data. They are omitted
#   from this first pass intentionally.
#
# OUTPUTS
#   josh_macro_research_raw.csv
#       Raw/stiched FRED daily panel.
#
#   josh_macro_research_month_end.csv
#       Last available observation in each calendar month.
#
#   josh_macro_research_features.csv
#       Pre-computed simple changes used only for convenience. The Python
#       research can rebuild all features from the raw series.
#
# ==============================================================================

rm(list = setdiff(ls(), character(0)))

suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(zoo)
})

### PARAMETERS -----------------------------------------------------------------

start_date <- as.Date("1995-01-01")

output_dir <- "~/quant_portfolio/python_research_data"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

fred_series <- c(
  "DGS2",
  "DGS10",
  "DFF",
  "BAA10Y",
  "DFII10",
  "T10YIE",
  "T5YIFR",
  "DTWEXB",
  "DTWEXBGS"
)


### DOWNLOAD FRED DATA ----------------------------------------------------------

fred_list <- lapply(
  fred_series,
  function(symbol) {

    cat("Downloading", symbol, "...\n")

    x <- getSymbols(
      symbol,
      src = "FRED",
      from = start_date,
      auto.assign = FALSE
    )

    colnames(x) <- symbol

    x
  }
)

macro_raw <- do.call(
  merge,
  c(
    fred_list,
    all = TRUE
  )
)

macro_raw <- macro_raw[
  index(macro_raw) >= start_date,
]


### FORWARD-FILL MARKET-DAY GAPS ------------------------------------------------
#
# FRED series have different holiday / publication calendars.
# Forward filling lets the eventual strategy use the most recently observable
# value on a strategy trading date without introducing future information.

macro_ffill <- zoo::na.locf(
  macro_raw,
  na.rm = FALSE
)


### STITCH BROAD DOLLAR INDEX ---------------------------------------------------

overlap <- merge(
  macro_ffill$DTWEXB,
  macro_ffill$DTWEXBGS,
  join = "inner"
)

overlap <- overlap[
  complete.cases(overlap),
]

usd_scale <- median(
  overlap[, 1] / overlap[, 2],
  na.rm = TRUE
)

usd_current_scaled <-
  macro_ffill$DTWEXBGS *
  as.numeric(usd_scale)

USD_BROAD <- macro_ffill$DTWEXB

modern_i <- index(macro_ffill) >
  as.Date("2019-12-31")

USD_BROAD[modern_i] <-
  usd_current_scaled[modern_i]

colnames(USD_BROAD) <- "USD_BROAD"

macro_raw <- merge(
  macro_raw,
  USD_BROAD
)


### DERIVED RAW ECONOMIC SERIES -------------------------------------------------

CURVE_10Y2Y <-
  macro_ffill$DGS10 -
  macro_ffill$DGS2

colnames(CURVE_10Y2Y) <-
  "CURVE_10Y2Y"

macro_raw <- merge(
  macro_raw,
  CURVE_10Y2Y
)


### EXPORT RAW DAILY PANEL ------------------------------------------------------

raw_export <- data.frame(
  Date = as.Date(index(macro_raw)),
  coredata(macro_raw),
  check.names = FALSE
)

write.csv(
  raw_export,
  file.path(
    output_dir,
    "josh_macro_research_raw.csv"
  ),
  row.names = FALSE,
  na = ""
)


### MONTH-END PANEL -------------------------------------------------------------

macro_daily_complete <- zoo::na.locf(
  macro_raw,
  na.rm = FALSE
)

month_end_i <- endpoints(
  macro_daily_complete,
  "months"
)

month_end_i <- month_end_i[
  month_end_i > 0
]

macro_month_end <-
  macro_daily_complete[
    month_end_i,
  ]

month_export <- data.frame(
  Date = as.Date(index(macro_month_end)),
  coredata(macro_month_end),
  check.names = FALSE
)

write.csv(
  month_export,
  file.path(
    output_dir,
    "josh_macro_research_month_end.csv"
  ),
  row.names = FALSE,
  na = ""
)


### SIMPLE FEATURE PANEL --------------------------------------------------------
#
# These transformations are PRE-REGISTERED convenience features, not optimized
# specifications. Python research should test them in small families rather than
# search hundreds of arbitrary windows.
#
# Approximate trading-day horizons:
#   21  = 1 month
#   63  = 3 months
#   126 = 6 months
#
# Sign conventions used later:
#
# Treasuries:
#   + curve slope
#   - change in nominal 10y yield
#   - change in real 10y yield
#   - change in fed funds
#   - change in breakeven inflation
#   + change in credit spread as a possible flight-to-quality confirmation
#
# Gold:
#   - change in real 10y yield
#   - broad-dollar return
#   + change in breakeven inflation
#
# REITs:
#   - change in real yield
#   - change in Baa spread
#
# Equities:
#   - change in Baa spread
#   + curve slope (to be tested cautiously, not assumed universally)

x <- zoo::na.locf(
  macro_raw,
  na.rm = FALSE
)

feature_panel <- merge(
  x,

  DGS10_CHG_63  = x$DGS10 - lag(x$DGS10, 63),
  DGS10_CHG_126 = x$DGS10 - lag(x$DGS10, 126),

  DFF_CHG_63    = x$DFF - lag(x$DFF, 63),
  DFF_CHG_126   = x$DFF - lag(x$DFF, 126),

  BAA10Y_CHG_63  = x$BAA10Y - lag(x$BAA10Y, 63),
  BAA10Y_CHG_126 = x$BAA10Y - lag(x$BAA10Y, 126),

  DFII10_CHG_63  = x$DFII10 - lag(x$DFII10, 63),
  DFII10_CHG_126 = x$DFII10 - lag(x$DFII10, 126),

  T10YIE_CHG_63  = x$T10YIE - lag(x$T10YIE, 63),
  T10YIE_CHG_126 = x$T10YIE - lag(x$T10YIE, 126),

  USD_RET_63 =
    (x$USD_BROAD / lag(x$USD_BROAD, 63)) - 1,

  USD_RET_126 =
    (x$USD_BROAD / lag(x$USD_BROAD, 126)) - 1
)

feature_export <- data.frame(
  Date = as.Date(index(feature_panel)),
  coredata(feature_panel),
  check.names = FALSE
)

write.csv(
  feature_export,
  file.path(
    output_dir,
    "josh_macro_research_features.csv"
  ),
  row.names = FALSE,
  na = ""
)


cat(
  "\nMacro research files written to:\n",
  normalizePath(output_dir),
  "\n\nDollar stitch scale factor:",
  round(as.numeric(usd_scale), 6),
  "\n"
)
