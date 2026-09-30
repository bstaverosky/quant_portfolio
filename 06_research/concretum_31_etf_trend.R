# CONCRETUM: 31 INVESTABLE SECTOR / INDUSTRY ETFs ------------------------------
# Independent R research translation of the ETF section of
# "A Century of Profitable Industry Trends" (Antonacci et al., section 6).
# Paper: https://cmtassociation.org/wp-content/uploads/2026/02/Dow-Winner-A-Century-of-Profitable-Trends.pdf
# Article: https://concretumgroup.substack.com/p/backtest-a-profitable-trend-following
# This is an independent approximation, NOT Concretum's execution engine.

# METHODOLOGY ------------------------------------------------------------------
# {
# 1. OBJECTIVE AND INVESTABLE UNIVERSE
#    Reproduce the paper's ETF experiment, NOT its 1926-2024 hypothetical
#    Fama-French 48-industry experiment. The universe below contains the exact
#    31 State Street SPDR sector and industry ETFs named in the paper's Table 6.
#    Download REAL historical prices via Yahoo Finance; no ETF prices are
#    backfilled before their first available observations. ETF fund expenses
#    and tracking effects are ALREADY included in actual adjusted returns.
#    Overlapping sector/industry holdings are deliberate, as in the paper.
#    ETF eligibility begins ONLY after its own uninterrupted up_days/down_days
#    indicator history and a 20-return volatility estimate become available.
#    Do not use future ETF availability to synthesize earlier positions.
#    Signals use dividend/split-adjusted CLOSES (not intraday highs/lows),
#    recreating the paper's close-only industry-ETF methodology.
#
# 2. INDEPENDENT ENTRY SIGNAL FOR EACH ETF, AT THE END OF TRADING DAY t
#    Donchian upper = rolling 20-day MAX of adjusted closes, INCLUDING t.
#    Keltner upper = EMA(adjusted close,20) + 2 * 1.4 * rolling mean of
#       ABSOLUTE adjusted CLOSE changes over 20 observations.
#    Entry threshold = MIN(Donchian upper, Keltner upper).
#    If flat, enter LONG when adjusted close[t] >= entry threshold[t-1]
#    and both bands have meaningful positive width. The previous day's
#    upper band avoids looking at an entry threshold calculated using a
#    future observation. No short positions; no relative ranking.
#    This deliberately uses the paper's adjusted CLOSE-change ATR proxy,
#    EVEN THOUGH ETF OHLC exists: using true ATR changes the signal.
#
# 3. TRAILING-STOP EXIT
#    Donchian lower = rolling 40-day MIN of adjusted closes.
#    Keltner lower = EMA(close,40) - 2 * 1.4 * mean(abs(close changes),40).
#    Lower band = MAX(Donchian lower, Keltner lower).
#    For an existing long, stop[t] = MAX(stop[t-1],lower band[t]).
#    Exit at close t if close[t] <= stop[t]; never lower a live stop.
#    A stopped ETF can reenter on a LATER session's breakout.
#    This is a CLOSE-BASED stop, NOT an intraday stop/guaranteed fill.
#
# 4. VOLATILITY POSITION SIZING, ETF INCEPTION, AND EXPOSURE LIMITS
#    Vol[t] = population standard deviation (ddof=0) of the 20 most recent
#    adjusted-close TOTAL RETURNS for each ETF, observed by close t.
#    If long, weight[t] = (target_vol / N) / vol[t], otherwise weight=0.
#    Default target_vol = 0.015, N = 31 (paper's fixed universe size).
#    With the optional denominator_mode='available', N instead counts ETFs
#    which have completed their own indicator warmup by date t. This
#    sensitivity is NOT claimed to be the paper's exact denominator choice.
#    Cap any ETF at 20% of NAV, then pro-rata scale the entire portfolio to
#    no more than 200% gross LONG exposure. No discretionary ETF selection.
#    IMPORTANT: target_vol=0.015 is a DAILY position-sizing numerator,
#    not a 1.5% annualized portfolio-volatility target.
#
# 5. EXECUTION, EXTRA LAG, AND REBALANCING
#    Calculate today's signal/target using today's adjusted close.
#    Execute the target AT TODAY'S CLOSE for the NEXT close-to-close return.
#    No same-day return is credited to a target generated at today's close.
#    extra_lag_days=1 means execute today's signal at TOMORROW'S close;
#    first exposure is the return from tomorrow's close to the next close.
#    This close-fill assumption is idealized and may not be achievable
#    with an order submitted after the signal close; a 1-day lag is more
#    conservative. Test extra_lag_days=1,2 for timing sensitivity.
#    Target weights are recomputed daily. Rebalance threshold=10% means
#    skip an adjustment if abs(target shares - current shares) / current
#    shares <=10%. Because both are evaluated at one ETF closing price,
#    the relative share difference equals the relative weight difference.
#    New entries and full exits ALWAYS trade; if a skipped adjustment would
#    breach exposure/position caps, all names rebalance to their targets.
#    This is a continuous/fractional-share PORTFOLIO approximation; an
#    individual whole-share/account-specific trade list is NOT produced.
#
# 6. INTEREST, BORROWING, FEES AND CASH
#    Idle (1 - equity gross weight) earns daily Fama-French RF, sourced from
#    Kenneth French's DAILY factor file (historical one-month T-bill proxy).
#    When gross is >100%, negative cash pays that RF plus optional annual
#    broker_borrow_spread (default 0; realistic borrowed funds cost more).
#    Real ETF adjusted returns already reflect their ongoing ETF expenses;
#    DO NOT subtract ETF expense ratios a second time.
#    At close, approximated commission = abs(dollar turnover)/ETF raw closing
#    price * $0.0035/share, subject to optional $0.35 min per ETF order.
#    Optional slippage/trade_bps charged on gross ETF dollar notional.
#    Costs are deducted from daily NAV at trade close. Cash absorbs costs.
#    Historical adjusted closes construct returns; UNADJUSTED CLOSES estimate
#    contemporaneous traded shares/commissions (not shares at 2026 prices).
#    No taxes, spread dynamics, market impact, fractional-share limitations,
#    margin haircuts, forced liquidation, or live-execution guarantees.
#
# 7. HISTORICAL TESTS AND OUTPUTS
#    PAPER WINDOW: 2005-01 through 2024-03 (31-ETF dynamic inception history).
#    FULL: 2005-01 through latest date shared by the downloaded data.
#    ALL-31 PERIOD: 2018+ once all funds have sufficient actual data.
#    Prints CAGR, EXCESS Sharpe, volatility, maximum drawdown, average gross
#    leverage, dollars and turnover; compares SPY on EXACTLY matched dates.
#    Also runs a zero-trading-cost sensitivity with IDENTICAL signals.
#    Optional performance charts and latest ETF target-weight printout.
#    No exports to your portfolio aggregation directories; only R objects.
#
# 8. LIMITATIONS / REPLICATION CHECK
#    Paper Table 7 reports ETF CAGR 7.7%, Sharpe 0.61, drawdown 24% for
#    Jan 2005-Mar 2024. These are AUTHOR-reported, NOT this script's output.
#    Yahoo history can change or fail, adjustments/commission fills differ,
#    and paper's exact threshold, cash and funding execution may differ.
#    Set strict_universe=TRUE (default) to FAIL rather than silently run a
#    different ETF basket if any of the 31 downloads is unavailable.
#    ETF strategy is not the century-long paper result (~18.2% CAGR).
#    Test robustness to cash borrowing spread, order costs and lag before
#    considering portfolio deployment or leveraged ETF substitutions.
# }
# END METHODOLOGY --------------------------------------------------------------

# USER SETTINGS ----------------------------------------------------------------
start_date <- as.Date("2004-08-01")   # warm up 40-day indicators before Jan 2005
backtest_start <- as.Date("2005-01-03")
paper_end <- as.Date("2024-03-31")
full_universe_start <- as.Date("2018-06-01")
up_days <- 20L
down_days <- 40L
keltner_multiplier <- 2.0
atr_proxy_multiplier <- 1.4
target_vol <- 0.015
max_single_weight <- 0.20
max_gross_exposure <- 2.0
denominator_mode <- "fixed_31"       # "fixed_31" or "available"
extra_lag_days <- 0L                 # 0=trade at signal close; 1=one close late
rebalance_threshold <- 0.10         # 0=every daily resize; 0.10=10% share threshold
commission_per_share <- 0.0035      # paper's $0.0035 / traded share
minimum_commission <- 0.35          # paper's cost-sensitivity assumption
trade_bps <- 0                     # optional extra spread/slippage in bps per side
broker_borrow_spread <- 0          # annual spread above T-bill on gross > 1
initial_capital <- 100000
strict_universe <- TRUE             # fail if ANY paper ETF download fails
show_charts <- interactive()
cache_dir <- file.path(getwd(), "concretum_etf_cache")
use_local_cache <- FALSE            # TRUE uses cached RDS if present; may be stale

# EXACT 31 ETF TICKERS FROM PAPER TABLE 6 (not invented proxies) ---------------
etf_universe <- c(
  "XLF", "XLK", "XLE", "XLV", "XLI", "XBI", "XLU", "XLP", "XLY",
  "KRE", "XLB", "XLC", "XRT", "XOP", "XLRE", "XHB", "KBE", "XME",
  "KIE", "XSD", "XAR", "XES", "KCE", "XNTK", "XHE", "XSW", "XPH",
  "XTN", "XHS", "XITK", "XTL"
)
benchmark_ticker <- "SPY"

suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(zoo)
  library(TTR)
  library(PerformanceAnalytics)
})
options(timeout = 300)
stopifnot(length(etf_universe) == 31L, !anyDuplicated(etf_universe),
          denominator_mode %in% c("fixed_31", "available"),
          up_days >= 2L, down_days >= up_days,
          extra_lag_days >= 0L, extra_lag_days == as.integer(extra_lag_days),
          target_vol > 0, 0 < max_single_weight, max_gross_exposure > 0,
          rebalance_threshold >= 0, commission_per_share >= 0,
          minimum_commission >= 0, trade_bps >= 0,
          broker_borrow_spread >= 0, initial_capital > 0)

# DOWNLOAD ACTUAL ETF PRICES (ADJUSTED FOR RETURNS, RAW CLOSE FOR COMMISSION) --
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
fetch_etf <- function(sym) {
  cache <- file.path(cache_dir, paste0(sym, ".rds"))
  if (use_local_cache && file.exists(cache)) return(readRDS(cache))
  message("Downloading ", sym)
  x <- tryCatch(quantmod::getSymbols(sym, src = "yahoo", from = start_date,
                         auto.assign = FALSE, warnings = FALSE),
                error = function(e) NULL)
  if (is.null(x) || NROW(x) < 60L) stop("No usable Yahoo history for ", sym)
  out <- merge(quantmod::Ad(x), quantmod::Cl(x))
  colnames(out) <- c("Adjusted", "Unadjusted")
  if (any(!is.finite(as.numeric(out[, 1]))) ||
      any(as.numeric(out[, 1]) <= 0) ||
      any(!is.finite(as.numeric(out[, 2]))) ||
      any(as.numeric(out[, 2]) <= 0)) stop("Invalid prices for ", sym)
  if (use_local_cache) saveRDS(out, cache)
  out
}

symbols <- c(benchmark_ticker, etf_universe)
raw <- setNames(vector("list", length(symbols)), symbols)
failed <- character()
for (sym in symbols) {
  x <- tryCatch(fetch_etf(sym), error = function(e) {
    warning(conditionMessage(e)); NULL
  })
  if (is.null(x)) failed <- c(failed, sym) else raw[[sym]] <- x
}
if (benchmark_ticker %in% failed) stop("SPY download failed; cannot backtest.")
if (length(failed) && strict_universe)
  stop("Paper ETF data unavailable: ", paste(failed, collapse = ", "),
       ". Set strict_universe <- FALSE to deliberately run a reduced universe.")
if (length(failed)) {
  message("Reduced basket (NOT exact paper replication): excluded ",
          paste(failed, collapse = ", "))
  etf_universe <- setdiff(etf_universe, failed)
}
if (length(etf_universe) < 5L) stop("Too few ETFs loaded.")

# Stop at last observation shared by all required ETF providers, NOT a forward fill.
raw <- raw[c(benchmark_ticker, etf_universe)]
last_shared <- min(vapply(raw, function(x) as.numeric(last(index(x))), numeric(1)))
last_shared <- as.Date(last_shared, origin = "1970-01-01")
calendar <- index(raw[[benchmark_ticker]])
calendar <- calendar[calendar >= start_date & calendar <= last_shared]
if (length(calendar) < 1000L || max(calendar) < backtest_start)
  stop("Insufficient overlapping SPY ETF history.")

adjusted <- unadjusted <- matrix(NA_real_, length(calendar), length(etf_universe),
                                dimnames = list(NULL, etf_universe))
for (j in seq_along(etf_universe)) {
  sym <- etf_universe[j]
  aligned <- merge(xts::xts(rep(NA_real_, length(calendar)), order.by = calendar),
                   raw[[sym]], join = "left")
  aligned <- aligned[calendar]
  adjusted[, j] <- as.numeric(aligned[, 2])
  unadjusted[, j] <- as.numeric(aligned[, 3])
  valid <- which(is.finite(adjusted[, j]))
  if (!length(valid)) stop("ETF has no data in SPY calendar: ", sym)
  inside <- seq.int(min(valid), max(valid))
  if (any(!is.finite(adjusted[inside, j])) ||
      any(!is.finite(unadjusted[inside, j])))
    stop("Missing ETF bars INSIDE live history for ", sym,
         ". Do not forward-fill returns or invent trades; inspect provider.")
}
spy <- as.numeric(raw[[benchmark_ticker]][calendar, "Adjusted"])
if (any(!is.finite(spy)) || any(spy <= 0)) stop("Missing SPY benchmark prices")

# HISTORICAL DAILY RISK-FREE SERIES (KEN FRENCH, NOT FUTURE OBSERVATIONS) ------
ff_url <- paste0("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/",
                 "F-F_Research_Data_Factors_daily_CSV.zip")
ff_zip <- file.path(cache_dir, "F-F_Research_Data_Factors_daily_CSV.zip")
if (!use_local_cache || !file.exists(ff_zip)) {
  message("Downloading Kenneth French daily RF")
  tryCatch(utils::download.file(ff_url, ff_zip, mode = "wb", quiet = TRUE,
                                method = "libcurl"),
           error = function(e) stop("French RF download failed: ",
                                   conditionMessage(e),
                                   ". Download FF ZIP manually into: ", ff_zip))
}
if (!file.exists(ff_zip) || file.info(ff_zip)$size < 100L)
  stop("Missing or corrupt French RF zip: ", ff_zip)
ff_members <- utils::unzip(ff_zip, list = TRUE)$Name
ff_csv <- ff_members[grepl("\\.csv$", ff_members, ignore.case = TRUE)]
if (length(ff_csv) != 1L) stop("Expected one CSV inside RF zip")
ff_extract <- utils::unzip(ff_zip, files = ff_csv, exdir = cache_dir,
                           overwrite = TRUE)
ff_lines <- readLines(ff_extract, warn = FALSE, encoding = "UTF-8")
ff_daily <- grepl("^\\s*[0-9]{8}\\s*,", ff_lines)
ff_start <- which(ff_daily)[1]
if (is.na(ff_start)) stop("French RF file has no daily observations")
ff_after <- which(!ff_daily[ff_start:length(ff_lines)])
ff_end <- if (length(ff_after)) ff_start + ff_after[1] - 2L else length(ff_lines)
ff_tab <- utils::read.csv(text = paste(ff_lines[ff_start:ff_end], collapse = "\n"),
                          header = FALSE, strip.white = TRUE)
if (NCOL(ff_tab) != 5L) stop("French RF daily CSV format has changed")
ff_dates <- as.Date(trimws(as.character(ff_tab[[1]])), "%Y%m%d")
ff_rf <- xts::xts(as.numeric(ff_tab[[5]]) / 100, order.by = ff_dates)
rf_grid <- xts::xts(rep(NA_real_, length(calendar)), order.by = calendar)
rf_join <- merge(rf_grid, ff_rf, join = "left")[calendar]
rf_raw <- as.numeric(rf_join[, 2])
if (any(!is.finite(rf_raw[calendar <= max(ff_dates)])))
  stop("Historical Fama-French RF dates are missing. Inspect ETF/FF calendars.")
# French factors can be published later than live ETF bars. Use only the last
# ALREADY RELEASED daily RF value for trailing dates, and disclose the count.
rf <- as.numeric(zoo::na.locf(rf_join[, 2], na.rm = FALSE))
if (any(!is.finite(rf)))
  stop("RF unavailable at the beginning of the ETF backtest.")
if (any(!is.finite(rf_raw)))
  warning(sum(!is.finite(rf_raw)), " recent ETF days extend past published FF RF;",
          " carrying the last released daily RF (cash approximation).")

# CALCULATE PRICE-ONLY INDICATORS AND ORIGINAL CLOSE-ONLY PROXY ---------------
Tn <- length(calendar)
Nn <- length(etf_universe)
ret <- matrix(NA_real_, Tn, Nn, dimnames = list(NULL, etf_universe))
VOL <- UPPER <- LOWER <- matrix(NA_real_, Tn, Nn,
                                dimnames = list(NULL, etf_universe))
ready <- matrix(FALSE, Tn, Nn, dimnames = list(NULL, etf_universe))

roll_mean_min <- function(x, n, min_obs = n - 1L) {
  good <- is.finite(x)
  x0 <- ifelse(good, x, 0)
  sums <- c(0, cumsum(x0))
  counts <- c(0, cumsum(as.integer(good)))
  ix <- seq_along(x)
  from <- pmax(1L, ix - n + 1L)
  number <- counts[ix + 1L] - counts[from]
  avg <- (sums[ix + 1L] - sums[from]) / pmax(number, 1L)
  avg[number < min_obs] <- NA_real_
  avg
}

pandas_ema <- function(x, n) {
  ans <- rep(NA_real_, length(x))
  if (!length(x)) return(ans)
  ans[1] <- x[1]
  a <- 2 / (n + 1)
  if (length(x) > 1L)
    for (k in 2:length(x)) ans[k] <- a * x[k] + (1 - a) * ans[k - 1L]
  ans
}
for (j in seq_len(Nn)) {
  ix <- which(is.finite(adjusted[, j]))
  seg <- seq.int(ix[1], tail(ix, 1))
  p <- adjusted[seg, j]
  r <- c(NA_real_, diff(p) / head(p, -1L))
  delta <- c(NA_real_, abs(diff(p)))
  ema_up <- pandas_ema(p, up_days)
  ema_down <- pandas_ema(p, down_days)
  du <- as.numeric(TTR::runMax(p, n = up_days))
  dl <- as.numeric(TTR::runMin(p, n = down_days))
  ku <- ema_up + keltner_multiplier * atr_proxy_multiplier *
    roll_mean_min(delta, up_days)
  kd <- ema_down - keltner_multiplier * atr_proxy_multiplier *
    roll_mean_min(delta, down_days)
  v <- c(NA_real_, as.numeric(TTR::runSD(r[-1L], n = up_days))) *
    sqrt((up_days - 1) / up_days)  # ddof=0, matching paper's Python
  ret[seg, j] <- r
  VOL[seg, j] <- v
  UPPER[seg, j] <- pmin(du, ku)
  LOWER[seg, j] <- pmax(dl, kd)
  ready[seg, j] <- is.finite(UPPER[seg, j]) &
    is.finite(LOWER[seg, j]) & is.finite(v) & v > 0 &
    is.finite(ret[seg, j])
}

# STATE MACHINE: A SEPARATE RATCHETING STOP FOR EACH ETF -----------------------
active <- matrix(FALSE, Tn, Nn, dimnames = list(NULL, etf_universe))
trailing_stop <- matrix(NA_real_, Tn, Nn,
                        dimnames = list(NULL, etf_universe))
for (t in 2:Tn) {
  eligible <- ready[t, ]
  new_long <- eligible & !active[t - 1L, ] & ready[t - 1L, ] &
    adjusted[t, ] >= UPPER[t - 1L, ] &
    UPPER[t - 1L, ] > LOWER[t - 1L, ]
  live_stop <- pmax(trailing_stop[t - 1L, ], LOWER[t, ])
  stay_long <- eligible & active[t - 1L, ] &
    is.finite(live_stop) & adjusted[t, ] > live_stop
  new_long[is.na(new_long)] <- FALSE
  stay_long[is.na(stay_long)] <- FALSE
  active[t, new_long | stay_long] <- TRUE
  trailing_stop[t, new_long] <- LOWER[t, new_long]
  trailing_stop[t, stay_long] <- live_stop[stay_long]
}

# CLOSE-t TARGET; NEXT-DAY RETURN USES PREVIOUS CLOSE'S EXECUTED WEIGHTS ------
denominator <- if (denominator_mode == "fixed_31")
  rep(length(etf_universe), Tn) else pmax(rowSums(ready), 1)
raw_weight <- target_vol / VOL
raw_weight[!is.finite(raw_weight)] <- 0
raw_weight <- raw_weight * active / denominator
raw_weight <- pmin(raw_weight, max_single_weight)
raw_weight[!is.finite(raw_weight)] <- 0
scaler <- pmin(1, max_gross_exposure / pmax(rowSums(raw_weight), 1e-12))
target_weight <- raw_weight * scaler
if (any(!is.finite(target_weight)) || any(target_weight < -1e-12) ||
    any(rowSums(target_weight) > max_gross_exposure + 1e-8))
  stop("Target weights violate allocation constraints")

# SIMULATION: mark prior weights to today's close, then trade at today's close.
# The first eligible investment date is the defined Jan-2005 backtest start.
run_engine <- function(commission = commission_per_share,
                       min_commission = minimum_commission,
                       slippage_bps = trade_bps,
                       threshold = rebalance_threshold,
                       lag_days = extra_lag_days) {
  portfolio_return <- rep(NA_real_, Tn)
  gross_weight <- rep(NA_real_, Tn)
  notional_turnover <- rep(NA_real_, Tn)
  trading_cost <- rep(NA_real_, Tn)
  equity <- rep(NA_real_, Tn)
  held_weight <- matrix(NA_real_, Tn, Nn,
                        dimnames = list(NULL, etf_universe))
  n_trades <- integer(Tn)
  date_start <- which(calendar >= backtest_start)[1]
  if (is.na(date_start)) stop("Backtest start follows data end")
  nav <- initial_capital
  w <- rep(0, Nn)   # weights held AFTER previous close
  for (t in date_start:Tn) {
    # If a held ETF has no valid return, STOP rather than creating fake data.
    r <- ret[t, ]
    if (any(w > 1e-12 & !is.finite(r)))
      stop("Missing ETF total return while held on ", calendar[t], ": ",
           paste(etf_universe[which(w > 1e-12 & !is.finite(r))], collapse=", "))
    r[!is.finite(r)] <- 0  # never held: harmless arithmetic placeholder
    gross_before <- sum(w)
    cash_w <- 1 - gross_before
    funding_drag <- max(gross_before - 1, 0) * broker_borrow_spread / 252
    daily_gross <- sum(w * r) + cash_w * rf[t] - funding_drag
    if (!is.finite(daily_gross) || daily_gross <= -1)
      stop("Portfolio NAV exhausted on ", calendar[t])
    nav_before_trades <- nav * (1 + daily_gross)
    drifted <- w * (1 + r) / (1 + daily_gross)

    # lag_days=0 => signal from today's close executes at today's close.
    # lag_days=1 => yesterday's close signal executes at today's close.
    from_signal <- t - as.integer(lag_days)
    ideal <- if (from_signal >= date_start) target_weight[from_signal, ]
             else rep(0, Nn)
    trade_w <- ideal
    if (threshold > 0) {
      change_ratio <- abs(ideal - drifted) / pmax(drifted, 1e-12)
      skip <- drifted > 0 & ideal > 0 & change_ratio <= threshold
      trade_w[skip] <- drifted[skip]
      # Risk caps override a small-trade threshold.
      if (sum(trade_w) > max_gross_exposure + 1e-10 ||
          any(trade_w > max_single_weight + 1e-10))
        trade_w <- ideal
    }
    traded <- abs(trade_w - drifted)
    ord <- which(traded > 1e-10)
    dollar_notional <- nav_before_trades * traded
    share_count <- dollar_notional[ord] / unadjusted[t, ord]
    if (any(!is.finite(share_count)))
      stop("Missing ETF unadjusted trade close on ", calendar[t])
    costs <- if (length(ord))
      sum(pmax(min_commission, commission * share_count) +
            slippage_bps / 10000 * dollar_notional[ord]) else 0
    if (costs >= nav_before_trades) stop("Trade costs exhausted NAV")
    nav_end <- nav_before_trades - costs
    portfolio_return[t] <- nav_end / nav - 1
    gross_weight[t] <- sum(trade_w)
    notional_turnover[t] <- sum(traded)
    trading_cost[t] <- costs
    n_trades[t] <- length(ord)
    equity[t] <- nav_end
    held_weight[t, ] <- trade_w
    w <- trade_w              # target % of after-cost NAV; cash absorbs fees
    nav <- nav_end
  }
  list(
    returns = xts::xts(portfolio_return, order.by = calendar),
    weights = xts::xts(held_weight, order.by = calendar),
    gross = xts::xts(gross_weight, order.by = calendar),
    turnover = xts::xts(notional_turnover, order.by = calendar),
    costs = xts::xts(trading_cost, order.by = calendar),
    trades = xts::xts(n_trades, order.by = calendar),
    equity = xts::xts(equity, order.by = calendar)
  )
}

actual <- run_engine()
zero_cost <- run_engine(commission=0, min_commission=0, slippage_bps=0)
spy_ret <- xts::xts(c(NA_real_, diff(spy) / head(spy, -1L)), order.by = calendar)
rf_xts <- xts::xts(rf, order.by = calendar)

# PERFORMANCE REPORTS: ALIGN START/END IDENTICALLY ACROSS SERIES --------------
period_stats <- function(first_date, last_date, label) {
  dates <- calendar >= first_date & calendar <= last_date &
    is.finite(as.numeric(actual$returns)) & is.finite(as.numeric(spy_ret))
  if (sum(dates) < 42L) {
    cat("\n", label, ": fewer than 42 days; skipped\n", sep = "")
    return(NULL)
  }
  a <- as.numeric(actual$returns[dates])
  z <- as.numeric(zero_cost$returns[dates])
  b <- as.numeric(spy_ret[dates])
  r <- rf[dates]
  metric <- function(v) {
    wealth <- cumprod(1 + v)
    dd <- wealth / cummax(c(1, wealth))[-1L] - 1
    xs <- v - r
    c(CAGR = 100 * (tail(wealth, 1)^(252 / length(v)) - 1),
      Sharpe = if (sd(xs) > 0) sqrt(252) * mean(xs) / sd(xs) else NA_real_,
      Vol = 100 * sd(v) * sqrt(252),
      MaxDD = 100 * min(dd))
  }
  out <- rbind(ETF_Strategy = metric(a),
               ETF_No_Trade_Cost = metric(z),
               SPY = metric(b))
  cat("\n========== ", label, " =========\n", sep="")
  cat("Sample:", as.character(min(calendar[dates])), "to",
      as.character(max(calendar[dates])), "|", sum(dates), "days\n")
  print(round(out, 2))
  cat("Mean gross exposure:",
      round(mean(as.numeric(actual$gross[dates])), 3), "x NAV",
      "| Mean daily gross traded notional:",
      round(100 * mean(as.numeric(actual$turnover[dates])), 3), "% NAV",
      "| Trades:", sum(as.numeric(actual$trades[dates])),
      "| Commission + slippage: $",
      round(sum(as.numeric(actual$costs[dates])), 2), "\n")
  invisible(out)
}

paper_results <- period_stats(backtest_start, paper_end,
                              "PAPER MATCH: JAN 2005 TO MAR 2024")
full_results <- period_stats(backtest_start, max(calendar),
                             "ALL AVAILABLE ACTUAL ETF HISTORY")
first_ready <- vapply(seq_len(Nn), function(j) {
  good <- which(ready[, j])
  if (!length(good)) stop("No eligible history: ", etf_universe[j])
  as.numeric(calendar[good[1]])
}, numeric(1))
common_start <- max(full_universe_start,
                    as.Date(max(first_ready), origin = "1970-01-01"))
common_results <- period_stats(common_start, max(calendar),
                               "ALL ETFs ELIGIBLE (POST-2018)")

# PRINT CURRENT TARGETS: today-close signals vs delayed executable targets ----
last_t <- Tn
exec_signal_idx <- last_t - as.integer(extra_lag_days)
latest_target <- target_weight[last_t, ]
next_execution_target <- if (exec_signal_idx >= 1L)
  target_weight[exec_signal_idx, ] else rep(0, Nn)
names(latest_target) <- names(next_execution_target) <- etf_universe
cat("\n========== LATEST SIGNAL (NOT A LIVE ORDER) =========\n")
cat("Data through:", as.character(calendar[last_t]), "\n")
cat("Close-generated targets for next close-to-close return (zero lag):\n")
print(round(latest_target[latest_target > 0], 4))
cat("\nLag-adjusted targets used by last backtested closing trade:\n")
print(round(next_execution_target[next_execution_target > 0], 4))
cat("Actual latest held ETF weights (rebalance-threshold adjusted):\n")
last_held <- as.numeric(actual$weights[last_t, ])
names(last_held) <- etf_universe
print(round(last_held[last_held > 0], 4))
cat("Cash balance weight:", round(1 - sum(last_held), 4),
    "| Gross equity leverage:", round(sum(last_held), 4), "\n")
if (Sys.Date() - last(calendar) > 5L)
  warning("Historical prices are more than 5 calendar days old; do not trade this output.")

if (show_charts) {
  curve <- merge(actual$returns, zero_cost$returns, spy_ret)
  colnames(curve) <- c("ETF trend / fees", "ETF trend / zero costs", "SPY")
  curve <- curve[paste0(backtest_start, "/")]
  curve <- curve[complete.cases(curve)]
  PerformanceAnalytics::charts.PerformanceSummary(curve,
    main = "Concretum: 31 actual industry ETFs vs SPY")
  plot(actual$gross[paste0(backtest_start, "/")],
       main = "Actual ETF gross notional / NAV", ylab = "x NAV")
}

# Useful objects remain in memory; NO automatic files/strategy_outputs writes.
backtest <- list(returns = actual$returns, benchmark = spy_ret,
                 no_cost_returns = zero_cost$returns,
                 target_weights = xts::xts(target_weight, order.by = calendar),
                 actual_weights = actual$weights,
                 active = xts::xts(1 * active, order.by = calendar),
                 stops = xts::xts(trailing_stop, order.by = calendar),
                 gross_exposure = actual$gross,
                 traded_notional = actual$turnover,
                 cost_dollars = actual$costs,
                 equity = actual$equity,
                 cash_rate = rf_xts,
                 paper_results = paper_results,
                 full_results = full_results,
                 common_results = common_results,
                 universe = etf_universe,
                 latest_target = latest_target,
                 latest_executed = last_held)
