# CONCRETUM-INSPIRED SINGLE-ETF TREND FOLLOWING + OPTIONAL LEVERAGE -----------
# Research adaptation of the entry/exit signals in:
# https://concretumgroup.substack.com/p/backtest-a-profitable-trend-following
# This is NOT a reproduction of the authors' 48-industry portfolio performance.
# It uses one real ETF and user-selected constant exposure while the signal is on.
#
# METHODOLOGY ---------------------------------------------------------------
# {
# 1. PURPOSE / ASSET
#    Download historical OHLC and adjusted closing prices for ONE ETF (default
#    QQQ). No synthetic pre-inception history, no future market data, no
#    cross-sectional signals, and no short positions. Adjusted closes capture
#    split/dividend-adjusted ETF total returns (subject to data-provider quality).
#    Base ETF expense ratios are ALREADY embedded in these actual returns.
#
# 2. ORIGINAL PAPER'S TWO ENTRY INDICATORS, EVALUATED AT CLOSE t
#    Donchian upper = MAX of adjusted closes over the last up_days (20).
#    Keltner upper = EMA(adjusted close, up_days)
#                    + keltner_multiplier * estimated ATR(up_days).
#    In paper_proxy mode, estimated ATR(n) = atr_proxy_multiplier (1.4)
#                    * mean(abs(adjusted close[t] - adjusted close[t-1]), n).
#    This is the paper's closing-price-only approximation: 2 * 1.4 = 2.8.
#    Entry threshold = MIN(Donchian upper, Keltner upper).
#    IF FLAT AND adjusted close[t] >= entry threshold[t-1], signal LONG.
#    Yesterday's threshold avoids comparing against a band that has already
#    incorporated today's breakout. The bands need NOT agree.
#
# 3. THE TWO EXIT INDICATORS AND RATCHETING STOP
#    Donchian lower = MIN of adjusted closes over down_days (40).
#    Keltner lower = EMA(adjusted close, down_days)
#                    - keltner_multiplier * estimated ATR(down_days).
#    Candidate lower band = MAX(Donchian lower, Keltner lower).
#    While LONG, trailing_stop[t] = MAX(trailing_stop[t-1], lower_band[t]).
#    Signal FLAT if adjusted close[t] <= trailing_stop[t]. The stop can rise
#    but NEVER decline during an active trade. Reentry is allowed only on a
#    LATER day after a fresh entry condition; there is no same-close flip.
#    Stops are evaluated at the CLOSE, not executable intraday stop orders.
#
# 4. OPTIONAL TRUE ATR (A SENSITIVITY TEST, NOT IDENTICAL TO PAPER)
#    Set atr_method='true_atr' to use TTR's Wilder-smoothed ATR from adjusted
#    high, low and close instead of 1.4 * average absolute close change.
#    Default Donchian bands ALWAYS use adjusted closes, matching the paper.
#    Optionally set donchian_use_high_low=TRUE to use adjusted high/low bands;
#    this ALSO changes the paper's signals. The ETF's daily OHLC are scaled
#    by adjusted_close/raw_close to put all bands on the same price basis.
#
# 5. POSITION / LEVERAGE (SINGLE-ETF ADAPTATION, NOT PAPER RISK SIZING)
#    When LONG, target ETF dollar exposure = leverage * account equity;
#    when FLAT, target ETF exposure = 0. leverage=1 means 100% ETF;
#    leverage=2 means 200% ETF and -100% financing cash; leverage=0.5
#    means 50% ETF + 50% T-bill cash. leverage=0 means always cash.
#    Reset to this target at EVERY closing rebalance. It is a DAILY-RESET
#    synthetic exposure/margin model, NOT actual TQQQ/UPRO and NOT a
#    one-time buy-and-hold margin loan. There is no modeled ETF tracking
#    error or extra leveraged-ETF fee. Amplified daily losses can bankrupt
#    the hypothetical account; no margin calls/forced liquidations modeled.
#
# 6. CAUSAL TRADE EXECUTION AND OPTIONAL EXTRA SIGNAL LAG
#    Signals use information observable at CLOSE t. With extra_lag_days=0,
#    the trade executes at CLOSE t+1, then earns ETF return from CLOSE
#    t+1 to CLOSE t+2. In the daily simulator, after processing return on
#    date i, the executable signal is from date i-1-extra_lag_days.
#    extra_lag_days=1 makes EVERY signal trade one more close late.
#    Signal state and execution state are separate; this avoids using a
#    newly observed closing signal to profit from that same day's return.
#    Fills at the next close are a model assumption, not guaranteed quotes.
#
# 7. CASH, BORROWING AND TURNOVER COSTS
#    Positive cash earns prior-observed FRED DTB3 (3-month Treasury yield)
#    /100/252; negative cash pays that rate PLUS borrowing_spread/252.
#    When the FRED download is unavailable, a user-set fixed fallback rate
#    is used WITH AN EXPLICIT WARNING. The lagged yield prevents peeking.
#    Trading cost = trade_bps/10000 times ETF NOTIONAL changed at each close.
#    Rebalancing costs include daily leverage resets, not just entries/exits.
#    Borrowing is charged only while dollar exposure exceeds 100% NAV.
#    Financing and trade costs are approximate; no taxes, bid/ask evolution,
#    slippage beyond trade_bps, margin maintenance, or whole-share effects.
#
# 8. RESULTS / COMPARISONS
#    Calculate net strategy versus 1x buy-and-hold OF THE SAME ETF on
#    IDENTICAL trading dates. Also compare constant-leverage buy-and-hold,
#    which is DAILY RESET using the same funding/cost model. Report CAGR,
#    volatility, excess-return Sharpe, max drawdown, time in market,
#    average gross exposure, annualized traded notional, and trading costs.
#    Save optional daily returns and executed trade records to CSV.
#    Adjusting leverage changes exposure, NOT the underlying breakout or
#    trailing-stop calculations. The paper's multi-industry diversification
#    and inverse-volatility portfolio sizing are NOT included here.
# }
# END METHODOLOGY -----------------------------------------------------------

# USER SETTINGS -------------------------------------------------------------
etf_ticker <- "SPY"                    # e.g. "SPY", "XLK", "XLE", "XBI"
leverage <- 2.0                       # 0=cash, 0.5=half ETF, 1=ETF, 2=2x, 3=3x
history_start <- as.Date("1998-01-01") # request earlier data to warm up indicators
backtest_start <- as.Date("2001-01-01")
backtest_end <- Sys.Date()

up_days <- 20L
down_days <- 40L
keltner_multiplier <- 2.0
atr_proxy_multiplier <- 1.4
atr_method <- "paper_proxy"           # "paper_proxy" or "true_atr"
donchian_use_high_low <- FALSE         # FALSE matches paper's close-only method

extra_lag_days <- 0L                  # 0=NEXT close; 1=two closes after signal
trade_bps <- 5                        # one-way ETF NOTIONAL turnover cost
borrowing_spread <- 0.015            # annual spread ABOVE T-bill when >100% ETF
fallback_cash_rate <- 0.02           # annual only if FRED DTB3 fails
initial_capital <- 100000

show_charts <- interactive()
export_csv <- TRUE
output_dir <- file.path(getwd(), "concretum_single_etf_results")

# DEPENDENCIES / VALIDATION --------------------------------------------------
required_packages <- c("quantmod", "xts", "zoo", "TTR", "PerformanceAnalytics")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                                logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install missing packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(zoo)
  library(TTR)
  library(PerformanceAnalytics)
})
stopifnot(is.character(etf_ticker), length(etf_ticker) == 1L,
          nzchar(etf_ticker), length(leverage) == 1L, is.finite(leverage),
          leverage >= 0, up_days >= 2L, down_days >= 2L,
          keltner_multiplier > 0, atr_proxy_multiplier > 0,
          atr_method %in% c("paper_proxy", "true_atr"),
          extra_lag_days >= 0L, extra_lag_days == as.integer(extra_lag_days),
          trade_bps >= 0, borrowing_spread >= 0, fallback_cash_rate >= 0,
          initial_capital > 0, history_start < backtest_end,
          backtest_start <= backtest_end)
options(timeout = max(300, getOption("timeout", 60)))

# FETCH REAL ETF DATA AND ADJUST DAILY OHLC TO TOTAL-RETURN PRICE BASIS ------
message("Downloading ", etf_ticker, " daily prices from Yahoo Finance...")
raw <- tryCatch(
  quantmod::getSymbols(etf_ticker, src = "yahoo", from = history_start,
                       to = backtest_end + 1, auto.assign = FALSE,
                       warnings = FALSE),
  error = function(e) stop("ETF download failed for ", etf_ticker, ": ",
                           conditionMessage(e)))
if (NROW(raw) < max(up_days, down_days) + 20L) {
  stop("Too little ETF history for the requested lookbacks: ", etf_ticker)
}
raw_close <- as.numeric(quantmod::Cl(raw))
adj_close <- as.numeric(quantmod::Ad(raw))
open_raw <- as.numeric(quantmod::Op(raw))
high_raw <- as.numeric(quantmod::Hi(raw))
low_raw <- as.numeric(quantmod::Lo(raw))
dates <- as.Date(zoo::index(raw))
valid_prices <- is.finite(raw_close) & is.finite(adj_close) &
  is.finite(open_raw) & is.finite(high_raw) & is.finite(low_raw) &
  raw_close > 0 & adj_close > 0 & open_raw > 0 & high_raw > 0 & low_raw > 0
if (!all(valid_prices)) {
  stop("ETF price history has missing/nonpositive OHLC or adjusted closes; ",
       "do not silently forward-fill missing trading bars.")
}
adjustment <- adj_close / raw_close
adj_high <- high_raw * adjustment
adj_low <- low_raw * adjustment
n <- length(dates)
price <- xts::xts(adj_close, order.by = dates)
colnames(price) <- etf_ticker
etf_return <- c(NA_real_, adj_close[-1L] / head(adj_close, -1L) - 1)
if (any(!is.finite(etf_return[-1L])) || any(etf_return[-1L] <= -1)) {
  stop("Invalid ETF daily returns, or ETF lost 100% in a day.")
}

# RISK-FREE YIELD: PREVIOUS SESSION'S 3-MONTH TREASURY YIELD -----------------
message("Downloading FRED DTB3 for historical cash/financing...")
rf_data <- tryCatch(
  quantmod::getSymbols("DTB3", src = "FRED", from = history_start,
                       to = backtest_end + 1, auto.assign = FALSE),
  error = function(e) NULL)
if (is.null(rf_data) || NROW(rf_data) == 0L) {
  warning("FRED DTB3 unavailable: using fixed fallback_cash_rate=",
          fallback_cash_rate, " per year for the ENTIRE history.")
  rf_daily <- rep(fallback_cash_rate / 252, n)
  cash_source <- "constant fallback, NOT historical RF"
} else {
  rate_aligned <- merge(price, rf_data, join = "left")[, 2]
  rate_aligned <- zoo::na.locf(rate_aligned, na.rm = FALSE)
  annual_rate <- as.numeric(rate_aligned) / 100
  # Date i's financing uses only the Treasury yield observed through i-1.
  annual_rate <- c(NA_real_, head(annual_rate, -1L))
  missing_rf <- !is.finite(annual_rate)
  if (any(missing_rf)) {
    warning(sum(missing_rf), " initial/unavailable DTB3 observations use ",
            "fallback_cash_rate (not future FRED readings).")
    annual_rate[missing_rf] <- fallback_cash_rate
  }
  rf_daily <- annual_rate / 252
  cash_source <- "previous-observed FRED DTB3"
}

# SIGNAL INDICATORS: PAPER'S CLOSE-ONLY METHOD BY DEFAULT -------------------
# First day's daily close change is unavailable. Seed it with zero ONLY for
# computation; the warmup gate below forbids trading until this seed has
# rolled out of BOTH 20- and 40-day windows.
close_changes <- c(0, abs(diff(adj_close)))
close_change_xts <- xts::xts(close_changes, order.by = dates)

get_atr_estimate <- function(days) {
  if (atr_method == "paper_proxy") {
    as.numeric(TTR::runMean(close_change_xts, n = days)) * atr_proxy_multiplier
  } else {
    # Adjust high, low, close using the day's adjusted/raw-close factor.
    hlc <- xts::xts(cbind(adj_high, adj_low, adj_close), order.by = dates)
    colnames(hlc) <- c("High", "Low", "Close")
    as.numeric(TTR::ATR(hlc, n = days)[, "atr"])
  }
}

upper_donchian_input <- if (donchian_use_high_low) adj_high else adj_close
lower_donchian_input <- if (donchian_use_high_low) adj_low else adj_close
upper_donchian <- as.numeric(TTR::runMax(
  xts::xts(upper_donchian_input, order.by = dates), n = up_days))
lower_donchian <- as.numeric(TTR::runMin(
  xts::xts(lower_donchian_input, order.by = dates), n = down_days))
upper_keltner <- as.numeric(TTR::EMA(price, n = up_days)) +
  keltner_multiplier * get_atr_estimate(up_days)
lower_keltner <- as.numeric(TTR::EMA(price, n = down_days)) -
  keltner_multiplier * get_atr_estimate(down_days)
upper_band <- pmin(upper_donchian, upper_keltner)
lower_band <- pmax(lower_donchian, lower_keltner)
previous_upper <- c(NA_real_, head(upper_band, -1L))

# INDEPENDENT SIGNAL STATE MACHINE: LONG=1, FLAT=0 --------------------------
signal <- rep(0L, n)
trailing_stop <- rep(NA_real_, n)
event <- rep("", n)
ready <- (seq_len(n) > max(up_days, down_days) + 1L) &
  is.finite(previous_upper) & is.finite(lower_band) &
  is.finite(etf_return)
long <- FALSE
live_stop <- NA_real_
for (i in seq_len(n)) {
  if (!ready[i]) next
  if (!long) {
    if (adj_close[i] >= previous_upper[i]) {
      long <- TRUE
      live_stop <- lower_band[i]
      event[i] <- "ENTRY_SIGNAL"
    }
  } else {
    live_stop <- max(live_stop, lower_band[i])
    if (adj_close[i] <= live_stop) {
      long <- FALSE
      event[i] <- "EXIT_SIGNAL"
      live_stop <- NA_real_
    }
  }
  signal[i] <- as.integer(long)
  if (long) trailing_stop[i] <- live_stop
}

# CAUSAL DAILY-RESET NAV ENGINE --------------------------------------------
# Processing close i: first earn the close i-1 -> i return on LAST close's
# target, then trade to signal[i-1-extra_lag_days] at close i. The new
# target first earns the next close-to-close return, never a past return.
simulate <- function(desired_signal, delay = extra_lag_days) {
  stopifnot(length(desired_signal) == n, delay >= 0L)
  nav <- rep(NA_real_, n)
  realized <- rep(NA_real_, n)
  held_weight <- rep(NA_real_, n)
  posttrade_weight <- rep(NA_real_, n)
  trade_fraction <- rep(0, n)
  cost_dollars <- rep(0, n)
  trade_rows <- list()
  nav[1L] <- initial_capital
  realized[1L] <- 0
  posttrade_weight[1L] <- 0
  held_weight[1L] <- 0
  current_target <- 0
  row_number <- 0L

  for (i in 2:n) {
    held_weight[i] <- current_target
    r <- etf_return[i]
    rf <- rf_daily[i]
    financing <- (1 - current_target) * rf -
      max(current_target - 1, 0) * borrowing_spread / 252
    gross_factor <- 1 + current_target * r + financing
    if (!is.finite(gross_factor) || gross_factor <= 0) {
      stop("Portfolio NAV exhausted at ", dates[i], " with leverage=",
           leverage, ". Reduce leverage; a synthetic leveraged account ",
           "cannot automatically recover from bankruptcy.")
    }
    nav_before_cost <- nav[i - 1L] * gross_factor
    drift_weight <- current_target * (1 + r) / gross_factor

    origin <- i - 1L - delay
    next_target <- if (origin >= 1L) leverage * desired_signal[origin] else 0
    # Never trade into an indicator before it is warmed up.
    if (origin >= 1L && !ready[origin]) next_target <- 0
    if (!is.finite(next_target) || next_target < 0) stop("Invalid target weight")

    traded <- abs(next_target - drift_weight)
    cost <- nav_before_cost * traded * trade_bps / 10000
    nav[i] <- nav_before_cost - cost
    if (!is.finite(nav[i]) || nav[i] <= 0) {
      stop("NAV exhausted by transaction costs at ", dates[i])
    }
    realized[i] <- nav[i] / nav[i - 1L] - 1
    trade_fraction[i] <- traded
    cost_dollars[i] <- cost
    posttrade_weight[i] <- next_target

    if (traded > 1e-10) {
      row_number <- row_number + 1L
      kind <- if (current_target == 0 && next_target > 0) "BUY_ENTRY" else
        if (current_target > 0 && next_target == 0) "SELL_EXIT" else
          "DAILY_REBALANCE"
      trade_rows[[row_number]] <- data.frame(
        execution_date = dates[i],
        signal_date = if (origin >= 1L) dates[origin] else as.Date(NA),
        ticker = etf_ticker, action = kind,
        adjusted_close = adj_close[i],
        previous_target_leverage = current_target,
        pretrade_drift_leverage = drift_weight,
        new_target_leverage = next_target,
        traded_notional_fraction = traded,
        estimated_cost_dollars = cost,
        nav_after_cost = nav[i])
    }
    current_target <- next_target
  }

  list(nav = nav, returns = realized, held = held_weight,
       weight = posttrade_weight, turnover = trade_fraction,
       costs = cost_dollars,
       trades = if (length(trade_rows)) do.call(rbind, trade_rows) else
         data.frame(execution_date = as.Date(character()),
                    signal_date = as.Date(character()),
                    ticker = character(), action = character(),
                    adjusted_close = numeric(),
                    previous_target_leverage = numeric(),
                    pretrade_drift_leverage = numeric(),
                    new_target_leverage = numeric(),
                    traded_notional_fraction = numeric(),
                    estimated_cost_dollars = numeric(),
                    nav_after_cost = numeric()))
}

strategy <- simulate(signal)
constant_leverage <- simulate(rep(1L, n), delay = 0L)

# SAME-DATE BACKTESTS AND STATS ---------------------------------------------
valid_analysis <- which(dates >= backtest_start & dates <= backtest_end &
  ready & seq_len(n) > (2L + extra_lag_days))
if (length(valid_analysis) < 252L) {
  stop("Fewer than 252 valid backtest trading days. Move backtest_start ",
       "earlier or select an ETF with longer history.")
}
# Use one contiguous segment so all daily returns/benchmark dates match.
first_day <- min(valid_analysis)
last_day <- max(valid_analysis)
ix <- seq.int(first_day, last_day)
if (!all(ready[ix])) stop("Noncontiguous indicator history inside test window")

# Rebase NAVs to 1 at the close BEFORE the first evaluated return.
normalize_nav <- function(r) cumprod(1 + r[ix])
strategy_curve <- normalize_nav(strategy$returns)
constant_curve <- normalize_nav(constant_leverage$returns)
benchmark_curve <- normalize_nav(etf_return)
series <- xts::xts(
  cbind(Strategy = strategy$returns[ix],
        ETF_BuyHold = etf_return[ix],
        Constant_Leverage = constant_leverage$returns[ix]),
  order.by = dates[ix])
rf_series <- xts::xts(rf_daily[ix], order.by = dates[ix])

metric <- function(r, rf, held, turnover, costs) {
  years <- as.numeric(difftime(dates[ix[length(ix)]], dates[ix[1L]] - 1,
                               units = "days")) / 365.25
  growth <- prod(1 + r)
  equity <- cumprod(1 + r)
  previous_peaks <- cummax(c(1, equity))[-1L]
  drawdown <- equity / previous_peaks - 1
  volatility <- stats::sd(r) * sqrt(252)
  excess <- r - rf
  sharpe <- if (stats::sd(excess) > 0) {
    mean(excess) / stats::sd(excess) * sqrt(252)
  } else NA_real_
  c(CAGR = growth^(1 / years) - 1,
    AnnualVol = volatility,
    ExcessSharpe = sharpe,
    MaxDrawdown = min(drawdown),
    TimeInMarket = mean(held > 0),
    AvgGrossExposure = mean(abs(held)),
    AnnualTradedNotional = mean(turnover) * 252,
    SumTradeCostsUSD = sum(costs))
}
metrics <- rbind(
  Strategy = metric(strategy$returns[ix], rf_daily[ix],
                    strategy$held[ix], strategy$turnover[ix],
                    strategy$costs[ix] * initial_capital / strategy$nav[first_day - 1L]),
  ETF_BuyHold = metric(etf_return[ix], rf_daily[ix], rep(1, length(ix)),
                       rep(0, length(ix)), rep(0, length(ix))),
  Constant_Leverage = metric(constant_leverage$returns[ix], rf_daily[ix],
                             constant_leverage$held[ix],
                             constant_leverage$turnover[ix],
                             constant_leverage$costs[ix] * initial_capital /
                               constant_leverage$nav[first_day - 1L]))

cat("\n================ SINGLE ETF TREND-FOLLOWING BACKTEST ================\n")
cat("ETF: ", etf_ticker, " | Leverage when long: ", leverage,
    "x | ATR: ", atr_method, " | Additional lag: ", extra_lag_days,
    " trading days\n", sep = "")
cat("Test: ", as.character(dates[ix[1L]]), " through ",
    as.character(dates[ix[length(ix)]]), " (", length(ix), " trading days)\n", sep = "")
cat("Cash/borrow base: ", cash_source, "; annual borrow spread: ",
    borrowing_spread, "; trading cost: ", trade_bps, " bps\n", sep = "")
cat("NOTE: Original paper's diversified 48-industry risk sizing is NOT used.\n\n")
print(round(metrics, 4))
cat("\nStrategy entry signals: ", sum(event[ix] == "ENTRY_SIGNAL"),
    "; exit signals: ", sum(event[ix] == "EXIT_SIGNAL"), "\n", sep = "")
cat("Executed entry orders within test: ",
    sum(strategy$trades$action == "BUY_ENTRY" &
          strategy$trades$execution_date %in% dates[ix]),
    "; executed exit orders: ",
    sum(strategy$trades$action == "SELL_EXIT" &
          strategy$trades$execution_date %in% dates[ix]), "\n", sep = "")
cat("Latest close ", tail(dates, 1L), ": signal=",
    if (tail(signal, 1L) == 1L) "LONG" else "CASH",
    "; latest stop=", if (is.finite(tail(trailing_stop, 1L)))
      round(tail(trailing_stop, 1L), 4) else "NA",
    "; target exposure=", tail(signal, 1L) * leverage, "x\n", sep = "")

# Inspectable objects, not a claim that the system works live.
daily <- data.frame(
  date = dates[ix], adjusted_close = adj_close[ix],
  entry_band_previous_close = previous_upper[ix],
  exit_band = lower_band[ix], trailing_stop = trailing_stop[ix],
  signal_long = signal[ix], signal_event = event[ix],
  actual_held_leverage = strategy$held[ix],
  target_leverage_after_close = strategy$weight[ix],
  strategy_return = strategy$returns[ix],
  etf_return = etf_return[ix],
  constant_leverage_return = constant_leverage$returns[ix],
  risk_free_daily = rf_daily[ix],
  strategy_turnover = strategy$turnover[ix],
  strategy_cost_dollars = strategy$costs[ix] * initial_capital /
    strategy$nav[first_day - 1L],
  strategy_growth_of_1 = strategy_curve,
  etf_growth_of_1 = benchmark_curve,
  constant_leverage_growth_of_1 = constant_curve)
trade_log <- subset(strategy$trades,
                    execution_date >= dates[first_day] &
                      execution_date <= dates[last_day])
backtest <- list(settings = list(ticker = etf_ticker, leverage = leverage,
                                  atr_method = atr_method,
                                  extra_lag_days = extra_lag_days,
                                  trade_bps = trade_bps),
                 metrics = metrics, daily = daily, trades = trade_log,
                 signals = xts::xts(signal, order.by = dates),
                 returns = series, rf = rf_series,
                 full_simulation = strategy)

if (export_csv) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(daily, file.path(output_dir, "daily_backtest.csv"),
                   row.names = FALSE)
  utils::write.csv(trade_log, file.path(output_dir, "executed_trades.csv"),
                   row.names = FALSE)
  utils::write.csv(data.frame(Portfolio = rownames(metrics), metrics,
                              row.names = NULL),
                   file.path(output_dir, "performance_summary.csv"),
                   row.names = FALSE)
  message("CSV outputs saved to: ", normalizePath(output_dir))
}

if (show_charts) {
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(2, 1), mar = c(3, 4, 3, 1))
  matplot(dates[ix], cbind(strategy_curve, benchmark_curve, constant_curve),
          type = "l", lty = 1, lwd = 1.6, log = "y",
          col = c("#196D9A", "#777777", "#CC7A29"),
          main = paste(etf_ticker, "Concretum-style single-ETF strategy"),
          xlab = "", ylab = "Growth of $1 (log scale)")
  legend("topleft", c("Trend strategy", "ETF buy-and-hold",
                      paste0("Constant ", leverage, "x leverage")),
         col = c("#196D9A", "#777777", "#CC7A29"),
         lty = 1, bty = "n", cex = 0.8)
  plot(dates[ix], strategy$held[ix], type = "l", col = "#196D9A",
       main = "Actual ETF exposure during each close-to-close return",
       xlab = "Date", ylab = "Multiple of NAV")
  abline(h = 1, lty = 3)
  par(old_par)
}
