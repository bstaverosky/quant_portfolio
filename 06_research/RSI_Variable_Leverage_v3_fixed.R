# Reddit RSI: variable exposure to a single equity ETF (QQQ by default).
# No GLD, CTA, UVXY or defensive ETF rotation. Uses QQQ, optional TQQQ and cash.
# Signals at close t; lagged_days=0 enters at close t, earning the NEXT session return.
# lagged_days=1 enters at close t+1, earning the return from t+1 to t+2.
# Synthetic 3x is a DAILY-RESET proxy, not a historical traded instrument.
# Nothing is exported to strategy_outputs; all results stay in memory.

rm(list = ls())
suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(PerformanceAnalytics)
})
options(timeout = 300)

### 1. SETTINGS: CHANGE THESE ###
base_ticker <- "QQQ"           # E.g., "SPY"; signals are calculated on THIS ETF.
leveraged_ticker <- "TQQQ"      # Matching DAILY 3x ETF: QQQ -> TQQQ; SPY -> UPRO.
start_date <- "1993-01-01"
rsi_days <- 14L
sma_days <- 200L
rsi_buy_level <- 30
rsi_overheated_level <- 80

# Effective DAILY equity exposure for each RSI / trend state (0x to 3x).
leverage_oversold   <- 3.00     # RSI < 30
leverage_bull       <- 0  # RSI 30..80, price > 200-day SMA
leverage_bear       <- 0     # RSI 30..80, price <= 200-day SMA
leverage_overbought <- 0.00     # RSI > 80; no volatility ETFs

lagged_days <- 0               # EXTRA trading days late: 0 = original, 1 = 1 day late.
compare_lags <- TRUE            # Compare lagged_days against zero EXTRA delay on same dates.
trade_bps <- 5                  # One-way transaction cost, basis points of gross dollars traded.
funding_spread <- 0.01          # Synthetic borrowing spread OVER lagged T-bill yield, annual.
synthetic_3x_fee <- 0.0095      # Synthetic 3x ETF fee, annual; QQQ fee already in QQQ returns.
show_charts <- interactive()

# Trade list describes the REAL-ETF implementation, not a trade in a synthetic asset.
run_trade_list <- FALSE         # Fill in ALL holdings before enabling.
current_shares <- c(Cash = 5000, QQQ = 0, TQQQ = 0) # Cash is dollars; ETFs are shares.
cash_inflow <- 0
price_overrides <- NULL        # Named LIVE trade-price overrides, e.g. c(QQQ=600,TQQQ=100).

stopifnot(is.character(base_ticker), length(base_ticker) == 1L, nzchar(base_ticker),
          is.character(leveraged_ticker), length(leveraged_ticker) == 1L,
          nzchar(leveraged_ticker), base_ticker != leveraged_ticker)
exposures <- c(Oversold = leverage_oversold, Bull = leverage_bull,
               Bear = leverage_bear, Overbought = leverage_overbought)
if (any(!is.finite(exposures)) || any(exposures < 0 | exposures > 3))
  stop("Each leverage setting must be between 0 and 3 (long-only QQQ / 3x ETF / cash).")
if (length(lagged_days) != 1L || !is.numeric(lagged_days) ||
    !is.finite(lagged_days) || lagged_days < 0 || lagged_days != floor(lagged_days))
  stop("lagged_days must be a nonnegative whole number.")
lagged_days <- as.integer(lagged_days)
if (!is.finite(trade_bps) || trade_bps < 0 || !is.finite(funding_spread) ||
    funding_spread < 0 || !is.finite(synthetic_3x_fee) || synthetic_3x_fee < 0)
  stop("Costs must be finite and nonnegative.")

### 2. PRICES, RATE AND DAILY RETURNS ###
fetch_adjusted <- function(ticker) {
  message("Downloading ", ticker)
  raw <- try(getSymbols(ticker, src = "yahoo", from = start_date,
                        auto.assign = FALSE, warnings = FALSE), silent = TRUE)
  if (inherits(raw, "try-error") || !NROW(raw)) stop("Yahoo download failed: ", ticker)
  p <- Ad(raw); colnames(p) <- ticker
  p
}
base_prices <- fetch_adjusted(base_ticker)
leveraged_prices <- try(fetch_adjusted(leveraged_ticker), silent = TRUE)
have_actual <- !inherits(leveraged_prices, "try-error")
if (!have_actual) warning("3x ETF unavailable; running synthetic backtest only.")

tbill <- try(getSymbols("DTB3", src = "FRED", from = start_date,
                        auto.assign = FALSE), silent = TRUE)
if (inherits(tbill, "try-error") || !NROW(tbill))
  stop("FRED DTB3 unavailable: needed for historical cash and synthetic financing.")

# QQQ (or chosen underlying) defines the trading calendar. No pre-inception backfill.
prices <- base_prices
if (have_actual) prices <- merge(prices, leveraged_prices, join = "left")
underlying_return <- as.numeric(prices[, base_ticker] / lag(prices[, base_ticker], 1) - 1)

# FRED DTB3 is an annualized bank-discount quote, used as an APPROXIMATE cash yield.
# Last available quote is lagged one trading session to avoid using today's future rate.
rate_aligned <- merge(prices[, base_ticker], tbill, join = "left")[, 2]
rate_aligned <- zoo::na.locf(rate_aligned, na.rm = FALSE)
rf_daily <- as.numeric(lag(rate_aligned, k = 1L)) / 100 / 252
cash_return <- rf_daily

# Synthetic TQQQ / UPRO: daily 3x underlying less financing on 2 borrowed dollars
# and a separate modeled 3x fund expense. No post-hoc 3x scaling of CAGR.
synthetic_3x <- 3 * underlying_return -
  2 * (rf_daily + funding_spread / 252) - synthetic_3x_fee / 252
if (any(synthetic_3x[is.finite(synthetic_3x)] <= -1))
  stop("Synthetic 3x return reached -100%; proxy needs a more realistic failure model.")
actual_3x <- if (have_actual) {
  as.numeric(prices[, leveraged_ticker] / lag(prices[, leveraged_ticker], 1) - 1)
} else {
  rep(NA_real_, NROW(prices))
}

### 3. SIGNALS AND REAL-ETF TARGET WEIGHTS ###
rsi <- as.numeric(RSI(prices[, base_ticker], n = rsi_days))
sma <- as.numeric(SMA(prices[, base_ticker], n = sma_days))
p <- as.numeric(prices[, base_ticker])
good <- which(is.finite(rsi) & is.finite(sma) & is.finite(rf_daily) &
              is.finite(underlying_return))
if (!length(good)) stop("Not enough prices and T-bill data for a valid signal.")

state <- rep(NA_character_, NROW(prices))
state[good] <- ifelse(rsi[good] < rsi_buy_level, "Oversold",
               ifelse(rsi[good] > rsi_overheated_level, "Overbought",
               ifelse(p[good] > sma[good], "Bull", "Bear")))
leverage_signal <- xts(rep(NA_real_, NROW(prices)), order.by = index(prices))
colnames(leverage_signal) <- "TargetLeverage"
leverage_signal[good, 1] <- unname(exposures[state[good]])

# Exactly implement the effective exposure via REAL tradable ETF weights:
#   0 <= L <= 1: L * QQQ + (1-L) * cash
#   1 <  L <= 3: ((3-L)/2) * QQQ + ((L-1)/2) * TQQQ
# This is a DAILY-REBALANCED mix: 1.10x = 95% QQQ + 5% TQQQ.
# Even the synthetic test uses the same instrument weights, replacing ONLY TQQQ's
# daily return with the synthetic 3x proxy for longer historical coverage.
exposure_to_weights <- function(L) {
  if (any(!is.finite(L) | L < 0 | L > 3)) stop("Exposure outside [0, 3].")
  cbind(UNDERLYING = ifelse(L <= 1, L, (3 - L) / 2),
        THREE_X = ifelse(L <= 1, 0, (L - 1) / 2),
        CASH = ifelse(L <= 1, 1 - L, 0))
}
signal_weights <- xts(matrix(NA_real_, NROW(prices), 3,
                             dimnames = list(NULL, c("UNDERLYING", "THREE_X", "CASH"))),
                      order.by = index(prices))
signal_weights[good, ] <- exposure_to_weights(as.numeric(leverage_signal[good, ]))
if (any(abs(rowSums(as.matrix(signal_weights[good, ])) - 1) > 1e-10))
  stop("Signal portfolio weights do not sum to 100%.")

# The close-t signal earns the close-t -> close-(t+1) return. An EXTRA delay of
# one trading day therefore uses lag k=2, NOT k=1.
weights <- lag(signal_weights, k = 1L + lagged_days)
weights_on_time <- lag(signal_weights, k = 1L)

### 4. BACKTEST ENGINE ###
asset_return_matrix <- function(three_x) {
  xts(cbind(UNDERLYING = underlying_return, THREE_X = three_x,
            CASH = cash_return), order.by = index(prices))
}

run_backtest <- function(three_x, holding_weights = weights,
                         earliest = as.Date("1900-01-01"),
                         latest = as.Date("9999-12-31")) {
  asset_returns <- asset_return_matrix(three_x)
  rows <- which(index(prices) >= earliest & index(prices) <= latest &
                is.finite(as.numeric(holding_weights[, "UNDERLYING"])) &
                is.finite(underlying_return) & is.finite(cash_return))
  if (!length(rows)) stop("No eligible dates for this backtest.")
  wt <- as.matrix(holding_weights[rows, ])
  ar <- as.matrix(asset_returns[rows, ])
  if (any(wt > 1e-12 & !is.finite(ar)))
    stop("Missing return on a held asset; check ETF inception and data gaps.")
  ar[!is.finite(ar)] <- 0  # An unavailable asset must have ZERO weight.

  # Calculate pre-trade weights from prior day's portfolio AFTER its returns.
  prior_w <- as.matrix(lag(holding_weights, k = 1L)[rows, ])
  prior_r <- as.matrix(lag(asset_returns, k = 1L)[rows, ])
  prior_w[!is.finite(prior_w)] <- 0
  prior_r[!is.finite(prior_r)] <- 0
  pretrade <- prior_w * (1 + prior_r)
  denom <- rowSums(pretrade)
  if (any(denom[-1] <= 0)) stop("Prior portfolio wealth is nonpositive.")
  pretrade <- pretrade / ifelse(denom > 0, denom, 1)
  pretrade[1, ] <- c(0, 0, 1) # Begin the backtest in cash; charge initial entry.
  turnover <- rowSums(abs(wt - pretrade))  # Buys + sells as a fraction of NAV.
  gross <- rowSums(wt * ar)
  net <- gross - trade_bps / 10000 * turnover
  if (any(!is.finite(net) | net <= -1)) stop("Invalid or bankrupt daily strategy return.")
  out <- xts(net, order.by = index(prices)[rows]); colnames(out) <- "Strategy"
  list(returns = out,
       turnover = xts(turnover, order.by = index(out)),
       weights = holding_weights[rows, ],
       exposure = xts(as.numeric(holding_weights[rows, "UNDERLYING"]) +
                      3 * as.numeric(holding_weights[rows, "THREE_X"]),
                      order.by = index(out)))
}

extended <- run_backtest(synthetic_3x)
actual <- NULL
synthetic_matched <- NULL
actual_start <- as.Date(NA)
if (have_actual) {
  valid_actual <- which(is.finite(actual_3x) & is.finite(underlying_return) &
                        is.finite(cash_return) &
                        is.finite(as.numeric(weights[, "UNDERLYING"])))
  if (length(valid_actual)) {
    actual_start <- as.Date(index(prices)[valid_actual[1]])
    actual_end <- as.Date(index(prices)[tail(valid_actual, 1L)])
    actual <- run_backtest(actual_3x, earliest = actual_start, latest = actual_end)
    # Re-run BOTH on identical actual dates with identical starting cash and costs.
    synthetic_matched <- run_backtest(synthetic_3x, earliest = actual_start,
                                     latest = actual_end)
    if (!identical(index(actual$returns), index(synthetic_matched$returns)))
      stop("Synthetic / actual matched dates do not align.")
  } else warning("No usable actual 3x ETF returns; synthetic-only results.")
}

### 5. RESULTS AND SAME-DATE DELAY EXPERIMENT ###
cash_sharpe <- function(x) {
  rf <- cash_return[match(index(x), index(prices))]
  excess <- as.numeric(x) - rf
  if (length(excess) < 2L || !is.finite(sd(excess)) || sd(excess) == 0) return(NA_real_)
  sqrt(252) * mean(excess) / sd(excess)
}
metrics <- function(x) {
  c(CAGR = 100 * as.numeric(Return.annualized(x, scale = 252)),
    Sharpe = cash_sharpe(x),
    Vol = 100 * sd(as.numeric(x)) * sqrt(252),
    MaxDD = -100 * as.numeric(maxDrawdown(x)))
}
bench <- function(dates) {
  x <- xts(underlying_return[match(dates, index(prices))], order.by = dates)
  colnames(x) <- base_ticker
  x
}
results <- rbind("Synthetic (extended)" = metrics(extended$returns),
                 "Buy & hold (same dates)" = metrics(bench(index(extended$returns))))
if (!is.null(actual)) {
  results <- rbind(results,
                   "Synthetic (matched)" = metrics(synthetic_matched$returns),
                   "Actual ETFs (matched)" = metrics(actual$returns),
                   "Buy & hold (matched)" = metrics(bench(index(actual$returns))))
}
cat("\n========== RSI VARIABLE LEVERAGE: ", base_ticker, " =========\n", sep = "")
cat("Leverages:", paste(names(exposures), exposures, collapse = ", "),
    "| EXTRA delay:", lagged_days, "trading days\n")
cat("Synthetic:", as.character(first(index(extended$returns))), "to",
    as.character(last(index(extended$returns))), "\n")
if (!is.null(actual))
  cat("Actual ETF matched:", as.character(first(index(actual$returns))), "to",
      as.character(last(index(actual$returns))), "\n")
print(round(results, 2))
cat("Mean daily gross turnover (synthetic, buys+sells):",
    round(100 * mean(as.numeric(extended$turnover)), 3), "% NAV\n")

lag_comparison <- NULL
if (compare_lags && lagged_days > 0L) {
  compare_pair <- function(on_time, delayed) {
    x <- merge(on_time$returns, delayed$returns, join = "inner")
    colnames(x) <- c("On-time (0 extra)", paste0("Delayed (+", lagged_days, ")"))
    x[complete.cases(x)]
  }
  # Re-start BOTH variants on the same first delayed date, from cash,
  # to avoid an artificial one-time initial-cost difference.
  on_time_extended <- run_backtest(synthetic_3x, weights_on_time,
                                   earliest = as.Date(first(index(extended$returns))))
  lag_comparison <- list(synthetic = compare_pair(on_time_extended, extended))
  if (!is.null(actual)) {
    on_time_actual <- run_backtest(actual_3x, weights_on_time,
                                  earliest = as.Date(first(index(actual$returns))),
                                  latest = as.Date(last(index(actual$returns))))
    lag_comparison$actual <- compare_pair(on_time_actual, actual)
  }
  for (label in names(lag_comparison)) {
    x <- lag_comparison[[label]]
    cat("\n========== SAME-DATE EXTRA LAG: ", label, " =========\n", sep = "")
    cat(as.character(first(index(x))), "to", as.character(last(index(x))), "\n")
    print(round(rbind("On-time" = metrics(x[, 1]),
                      "Delayed" = metrics(x[, 2])), 2))
  }
}

if (show_charts) {
  x <- merge(extended$returns, bench(index(extended$returns)), join = "inner")
  colnames(x) <- c("Synthetic", paste0(base_ticker, " buy & hold"))
  charts.PerformanceSummary(x, main = paste(base_ticker, "variable-leverage synthetic"))
  if (!is.null(actual)) {
    x <- merge(synthetic_matched$returns, actual$returns,
               bench(index(actual$returns)), join = "inner")
    colnames(x) <- c("Synthetic", "Actual ETFs", paste0(base_ticker, " buy & hold"))
    charts.PerformanceSummary(x, main = paste(base_ticker, "matched actual / synthetic"))
  }
  if (!is.null(lag_comparison))
    charts.PerformanceSummary(lag_comparison$synthetic,
                              main = paste(base_ticker, "execution lag sensitivity"))
}

### 6. NEXT-SESSION TARGET: EXTRA DELAY APPLIED ###
latest_row <- tail(which(complete.cases(signal_weights)), 1L)
executed_row <- latest_row - lagged_days
if (executed_row < good[1]) stop("Not enough history for the next-session lag.")
latest_signal_date <- as.Date(index(signal_weights)[latest_row])
executed_signal_date <- as.Date(index(signal_weights)[executed_row])
current_state <- state[executed_row]
model_leverage <- as.numeric(leverage_signal[executed_row, 1])
model_target <- as.numeric(signal_weights[executed_row, ])
names(model_target) <- c(base_ticker, leveraged_ticker, "Cash")
cat("\n========== NEXT-SESSION TARGET =========\n")
cat("Latest data:", as.character(latest_signal_date),
    "| Used signal:", as.character(executed_signal_date),
    "| State:", current_state, "| Effective exposure:", model_leverage, "x\n")
print(round(model_target[model_target > 0], 5))
if (Sys.Date() - latest_signal_date > 5)
  warning("Latest closing signal is more than five calendar days old.")

### 7. OPTIONAL WHOLE-SHARE TRADE LIST (REAL ETFs) ###
canonicalize_named <- function(x) {
  if (is.null(x)) return(numeric())
  if (!is.numeric(x) || is.null(names(x)) || any(!nzchar(names(x))) ||
      any(!is.finite(x))) stop("Holdings / overrides require finite named numbers.")
  names(x) <- ifelse(tolower(names(x)) == "cash", "Cash", toupper(names(x)))
  summed <- tapply(x, names(x), sum)
  setNames(as.numeric(summed), names(summed))
}
fetch_trade_price <- function(ticker) {
  x <- getSymbols(ticker, src = "yahoo", from = Sys.Date() - 21,
                  auto.assign = FALSE, warnings = FALSE)
  close <- Cl(x)
  if (!NROW(close) || !is.finite(as.numeric(last(close))) ||
      as.numeric(last(close)) <= 0) stop("No recent trade price: ", ticker)
  c(price = as.character(as.numeric(last(close))),
    price_date = as.character(last(index(close))), source = "latest_close")
}
generate_trade_list <- function(target_w, holdings, inflow = 0, overrides = NULL) {
  target_w <- canonicalize_named(target_w)
  holdings <- canonicalize_named(holdings)
  overrides <- canonicalize_named(overrides)
  if (any(target_w < 0) || abs(sum(target_w) - 1) > 1e-8)
    stop("Target weights must be nonnegative and add up to one.")
  if (any(holdings < 0) || !is.finite(inflow)) stop("Invalid shares or inflow.")
  universe <- unique(c(names(holdings), names(target_w), "Cash"))
  target <- current <- setNames(rep(0, length(universe)), universe)
  target[names(target_w)] <- target_w
  current[names(holdings)] <- holdings
  current["Cash"] <- current["Cash"] + inflow
  if (current["Cash"] < 0) stop("Negative cash after inflow.")
  keep <- target > 0 | current > 0 | names(target) == "Cash"
  target <- target[keep]; current <- current[keep]
  symbols <- setdiff(names(target), "Cash")
  info <- lapply(symbols, function(s) {
    if (s %in% names(overrides)) {
      if (overrides[s] <= 0) stop("Invalid price override: ", s)
      c(price = as.character(overrides[s]), price_date = as.character(Sys.Date()),
        source = "override")
    } else fetch_trade_price(s)
  })
  names(info) <- symbols
  px <- setNames(rep(1, length(target)), names(target))
  if (length(symbols)) px[symbols] <- vapply(info, function(z) as.numeric(z["price"]), numeric(1))
  account_value <- sum(current * px)
  if (!is.finite(account_value) || account_value <= 0) stop("Account value must be positive.")
  desired <- current
  if (length(symbols)) desired[symbols] <- floor(account_value * target[symbols] / px[symbols])
  desired["Cash"] <- account_value - sum(desired[symbols] * px[symbols])
  trades <- desired - current
  out <- data.frame(ticker = names(target),
                    action = ifelse(names(target) == "Cash", "CASH",
                                    ifelse(trades > 0, "BUY", ifelse(trades < 0, "SELL", "HOLD"))),
                    price = as.numeric(px), current_shares = as.numeric(current),
                    target_weight = as.numeric(target), target_shares = as.numeric(desired),
                    trade_shares = as.numeric(trades), trade_value = as.numeric(trades * px),
                    stringsAsFactors = FALSE)
  out <- out[out$trade_shares != 0 | out$ticker == "Cash", , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "account_value") <- account_value
  out
}
trade_list <- NULL
if (run_trade_list) {
  if (!have_actual || is.null(actual)) stop("Live trade list needs an available 3x ETF.")
  if (!all(c("Cash", base_ticker, leveraged_ticker) %in% names(current_shares)))
    stop("Update current_shares with Cash, base_ticker, and leveraged_ticker.")
  trade_list <- generate_trade_list(model_target, current_shares, cash_inflow, price_overrides)
  cat("\n========== WHOLE-SHARE TRADE LIST =========\n")
  cat("Account value: $", format(round(attr(trade_list, "account_value"), 2),
                                  big.mark = ",", nsmall = 2), "\n", sep = "")
  print(trade_list, row.names = FALSE)
} else cat("\nTrade list OFF. Update current_shares and enable it when ready.\n")

# Useful objects remain in R's workspace. Nothing is written to disk.
backtest <- list(metrics = results, exposures = exposures, lagged_days = lagged_days,
                 extended = extended, actual = actual, synthetic_matched = synthetic_matched,
                 lag_comparison = lag_comparison, signals = xts(state, order.by = index(prices)),
                 signal_leverage = leverage_signal, signal_weights = signal_weights,
                 holding_weights = weights, prices = prices, cash_returns =
                   xts(cash_return, order.by = index(prices)), trade_list = trade_list)
