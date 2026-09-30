# Reddit RSI strategy — QQQ / TQQQ version
# Signals are formed at close t and implemented for the close-t to close-(t+1) return.
# The script reports synthetic and actual-ETF histories, prints the next-session target,
# and can create a whole-share trade list. It does not export strategy-output files.

rm(list = ls())
suppressPackageStartupMessages({library(quantmod); library(TTR); library(PerformanceAnalytics)})
options(timeout = 300)

### 1. USER SETTINGS ###
base_ticker <- "QQQ"
leveraged_ticker <- "TQQQ"
start_date <- "1993-01-01"
rsi_days <- 14
sma_days <- 200
rsi_buy_level <- 30
rsi_overheated_level <- 80
trade_bps <- 5                 # One-way trading cost applied to gross notional traded.
funding_spread <- 0.01         # Synthetic 3x financing spread over the T-bill rate.
synthetic_fee <- 0.0095        # Synthetic annual expense ratio.
publication_date <- as.Date("2025-12-15")
show_charts <- interactive()

# Set TRUE only after entering the complete holdings for the account running this strategy.
run_trade_list <- TRUE
current_shares <- c(Cash = 5000, TQQQ = 0, UVXY = 0, CTA = 0, GLD = 0) # Cash is dollars; others are shares.
cash_inflow <- 0
price_overrides <- NULL        # Optional named vector, e.g. c(TQQQ = 100.25).
prefer_live_quotes <- TRUE

### 2. DATA HELPERS ###
fetch_adjusted <- function(ticker, from = start_date) {
  message("Downloading history: ", ticker)
  x <- try(getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE, warnings = FALSE), silent = TRUE)
  if (inherits(x, "try-error") || !NROW(x)) stop("Historical download failed for ", ticker)
  out <- Ad(x); colnames(out) <- ticker; out
}

canonicalize_named <- function(x, label) {
  if (is.null(x)) return(numeric())
  if (!is.numeric(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!is.finite(x))) stop(label, " must be a finite named numeric vector")
  names(x) <- ifelse(tolower(names(x)) == "cash", "Cash", toupper(names(x)))
  summed <- tapply(x, names(x), sum); setNames(as.numeric(summed), names(summed))
}

symbols <- c(base_ticker, leveraged_ticker, "GLD", "CTA", "UVXY", "BIL")
prices_list <- setNames(lapply(symbols, fetch_adjusted), symbols)
tbill <- try(getSymbols("DTB3", src = "FRED", from = start_date, auto.assign = FALSE), silent = TRUE)
if (inherits(tbill, "try-error") || !NROW(tbill)) stop("FRED DTB3 download failed")

# Align everything to the base ETF calendar and stop at the last date shared by every downloaded ETF.
prices <- do.call(merge, unname(prices_list)); prices <- prices[index(prices_list[[base_ticker]])]
last_common <- as.Date(min(vapply(prices_list, function(x) as.numeric(last(index(x))), numeric(1))), origin = "1970-01-01")
prices <- zoo::na.locf(prices[index(prices) <= last_common], na.rm = FALSE)
returns <- prices / lag(prices) - 1

# Cash earns BIL when available and otherwise uses the lagged 3-month T-bill yield.
rate <- lag(zoo::na.locf(merge(prices[, 1], tbill, join = "left")[, 2], na.rm = FALSE), 1)
rf_daily <- as.numeric(rate) / 100 / 252
cash_return <- ifelse(is.finite(as.numeric(returns[, "BIL"])), as.numeric(returns[, "BIL"]), rf_daily)

# Synthetic leveraged return = 3x daily base return less borrowing and fund expenses.
base_return <- as.numeric(returns[, base_ticker])
synthetic_3x <- 3 * base_return - 2 * (rf_daily + funding_spread / 252) - synthetic_fee / 252
if (any(synthetic_3x[is.finite(synthetic_3x)] <= -1)) stop("Synthetic 3x series lost at least 100% in one day")

### 3. SIGNALS AND TARGET WEIGHTS ###
rsi <- as.numeric(RSI(prices[, base_ticker], n = rsi_days))
sma <- as.numeric(SMA(prices[, base_ticker], n = sma_days))
good <- which(is.finite(rsi) & is.finite(sma) & is.finite(as.numeric(prices[, "GLD"])) & is.finite(rf_daily))
if (!length(good)) stop("Not enough overlapping history to initialize the strategy")

state <- rep(NA_character_, NROW(prices))
state[good] <- ifelse(rsi[good] < rsi_buy_level, "Dip",
                      ifelse(rsi[good] > rsi_overheated_level, "Overheated",
                             ifelse(as.numeric(prices[good, base_ticker]) > sma[good], "Trend", "Defensive")))

# Signal weights are next-session targets. Backtest weights are lagged one trading day below.
signal_weights <- xts(matrix(NA_real_, NROW(prices), 5, dimnames = list(NULL, c("LEV", "UVXY", "CTA", "GLD", "CASH"))), order.by = index(prices))
signal_weights[good, ] <- 0
signal_weights[good, "LEV"] <- as.numeric(state[good] == "Dip") + as.numeric(state[good] == "Trend") / 3
signal_weights[good, "UVXY"] <- as.numeric(state[good] == "Overheated")
signal_weights[good, "CTA"] <- as.numeric(state[good] %in% c("Trend", "Defensive")) / 3
signal_weights[good, "GLD"] <- as.numeric(state[good] %in% c("Trend", "Defensive")) / 3
signal_weights[good, "CASH"] <- as.numeric(state[good] == "Defensive") / 3

# Before UVXY or CTA existed, redirect its intended weight to cash rather than inventing returns.
for (ticker in c("UVXY", "CTA")) {
  missing <- good[is.na(as.numeric(prices[good, ticker]))]
  if (length(missing)) { signal_weights[missing, "CASH"] <- signal_weights[missing, "CASH"] + signal_weights[missing, ticker]; signal_weights[missing, ticker] <- 0 }
}
weights <- lag(signal_weights, 1)

### 4. BACKTEST ENGINE ###
partial_start <- do.call(max, lapply(c(leveraged_ticker, "UVXY", "GLD", "BIL"), function(x) min(index(prices_list[[x]]))))
actual_start <- do.call(max, lapply(c(leveraged_ticker, "UVXY", "CTA", "GLD", "BIL"), function(x) min(index(prices_list[[x]]))))

run_backtest <- function(leveraged_return, first_day) {
  asset_returns <- xts(cbind(LEV = leveraged_return, UVXY = as.numeric(returns[, "UVXY"]), CTA = as.numeric(returns[, "CTA"]),
                              GLD = as.numeric(returns[, "GLD"]), CASH = cash_return), order.by = index(prices))
  rows <- which(index(prices) > first_day & is.finite(as.numeric(weights[, "LEV"])))
  if (!length(rows)) stop("No eligible backtest dates")
  wt <- as.matrix(weights[rows, ]); ar <- as.matrix(asset_returns[rows, ])
  if (any(wt > 0 & !is.finite(ar))) stop("An asset has a missing return while the strategy holds it")
  ar[!is.finite(ar)] <- 0

  # Estimate trade size from the prior portfolio after it drifts with the prior day's returns.
  prior_w <- as.matrix(lag(weights)[rows, ]); prior_r <- as.matrix(lag(asset_returns)[rows, ])
  prior_w[!is.finite(prior_w)] <- 0; prior_r[!is.finite(prior_r)] <- 0
  pretrade <- prior_w * (1 + prior_r); denominator <- rowSums(pretrade)
  pretrade <- pretrade / ifelse(denominator > 0, denominator, 1); pretrade[1, ] <- 0
  turnover <- rowSums(abs(wt - pretrade))
  strategy_return <- xts(rowSums(wt * ar) - turnover * trade_bps / 10000, order.by = index(prices)[rows])
  colnames(strategy_return) <- "Strategy"
  list(returns = strategy_return, turnover = xts(turnover, order.by = index(strategy_return)), weights = weights[rows, ])
}

extended <- run_backtest(synthetic_3x, index(prices)[good[1]])
partial_actual <- run_backtest(as.numeric(returns[, leveraged_ticker]), partial_start) # CTA allocation is cash before CTA inception.
actual <- run_backtest(as.numeric(returns[, leveraged_ticker]), actual_start)
synthetic_matched <- extended$returns[index(actual$returns)]
benchmark_extended <- returns[index(extended$returns), base_ticker]
benchmark_partial <- returns[index(partial_actual$returns), base_ticker]
benchmark_matched <- returns[index(actual$returns), base_ticker]

### 5. PERFORMANCE REPORT ###
cash_sharpe <- function(x) { excess <- as.numeric(x) - cash_return[match(index(x), index(prices))]; sqrt(252) * mean(excess) / sd(excess) }
metrics <- function(x) c(CAGR = 100 * as.numeric(Return.annualized(x, scale = 252)), Sharpe = cash_sharpe(x),
                         Vol = 100 * sd(as.numeric(x)) * sqrt(252), MaxDD = -100 * as.numeric(maxDrawdown(x)))
post <- actual$returns[paste0(publication_date, "/")]; benchmark_post <- benchmark_matched[paste0(publication_date, "/")]
results <- rbind("Synthetic 3x, extended" = metrics(extended$returns), "Buy & hold, extended" = metrics(benchmark_extended),
                 "Actual 3x/UVXY; pre-CTA cash" = metrics(partial_actual$returns), "Buy & hold, same early window" = metrics(benchmark_partial),
                 "Synthetic 3x, matched" = metrics(synthetic_matched), "Actual ETFs, matched" = metrics(actual$returns),
                 "Buy & hold, matched" = metrics(benchmark_matched))
if (NROW(post) > 10) results <- rbind(results, "Actual ETFs, post-publication" = metrics(post), "Buy & hold, post-publication" = metrics(benchmark_post))

cat("\n========== ", base_ticker, " / ", leveraged_ticker, " RESULTS ==========\n", sep = "")
cat("Extended:", as.character(first(index(extended$returns))), "to", as.character(last(index(extended$returns))),
    "| Full actual-ETF window:", as.character(first(index(actual$returns))), "to", as.character(last(index(actual$returns))), "\n")
print(round(results, 2))
cat("Mean daily gross traded notional, buys + sells:", round(mean(as.numeric(actual$turnover)) * 100, 2), "% of NAV\n")

if (show_charts) {
  long_chart <- merge(extended$returns, benchmark_extended, join = "inner"); colnames(long_chart) <- c("Synthetic 3x / proxy gaps", paste0(base_ticker, " buy & hold"))
  real_chart <- merge(synthetic_matched, actual$returns, benchmark_matched, join = "inner"); colnames(real_chart) <- c("Synthetic 3x", "Actual ETFs", paste0(base_ticker, " buy & hold"))
  charts.PerformanceSummary(long_chart, main = paste(base_ticker, "extended history"))
  charts.PerformanceSummary(real_chart, main = paste(base_ticker, "matched actual-ETF history"))
}

### 6. NEXT-SESSION TARGET ###
signal_date <- as.Date(last(index(signal_weights[complete.cases(signal_weights)])))
current_state <- as.character(last(na.omit(xts(state, order.by = index(prices)))))
model_target <- as.numeric(last(signal_weights[complete.cases(signal_weights)]))
names(model_target) <- c(leveraged_ticker, "UVXY", "CTA", "GLD", "Cash")
cat("\n========== NEXT-SESSION TARGET ==========\n")
cat("Signal date:", as.character(signal_date), "| State:", current_state, "\n")
print(round(model_target[model_target != 0], 4))
if (Sys.Date() - signal_date > 5) warning("The latest signal is more than five calendar days old")

### 7. OPTIONAL WHOLE-SHARE TRADE LIST ###
fetch_trade_price <- function(ticker, as_of = Sys.Date(), prefer_live = TRUE, override = NA_real_) {
  if (is.finite(override) && override > 0) return(c(price = override, price_date = as.character(as_of), source = "override"))
  now_et <- as.POSIXlt(Sys.time(), tz = "America/New_York")
  market_open <- as_of == Sys.Date() && now_et$wday %in% 1:5 && (now_et$hour > 9 || (now_et$hour == 9 && now_et$min >= 30)) && now_et$hour < 16
  if (prefer_live && market_open) {
    quote <- try(getQuote(ticker, src = "yahoo"), silent = TRUE)
    if (!inherits(quote, "try-error") && NROW(quote)) {
      price_col <- intersect(c("Last", "Price"), colnames(quote))
      if (length(price_col) && is.finite(as.numeric(quote[1, price_col[1]])) && as.numeric(quote[1, price_col[1]]) > 0)
        return(c(price = as.numeric(quote[1, price_col[1]]), price_date = as.character(as_of), source = "live"))
    }
    warning("Live quote unavailable for ", ticker, "; using the latest close")
  }
  x <- try(getSymbols(ticker, src = "yahoo", from = as_of - 14, to = as_of + 1, auto.assign = FALSE, warnings = FALSE), silent = TRUE)
  if (inherits(x, "try-error") || !NROW(x)) stop("Trade-price download failed for ", ticker)
  close <- Cl(x); close <- close[index(close) <= as_of]
  if (!NROW(close) || !is.finite(as.numeric(last(close))) || as.numeric(last(close)) <= 0) stop("No valid trade price for ", ticker)
  c(price = as.numeric(last(close)), price_date = as.character(last(index(close))), source = "close")
}

generate_trade_list <- function(target_w, holdings, cash_inflow = 0, as_of = Sys.Date(), overrides = NULL, prefer_live = TRUE) {
  target_w <- canonicalize_named(target_w, "target_w"); holdings <- canonicalize_named(holdings, "current_shares"); overrides <- canonicalize_named(overrides, "price_overrides")
  if (any(target_w < 0) || abs(sum(target_w) - 1) > 1e-8) stop("Target weights must be nonnegative and sum to 1")
  if (any(holdings < 0)) stop("Current shares must be nonnegative")
  universe <- union(names(target_w), names(holdings)); target <- current <- setNames(rep(0, length(universe)), universe)
  target[names(target_w)] <- target_w; current[names(holdings)] <- holdings
  if (!"Cash" %in% universe) { universe <- c("Cash", universe); target <- c(Cash = 0, target); current <- c(Cash = 0, current) }
  current["Cash"] <- current["Cash"] + cash_inflow
  if (current["Cash"] < 0) stop("cash_inflow exceeds available cash")
  keep <- target != 0 | current != 0 | universe == "Cash"; universe <- universe[keep]; target <- target[universe]; current <- current[universe]
  override_px <- setNames(rep(NA_real_, length(universe)), universe); override_px[intersect(names(overrides), universe)] <- overrides[intersect(names(overrides), universe)]
  info <- lapply(universe, function(x) if (x == "Cash") c(price = 1, price_date = as.character(as_of), source = "cash") else fetch_trade_price(x, as_of, prefer_live, override_px[x]))
  prices_now <- setNames(as.numeric(vapply(info, `[[`, character(1), "price")), universe)
  total_value <- sum(current * prices_now)
  if (!is.finite(total_value) || total_value <= 0) stop("Account value must be positive")
  target_shares <- floor(total_value * target / prices_now); noncash <- universe != "Cash"
  target_shares["Cash"] <- total_value - sum(target_shares[noncash] * prices_now[noncash])
  trade_shares <- target_shares - current
  action <- ifelse(universe == "Cash", "CASH", ifelse(trade_shares > 0, "BUY", ifelse(trade_shares < 0, "SELL", "HOLD")))
  out <- data.frame(ticker = universe, action, price = round(prices_now, 4), price_date = vapply(info, `[[`, character(1), "price_date"),
                    price_source = vapply(info, `[[`, character(1), "source"), current_shares = as.numeric(current), target_weight = round(target, 6),
                    target_shares = as.numeric(target_shares), trade_shares = as.numeric(trade_shares),
                    trade_value = round(as.numeric(trade_shares * prices_now), 2), stringsAsFactors = FALSE)
  out <- out[out$trade_shares != 0 | out$ticker == "Cash", , drop = FALSE]; rownames(out) <- NULL
  attr(out, "account_value") <- total_value; out
}

trade_list <- NULL
if (run_trade_list) {
  trade_list <- generate_trade_list(model_target, current_shares, cash_inflow, Sys.Date(), price_overrides, prefer_live_quotes)
  cat("\n========== TRADE LIST ==========\n")
  cat("Account value: $", format(round(attr(trade_list, "account_value"), 2), big.mark = ",", nsmall = 2), "\n", sep = "")
  print(trade_list, row.names = FALSE)
} else {
  cat("\nTrade-list generation is OFF. Enter complete current_shares and set run_trade_list <- TRUE when ready.\n")
}

# Main objects retained in memory for further analysis; nothing is written to strategy_outputs.
backtest <- list(metrics = results, extended = extended, partial_actual = partial_actual, actual = actual,
                 synthetic_matched = synthetic_matched, benchmark = benchmark_matched,
                 signals = xts(state, order.by = index(prices)), signal_weights = signal_weights,
                 prices = prices, cash_returns = xts(cash_return, order.by = index(prices)))

