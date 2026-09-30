rm(list = ls())

suppressPackageStartupMessages({
  library(quantmod)
  library(PerformanceAnalytics)
})

options(timeout = 300)

### CONFIGURATION ###
# Change weights or add/remove ETFs here. Weights must sum to 1.
target_weights <- c(QQWZ = 0.35, BUL = 0.25, MTUM = 0.20, CALF = 0.10, XMHQ = 0.10)

benchmark <- "SPY"
backtest_start <- "2000-01-01"
backtest_rebalance <- "years"      # "days", "weeks", "months", "quarters", "years", or "none"
risk_free_rate <- 0
show_charts <- interactive()

# Tax-aware live execution. cash_only makes no sales; full_rebalance trades directly to target.
execution_mode <- "full_rebalance"      # "cash_only" or "full_rebalance"
no_trade_band <- 0.02              # Absolute weight band; used by cash_only (0.02 = 2 percentage points)
cash_inflow <- 0
rebalance_date <- Sys.Date()
prefer_live_quotes <- TRUE
price_overrides <- NULL             # Optional named vector, e.g. c(QQWZ = 55.25)
trade_output_file <- NULL           # Optional CSV path; NULL prints only

# Cash is dollars; everything else is shares. Include every non-target holding if it should count in account value.
current_shares <- c(Cash = 425, 
                    QQWZ = 0, 
                    BUL = 54, 
                    MTUM = 0, 
                    CALF = 0, 
                    XMHQ = 26,
                    SPY = 3,
                    PAMC = 55,
                    XSVM = 48)

### VALIDATION AND HELPERS ###
canonicalize_named <- function(x, label) {
  if (is.null(x)) return(numeric())
  if (!is.numeric(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!is.finite(x))) stop(label, " must be a finite named numeric vector")
  names(x) <- ifelse(tolower(names(x)) == "cash", "Cash", toupper(names(x)))
  summed <- tapply(x, names(x), sum)
  setNames(as.numeric(summed), names(summed))
}

target_weights <- canonicalize_named(target_weights, "target_weights")
if ("Cash" %in% names(target_weights)) stop("Do not include Cash in target_weights; residual cash is handled automatically")
if (any(target_weights < 0) || abs(sum(target_weights) - 1) > 1e-8) stop("target_weights must be nonnegative and sum to exactly 1; current sum is ", sum(target_weights))
if (!execution_mode %in% c("cash_only", "full_rebalance")) stop("execution_mode must be cash_only or full_rebalance")
if (!backtest_rebalance %in% c("days", "weeks", "months", "quarters", "years", "none")) stop("Invalid backtest_rebalance")
if (!is.numeric(no_trade_band) || length(no_trade_band) != 1 || !is.finite(no_trade_band) || no_trade_band < 0 || no_trade_band >= 1) stop("no_trade_band must be between 0 and 1")

download_adjusted_prices <- function(tickers, from) {
  out <- lapply(tickers, function(ticker) {
    message("Downloading history: ", ticker)
    x <- try(getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE, warnings = FALSE), silent = TRUE)
    if (inherits(x, "try-error") || !nrow(x)) stop("Historical download failed for ", ticker)
    px <- zoo::na.locf(Ad(x), na.rm = FALSE)
    colnames(px) <- ticker
    px
  })
  prices <- do.call(merge, out)
  prices[complete.cases(prices)]
}

performance_table <- function(x, rf = 0) {
  metric <- function(fun, ...) as.numeric(fun(x, ...))
  out <- rbind(CAGR = metric(Return.annualized), Volatility = metric(StdDev.annualized),
               Sharpe = metric(SharpeRatio.annualized, Rf = rf), Sortino = metric(SortinoRatio, MAR = rf),
               `Max Drawdown` = -metric(maxDrawdown), Calmar = metric(CalmarRatio))
  colnames(out) <- colnames(x)
  round(out, 3)
}

fetch_trade_price <- function(ticker, as_of = Sys.Date(), prefer_live = TRUE, override = NA_real_) {
  if (is.finite(override) && override > 0) return(c(price = override, price_date = as.character(as_of), source = "override"))
  now_et <- as.POSIXlt(Sys.time(), tz = "America/New_York")
  market_open <- as_of == Sys.Date() && now_et$wday %in% 1:5 && (now_et$hour > 9 || (now_et$hour == 9 && now_et$min >= 30)) && now_et$hour < 16
  if (prefer_live && market_open) {
    quote <- try(getQuote(ticker, src = "yahoo"), silent = TRUE)
    if (!inherits(quote, "try-error") && nrow(quote)) {
      price_col <- intersect(c("Last", "Price"), colnames(quote))
      if (length(price_col) && is.finite(as.numeric(quote[1, price_col[1]])) && as.numeric(quote[1, price_col[1]]) > 0)
        return(c(price = as.numeric(quote[1, price_col[1]]), price_date = as.character(as_of), source = "live"))
    }
    warning("Live quote unavailable for ", ticker, "; using the latest close")
  }
  x <- try(getSymbols(ticker, src = "yahoo", from = as_of - 14, to = as_of + 1, auto.assign = FALSE, warnings = FALSE), silent = TRUE)
  if (inherits(x, "try-error") || !nrow(x)) stop("Price download failed for ", ticker)
  px <- Cl(x); px <- px[index(px) <= as_of]
  if (!nrow(px) || !is.finite(as.numeric(last(px))) || as.numeric(last(px)) <= 0) stop("No valid price for ", ticker, " on or before ", as_of)
  c(price = as.numeric(last(px)), price_date = as.character(last(index(px))), source = "close")
}

generate_trade_list <- function(target_w, current_shares, cash_inflow = 0, as_of = Sys.Date(), mode = "cash_only",
                                no_trade_band = 0.02, price_overrides = NULL, prefer_live = TRUE, nonzero_only = TRUE) {
  target_w <- canonicalize_named(target_w, "target_w")
  current_shares <- canonicalize_named(current_shares, "current_shares")
  price_overrides <- canonicalize_named(price_overrides, "price_overrides")
  if (any(current_shares < 0)) stop("current_shares must be nonnegative")
  if (!is.numeric(cash_inflow) || length(cash_inflow) != 1 || !is.finite(cash_inflow)) stop("cash_inflow must be one finite number")

  universe <- union(c("Cash", names(target_w)), names(current_shares))
  target <- setNames(rep(0, length(universe)), universe); target[names(target_w)] <- target_w
  current <- setNames(rep(0, length(universe)), universe); current[names(current_shares)] <- current_shares
  current["Cash"] <- current["Cash"] + cash_inflow
  if (current["Cash"] < 0) stop("cash_inflow exceeds available cash")
  keep <- target != 0 | current != 0 | universe == "Cash"
  universe <- universe[keep]; target <- target[universe]; current <- current[universe]
  overrides <- setNames(rep(NA_real_, length(universe)), universe)
  overrides[intersect(names(price_overrides), universe)] <- price_overrides[intersect(names(price_overrides), universe)]

  info <- lapply(universe, function(ticker) {
    if (ticker == "Cash") return(c(price = 1, price_date = as.character(as_of), source = "cash"))
    message("Downloading trade price: ", ticker)
    fetch_trade_price(ticker, as_of, prefer_live, overrides[ticker])
  })
  prices <- setNames(as.numeric(vapply(info, `[[`, character(1), "price")), universe)
  price_dates <- vapply(info, `[[`, character(1), "price_date")
  sources <- vapply(info, `[[`, character(1), "source")
  current_value <- current * prices
  total_value <- sum(current_value)
  if (!is.finite(total_value) || total_value <= 0) stop("Account value must be positive after cash_inflow")

  target_dollars <- total_value * target
  target_shares <- current
  if (mode == "full_rebalance") {
    noncash <- universe != "Cash"
    target_shares[noncash] <- floor(target_dollars[noncash] / prices[noncash])
    target_shares["Cash"] <- total_value - sum(target_shares[noncash] * prices[noncash])
    warning("full_rebalance may realize taxable gains; review every sell against cost basis and holding period before trading")
  } else {
    tickers <- names(target_w)
    current_weight <- current_value / total_value
    eligible <- tickers[current_weight[tickers] < target_w[tickers] - no_trade_band]
    deficits <- pmax(target_dollars[eligible] - current_value[eligible], 0)
    budget <- max(current["Cash"], 0)
    buys <- setNames(rep(0, length(tickers)), tickers)
    if (length(eligible) && budget > 0 && sum(deficits) > 0) {
      allocation <- min(budget, sum(deficits)) * deficits / sum(deficits)
      buys[eligible] <- floor(allocation / prices[eligible])
      remaining <- budget - sum(buys * prices[tickers])
      post_value <- current_value[tickers] + buys * prices[tickers]
      iterations <- 0L
      repeat {
        candidates <- eligible[prices[eligible] <= remaining & post_value[eligible] + prices[eligible] <= target_dollars[eligible]]
        if (!length(candidates) || iterations >= 10000L) break
        pick <- candidates[which.max(target_w[candidates] - post_value[candidates] / total_value)]
        buys[pick] <- buys[pick] + 1; post_value[pick] <- post_value[pick] + prices[pick]; remaining <- remaining - prices[pick]
        iterations <- iterations + 1L
      }
    }
    target_shares[tickers] <- current[tickers] + buys
    target_shares["Cash"] <- current["Cash"] - sum(buys * prices[tickers])
  }

  trade_shares <- target_shares - current
  post_value <- target_shares * prices
  action <- ifelse(universe == "Cash", "CASH", ifelse(trade_shares > 0, "BUY", ifelse(trade_shares < 0, "SELL", "HOLD")))
  trades <- data.frame(ticker = universe, action, price = round(prices, 4), price_date = price_dates, price_source = sources,
                       current_shares = as.numeric(current), current_weight = round(as.numeric(current_value / total_value), 6),
                       target_weight = round(as.numeric(target), 6), target_shares = as.numeric(target_shares),
                       trade_shares = as.numeric(trade_shares), trade_value = round(as.numeric(trade_shares * prices), 2),
                       post_trade_weight = round(as.numeric(post_value / total_value), 6), stringsAsFactors = FALSE)
  if (nonzero_only) trades <- trades[trades$trade_shares != 0 | trades$ticker == "Cash", , drop = FALSE]
  trades <- trades[order(match(trades$action, c("SELL", "BUY", "CASH", "HOLD")), trades$ticker), , drop = FALSE]
  rownames(trades) <- NULL
  attr(trades, "account_value") <- total_value
  trades
}

### LIVE-HISTORY BACKTEST ###
download_tickers <- unique(c(names(target_weights), benchmark))
prices <- download_adjusted_prices(download_tickers, backtest_start)
returns <- na.omit(Return.calculate(prices))
if (nrow(returns) < 252) warning("The common live-history sample contains fewer than 252 trading days")
asset_returns <- returns[, names(target_weights), drop = FALSE]
benchmark_returns <- returns[, benchmark, drop = FALSE]
rebalance_arg <- if (backtest_rebalance == "none") NA else backtest_rebalance

portfolio_rebalanced <- Return.portfolio(asset_returns, weights = target_weights, rebalance_on = rebalance_arg, geometric = TRUE)
portfolio_buy_hold <- Return.portfolio(asset_returns, weights = target_weights, rebalance_on = NA, geometric = TRUE)
comparison <- na.omit(merge(portfolio_rebalanced, portfolio_buy_hold, benchmark_returns, join = "inner"))
colnames(comparison) <- c(paste0("Target (", backtest_rebalance, ")"), "Target (buy/hold)", benchmark)

cat("\n========== LIVE-HISTORY BACKTEST ==========\n")
cat("Start:", as.character(first(index(comparison))), " | End:", as.character(last(index(comparison))), "\n")
cat("Rebalancing:", backtest_rebalance, "\n")
print(performance_table(comparison, risk_free_rate))
cat("\nCalendar returns:\n")
print(round(table.CalendarReturns(comparison), 3))
if (show_charts) charts.PerformanceSummary(comparison, main = "High-Octane Static ETF Allocation")

### CURRENT ALLOCATION AND TRADE LIST ###
trade_list <- generate_trade_list(target_weights, current_shares, cash_inflow, rebalance_date, execution_mode,
                                  no_trade_band, price_overrides, prefer_live_quotes)
cat("\n========== TARGET ALLOCATION ==========\n")
print(round(target_weights, 4))
cat("\n========== TRADE LIST: ", toupper(execution_mode), " ==========\n", sep = "")
cat("Account value: $", format(round(attr(trade_list, "account_value"), 2), big.mark = ",", nsmall = 2), "\n", sep = "")
print(trade_list, row.names = FALSE)

if (!is.null(trade_output_file)) {
  forbidden_dir <- normalizePath(path.expand("~/quant_portfolio/03_portfolio_aggregation/strategy_outputs"), mustWork = FALSE)
  selected_dir <- normalizePath(dirname(trade_output_file), mustWork = FALSE)
  if (selected_dir == forbidden_dir || startsWith(selected_dir, paste0(forbidden_dir, .Platform$file.sep))) stop("trade_output_file may not be inside the quant_portfolio strategy output directory")
  write.csv(trade_list, trade_output_file, row.names = FALSE)
  message("Trade list written to: ", normalizePath(trade_output_file, mustWork = FALSE))
}
