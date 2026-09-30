# remotes::install_github("joshuaulrich/ftblog")
rm(list = ls())

suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
})

options(timeout = 300)

### CONFIGURATION ###
use_cash <- TRUE
upro_weight <- 0.33
strat_weight <- 1 - upro_weight
show_charts <- interactive()

# Holdings for the account dedicated to this strategy. Cash is in dollars; all other values are shares.
# Any non-target holding with a nonzero share count will receive a full-liquidation trade.
current_shares <- c(cash = 4864, QQQ = 9, TQQQ = 0, SPY = 34, UPRO = 0, EURL = 0,
                    EDC = 0, DRN = 0, TYD = 0, TMF = 0, SHNY = 0)
cash_inflow <- 0
price_overrides <- NULL       # Optional named vector, e.g. c(UPRO = 112.50)
prefer_live_quotes <- TRUE
trade_output_file <- NULL     # Optional path; NULL keeps the trade list in memory/console only

stopifnot(is.numeric(upro_weight), length(upro_weight) == 1, is.finite(upro_weight), upro_weight >= 0, upro_weight <= 1)

### FUNCTIONS ###
.find_top_momo_columns <- function(returns, n_assets = 5, type = c("relative", "positive", "above average")) {
  type <- match.arg(type)
  ok <- is.finite(returns)
  include <- switch(type, relative = ok, positive = ok & returns > 0, `above average` = ok & returns > mean(returns[ok]))
  ranked <- order(returns, decreasing = TRUE, na.last = NA)
  head(ranked[ranked %in% which(include)], n_assets)
}

portf_return_momo_equal_risk <- function(returns, n_assets = 5, n_days = 120, n_days_vol = 60,
                                         momo_type = c("relative", "positive", "above average"),
                                         otype = c("returns", "weights")) {
  if (!is.xts(returns) || nrow(returns) <= n_days) stop("returns must be an xts object with more than n_days rows")
  momo_type <- match.arg(momo_type)
  otype <- match.arg(otype)
  month_end_i <- endpoints(returns, "months")
  month_end_i <- month_end_i[month_end_i > n_days & month_end_i < nrow(returns)] # Never treat an incomplete final month as month-end.
  weights <- returns * NA_real_
  
  for (i in month_end_i) {
    window <- returns[(i - n_days):i, ]
    momentum <- apply(1 + window, 2, prod, na.rm = TRUE) - 1
    top_cols <- .find_top_momo_columns(momentum, n_assets, momo_type)
    weights[i, ] <- 0
    if (length(top_cols) >= 2) {
      w <- try(portf_wts_equal_risk(window[, top_cols], n_days_vol), silent = TRUE)
      if (inherits(w, "try-error") || any(!is.finite(w)) || sum(w) <= 0) w <- rep(1 / length(top_cols), length(top_cols))
      weights[i, top_cols] <- as.numeric(w) / sum(w)
    } else if (length(top_cols) == 1) {
      weights[i, top_cols] <- 1
    } else if ("Cash" %in% colnames(weights)) {
      weights[i, "Cash"] <- 1
    }
  }
  
  weights <- zoo::na.locf(lag(weights), na.rm = FALSE)
  if (otype == "weights") return(weights)
  out <- xts(rowSums(returns * weights), index(returns))
  names(out) <- "R_momo_eq_risk"
  out
}

download_adjusted_returns <- function(tickers, from) {
  out <- lapply(tickers, function(ticker) {
    message("Downloading returns: ", ticker)
    x <- try(getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE, warnings = FALSE), silent = TRUE)
    if (inherits(x, "try-error") || !nrow(x)) stop("Return download failed for ", ticker)
    px <- zoo::na.locf(Ad(x), na.rm = FALSE)
    colnames(px) <- ticker
    Return.calculate(px)
  })
  do.call(merge, out)
}

canonicalize_named <- function(x, label) {
  if (is.null(x)) return(numeric())
  if (!is.numeric(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!is.finite(x))) stop(label, " must be a finite named numeric vector")
  names(x) <- ifelse(tolower(names(x)) == "cash", "Cash", toupper(names(x)))
  summed <- tapply(x, names(x), sum)
  setNames(as.numeric(summed), names(summed))
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

generate_trade_list <- function(target_w, current_shares, cash_inflow = 0, as_of = Sys.Date(),
                                price_overrides = NULL, prefer_live = TRUE, nonzero_only = TRUE) {
  target_w <- canonicalize_named(target_w, "target_w")
  current_shares <- canonicalize_named(current_shares, "current_shares")
  price_overrides <- canonicalize_named(price_overrides, "price_overrides")
  if (any(target_w < 0) || sum(target_w) <= 0) stop("target_w must be nonnegative and have a positive sum")
  if (any(current_shares < 0)) stop("current_shares must be nonnegative")
  if (!is.numeric(cash_inflow) || length(cash_inflow) != 1 || !is.finite(cash_inflow)) stop("cash_inflow must be one finite number")
  if (abs(sum(target_w) - 1) > 1e-8) { warning("Target weights sum to ", round(sum(target_w), 8), "; normalizing to 1"); target_w <- target_w / sum(target_w) }
  
  universe <- union(names(target_w), names(current_shares))
  target <- setNames(rep(0, length(universe)), universe); target[names(target_w)] <- target_w
  current <- setNames(rep(0, length(universe)), universe); current[names(current_shares)] <- current_shares
  keep <- target != 0 | current != 0 | universe == "Cash"
  universe <- universe[keep]; target <- target[universe]; current <- current[universe]
  if (!"Cash" %in% universe) { universe <- c("Cash", universe); target <- c(Cash = 0, target); current <- c(Cash = 0, current) }
  current["Cash"] <- current["Cash"] + cash_inflow
  
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
  total_value <- sum(current * prices)
  if (!is.finite(total_value) || total_value <= 0) stop("Account value must be positive after cash_inflow")
  
  target_dollars <- total_value * target
  target_shares <- floor(target_dollars / prices)
  noncash <- universe != "Cash"
  target_shares["Cash"] <- total_value - sum(target_shares[noncash] * prices[noncash])
  trade_shares <- target_shares - current
  action <- ifelse(universe == "Cash", "CASH", ifelse(trade_shares > 0, "BUY", ifelse(trade_shares < 0, "SELL", "HOLD")))
  trades <- data.frame(ticker = universe, action, price = round(prices, 4), price_date = price_dates, price_source = sources,
                       current_shares = as.numeric(current), target_weight = round(target, 6), target_shares = as.numeric(target_shares),
                       trade_shares = as.numeric(trade_shares), trade_value = round(as.numeric(trade_shares * prices), 2), stringsAsFactors = FALSE)
  rownames(trades) <- NULL
  if (nonzero_only) trades <- trades[trades$trade_shares != 0 | trades$ticker == "Cash", , drop = FALSE]
  attr(trades, "account_value") <- total_value
  trades
}

### LOAD AND UPDATE HISTORICAL DATA ###
data(aaa_returns, package = "ftblog")
etfs <- c("SPY", "VGK", "EWJ", "EEM", "ICF", "RWX", "IEF", "TLT", "DBC", "GLD")
expected_names <- if (use_cash) c("Cash", etfs) else etfs
returns <- if (use_cash) aaa_returns else aaa_returns[, -1]
if (ncol(returns) != length(expected_names)) stop("Unexpected aaa_returns column count; expected ", length(expected_names), " and found ", ncol(returns))
colnames(returns) <- expected_names
history_end <- as.Date(last(index(returns)))
assets <- download_adjusted_returns(etfs, history_end - 14)
if (use_cash) assets$Cash <- 0
assets <- assets[, expected_names]
assets <- assets[paste0(history_end + 1, "/")]
returns <- rbind(returns, assets)
returns <- returns[!duplicated(index(returns), fromLast = TRUE)]
if (!use_cash) returns$Cash <- 0

r_full <- returns[, c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD")]
r_full$Cash <- 1e-12
r_full <- r_full[complete.cases(r_full)]
if (nrow(r_full) < 121) stop("Insufficient complete return history")

### STRATEGY ###
strat_returns <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average")
strat_wts <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "weights")
names(strat_returns) <- "Unlevered_Momentum"

upro_prices <- getSymbols("UPRO", src = "yahoo", from = "2009-01-01", auto.assign = FALSE, warnings = FALSE)
upro_actual <- Return.calculate(zoo::na.locf(Ad(upro_prices), na.rm = FALSE))
names(upro_actual) <- "UPRO_Actual"
upro_synthetic <- 3 * r_full$SPY
names(upro_synthetic) <- "UPRO_Synthetic"

full <- na.omit(merge(strat_returns, upro_synthetic, join = "inner"))
blend_synthetic <- strat_weight * full$Unlevered_Momentum + upro_weight * full$UPRO_Synthetic
names(blend_synthetic) <- "Blend_Synthetic"
actual <- na.omit(merge(full, upro_actual, join = "inner"))
blend_actual <- strat_weight * actual$Unlevered_Momentum + upro_weight * actual$UPRO_Actual
names(blend_actual) <- "Blend_Actual"
blend_synthetic_matched <- blend_synthetic[index(blend_actual)]
names(blend_synthetic_matched) <- "Blend_Synthetic_Matched"

blend_wts <- strat_weight * strat_wts[index(blend_synthetic)]
blend_wts$UPRO <- upro_weight
colnames(blend_wts) <- c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD", "UPRO")

### PERFORMANCE REPORT ###
comparison_full <- na.omit(merge(blend_synthetic, full$Unlevered_Momentum, full$UPRO_Synthetic, r_full$SPY, join = "inner"))
colnames(comparison_full) <- c("33% Synthetic UPRO + Momentum", "Unlevered Momentum", "Synthetic UPRO", "S&P 500")
comparison_matched <- na.omit(merge(blend_actual, blend_synthetic_matched, actual$Unlevered_Momentum, actual$UPRO_Actual, r_full$SPY, join = "inner"))
colnames(comparison_matched) <- c("33% Actual UPRO + Momentum", "33% Synthetic UPRO + Momentum", "Unlevered Momentum", "Actual UPRO", "S&P 500")

print_report <- function(x, title) {
  cat("\n========== ", title, " ==========\n", sep = "")
  cat("Start:", as.character(first(index(x))), " | End:", as.character(last(index(x))), "\n")
  print(round(rbind(table.AnnualizedReturns(x), `Worst Drawdown` = -maxDrawdown(x)), 3))
  if (show_charts) charts.PerformanceSummary(x, main = title)
}
print_report(comparison_full, "FULL SYNTHETIC HISTORY")
print_report(comparison_matched, "MATCHED ACTUAL VS SYNTHETIC")
pre_upro <- comparison_full[index(comparison_full) < first(index(blend_actual))]
if (nrow(pre_upro)) print_report(pre_upro, "PRE-UPRO SYNTHETIC HISTORY")

### CURRENT TARGET AND TRADE LIST ###
target_date <- as.Date(last(index(blend_wts)))
target_weights <- as.numeric(last(blend_wts))
names(target_weights) <- colnames(blend_wts)
if (any(!is.finite(target_weights))) stop("Latest target weights are unavailable")

cat("\n========== CURRENT TARGET ==========\n")
cat("Target effective date:", as.character(target_date), "\n")
print(round(target_weights[target_weights != 0], 4))

trade_list <- generate_trade_list(target_weights, current_shares, cash_inflow, Sys.Date(), price_overrides, prefer_live_quotes)
cat("\n========== TRADE LIST ==========\n")
cat("Account value: $", format(round(attr(trade_list, "account_value"), 2), big.mark = ",", nsmall = 2), "\n", sep = "")
print(trade_list, row.names = FALSE)

if (!is.null(trade_output_file)) {
  forbidden_dir <- normalizePath(path.expand("~/quant_portfolio/03_portfolio_aggregation/strategy_outputs"), mustWork = FALSE)
  selected_dir <- normalizePath(dirname(trade_output_file), mustWork = FALSE)
  if (selected_dir == forbidden_dir || startsWith(selected_dir, paste0(forbidden_dir, .Platform$file.sep))) stop("trade_output_file may not be inside the quant_portfolio strategy output directory")
  write.csv(trade_list, trade_output_file, row.names = FALSE)
  message("Trade list written to: ", normalizePath(trade_output_file, mustWork = FALSE))
}
