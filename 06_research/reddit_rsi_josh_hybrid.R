# Reddit RSI x unlevered Josh multi-asset momentum (QQQ / TQQQ).
# Close-t signals -> next trading day's close-to-close returns; daily target rebalancing.
# Requires: quantmod, TTR, xts, zoo, PerformanceAnalytics, FRAPO. No dplyr/plyr.
# Uses Yahoo adjusted prices for backtests, actual closing quotes for whole-share trades.
# Historical Josh inputs use live ETF data, NOT ftblog::aaa_returns or its 2023 splice.

suppressPackageStartupMessages({library(quantmod); library(TTR); library(PerformanceAnalytics); library(FRAPO)})
options(timeout = 300)

### 1. SETTINGS ###
base_ticker <- "QQQ"; leveraged_ticker <- "TQQQ"; start_date <- "1999-01-01"
rsi_days <- 14; sma_days <- 200; rsi_buy <- 30; rsi_hot <- 80
josh_lookback <- 120; josh_vol_days <- 42; josh_top_n <- 3
josh_weight <- 2/3                    # Remaining allocation in normal bull AND bear regimes.
overheated_josh_weight <- 0.5          # 0 = all cash/BIL; 0.5 = 50% Josh / 50% cash.
trade_bps <- 5                        # Per one-way notional; buys + sells both count.
funding_spread <- 0.01; synthetic_fee <- 0.0095; show_charts <- interactive()
run_trade_list <- FALSE               # Turn on only after entering ALL holdings in this account.
current_shares <- c(Cash=0, TQQQ=0, SPY=0, VGK=0, EEM=0, ICF=0, IEF=0, TLT=0, GLD=0, BIL=0)
cash_inflow <- 0; price_overrides <- NULL; prefer_live_quotes <- TRUE
stopifnot(josh_weight >= 0, josh_weight <= 1, overheated_josh_weight >= 0, overheated_josh_weight <= 1)

### 2. DOWNLOAD AND ALIGN PRICES ###
josh_assets <- c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD")
trade_assets <- c("LEV", setdiff(josh_assets, "Cash"), "Cash")
tickers <- unique(c(base_ticker, leveraged_ticker, setdiff(josh_assets, "Cash"), "BIL"))
fetch <- function(s) {
  message("Downloading ", s)
  x <- try(getSymbols(s, src="yahoo", from=start_date, auto.assign=FALSE, warnings=FALSE), silent=TRUE)
  if (inherits(x, "try-error") || !NROW(x)) stop("No Yahoo history for ", s)
  p <- Ad(x); colnames(p) <- s; p
}
price_list <- setNames(lapply(tickers, fetch), tickers)
prices <- do.call(merge, unname(price_list)); prices <- prices[index(price_list[[base_ticker]])]
common_end <- as.Date(min(vapply(price_list, function(x) as.numeric(last(index(x))), numeric(1))), origin="1970-01-01")
prices <- zoo::na.locf(prices[index(prices) <= common_end], na.rm=FALSE)
R <- prices / lag(prices, 1) - 1
bill <- try(getSymbols("DTB3", src="FRED", from=start_date, auto.assign=FALSE), silent=TRUE)
if (inherits(bill, "try-error") || !NROW(bill)) stop("FRED DTB3 download failed")
rate <- zoo::na.locf(merge(prices[, 1], bill, join="left")[, 2], na.rm=FALSE)
rf <- as.numeric(lag(rate, 1)) / 100 / 252 # Approximate daily yield using prior available observation.
cash_r <- ifelse(is.finite(as.numeric(R[, "BIL"])), as.numeric(R[, "BIL"]), rf)
base_r <- as.numeric(R[, base_ticker]); lev_synthetic <- 3 * base_r - 2 * (rf + funding_spread/252) - synthetic_fee/252
if (any(lev_synthetic[is.finite(lev_synthetic)] <= -1)) stop("Synthetic 3x ETF return at or below -100%")

### 3. REPLICATE JOSH'S UNLEVERED MONTHLY MOMENTUM / ERC SLEEVE ###
# Josh universe: Cash, SPY, VGK, EEM, ICF, IEF, TLT, GLD.
# At month end rank 121 observations (t-120 through t); select up to 3 above
# the cross-sectional mean; require >=2; allocate with FRAPO equal-risk contribution
# on the last 42 trading returns. No additional 3x multiplication on Josh.
# Cash is ~0 only for Josh's SIGNAL calculation; it earns BIL / T-bills in P&L.
josh_signal_r <- merge(xts(rep(1e-12, NROW(R)), order.by=index(R)), R[, setdiff(josh_assets, "Cash")], join="left")
colnames(josh_signal_r) <- josh_assets
josh_w_month <- xts(matrix(NA_real_, NROW(R), length(josh_assets), dimnames=list(NULL, josh_assets)), order.by=index(R))
month_ends <- endpoints(josh_signal_r, "months"); month_ends <- month_ends[month_ends > josh_lookback]
# endpoints() treats the last observed day of an unfinished month as month end.
next_weekday <- function(d) { d <- d + 1; while (as.POSIXlt(d)$wday %in% c(0, 6)) d <- d + 1; d }
if (length(month_ends) && format(last(index(R)), "%Y-%m") == format(Sys.Date(), "%Y-%m") &&
    format(next_weekday(last(index(R))), "%Y-%m") == format(last(index(R)), "%Y-%m")) month_ends <- head(month_ends, -1)
for (i in month_ends) {
  window <- josh_signal_r[(i-josh_lookback):i, ]
  if (any(!is.finite(as.matrix(window)))) next
  mom <- apply(1 + as.matrix(window), 2, prod) - 1
  eligible <- which(mom > mean(mom)); chosen <- head(order(mom, decreasing=TRUE)[order(mom, decreasing=TRUE) %in% eligible], josh_top_n)
  w <- setNames(rep(0, length(josh_assets)), josh_assets)
  if (length(chosen) >= 2) {
    covar <- cov(tail(as.matrix(window[, chosen]), josh_vol_days))
    # Cash's near-zero variance can make an exact ERC solution numerically singular.
    covar <- covar + diag(1e-12, nrow(covar))
    fit <- try(suppressWarnings(capture.output(solution <- FRAPO::PERC(covar, percentage=FALSE))), silent=TRUE)
    if (inherits(fit, "try-error")) stop("FRAPO::PERC failed on ", index(R)[i], "; check input covariance")
    alloc <- as.numeric(FRAPO::Weights(solution))
    if (length(alloc) != length(chosen) || any(!is.finite(alloc)) || any(alloc < -1e-8) || sum(alloc) <= 0)
      stop("Invalid ERC weights on ", index(R)[i])
    w[chosen] <- pmax(alloc, 0) / sum(pmax(alloc, 0))
  } else w["Cash"] <- 1 # Explicit cash fallback instead of an uninvested 0%-weight portfolio.
  josh_w_month[i, ] <- w
}
josh_signal_w <- zoo::na.locf(josh_w_month, na.rm=FALSE) # Close-t portfolio targets; apply lag below.

### 4. RSI REGIMES AND COMBINED UNDERLYING ETF TARGETS ###
rsi <- as.numeric(RSI(prices[, base_ticker], n=rsi_days)); sma <- as.numeric(SMA(prices[, base_ticker], n=sma_days))
good <- which(is.finite(rsi) & is.finite(sma) & is.finite(rf) & complete.cases(josh_signal_w))
if (!length(good)) stop("No dates with RSI, 200-day SMA and Josh weights")
state <- rep(NA_character_, NROW(R))
state[good] <- ifelse(rsi[good] < rsi_buy, "Dip", ifelse(rsi[good] > rsi_hot, "Overheated",
                  ifelse(as.numeric(prices[good, base_ticker]) > sma[good], "Bull", "Bear")))
signal_w <- xts(matrix(NA_real_, NROW(R), length(trade_assets), dimnames=list(NULL, trade_assets)), order.by=index(R))
signal_w[good, ] <- 0
for (i in good) {
  jw <- as.numeric(josh_signal_w[i, ]); names(jw) <- josh_assets
  if (state[i] == "Dip") signal_w[i, "LEV"] <- 1
  if (state[i] == "Overheated") {
    signal_w[i, josh_assets] <- overheated_josh_weight * jw
    signal_w[i, "Cash"] <- signal_w[i, "Cash"] + 1 - overheated_josh_weight
  }
  if (state[i] == "Bull") {
    signal_w[i, "LEV"] <- 1 - josh_weight
    signal_w[i, josh_assets] <- josh_weight * jw
  }
  if (state[i] == "Bear") {
    signal_w[i, "Cash"] <- 1 - josh_weight
    signal_w[i, josh_assets] <- as.numeric(signal_w[i, josh_assets]) + josh_weight * jw
  }
}
if (any(abs(rowSums(as.matrix(signal_w[good, ])) - 1) > 1e-7)) stop("Regime weights do not add to 100%")
weights <- lag(signal_w, 1) # Explicit: close-t signal -> t+1 return, no future prices in signals.

### 5. DAILY ETF-LEVEL BACKTEST WITH TURNOVER COSTS ###
asset_returns <- function(lev_r) {
  a <- merge(xts(lev_r, order.by=index(R)), R[, setdiff(josh_assets, "Cash")], xts(cash_r, order.by=index(R)), join="left")
  colnames(a) <- trade_assets; a
}
run_bt <- function(lev_r, first_day) {
  a <- asset_returns(lev_r)
  ix <- which(index(R) > first_day & complete.cases(weights))
  if (!length(ix)) stop("No backtest dates after ", first_day)
  w <- as.matrix(weights[ix, ]); r <- as.matrix(a[ix, ])
  if (any(w > 1e-12 & !is.finite(r))) stop("Missing return for an asset actually held")
  r[!is.finite(r)] <- 0 # Only unused instruments can have unavailable returns.
  old_w <- as.matrix(lag(weights, 1)[ix, ]); old_r <- as.matrix(lag(a, 1)[ix, ])
  old_w[!is.finite(old_w)] <- 0; old_r[!is.finite(old_r)] <- 0
  pretrade <- old_w * (1 + old_r); denom <- rowSums(pretrade)
  pretrade <- pretrade / ifelse(denom > 0, denom, 1); pretrade[1, ] <- 0
  turnover <- rowSums(abs(w - pretrade))
  ret <- xts(rowSums(w * r) - trade_bps/1e4 * turnover, order.by=index(R)[ix]); colnames(ret) <- "Strategy"
  list(returns=ret, turnover=xts(turnover, order.by=index(ret)), weights=weights[ix, ])
}
extended <- run_bt(lev_synthetic, index(R)[good[1]])
actual_start <- first(index(price_list[[leveraged_ticker]]))
actual <- run_bt(as.numeric(R[, leveraged_ticker]), actual_start)
synth_matched <- run_bt(lev_synthetic, actual_start)$returns # Same initial funding, dates and trading-cost convention.
bench_extended <- R[index(extended$returns), base_ticker]; bench_matched <- R[index(actual$returns), base_ticker]
josh_holding <- lag(josh_signal_w, 1)
josh_asset_r <- merge(R[, setdiff(josh_assets, "Cash")], xts(cash_r, order.by=index(R)), join="left")
colnames(josh_asset_r) <- c(setdiff(josh_assets, "Cash"), "Cash")
josh_asset_r <- josh_asset_r[, josh_assets]; josh_rows <- which(complete.cases(josh_holding))
jw <- as.matrix(josh_holding[josh_rows, ]); jr <- as.matrix(josh_asset_r[josh_rows, ])
if (any(jw > 1e-12 & !is.finite(jr))) stop("Missing held Josh ETF history")
jr[!is.finite(jr)] <- 0
josh_returns <- xts(rowSums(jw * jr), order.by=index(R)[josh_rows]); colnames(josh_returns) <- "Josh unlevered (before trading costs)"

### 6. METRICS / NEXT-SESSION TARGET ###
metrics <- function(x) {
  z <- as.numeric(x); excess <- z - cash_r[match(index(x), index(R))]
  c(CAGR=100*as.numeric(Return.annualized(x, scale=252)), Sharpe=sqrt(252)*mean(excess)/sd(excess),
    Vol=100*sd(z)*sqrt(252), MaxDD=-100*as.numeric(maxDrawdown(x)))
}
results <- rbind("Hybrid: synthetic 3x, extended"=metrics(extended$returns), "QQQ buy & hold, extended"=metrics(bench_extended),
                 "Hybrid: synthetic 3x, matched"=metrics(synth_matched), "Hybrid: actual TQQQ"=metrics(actual$returns),
                 "QQQ buy & hold, matched"=metrics(bench_matched), "Josh standalone, matched"=metrics(josh_returns[index(actual$returns)]))
cat("\n========== REDDIT RSI x JOSH HYBRID ==========\n")
cat("Extended:", as.character(first(index(extended$returns))), "to", as.character(last(index(extended$returns))),
    "| actual TQQQ:", as.character(first(index(actual$returns))), "to", as.character(last(index(actual$returns))), "\n")
print(round(results, 2))
cat("Daily gross turnover, actual (buys + sells):", round(mean(as.numeric(actual$turnover))*100, 2), "% of NAV\n")
cat("Days by regime (actual window):\n"); print(table(state[match(index(actual$returns), index(R))]))
if (show_charts) {
  x <- merge(extended$returns, bench_extended, join="inner"); colnames(x) <- c("Hybrid synthetic", "QQQ")
  charts.PerformanceSummary(x, main="Hybrid, extended synthetic TQQQ")
  x <- merge(synth_matched, actual$returns, bench_matched, join="inner"); colnames(x) <- c("Hybrid synthetic", "Hybrid actual", "QQQ")
  charts.PerformanceSummary(x, main="Hybrid, identical actual-TQQQ dates")
}
latest <- tail(which(complete.cases(signal_w)), 1)
if (!length(latest)) stop("No current target")
# Put all implementable tickers into a single uniquely named vector; no synthetic cash order.
raw_target <- as.numeric(signal_w[latest, ]); names(raw_target) <- trade_assets
model_target <- c(setNames(raw_target["LEV"], leveraged_ticker), raw_target[setdiff(josh_assets, "Cash")], BIL=raw_target["Cash"])
model_target <- setNames(as.numeric(model_target), c(leveraged_ticker, setdiff(josh_assets, "Cash"), "BIL"))
cat("\n========== NEXT-SESSION TARGET ==========\n")
cat("Signal date:", as.character(index(R)[latest]), "| state:", state[latest], "| RSI:", round(rsi[latest], 2), "\n")
print(round(model_target[model_target > 1e-9], 5))
if (Sys.Date() - index(R)[latest] > 5) warning("Signal is more than five calendar days old")

### 7. OPTIONAL WHOLE-SHARE TRADE LIST (BIL IS CASH EXPOSURE) ###
canonical <- function(x, label) {
  if (is.null(x)) return(numeric())
  if (!is.numeric(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!is.finite(x))) stop(label, " must be a named finite numeric vector")
  names(x) <- ifelse(tolower(names(x)) == "cash", "Cash", toupper(names(x)))
  t <- tapply(x, names(x), sum); setNames(as.numeric(t), names(t))
}
fetch_trade_price <- function(s, as_of=Sys.Date(), override=NA_real_, live=TRUE) {
  if (is.finite(override) && override > 0) return(c(price=as.character(override), date=as.character(as_of), source="override"))
  clock <- as.POSIXlt(Sys.time(), tz="America/New_York")
  market_open <- as_of == Sys.Date() && clock$wday %in% 1:5 && (clock$hour > 9 || (clock$hour == 9 && clock$min >= 30)) && clock$hour < 16
  if (live && market_open) {
    q <- try(getQuote(s, src="yahoo"), silent=TRUE)
    if (!inherits(q, "try-error") && NROW(q)) for (p in intersect(c("Last", "Price"), colnames(q)))
      if (is.finite(as.numeric(q[1, p])) && as.numeric(q[1, p]) > 0)
        return(c(price=as.character(as.numeric(q[1, p])), date=as.character(as_of), source="quote"))
  }
  p <- try(getSymbols(s, src="yahoo", from=as_of-14, to=as_of+1, auto.assign=FALSE, warnings=FALSE), silent=TRUE)
  if (inherits(p, "try-error") || !NROW(p)) stop("Cannot quote ", s)
  close <- Cl(p); close <- close[index(close) <= as_of]
  if (!NROW(close) || !is.finite(as.numeric(last(close)))) stop("No trade price for ", s)
  c(price=as.character(as.numeric(last(close))), date=as.character(last(index(close))), source="close")
}
make_trades <- function(target, holdings, inflow=0, overrides=NULL) {
  target <- canonical(target, "target"); holdings <- canonical(holdings, "holdings"); overrides <- canonical(overrides, "overrides")
  if (any(target < 0) || abs(sum(target)-1) > 1e-7 || any(holdings < 0)) stop("Invalid target or current shares")
  if (!"Cash" %in% names(holdings)) stop("Include Cash dollars in current_shares")
  holdings["Cash"] <- holdings["Cash"] + inflow
  if (holdings["Cash"] < 0) stop("Insufficient cash for outflow")
  assets <- union(names(target), names(holdings)); wanted <- held <- setNames(rep(0, length(assets)), assets)
  wanted[names(target)] <- target; held[names(holdings)] <- holdings
  assets <- assets[wanted != 0 | held != 0 | assets == "Cash"]; wanted <- wanted[assets]; held <- held[assets]
  px <- setNames(rep(1, length(assets)), assets); stamp <- origin <- setNames(rep("cash", length(assets)), assets)
  for (s in setdiff(assets, "Cash")) {
    q <- fetch_trade_price(s, override=if (s %in% names(overrides)) overrides[s] else NA_real_, live=prefer_live_quotes)
    px[s] <- as.numeric(q["price"]); stamp[s] <- q["date"]; origin[s] <- q["source"]
  }
  nav <- sum(held*px); if (!is.finite(nav) || nav <= 0) stop("Positive account value required")
  target_shares <- floor(nav*wanted/px); target_shares["Cash"] <- nav - sum(target_shares[assets != "Cash"]*px[assets != "Cash"])
  delta <- target_shares - held
  out <- data.frame(ticker=assets, action=ifelse(assets=="Cash", "CASH", ifelse(delta>0, "BUY", ifelse(delta<0, "SELL", "HOLD"))),
                    price=round(px, 4), price_date=stamp, price_source=origin, current=as.numeric(held),
                    target_weight=round(wanted, 6), target_shares=as.numeric(target_shares), trade_shares=as.numeric(delta),
                    trade_value=round(as.numeric(delta*px), 2), row.names=NULL)
  out <- out[out$trade_shares != 0 | out$ticker == "Cash", ]; rownames(out) <- NULL
  attr(out, "account_value") <- nav; out
}
trade_list <- NULL
if (run_trade_list) {
  trade_list <- make_trades(model_target, current_shares, cash_inflow, price_overrides)
  cat("\n========== WHOLE-SHARE TRADE LIST ==========\n"); print(trade_list, row.names=FALSE)
} else cat("\nTrade list OFF. Enter ALL positions in current_shares and set run_trade_list <- TRUE.\n")

# No exports to quant_portfolio strategy_outputs (or anywhere else).
backtest <- list(metrics=results, extended=extended, actual=actual, synthetic_matched=synth_matched,
                 josh_returns=josh_returns, josh_weights=josh_signal_w, regime=xts(state, order.by=index(R)),
                 signal_weights=signal_w, actual_weights=actual$weights, next_target=model_target, prices=prices)
