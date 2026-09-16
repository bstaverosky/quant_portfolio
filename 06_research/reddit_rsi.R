
# Reddit RSI / 200-day strategy: synthetic 3x history and actual-ETF comparison.
# Uses adjusted ETF prices; no dplyr/plyr. Signals at close t, positions held t+1.
suppressPackageStartupMessages({library(quantmod); library(TTR); library(PerformanceAnalytics)})

from <- "1993-01-01"; trade_bps <- 5; funding_spread <- 0.01; synthetic_fee <- 0.0095
symbols <- c("SPY", "QQQ", "UPRO", "TQQQ", "GLD", "CTA", "UVXY", "BIL")
fetch <- function(s) { x <- Ad(getSymbols(s, src="yahoo", from=from, auto.assign=FALSE)); colnames(x) <- s; x }
px <- setNames(lapply(symbols, fetch), symbols)
tbill <- getSymbols("DTB3", src="FRED", from=from, auto.assign=FALSE) # 3-month T-bill discount rate, % p.a.

analyze <- function(base="QQQ", lev="TQQQ") {
  P <- do.call(merge, unname(px)); P <- P[index(px[[base]])]
  last_common <- min(vapply(px, function(z) as.numeric(tail(index(z), 1)), numeric(1)))
  P <- zoo::na.locf(P[index(P) <= as.Date(last_common, origin="1970-01-01")], na.rm=FALSE)
  R <- P / lag(P, 1) - 1
  rate <- lag(zoo::na.locf(merge(P[, 1], tbill, join="left")[, 2], na.rm=FALSE), 1)
  rf <- as.numeric(rate) / 100 / 252
  cash <- ifelse(is.finite(as.numeric(R[, "BIL"])), as.numeric(R[, "BIL"]), rf)
  equity <- as.numeric(R[, base]); synthetic <- 3 * equity - 2 * (rf + funding_spread/252) - synthetic_fee/252
  if (any(synthetic[is.finite(synthetic)] <= -1)) stop("Synthetic 3x equity lost 100% in a day; cannot treat it as an ETF.")
  
  rsi <- as.numeric(RSI(P[, base], n=14)); sma <- as.numeric(SMA(P[, base], n=200))
  good <- which(is.finite(rsi) & is.finite(sma) & is.finite(as.numeric(P[, "GLD"])) & is.finite(rf))
  if (!length(good)) stop("Not enough overlapping data to initialize strategy.")
  state <- rep(NA_character_, NROW(P))
  state[good] <- ifelse(rsi[good] < 30, "Dip", ifelse(rsi[good] > 80, "Overheated", ifelse(as.numeric(P[good, base]) > sma[good], "Trend", "Defensive")))
  W <- xts(matrix(NA_real_, NROW(P), 5, dimnames=list(NULL, c("LEV", "UVXY", "CTA", "GLD", "CASH"))), order.by=index(P)); W[good, ] <- 0
  W[good, "LEV"] <- as.numeric(state[good] == "Dip") + as.numeric(state[good] == "Trend")/3
  W[good, "UVXY"] <- as.numeric(state[good] == "Overheated")
  W[good, "CTA"] <- as.numeric(state[good] %in% c("Trend", "Defensive"))/3
  W[good, "GLD"] <- as.numeric(state[good] %in% c("Trend", "Defensive"))/3
  W[good, "CASH"] <- as.numeric(state[good] == "Defensive")/3
  for (s in c("UVXY", "CTA")) {
    missing <- good[is.na(as.numeric(P[good, s]))]
    if (length(missing)) { W[missing, "CASH"] <- W[missing, "CASH"] + W[missing, s]; W[missing, s] <- 0 }
  }
  W <- lag(W, 1)  # Close-t signals become close-t to close-(t+1) holdings.
  partial_start <- do.call(max, lapply(c(lev, "UVXY", "GLD", "BIL"), function(s) min(index(px[[s]]))))
  actual_start <- do.call(max, lapply(c(lev, "UVXY", "CTA", "GLD", "BIL"), function(s) min(index(px[[s]]))))
  
  run_bt <- function(lev_return, first_day) {
    A <- xts(cbind(LEV=lev_return, UVXY=as.numeric(R[, "UVXY"]), CTA=as.numeric(R[, "CTA"]), GLD=as.numeric(R[, "GLD"]), CASH=cash), order.by=index(P))
    ix <- which(index(P) > first_day & is.finite(as.numeric(W[, "LEV"])))
    if (!length(ix)) stop("No eligible backtest dates.")
    wt <- as.matrix(W[ix, ]); ar <- as.matrix(A[ix, ])
    if (any(wt > 0 & !is.finite(ar))) stop("Missing return for an asset while the strategy holds it.")
    ar[!is.finite(ar)] <- 0
    prev_w <- as.matrix(lag(W, 1)[ix, ]); prev_r <- as.matrix(lag(A, 1)[ix, ])
    prev_w[!is.finite(prev_w)] <- 0; prev_r[!is.finite(prev_r)] <- 0
    pretrade <- prev_w * (1 + prev_r); denom <- rowSums(pretrade)
    pretrade <- pretrade / ifelse(denom > 0, denom, 1); pretrade[1, ] <- 0
    turnover <- rowSums(abs(wt - pretrade)); out <- xts(rowSums(wt * ar) - turnover * trade_bps/10000, order.by=index(P)[ix])
    colnames(out) <- "Strategy"; list(returns=out, turnover=xts(turnover, order.by=index(out)), weights=W[ix, ])
  }
  
  extended <- run_bt(synthetic, index(P)[good[1]])
  partial_actual <- run_bt(as.numeric(R[, lev]), partial_start) # Actual 3x/UVXY; CTA is cash until its inception.
  actual <- run_bt(as.numeric(R[, lev]), actual_start)
  synth_match <- extended$returns[index(actual$returns)]
  bench_extended <- R[index(extended$returns), base]; bench_partial <- R[index(partial_actual$returns), base]; bench_match <- R[index(actual$returns), base]
  cash_sharpe <- function(z) { excess <- as.numeric(z) - cash[match(index(z), index(P))]; sqrt(252) * mean(excess) / sd(excess) }
  metrics <- function(z) c(CAGR=100*as.numeric(Return.annualized(z, scale=252)), Sharpe=cash_sharpe(z), Vol=100*sd(as.numeric(z))*sqrt(252), MaxDD=-100*as.numeric(maxDrawdown(z)))
  post <- actual$returns["2025-12-15/"]; bench_post <- bench_match["2025-12-15/"]
  result <- rbind("Synthetic 3x, extended"=metrics(extended$returns), "Buy & hold, extended"=metrics(bench_extended),
                  "Actual 3x/UVXY; pre-CTA cash"=metrics(partial_actual$returns), "Buy & hold, same early window"=metrics(bench_partial),
                  "Synthetic 3x, matched"=metrics(synth_match), "Actual ETFs, matched"=metrics(actual$returns),
                  "Buy & hold, matched"=metrics(bench_match))
  if (NROW(post) > 10) result <- rbind(result, "Actual ETFs, post-publication"=metrics(post), "Buy & hold, post-publication"=metrics(bench_post))
  cat("\n", base, "/", lev, " | extended:", as.character(first(index(extended$returns))), "to", as.character(last(index(extended$returns))),
      " | actual 3x/UVXY:", as.character(first(index(partial_actual$returns))), " | full actual-ETF window:", as.character(first(index(actual$returns))), "to", as.character(last(index(actual$returns))), "\n")
  print(round(result, 2)); cat("Mean daily gross traded notional, buys + sells (actual ETF strategy):", round(mean(as.numeric(actual$turnover))*100, 2), "% of NAV\n")
  long_chart <- merge(extended$returns, bench_extended, join="inner"); colnames(long_chart) <- c("Synthetic 3x / proxy gaps", paste0(base, " buy & hold"))
  real_chart <- merge(synth_match, actual$returns, bench_match, join="inner"); colnames(real_chart) <- c("Synthetic 3x", "Actual 3x ETFs", paste0(base, " buy & hold"))
  charts.PerformanceSummary(long_chart, main=paste(base, "extended: synthetic 3x; missing early funds -> cash"))
  charts.PerformanceSummary(real_chart, main=paste(base, "matched: synthetic vs actual leveraged ETFs"))
  invisible(list(metrics=result, extended=extended, partial_actual=partial_actual, actual=actual, synthetic_matched=synth_match, benchmark=bench_match, signals=xts(state, order.by=index(P)), prices=P, cash=xts(cash, order.by=index(P))))
}

qqq <- analyze("QQQ", "TQQQ")
spy <- analyze("SPY", "UPRO")