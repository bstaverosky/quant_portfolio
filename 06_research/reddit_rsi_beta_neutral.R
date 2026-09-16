# Beta-hedged Reddit RSI strategy: QQQ/TQQQ and SPY/UPRO, synthetic and actual ETFs.
# Long-only ETF positions; SPXS supplies negative S&P 500 exposure. No dplyr/plyr.
# Signals and beta estimates use information through close t; earn returns t+1.
suppressPackageStartupMessages({library(quantmod); library(TTR); library(PerformanceAnalytics)})

from <- "1993-01-01"; beta_days <- 126; min_beta_days <- 60; trade_bps <- 5
funding_spread <- 0.01; synthetic_fee <- 0.0095
symbols <- c("SPY", "QQQ", "UPRO", "TQQQ", "SPXS", "GLD", "CTA", "UVXY", "BIL")
fetch <- function(s) { z <- Ad(getSymbols(s, src="yahoo", from=from, auto.assign=FALSE)); colnames(z) <- s; z }
px <- setNames(lapply(symbols, fetch), symbols)
tbill <- getSymbols("DTB3", src="FRED", from=from, auto.assign=FALSE)

analyze <- function(base="QQQ", lev="TQQQ") {
  P <- do.call(merge, unname(px)); P <- P[index(px[[base]])]
  last_day <- min(vapply(px, function(z) as.numeric(last(index(z))), numeric(1)))
  P <- zoo::na.locf(P[index(P) <= as.Date(last_day, origin="1970-01-01")], na.rm=FALSE)
  R <- P / lag(P, 1) - 1; n <- NROW(P); dates <- index(P)
  rate <- lag(zoo::na.locf(merge(P[, 1], tbill, join="left")[, 2], na.rm=FALSE), 1)
  rf <- as.numeric(rate)/100/252; cash <- ifelse(is.finite(as.numeric(R[, "BIL"])), as.numeric(R[, "BIL"]), rf)
  syn3 <- function(r) 3*r - 2*(rf + funding_spread/252) - synthetic_fee/252
  synshort <- -3*as.numeric(R[, "SPY"]) + 4*rf - 3*funding_spread/252 - synthetic_fee/252
  if (any(syn3(as.numeric(R[, base]))[is.finite(syn3(as.numeric(R[, base])))] <= -1)) stop("Synthetic 3x long lost >=100% in one day.")
  common <- c("LEV", "GLD", "CTA", "UVXY", "CASH", "HEDGE_UPRO", "HEDGE_SPXS")
  returns_for <- function(synthetic=TRUE) {
    m <- cbind(LEV=if (synthetic) syn3(as.numeric(R[, base])) else as.numeric(R[, lev]),
               GLD=as.numeric(R[, "GLD"]), CTA=as.numeric(R[, "CTA"]), UVXY=as.numeric(R[, "UVXY"]), CASH=cash,
               HEDGE_UPRO=if (synthetic) syn3(as.numeric(R[, "SPY"])) else as.numeric(R[, "UPRO"]),
               HEDGE_SPXS=if (synthetic) synshort else as.numeric(R[, "SPXS"]))
    xts(m, order.by=dates)
  }
  # Rolling CAPM beta of each ETF against SPY. Each regression uses <=126 *past/current* daily observations.
  rolling_betas <- function(A) {
    b <- matrix(NA_real_, n, length(common), dimnames=list(NULL, common)); b[, "CASH"] <- 0
    mkt <- as.numeric(R[, "SPY"])
    for (j in setdiff(common, "CASH")) {
      x <- as.numeric(A[, j])
      for (i in seq_len(n)) {
        lo <- max(1L, i-beta_days+1L); ii <- lo:i; ok <- is.finite(x[ii]) & is.finite(mkt[ii])
        if (sum(ok) >= min_beta_days) { v <- var(mkt[ii][ok]); if (is.finite(v) && v > 0) b[i, j] <- cov(x[ii][ok], mkt[ii][ok])/v }
      }
    }
    b
  }
  rsi <- as.numeric(RSI(P[, base], n=14)); sma <- as.numeric(SMA(P[, base], n=200))
  state <- ifelse(rsi < 30, "Dip", ifelse(rsi > 80, "Overheated", ifelse(as.numeric(P[, base]) > sma, "Trend", "Defensive")))
  state[!is.finite(rsi) | !is.finite(sma)] <- NA_character_
  core_weights <- function(B) {
    W <- matrix(NA_real_, n, length(common), dimnames=list(NULL, common))
    for (i in which(!is.na(state))) {
      w <- setNames(rep(0, length(common)), common)
      if (state[i] == "Dip") w["LEV"] <- 1
      if (state[i] == "Overheated") w["UVXY"] <- 1
      if (state[i] == "Trend") w[c("LEV", "GLD", "CTA")] <- 1/3
      if (state[i] == "Defensive") w[c("GLD", "CTA", "CASH")] <- 1/3
      # Unavailable or insufficiently seasoned ETF: hold its earmarked allocation in cash instead.
      for (s in c("UVXY", "CTA")) if (!is.finite(as.numeric(P[i, s])) || !is.finite(B[i, s])) { w["CASH"] <- w["CASH"] + w[s]; w[s] <- 0 }
      if (!is.finite(as.numeric(P[i, "GLD"])) || !is.finite(B[i, "GLD"])) next
      W[i, ] <- w
    }
    W
  }
  make_weights <- function(A, B) {
    core <- core_weights(B); neutral <- core; target_beta <- rep(NA_real_, n)
    for (i in which(is.finite(core[, "CASH"]))) {
      held <- which(core[i, ] > 0)
      if (any(!is.finite(B[i, held])) || any(!is.finite(as.numeric(A[i, held])))) { neutral[i, ] <- NA; next }
      b0 <- sum(core[i, held] * B[i, held]); target_beta[i] <- b0
      h <- if (b0 > 0) "HEDGE_SPXS" else "HEDGE_UPRO"; bh <- B[i, h]
      if (abs(b0) < 1e-10) { target_beta[i] <- 0; next }
      if (!is.finite(bh) || !is.finite(as.numeric(A[i, h])) || b0*bh >= 0 || abs(bh) < .5) { neutral[i, ] <- NA; next }
      hedge_ratio <- abs(b0/bh); neutral[i, ] <- core[i, ]/(1+hedge_ratio)
      neutral[i, h] <- neutral[i, h] + hedge_ratio/(1+hedge_ratio)
      target_beta[i] <- sum(neutral[i, ] * B[i, ])
    }
    list(core=core, neutral=neutral, target_beta=target_beta)
  }
  # Weights specified at close t are held for close-t to close-(t+1). Costs on gross buys+sells.
  backtest <- function(A, W, start_day) {
    holdings <- lag(xts(W, order.by=dates), 1)
    ix <- which(dates > start_day & is.finite(as.numeric(holdings[, "CASH"])))
    if (!length(ix)) stop("No eligible backtest dates.")
    if (any(diff(ix) != 1)) stop("Missing hedge estimates within backtest; check source data.")
    w <- as.matrix(holdings[ix, ]); ar <- as.matrix(A[ix, ])
    if (any(w > 0 & !is.finite(ar))) stop("A held ETF has a missing daily return.")
    ar[!is.finite(ar)] <- 0; turnover <- numeric(length(ix)); turnover[1] <- sum(abs(w[1, ]))
    if (length(ix) > 1) for (i in 2:length(ix)) {
      drift <- w[i-1, ] * (1+ar[i-1, ]); drift <- drift/sum(drift)
      turnover[i] <- sum(abs(w[i, ]-drift))
    }
    z <- xts(rowSums(w*ar) - turnover*trade_bps/10000, order.by=dates[ix]); colnames(z) <- "Strategy"
    list(returns=z, weights=holdings[ix, ], turnover=xts(turnover, order.by=dates[ix]))
  }
  stats <- function(z) {
    mr <- as.numeric(R[index(z), "SPY"]); cr <- cash[match(index(z), dates)]; x <- as.numeric(z)
    c(CAGR=100*as.numeric(Return.annualized(z, scale=252)), Sharpe=sqrt(252)*mean(x-cr)/sd(x-cr),
      Vol=100*sd(x)*sqrt(252), MaxDD=-100*as.numeric(maxDrawdown(z)), SPY_beta=cov(x, mr)/var(mr))
  }
  synthetic_A <- returns_for(TRUE); actual_A <- returns_for(FALSE)
  synthetic_B <- rolling_betas(synthetic_A); actual_B <- rolling_betas(actual_A)
  sw <- make_weights(synthetic_A, synthetic_B); aw <- make_weights(actual_A, actual_B)
  # Actual partial window requires leveraged long, both hedge ETFs, UVXY and GLD. CTA can be cash initially.
  partial_day <- max(vapply(px[c(lev, "UPRO", "SPXS", "UVXY", "GLD", "BIL")], function(z) as.numeric(first(index(z))), numeric(1)))
  full_day <- max(partial_day, as.numeric(first(index(px[["CTA"]]))))
  partial_day <- as.Date(partial_day, origin="1970-01-01"); full_day <- as.Date(full_day, origin="1970-01-01")
  synthetic <- backtest(synthetic_A, sw$neutral, dates[1]); actual_partial <- backtest(actual_A, aw$neutral, partial_day)
  actual <- backtest(actual_A, aw$neutral, full_day); original <- backtest(actual_A, aw$core, full_day)
  matched_synthetic <- synthetic$returns[index(actual$returns)]
  matched_original <- original$returns[index(actual$returns)]
  bench <- R[index(actual$returns), base]; colnames(bench) <- paste0(base, " buy & hold")
  report <- rbind("Synthetic beta-neutral, extended"=stats(synthetic$returns),
                  "Actual beta-neutral, pre-CTA cash"=stats(actual_partial$returns),
                  "Synthetic beta-neutral, matched"=stats(matched_synthetic),
                  "Actual beta-neutral, matched"=stats(actual$returns),
                  "Original unhedged, matched"=stats(matched_original),
                  "Buy-and-hold, matched"=stats(bench))
  cat("\n", base, "strategy (", lev, "): extended", as.character(first(index(synthetic$returns))), "to", as.character(last(index(synthetic$returns))),
      "| actual partial", as.character(first(index(actual_partial$returns))), "| fully actual", as.character(first(index(actual$returns))), "\n", sep=" ")
  print(round(report, 3))
  cat("Actual matched: mean daily gross traded notional", round(mean(as.numeric(actual$turnover))*100, 2), "% NAV;",
      "mean SPXS weight", round(mean(as.numeric(actual$weights[, "HEDGE_SPXS"]))*100, 1), "%; mean UPRO hedge weight",
      round(mean(as.numeric(actual$weights[, "HEDGE_UPRO"]))*100, 1), "%\n")
  chart_data <- merge(actual$returns, matched_synthetic, matched_original, bench, join="inner")
  colnames(chart_data) <- c("Actual beta-neutral", "Synthetic beta-neutral", "Original unhedged", paste0(base, " buy & hold"))
  charts.PerformanceSummary(chart_data, main=paste(base, "beta-neutral comparison: identical dates"))
  invisible(list(metrics=report, actual=actual, actual_partial=actual_partial, synthetic=synthetic,
                 original=original, synthetic_matched=matched_synthetic, benchmark=bench,
                 actual_beta=xts(actual_B, order.by=dates), synthetic_beta=xts(synthetic_B, order.by=dates),
                 signals=xts(state, order.by=dates), prices=P))
}

qqq <- analyze("QQQ", "TQQQ")
spy <- analyze("SPY", "UPRO") # Diagnostic: UPRO + SPXS largely cancels S&P equity exposure, but retains costs.
