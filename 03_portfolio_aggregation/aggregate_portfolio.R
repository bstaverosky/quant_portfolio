rm(list=ls())
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(quadprog)

# [METHODOLOGY] ----------------------------------------------------------- #
# Load every *_output.rds except Adaptive_Leverage_V1 and Josh_Strat (V1).
# V1 analytics/exports remain in source scripts. One return column per sleeve.
# Compare all sleeves and aggregates over the same walk-forward dates.
# Repair legacy V2_ ticker prefixes only for the selected adaptive V2 export.
# risk_method selects true ERC or long-only minimum variance for the other
# 50% of the blend; Kearns MU remains the first 50%. Default is true ERC.
# allocation_mode="Manual" overrides the final blend with named fixed sleeve
# weights in both backtest and live ETF targets. ERC/MinVar and MU remain
# comparison portfolios. Omitted strategies get zero; names must exist and
# weights must be finite, nonnegative, and sum to one. Manual backtests assume
# daily rebalancing to these fixed strategy weights, with no allocator costs.
# Both risk methods use the same past-only covariance window, symmetrized
# and regularized by 1e-8 of average variance for numerical stability.
# ERC solves equal variance contributions via positive coordinate descent;
# MinVar solves w'Sw subject to sum(w)=1 and w>=0 via solve.QP.
# Multiplicative: Kearns, Trading Without Regret (Nomura 2016), slide 3:
#   p_next proportional to p * (1 - eta * loss).
# Fixed scale B: loss=(B-return)/(2B), assuming returns in [-B,B]. No daily
# min/max scaling or clipping. Stop on a bound breach; change B and rerun.
# Unknown horizon: doubling epochs H=1,2,4,...; uniform restart each epoch,
# eta=min(0.5,sqrt(log(N)/H)). This standard wrapper uses no future history
# and yields sublinear additive regret, not a terminal-wealth guarantee.
# Allocate BEFORE observing each day's return; update for the following day.
# Compare raw additive returns and bounded losses on the same evaluation dates.
# No allocator transaction costs are deducted; source costs stay as exported.
# Source: https://www.nomura.com/events/10th-annual-global-quantitative-investment-strategies-conference/resources/upload/Trading_without_Regret_Kearns.pdf
# Daily asset weights supplied by each sleeve are already lagged for that day.
# Latest targets reuse the latest supplied sleeve weights with next-day strategy
# allocations; rerun both source scripts before aggregation. These are latest
# exported exposures, not newly calculated next-session sleeve signals.
# Josh returns remain its existing 3x underlying proxy, not actual levered ETF
# returns; embedded financing/tracking costs are not modeled by that source.
# ----------------------------------------------------------------------- #
options(timeout = 300)
input_dir <- path.expand("~/quant_portfolio/03_portfolio_aggregation/strategy_outputs")
excluded_strategies <- c("Adaptive_Leverage_V1", "Josh_Strat")
allocation_mode <- "Manual"  # "Blend" or "Manual"
manual_strategy_weights <- c(Adaptive_Leverage_V2 = 0.20, Josh_Asset_Family = 0.80)
allocation_mode <- match.arg(allocation_mode, c("Blend", "Manual"))
allocation_label <- if (allocation_mode == "Manual") "Manual" else "Blend"
risk_method <- "ERC"  # "ERC" or "MinVar"
risk_method <- match.arg(risk_method, c("ERC", "MinVar"))
window_length <- 504
mu_return_bound <- 0.25  # predeclared maximum absolute daily sleeve return
strategy_files <- list.files(input_dir, pattern = "_output\\.rds$", full.names = TRUE)
strategy_ids <- sub("_output\\.rds$", "", basename(strategy_files))
keep <- !strategy_ids %in% excluded_strategies
strategy_files <- strategy_files[keep]
strategy_ids <- strategy_ids[keep]
if (!length(strategy_files)) stop("No eligible strategy outputs found in ", input_dir)
cat("\nLoaded strategies: ", paste(strategy_ids, collapse = ", "), "\n", sep = "")

manual_w <- setNames(rep(0, length(strategy_ids)), strategy_ids)
if (allocation_mode == "Manual") {
  mw <- manual_strategy_weights
  if (!length(mw) || is.null(names(mw)) || anyNA(names(mw)) ||
      any(!nzchar(names(mw))) || anyDuplicated(names(mw)) ||
      any(!is.finite(mw)) || any(mw < 0) || abs(sum(mw) - 1) > 1e-6)
    stop("Manual weights must have unique strategy names, be nonnegative and sum to 1")
  missing_ids <- setdiff(names(mw), strategy_ids)
  if (length(missing_ids)) stop("Manual strategies not loaded: ", paste(missing_ids, collapse = ", "))
  manual_w[names(mw)] <- mw
  print(manual_w)
}

strategy_list <- setNames(lapply(strategy_files, readRDS), strategy_ids)
for (nm in strategy_ids) {
  s <- strategy_list[[nm]]
  if (!inherits(s$returns, "xts") || ncol(s$returns) != 1L ||
      !inherits(s$weights, "xts")) stop("Invalid returns/weights export: ", nm)
  tk <- colnames(s$weights)
  if (nm == "Adaptive_Leverage_V2") tk <- sub("^V2_", "", tk)
  if (any(grepl("^V[12]_", tk))) stop("Mixed version weight columns: ", nm)
  tk[tolower(tk) == "cash"] <- "cash"
  if (anyNA(tk) || anyDuplicated(tk) || any(!nzchar(tk))) stop("Invalid tickers: ", nm)
  colnames(s$weights) <- tk
  colnames(s$returns) <- nm
  s$returns <- s$returns[complete.cases(s$returns)]
  if (!nrow(s$returns) || !nrow(s$weights)) stop("Empty export: ", nm)
  W <- coredata(s$weights[complete.cases(s$weights)])
  if (!nrow(W) || any(!is.finite(W)) || any(W < -1e-8) ||
      any(abs(rowSums(W) - 1) > 1e-6)) stop("Invalid asset weights: ", nm)
  strategy_list[[nm]] <- s
}
return_list <- lapply(strategy_list, function(x) x$returns)
aligned_returns <- na.omit(do.call(merge, c(return_list, list(all = FALSE))))
if (nrow(aligned_returns) <= window_length) stop("Insufficient common return history")
if (any(!is.finite(coredata(aligned_returns)))) stop("Nonfinite returns")
print(data.frame(strategy = strategy_ids,
  return_date = sapply(strategy_list, function(s) as.character(last(index(s$returns)))),
  weight_date = sapply(strategy_list, function(s) as.character(last(index(s$weights))))))

# === PREPARE STRATEGY-LEVEL FUNCTIONS ===
risk_covariance <- function(R) {
  S <- cov(as.matrix(R))
  if (any(!is.finite(S))) stop("Invalid covariance matrix")
  S <- (S + t(S)) / 2
  S + diag(max(mean(diag(S)), 1e-12) * 1e-8, ncol(S))
}
calc_erc_weights <- function(S, tol = 1e-8, max_iter = 5000L) {
  n <- ncol(S)
  if (n == 1L) return(1)
  S <- S / mean(diag(S))  # scale does not affect allocation
  x <- rep(1 / sqrt(n), n); budget <- rep(1 / n, n)
  for (iter in seq_len(max_iter)) {
    for (j in seq_len(n)) {
      cross <- sum(S[j, ] * x) - S[j, j] * x[j]
      disc <- sqrt(cross^2 + 4 * S[j, j] * budget[j])
      x[j] <- if (cross >= 0) 2 * budget[j] / (disc + cross) else
        (disc - cross) / (2 * S[j, j])
    }
    rc <- x * as.numeric(S %*% x)
    if (max(abs(rc / sum(rc) - budget)) < tol) return(x / sum(x))
  }
  stop("ERC did not converge; no silent fallback")
}
calc_minvar_weights <- function(S) {
  n <- ncol(S)
  if (n == 1L) return(1)
  fit <- solve.QP(S / mean(diag(S)), rep(0, n),
    cbind(rep(1, n), diag(n)), c(1, rep(0, n)), meq = 1)
  w <- pmax(fit$solution, 0)
  w / sum(w)
}
calc_risk_weights <- function(R) {
  S <- risk_covariance(R)
  if (risk_method == "ERC") calc_erc_weights(S) else calc_minvar_weights(S)
}
calc_multiplicative_weights <- function(weights, returns, eta, bound) {
  if (length(weights) != length(returns) || any(!is.finite(returns)) ||
      any(!is.finite(weights)) || any(weights < 0) || abs(sum(weights)-1)>1e-6)
    stop("Invalid multiplicative inputs")
  if (!is.finite(bound) || bound <= 0 || any(abs(returns) > bound))
    stop("Kearns return bound breached; increase mu_return_bound and rerun")
  if (!is.finite(eta) || eta <= 0 || eta > 0.5) stop("eta must be in (0, 0.5]")
  losses <- (bound - returns) / (2 * bound)
  log_w <- log(weights) + log1p(-eta * losses)
  w <- exp(log_w - max(log_w))
  setNames(w / sum(w), names(weights))
}
generate_trade_list         <- function(target_w, current_shares = NULL, cash_inflow = 0, date = Sys.Date()) {
  require(quantmod)
  
  if (is.null(names(target_w)) || anyDuplicated(names(target_w)) ||
      any(!is.finite(target_w)) || any(target_w < 0) ||
      abs(sum(target_w) - 1) > 1e-6) stop("Invalid target weights")
  if (any(grepl("^V[12]_", names(target_w)))) stop("Version label used as ticker")
  all_tickers <- union(names(target_w), names(current_shares))
  target_w <- setNames(target_w[match(all_tickers, names(target_w))], all_tickers)
  target_w[is.na(target_w)] <- 0
  cur_sh <- setNames(rep(0, length(all_tickers)), all_tickers)
  if (!is.null(current_shares)) cur_sh[names(current_shares)] <- current_shares
  prices <- sapply(all_tickers, function(tk) {
    if (tolower(tk) == "cash") return(1)
    if (target_w[tk] == 0 && cur_sh[tk] == 0) return(1)
    px <- try(getSymbols(tk, from = as.Date(date) - 14, to = as.Date(date) + 1,
      auto.assign = FALSE, src = "yahoo"), silent = TRUE)
    if (inherits(px, "try-error") || !nrow(px)) stop("Price fetch failed for ", tk)
    px <- Cl(px)[index(px) <= as.Date(date)]
    price <- as.numeric(last(na.omit(px)))
    if (length(price) != 1L || !is.finite(price) || price <= 0) stop("Invalid price: ", tk)
    price
  })

  # 3) Compute current market values and total capital
  mkt_val   <- cur_sh * prices
  total_val <- sum(mkt_val, na.rm = TRUE) + cash_inflow
  
  # 4) Compute target dollar allocations
  target_val <- total_val * target_w
  
  # 5) Compute target shares
  target_shares <- sapply(all_tickers, function(tk) {
    if (tolower(tk) == "cash") {
      return(target_val[tk])  # dollars of cash
    }
    floor(target_val[tk] / prices[tk])
  })
  
  # 6) Compute trades
  trade_shares <- target_shares - cur_sh
  
  # 7) Assemble and return
  trades <- data.frame(
    ticker         = all_tickers,
    price          = prices,
    current_shares = as.numeric(cur_sh),
    target_shares  = as.numeric(target_shares),
    trade_shares   = as.numeric(trade_shares),
    stringsAsFactors = FALSE
  )
  # drop zero-trades
  trades[trades$trade_shares != 0, , drop = FALSE]
}

# === SETUP WALK-FORWARD ===
start_index <- window_length + 1
n_periods   <- nrow(aligned_returns)
dates       <- index(aligned_returns)[seq.int(start_index, n_periods)]

# Strategy-level return containers
risk_returns   <- mu_returns   <- blend_returns   <- aligned_returns[0,1,drop=FALSE]

# Strategy-level weight containers
strategy_names     <- colnames(aligned_returns)
risk_weights_xts   <- xts(matrix(NA, nrow = length(dates), ncol = length(strategy_names)), order.by = dates)
mu_weights_xts    <- risk_weights_xts
blend_weights_xts <- risk_weights_xts
colnames(risk_weights_xts)   <- strategy_names
colnames(mu_weights_xts)    <- strategy_names
colnames(blend_weights_xts) <- strategy_names

# ETF-level ticker universe
eft_universe <- unique(unlist(lapply(strategy_list, function(s) colnames(s$weights))))
risk_etf_xts   <- xts(matrix(0, nrow = length(dates), ncol = length(eft_universe)), order.by = dates)
mu_etf_xts    <- risk_etf_xts
blend_etf_xts <- risk_etf_xts
colnames(risk_etf_xts)   <- eft_universe
colnames(mu_etf_xts)    <- eft_universe
colnames(blend_etf_xts) <- eft_universe

# Track Historical Weights #
prev_mu_weights <- rep(1 / ncol(aligned_returns), ncol(aligned_returns))
names(prev_mu_weights) <- colnames(aligned_returns)

# Doubling epochs start at the common evaluation start; no warm-up payoffs.
mu_epoch_length <- 1L
mu_epoch_day <- 0L
mu_regret_bound_loss <- 0

# === WALK-FORWARD LOOP ===
for (i in seq(start_index, n_periods)) {
  print(i)
  date_index <- index(aligned_returns)[i]
  R_window <- aligned_returns[(i - window_length):(i - 1), ]
  R_next   <- aligned_returns[i, , drop=FALSE]
  
  # Strategy-level weights
  w_risk   <- calc_risk_weights(R_window)
  recent_returns <- as.numeric(R_next)  # vector of most recent returns
  if (mu_epoch_day == mu_epoch_length) {
    mu_regret_bound_loss <- mu_regret_bound_loss +
      mu_eta * mu_epoch_length + log(length(prev_mu_weights)) / mu_eta
    mu_epoch_length <- 2L * mu_epoch_length
    mu_epoch_day <- 0L
    prev_mu_weights[] <- 1 / length(prev_mu_weights)
  }
  mu_eta <- min(0.5, sqrt(log(max(2L, length(prev_mu_weights))) / mu_epoch_length))
  w_mu <- prev_mu_weights  # today's allocation only uses prior returns
  prev_mu_weights <- calc_multiplicative_weights(w_mu, recent_returns,
    eta = mu_eta, bound = mu_return_bound)
  mu_epoch_day <- mu_epoch_day + 1L
  w_blend <- if (allocation_mode == "Manual") manual_w else 0.5 * w_risk + 0.5 * w_mu
  
  risk_weights_xts[i - start_index + 1, ]   <- w_risk
  mu_weights_xts[i - start_index + 1, ]    <- w_mu
  blend_weights_xts[i - start_index + 1, ] <- w_blend
  
  # Skip return computation only
  if (i < n_periods) {
    R_next <- aligned_returns[i, , drop = FALSE]
    risk_returns   <- rbind(risk_returns,   xts(as.numeric(R_next %*% w_risk),   order.by = index(R_next)))
    mu_returns    <- rbind(mu_returns,    xts(as.numeric(R_next %*% w_mu),    order.by = index(R_next)))
    blend_returns <- rbind(blend_returns, xts(as.numeric(R_next %*% w_blend), order.by = index(R_next)))
  } else {
    # Optionally compute return for the last date if needed
    R_last <- aligned_returns[i, , drop = FALSE]
    risk_returns   <- rbind(risk_returns,   xts(as.numeric(R_last %*% w_risk),   order.by = index(R_last)))
    mu_returns    <- rbind(mu_returns,    xts(as.numeric(R_last %*% w_mu),    order.by = index(R_last)))
    blend_returns <- rbind(blend_returns, xts(as.numeric(R_last %*% w_blend), order.by = index(R_last)))
  }
  
  
  # ETF-level aggregation
  asset_w_list <- lapply(strategy_list, function(s) {
    wt <- s$weights
    dates_avail <- index(wt)[index(wt) <= date_index]
    if (length(dates_avail) == 0) stop("No asset weights available on ", date_index)
    wt[dates_avail[length(dates_avail)], ]
  })
  names(asset_w_list) <- strategy_names
  
  build_etf <- function(strat_w) {
    etf_w <- setNames(rep(0, length(eft_universe)), eft_universe)
    for (j in seq_along(strat_w)) {
      name_j <- strategy_names[j]
      wj_strat <- strat_w[j]
      aw <- asset_w_list[[name_j]]
      tickers <- colnames(aw)
      for (k in seq_along(aw)) {
        etf_w[tickers[k]] <- etf_w[tickers[k]] + aw[, k] * wj_strat
      }
    }
    if (any(!is.finite(etf_w)) || abs(sum(etf_w) - 1) > 1e-6) stop("Invalid aggregated weights")
    etf_w / sum(etf_w)
  }
  
  erf <- build_etf(w_risk)
  muf <- build_etf(w_mu)
  blf <- build_etf(w_blend)
  
  risk_etf_xts[i - start_index + 1, ]   <- erf
  mu_etf_xts[i - start_index + 1, ]    <- muf
  blend_etf_xts[i - start_index + 1, ] <- blf
}

# === PERFORMANCE ===
portfolios <- merge(
  xts(risk_returns,   order.by=index(risk_returns)),
  xts(mu_returns,    order.by=index(mu_returns)),
  xts(blend_returns, order.by=index(blend_returns))
)
colnames(portfolios) <- c(risk_method, "Multiplicative", allocation_label)

cat("\n=== Walk-Forward Performance Summary ===\n")
perf_table <- rbind(
  Return.annualized(portfolios),
  SharpeRatio.annualized(portfolios)
)
rownames(perf_table) <- c("Annual Return","Annual Sharpe")
print(round(perf_table,3))

# === PLOTS ===
charts.PerformanceSummary(portfolios["2015/"], legend.loc="topleft", main="Walk-Forward Strategy Comparison")
Return.annualized(portfolios["2014/"])

charts.PerformanceSummary(portfolios, legend.loc="topleft", main="Walk-Forward Strategy Comparison")
Return.annualized(portfolios)
SharpeRatio.annualized(portfolios)

charts.PerformanceSummary(portfolios["2025-09-11/"], legend.loc="topleft", main="Walk-Forward Strategy Comparison")
Return.annualized(portfolios["2026-01-01/"])
Return.annualized(portfolios["2024-11-18/2026-09-11"])

# === HISTORICAL WEIGHTS OUTPUT ===
# Strategy-level:
print(tail(risk_weights_xts))
print(tail(mu_weights_xts))
print(tail(blend_weights_xts))
# ETF-level:
print(tail(risk_etf_xts))
print(tail(mu_etf_xts))
print(tail(blend_etf_xts))

# Get trade list #

# Next-day allocation, including an epoch restart if due; latest sleeve exposures.
if (mu_epoch_day == mu_epoch_length) prev_mu_weights[] <- 1 / length(prev_mu_weights)
live_risk <- calc_risk_weights(tail(aligned_returns, window_length))
live_blend <- if (allocation_mode == "Manual") manual_w else 0.5 * live_risk + 0.5 * prev_mu_weights
asset_w_list <- lapply(strategy_list, function(s) last(s$weights))
tw <- build_etf(live_blend)
cat("\nLatest combined target weights (rerun source scripts to refresh):\n")
print(round(tw, 6))

# And you currently hold:
current <- c(cash = 4300,
             QQQ = 0,
             TQQQ = 0,
             SPY = 0,
             UPRO = 281,
             EURL = 277,
             EDC = 78,
             DRN = 0,
             TYD = 0,
             TMF = 0,
             SHNY = 0)

# With $5,000 new cash coming in:
trade_list <- tryCatch(generate_trade_list(
  target_w       = tw,
  current_shares = current,
  cash_inflow    = 0,
  date           = last(index(blend_etf_xts))
), error = function(e) { warning("Trade list unavailable: ", conditionMessage(e)); NULL })

print(trade_list)

# === INDIVIDUAL STRATEGIES VS AGGREGATES ===
# Same dates and starting capital for a fair comparison; source return types
# are preserved (Josh is currently synthetic, Adaptive v2 uses actual ETFs).
individual_returns <- aligned_returns[index(portfolios), , drop = FALSE]
colnames(individual_returns) <- paste0("Strategy: ", colnames(individual_returns))
comparison_returns <- merge(portfolios, individual_returns, all = FALSE)
comparison_returns <- comparison_returns[complete.cases(comparison_returns)]
cat("\n=== Individual Strategies vs Aggregates (Common Period) ===\n")
print(round(table.AnnualizedReturns(comparison_returns), 3))
print(round(maxDrawdown(comparison_returns), 3))
charts.PerformanceSummary(comparison_returns, legend.loc = "topleft",
  main = paste("Individual Strategies vs", risk_method, "/ Kearns MU /", allocation_label))


# === KEARNS ADDITIVE REGRET DIAGNOSTICS ===
expert_R <- as.matrix(individual_returns)
expert_loss <- (mu_return_bound - expert_R) / (2 * mu_return_bound)
mu_R <- as.numeric(portfolios$Multiplicative)
mu_loss <- (mu_return_bound - mu_R) / (2 * mu_return_bound)
T_mu <- length(mu_R)
best_expert <- which.max(colSums(expert_R))
regret_additive <- max(colSums(expert_R)) - sum(mu_R)
regret_loss <- sum(mu_loss) - min(colSums(expert_loss))
# Per epoch: loss regret <= eta * H + log(N)/eta; sum against best fixed expert.
loss_bound <- mu_regret_bound_loss + mu_eta * mu_epoch_day +
  log(ncol(expert_R)) / mu_eta
cat("\n=== Kearns Regret Diagnostics (Additive, Not Compounded) ===\n")
print(data.frame(days = T_mu, best_additive_expert = colnames(expert_R)[best_expert],
  additive_regret = regret_additive, regret_per_day = regret_additive / T_mu,
  bounded_loss_regret = regret_loss, theoretical_loss_bound = loss_bound))
if (regret_loss > loss_bound + 1e-6) stop("Regret bound verification failed")
