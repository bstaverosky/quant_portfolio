# ==============================================================================
# JOSH MULTI-ASSET MOMENTUM: ORIGINAL + ASSET-FAMILY-AWARE
# ==============================================================================

# region METHODOLOGY
# ORIGINAL:
# - Monthly rebalance; 120-day momentum; top 3 above cross-sectional average.
# - 42-day ERC; lag weights 1 trading day; 3x research return.
#
# ASSET-FAMILY-AWARE / V2:
# - Rank 63/126/189-day momentum and average the ranks.
# - Keep only assets above the cross-sectional average rank.
# - Equities (SPY/VGK/EEM): 2 of 63/126/252-day returns > 0; max 2.
# - Rates (IEF/TLT): both 126/252-day returns > 0; max 1.
# - Gold (GLD): both 126/252-day returns > 0.
# - REITs (ICF): no extra gate.
# - Up to 5 assets; 42-day ERC; lag 1 day; hold to next month.
#
# AUDIT / LIVE-SIGNAL SAFETY:
# - The final partial month is NOT treated as a completed month-end.
# - audit_original: latest completed original signal by asset.
# - audit_family: latest completed V2 signal, gates, ranks, and exclusion reason.
# - audit_data: recent data-quality / return checks.
# - family_decision_history: last 12 completed V2 selections.
# - family_audit_history: last 12 completed V2 signal tables.
# - plot_signal_audit(): 4-panel visual audit of the latest completed signal.
#
# DATA:
# - aaa_returns supplies long history.
# - Yahoo ETF Close returns are appended after 2023-12-29 to preserve the
#   existing implementation.
# endregion


rm(list = ls())

suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  library(xts)
  library(zoo)
  source("~/quant_portfolio/02_strategies/utils.R")
})


# ---- Parameters ---------------------------------------------------------------

use_cash <- TRUE
leverage <- 3
splice_date <- as.Date("2023-12-29")

orig_n <- 3
orig_mom <- 120
orig_vol <- 42

fam_n <- 5
fam_vol <- 42
fam_rank_h <- c(63, 126, 189)
eq_gate_h <- c(63, 126, 252)
rate_gate_h <- c(126, 252)
gold_gate_h <- c(126, 252)

export_dir <- path.expand("~/quant_portfolio/03_portfolio_aggregation/strategy_outputs")


# ---- Helpers -----------------------------------------------------------------

completed_month_ends <- function(R) {
  idx <- endpoints(R, "months")
  idx <- idx[idx > 0]
  if (length(idx) && tail(idx, 1) == nrow(R)) idx <- head(idx, -1)
  idx
}

erc <- function(R, n = 42) {
  S <- cov(tail(R, n))
  capture.output(fit <- FRAPO::PERC(S, percentage = FALSE))
  w <- as.numeric(FRAPO::Weights(fit))
  names(w) <- colnames(R)
  w
}

cumret <- function(R, i, n, asset = NULL) {
  x <- if (is.null(asset)) R[(i - n):i, ] else R[(i - n):i, asset]
  if (is.null(asset)) apply(1 + x, 2, prod) - 1 else as.numeric(prod(1 + x) - 1)
}

asset_family <- function(x) {
  if (x %in% c("SPY", "VGK", "EEM")) return("Equity")
  if (x %in% c("IEF", "TLT")) return("Rates")
  if (x == "ICF") return("REIT")
  if (x == "GLD") return("Gold")
  if (x == "Cash") return("Cash")
  "Other"
}

family_gate <- function(R, i, asset) {
  f <- asset_family(asset)
  if (f %in% c("Cash", "REIT")) return(TRUE)
  if (f == "Equity") return(sum(sapply(eq_gate_h, \(h) cumret(R, i, h, asset)) > 0) >= 2)
  if (f == "Rates")  return(all(sapply(rate_gate_h, \(h) cumret(R, i, h, asset)) > 0))
  if (f == "Gold")   return(all(sapply(gold_gate_h, \(h) cumret(R, i, h, asset)) > 0))
  TRUE
}

family_signal <- function(R, i) {
  M <- sapply(fam_rank_h, \(h) cumret(R, i, h))
  rownames(M) <- colnames(R)
  K <- apply(M, 2, \(x) rank(x, ties.method = "average") / sum(!is.na(x)))
  list(momentum = M, ranks = K, score = rowMeans(K, na.rm = TRUE))
}


# ---- Exact selection + audit logic -------------------------------------------

original_selection <- function(R, i) {
  mom <- cumret(R, i, orig_mom)
  above <- mom > mean(mom, na.rm = TRUE)
  ord <- order(mom, decreasing = TRUE)
  sel <- head(ord[above[ord]], orig_n)

  audit <- data.frame(
    Asset = colnames(R),
    Mom120 = as.numeric(mom),
    CrossSectionMean = mean(mom, na.rm = TRUE),
    AboveMean = as.logical(above),
    Rank = rank(-mom, ties.method = "min"),
    Selected = seq_along(mom) %in% sel,
    row.names = NULL
  )

  list(selected = sel, audit = audit)
}

family_selection <- function(R, i) {
  sig <- family_signal(R, i)
  score <- sig$score
  threshold <- mean(score, na.rm = TRUE)
  cand <- names(sort(score[score > threshold], decreasing = TRUE))

  gate63  <- sapply(colnames(R), \(a) cumret(R, i, 63, a))
  gate126 <- sapply(colnames(R), \(a) cumret(R, i, 126, a))
  gate252 <- sapply(colnames(R), \(a) cumret(R, i, 252, a))
  gate_ok <- sapply(colnames(R), \(a) family_gate(R, i, a))

  audit <- data.frame(
    Asset = colnames(R),
    Family = sapply(colnames(R), asset_family),
    Mom63 = sig$momentum[, 1],
    Mom126 = sig$momentum[, 2],
    Mom189 = sig$momentum[, 3],
    Rank63 = sig$ranks[, 1],
    Rank126 = sig$ranks[, 2],
    Rank189 = sig$ranks[, 3],
    AvgRank = score,
    RankThreshold = threshold,
    AboveAvgRank = score > threshold,
    Gate63 = gate63,
    Gate126 = gate126,
    Gate252 = gate252,
    GatePass = gate_ok,
    Selected = FALSE,
    Reason = ifelse(score > threshold, "eligible", "below_avg_rank"),
    row.names = NULL
  )

  sel <- character()
  n_eq <- 0
  n_rates <- 0

  for (a in cand) {
    j <- match(a, audit$Asset)
    f <- asset_family(a)

    if (!gate_ok[a]) {
      audit$Reason[j] <- "gate_fail"
      next
    }
    if (f == "Equity" && n_eq >= 2) {
      audit$Reason[j] <- "equity_cap"
      next
    }
    if (f == "Rates" && n_rates >= 1) {
      audit$Reason[j] <- "rates_cap"
      next
    }
    if (length(sel) >= fam_n) {
      audit$Reason[j] <- "max_holdings"
      next
    }

    sel <- c(sel, a)
    audit$Selected[j] <- TRUE
    audit$Reason[j] <- "selected"

    if (f == "Equity") n_eq <- n_eq + 1
    if (f == "Rates") n_rates <- n_rates + 1
  }

  list(selected = sel, audit = audit, signal = sig)
}


# ---- Original strategy --------------------------------------------------------

run_original <- function(R) {
  idx <- completed_month_ends(R)
  idx <- idx[idx > orig_mom]
  W <- R * NA

  for (i in idx) {
    sel <- original_selection(R, i)$selected
    W[i, ] <- 0
    if (length(sel) >= 2) W[i, sel] <- erc(R[(i - orig_mom):i, sel, drop = FALSE], orig_vol)
    if (length(sel) == 1) W[i, sel] <- 1
  }

  W <- na.locf(lag(W), na.rm = FALSE)
  Rp <- xts(rowSums(R * W), index(R))
  colnames(Rp) <- "Original"
  list(returns = Rp, weights = W)
}


# ---- Asset-family-aware strategy ---------------------------------------------

run_family <- function(R) {
  need <- max(c(fam_rank_h, eq_gate_h, rate_gate_h, gold_gate_h))
  idx <- completed_month_ends(R)
  idx <- idx[idx > need]
  W <- R * NA

  for (i in idx) {
    sel <- family_selection(R, i)$selected
    W[i, ] <- 0
    if (length(sel) >= 2) W[i, sel] <- erc(R[(i - fam_vol + 1):i, sel, drop = FALSE], fam_vol)
    if (length(sel) == 1) W[i, sel] <- 1
    if (length(sel) == 0 && "Cash" %in% colnames(R)) W[i, "Cash"] <- 1
  }

  W <- na.locf(lag(W), na.rm = FALSE)
  Rp <- xts(rowSums(R * W), index(R))
  colnames(Rp) <- "Asset_Family"
  list(returns = Rp, weights = W)
}


# ---- Data --------------------------------------------------------------------

data(aaa_returns, package = "ftblog")

etfs <- c("SPY", "VGK", "EWJ", "EEM", "ICF", "RWX", "IEF", "TLT", "DBC", "GLD")

assets <- lapply(etfs, function(x) {
  px <- getSymbols(x, auto.assign = FALSE)[, 4]
  colnames(px) <- x
  Return.calculate(na.locf(px, na.rm = FALSE))
})

assets <- do.call(cbind, assets)

if (use_cash) {
  returns <- aaa_returns
  colnames(returns) <- c("Cash", etfs)
  assets$Cash <- 0
  assets <- assets[, c("Cash", etfs)]
} else {
  returns <- aaa_returns[, -1]
  colnames(returns) <- etfs
}

assets <- assets[index(assets) > splice_date]
returns <- rbind(returns, assets)

universe <- c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD")
if (!use_cash) universe <- setdiff(universe, "Cash")

r_full <- returns[, universe]
if ("Cash" %in% colnames(r_full)) r_full$Cash <- 1e-12


# ---- Run ---------------------------------------------------------------------

orig <- run_original(r_full)
fam  <- run_family(r_full)

comparison <- merge(orig$returns * leverage, fam$returns * leverage, r_full$SPY)
colnames(comparison) <- c("Original Josh", "Asset Family Aware", "SPY")
comparison <- comparison[complete.cases(comparison)]


# ---- Analytics ---------------------------------------------------------------

print(table.AnnualizedReturns(comparison))
print(SharpeRatio.annualized(comparison))
print(maxDrawdown(comparison))

charts.PerformanceSummary(comparison, main = "Original Josh vs Asset-Family-Aware")
charts.PerformanceSummary(comparison["2024/"])
Return.annualized(comparison["2016/2018"])

cat("\nCurrent original weights:\n")
print(round(last(orig$weights), 4))

cat("\nCurrent family-aware weights:\n")
print(round(last(fam$weights), 4))


# ==============================================================================
# SIGNAL / DATA AUDIT
# ==============================================================================

orig_idx_all <- completed_month_ends(r_full)
orig_idx <- tail(orig_idx_all[orig_idx_all > orig_mom], 1)

fam_need <- max(c(fam_rank_h, eq_gate_h, rate_gate_h, gold_gate_h))
fam_idx_all <- completed_month_ends(r_full)
fam_idx <- tail(fam_idx_all[fam_idx_all > fam_need], 1)

latest_data_date <- as.Date(last(index(r_full)))
original_signal_date <- as.Date(index(r_full)[orig_idx])
family_signal_date <- as.Date(index(r_full)[fam_idx])

audit_original <- original_selection(r_full, orig_idx)$audit
audit_family <- family_selection(r_full, fam_idx)$audit

audit_data <- data.frame(
  Asset = colnames(r_full),
  NA_Last252 = sapply(r_full, \(x) sum(is.na(tail(x, 252)))),
  ZeroPct_Last63 = sapply(r_full, \(x) mean(abs(tail(x, 63)) < 1e-14, na.rm = TRUE)),
  Ret1 = sapply(colnames(r_full), \(a) cumret(r_full, nrow(r_full), 1, a)),
  Ret21 = sapply(colnames(r_full), \(a) cumret(r_full, nrow(r_full), 21, a)),
  Ret63 = sapply(colnames(r_full), \(a) cumret(r_full, nrow(r_full), 63, a)),
  Ret126 = sapply(colnames(r_full), \(a) cumret(r_full, nrow(r_full), 126, a)),
  Ret252 = sapply(colnames(r_full), \(a) cumret(r_full, nrow(r_full), 252, a)),
  row.names = NULL
)

hist_idx <- tail(fam_idx_all[fam_idx_all > fam_need], 12)

family_decision_history <- do.call(rbind, lapply(hist_idx, function(i) {
  d <- family_selection(r_full, i)
  data.frame(
    SignalDate = as.Date(index(r_full)[i]),
    Selected = if (length(d$selected)) paste(d$selected, collapse = ", ") else "CASH",
    CashOnly = length(d$selected) == 0
  )
}))

family_audit_history <- do.call(rbind, lapply(hist_idx, function(i) {
  d <- family_selection(r_full, i)$audit
  cbind(SignalDate = as.Date(index(r_full)[i]), d)
}))

risky_cols <- setdiff(colnames(fam$weights), "Cash")
if (length(risky_cols) && sum(as.numeric(last(fam$weights[, risky_cols])), na.rm = TRUE) == 0) {
  warning("V2 is currently 100% Cash. Inspect audit_family$Reason and plot_signal_audit().")
}

cat("\nLatest data date:", as.character(latest_data_date), "\n")
cat("Original signal date:", as.character(original_signal_date), "\n")
cat("V2 signal date:", as.character(family_signal_date), "\n\n")

cat("Latest V2 audit:\n")
print(
  audit_family[
    order(audit_family$AvgRank, decreasing = TRUE),
    c("Asset","Family","Mom63","Mom126","Mom189","AvgRank","AboveAvgRank",
      "Gate63","Gate126","Gate252","GatePass","Selected","Reason")
  ],
  row.names = FALSE
)

cat("\nLast 12 V2 decisions:\n")
print(family_decision_history, row.names = FALSE)

cat("\nData audit:\n")
print(audit_data, row.names = FALSE)


# ---- Visual signal audit ------------------------------------------------------

plot_signal_audit <- function() {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(2, 2), mar = c(7, 4, 3, 1))

  M <- t(as.matrix(audit_family[, c("Mom63", "Mom126", "Mom189")]))
  barplot(
    M, beside = TRUE, names.arg = audit_family$Asset, las = 2,
    main = paste("V2 momentum:", family_signal_date),
    ylab = "Cumulative return", legend.text = c("63d","126d","189d")
  )
  abline(h = 0, lty = 2)

  b <- barplot(
    audit_family$AvgRank, names.arg = audit_family$Asset, las = 2,
    main = "V2 average momentum rank", ylab = "Average percentile rank"
  )
  abline(h = unique(audit_family$RankThreshold), lty = 2, lwd = 2)
  text(b, audit_family$AvgRank, labels = ifelse(audit_family$Selected, "*", ""), pos = 3)

  G <- t(as.matrix(audit_family[, c("Gate63", "Gate126", "Gate252")]))
  barplot(
    G, beside = TRUE, names.arg = audit_family$Asset, las = 2,
    main = "Absolute-momentum gate inputs",
    ylab = "Cumulative return", legend.text = c("63d","126d","252d")
  )
  abline(h = 0, lty = 2)

  barplot(
    audit_original$Mom120, names.arg = audit_original$Asset, las = 2,
    main = paste("Original 120d momentum:", original_signal_date),
    ylab = "Cumulative return"
  )
  abline(h = unique(audit_original$CrossSectionMean), lty = 2, lwd = 2)
}

plot_signal_audit()


# ---- Export ------------------------------------------------------------------

name_map <- c(
  Cash = "cash", SPY = "UPRO", VGK = "EURL", EEM = "EDC",
  ICF = "DRN", IEF = "TYD", TLT = "TMF", GLD = "SHNY"
)

export_one <- function(name, result) {
  R <- result$returns * leverage
  colnames(R) <- name
  R <- R[complete.cases(R)]

  W <- result$weights[index(R)]
  colnames(W) <- unname(name_map[colnames(W)])

  export_strategy_output(
    strategy_name = name,
    returns_xts = R,
    weights_xts = W,
    output_dir = export_dir
  )
}

export_one("Josh_Strat", orig)
export_one("Josh_Asset_Family", fam)
