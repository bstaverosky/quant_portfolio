# ==============================================================================
# JOSH MULTI-ASSET MOMENTUM: ORIGINAL + ASSET-FAMILY-AWARE
# ==============================================================================

# region METHODOLOGY
# ORIGINAL:
# - Monthly rebalance.
# - 120-day momentum.
# - Hold up to 3 assets with momentum above the cross-sectional average.
# - 42-day ERC weighting.
# - Lag weights 1 trading day.
# - Multiply strategy return by 3 for research comparison.
#
# ASSET-FAMILY-AWARE:
# - Rank assets on 63/126/189-day momentum; average the three ranks.
# - Keep only assets above the cross-sectional average rank.
# - Equities (SPY/VGK/EEM): require 2 of 63/126/252-day returns > 0; max 2.
# - Rates (IEF/TLT): require both 126/252-day returns > 0; max 1.
# - Gold (GLD): require both 126/252-day returns > 0.
# - REITs (ICF): no extra gate.
# - Keep up to 5 assets, weight with 42-day ERC, lag 1 day, hold to next month.
#
# DATA:
# - aaa_returns supplies long history.
# - Yahoo ETF Close returns are appended after 2023-12-29 to preserve your
#   existing implementation exactly.
#
# NOTE:
# - This is the compact integrated version.
# - ERC ensemble, covariance tie-break, and volatility brake are NOT included.
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

export_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"


# ---- Helpers -----------------------------------------------------------------

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

rank_score <- function(R, i) {
  M <- sapply(fam_rank_h, \(h) cumret(R, i, h))
  rownames(M) <- colnames(R)
  rowMeans(apply(M, 2, \(x) rank(x, ties.method = "average") / length(x)))
}


# ---- Original strategy --------------------------------------------------------

run_original <- function(R) {
  idx <- endpoints(R, "months")
  idx <- idx[idx > orig_mom]
  W <- R * NA

  for (i in idx) {
    mom <- cumret(R, i, orig_mom)
    sel <- order(mom, decreasing = TRUE)
    sel <- sel[mom[sel] > mean(mom, na.rm = TRUE)]
    sel <- head(sel, orig_n)

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
  idx <- endpoints(R, "months")
  idx <- idx[idx > need]
  W <- R * NA

  for (i in idx) {
    score <- rank_score(R, i)
    cand <- names(sort(score[score > mean(score, na.rm = TRUE)], decreasing = TRUE))

    sel <- character()
    n_eq <- 0
    n_rates <- 0

    for (a in cand) {
      f <- asset_family(a)
      if (!family_gate(R, i, a)) next
      if (f == "Equity" && n_eq >= 2) next
      if (f == "Rates"  && n_rates >= 1) next

      sel <- c(sel, a)
      if (f == "Equity") n_eq <- n_eq + 1
      if (f == "Rates")  n_rates <- n_rates + 1
      if (length(sel) >= fam_n) break
    }

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

charts.PerformanceSummary(
  comparison,
  main = "Original Josh vs Asset-Family-Aware"
)

charts.PerformanceSummary(comparison["2024/"])
Return.annualized(comparison["2016/2018"])

cat("\nCurrent original weights:\n")
print(round(last(orig$weights), 4))

cat("\nCurrent family-aware weights:\n")
print(round(last(fam$weights), 4))


# ---- Export ------------------------------------------------------------------

name_map <- c(
  Cash = "cash",
  SPY = "UPRO",
  VGK = "EURL",
  EEM = "EDC",
  ICF = "DRN",
  IEF = "TYD",
  TLT = "TMF",
  GLD = "SHNY"
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
