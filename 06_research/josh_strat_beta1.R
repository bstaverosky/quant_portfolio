
# remotes::install_github("joshuaulrich/ftblog")
rm(list = ls())

suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  source("~/quant_portfolio/02_strategies/utils.R")
})

### PARAMETERS ###
use_cash <- TRUE
upro_weight <- 0.33
strat_weight <- 1 - upro_weight

### FUNCTIONS ###
strat_summary <- function(returns, original_results = NULL) {
  stats <- table.AnnualizedReturns(returns)
  stats <- rbind(stats, "Worst Drawdown" = -maxDrawdown(returns))
  if (!is.null(original_results)) {
    stats <- cbind(original_results, stats)
    colnames(stats)[1] <- "Original"
  }
  round(stats, 3)
}

.find_top_momo_columns <- function(returns, n_assets = 5, type = c("relative", "positive", "above average")) {
  type <- match.arg(type)
  include_cols <- switch(type,
                         "relative" = rep(TRUE, length(returns)),
                         "positive" = returns > 0,
                         "above average" = returns > mean(returns, na.rm = TRUE)
  )
  which_cols <- which(include_cols)
  if (length(which_cols) == 0) return(integer())
  momo_rank <- order(returns, decreasing = TRUE)
  head(momo_rank[momo_rank %in% which_cols], n_assets)
}

portf_return_momo_equal_risk <- function(returns, n_assets = 5, n_days = 120, n_days_vol = 60, momo_type = c("relative", "positive", "above average"), otype = c("returns", "weights")) {
  month_end_i <- endpoints(returns, "months")
  month_end_i <- month_end_i[month_end_i > n_days]
  weights <- returns * NA
  momo_type <- match.arg(momo_type)
  otype <- match.arg(otype)
  
  for (i in month_end_i) {
    n_day_returns <- returns[(i - n_days):i, ]
    momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1
    weights[i, ] <- 0
    top_cols <- .find_top_momo_columns(momentum_returns, n_assets, momo_type)
    
    if (length(top_cols) >= 2) {
      weights[i, top_cols] <- portf_wts_equal_risk(n_day_returns[, top_cols], n_days_vol)
    }
  }
  
  weights <- zoo::na.locf(lag(weights), na.rm = FALSE)
  if (otype == "weights") return(weights)
  
  Rp <- xts(rowSums(returns * weights), index(returns))
  names(Rp) <- "R_momo_eq_risk"
  Rp
}

### LOAD HISTORICAL DATA ###
data(aaa_returns, package = "ftblog")

etfs <- c("SPY", "VGK", "EWJ", "EEM", "ICF", "RWX", "IEF", "TLT", "DBC", "GLD")

assets <- lapply(etfs, function(x) {
  print(x)
  df <- getSymbols(x, src = "yahoo", auto.assign = FALSE)
  df <- zoo::na.locf(Ad(df), na.rm = FALSE)
  names(df) <- x
  Return.calculate(df)
})

assets <- do.call("merge", assets)
asset_names <- etfs

if (use_cash) {
  returns <- aaa_returns
  assets$Cash <- 0
  assets <- assets[, c("Cash", asset_names)]
  asset_names <- c("Cash", asset_names)
} else {
  returns <- aaa_returns[, -1]
}

### COMBINE HISTORICAL AND CURRENT RETURNS ###
assets <- assets["2023-12-30/"]
names(returns) <- asset_names
returns <- rbind(returns, assets)

if (!use_cash) returns$Cash <- 0

r_full <- returns[, c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD")]
r_full$Cash <- 0.000000000001

### ORIGINAL UNLEVERED MOMENTUM STRATEGY ###
strat_returns <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "returns")
strat_wts <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "weights")

names(strat_returns) <- "Unlevered_Momentum"

### ACTUAL UPRO RETURNS ###
upro_prices <- getSymbols("UPRO", src = "yahoo", from = "2009-01-01", auto.assign = FALSE)
upro_prices <- zoo::na.locf(Ad(upro_prices), na.rm = FALSE)
upro_actual <- Return.calculate(upro_prices)
names(upro_actual) <- "UPRO_Actual"

### SYNTHETIC UPRO RETURNS: DAILY 3X SPY ###
upro_synthetic <- 3 * r_full$SPY
names(upro_synthetic) <- "UPRO_Synthetic"

### FULL HISTORY: SYNTHETIC UPRO ###
full <- merge(strat_returns, upro_synthetic, join = "inner")
full <- full[complete.cases(full), ]

blend_synthetic <- strat_weight * full$Unlevered_Momentum + upro_weight * full$UPRO_Synthetic
names(blend_synthetic) <- "Blend_Synthetic"

### ACTUAL UPRO: AVAILABLE HISTORY ###
actual <- merge(full, upro_actual, join = "inner")
actual <- actual[complete.cases(actual), ]

blend_actual <- strat_weight * actual$Unlevered_Momentum + upro_weight * actual$UPRO_Actual
names(blend_actual) <- "Blend_Actual"

### MATCH SYNTHETIC RETURNS TO ACTUAL UPRO HISTORY ###
blend_synthetic_matched <- blend_synthetic[index(blend_actual)]
names(blend_synthetic_matched) <- "Blend_Synthetic_Matched"

### PORTFOLIO WEIGHTS ###
blend_wts <- strat_weight * strat_wts[index(blend_synthetic)]
blend_wts$UPRO <- upro_weight
names(blend_wts) <- c("cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD", "UPRO")

blend_wts_actual <- blend_wts[index(blend_actual)]

### FULL HISTORY COMPARISON ###
comparison_full <- merge(blend_synthetic, full$Unlevered_Momentum, full$UPRO_Synthetic, r_full$SPY, join = "inner")
comparison_full <- comparison_full[complete.cases(comparison_full), ]
names(comparison_full) <- c("33% Synthetic UPRO + Momentum", "Unlevered Momentum", "Synthetic UPRO", "S&P 500")

cat("\n========== FULL SYNTHETIC HISTORY ==========\n")
cat("Start:", as.character(first(index(comparison_full))), "\n")
cat("End:", as.character(last(index(comparison_full))), "\n")

charts.PerformanceSummary(comparison_full)
print(round(table.AnnualizedReturns(comparison_full), 3))
print(round(maxDrawdown(comparison_full), 3))
print(round(SharpeRatio.annualized(comparison_full), 3))

### MATCHED HISTORY: ACTUAL VS SYNTHETIC ###
comparison_matched <- merge(blend_actual, blend_synthetic_matched, actual$Unlevered_Momentum, actual$UPRO_Actual, r_full$SPY, join = "inner")
comparison_matched <- comparison_matched[complete.cases(comparison_matched), ]
names(comparison_matched) <- c("33% Actual UPRO + Momentum", "33% Synthetic UPRO + Momentum", "Unlevered Momentum", "Actual UPRO", "S&P 500")

cat("\n========== MATCHED ACTUAL VS SYNTHETIC ==========\n")
cat("Start:", as.character(first(index(comparison_matched))), "\n")
cat("End:", as.character(last(index(comparison_matched))), "\n")

charts.PerformanceSummary(comparison_matched)
print(round(table.AnnualizedReturns(comparison_matched), 3))
print(round(maxDrawdown(comparison_matched), 3))
print(round(SharpeRatio.annualized(comparison_matched), 3))

### PRE-UPRO PERIOD ###
pre_upro <- comparison_full[index(comparison_full) < first(index(blend_actual))]

if (nrow(pre_upro) > 0) {
  cat("\n========== PRE-UPRO SYNTHETIC HISTORY ==========\n")
  cat("Start:", as.character(first(index(pre_upro))), "\n")
  cat("End:", as.character(last(index(pre_upro))), "\n")
  
  charts.PerformanceSummary(pre_upro)
  print(round(table.AnnualizedReturns(pre_upro), 3))
  print(round(maxDrawdown(pre_upro), 3))
  print(round(SharpeRatio.annualized(pre_upro), 3))
}

### ADDITIONAL ANALYTICS ###
Return.annualized(comparison_full)
Return.cumulative(comparison_full)

Return.annualized(comparison_matched)
Return.cumulative(comparison_matched)

charts.PerformanceSummary(comparison_matched["2015/"])
charts.PerformanceSummary(comparison_matched["2024/"])

### EXPORT BOTH STRATEGIES ###
output_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"

export_strategy_output(
  strategy_name = "Josh_Strat_33UPRO_Actual",
  returns_xts = blend_actual,
  weights_xts = blend_wts_actual,
  output_dir = output_dir
)

export_strategy_output(
  strategy_name = "Josh_Strat_33UPRO_Synthetic",
  returns_xts = blend_synthetic,
  weights_xts = blend_wts,
  output_dir = output_dir
)