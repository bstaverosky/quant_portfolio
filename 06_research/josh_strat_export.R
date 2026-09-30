#remotes::install_github("joshuaulrich/ftblog")
rm(list=ls())
suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  source("~/quant_portfolio/02_strategies/utils.R")  # adjust path as needed
})

### PARAMETERS ###
use_cash <- T

### FUNCTIONS ###
{
  strat_summary     <- function(returns,original_results = NULL) {
    stats <- table.AnnualizedReturns(returns)
    stats <- rbind(stats,
                   "Worst Drawdown" = -maxDrawdown(returns))
    
    if (!is.null(original_results)) {
      stats <- cbind(original_results, stats)
      colnames(stats)[1] <- "Original"
    }
    stats <- round(stats, 3)
    return(stats)
  }
  chart_performance <- function(R, title = "Performance"){
    stopifnot(all(c("Replication", "OOS") %in% colnames(R)))
    r <- R[, c("Replication", "OOS")]
    p <- chart.CumReturns(r,
                          main = title,
                          main.timespan = FALSE,
                          yaxis.right = TRUE)
    p <- addLegend("topleft", lty = 1, lwd = 1)
    p <- addSeries(r[,1], type = "h", main = "Return")
    p <- addSeries(r[,2], type = "h", on = 0, col = "red")
    p <- addSeries(Drawdowns(r), main = "Drawdown")
    p
  }
  .find_top_momo_columns <- function(returns,n_assets = 5,type = c("relative", "positive", "above average")){
    type <- match.arg(type)
    
    include_cols <- switch(type,
                           "relative"      = rep(TRUE, length(returns)),
                           "positive"      = returns > 0,
                           "above average" = returns > mean(returns, na.rm = TRUE))
    
    which_cols <- which(include_cols)
    
    if (length(which_cols) > 0) {
      # at least 1 column meets the 'type' criteria
      # rank returns from highest to lowest
      momo_rank <- order(returns, decreasing = TRUE)
      # which columns have the highest rank and meet the 'type' criteria?
      top_cols <- momo_rank[momo_rank %in% which_cols]
      # keep the top 'n_assets'
      top_cols <- head(top_cols, n_assets)
    } else {
      top_cols <- integer()
    }
    
    return(top_cols)
  }
  portf_return_momo_equal_risk <- function (returns, n_assets = 5, n_days = 120, n_days_vol = 60, momo_type = c("relative", "positive", "above average"), otype = c("returns", "weights")) {
    
    month_end_i <- endpoints(returns, "months")
    month_end_i <- month_end_i[month_end_i > n_days]
    weights <- returns * NA
    momo_type <- match.arg(momo_type)
    for (i in month_end_i) {
      print(i)
      n_day_returns <- returns[(i - n_days):i, ]
      momentum_returns <- apply(1 + n_day_returns, 2, prod) - 
        1
      weights[i, ] <- 0
      top_cols <- .find_top_momo_columns(momentum_returns, 
                                         n_assets, momo_type)
      if (length(top_cols) >= 2) {
        weights[i, top_cols] <- portf_wts_equal_risk(n_day_returns[,top_cols], n_days_vol)
      }
    }
    weights <- lag(weights)
    weights <- na.locf(weights)
    Rp <- xts(rowSums(returns * weights), index(returns), weights = weights)
    colnames(Rp) <- "R_momo_eq_risk"
    
    if(otype=="returns"){
      return(Rp)
    } else {
      return(weights)
    }
  }
  portf_return_momo_erc_brian <- function (returns, n_assets = 5, n_days = 120, n_days_vol = 60, momo_type = c("relative", "positive", "above average"), otype = c("returns", "weights")) {
    
    month_end_i <- endpoints(returns, "weeks")
    month_end_i <- month_end_i[month_end_i > n_days]
    weights <- returns * NA
    momo_type <- match.arg(momo_type)
    for (i in month_end_i) {
      n_day_returns <- returns[(i - n_days):i, ]
      momentum_returns <- apply(1 + n_day_returns, 2, prod) - 
        1
      weights[i, ] <- 0
      top_cols <- .find_top_momo_columns(momentum_returns, 
                                         n_assets, momo_type)
      if (length(top_cols) >= 2) {
        weights[i, top_cols] <- portf_wts_equal_risk(n_day_returns[,top_cols], n_days_vol)
      }
    }
    weights <- lag(weights)
    weights <- na.locf(weights)
    Rp <- xts(rowSums(returns * weights), index(returns), weights = weights)
    colnames(Rp) <- "R_momo_eq_risk"
    
    if(otype=="returns"){
      return(Rp)
    } else {
      return(weights)
    }
  }
  port_wts_equal_risk <- function (returns, n_days_vol = 60) 
  {
    if (!requireNamespace("FRAPO", quietly = TRUE)) {
      stop("please install the FRAPO package")
    }
    n_day_returns <- last(returns, n_days_vol)
    sigma <- cov(n_day_returns)
    capture.output({
      optim_portf <- FRAPO::PERC(sigma, percentage = FALSE)
    })
    return(FRAPO::Weights(optim_portf))
  }
  
  
}
### LOAD DATA ###
data(aaa_returns, package = "ftblog")

etfs <- c("SPY",
          "VGK",
          "EWJ",
          "EEM",
          "ICF",
          "RWX",
          "IEF",
          "TLT",
          "DBC",
          "GLD"
)

# assets <- lapply(etfs, FUN = function(x){
#   print(x)
#   # df <- getSymbols(x, 
#   #                  src = "yahoo", 
#   #                  from = "1950-01-01", 
#   #                  auto.assign = FALSE,
#   #                  warnings = FALSE, 
#   #                  method = "libcurl", 
#   #                  timeout = 60,
#   #                  connecttimeout=30)
#   
#   df <- getSymbols(x, auto.assign = FALSE)
#   
#   
#   df <- df[,4]
#   names(df) <- gsub(".Close", "", names(df))
#   returns <- Return.calculate(df)
#   returns
# })

assets <- lapply(etfs, FUN = function(x) {
  print(x)
  
  df <- getSymbols(x, auto.assign = FALSE)
  
  df <- df[, 4]
  names(df) <- gsub(".Close", "", names(df))
  
  # Fill missing prices with the previous available observation
  df <- zoo::na.locf(df, na.rm = FALSE)
  
  returns <- Return.calculate(df)
  
  returns
})

assets <- do.call("cbind", assets)
# ==============================================================================
# DATA ASSEMBLY + PYTHON RESEARCH EXPORT
# ==============================================================================
#
# METHODOLOGY NOTE -------------------------------------------------------------
# Purpose:
#   Build the exact historical return panel used by the Josh multi-asset
#   momentum strategy and export it in formats that can be recreated in Python.
#
# Historical construction:
#   1. `aaa_returns` supplies the long-history asset-class return series.
#   2. Beginning after 2023-12-29, actual ETF returns are appended.
#   3. Historical asset-class columns are renamed to the ETF that represents
#      that asset class so the series can be treated as one continuous history.
#
# Current ETF mappings:
#   Cash                         -> Cash
#   U.S. Equity                  -> SPY
#   European Equity             -> VGK
#   Japanese Equity             -> EWJ
#   Emerging Market Equity      -> EEM
#   U.S. Real Estate            -> ICF
#   International Real Estate   -> RWX
#   Intermediate Treasury       -> IEF
#   Long Treasury               -> TLT
#   Commodities                 -> DBC
#   Gold                        -> GLD
#
# Current strategy universe:
#   Cash, SPY, VGK, EEM, ICF, IEF, TLT, GLD
#
# Excluded from the current strategy:
#   EWJ, RWX, DBC
#
# Export files:
#   1. josh_asset_class_etf_returns_long.csv
#        Tidy research file containing:
#        Date / Asset_Class / ETF / Return / Source / Current_Strategy
#
#   2. josh_asset_class_etf_returns_wide.csv
#        Matrix-style return file convenient for Python/pandas.
#
#   3. josh_asset_class_etf_mapping.csv
#        Explicit asset-class-to-ETF mapping.
#
#   4. Later in the script:
#        josh_strategy_reference_returns.csv
#        josh_strategy_reference_weights.csv
#
# Important:
#   The ETF data below intentionally preserves the CURRENT implementation,
#   which uses Yahoo Close rather than Adjusted prices. This lets Python first
#   reproduce the R strategy exactly. We can subsequently test Adjusted/total
#   returns as a separate methodological improvement.
#
# ------------------------------------------------------------------------------


### ASSET CLASS / ETF MAP ###

asset_map <- data.frame(
  Asset_Class = c(
    "Cash",
    "US Equity",
    "European Equity",
    "Japanese Equity",
    "Emerging Market Equity",
    "US Real Estate",
    "International Real Estate",
    "Intermediate Term Treasury",
    "Long Term Treasury",
    "Commodities",
    "Gold"
  ),
  
  ETF = c(
    "Cash",
    "SPY",
    "VGK",
    "EWJ",
    "EEM",
    "ICF",
    "RWX",
    "IEF",
    "TLT",
    "DBC",
    "GLD"
  ),
  
  Current_Strategy = c(
    TRUE,   # Cash
    TRUE,   # SPY
    TRUE,   # VGK
    FALSE,  # EWJ
    TRUE,   # EEM
    TRUE,   # ICF
    FALSE,  # RWX
    TRUE,   # IEF
    TRUE,   # TLT
    FALSE,  # DBC
    TRUE    # GLD
  ),
  
  stringsAsFactors = FALSE
)


### PREPARE HISTORICAL ASSET-CLASS RETURNS ###

if (use_cash) {
  
  returns_hist <- aaa_returns
  
  # Add synthetic cash to ETF-era data
  assets$Cash <- 0
  
  # Put columns in same order as historical data
  assets <- assets[, asset_map$ETF]
  
} else {
  
  # Assume first aaa_returns column is Cash
  returns_hist <- aaa_returns[, -1]
  
  asset_map <- asset_map[asset_map$ETF != "Cash", ]
  
  assets <- assets[, asset_map$ETF]
}


# Rename historical series to their ETF proxy names
names(returns_hist) <- asset_map$ETF


### ETF ERA ###

# Keep exact splice used by current strategy
assets <- assets[index(assets) > as.Date("2023-12-29"), ]


### COMBINE LONG HISTORY + ETF ERA ###

returns <- rbind(
  returns_hist,
  assets
)

returns <- returns[order(index(returns)), ]


### CURRENT STRATEGY UNIVERSE ###

current_universe <- c(
  "Cash",
  "SPY",
  "VGK",
  "EEM",
  "ICF",
  "IEF",
  "TLT",
  "GLD"
)

if (!use_cash)
  current_universe <- current_universe[current_universe != "Cash"]


r_full <- returns[, current_universe]

# Tiny positive cash return retained to reproduce current implementation
if ("Cash" %in% colnames(r_full))
  r_full$Cash <- 0.000000000001


# ==============================================================================
# EXPORT DATA FOR PYTHON RESEARCH
# ==============================================================================

export_dir <- "~/quant_portfolio/python_research_data"

dir.create(
  export_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


### 1. ASSET CLASS / ETF MAPPING ###

write.csv(
  asset_map,
  file.path(
    export_dir,
    "josh_asset_class_etf_mapping.csv"
  ),
  row.names = FALSE
)


### 2. WIDE RETURN MATRIX ###

returns_wide <- data.frame(
  Date = as.Date(index(returns)),
  coredata(returns),
  check.names = FALSE
)

write.csv(
  returns_wide,
  file.path(
    export_dir,
    "josh_asset_class_etf_returns_wide.csv"
  ),
  row.names = FALSE,
  na = ""
)


### 3. LONG / TIDY RESEARCH DATASET ###

etf_start_date <- min(as.Date(index(assets)))

returns_long <- do.call(
  rbind,
  lapply(seq_len(nrow(asset_map)), function(i) {
    
    ticker <- asset_map$ETF[i]
    
    this_return <- as.numeric(returns[, ticker])
    this_date   <- as.Date(index(returns))
    
    source <- ifelse(
      this_date < etf_start_date,
      "aaa_returns_asset_class_history",
      "ETF_market_data"
    )
    
    # Cash in the ETF period is synthetic
    if (ticker == "Cash") {
      source[this_date >= etf_start_date] <- "Synthetic_Cash"
    }
    
    data.frame(
      Date = this_date,
      Asset_Class = asset_map$Asset_Class[i],
      ETF = ticker,
      Return = this_return,
      Source = source,
      Current_Strategy = asset_map$Current_Strategy[i],
      stringsAsFactors = FALSE
    )
  })
)

write.csv(
  returns_long,
  file.path(
    export_dir,
    "josh_asset_class_etf_returns_long.csv"
  ),
  row.names = FALSE,
  na = ""
)


cat(
  "\nPython research data exported to:\n",
  normalizePath(export_dir),
  "\n\n"
)

#### Calculate Strat and Analytics ####

strat_returns <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "returns")
strat_wts     <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "weights")

strat_3x <- strat_returns*3
#strat_3x <- strat_returns

charts.PerformanceSummary(strat_3x)
charts.PerformanceSummary(merge(strat_3x,returns$SPY))

Return.annualized(strat_3x["2024/2025"])
Return.cumulative(strat_3x["2024/2025"])

output <- merge(strat_3x,returns$SPY)
output <- output[complete.cases(output),]
names(output) <- c("Multi-Asset Momentum", "S&P 500")

charts.PerformanceSummary(output)

charts.PerformanceSummary(merge(strat_3x,returns$SPY))
Return.annualized(strat_3x)
Return.annualized(merge(strat_3x,returns$SPY)["2015/2025"])
Return.cumulative(merge(strat_3x,returns$SPY)["2015/2025"])
SharpeRatio.annualized(strat_3x["2015/"])

### LEVERAGED ETFS ###################################################################
# SPY -> UPRO
# VGK -> EURL (europe)
# EWJ -> EZJ (2X) (japan)
# EEM -> EDC (emerging markets)
# ICF -> DRN (us real estate)
# RWX -> nothing (international real estate)
# IEF -> TYD (US treasury 7-10 year)
# TLT -> TMF (20+ year treasury)
# DBC -> nothing (commodities) 
# GLD -> SHNY (gold)

#------EXPORT TO AGGREGATION ------------------------------------------------------------------------------#

names(strat_returns) <- "Josh_Strat"
returns_xts <- strat_returns * 3
returns_xts <- returns_xts[complete.cases(returns_xts),]

names(strat_wts) <- c("cash", "UPRO", "EURL", "EDC", "DRN", "TYD", "TMF", "SHNY")

export_strategy_output(
  strategy_name = "Josh_Strat",
  returns_xts = returns_xts,
  weights_xts = strat_wts,
  output_dir = "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
)

# ==============================================================================
# REFERENCE OUTPUTS FOR R -> PYTHON REPLICATION TEST
# ==============================================================================

reference_returns <- data.frame(
  Date = as.Date(index(strat_returns)),
  Strategy_Return = as.numeric(strat_returns),
  Strategy_Return_3x = as.numeric(strat_returns) * 3
)

write.csv(
  reference_returns,
  file.path(
    export_dir,
    "josh_strategy_reference_returns.csv"
  ),
  row.names = FALSE,
  na = ""
)


reference_weights <- data.frame(
  Date = as.Date(index(strat_wts)),
  coredata(strat_wts),
  check.names = FALSE
)

write.csv(
  reference_weights,
  file.path(
    export_dir,
    "josh_strategy_reference_weights.csv"
  ),
  row.names = FALSE,
  na = ""
)



















