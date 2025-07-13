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

assets <- lapply(etfs, FUN = function(x){
  print(x)
  df <- getSymbols(x, 
                   src = "yahoo", 
                   from = "1950-01-01", 
                   auto.assign = FALSE,
                   warnings = FALSE, 
                   method = "libcurl", 
                   timeout = 60,
                   connecttimeout=30)
  df <- df[,4]
  names(df) <- gsub(".Close", "", names(df))
  returns <- Return.calculate(df)
  returns
})

assets <- do.call("cbind", assets)

asset_names <- c("US.Equity",
                 "European.Equity",
                 "Japanese.Equity",            
                 "Emerging.Market.Equity",
                 "US.Real.Estate",
                 "International.Real.Estate",  
                 "Intermediate.Term.Treasury",
                 "Long.Term.Treasury",
                 "Commodities",
                 "Gold"
) 

asset_names <- c("SPY",
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

#returns <- aaa_returns[, -1]    # no cash

if (use_cash) {
  returns <- aaa_returns
  assets$Cash <- 0
  assets <- assets[,c("Cash", asset_names)]
  asset_names <- c("Cash", asset_names)
}

assets <- assets[(which(index(assets)=="2023-12-29")+1):nrow(assets),]
names(returns) <- asset_names

returns <- rbind(returns, assets)
r_full  <- returns
r_full  <- r_full[,c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD")]
r_full$Cash <- 0.000000000001
r_full <- r_full

#### Calculate Strat and Analytics ####

strat_returns <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "returns")
strat_wts     <- portf_return_momo_equal_risk(r_full, n_assets = 3, n_days = 120, n_days_vol = 42, momo_type = "above average", otype = "weights")

strat_3x <- strat_returns*3

charts.PerformanceSummary(strat_3x)
Return.annualized(strat_3x["2015/"])
SharpeRatio.annualized(strat_3x)

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





















