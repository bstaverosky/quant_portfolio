#remotes::install_github("joshuaulrich/ftblog")
rm(list=ls())

suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  source("~/quant_portfolio/02_strategies/utils.R")
})

### PARAMETERS ###
use_cash <- TRUE

### FUNCTIONS ###
{
  strat_summary <- function(returns, original_results=NULL) {
    stats <- table.AnnualizedReturns(returns)
    stats <- rbind(stats, "Worst Drawdown"=-maxDrawdown(returns))
    if (!is.null(original_results)) {
      stats <- cbind(original_results, stats)
      colnames(stats)[1] <- "Original"
    }
    round(stats, 3)
  }
  
  chart_performance <- function(R, title="Performance") {
    stopifnot(all(c("Replication","OOS") %in% colnames(R)))
    r <- R[,c("Replication","OOS")]
    p <- chart.CumReturns(r, main=title, main.timespan=FALSE, yaxis.right=TRUE)
    p <- addLegend("topleft", lty=1, lwd=1)
    p <- addSeries(r[,1], type="h", main="Return")
    p <- addSeries(r[,2], type="h", on=0, col="red")
    p <- addSeries(Drawdowns(r), main="Drawdown")
    p
  }
  
  .find_top_momo_columns <- function(returns, n_assets=5, type=c("relative","positive","above average")) {
    type <- match.arg(type)
    include_cols <- switch(type, "relative"=rep(TRUE,length(returns)), "positive"=returns > 0, "above average"=returns > mean(returns,na.rm=TRUE))
    which_cols <- which(include_cols)
    if (length(which_cols)>0) {
      momo_rank <- order(returns,decreasing=TRUE)
      top_cols <- momo_rank[momo_rank %in% which_cols]
      top_cols <- head(top_cols,n_assets)
    } else top_cols <- integer()
    top_cols
  }
  
  portf_return_momo_equal_risk <- function(returns, n_assets=5, n_days=120, n_days_vol=60, momo_type=c("relative","positive","above average"), otype=c("returns","weights")) {
    month_end_i <- endpoints(returns,"months")
    month_end_i <- month_end_i[month_end_i > n_days]
    weights <- returns*NA
    momo_type <- match.arg(momo_type)
    
    for (i in month_end_i) {
      n_day_returns <- returns[(i-n_days):i,]
      momentum_returns <- apply(1+n_day_returns,2,prod)-1
      weights[i,] <- 0
      top_cols <- .find_top_momo_columns(momentum_returns,n_assets,momo_type)
      if (length(top_cols)>=2) weights[i,top_cols] <- portf_wts_equal_risk(n_day_returns[,top_cols],n_days_vol)
    }
    
    weights <- lag(weights)
    weights <- na.locf(weights)
    Rp <- xts(rowSums(returns*weights),index(returns),weights=weights)
    colnames(Rp) <- "R_momo_eq_risk"
    
    if (otype=="returns") Rp else weights
  }
  
  portf_return_momo_erc_brian <- function(returns, n_assets=5, n_days=120, n_days_vol=60, momo_type=c("relative","positive","above average"), otype=c("returns","weights")) {
    month_end_i <- endpoints(returns,"weeks")
    month_end_i <- month_end_i[month_end_i > n_days]
    weights <- returns*NA
    momo_type <- match.arg(momo_type)
    
    for (i in month_end_i) {
      n_day_returns <- returns[(i-n_days):i,]
      momentum_returns <- apply(1+n_day_returns,2,prod)-1
      weights[i,] <- 0
      top_cols <- .find_top_momo_columns(momentum_returns,n_assets,momo_type)
      if (length(top_cols)>=2) weights[i,top_cols] <- portf_wts_equal_risk(n_day_returns[,top_cols],n_days_vol)
    }
    
    weights <- lag(weights)
    weights <- na.locf(weights)
    Rp <- xts(rowSums(returns*weights),index(returns),weights=weights)
    colnames(Rp) <- "R_momo_eq_risk"
    
    if (otype=="returns") Rp else weights
  }
  
  port_wts_equal_risk <- function(returns,n_days_vol=60) {
    if (!requireNamespace("FRAPO",quietly=TRUE)) stop("please install the FRAPO package")
    n_day_returns <- last(returns,n_days_vol)
    sigma <- cov(n_day_returns)
    capture.output({ optim_portf <- FRAPO::PERC(sigma,percentage=FALSE) })
    FRAPO::Weights(optim_portf)
  }
  
  get_yahoo_returns <- function(tickers,from="1950-01-01") {
    x <- lapply(tickers,function(s) {
      print(s)
      px <- getSymbols(s,src="yahoo",from=from,auto.assign=FALSE,warnings=FALSE)
      px <- Ad(px)
      colnames(px) <- s
      px <- zoo::na.locf(px,na.rm=FALSE)
      r <- Return.calculate(px)
      colnames(r) <- s
      r
    })
    do.call("merge",x)
  }
}

### LOAD BASE / UNLEVERED DATA ###
data(aaa_returns,package="ftblog")

etfs <- c("SPY","VGK","EWJ","EEM","ICF","RWX","IEF","TLT","DBC","GLD")
assets <- get_yahoo_returns(etfs)

asset_names <- c("SPY","VGK","EWJ","EEM","ICF","RWX","IEF","TLT","DBC","GLD")

if (use_cash) {
  returns <- aaa_returns
  assets$Cash <- 0
  assets <- assets[,c("Cash",asset_names)]
  asset_names <- c("Cash",asset_names)
}

assets <- assets["2023-12-30/"]
names(returns) <- asset_names
returns <- rbind(returns,assets)

r_full <- returns[,c("Cash","SPY","VGK","EEM","ICF","IEF","TLT","GLD")]
r_full$Cash <- 0.000000000001

### GENERATE SIGNALS / WEIGHTS ###
strat_returns <- portf_return_momo_equal_risk(r_full,n_assets=3,n_days=120,n_days_vol=42,momo_type="above average",otype="returns")
strat_wts <- portf_return_momo_equal_risk(r_full,n_assets=3,n_days=120,n_days_vol=42,momo_type="above average",otype="weights")

### SYNTHETIC 3X BACKTEST ###
strat_3x_synthetic <- strat_returns*3
colnames(strat_3x_synthetic) <- "Synthetic_3x"

### ACTUAL 3X ETF BACKTEST ###
# Cash -> Cash
# SPY  -> UPRO
# VGK  -> EURL
# EEM  -> EDC
# ICF  -> DRN
# IEF  -> TYD
# TLT  -> TMF
# GLD  -> SHNY

leveraged_etfs <- c("UPRO","EURL","EDC","DRN","TYD","TMF","SHNY")
leveraged_returns <- get_yahoo_returns(leveraged_etfs,from="2000-01-01")

leveraged_returns$Cash <- 0
leveraged_returns <- leveraged_returns[,c("Cash",leveraged_etfs)]

leveraged_wts <- strat_wts
colnames(leveraged_wts) <- c("Cash",leveraged_etfs)

common_dates <- intersect(index(leveraged_wts),index(leveraged_returns))
actual_wts <- leveraged_wts[common_dates]
actual_rets <- leveraged_returns[common_dates]

valid <- complete.cases(actual_wts) & complete.cases(actual_rets)
actual_wts <- actual_wts[valid]
actual_rets <- actual_rets[valid]

strat_3x_actual <- xts(rowSums(actual_wts*actual_rets),order.by=index(actual_rets))
colnames(strat_3x_actual) <- "Actual_3x_ETFs"

### FULL SYNTHETIC HISTORY ###
charts.PerformanceSummary(strat_3x_synthetic)

Return.annualized(strat_3x_synthetic)
Return.annualized(strat_3x_synthetic["2015/2025"])
Return.cumulative(strat_3x_synthetic["2015/2025"])
SharpeRatio.annualized(strat_3x_synthetic["2015/"])

### SYNTHETIC VS ACTUAL 3X ETFs ###
comparison <- merge(strat_3x_synthetic,strat_3x_actual,returns$SPY)
comparison <- comparison[complete.cases(comparison),]
colnames(comparison) <- c("Synthetic 3x","Actual 3x ETFs","S&P 500")

cat("\nCOMMON COMPARISON PERIOD:\n")
print(c(start=start(comparison),end=end(comparison)))

cat("\nPERFORMANCE SUMMARY:\n")
print(strat_summary(comparison))

charts.PerformanceSummary(comparison)
charts.PerformanceSummary(comparison[,c("Synthetic 3x","Actual 3x ETFs")])

### REALIZED IMPLEMENTATION GAP ###
synthetic_ann <- as.numeric(Return.annualized(comparison[,"Synthetic 3x"]))
actual_ann <- as.numeric(Return.annualized(comparison[,"Actual 3x ETFs"]))
implementation_gap <- actual_ann-synthetic_ann

cat("\nSynthetic 3x CAGR:",round(synthetic_ann*100,2),"%\n")
cat("Actual 3x ETF CAGR:",round(actual_ann*100,2),"%\n")
cat("Actual minus Synthetic:",round(implementation_gap*100,2),"% per year\n")

### YEAR-BY-YEAR COMPARISON ###
annual_comparison <- apply.yearly(comparison[,c("Synthetic 3x","Actual 3x ETFs")],Return.cumulative)
print(round(annual_comparison*100,2))

### CUMULATIVE RELATIVE WEALTH ###
relative_wealth <- cumprod(1+comparison[,"Actual 3x ETFs"])/cumprod(1+comparison[,"Synthetic 3x"])
colnames(relative_wealth) <- "Actual_vs_Synthetic"
chart.TimeSeries(relative_wealth,main="Actual 3x ETFs / Synthetic 3x Wealth",yaxis.right=TRUE)

### 2024-2025 COMPARISON ###
Return.annualized(comparison)
SharpeRatio.annualized(comparison)
Return.cumulative(comparison["2025-09-14/"])

### EXPORT BOTH VERSIONS ###
synthetic_export <- strat_3x_synthetic[complete.cases(strat_3x_synthetic)]
synthetic_wts <- leveraged_wts[index(synthetic_export)]

export_strategy_output(
  strategy_name="Josh_Strat_Synthetic3x",
  returns_xts=synthetic_export,
  weights_xts=synthetic_wts,
  output_dir="/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
)

actual_export <- strat_3x_actual[complete.cases(strat_3x_actual)]
actual_export_wts <- actual_wts[index(actual_export)]

export_strategy_output(
  strategy_name="Josh_Strat_Actual3x",
  returns_xts=actual_export,
  weights_xts=actual_export_wts,
  output_dir="/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
)