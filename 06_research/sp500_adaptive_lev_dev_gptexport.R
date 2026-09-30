rm(list=ls())
# Install libraries if not installed
# TEST - DID UPLOAD FROM LAPTOP WORK?

{
list.of.packages <- c("quantmod", 
                      "PerformanceAnalytics",
                      "xts",
                      "lubridate",
                      "knitr",
                      "kableExtra",
                      "ggplot2",
                      "ggthemes",
                      "xtable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
}
# Load Libraries
{
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(xtable)
#source("/home/brian/Documents/projects/adaptive_leverage/adhoc_functions.R")
source("~/quant_portfolio/02_strategies/utils.R")  # adjust path as needed
}
### LOAD ASSET TO TRADE ###
asset <- "^GSPC"
# asset <- getSymbols(asset, 
#                     src = "yahoo", 
#                     from = "1950-01-01", 
#                     auto.assign = FALSE,
#                     warnings = FALSE, 
#                     method = "libcurl", 
#                     timeout = 60,
#                     connecttimeout=30)

asset <- getSymbols(asset, auto.assign = FALSE, from = "1900-01-01")
asset <- asset[,4]
names(asset) <- "Close"
asset$Close <- na.locf(asset$Close)

# Today's Date
#newrow <- xts(data.frame(Close = 757.41), order.by = (as.Date("2026-09-15")))
#asset  <- rbind(asset,newrow)

##### USER INPUTS #####
smathres <- 1
volthres <- 1
p2hthres <- 0.9
svoldays <- 65
lvoldays <- 252
ssmadays <- 21
lsmadays <- 200
entry    <- 252
exit     <- 252

##### Factor Computation #####
asset$stvol <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<65){
    NA
  } else {
    sd(diff(log(tail(asset[1:x,"Close"],svoldays))), na.rm = TRUE)
  }
})
asset$ltvol <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<500){
    NA
  } else {
    sd(diff(log(tail(asset[1:x,"Close"],lvoldays))), na.rm = TRUE)
  }
})   

asset$vol_rat <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>=400){
    #sign(log(asset[x,"ltvol"]/asset[x,"stvol"]))
    asset[x,"stvol"]/asset[x,"ltvol"]
  } else {
    NA
  }
})


asset$sma_rat <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>=200){
    #sign(log(mean(tail(asset[1:x,"Close"],21))/mean(tail(asset[1:x,"Close"],200))))
    mean(tail(asset[1:x,"Close"],ssmadays))/mean(tail(asset[1:x,"Close"],lsmadays))
  } else {
    NA
  }
})


#Price to all time high ratio
asset$p2h <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>257){
    asset[x,"Close"]/max(asset[(x-252):(x-1),"Close"])
    #ifelse((asset[x,"Close"]/max(asset[(x-100):x,"Close"]))>.9,1,0)
    #ifelse((asset[x,"Close"]/max(asset[1:(x-1),"Close"]))>.9,1,0)
    
  } else {
    NA
  }
})

# N Day High Signal

asset$dh <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<entry){
    NA
  } else {
    max(asset[(x-entry):x,"Close"])
  }
})

asset$dl <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<entry){
    NA
  } else {
    min(asset[(x-exit):x,"Close"])
  }
})

#asset$dh <- lag(asset$dh,1)
#asset$dl <- lag(asset$dl,1)
asset$dhlsig <- NA

for(x in seq(nrow(asset))){
  #print(x)
  if(x<entry){
    asset[x,"dhlsig"] <- 0
  } else if(asset[x,"Close"][[1]]==asset[x,"dh"][[1]]|asset[(x-1),"dhlsig"][[1]]==1) {
    
    if(asset[x,"Close"][[1]]<asset[x,"dl"][[1]]){
      asset[x,"dhlsig"] <- 0
    } else {
      asset[x,"dhlsig"] <- 1
    }
  } else {
    asset[x,"dhlsig"] <- 0
  }
}
#asset$dhlsig <- 1
#asset$dhlsig <- lag(asset$dhlsig, 1)

asset$fwdret <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>=nrow(asset)-21){
    0
  } else {
    log(asset[x+22,"Close"][[1]]/asset[x+1,"Close"][[1]])
  }
})

asset$Return <- dailyReturn(asset$Close)

asset$smasig <- ifelse(asset$sma_rat>smathres,1,0)
asset$volsig <- ifelse(asset$vol_rat<volthres,1,0)
asset$p2hsig <- ifelse(asset$p2h>p2hthres,1,0)
asset$score <- rowSums(asset[,c("smasig","volsig","p2hsig")])
#asset$score <- rowSums(asset[,c("volsig")])

# asset$multiplier <- ifelse(asset$smasig == 1 & asset$volsig == 1 & asset$p2hsig == 1, 3,
#                     ifelse(asset$smasig == 1 & asset$volsig == 1 & asset$p2hsig == 0, 0.9,
#                     ifelse(asset$smasig == 0 & asset$volsig == 1 & asset$p2hsig == 1, 0.9,
#                     ifelse(asset$smasig == 1 & asset$volsig == 0 & asset$p2hsig == 1, 0.9,
#                     ifelse(asset$smasig == 1 & asset$volsig == 0 & asset$p2hsig == 0, 0.5,
#                     ifelse(asset$smasig == 0 & asset$volsig == 0 & asset$p2hsig == 1, 0.5,
#                     ifelse(asset$smasig == 0 & asset$volsig == 0 & asset$p2hsig == 0, 0,
#                     ifelse(asset$smasig == 0 & asset$volsig == 1 & asset$p2hsig == 0, 0,0))))))))
# 
# ##### LAGGED SIGNAL FOR ROBUSTNESS #####
# asset$multiplier <- lag(asset$multiplier, k=1)
# asset$strat <- asset$Return * asset$multiplier
          
##### LAGGED SIGNAL FOR ROBUSTNESS #####
asset$score <- stats::lag(asset$score, k=1)
asset$strat <- ifelse(asset$score==0,asset$Return*0.0,
               ifelse(asset$score==1,asset$Return*0.5,
               ifelse(asset$score==2,asset$Return*0.9,
               ifelse(asset$score==3,asset$Return*3,0))))

# Get Benchmark
bmk <- "SPY"
bmk <- getSymbols(bmk, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
bmk <- bmk[,4]
bmk$Benchmark_3X_Buy_and_Hold <- dailyReturn(bmk)*3
names(bmk) <- c("SPY.Close", "Benchmark_3X_Buy_and_Hold")




strat <- merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"])

### CONVERT TO MONTHLY DATA ###

## Total Backtest Performance
output <- merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"])
names(output) <- c("S&P 500 Adaptive Leverage", "S&P 500", "3X S&P 500 Buy and Hold")
charts.PerformanceSummary(output["2021-07/"], main = "S&P 500 Adaptive Leverage Strategy Performance")
charts.PerformanceSummary(output[,c("S&P 500 Adaptive Leverage", "S&P 500")]["2021-07/"])
SharpeRatio.annualized(output[,c("S&P 500 Adaptive Leverage", "S&P 500")])

asset$cash <- 0
asset$SPY  <- 0
asset$UPRO <- 0

asset$cash <- ifelse(asset$score == 0, 1,
              ifelse(asset$score == 1, 0.5,
              ifelse(asset$score == 2, 0.1,0)))

asset$SPY  <- ifelse(asset$score == 0, 0,
              ifelse(asset$score == 1, 0.5,
              ifelse(asset$score == 2, 0.9,0)))

asset$UPRO  <- ifelse(asset$score == 0, 0,
               ifelse(asset$score == 1, 0,
               ifelse(asset$score == 2, 0,1)))




# strategy_weights <- data.frame(
#   ticker = c("cash","SPY","UPRO"),
#   weight = c(0.6, 0.4)
# )

return_xts <- strat$strat
return_xts <- return_xts[complete.cases(return_xts),]
names(return_xts) <- "Adaptive_Leverage_SPY"

weights_xts <- asset[,c("cash", "SPY", "UPRO")]
weights_xts <- weights_xts[complete.cases(weights_xts),]

latest_weights_df <- tail(weights_xts, 1)


# Assume you already calculated these:
# - daily_returns_xts: an xts object of daily strategy returns
# - weights_xts: an xts object of daily ETF weights

export_strategy_output(
  strategy_name = "Adaptive_Leverage_SPY",
  returns_xts = return_xts,
  weights_xts = weights_xts,
  output_dir = "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
)


# ==============================================================================
# PYTHON RESEARCH EXPORT
# ==============================================================================
# METHODOLOGY
# ------------------------------------------------------------------------------
# Purpose:
#   Export the raw data, engineered features, signals, positions, returns,
#   benchmarks, and model parameters required to:
#
#   1. Reproduce the existing Adaptive Leverage strategy exactly in Python.
#   2. Re-estimate signals from raw data rather than trusting R calculations.
#   3. Test alternative thresholds/lookbacks/leverage schedules.
#   4. Add realistic cash returns, UPRO returns, transaction costs, and slippage.
#   5. Perform walk-forward / out-of-sample / parameter robustness research.
#
# Current strategy:
#   Signals:
#     SMA:    SMA(ssmadays) / SMA(lsmadays) > smathres
#     VOL:    short-term vol / long-term vol < volthres
#     P2H:    Close / prior 252-day high > p2hthres
#
#   Raw score = SMA signal + VOL signal + P2H signal
#
#   Trading score = raw score lagged one trading day.
#
#   Exposure:
#     Score 0 -> 0.0x equity
#     Score 1 -> 0.5x equity
#     Score 2 -> 0.9x equity
#     Score 3 -> 3.0x equity
#
#   Current R backtest assumes:
#     strategy return = today's index return * yesterday's signal exposure
#
# Notes:
#   fwdret is RESEARCH ONLY and contains future information.
#   dhlsig is exported even though it is not currently used in the strategy.
#   Synthetic 3x SPY is not the same as actual UPRO, so actual UPRO is exported.
# ==============================================================================

export_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/python_research/adaptive_leverage"

dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)


# ---- 1. RECONSTRUCT RAW + TRADE SCORES ---------------------------------------

asset$score_raw <- rowSums(
  asset[, c("smasig", "volsig", "p2hsig")],
  na.rm = FALSE
)

# asset$score is already the 1-day lagged score in your existing script
asset$score_trade <- asset$score

asset$multiplier <- ifelse(asset$score_trade == 0, 0.0,
                           ifelse(asset$score_trade == 1, 0.5,
                                  ifelse(asset$score_trade == 2, 0.9,
                                         ifelse(asset$score_trade == 3, 3.0, NA))))


# ---- 2. DAILY MODEL DATA ------------------------------------------------------

model_export <- asset[, c(
  "Close",
  "Return",
  "stvol",
  "ltvol",
  "vol_rat",
  "sma_rat",
  "p2h",
  "dh",
  "dl",
  "dhlsig",
  "fwdret",
  "smasig",
  "volsig",
  "p2hsig",
  "score_raw",
  "score_trade",
  "multiplier",
  "strat",
  "cash",
  "SPY",
  "UPRO"
)]

model_export <- data.frame(
  Date = index(model_export),
  coredata(model_export),
  row.names = NULL
)

write.csv(
  model_export,
  file.path(export_dir, "adaptive_leverage_daily.csv"),
  row.names = FALSE
)


# ---- 3. RAW TRADEABLE MARKET DATA --------------------------------------------

tickers <- c("SPY", "UPRO")

market_list <- lapply(tickers, function(ticker) {
  
  x <- getSymbols(
    ticker,
    src = "yahoo",
    from = "1900-01-01",
    auto.assign = FALSE
  )
  
  x <- data.frame(
    Date     = index(x),
    Open     = as.numeric(Op(x)),
    High     = as.numeric(Hi(x)),
    Low      = as.numeric(Lo(x)),
    Close    = as.numeric(Cl(x)),
    Adjusted = as.numeric(Ad(x)),
    Volume   = as.numeric(Vo(x))
  )
  
  x$Ticker <- ticker
  x
})

market_export <- do.call(rbind, market_list)

market_export <- market_export[, c(
  "Date", "Ticker", "Open", "High", "Low",
  "Close", "Adjusted", "Volume"
)]

write.csv(
  market_export,
  file.path(export_dir, "tradeable_market_data.csv"),
  row.names = FALSE
)


# ---- 4. S&P 500 INDEX SOURCE DATA --------------------------------------------

index_export <- data.frame(
  Date  = index(asset),
  Close = as.numeric(asset$Close)
)

write.csv(
  index_export,
  file.path(export_dir, "sp500_index.csv"),
  row.names = FALSE
)


# ---- 5. CASH / T-BILL PROXY --------------------------------------------------
# Yahoo ^IRX = 13-week Treasury bill yield.
# Export raw yield so Python can determine the appropriate daily cash-return
# methodology rather than hard-coding zero return for cash.

irx <- tryCatch(
  getSymbols(
    "^IRX",
    src = "yahoo",
    from = "1900-01-01",
    auto.assign = FALSE
  ),
  error = function(e) NULL
)

if(!is.null(irx)) {
  
  irx_export <- data.frame(
    Date = index(irx),
    IRX_Yield = as.numeric(Cl(irx))
  )
  
  write.csv(
    irx_export,
    file.path(export_dir, "cash_proxy_irx.csv"),
    row.names = FALSE
  )
}


# ---- 6. STRATEGY PARAMETERS --------------------------------------------------

parameters <- data.frame(
  parameter = c(
    "smathres",
    "volthres",
    "p2hthres",
    "svoldays",
    "lvoldays",
    "ssmadays",
    "lsmadays",
    "entry",
    "exit",
    "score_0_exposure",
    "score_1_exposure",
    "score_2_exposure",
    "score_3_exposure",
    "signal_lag_days"
  ),
  
  value = c(
    smathres,
    volthres,
    p2hthres,
    svoldays,
    lvoldays,
    ssmadays,
    lsmadays,
    entry,
    exit,
    0.0,
    0.5,
    0.9,
    3.0,
    1
  )
)

write.csv(
  parameters,
  file.path(export_dir, "strategy_parameters.csv"),
  row.names = FALSE
)


# ---- 7. BENCHMARK / FINAL RETURNS --------------------------------------------

performance_export <- merge(
  asset[, c("Return", "strat")],
  bmk[, "Benchmark_3X_Buy_and_Hold"]
)

names(performance_export) <- c(
  "SP500_Return",
  "Adaptive_Return",
  "Synthetic_3X_SPY_Return"
)

performance_export <- data.frame(
  Date = index(performance_export),
  coredata(performance_export),
  row.names = NULL
)

write.csv(
  performance_export,
  file.path(export_dir, "strategy_returns.csv"),
  row.names = FALSE
)


# ---- 8. EXPORT MANIFEST ------------------------------------------------------

manifest <- c(
  "ADAPTIVE LEVERAGE RESEARCH EXPORT",
  paste("Created:", Sys.time()),
  "",
  "adaptive_leverage_daily.csv",
  "  Complete feature/signal/position dataset.",
  "",
  "sp500_index.csv",
  "  Raw S&P 500 index close used to calculate signals.",
  "",
  "tradeable_market_data.csv",
  "  SPY and UPRO OHLCV + adjusted prices.",
  "",
  "cash_proxy_irx.csv",
  "  13-week Treasury bill yield for realistic cash returns.",
  "",
  "strategy_parameters.csv",
  "  Current model parameters and exposure mapping.",
  "",
  "strategy_returns.csv",
  "  Current strategy, S&P 500, and synthetic 3x returns.",
  "",
  "IMPORTANT:",
  "fwdret contains future information and must never be used as a live signal.",
  "score_raw is the same-day signal score.",
  "score_trade is the lagged score actually used for trading."
)

writeLines(
  manifest,
  file.path(export_dir, "README.txt")
)

cat("\nPython research export complete:\n", export_dir, "\n")






