rm(list = ls())

# Packages
pkgs <- c("quantmod","PerformanceAnalytics","xts","lubridate","knitr",
          "kableExtra","ggplot2","ggthemes","xtable")
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs)

library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(xtable)

source("~/quant_portfolio/02_strategies/utils.R")


###############################################################################
# HELPER FUNCTIONS
###############################################################################

# Exact date alignment, used for daily returns
align_exact <- function(x, dates) {
  loc <- match(as.Date(dates), as.Date(index(x)))
  out <- matrix(NA_real_, length(dates), NCOL(x))
  ok <- !is.na(loc)
  if(any(ok)) out[ok,] <- coredata(x[loc[ok],,drop=FALSE])
  out <- xts(out, order.by=dates)
  colnames(out) <- colnames(x)
  out
}

# Carry forward most recent available observation, used for signals
align_last <- function(x, dates) {
  loc <- findInterval(as.Date(dates), as.Date(index(x)))
  out <- matrix(NA_real_, length(dates), NCOL(x))
  ok <- loc > 0
  if(any(ok)) out[ok,] <- coredata(x[loc[ok],,drop=FALSE])
  out <- xts(out, order.by=dates)
  colnames(out) <- colnames(x)
  out
}


###############################################################################
# LOAD SPY
###############################################################################

asset <- getSymbols("SPY", auto.assign=FALSE, from="1900-01-01")
asset <- asset[,4,drop=FALSE]
names(asset) <- "Close"
asset$Close <- na.locf(asset$Close)

# Manual current price
manual_date <- as.Date("2026-09-15")
asset <- asset[as.Date(index(asset)) != manual_date]
newrow <- xts(data.frame(Close=757.41), order.by=manual_date)
asset <- rbind(asset,newrow)
asset <- asset[order(index(asset))]


###############################################################################
# LOAD UPRO
###############################################################################

UPRO <- getSymbols("UPRO", src="yahoo", from="1900-01-01", auto.assign=FALSE)
UPRO <- Ad(UPRO)
names(UPRO) <- "UPRO.Close"

UPRO_Return <- dailyReturn(UPRO)
names(UPRO_Return) <- "UPRO_Return"


###############################################################################
# LOAD DEFENSIVE ASSETS
###############################################################################

defensive_tickers <- c("TLT","QUAL","SPLV","GLD")

defensive_list <- lapply(defensive_tickers, function(ticker) {
  x <- getSymbols(ticker, src="yahoo", from="1900-01-01", auto.assign=FALSE)
  x <- Ad(x)
  colnames(x) <- ticker
  x
})

defensive_prices <- do.call(merge, c(defensive_list, all=TRUE))
colnames(defensive_prices) <- defensive_tickers


###############################################################################
# USER INPUTS
###############################################################################

smathres <- 1
volthres <- 1
p2hthres <- 0.9

svoldays <- 65
lvoldays <- 252

ssmadays <- 21
lsmadays <- 200

entry <- 252
exit <- 252

defensive_momentum_days <- 126


###############################################################################
# FACTOR COMPUTATION
###############################################################################

asset$stvol <- sapply(seq_len(nrow(asset)), function(x) {
  if(x < 65) NA else sd(diff(log(tail(asset[1:x,"Close"],svoldays))), na.rm=TRUE)
})

asset$ltvol <- sapply(seq_len(nrow(asset)), function(x) {
  if(x < 500) NA else sd(diff(log(tail(asset[1:x,"Close"],lvoldays))), na.rm=TRUE)
})

asset$vol_rat <- sapply(seq_len(nrow(asset)), function(x) {
  if(x >= 400) as.numeric(asset[x,"stvol"]) / as.numeric(asset[x,"ltvol"]) else NA
})

asset$sma_rat <- sapply(seq_len(nrow(asset)), function(x) {
  if(x >= 200) {
    mean(tail(asset[1:x,"Close"],ssmadays)) / mean(tail(asset[1:x,"Close"],lsmadays))
  } else NA
})

asset$p2h <- sapply(seq_len(nrow(asset)), function(x) {
  if(x > 257) as.numeric(asset[x,"Close"]) / max(asset[(x-252):(x-1),"Close"]) else NA
})

asset$dh <- sapply(seq_len(nrow(asset)), function(x) {
  if(x < entry) NA else max(asset[(x-entry):x,"Close"])
})

asset$dl <- sapply(seq_len(nrow(asset)), function(x) {
  if(x < exit) NA else min(asset[(x-exit):x,"Close"])
})


###############################################################################
# N-DAY HIGH / LOW SIGNAL
###############################################################################

asset$dhlsig <- NA

for(x in seq_len(nrow(asset))) {
  if(x < entry) {
    asset[x,"dhlsig"] <- 0
  } else if(as.numeric(asset[x,"Close"]) == as.numeric(asset[x,"dh"]) ||
            as.numeric(asset[x-1,"dhlsig"]) == 1) {
    if(as.numeric(asset[x,"Close"]) < as.numeric(asset[x,"dl"])) {
      asset[x,"dhlsig"] <- 0
    } else {
      asset[x,"dhlsig"] <- 1
    }
  } else {
    asset[x,"dhlsig"] <- 0
  }
}


###############################################################################
# FORWARD RETURN
###############################################################################

asset$fwdret <- sapply(seq_len(nrow(asset)), function(x) {
  if(x >= nrow(asset)-21) 0
  else log(as.numeric(asset[x+22,"Close"]) / as.numeric(asset[x+1,"Close"]))
})


###############################################################################
# DAILY RETURNS
###############################################################################

asset$Return <- dailyReturn(asset$Close)

# Align actual UPRO returns with SPY calendar
asset$UPRO_Return <- align_exact(UPRO_Return,index(asset))[,1]


###############################################################################
# PRIMARY SIGNALS
###############################################################################

asset$smasig <- ifelse(asset$sma_rat > smathres,1,0)
asset$volsig <- ifelse(asset$vol_rat < volthres,1,0)
asset$p2hsig <- ifelse(asset$p2h > p2hthres,1,0)

asset$score <- rowSums(asset[,c("smasig","volsig","p2hsig")])

# Use yesterday's signal
asset$score <- stats::lag(asset$score,k=1)


###############################################################################
# DEFENSIVE MOMENTUM
###############################################################################

# 126-day absolute momentum
defensive_momentum <- defensive_prices / stats::lag(defensive_prices,defensive_momentum_days) - 1
colnames(defensive_momentum) <- defensive_tickers

# Use yesterday's defensive signal
defensive_momentum <- stats::lag(defensive_momentum,k=1)

# Align cleanly to SPY calendar
defensive_momentum_signal <- align_last(defensive_momentum,index(asset))

# Defensive ETF returns
defensive_returns <- Return.calculate(defensive_prices,method="discrete")
colnames(defensive_returns) <- defensive_tickers
defensive_returns <- align_exact(defensive_returns,index(asset))


###############################################################################
# ORIGINAL STRATEGY WEIGHTS
###############################################################################

# Cash portion from original strategy
asset$original_cash_weight <- ifelse(asset$score==0,1,
                                     ifelse(asset$score==1,0.5,
                                            ifelse(asset$score==2,0.1,
                                                   ifelse(asset$score==3,0,NA))))

# SPY portion
asset$SPY <- ifelse(asset$score==0,0,
             ifelse(asset$score==1,0.5,
                           ifelse(asset$score==2,0.9,
                                  ifelse(asset$score==3,0.8,NA))))

# UPRO portion
asset$UPRO <- ifelse(is.na(asset$score),NA,ifelse(asset$score==3,0.2,0))


###############################################################################
# DEFENSIVE BUCKET
#
# ONLY replaces the cash allocation above.
#
# Positive 126-day momentum:
#   equal weight across positive ETFs
#
# All negative:
#   cash
###############################################################################

dw <- matrix(0,nrow=nrow(asset),ncol=5)
colnames(dw) <- c("cash","TLT","QUAL","SPLV","GLD")

for(i in seq_len(nrow(asset))) {
  
  bucket <- as.numeric(asset[i,"original_cash_weight"])
  
  if(is.na(bucket)) {
    dw[i,] <- NA
    next
  }
  
  if(bucket == 0) next
  
  mom <- as.numeric(defensive_momentum_signal[i,])
  positive <- which(!is.na(mom) & mom > 0)
  
  if(length(positive) == 0) {
    dw[i,"cash"] <- bucket
  } else {
    dw[i,defensive_tickers[positive]] <- bucket / length(positive)
  }
}


###############################################################################
# ADD DEFENSIVE WEIGHTS TO ASSET
###############################################################################

asset$cash <- dw[,"cash"]
asset$TLT  <- dw[,"TLT"]
asset$QUAL <- dw[,"QUAL"]
asset$SPLV <- dw[,"SPLV"]
asset$GLD  <- dw[,"GLD"]


###############################################################################
# CHECK WEIGHTS
###############################################################################

weight_columns <- c("cash","SPY","UPRO","TLT","QUAL","SPLV","GLD")
asset$weight_sum <- rowSums(coredata(asset[,weight_columns]),na.rm=TRUE)


###############################################################################
# RETURN MATRIX
###############################################################################

R <- cbind(
  SPY  = as.numeric(asset$Return),
  UPRO = as.numeric(asset$UPRO_Return),
  TLT  = as.numeric(defensive_returns[,"TLT"]),
  QUAL = as.numeric(defensive_returns[,"QUAL"]),
  SPLV = as.numeric(defensive_returns[,"SPLV"]),
  GLD  = as.numeric(defensive_returns[,"GLD"])
)

W <- cbind(
  SPY  = as.numeric(asset$SPY),
  UPRO = as.numeric(asset$UPRO),
  TLT  = as.numeric(asset$TLT),
  QUAL = as.numeric(asset$QUAL),
  SPLV = as.numeric(asset$SPLV),
  GLD  = as.numeric(asset$GLD)
)


###############################################################################
# STRATEGY RETURNS
###############################################################################

# Missing returns only matter if that asset is actually held
W.calc <- W
W.calc[is.na(W.calc)] <- 0

missing_held_return <- rowSums((W.calc > 0) & is.na(R)) > 0

R.calc <- R
R.calc[is.na(R.calc)] <- 0

strategy_return <- rowSums(W.calc * R.calc)
strategy_return[missing_held_return | is.na(as.numeric(asset$score))] <- NA

asset$strat <- xts(strategy_return,order.by=index(asset))


###############################################################################
# PERFORMANCE OUTPUT
###############################################################################

output <- asset[,c("strat","Return","UPRO_Return")]
names(output) <- c(
  "S&P 500 Adaptive Leverage + Defensive Momentum",
  "S&P 500",
  "UPRO Buy and Hold"
)

performance_output <- na.omit(output["2010/"])

charts.PerformanceSummary(
  performance_output,
  main="Adaptive Leverage + Defensive Momentum"
)

charts.PerformanceSummary(
  performance_output[,c("S&P 500 Adaptive Leverage + Defensive Momentum","S&P 500")],
  main="Adaptive Leverage + Defensive Momentum vs SPY"
)

Return.annualized(
  performance_output[,c("S&P 500 Adaptive Leverage + Defensive Momentum","S&P 500")],
  geometric=TRUE
)

SharpeRatio.annualized(
  performance_output[,c("S&P 500 Adaptive Leverage + Defensive Momentum","S&P 500")]
)

maxDrawdown(
  performance_output[,c("S&P 500 Adaptive Leverage + Defensive Momentum","S&P 500")]
)

table.CalendarReturns(
  performance_output[,c("S&P 500 Adaptive Leverage + Defensive Momentum","S&P 500")]
)


###############################################################################
# CURRENT SIGNALS / WEIGHTS
###############################################################################

return_xts <- na.omit(asset$strat)
names(return_xts) <- "Adaptive_Leverage_SPY"

weights_xts <- asset[,weight_columns]
weights_xts <- weights_xts[complete.cases(weights_xts),]

latest_weights_df <- tail(weights_xts,1)
latest_defensive_momentum <- tail(defensive_momentum_signal,1)

cat("\nLatest portfolio weights:\n")
print(latest_weights_df)

cat("\nLatest defensive momentum:\n")
print(latest_defensive_momentum)

cat("\nLatest score / original cash allocation:\n")
print(tail(asset[,c("score","original_cash_weight")],1))

cat("\nLatest weight sum:\n")
print(tail(asset[,c(weight_columns,"weight_sum")],1))


###############################################################################
# EXPORT
###############################################################################

export_strategy_output(
  strategy_name="Adaptive_Leverage_SPY",
  returns_xts=return_xts,
  weights_xts=weights_xts,
  output_dir="/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
)