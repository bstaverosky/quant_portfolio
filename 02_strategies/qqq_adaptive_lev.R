rm(list=ls())
# Install libraries if not installed
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
asset <- "QQQ"
asset <- getSymbols(asset, auto.assign = FALSE, from = "1900-01-01")
asset <- asset[,4]
names(asset) <- "Close"
asset$Close <- na.locf(asset$Close)

# Today's Date
# newrow <- xts(data.frame(Close = 589.00), order.by = (as.Date("2025-06-02")))
# asset  <- rbind(asset,newrow)

##### USER INPUTS #####
smathres <- 1
volthres <- 1
p2hthres <- 0.925
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
bmk <- "QQQ"
bmk <- getSymbols(bmk, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
bmk <- bmk[,4]
bmk$Benchmark_3X_Buy_and_Hold <- dailyReturn(bmk)*3
names(bmk) <- c("QQQ.Close", "Benchmark_3X_Buy_and_Hold")




strat <- merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"])

### CONVERT TO MONTHLY DATA ###

## Total Backtest Performance
output <- merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"])
names(output) <- c("NASDAQ 100 Adaptive Leverage", "NASDAQ 100", "3X NASDAQ 100 Buy and Hold")
charts.PerformanceSummary(output, main = "NASDAQ 100 Adaptive Leverage Strategy Performance")


asset$cash <- 0
asset$QQQ  <- 0
asset$TQQQ <- 0

asset$cash <- ifelse(asset$score == 0, 1,
              ifelse(asset$score == 1, 0.5,
              ifelse(asset$score == 2, 0.1,0)))

asset$QQQ  <- ifelse(asset$score == 0, 0,
              ifelse(asset$score == 1, 0.5,
              ifelse(asset$score == 2, 0.9,0)))

asset$TQQQ  <- ifelse(asset$score == 0, 0,
               ifelse(asset$score == 1, 0,
               ifelse(asset$score == 2, 0,1)))




# strategy_weights <- data.frame(
#   ticker = c("cash","SPY","UPRO"),
#   weight = c(0.6, 0.4)
# )

return_xts <- strat$strat
return_xts <- return_xts[complete.cases(return_xts),]
names(return_xts) <- "Adaptive_Leverage_QQQ"

weights_xts <- asset[,c("cash", "QQQ", "TQQQ")]
weights_xts <- weights_xts[complete.cases(weights_xts),]

latest_weights_df <- tail(weights_xts, 1)


# Assume you already calculated these:
# - daily_returns_xts: an xts object of daily strategy returns
# - weights_xts: an xts object of daily ETF weights

export_strategy_output(
  strategy_name = "Adaptive_Leverage_QQQ",
  returns_xts = return_xts,
  weights_xts = weights_xts,
  output_dir = "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
)









