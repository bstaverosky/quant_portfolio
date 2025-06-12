rm(list=ls())
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(xtable)
source("/home/brian/Documents/projects/scripts/adhoc_functions.R")

asset <- "SPY"
asset <- getSymbols(asset, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
asset <- asset[,4]
names(asset) <- "Close"
asset$Close <- na.locf(asset$Close)

# Get Benchmark
bmk <- "SPY"
bmk <- getSymbols(bmk, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
bmk <- bmk[,4]
bmk$Benchmark_3X_Buy_and_Hold <- dailyReturn(bmk)*3
names(bmk) <- c("SPY.Close", "Benchmark_3X_Buy_and_Hold")



# asset <- shufflePriceIndex(asset)
# names(asset) <- "Close"



##### USER INPUTS #####
smathres <- 1
volthres <- 1
#volthres <- 0.925
p2hthres <- 0.9
#p2hthres <- 0.925
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
asset$score <- lag(asset$score, k=1)
asset$strat <- ifelse(asset$score==0,asset$Return*0.0,
               ifelse(asset$score==1,asset$Return*0.5,
               ifelse(asset$score==2,asset$Return*0.9,
               ifelse(asset$score==3,asset$Return*1.5,0))))

load("/home/brian/Documents/projects/trading_strategies/mean_reversion/spy_mean_rev_strat.RData")
asset$meanrev <- strategy_returns
asset$strat <- ifelse(asset$score==0 | asset$score==1 | asset$score==2, asset$meanrev*1.5, asset$strat)




## Total Backtest Performance
output <- merge(asset[,c("strat", "Return")])
names(output) <- c("AdaptiveLeverage", "S&P500")
charts.PerformanceSummary(output, main = "Adaptive Leverage Strategy Performance")
charts.PerformanceSummary(output["2020/"])
Return.annualized(output)
SharpeRatio.annualized(output)
