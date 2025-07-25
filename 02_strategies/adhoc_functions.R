

### PORTFOLIO TURNOVER ###
portfolio_turnover <- function(weights1, weights2) {
  # Calculate the absolute difference between the two weight vectors
  diff <- abs(weights1 - weights2)
  
  # Calculate the sum of the absolute differences
  sum_diff <- sum(diff)
  
  # Calculate the portfolio turnover by dividing the sum of the absolute differences by 2
  turnover <- sum_diff / 2
  
  # Return the portfolio turnover
  return(turnover)
}

mysql_conv <- function(df, fname){
  x <- data.frame(df)
  x$date <- as.Date(row.names(x))
  tsplit <- split(x, x$date)
  
  tmod <- lapply(tsplit, FUN = function(x){
    pdate <- row.names(x)
    x$date <- NULL
    x <- data.frame(t(x))
    x$date <- pdate
    x$ticker <- row.names(x)
    names(x) <- c(fname, "date", "ticker")
    x <- x[,c("date", "ticker", fname)]
    x
  })
  tmod
  tframe <- do.call("rbind", tmod)
  tframe
}

### CONVERT RAW TO PANEL FORM ###
convert_to_panel <- function(fdata, fname, mobs){
  if(fname=="dreturn"|fname=="price"){
    plist <- split(fdata, fdata$date)
  } else {
    y <- split(fdata, row.names(fdata))
    plist <- lapply(y, FUN = function(x){
      #x <- y[[1]]
      qdate <- row.names(x)[[1]]
      x <- data.frame(t(x))
      names(x) <- fname
      x$ticker <- row.names(x)
      x$date <- qdate
      x <- x[,c("date", "ticker", fname)]
      x <- x[!x$ticker=="correlation",]
      x
    })
  }
  plist <- plist[(length(plist)-(mobs-1)):length(plist)]
  plist
}

##### CBIND LIST OF DATAFRAMES WITH DIFFERENT NUMBER OF ROWS #####
cbind_diff_frames <- function(dflist){
  maxrow <- nrow(dflist[[which(sapply(dflist, nrow)==max(sapply(dflist,nrow)))[[1]]]])
  dflistmod <- list()
  for (i in dflist){
    if(length(i)==maxrow){
      print("good")
    } else {
      refdf  <- dflist[[which(sapply(dflist, nrow)==max(sapply(dflist,nrow)))[[1]]]]
      rows <- row.names(refdf)
      ndf  <- row.names(i)
      
      appdf <- data.frame(corr = rep(NA, length(rows[which(!rows %in% ndf)])))
      row.names(appdf) <- rows[which(!rows %in% ndf)]
      names(appdf) <- names(i)
      appdf <- rbind(appdf,i)
      dflistmod[[names(appdf)]] <- appdf
    }
  }
  dflistmod
}

##### ROLLING CORRELATION #####

roll_cor <- function(x, y, window){
  df <- merge(x, y)
  df <- df[complete.cases(df),]
  olst <- list()
  if(nrow(df)>window){
    for (j in window:nrow(df)){
      #print(j)
      jdate <- index(df)[[j]]
      subf <- df[((j-(window-1)):j),]
      cr   <- cor(subf[,1],subf[,2])
      
      outdf <- data.frame(correlation = cr)
      names(outdf) <- names(df)[[2]]
      row.names(outdf) <- jdate
      olst[[jdate]] <- outdf
    }
  } else {
    jdate <- tail(index(df),1)
    outdf <- data.frame(correlation = NA)
    row.names(outdf) <- jdate
    olst[[jdate]] <- outdf
  }
  outdf <- do.call("rbind", olst)
  outdf
}



##### GET ACTIVE RETURN #####
get_act_ret <- function(returns, benchmark){
  actret <- lapply(returns, FUN = function(x){
    df <- merge(x, bmk)
    df <- df[complete.cases(df),]
    df[,1] <- df[,1]-df[,2]
    df[,1]
  })
  actret
}


##### CALCULATE DAILY RETURNS #####
pricetoreturn <- function(prices){
  returns <- lapply(prices, FUN = function(x){
    print(names(x))
    returns <- Return.calculate(x)
    returns
  })
  returns
}

##### DOWNLOAD UNIVERSE PRICES #####
get_closing_prices <- function(symbols){
  prices <- lapply(symbols, FUN = function(x){
    print(x)
    df <- getSymbols(x, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    df <- df[,4]
    names(df) <- gsub(".Close", "", names(df))
    df
  })
  prices
}


##### GET SP500 DATA #####

get_sp500_data <- function(){
  sp500_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
  symbols_table <- sp500_wiki %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table()
  symbols_table <- symbols_table[[1]]
  symbols <- as.character(symbols_table$Symbol)
  symbols <- gsub("\\.","-", symbols) 
  symbols_table 
}

##### GET RETURNS FUNCTION #####
get_returns <- function(symbols){
  lapply(symbols, FUN = function(x){
    print(x)
    df <- getSymbols(x, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    df <- df[,4]
    names(df) <- gsub(".Close", "", names(df))
    returns <- Return.calculate(df)
    returns
  })
}



##### WINSORIZE FUNCTION #####

winsorize <- function(x, upper=.90, lower=.10){
  top <- as.numeric(quantile(x, upper))
  btm <- as.numeric(quantile(x, lower))
  nv <- sapply(x, FUN = function(n){
    ifelse(n>top,top,
           ifelse(n<btm,btm,n))
  })
  nv
}

##### STANDARDIZE FUNCTION #####

standardize <- function(x){
  a  <- mean(x, na.rm=TRUE)
  s  <- sd(x,na.rm=TRUE)
  dv <- sapply(x, FUN = function(n){
    (n-a)/s
  })
}

##### HITRATE FUNCTION #####
get_hitrate <- function(actual, predicted){
  sum(ifelse(sign(actual)==sign(predicted),1,0))/length(predicted)
}

##### BASERATE FUNCTION #####
get_baserate <- function(actual, predicted){
  sum(ifelse(rep(1,length(predicted))==sign(actual),1,0))/length(actual)
}

##### STEP FORWARD WINZORIZE AND Z SCORE FUNCTION #####

# stepforward_winzorize_standardize <- function(asset, exvec){
#   output <- lapply(1:nrow(asset), FUN = function(i){
#     print(paste0("winzorizing and standardizing row ", i))
#     asset[1:i,!names(asset) %in% exvec] <- apply(asset[1:i,!names(asset) %in% exvec], 2, winsorize)
#     asset[1:i,!names(asset) %in% exvec] <- apply(asset[1:i,!names(asset) %in% exvec], 2, scale, center=TRUE, scale=TRUE)
#     asset[i,]
#   })
#   outframe <- do.call("rbind", output)
#   outframe
# }

##### STEP FORWARD WINZORIZE AND Z SCORE FUNCTION #####

stepforward_winzorize_standardize <- function(..., asset, exvec, start=1){
  output <- lapply(start:nrow(asset), FUN = function(i){
    print(paste0("winzorizing and standardizing row ", i))
    asset[1:i,!names(asset) %in% exvec] <- apply(asset[1:i,!names(asset) %in% exvec], 2, winsorize)
    asset[1:i,!names(asset) %in% exvec] <- apply(asset[1:i,!names(asset) %in% exvec], 2, scale, center=TRUE, scale=TRUE)
    asset[i,]
  })
  outframe <- do.call("rbind", output)
  outframe
}

##### TRADING FACTOR REGRESSION ANALYSIS #####
create_trading_factor_regressions <- function(df, excols){
  factors <- names(df)[!names(df) %in% excols]
  factor.perf.list <- lapply(factors, FUN = function(x){
    regout <- summary(lm(df[,"Return"] ~ df[,x], data = df))
    plot(df[,x], df[,"Return"], pch=20, ylab = "1-Month Forward Return", xlab = x)
    abline(lm(df[,"Return"]~df[,x]), col="red")
    p <- recordPlot()
    list(regout, p)
  })
  factor.perf.list
}

##### GET WINLOSS RATIOS #####

get_winloss_ratios <- function(asset){
  # Get win/loss ratio
  asset$inout <- NA
  x <- 0
  for(i in 1:(nrow(asset)-1)){
    if(is.na(asset[i,"signal"][[1]])){
      asset[i, "inout"] <- NA
    } else if (asset[i,"signal"][[1]]==1){
      asset[i,"inout"] <- x
    } else if (asset[i,"signal"][[1]]==0){
      asset[i, "inout"] <- NA
      if(asset[i+1,"signal"][[1]]!=-1){
        x <- x + 1
      } else {}
    }
  }
  tlist <- split(asset, asset$inout)
  trets <- sapply(tlist, FUN = function(x){
    x <- xts(x)
    Return.cumulative(x$strat)
  })
  # Win Rate
  winrate <- length(which(trets>0))/(length(which(trets<0))+length(which(trets>0)))
  # Total Win/Loss Ratio:
  winloss_ratio <- length(which(trets>0))/length(which(trets<0))
  # Win/Loss with Mean Return
  mean_winloss_ratio <- (length(which(trets>0))/length(trets) * mean(trets[which(trets>0)]))/(length(which(trets<0))/length(trets)*abs(mean(trets[which(trets<0)])))
  
  output <- data.frame(winrate = winrate,
                       loserate = 1-length(which(trets>0))/(length(which(trets<0))+length(which(trets>0))),
                       AveWinReturn = mean(trets[which(trets>0)]),
                       AveLoseReturn = mean(trets[which(trets<0)]),
                       winloss_ratio = winloss_ratio,
                       mean_winloss_ratio = mean_winloss_ratio,
                       winloss_spread = (length(which(trets>0))/length(trets)*mean(trets[which(trets>0)]))+(length(which(trets<0))/length(trets)*mean(trets[which(trets<0)])))
}

##### Get data for asset and create technical factors and winzorize/standarize #####
# get_factor_data <- function(){
#   ##### SCRIPT DESCRIPTION ######################
#   # The purpose of this script is to create a winzorized and 
#   # zcored data set on a step forward basis
#   ###############################################
#   require(TTR)
#   require(PerformanceAnalytics)
#   require(quantmod)
#   library(caret)
#   library(Metrics)
#   library(coefplot)
#   source("/home/bstaverosky/Documents/projects/R/adhoc_functions.R")
#   
#   symbols <- c("SPY")
#   orig.data <- getSymbols(symbols, from = '1990-01-01-01', auto.assign = FALSE)
#   
#   etflist <- lapply(list(orig.data), FUN = function(x){
#     tic <- as.character(substr(names(x)[[1]],1,3))
#     names(x) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
#     x$BBANDS_DN_20   <- BBands(x[,c("High","Low","Close")], n=20)[,1]
#     x$BBANDS_MAVG_20 <- BBands(x[,c("High","Low","Close")], n=20)[,2]
#     x$BBANDS_UP_20   <- BBands(x[,c("High","Low","Close")], n=20)[,3]
#     x$BBANDS_PCTB_20 <- BBands(x[,c("High","Low","Close")], n=20)[,4]
#     x$BBANDS_DN_50   <- BBands(x[,c("High","Low","Close")], n=50)[,1]
#     x$BBANDS_MAVG_50 <- BBands(x[,c("High","Low","Close")], n=50)[,2]
#     x$BBANDS_UP_50   <- BBands(x[,c("High","Low","Close")], n=50)[,3]
#     x$BBANDS_PCTB_50 <- BBands(x[,c("High","Low","Close")], n=50)[,4]
#     x$RSI200 <- RSI(x$Close, n=200)
#     x$RSI150 <- RSI(x$Close, n=150)
#     x$RSI100 <- RSI(x$Close, n=100)
#     x$RSI50  <- RSI(x$Close, n=50)
#     x$RSI25  <- RSI(x$Close, n=25)
#     x$RSI14  <- RSI(x$Close, n=14)
#     x$RSI7   <- RSI(x$Close, n=7)
#     x$CMO200 <- CMO(x$Close, n=200)
#     x$CMO150 <- CMO(x$Close, n=150)
#     x$CMO100 <- CMO(x$Close, n=100)
#     x$CMO50  <- CMO(x$Close, n=50)
#     x$CMO25  <- CMO(x$Close, n=25)
#     x$CMO14  <- CMO(x$Close, n=14)
#     x$CMO7   <- CMO(x$Close, n=7)
#     x$EMA200 <- EMA(x$Close, n=200)
#     x$EMA150 <- EMA(x$Close, n=150)
#     x$EMA100 <- EMA(x$Close, n=100)
#     x$EMA50  <- EMA(x$Close, n=50)
#     x$EMA30 <- EMA(x$Close, n=30)
#     x$EMA10 <- EMA(x$Close, n=10)
#     x$EMA5  <- EMA(x$Close, n=5)
#     x$DVI252 <- DVI(x$Close, n=252)
#     x$DVI200 <- DVI(x$Close, n=200)
#     x$DVI150 <- DVI(x$Close, n=150)
#     x$DVI100 <- DVI(x$Close, n=100)
#     x$DVI50  <- DVI(x$Close, n=50)
#     x$DVI25  <- DVI(x$Close, n=25)
#     x$DVI10  <- DVI(x$Close, n=10)
#     x$MOM <- momentum(x$Close, n = 252)
#     x$MOM9M <- momentum(x$Close, n = 189)
#     x$MOM6M <- momentum(x$Close, n = 121)
#     x$MOM3M <- momentum(x$Close, n = 63)
#     x$MOM1M <- momentum(x$Close, n = 21)
#     x$MOM10D <- momentum(x$Close, n = 10)
#     x$MOM5D <- momentum(x$Close, n = 5)
#     x$ROC <- ROC(x$Close, n = 252)
#     x$ROC9M <- ROC(x$Close, n = 189)
#     x$ROC6M <- ROC(x$Close, n = 121)
#     x$ROC3M <- ROC(x$Close, n = 63)
#     x$ROC1M <- ROC(x$Close, n = 21)
#     x$ROC10D <- ROC(x$Close, n = 10)
#     x$ROC5D <- ROC(x$Close, n = 5)
#     x$MACD1 <- MACD(x$Close,12,26,9)[,1]
#     x$MACD2 <- MACD(x$Close,12,26,9)[,2]
#     x$SMA200 <- SMA(x$Close, n=200)
#     x$SMA150 <- SMA(x$Close, n=150)
#     x$SMA100 <- SMA(x$Close, n=100)
#     x$SMA50  <- SMA(x$Close, n=50)
#     x$SMA20  <- SMA(x$Close, n=20)
#     x$SMA10  <- SMA(x$Close, n=10)
#     x$SMA5   <- SMA(x$Close, n=5)
#     x$sma200_slope <- sapply(seq(nrow(x)), FUN = function(i){
#       if(i>221){
#         (x$SMA200[[i]]-x$SMA200[[i-21]])/21
#       } else {
#         NA
#       }
#     })
#     x$Lag_Close <- lag(x$Close,-1)
#     x$Return <- lag(ROC(x$Lag_Close,21),-21)*100
#     x$Lag_Close <- NULL
#     x$Ticker <- NA
#     x$Ticker <- tic
#     x <- x[complete.cases(x),]
#     x <- as.data.frame(x)
#     x$Date <- as.Date(row.names(x))
#     ##### Convert Factors to Numeric #####
#     tonumeric <- names(x)[!names(x) %in% c("Ticker","Date")]
#     x[,tonumeric] <- apply(x[,tonumeric], 2, as.numeric)
#     x
#   })
#   
#   # Prepare data
#   asset <- etflist[[1]]
#   asset <- asset[order(asset$Date),]
#   to.remove <- c("Ticker", 
#                  "Date", 
#                  "Open",
#                  "High",
#                  "Low",
#                  "Close",
#                  "Volume",
#                  "Adjusted")
#   asset <- asset[,-which(names(asset) %in% to.remove)]
#   
#   ##### STEP FORWARD WINZORIZE AND Z SCORE FUNCTION #####
#   
#   exvec <- c("Date", "Return")
#   output <- stepforward_winzorize_standardize(asset, exvec)
#   output
#   
# }

##### GET PRICE DATA FOR ASSET, CALCULATE FACTORS, WINZORIZE & STANDARDIZE #####
# get_factor_data <- function(...){
#   ##### SCRIPT DESCRIPTION ######################
#   # The purpose of this script is to create a winzorized and 
#   # zcored data set on a step forward basis
#   ###############################################
#   require(TTR)
#   require(PerformanceAnalytics)
#   require(quantmod)
#   library(caret)
#   library(Metrics)
#   library(coefplot)
#   #source("/home/bstaverosky/Documents/projects/R/adhoc_functions.R")
#   symbols <- c("SPY")
#   orig.data <- getSymbols(symbols, from = '1990-01-01-01',  auto.assign = FALSE)
#   etflist <- lapply(list(orig.data), FUN = function(x){
#     tic <- as.character(substr(names(x)[[1]],1,3))
#     names(x) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
#     x$BBANDS_DN_20   <- BBands(x[,c("High","Low","Close")], n=20)[,1]
#     x$BBANDS_MAVG_20 <- BBands(x[,c("High","Low","Close")], n=20)[,2]
#     x$BBANDS_UP_20   <- BBands(x[,c("High","Low","Close")], n=20)[,3]
#     x$BBANDS_PCTB_20 <- BBands(x[,c("High","Low","Close")], n=20)[,4]
#     x$BBANDS_DN_50   <- BBands(x[,c("High","Low","Close")], n=50)[,1]
#     x$BBANDS_MAVG_50 <- BBands(x[,c("High","Low","Close")], n=50)[,2]
#     x$BBANDS_UP_50   <- BBands(x[,c("High","Low","Close")], n=50)[,3]
#     x$BBANDS_PCTB_50 <- BBands(x[,c("High","Low","Close")], n=50)[,4]
#     x$RSI200 <- RSI(x$Close, n=200)
#     x$RSI150 <- RSI(x$Close, n=150)
#     x$RSI100 <- RSI(x$Close, n=100)
#     x$RSI50  <- RSI(x$Close, n=50)
#     x$RSI25  <- RSI(x$Close, n=25)
#     x$RSI14  <- RSI(x$Close, n=14)
#     x$RSI7   <- RSI(x$Close, n=7)
#     x$CMO200 <- CMO(x$Close, n=200)
#     x$CMO150 <- CMO(x$Close, n=150)
#     x$CMO100 <- CMO(x$Close, n=100)
#     x$CMO50  <- CMO(x$Close, n=50)
#     x$CMO25  <- CMO(x$Close, n=25)
#     x$CMO14  <- CMO(x$Close, n=14)
#     x$CMO7   <- CMO(x$Close, n=7)
#     x$EMA200 <- EMA(x$Close, n=200)
#     x$EMA150 <- EMA(x$Close, n=150)
#     x$EMA100 <- EMA(x$Close, n=100)
#     x$EMA50  <- EMA(x$Close, n=50)
#     x$EMA30 <- EMA(x$Close, n=30)
#     x$EMA10 <- EMA(x$Close, n=10)
#     x$EMA5  <- EMA(x$Close, n=5)
#     x$DVI252 <- DVI(x$Close, n=252)
#     x$DVI200 <- DVI(x$Close, n=200)
#     x$DVI150 <- DVI(x$Close, n=150)
#     x$DVI100 <- DVI(x$Close, n=100)
#     x$DVI50  <- DVI(x$Close, n=50)
#     x$DVI25  <- DVI(x$Close, n=25)
#     x$DVI10  <- DVI(x$Close, n=10)
#     x$MOM <- momentum(x$Close, n = 252)
#     x$MOM9M <- momentum(x$Close, n = 189)
#     x$MOM6M <- momentum(x$Close, n = 121)
#     x$MOM3M <- momentum(x$Close, n = 63)
#     x$MOM1M <- momentum(x$Close, n = 21)
#     x$MOM10D <- momentum(x$Close, n = 10)
#     x$MOM5D <- momentum(x$Close, n = 5)
#     x$ROC <- ROC(x$Close, n = 252)
#     x$ROC9M <- ROC(x$Close, n = 189)
#     x$ROC6M <- ROC(x$Close, n = 121)
#     x$ROC3M <- ROC(x$Close, n = 63)
#     x$ROC1M <- ROC(x$Close, n = 21)
#     x$ROC10D <- ROC(x$Close, n = 10)
#     x$ROC5D <- ROC(x$Close, n = 5)
#     x$MACD1 <- MACD(x$Close,12,26,9)[,1]
#     x$MACD2 <- MACD(x$Close,12,26,9)[,2]
#     x$SMA200 <- SMA(x$Close, n=200)
#     x$SMA150 <- SMA(x$Close, n=150)
#     x$SMA100 <- SMA(x$Close, n=100)
#     x$SMA50  <- SMA(x$Close, n=50)
#     x$SMA20  <- SMA(x$Close, n=20)
#     x$SMA10  <- SMA(x$Close, n=10)
#     x$SMA5   <- SMA(x$Close, n=5)
#     x$sma200_slope <- sapply(seq(nrow(x)), FUN = function(i){
#       if(i>221){
#         (x$SMA200[[i]]-x$SMA200[[i-21]])/21
#       } else {
#         NA
#       }
#     })
#     x$Lag_Close <- lag(x$Close,-1)
#     x$Return <- lag(ROC(x$Lag_Close,21),-21)*100
#     x$Lag_Close <- NULL
#     x$Ticker <- NA
#     x$Ticker <- tic
#     x <- x[complete.cases(x),]
#     x <- as.data.frame(x)
#     x$Date <- as.Date(row.names(x))
#     ##### Convert Factors to Numeric #####
#     tonumeric <- names(x)[!names(x) %in% c("Ticker","Date")]
#     x[,tonumeric] <- apply(x[,tonumeric], 2, as.numeric)
#     x
#   })
#   
#   # Prepare data
#   assetdf <- etflist[[1]]
#   assetdf <- assetdf[order(assetdf$Date),]
#   to.remove <- c("Ticker", 
#                  "Date", 
#                  "Open",
#                  "High",
#                  "Low",
#                  "Close",
#                  "Volume",
#                  "Adjusted")
#   assetdf <- assetdf[,-which(names(assetdf) %in% to.remove)]
#   
#   ##### STEP FORWARD WINZORIZE AND Z SCORE FUNCTION #####
#   
#   exclude <- c("Date", "Return")
#   output <- stepforward_winzorize_standardize(asset=assetdf, exvec = exclude, start, ...)
#   output
#   
# }

get_factor_data <- function(..., OOS = FALSE){
  ##### SCRIPT DESCRIPTION ######################
  # The purpose of this script is to create a winzorized and 
  # zcored data set on a step forward basis
  ###############################################
  require(TTR)
  require(PerformanceAnalytics)
  require(quantmod)
  library(caret)
  library(Metrics)
  library(coefplot)
  #source("/home/bstaverosky/Documents/projects/R/adhoc_functions.R")
  symbols <- c("SPY")
  orig.data <- getSymbols(symbols, from = '1990-01-01-01',  auto.assign = FALSE)
  etflist <- lapply(list(orig.data), FUN = function(x){
    tic <- as.character(substr(names(x)[[1]],1,3))
    names(x) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    x$BBANDS_DN_20   <- BBands(x[,c("High","Low","Close")], n=20)[,1]
    x$BBANDS_MAVG_20 <- BBands(x[,c("High","Low","Close")], n=20)[,2]
    x$BBANDS_UP_20   <- BBands(x[,c("High","Low","Close")], n=20)[,3]
    x$BBANDS_PCTB_20 <- BBands(x[,c("High","Low","Close")], n=20)[,4]
    x$BBANDS_DN_50   <- BBands(x[,c("High","Low","Close")], n=50)[,1]
    x$BBANDS_MAVG_50 <- BBands(x[,c("High","Low","Close")], n=50)[,2]
    x$BBANDS_UP_50   <- BBands(x[,c("High","Low","Close")], n=50)[,3]
    x$BBANDS_PCTB_50 <- BBands(x[,c("High","Low","Close")], n=50)[,4]
    x$RSI200 <- RSI(x$Close, n=200)
    x$RSI150 <- RSI(x$Close, n=150)
    x$RSI100 <- RSI(x$Close, n=100)
    x$RSI50  <- RSI(x$Close, n=50)
    x$RSI25  <- RSI(x$Close, n=25)
    x$RSI14  <- RSI(x$Close, n=14)
    x$RSI7   <- RSI(x$Close, n=7)
    x$CMO200 <- CMO(x$Close, n=200)
    x$CMO150 <- CMO(x$Close, n=150)
    x$CMO100 <- CMO(x$Close, n=100)
    x$CMO50  <- CMO(x$Close, n=50)
    x$CMO25  <- CMO(x$Close, n=25)
    x$CMO14  <- CMO(x$Close, n=14)
    x$CMO7   <- CMO(x$Close, n=7)
    x$EMA200 <- EMA(x$Close, n=200)
    x$EMA150 <- EMA(x$Close, n=150)
    x$EMA100 <- EMA(x$Close, n=100)
    x$EMA50  <- EMA(x$Close, n=50)
    x$EMA30 <- EMA(x$Close, n=30)
    x$EMA10 <- EMA(x$Close, n=10)
    x$EMA5  <- EMA(x$Close, n=5)
    x$DVI252 <- DVI(x$Close, n=252)
    x$DVI200 <- DVI(x$Close, n=200)
    x$DVI150 <- DVI(x$Close, n=150)
    x$DVI100 <- DVI(x$Close, n=100)
    x$DVI50  <- DVI(x$Close, n=50)
    x$DVI25  <- DVI(x$Close, n=25)
    x$DVI10  <- DVI(x$Close, n=10)
    x$MOM <- momentum(x$Close, n = 252)
    x$MOM9M <- momentum(x$Close, n = 189)
    x$MOM6M <- momentum(x$Close, n = 121)
    x$MOM3M <- momentum(x$Close, n = 63)
    x$MOM1M <- momentum(x$Close, n = 21)
    x$MOM10D <- momentum(x$Close, n = 10)
    x$MOM5D <- momentum(x$Close, n = 5)
    x$ROC <- ROC(x$Close, n = 252)
    x$ROC9M <- ROC(x$Close, n = 189)
    x$ROC6M <- ROC(x$Close, n = 121)
    x$ROC3M <- ROC(x$Close, n = 63)
    x$ROC1M <- ROC(x$Close, n = 21)
    x$ROC10D <- ROC(x$Close, n = 10)
    x$ROC5D <- ROC(x$Close, n = 5)
    x$MACD1 <- MACD(x$Close,12,26,9)[,1]
    x$MACD2 <- MACD(x$Close,12,26,9)[,2]
    x$SMA200 <- SMA(x$Close, n=200)
    x$SMA150 <- SMA(x$Close, n=150)
    x$SMA100 <- SMA(x$Close, n=100)
    x$SMA50  <- SMA(x$Close, n=50)
    x$SMA20  <- SMA(x$Close, n=20)
    x$SMA10  <- SMA(x$Close, n=10)
    x$SMA5   <- SMA(x$Close, n=5)
    x$sma200_slope <- sapply(seq(nrow(x)), FUN = function(i){
      if(i>221){
        (x$SMA200[[i]]-x$SMA200[[i-21]])/21
      } else {
        NA
      }
    })
    x$Lag_Close <- lag(x$Close,-1)
    
    if(OOS==FALSE){
      x$Return <- lag(ROC(x$Lag_Close,21),-21)*100
    } else {
      x$Return <- NULL
    }
    
    x$Lag_Close <- NULL
    x$Ticker <- NA
    x$Ticker <- tic
    x <- x[complete.cases(x),]
    x <- as.data.frame(x)
    x$Date <- as.Date(row.names(x))
    ##### Convert Factors to Numeric #####
    tonumeric <- names(x)[!names(x) %in% c("Ticker","Date")]
    x[,tonumeric] <- apply(x[,tonumeric], 2, as.numeric)
    x
  })
  
  # Prepare data
  assetdf <- etflist[[1]]
  assetdf <- assetdf[order(assetdf$Date),]
  to.remove <- c("Ticker", 
                 "Date", 
                 "Open",
                 "High",
                 "Low",
                 "Close",
                 "Volume",
                 "Adjusted")
  assetdf <- assetdf[,-which(names(assetdf) %in% to.remove)]
  
  ##### STEP FORWARD WINZORIZE AND Z SCORE FUNCTION #####
  
  exclude <- c("Date", "Return")
  if(OOS==TRUE){
    output <- stepforward_winzorize_standardize(asset=assetdf, exvec = exclude, start = nrow(assetdf))
  } else {
    output <- stepforward_winzorize_standardize(asset=assetdf, exvec = exclude, start, ...)
  }
  output
  
}

##### GET THREE FACTOR TIME SERIES MODEL #####

##### USER INPUTS #####
smathres <- 1
volthres <- 1
p2hthres <- 0.9
p2hlb    <- 252
svoldays <- 65
lvoldays <- 252
ssmadays <- 21
lsmadays <- 200
entry    <- 252
exit     <- 252



get_adap_lev_model <- function(asset,
                               smathres,
                               volthres,
                               p2hthres,
                               p2hlb,
                               svoldays,
                               lvoldays,
                               ssmadays,
                               lsmadays,
                               entry,
                               exit){
  
  ##### Factor Computation #####
  
  test <- lapply(seq(nrow(asset)), FUN = function(x){
    if(x<65){stvol <- NA} else {stvol <- sd(diff(log(tail(asset[1:x,"Close"],svoldays))), na.rm = TRUE)}
    if(x<500){ltvol <- NA} else {ltvol <- sd(diff(log(tail(asset[1:x,"Close"],lvoldays))), na.rm = TRUE)}
    
    list(stvol, ltvol)
    #stvol
    
  })
  
  
  
  
  
  
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
      asset[x,"Close"]/max(asset[(x-p2hlb):(x-1),"Close"])
      #asset[x,"Close"]/max(asset[(x-252):(x-1),"Close"])
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
    print(x)
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
  asset$score <- rowSums(asset[,c("smasig","volsig","p2hsig", "dhlsig")])
  
  
  
  
}





