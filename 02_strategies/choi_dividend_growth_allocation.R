# Choi's Dividend and Growth Allocation Strategy (AllocateSmartly Version)
# Requirements: quantmod, PerformanceAnalytics, xts, zoo

library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(zoo)

# Define tickers
equity_etfs <- c("QQQ", "SCHD")           # Growth and Dividend ETFs
defensive_etfs <- c("BIL", "TLT", "PDBC")  # Short-term bonds, long-term bonds, commodities
all_tickers <- c(equity_etfs, defensive_etfs, "^IRX", "^TNX")  # Add yield curve tickers

# Load data from Yahoo (from 2007 to present)
start_date <- as.Date("2007-01-01")
getSymbols(all_tickers, from = start_date)

# Extract 3M and 10Y treasury yields
irx <- Cl(IRX) / 100  # 3-month yield in decimal
tnx <- Cl(TNX) / 100  # 10-year yield in decimal
yield_spread <- tnx - irx

# Calculate monthly prices and returns for all ETFs
daily_prices <- na.omit(merge(Cl(QQQ), Cl(SCHD), Cl(BIL), Cl(TLT), Cl(PDBC)))
monthly_prices <- to.monthly(daily_prices, indexAt = "lastof", OHLC = FALSE)
monthly_returns <- ROC(monthly_prices, type = "discrete")
monthly_returns <- na.omit(monthly_returns)
colnames(monthly_returns) <- c("QQQ", "SCHD", "BIL", "TLT", "PDBC")

# Momentum function: average of 1, 3, 6, 9, 12 month returns
momentum_score <- function(prices) {
  prices <- na.omit(prices)
  r1  <- ROC(prices, n = 1, type = "discrete")
  r3  <- ROC(prices, n = 3, type = "discrete")
  r6  <- ROC(prices, n = 6, type = "discrete")
  r9  <- ROC(prices, n = 9, type = "discrete")
  r12 <- ROC(prices, n = 12, type = "discrete")
  mom <- merge(r1, r3, r6, r9, r12)
  avg_mom <- rowMeans(mom, na.rm = TRUE)
  return(xts(avg_mom, order.by = index(prices)))
}

# Calculate momentum scores for each ETF
momentum_data <- merge(
  momentum_score(Cl(to.monthly(QQQ, indexAt = "lastof", OHLC = FALSE))),
  momentum_score(Cl(to.monthly(SCHD, indexAt = "lastof", OHLC = FALSE))),
  momentum_score(Cl(to.monthly(BIL, indexAt = "lastof", OHLC = FALSE))),
  momentum_score(Cl(to.monthly(TLT, indexAt = "lastof", OHLC = FALSE))),
  momentum_score(Cl(to.monthly(PDBC, indexAt = "lastof", OHLC = FALSE)))
)
colnames(momentum_data) <- c("QQQ", "SCHD", "BIL", "TLT", "PDBC")

# Yield spread signal
yield_spread_monthly <- to.monthly(yield_spread, indexAt = "lastof", OHLC = FALSE)
yield_spread_monthly <- Cl(yield_spread_monthly)
inversion_flag <- yield_spread_monthly < -0.005

# Align data
dates <- index(momentum_data)
weights <- xts(matrix(0, nrow = length(dates), ncol = 5), order.by = dates)
colnames(weights) <- c("QQQ", "SCHD", "BIL", "TLT", "PDBC")

# Decision logic
for (i in 1:nrow(weights)) {
  print(i)
  date <- index(weights)[i]
  inv <- ifelse(!is.na(inversion_flag[date]), inversion_flag[date], FALSE)
  m <- momentum_data[date, ]
  if (!inv) {
    best <- names(which.max(m[c("QQQ", "SCHD")]))
    weights[i, best] <- 1
  } else {
    best <- names(which.max(m[c("BIL", "TLT", "PDBC")]))
    weights[i, best] <- 1
  }
}

# Align returns
monthly_returns_aligned <- monthly_returns[index(weights)]
strategy_returns <- Return.portfolio(monthly_returns_aligned, weights = weights)

# Plot performance
charts.PerformanceSummary(strategy_returns, main = "Choi DGA Strategy (AllocateSmartly Version)")
