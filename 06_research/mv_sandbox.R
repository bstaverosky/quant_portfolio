# Required Libraries
library(quantmod)
library(PerformanceAnalytics)
library(xts)

# Get Data
symbols <- c("SPY", "EFA", "IEF", "GLD", "DBC")
getSymbols(symbols, from = "1900-01-01", src = "yahoo")

# Daily prices
prices_daily <- na.omit(merge(Ad(SPY), Ad(EFA), Ad(IEF), Ad(GLD), Ad(DBC)))
names(prices_daily) <- symbols

# Weekly endpoints
ep <- endpoints(prices_daily, on = "weeks")

# 1. Z-Score Reversion on SPY
spy_price <- Ad(SPY)
spy_weekly_price <- spy_price[ep]
spy_mean <- rollapply(spy_weekly_price, 12, mean, align = 'right', fill = NA)
spy_sd <- rollapply(spy_weekly_price, 12, sd, align = 'right', fill = NA)
spy_z <- (spy_weekly_price - spy_mean) / spy_sd
z_signal <- Lag(ifelse(spy_z < -1, 1, ifelse(spy_z > 1, -1, 0)))
z_signal_daily <- na.locf(merge(z_signal, spy_price), fromLast = FALSE)[,1]
z_returns_daily <- dailyReturn(spy_price) * z_signal_daily

# 2. RSI-Based Weekly Reversion
spy_rsi <- RSI(spy_weekly_price, n = 2)
rsi_signal <- Lag(ifelse(spy_rsi < 30, 1, ifelse(spy_rsi > 70, -1, 0)))
rsi_signal_daily <- na.locf(merge(rsi_signal, spy_price), fromLast = FALSE)[,1]
rsi_returns_daily <- dailyReturn(spy_price) * rsi_signal_daily

# 3. Bollinger Band Strategy
bbands <- BBands(spy_weekly_price, n = 20, sd = 0.4)
bb_signal <- Lag(ifelse(spy_weekly_price < bbands$dn, 1, 0))
bb_signal_daily <- na.locf(merge(bb_signal, spy_price), fromLast = FALSE)[,1]
bb_returns_daily <- dailyReturn(spy_price) * bb_signal_daily

# 4. Multi-Asset Weekly Reversion
multi_weekly_prices <- prices_daily[ep]
colnames(multi_weekly_prices) <- symbols
z_scores <- na.omit((multi_weekly_prices - rollapply(multi_weekly_prices, 4, mean, align = 'right', fill = NA)) /
                      rollapply(multi_weekly_prices, 4, sd, align = 'right', fill = NA))

# Ensure proper alignment before apply
z_scores <- z_scores[complete.cases(z_scores), ]
min_assets <- apply(z_scores, 1, function(row) names(row)[which.min(row)])
long_signal <- xts(sapply(symbols, function(sym) as.numeric(min_assets == sym)), order.by = index(z_scores))
colnames(long_signal) <- symbols

long_signal_daily <- na.locf(merge(long_signal, prices_daily), fromLast = FALSE)[, 1:5]
returns_daily_multi <- Return.calculate(prices_daily)
multi_strategy_returns_daily <- rowSums(returns_daily_multi * long_signal_daily, na.rm = TRUE) / rowSums(long_signal_daily, na.rm = TRUE)
multi_strategy_returns_daily <- xts(multi_strategy_returns_daily, order.by = index(returns_daily_multi))

# 5. Mean Reversion + Trend Filter
trend_20w <- rollmean(spy_weekly_price, 20, align = 'right', fill = NA)
trend_filter_signal <- Lag(ifelse(spy_weekly_price < trend_20w & spy_z < -1, 1, 0))
trend_signal_daily <- na.locf(merge(trend_filter_signal, spy_price), fromLast = FALSE)[,1]
trend_returns_daily <- dailyReturn(spy_price) * trend_signal_daily

# Combine all strategies (daily returns)
strategy_returns_daily <- na.omit(merge(z_returns_daily, rsi_returns_daily, bb_returns_daily, multi_strategy_returns_daily, trend_returns_daily))
names(strategy_returns_daily) <- c("Z_Score", "RSI", "BBands", "MultiAsset", "TrendFilter")

# Analyze
charts.PerformanceSummary(strategy_returns_daily)
charts.PerformanceSummary(strategy_returns_daily$BBands)
charts.PerformanceSummary(strategy_returns_daily$RSI)
table.Performance(strategy_returns_daily)
