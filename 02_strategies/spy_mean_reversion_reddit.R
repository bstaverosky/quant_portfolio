# Load necessary libraries
rm(list=ls())
library(quantmod)
library(PerformanceAnalytics)
library(TTR)

# Get SPY data
asset <- getSymbols("SPY", from = "1900-01-01", auto.assign = F)

# Calculate the rolling mean of High minus Low over the last 25 days
HL_mean_25 <- rollapply(HLC(asset)[,2] - HLC(asset)[,3], width = 25, FUN = mean, align = "right", fill = NA)

# Calculate the IBS indicator
IBS <- (Cl(asset) - Lo(asset)) / (Hi(asset) - Lo(asset))

# Calculate the lower band as the rolling High over the last 10 days minus 2.5 x the rolling mean of High minus Low
Rolling_High_10 <- rollapply(Hi(asset), width = 10, FUN = max, align = "right", fill = NA)
Lower_Band <- Rolling_High_10 - 2.5 * HL_mean_25

# Generate signals
signals <- ifelse(Cl(asset) < Lower_Band & IBS < 0.3, 1, 0)
signals <- Lag(signals) # Shift signals to align with the next day's open

# Compute the returns
strategy_returns <- ROC(Cl(asset)) * signals
strategy_returns[which(is.na(strategy_returns))] <- 0

strategy_returns <- strategy_returns * 1

# Define a function to calculate strategy performance
strategy_performance <- function(returns) {
  cumulative_returns <- cumprod(1 + returns) - 1
  cumulative_returns[is.na(cumulative_returns)] <- 0
  return(cumulative_returns)
}

# Calculate the cumulative returns
cumulative_returns <- strategy_performance(strategy_returns)

# Plot the performance
plot(cumulative_returns, type = "l", col = "blue", main = "Trading Strategy Cumulative Returns", xlab = "Date", ylab = "Cumulative Returns")

charts.PerformanceSummary(strategy_returns)
Return.annualized(strategy_returns)
SharpeRatio.annualized(strategy_returns)

save(strategy_returns, file = "/home/brian/Documents/projects/trading_strategies/mean_reversion/spy_mean_rev_strat.RData")


# Add buy and sell signals to the plot
#buy_signals <- which(signals == 1)
#points(index(SPY)[buy_signals], cumulative_returns[buy_signals], col = "green", pch = 16)
