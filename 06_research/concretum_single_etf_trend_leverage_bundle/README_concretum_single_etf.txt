CONCRETUM-INSPIRED SINGLE-ETF TREND STRATEGY
=========================================

Run concretum_single_etf_trend_leverage.R in R or RStudio.

Install dependencies once:
  install.packages(c("quantmod", "xts", "zoo", "TTR", "PerformanceAnalytics"))

Edit the USER SETTINGS block at the top of the R script, notably:
  etf_ticker <- "QQQ"          # SPY, XLK, XLE, XBI, etc.
  leverage <- 1.0             # 2.0 = 200% ETF exposure while long
  atr_method <- "paper_proxy" # use "true_atr" as a sensitivity test
  extra_lag_days <- 0L       # signal observed at t close, trade at t+1 close
  trade_bps <- 5
  borrowing_spread <- 0.015

Run: source("concretum_single_etf_trend_leverage.R")

What you get:
 - An extensive collapsible METHODOLOGY section near the top of the R script.
 - An in-memory `backtest` list with daily data, executed trades, and metrics.
 - Strategy versus same-period 1x ETF buy-and-hold and constant daily-reset
   exposure equal to `leverage` (three curves and summary metrics).
 - CSV exports, by default, in concretum_single_etf_results under the R
   working directory (daily_backtest.csv, executed_trades.csv,
   performance_summary.csv).
 - Two charts when run interactively with show_charts = TRUE.

Important:
 - This is a single-ETF ADAPTATION of the Concretum Donchian/Keltner
   entry and exit, not the paper's diversified 48-industry backtest.
 - Paper proxy uses 1.4 * mean absolute daily adjusted CLOSE change as
   an ATR approximation. True ATR mode uses adjusted OHLC instead.
 - The position is rebalanced DAILY and leverage is a synthetic cash/
   borrowing exposure. It does not model the actual returns of TQQQ/UPRO.
 - Signal t is filled at the NEXT closing price, and starts earning returns
   only after that fill. Both exit and entry signals obey extra_lag_days.
 - R has not been available in the generation environment, so the script
   has NOT been executed against historical data here.
