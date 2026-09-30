ADAPTIVE LEVERAGE RESEARCH EXPORT
Created: 2026-09-25 03:12:20.899412

adaptive_leverage_daily.csv
  Complete feature/signal/position dataset.

sp500_index.csv
  Raw S&P 500 index close used to calculate signals.

tradeable_market_data.csv
  SPY and UPRO OHLCV + adjusted prices.

cash_proxy_irx.csv
  13-week Treasury bill yield for realistic cash returns.

strategy_parameters.csv
  Current model parameters and exposure mapping.

strategy_returns.csv
  Current strategy, S&P 500, and synthetic 3x returns.

IMPORTANT:
fwdret contains future information and must never be used as a live signal.
score_raw is the same-day signal score.
score_trade is the lagged score actually used for trading.
