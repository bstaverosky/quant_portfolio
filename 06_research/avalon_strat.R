###############################################################################
# AVALON INDUSTRY / SECTOR MOMENTUM + TAA
# MODIFIED DEFENSIVE MOMENTUM SLEEVE
#
# Strategy:
#
# OFFENSIVE:
#   1. Universe = 9 original SPDR sector ETFs
#   2. Rank monthly using volatility-adjusted momentum
#   3. Hold top 3 sectors
#   4. Risk-on only when:
#        SPY > 200-day SMA
#        SPY realized volatility <= threshold
#   5. Individual sector exits early if price <= 50-day EMA
#
# DEFENSIVE:
#   Universe = IEF, SHY, GLD, CASH
#
#   Calculate 6-month momentum for:
#       IEF
#       SHY
#       GLD
#
#   If all 3 have positive momentum:
#       Equal-weight all 3
#
#   If 2 have positive momentum:
#       50/50
#
#   If 1 has positive momentum:
#       100% that ETF
#
#   If none have positive momentum:
#       100% CASH
#
#   The defensive sleeve is used:
#       - When the overall market regime is risk-off
#       - For any offensive sector slot stopped by its 50-day EMA
#
# BENCHMARK:
#   SPY buy-and-hold
#
# Packages:
#   quantmod
#   xts
#   PerformanceAnalytics
#
# NO dplyr
# NO plyr
###############################################################################


###############################################################################
# 1. PACKAGES
###############################################################################

library(quantmod)
library(xts)
library(PerformanceAnalytics)


###############################################################################
# 2. BACKTEST SETTINGS
###############################################################################

start_date <- as.Date("2017-01-01")
end_date   <- as.Date("2026-07-30")

# Extra history needed for indicators and momentum.
download_start <- as.Date("2015-01-01")


###############################################################################
# 3. OFFENSIVE STRATEGY PARAMETERS
###############################################################################

# Approximately six months.
momentum_days <- 126

# Number of sectors held.
top_n <- 3


# ---------------------------------------------------------------------------
# VOLATILITY-ADJUSTED MOMENTUM
# ---------------------------------------------------------------------------

sector_vol_days <- 63

# Momentum score:
#
#     momentum / volatility^exponent
#
# Higher values penalize volatile sectors more heavily.

vol_penalty_exponent <- 2


# ---------------------------------------------------------------------------
# MARKET REGIME
# ---------------------------------------------------------------------------

spy_sma_days <- 200

spy_vol_days <- 20

# Risk-on requires SPY annualized realized volatility <= this level.

risk_vol_cutoff <- 0.25


# ---------------------------------------------------------------------------
# INDIVIDUAL SECTOR STOP
# ---------------------------------------------------------------------------

sector_ema_days <- 50


###############################################################################
# 4. DEFENSIVE STRATEGY PARAMETERS
###############################################################################

# Momentum horizon for IEF, SHY and GLD.

defensive_momentum_days <- 126


###############################################################################
# 5. TRANSACTION COSTS
###############################################################################

# Basis points per dollar traded.
#
# 5 bps:
#
# transaction_cost_bps <- 5
#
# Zero initially for clean strategy comparison.

transaction_cost_bps <- 0


###############################################################################
# 6. ETF UNIVERSES
###############################################################################

sector_etfs <- c(
  "XLK",   # Technology
  "XLF",   # Financials
  "XLY",   # Consumer Discretionary
  "XLP",   # Consumer Staples
  "XLV",   # Health Care
  "XLI",   # Industrials
  "XLE",   # Energy
  "XLU",   # Utilities
  "XLB"    # Materials
)


defensive_etfs <- c(
  "IEF",
  "SHY",
  "GLD"
)


benchmark <- "SPY"


download_tickers <- unique(
  c(
    benchmark,
    sector_etfs,
    defensive_etfs
  )
)


###############################################################################
# 7. DOWNLOAD ADJUSTED CLOSE PRICES
###############################################################################

get_adjusted_close <- function(symbol,
                               from,
                               to) {
  
  x <- suppressWarnings(
    getSymbols(
      symbol,
      src = "yahoo",
      from = from,
      to = to + 1,
      auto.assign = FALSE
    )
  )
  
  px <- Ad(x)
  
  colnames(px) <- symbol
  
  return(px)
}


price_list <- list()


for (ticker in download_tickers) {
  
  cat("Downloading:", ticker, "\n")
  
  price_list[[ticker]] <- get_adjusted_close(
    ticker,
    download_start,
    end_date
  )
}


###############################################################################
# 8. MERGE PRICE SERIES
###############################################################################

prices <- do.call(
  merge,
  c(
    price_list,
    all = FALSE
  )
)


prices <- na.omit(prices)

prices <- prices[
  index(prices) <= end_date
]


###############################################################################
# 9. DAILY RETURNS
###############################################################################

daily_returns <- Return.calculate(
  prices,
  method = "discrete"
)


###############################################################################
# 10. OFFENSIVE SECTOR PRICES
###############################################################################

sector_prices <- prices[
  ,
  sector_etfs
]


###############################################################################
# 11. SECTOR MOMENTUM
###############################################################################

sector_momentum <- (
  sector_prices /
    lag(
      sector_prices,
      momentum_days
    )
) - 1


colnames(sector_momentum) <- sector_etfs


###############################################################################
# 12. ROLLING VOLATILITY FUNCTION
###############################################################################

rolling_volatility <- function(return_series,
                               n) {
  
  output <- xts(
    matrix(
      NA_real_,
      nrow = NROW(return_series),
      ncol = NCOL(return_series)
    ),
    order.by = index(return_series)
  )
  
  
  colnames(output) <- colnames(return_series)
  
  
  for (j in seq_len(NCOL(return_series))) {
    
    x <- as.numeric(
      return_series[, j]
    )
    
    
    output[, j] <- TTR::runSD(
      x,
      n = n
    ) * sqrt(252)
  }
  
  
  return(output)
}


###############################################################################
# 13. SECTOR VOLATILITY
###############################################################################

sector_returns <- daily_returns[
  ,
  sector_etfs
]


sector_volatility <- rolling_volatility(
  sector_returns,
  sector_vol_days
)


###############################################################################
# 14. VOLATILITY-ADJUSTED SECTOR MOMENTUM SCORE
###############################################################################

sector_score <- sector_momentum /
  (
    sector_volatility ^
      vol_penalty_exponent
  )


colnames(sector_score) <- sector_etfs


###############################################################################
# 15. SPY PRICE
###############################################################################

spy_price <- prices[
  ,
  "SPY"
]


###############################################################################
# 16. SPY 200-DAY SMA
###############################################################################

spy_sma200 <- TTR::SMA(
  spy_price,
  n = spy_sma_days
)


colnames(spy_sma200) <- "SPY_SMA200"


###############################################################################
# 17. SPY REALIZED VOLATILITY
###############################################################################

spy_return <- daily_returns[
  ,
  "SPY"
]


spy_volatility <- rolling_volatility(
  spy_return,
  spy_vol_days
)


colnames(spy_volatility) <- "SPY_VOL"


###############################################################################
# 18. SECTOR 50-DAY EMAS
###############################################################################

sector_ema <- xts(
  matrix(
    NA_real_,
    nrow = NROW(sector_prices),
    ncol = NCOL(sector_prices)
  ),
  order.by = index(sector_prices)
)


colnames(sector_ema) <- sector_etfs


for (j in seq_along(sector_etfs)) {
  
  sector_ema[, j] <- TTR::EMA(
    sector_prices[, j],
    n = sector_ema_days
  )
}


###############################################################################
# 19. DEFENSIVE PRICES
###############################################################################

defensive_prices <- prices[
  ,
  defensive_etfs
]


###############################################################################
# 20. DEFENSIVE MOMENTUM
###############################################################################

defensive_momentum <- (
  defensive_prices /
    lag(
      defensive_prices,
      defensive_momentum_days
    )
) - 1


colnames(defensive_momentum) <- defensive_etfs


###############################################################################
# 21. DEFENSIVE ALLOCATION FUNCTION
###############################################################################

get_defensive_weights <- function(row_number,
                                  defensive_momentum,
                                  defensive_etfs) {
  
  mom <- as.numeric(
    defensive_momentum[
      row_number,
      defensive_etfs
    ]
  )
  
  
  names(mom) <- defensive_etfs
  
  
  ###########################################################################
  # INITIALIZE
  ###########################################################################
  
  weights <- c(
    IEF  = 0,
    SHY  = 0,
    GLD  = 0,
    CASH = 0
  )
  
  
  ###########################################################################
  # IDENTIFY POSITIVE MOMENTUM ASSETS
  ###########################################################################
  
  positive_assets <- names(mom)[
    !is.na(mom) &
      mom > 0
  ]
  
  
  ###########################################################################
  # NO POSITIVE MOMENTUM -> CASH
  ###########################################################################
  
  if (length(positive_assets) == 0) {
    
    weights["CASH"] <- 1
    
    return(weights)
  }
  
  
  ###########################################################################
  # EQUAL-WEIGHT ALL POSITIVE DEFENSIVE ASSETS
  ###########################################################################
  
  weights[positive_assets] <-
    1 / length(positive_assets)
  
  
  return(weights)
}


###############################################################################
# 22. MONTH-END OBSERVATIONS
###############################################################################

month_end_locations <- endpoints(
  prices,
  on = "months"
)


month_end_locations <- month_end_locations[
  month_end_locations > 0 &
    month_end_locations <= NROW(prices)
]


month_end_dates <- index(prices)[
  month_end_locations
]


###############################################################################
# 23. STRATEGY ASSET UNIVERSE
###############################################################################

strategy_assets <- c(
  sector_etfs,
  defensive_etfs,
  "CASH"
)


###############################################################################
# 24. TARGET WEIGHT OBJECT
###############################################################################

target_weights <- xts(
  matrix(
    NA_real_,
    nrow = NROW(prices),
    ncol = length(strategy_assets)
  ),
  order.by = index(prices)
)


colnames(target_weights) <- strategy_assets


###############################################################################
# 25. CURRENT MONTHLY SECTOR SELECTION
###############################################################################

selected_sectors <- character(0)


###############################################################################
# 26. SECTOR STOP STATUS
###############################################################################

stopped <- rep(
  FALSE,
  length(sector_etfs)
)


names(stopped) <- sector_etfs


###############################################################################
# 27. DAILY STRATEGY LOOP
###############################################################################

for (i in seq_len(NROW(prices))) {
  
  
  current_date <- index(prices)[i]
  
  
  ###########################################################################
  # A. MONTHLY SECTOR MOMENTUM RANKING
  ###########################################################################
  
  if (i %in% month_end_locations) {
    
    
    scores <- as.numeric(
      sector_score[
        i,
        sector_etfs
      ]
    )
    
    
    names(scores) <- sector_etfs
    
    
    valid_scores <- scores[
      !is.na(scores) &
        is.finite(scores)
    ]
    
    
    #########################################################################
    # SELECT TOP N
    #########################################################################
    
    if (length(valid_scores) >= top_n) {
      
      ranked_scores <- sort(
        valid_scores,
        decreasing = TRUE
      )
      
      
      selected_sectors <- names(
        ranked_scores[
          seq_len(top_n)
        ]
      )
      
      
      #######################################################################
      # RESET STOPS AT EACH NEW MONTHLY RANKING
      #######################################################################
      
      stopped[] <- FALSE
    }
  }
  
  
  ###########################################################################
  # B. INDIVIDUAL SECTOR 50-DAY EMA STOPS
  ###########################################################################
  
  if (length(selected_sectors) > 0) {
    
    
    for (ticker in selected_sectors) {
      
      
      current_sector_price <- as.numeric(
        sector_prices[
          i,
          ticker
        ]
      )
      
      
      current_sector_ema <- as.numeric(
        sector_ema[
          i,
          ticker
        ]
      )
      
      
      if (
        !is.na(current_sector_price) &&
        !is.na(current_sector_ema) &&
        current_sector_price <= current_sector_ema
      ) {
        
        stopped[ticker] <- TRUE
      }
    }
  }
  
  
  ###########################################################################
  # C. CURRENT SPY REGIME DATA
  ###########################################################################
  
  current_spy <- as.numeric(
    spy_price[i]
  )
  
  
  current_spy_sma <- as.numeric(
    spy_sma200[i]
  )
  
  
  current_spy_vol <- as.numeric(
    spy_volatility[i]
  )
  
  
  ###########################################################################
  # SKIP PERIODS BEFORE INDICATORS EXIST
  ###########################################################################
  
  if (
    is.na(current_spy) ||
    is.na(current_spy_sma) ||
    is.na(current_spy_vol) ||
    length(selected_sectors) == 0
  ) {
    
    next
  }
  
  
  ###########################################################################
  # D. MARKET REGIME
  ###########################################################################
  
  trend_ok <- (
    current_spy >
      current_spy_sma
  )
  
  
  volatility_ok <- (
    current_spy_vol <=
      risk_vol_cutoff
  )
  
  
  risk_on <- (
    trend_ok &&
      volatility_ok
  )
  
  
  ###########################################################################
  # E. DEFENSIVE MOMENTUM ALLOCATION
  ###########################################################################
  
  defensive_weights <- get_defensive_weights(
    row_number = i,
    defensive_momentum = defensive_momentum,
    defensive_etfs = defensive_etfs
  )
  
  
  ###########################################################################
  # F. INITIALIZE TODAY'S PORTFOLIO
  ###########################################################################
  
  todays_weights <- rep(
    0,
    length(strategy_assets)
  )
  
  
  names(todays_weights) <- strategy_assets
  
  
  ###########################################################################
  # G. RISK-ON PORTFOLIO
  ###########################################################################
  
  if (risk_on) {
    
    
    #########################################################################
    # EACH SELECTED SECTOR GETS ONE EQUAL-WEIGHT SLOT
    #########################################################################
    
    sector_slot_weight <- 1 / top_n
    
    
    #########################################################################
    # ALLOCATE ACTIVE SECTOR POSITIONS
    #########################################################################
    
    for (ticker in selected_sectors) {
      
      
      if (!stopped[ticker]) {
        
        todays_weights[ticker] <-
          sector_slot_weight
      }
    }
    
    
    #########################################################################
    # DETERMINE UNUSED CAPITAL
    #
    # Any stopped sector slot moves to the defensive sleeve.
    #########################################################################
    
    total_sector_weight <- sum(
      todays_weights[
        sector_etfs
      ]
    )
    
    
    defensive_total <- 1 -
      total_sector_weight
    
    
    #########################################################################
    # DISTRIBUTE DEFENSIVE CAPITAL
    #########################################################################
    
    for (asset in names(defensive_weights)) {
      
      todays_weights[asset] <-
        defensive_total *
        defensive_weights[asset]
    }
  }
  
  
  ###########################################################################
  # H. RISK-OFF PORTFOLIO
  ###########################################################################
  
  if (!risk_on) {
    
    
    #########################################################################
    # FULL PORTFOLIO GOES TO DEFENSIVE MOMENTUM SLEEVE
    #########################################################################
    
    for (asset in names(defensive_weights)) {
      
      todays_weights[asset] <-
        defensive_weights[asset]
    }
  }
  
  
  ###########################################################################
  # I. SAVE TARGET WEIGHTS
  ###########################################################################
  
  target_weights[
    i,
  ] <- todays_weights
}


###############################################################################
# 28. EXECUTION LAG
###############################################################################

# All signals use closing data.
#
# Therefore today's signal becomes tomorrow's portfolio.
#
# This avoids look-ahead bias.

executed_weights <- lag(
  target_weights,
  k = 1
)


###############################################################################
# 29. STRATEGY ASSET RETURNS
###############################################################################

strategy_asset_returns <- daily_returns[
  ,
  c(
    sector_etfs,
    defensive_etfs
  )
]


###############################################################################
# 30. CASH RETURN SERIES
###############################################################################

# Currently modeled as zero return.
#
# This is intentionally conservative.
#
# A more realistic implementation can later replace this with:
#
#   - 3-month Treasury bill total returns
#   - SGOV
#   - BIL
#   - Fed Funds
#
# For now:
#
# CASH RETURN = 0%

cash_return <- xts(
  rep(
    0,
    NROW(strategy_asset_returns)
  ),
  order.by = index(strategy_asset_returns)
)


colnames(cash_return) <- "CASH"


###############################################################################
# 31. ADD CASH TO RETURN MATRIX
###############################################################################

strategy_asset_returns <- merge(
  strategy_asset_returns,
  cash_return,
  join = "inner"
)


strategy_asset_returns <- strategy_asset_returns[
  ,
  strategy_assets
]


###############################################################################
# 32. GROSS STRATEGY RETURNS
###############################################################################

strategy_return_gross <- xts(
  rep(
    NA_real_,
    NROW(strategy_asset_returns)
  ),
  order.by = index(strategy_asset_returns)
)


colnames(strategy_return_gross) <- "Avalon_Gross"


for (i in seq_len(NROW(strategy_asset_returns))) {
  
  
  w <- as.numeric(
    executed_weights[
      index(strategy_asset_returns)[i],
    ]
  )
  
  
  r <- as.numeric(
    strategy_asset_returns[i, ]
  )
  
  
  if (
    length(w) == length(r) &&
    all(!is.na(w)) &&
    all(!is.na(r))
  ) {
    
    strategy_return_gross[i] <-
      sum(
        w * r
      )
  }
}


###############################################################################
# 33. PORTFOLIO TURNOVER
###############################################################################

turnover <- xts(
  rep(
    NA_real_,
    NROW(executed_weights)
  ),
  order.by = index(executed_weights)
)


colnames(turnover) <- "Turnover"


for (i in 2:NROW(executed_weights)) {
  
  
  current_weights <- as.numeric(
    executed_weights[i, ]
  )
  
  
  previous_weights <- as.numeric(
    executed_weights[
      i - 1,
    ]
  )
  
  
  if (
    all(!is.na(current_weights)) &&
    all(!is.na(previous_weights))
  ) {
    
    turnover[i] <- sum(
      abs(
        current_weights -
          previous_weights
      )
    )
  }
}


###############################################################################
# 34. TRANSACTION COSTS
###############################################################################

transaction_cost <- turnover *
  transaction_cost_bps /
  10000


colnames(transaction_cost) <- "Transaction_Cost"


###############################################################################
# 35. NET STRATEGY RETURN
###############################################################################

strategy_return <- (
  strategy_return_gross -
    transaction_cost
)


colnames(strategy_return) <- "Avalon_Modified"


###############################################################################
# 36. SPY BENCHMARK
###############################################################################

spy_benchmark <- daily_returns[
  ,
  "SPY"
]


colnames(spy_benchmark) <- "SPY"


###############################################################################
# 37. COMBINE STRATEGY AND BENCHMARK
###############################################################################

comparison <- merge(
  strategy_return,
  spy_benchmark,
  join = "inner"
)


comparison <- comparison[
  paste0(
    start_date,
    "/",
    end_date
  )
]


comparison <- na.omit(
  comparison
)


###############################################################################
# 38. ANNUALIZED PERFORMANCE
###############################################################################

cat("\n")
cat("============================================================\n")
cat("ANNUALIZED PERFORMANCE\n")
cat("============================================================\n")


print(
  table.AnnualizedReturns(
    comparison,
    scale = 252
  )
)


###############################################################################
# 39. CAGR
###############################################################################

cat("\n")
cat("============================================================\n")
cat("CAGR\n")
cat("============================================================\n")


print(
  Return.annualized(
    comparison,
    scale = 252,
    geometric = TRUE
  )
)


###############################################################################
# 40. MAXIMUM DRAWDOWN
###############################################################################

cat("\n")
cat("============================================================\n")
cat("MAXIMUM DRAWDOWN\n")
cat("============================================================\n")


print(
  maxDrawdown(
    comparison
  )
)


###############################################################################
# 41. SHARPE RATIO
###############################################################################

cat("\n")
cat("============================================================\n")
cat("SHARPE RATIO\n")
cat("============================================================\n")


print(
  SharpeRatio.annualized(
    comparison,
    scale = 252,
    geometric = TRUE
  )
)


###############################################################################
# 42. SORTINO RATIO
###############################################################################

cat("\n")
cat("============================================================\n")
cat("SORTINO RATIO\n")
cat("============================================================\n")


print(
  SortinoRatio(
    comparison
  )
)


###############################################################################
# 43. CALMAR RATIO
###############################################################################

cat("\n")
cat("============================================================\n")
cat("CALMAR RATIO\n")
cat("============================================================\n")


print(
  CalmarRatio(
    comparison
  )
)


###############################################################################
# 44. PERFORMANCE SUMMARY CHART
###############################################################################

charts.PerformanceSummary(
  comparison,
  wealth.index = TRUE,
  main = "Modified Avalon Sector Momentum + TAA vs SPY"
)


###############################################################################
# 45. LOG EQUITY CURVE
###############################################################################

chart.CumReturns(
  comparison,
  wealth.index = TRUE,
  geometric = TRUE,
  ylog = TRUE,
  main = "Modified Avalon Strategy vs SPY"
)


###############################################################################
# 46. DRAWDOWN CHART
###############################################################################

chart.Drawdown(
  comparison,
  main = "Drawdowns: Modified Avalon Strategy vs SPY"
)


###############################################################################
# 47. CALENDAR YEAR RETURNS
###############################################################################

cat("\n")
cat("============================================================\n")
cat("CALENDAR YEAR RETURNS\n")
cat("============================================================\n")


print(
  table.CalendarReturns(
    comparison
  )
)


###############################################################################
# 48. MONTHLY RETURNS
###############################################################################

monthly_returns <- apply.monthly(
  comparison,
  Return.cumulative
)


###############################################################################
# 49. ROLLING 1-YEAR RETURNS
###############################################################################

rolling_1yr <- rollapply(
  comparison,
  width = 252,
  FUN = function(x) {
    
    apply(
      x,
      2,
      Return.cumulative
    )
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)


colnames(rolling_1yr) <- c(
  "Strategy_1Y",
  "SPY_1Y"
)


###############################################################################
# 50. ROLLING 3-YEAR CAGR
###############################################################################

rolling_3yr <- rollapply(
  comparison,
  width = 252 * 3,
  FUN = function(x) {
    
    apply(
      x,
      2,
      function(y) {
        
        Return.annualized(
          y,
          scale = 252,
          geometric = TRUE
        )
      }
    )
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)


colnames(rolling_3yr) <- c(
  "Strategy_3Y",
  "SPY_3Y"
)


###############################################################################
# 51. ROLLING 5-YEAR CAGR
###############################################################################

rolling_5yr <- rollapply(
  comparison,
  width = 252 * 5,
  FUN = function(x) {
    
    apply(
      x,
      2,
      function(y) {
        
        Return.annualized(
          y,
          scale = 252,
          geometric = TRUE
        )
      }
    )
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)


colnames(rolling_5yr) <- c(
  "Strategy_5Y",
  "SPY_5Y"
)


###############################################################################
# 52. ROLLING OUTPERFORMANCE
###############################################################################

win_rate_1yr <- mean(
  rolling_1yr[, 1] >
    rolling_1yr[, 2],
  na.rm = TRUE
)


win_rate_3yr <- mean(
  rolling_3yr[, 1] >
    rolling_3yr[, 2],
  na.rm = TRUE
)


win_rate_5yr <- mean(
  rolling_5yr[, 1] >
    rolling_5yr[, 2],
  na.rm = TRUE
)


cat("\n")
cat("============================================================\n")
cat("ROLLING OUTPERFORMANCE VS SPY\n")
cat("============================================================\n")


cat(
  "1-year win rate:",
  round(
    win_rate_1yr * 100,
    2
  ),
  "%\n"
)


cat(
  "3-year win rate:",
  round(
    win_rate_3yr * 100,
    2
  ),
  "%\n"
)


cat(
  "5-year win rate:",
  round(
    win_rate_5yr * 100,
    2
  ),
  "%\n"
)


###############################################################################
# 53. ANNUAL TURNOVER
###############################################################################

annual_turnover <- apply.yearly(
  turnover[
    paste0(
      start_date,
      "/",
      end_date
    )
  ],
  sum,
  na.rm = TRUE
)


cat("\n")
cat("============================================================\n")
cat("TURNOVER\n")
cat("============================================================\n")


cat(
  "Average annual gross turnover:",
  round(
    mean(
      annual_turnover,
      na.rm = TRUE
    ) * 100,
    2
  ),
  "%\n"
)


###############################################################################
# 54. FINAL TARGET PORTFOLIO
###############################################################################

valid_weights <- target_weights[
  paste0(
    start_date,
    "/",
    end_date
  )
]


valid_weights <- valid_weights[
  apply(
    valid_weights,
    1,
    function(x) {
      all(!is.na(x))
    }
  )
]


final_weights <- tail(
  valid_weights,
  1
)


cat("\n")
cat("============================================================\n")
cat("FINAL PORTFOLIO WEIGHTS\n")
cat("============================================================\n")


print(
  round(
    t(final_weights) * 100,
    2
  )
)


###############################################################################
# 55. FINAL SECTOR RANKINGS
###############################################################################

last_sector_scores <- tail(
  na.omit(
    sector_score[
      paste0(
        start_date,
        "/",
        end_date
      )
    ]
  ),
  1
)


ranking_values <- as.numeric(
  last_sector_scores
)


names(ranking_values) <- colnames(
  last_sector_scores
)


ranking_values <- sort(
  ranking_values,
  decreasing = TRUE
)


cat("\n")
cat("============================================================\n")
cat("FINAL SECTOR MOMENTUM RANKINGS\n")
cat("============================================================\n")


print(
  ranking_values
)


###############################################################################
# 56. FINAL DEFENSIVE MOMENTUM
###############################################################################

last_defensive_momentum <- tail(
  na.omit(
    defensive_momentum[
      paste0(
        start_date,
        "/",
        end_date
      )
    ]
  ),
  1
)


defensive_momentum_values <- as.numeric(
  last_defensive_momentum
)


names(defensive_momentum_values) <- colnames(
  last_defensive_momentum
)


cat("\n")
cat("============================================================\n")
cat("FINAL DEFENSIVE MOMENTUM\n")
cat("============================================================\n")


print(
  round(
    defensive_momentum_values * 100,
    2
  )
)


###############################################################################
# 57. FINAL DEFENSIVE ALLOCATION
###############################################################################

last_row <- NROW(
  defensive_momentum
)


final_defensive_weights <- get_defensive_weights(
  row_number = last_row,
  defensive_momentum = defensive_momentum,
  defensive_etfs = defensive_etfs
)


cat("\n")
cat("============================================================\n")
cat("FINAL DEFENSIVE ALLOCATION\n")
cat("============================================================\n")


print(
  round(
    final_defensive_weights * 100,
    2
  )
)


###############################################################################
# 58. FINAL MARKET REGIME
###############################################################################

last_spy <- as.numeric(
  tail(
    spy_price,
    1
  )
)


last_sma <- as.numeric(
  tail(
    spy_sma200,
    1
  )
)


last_vol <- as.numeric(
  tail(
    spy_volatility,
    1
  )
)


final_risk_on <- (
  last_spy >
    last_sma &&
    last_vol <=
    risk_vol_cutoff
)


cat("\n")
cat("============================================================\n")
cat("FINAL MARKET REGIME\n")
cat("============================================================\n")


cat(
  "SPY:",
  round(
    last_spy,
    2
  ),
  "\n"
)


cat(
  "SPY 200-day SMA:",
  round(
    last_sma,
    2
  ),
  "\n"
)


cat(
  "SPY realized volatility:",
  round(
    last_vol * 100,
    2
  ),
  "%\n"
)


cat(
  "Risk On:",
  final_risk_on,
  "\n"
)


###############################################################################
# 59. DEFENSIVE SLEEVE HISTORY
###############################################################################

# Optional diagnostic object showing what the defensive portfolio would
# have held each day.

defensive_weight_history <- xts(
  matrix(
    NA_real_,
    nrow = NROW(prices),
    ncol = 4
  ),
  order.by = index(prices)
)


colnames(defensive_weight_history) <- c(
  "IEF",
  "SHY",
  "GLD",
  "CASH"
)


for (i in seq_len(NROW(prices))) {
  
  w <- get_defensive_weights(
    row_number = i,
    defensive_momentum = defensive_momentum,
    defensive_etfs = defensive_etfs
  )
  
  
  defensive_weight_history[i, ] <- w
}


###############################################################################
# 60. DEFENSIVE ASSET POSITIVE-MOMENTUM COUNT
###############################################################################

positive_defensive_count <- xts(
  rep(
    NA_real_,
    NROW(defensive_momentum)
  ),
  order.by = index(defensive_momentum)
)


for (i in seq_len(NROW(defensive_momentum))) {
  
  mom <- as.numeric(
    defensive_momentum[i, ]
  )
  
  
  positive_defensive_count[i] <- sum(
    mom > 0,
    na.rm = TRUE
  )
}


colnames(positive_defensive_count) <-
  "Positive_Defensive_Assets"


###############################################################################
# 61. OPTIONAL DIAGNOSTIC PLOT
###############################################################################

plot(
  positive_defensive_count[
    paste0(
      start_date,
      "/",
      end_date
    )
  ],
  main = "Number of Defensive ETFs With Positive Momentum",
  ylab = "Positive ETFs",
  xlab = ""
)