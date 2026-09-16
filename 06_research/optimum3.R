# ============================================================
# OPTIMUM3 APPROXIMATION
#
# Packages:
#   quantmod
#   xts
#   PerformanceAnalytics
#
# NO dplyr
# NO plyr
#
# ------------------------------------------------------------
# APPROXIMATED RULES
#
# Universe:
#   SPY QQQ VNQ REM IEF TLT TIP VGK EWJ
#   SCZ EEM RWX BWX DBC GLD
#
# Rebalance:
#   Monthly
#
# Momentum:
#   Average of 1, 3, 6, and 12-month total returns
#
# Absolute momentum:
#   Composite momentum must be > 0
#
# Relative momentum:
#   Keep top 7 assets
#
# Diversification:
#   Calculate trailing daily correlations
#   Test every possible 3-asset combination
#   Select combination with lowest average pairwise correlation
#
# Portfolio:
#   Equal weight selected assets
#   1/3 each
#
# If fewer than 3 assets have positive momentum:
#   Remaining allocation stays in cash at 0% return
#
# IMPORTANT:
#   This is an approximation of Optimum3.
#   Exact proprietary rules are not publicly disclosed.
# ============================================================


library(quantmod)
library(xts)
library(PerformanceAnalytics)


# ============================================================
# PARAMETERS
# ============================================================

tickers <- c(
  "SPY",
  "QQQ",
  "VNQ",
  "REM",
  "IEF",
  "TLT",
  "TIP",
  "VGK",
  "EWJ",
  "SCZ",
  "EEM",
  "RWX",
  "BWX",
  "DBC",
  "GLD"
)

start_date <- "2006-01-01"

momentum_months <- c(
  1,
  3,
  6,
  12
)

top_n <- 7

hold_n <- 3

correlation_days <- 126


# ============================================================
# DOWNLOAD PRICES
# ============================================================

download_prices <- function(
    tickers,
    from = "2006-01-01") {
  
  data_env <- new.env()
  
  getSymbols(
    tickers,
    src = "yahoo",
    from = from,
    env = data_env,
    auto.assign = TRUE,
    warnings = FALSE
  )
  
  price_list <- vector(
    mode = "list",
    length = length(tickers)
  )
  
  for (i in seq_along(tickers)) {
    
    symbol <- tickers[i]
    
    price_list[[i]] <- Ad(
      data_env[[symbol]]
    )
  }
  
  prices <- do.call(
    merge,
    price_list
  )
  
  colnames(prices) <- tickers
  
  # Require all ETFs to have valid data.
  # This means the strategy begins once the entire
  # Optimum3 universe exists.
  prices <- na.omit(prices)
  
  return(prices)
}


prices <- download_prices(
  tickers = tickers,
  from = start_date
)


# ============================================================
# DAILY ASSET RETURNS
# ============================================================

daily_returns <- Return.calculate(
  prices,
  method = "discrete"
)

daily_returns <- na.omit(
  daily_returns
)


# ============================================================
# FUNCTION:
# SELECT LOWEST-CORRELATION COMBINATION
# ============================================================

select_min_corr_triplet <- function(
    candidate_names,
    return_window,
    hold_n = 3) {
  
  
  # ----------------------------------------------------------
  # If fewer assets exist than required, just return them.
  # ----------------------------------------------------------
  
  if (length(candidate_names) < hold_n) {
    
    return(
      list(
        assets = candidate_names,
        avg_correlation = NA_real_
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Candidate returns
  # ----------------------------------------------------------
  
  candidate_returns <-
    return_window[
      ,
      candidate_names,
      drop = FALSE
    ]
  
  
  # ----------------------------------------------------------
  # Correlation matrix
  # ----------------------------------------------------------
  
  correlation_matrix <- cor(
    coredata(candidate_returns),
    use = "pairwise.complete.obs"
  )
  
  rownames(correlation_matrix) <-
    candidate_names
  
  colnames(correlation_matrix) <-
    candidate_names
  
  
  # ----------------------------------------------------------
  # Generate all possible combinations
  # ----------------------------------------------------------
  
  combinations <- combn(
    candidate_names,
    hold_n
  )
  
  
  combo_scores <- rep(
    NA_real_,
    ncol(combinations)
  )
  
  
  # ----------------------------------------------------------
  # Calculate average pairwise correlation
  # for each possible portfolio
  # ----------------------------------------------------------
  
  for (j in seq_len(ncol(combinations))) {
    
    combo <- combinations[, j]
    
    sub_corr <- correlation_matrix[
      combo,
      combo,
      drop = FALSE
    ]
    
    pairwise_corrs <-
      sub_corr[
        upper.tri(sub_corr)
      ]
    
    
    if (all(is.na(pairwise_corrs))) {
      
      combo_scores[j] <- Inf
      
    } else {
      
      combo_scores[j] <- mean(
        pairwise_corrs,
        na.rm = TRUE
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Choose portfolio with lowest correlation
  # ----------------------------------------------------------
  
  best_index <- which.min(
    combo_scores
  )
  
  best_assets <-
    combinations[
      ,
      best_index
    ]
  
  best_corr <-
    combo_scores[
      best_index
    ]
  
  
  return(
    list(
      assets = best_assets,
      avg_correlation = best_corr
    )
  )
}


# ============================================================
# MAIN OPTIMUM3 FUNCTION
# ============================================================

run_optimum3 <- function(
    prices,
    momentum_months = c(1, 3, 6, 12),
    top_n = 7,
    hold_n = 3,
    correlation_days = 126,
    drop_current_partial_month = TRUE) {
  
  
  # ==========================================================
  # DAILY RETURNS
  # ==========================================================
  
  daily_returns <- Return.calculate(
    prices,
    method = "discrete"
  )
  
  daily_returns <- na.omit(
    daily_returns
  )
  
  
  # ==========================================================
  # MONTH-END OBSERVATIONS
  # ==========================================================
  
  month_end_points <- endpoints(
    prices,
    on = "months"
  )
  
  month_end_points <-
    month_end_points[
      month_end_points > 0
    ]
  
  
  # ----------------------------------------------------------
  # If using live Yahoo data, endpoints() considers the latest
  # available day to be the "month end".
  #
  # Example:
  #
  # September 15 would appear as the September endpoint.
  #
  # Drop that partial month if requested.
  # ----------------------------------------------------------
  
  if (
    drop_current_partial_month &&
    length(month_end_points) > 0
  ) {
    
    last_price_date <-
      as.Date(
        tail(
          index(prices),
          1
        )
      )
    
    current_date <-
      Sys.Date()
    
    same_month_as_today <-
      format(last_price_date, "%Y-%m") ==
      format(current_date, "%Y-%m")
    
    
    if (same_month_as_today) {
      
      month_end_points <-
        head(
          month_end_points,
          -1
        )
    }
  }
  
  
  monthly_prices <-
    prices[
      month_end_points,
      ,
      drop = FALSE
    ]
  
  
  # ==========================================================
  # STORAGE OBJECTS
  # ==========================================================
  
  signal_weights <- xts(
    matrix(
      0,
      nrow = nrow(monthly_prices),
      ncol = ncol(monthly_prices)
    ),
    order.by = index(monthly_prices)
  )
  
  colnames(signal_weights) <-
    colnames(monthly_prices)
  
  
  momentum_scores <- xts(
    matrix(
      NA_real_,
      nrow = nrow(monthly_prices),
      ncol = ncol(monthly_prices)
    ),
    order.by = index(monthly_prices)
  )
  
  colnames(momentum_scores) <-
    colnames(monthly_prices)
  
  
  selection_log <- vector(
    mode = "list",
    length = nrow(monthly_prices)
  )
  
  
  # Need full 12-month lookback
  first_signal_row <-
    max(momentum_months) + 1L
  
  
  # ==========================================================
  # LOOP THROUGH MONTH-END SIGNAL DATES
  # ==========================================================
  
  for (
    i in first_signal_row:
    nrow(monthly_prices)
  ) {
    
    
    signal_date <-
      index(monthly_prices)[i]
    
    
    # ========================================================
    # MOMENTUM CALCULATION
    #
    # IMPORTANT FIX:
    #
    # Convert xts rows to ordinary numeric vectors BEFORE
    # doing arithmetic.
    #
    # Otherwise xts tries to align observations by date.
    # ========================================================
    
    current_prices <- as.numeric(
      coredata(
        monthly_prices[
          i,
          ,
          drop = FALSE
        ]
      )
    )
    
    
    momentum_list <- lapply(
      momentum_months,
      function(k) {
        
        
        lagged_prices <- as.numeric(
          coredata(
            monthly_prices[
              i - k,
              ,
              drop = FALSE
            ]
          )
        )
        
        
        current_prices /
          lagged_prices -
          1
      }
    )
    
    
    # --------------------------------------------------------
    # IMPORTANT FIX:
    #
    # Explicitly cbind the list into a matrix.
    #
    # This prevents sapply() from simplifying unexpectedly.
    # --------------------------------------------------------
    
    momentum_matrix <- do.call(
      cbind,
      momentum_list
    )
    
    
    rownames(momentum_matrix) <-
      colnames(monthly_prices)
    
    
    colnames(momentum_matrix) <-
      paste0(
        momentum_months,
        "M"
      )
    
    
    # ========================================================
    # COMPOSITE MOMENTUM SCORE
    # ========================================================
    
    momentum_score <- rowMeans(
      momentum_matrix,
      na.rm = TRUE
    )
    
    
    names(momentum_score) <-
      colnames(monthly_prices)
    
    
    momentum_scores[
      i,
    ] <- momentum_score
    
    
    # ========================================================
    # ABSOLUTE MOMENTUM FILTER
    #
    # Composite score must be positive.
    # ========================================================
    
    positive_assets <- names(
      momentum_score[
        is.finite(momentum_score) &
          momentum_score > 0
      ]
    )
    
    
    # ========================================================
    # RELATIVE MOMENTUM
    #
    # Rank surviving assets from strongest to weakest.
    # ========================================================
    
    ranked_assets <- names(
      sort(
        momentum_score[
          positive_assets
        ],
        decreasing = TRUE
      )
    )
    
    
    candidates <- head(
      ranked_assets,
      top_n
    )
    
    
    # ========================================================
    # DAILY CORRELATION WINDOW
    # ========================================================
    
    historical_returns <- daily_returns[
      paste0(
        "/",
        signal_date
      )
    ]
    
    
    if (
      nrow(historical_returns) >=
      correlation_days
    ) {
      
      correlation_window <- tail(
        historical_returns,
        correlation_days
      )
      
    } else {
      
      correlation_window <-
        historical_returns
    }
    
    
    # ========================================================
    # SELECT FINAL PORTFOLIO
    # ========================================================
    
    selected_assets <-
      character(0)
    
    avg_corr <-
      NA_real_
    
    
    # --------------------------------------------------------
    # Normal situation:
    # At least 3 momentum-qualified candidates.
    # --------------------------------------------------------
    
    if (
      length(candidates) >=
      hold_n
    ) {
      
      
      selection <-
        select_min_corr_triplet(
          candidate_names =
            candidates,
          return_window =
            correlation_window,
          hold_n =
            hold_n
        )
      
      
      selected_assets <-
        selection$assets
      
      
      avg_corr <-
        selection$avg_correlation
      
      
      # Equal-weight portfolio
      signal_weights[
        i,
        selected_assets
      ] <- 1 / hold_n
    }
    
    
    # --------------------------------------------------------
    # Fewer than 3 positive-momentum assets.
    #
    # Each asset still gets one 1/3 portfolio slot.
    # Remaining capital stays in cash.
    # --------------------------------------------------------
    
    if (
      length(candidates) > 0 &&
      length(candidates) < hold_n
    ) {
      
      
      selected_assets <-
        candidates
      
      
      signal_weights[
        i,
        selected_assets
      ] <- 1 / hold_n
    }
    
    
    # ========================================================
    # CASH WEIGHT
    # ========================================================
    
    cash_weight <-
      1 -
      sum(
        as.numeric(
          signal_weights[
            i,
          ]
        )
      )
    
    
    # ========================================================
    # SIGNAL LOG
    # ========================================================
    
    selection_log[[i]] <- data.frame(
      
      Date =
        as.Date(signal_date),
      
      Positive_Assets =
        length(
          positive_assets
        ),
      
      Candidates =
        paste(
          candidates,
          collapse = ", "
        ),
      
      Selected =
        paste(
          selected_assets,
          collapse = ", "
        ),
      
      Average_Correlation =
        avg_corr,
      
      Cash_Weight =
        cash_weight,
      
      stringsAsFactors =
        FALSE
    )
  }
  
  
  # ==========================================================
  # CLEAN SIGNAL LOG
  # ==========================================================
  
  valid_logs <- !sapply(
    selection_log,
    is.null
  )
  
  
  selection_log <- do.call(
    rbind,
    selection_log[
      valid_logs
    ]
  )
  
  
  # ==========================================================
  # CREATE DAILY WEIGHT OBJECT
  # ==========================================================
  
  daily_weights <- xts(
    matrix(
      NA_real_,
      nrow = nrow(daily_returns),
      ncol = ncol(daily_returns)
    ),
    order.by = index(daily_returns)
  )
  
  colnames(daily_weights) <-
    colnames(daily_returns)
  
  
  # ==========================================================
  # INSERT MONTH-END SIGNAL WEIGHTS
  # ==========================================================
  
  signal_dates <- intersect(
    index(signal_weights),
    index(daily_weights)
  )
  
  
  daily_weights[
    signal_dates,
  ] <- signal_weights[
    signal_dates,
  ]
  
  
  # ==========================================================
  # CARRY WEIGHTS FORWARD
  # ==========================================================
  
  daily_weights <- zoo::na.locf(
    daily_weights,
    na.rm = FALSE
  )
  
  
  # ==========================================================
  # LAG WEIGHTS ONE TRADING DAY
  #
  # Signal uses month-end closing prices.
  #
  # Therefore the newly calculated portfolio cannot earn
  # the month-end return.
  #
  # New holdings begin the following trading day.
  # ==========================================================
  
  held_weights <- lag(
    daily_weights,
    k = 1
  )
  
  
  held_weights[
    is.na(held_weights)
  ] <- 0
  
  
  # ==========================================================
  # STRATEGY DAILY RETURNS
  # ==========================================================
  
  strategy_return_values <- rowSums(
    
    coredata(
      daily_returns
    ) *
      
      coredata(
        held_weights
      ),
    
    na.rm = TRUE
  )
  
  
  strategy_returns <- xts(
    strategy_return_values,
    order.by = index(daily_returns)
  )
  
  
  colnames(strategy_returns) <-
    "Optimum3"
  
  
  # ==========================================================
  # REMOVE INITIAL PRE-SIGNAL PERIOD
  # ==========================================================
  
  gross_exposure <- xts(
    
    rowSums(
      coredata(
        held_weights
      )
    ),
    
    order.by =
      index(held_weights)
  )
  
  
  invested_days <- which(
    gross_exposure > 0
  )
  
  
  if (
    length(invested_days) > 0
  ) {
    
    first_invested_day <-
      invested_days[1]
    
    
    strategy_returns <-
      strategy_returns[
        first_invested_day:
          nrow(strategy_returns)
      ]
    
    
    held_weights <-
      held_weights[
        first_invested_day:
          nrow(held_weights)
      ]
  }
  
  
  # ==========================================================
  # RETURN RESULTS
  # ==========================================================
  
  return(
    list(
      
      returns =
        strategy_returns,
      
      weights =
        held_weights,
      
      signal_weights =
        signal_weights,
      
      momentum =
        momentum_scores,
      
      selection_log =
        selection_log,
      
      daily_asset_returns =
        daily_returns
    )
  )
}


# ============================================================
# RUN OPTIMUM3
# ============================================================

opt3 <- run_optimum3(
  
  prices =
    prices,
  
  momentum_months =
    momentum_months,
  
  top_n =
    top_n,
  
  hold_n =
    hold_n,
  
  correlation_days =
    correlation_days,
  
  drop_current_partial_month =
    TRUE
)


# ============================================================
# STRATEGY RETURNS
# ============================================================

opt3_returns <-
  opt3$returns


# ============================================================
# SPY BENCHMARK
# ============================================================

spy_returns <- daily_returns[
  index(opt3_returns),
  "SPY",
  drop = FALSE
]


comparison <- merge(
  opt3_returns,
  spy_returns
)


colnames(comparison) <- c(
  "Optimum3",
  "SPY"
)


comparison <- na.omit(
  comparison
)


# ============================================================
# PERFORMANCE TABLE
# ============================================================

print(
  table.AnnualizedReturns(
    comparison,
    Rf = 0
  )
)


# ============================================================
# CAGR
# ============================================================

opt3_cagr <- Return.annualized(
  opt3_returns,
  scale = 252,
  geometric = TRUE
)


spy_cagr <- Return.annualized(
  comparison[, "SPY"],
  scale = 252,
  geometric = TRUE
)


cat(
  "\nOptimum3 CAGR:",
  round(
    as.numeric(opt3_cagr) * 100,
    2
  ),
  "%\n"
)


cat(
  "SPY CAGR:",
  round(
    as.numeric(spy_cagr) * 100,
    2
  ),
  "%\n"
)


# ============================================================
# VOLATILITY
# ============================================================

opt3_vol <- StdDev.annualized(
  opt3_returns,
  scale = 252
)


cat(
  "Optimum3 Annualized Volatility:",
  round(
    as.numeric(opt3_vol) * 100,
    2
  ),
  "%\n"
)


# ============================================================
# SHARPE RATIO
# ============================================================

opt3_sharpe <- SharpeRatio.annualized(
  opt3_returns,
  Rf = 0,
  scale = 252
)


cat(
  "Optimum3 Sharpe Ratio:",
  round(
    as.numeric(opt3_sharpe),
    2
  ),
  "\n"
)


# ============================================================
# MAX DRAWDOWN
# ============================================================

opt3_mdd <- maxDrawdown(
  opt3_returns
)


cat(
  "Optimum3 Max Drawdown:",
  round(
    as.numeric(opt3_mdd) * 100,
    2
  ),
  "%\n"
)


# ============================================================
# PERFORMANCE SUMMARY CHART
# ============================================================

charts.PerformanceSummary(
  comparison,
  main =
    "Optimum3 Approximation vs SPY"
)


# ============================================================
# CALENDAR-YEAR RETURNS
# ============================================================

annual_returns <- table.CalendarReturns(
  comparison
)

print(
  annual_returns
)


# ============================================================
# MOST RECENT SIGNALS
# ============================================================

cat(
  "\nMost Recent 12 Signals:\n"
)

print(
  tail(
    opt3$selection_log,
    12
  )
)


# ============================================================
# CURRENT / MOST RECENT SIGNAL
# ============================================================

cat(
  "\nMost Recent Signal:\n"
)

print(
  tail(
    opt3$selection_log,
    1
  )
)


# ============================================================
# MOST RECENT TARGET WEIGHTS
# ============================================================

latest_weights <- tail(
  opt3$signal_weights,
  1
)


latest_nonzero <- latest_weights[
  ,
  as.numeric(latest_weights) > 0,
  drop = FALSE
]


cat(
  "\nMost Recent Target Weights:\n"
)

print(
  latest_nonzero
)