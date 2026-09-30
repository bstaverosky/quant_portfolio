# ==============================================================================
# JOSH MULTI-ASSET MOMENTUM - FULL INTEGRATED SCRIPT
# ORIGINAL STRATEGY + ASSET-FAMILY-AWARE STRATEGY
# ==============================================================================
#
# METHODOLOGY NOTE -------------------------------------------------------------
#
# PURPOSE
#   This is a standalone, end-to-end version of the original Josh strategy
#   script with the researched Asset-Family-Aware model integrated directly
#   into it.
#
# DATA
#   Long-history asset-class returns:
#       ftblog::aaa_returns
#
#   ETF-era proxies:
#       Cash -> synthetic zero return
#       SPY  -> U.S. equities
#       VGK  -> European equities
#       EWJ  -> Japanese equities
#       EEM  -> emerging-market equities
#       ICF  -> U.S. real estate
#       RWX  -> international real estate
#       IEF  -> intermediate Treasuries
#       TLT  -> long Treasuries
#       DBC  -> commodities
#       GLD  -> gold
#
#   The ETF history is appended beginning after 2023-12-29, matching the
#   original script. Yahoo Close is used rather than Adjusted so this remains
#   comparable with the existing research. A total-return splice should be
#   researched separately.
#
# -------------------------------------------------------------------------------
# ORIGINAL JOSH STRATEGY
#
#   1. Rebalance monthly.
#   2. Calculate 120-trading-day cumulative momentum.
#   3. Retain up to the top 3 assets whose momentum is above the cross-sectional
#      average.
#   4. Equal-risk-contribution weight the selected assets using 42 days of
#      trailing returns.
#   5. Lag weights one trading day.
#   6. Hold weights until the next monthly rebalance.
#   7. Research return is multiplied by 3.
#
# -------------------------------------------------------------------------------
# ASSET-FAMILY-AWARE STRATEGY
#
#   UNIVERSAL MOMENTUM SIGNAL
#     - Calculate cumulative return over 63, 126 and 189 trading days.
#     - Cross-sectionally rank every asset at each horizon.
#     - Average the three rank scores.
#     - Only assets with an average score above the cross-sectional mean remain
#       eligible.
#
#   EQUITIES: SPY / VGK / EEM
#     - At least 2 of the 63 / 126 / 252-day absolute returns must be positive.
#     - Maximum TWO regional equity exposures at one time.
#
#   RATES: IEF / TLT
#     - BOTH 126 and 252-day absolute returns must be positive.
#     - Maximum ONE Treasury-duration exposure at one time.
#
#   GOLD: GLD
#     - BOTH 126 and 252-day absolute returns must be positive.
#
#   U.S. REITs: ICF
#     - No additional absolute-momentum gate. A separate ICF gate hurt
#       validation in the research.
#
#   CASH
#     - Remains in the ranking universe.
#
#   FINAL PORTFOLIO
#     - Process candidates from strongest universal momentum score downward.
#     - Apply family gates and concentration limits.
#     - Retain up to 5 assets.
#     - ERC-weight selected assets using 42 trailing trading days.
#     - Lag one trading day.
#     - Hold until the next month-end rebalance.
#     - Research return is multiplied by 3.
#
# RESEARCH SNAPSHOT
#   Approximate gross 3x Python-replication results, common history:
#
#                              CAGR        Sharpe       Max DD
#   Universal ensemble          ~35.0%      ~1.166       ~-41.8%
#   Asset-family-aware          ~38.1%      ~1.179       ~-38.2%
#
#   Validation 2015-2020:
#     Universal                 ~20.1% CAGR / ~0.91 Sharpe
#     Family-aware              ~26.2% CAGR / ~1.01 Sharpe
#
#   Holdout 2021-2026:
#     Universal                 ~31.5% CAGR / ~1.03 Sharpe
#     Family-aware              ~35.0% CAGR / ~1.08 Sharpe
#
# IMPORTANT
#   These are historical research results, not expected future returns.
#   Transaction costs, leveraged-ETF financing, tracking error, taxes and
#   slippage are not fully modeled by simply multiplying the unlevered return
#   series by 3.
#
# ==============================================================================


rm(list = ls())


### PACKAGES -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  library(xts)
  library(zoo)

  source("~/quant_portfolio/02_strategies/utils.R")
})


### PARAMETERS -----------------------------------------------------------------

use_cash <- TRUE

strategy_leverage <- 3

# Original strategy
original_n_assets  <- 3
original_n_days    <- 120
original_n_days_vol <- 42
original_momo_type <- "above average"

# Asset-family-aware strategy
family_universal_horizons <- c(63, 126, 189)

family_equity_gate_horizons <- c(63, 126, 252)
family_rates_gate_horizons  <- c(126, 252)
family_gold_gate_horizons   <- c(126, 252)

family_max_equity_positions <- 2
family_max_rates_positions  <- 1

family_n_assets <- 5
family_n_days_vol <- 42

# Data splice
etf_splice_date <- as.Date("2023-12-29")

# Export
export_outputs <- TRUE

aggregation_output_dir <-
  "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"


### GENERAL FUNCTIONS ----------------------------------------------------------

strat_summary <- function(
    returns,
    original_results = NULL) {

  stats <- table.AnnualizedReturns(
    returns
  )

  stats <- rbind(
    stats,
    "Worst Drawdown" =
      -maxDrawdown(returns)
  )

  if (!is.null(original_results)) {

    stats <- cbind(
      original_results,
      stats
    )

    colnames(stats)[1] <- "Original"
  }

  round(
    stats,
    3
  )
}


chart_performance <- function(
    R,
    title = "Performance") {

  stopifnot(
    all(
      c(
        "Replication",
        "OOS"
      ) %in% colnames(R)
    )
  )

  r <- R[
    ,
    c(
      "Replication",
      "OOS"
    )
  ]

  p <- chart.CumReturns(
    r,
    main = title,
    main.timespan = FALSE,
    yaxis.right = TRUE
  )

  p <- addLegend(
    "topleft",
    lty = 1,
    lwd = 1
  )

  p <- addSeries(
    r[, 1],
    type = "h",
    main = "Return"
  )

  p <- addSeries(
    r[, 2],
    type = "h",
    on = 0,
    col = "red"
  )

  p <- addSeries(
    Drawdowns(r),
    main = "Drawdown"
  )

  p
}


.find_top_momo_columns <- function(
    returns,
    n_assets = 5,
    type = c(
      "relative",
      "positive",
      "above average"
    )) {

  type <- match.arg(type)

  include_cols <- switch(
    type,

    "relative" =
      rep(
        TRUE,
        length(returns)
      ),

    "positive" =
      returns > 0,

    "above average" =
      returns >
      mean(
        returns,
        na.rm = TRUE
      )
  )

  which_cols <- which(
    include_cols
  )

  if (length(which_cols) > 0) {

    momo_rank <- order(
      returns,
      decreasing = TRUE
    )

    top_cols <-
      momo_rank[
        momo_rank %in%
          which_cols
      ]

    top_cols <- head(
      top_cols,
      n_assets
    )

  } else {

    top_cols <- integer()
  }

  top_cols
}


### ERC FUNCTION ---------------------------------------------------------------

portf_wts_equal_risk_local <- function(
    returns,
    n_days_vol = 60) {

  if (!requireNamespace(
    "FRAPO",
    quietly = TRUE
  )) {

    stop(
      "Please install the FRAPO package."
    )
  }

  x <- tail(
    returns,
    n_days_vol
  )

  sigma <- cov(x)

  capture.output({

    optim_portf <- FRAPO::PERC(
      sigma,
      percentage = FALSE
    )
  })

  w <- as.numeric(
    FRAPO::Weights(
      optim_portf
    )
  )

  names(w) <- colnames(x)

  w
}


### ORIGINAL MOMENTUM STRATEGY -------------------------------------------------

portf_return_momo_equal_risk <- function(
    returns,
    n_assets = 5,
    n_days = 120,
    n_days_vol = 60,
    momo_type = c(
      "relative",
      "positive",
      "above average"
    ),
    otype = c(
      "returns",
      "weights"
    )) {

  momo_type <- match.arg(
    momo_type
  )

  otype <- match.arg(
    otype
  )

  month_end_i <- endpoints(
    returns,
    "months"
  )

  month_end_i <- month_end_i[
    month_end_i > n_days
  ]

  weights <- returns * NA

  for (i in month_end_i) {

    n_day_returns <- returns[
      (i - n_days):i,
      ,
      drop = FALSE
    ]

    momentum_returns <- apply(
      1 + n_day_returns,
      2,
      prod
    ) - 1

    weights[i, ] <- 0

    top_cols <-
      .find_top_momo_columns(
        momentum_returns,
        n_assets,
        momo_type
      )

    if (length(top_cols) >= 2) {

      weights[
        i,
        top_cols
      ] <-
        portf_wts_equal_risk_local(
          n_day_returns[
            ,
            top_cols,
            drop = FALSE
          ],
          n_days_vol
        )

    } else if (length(top_cols) == 1) {

      weights[
        i,
        top_cols
      ] <- 1
    }
  }

  # Signal determined at month-end, traded next row/day.
  weights <- lag(
    weights,
    1
  )

  weights <- zoo::na.locf(
    weights,
    na.rm = FALSE
  )

  Rp <- xts(
    rowSums(
      returns *
        weights,
      na.rm = FALSE
    ),
    index(returns),
    weights = weights
  )

  colnames(Rp) <-
    "R_momo_eq_risk"

  if (otype == "returns") {
    return(Rp)
  }

  weights
}


### OPTIONAL WEEKLY VERSION FROM ORIGINAL SCRIPT -------------------------------

portf_return_momo_erc_brian <- function(
    returns,
    n_assets = 5,
    n_days = 120,
    n_days_vol = 60,
    momo_type = c(
      "relative",
      "positive",
      "above average"
    ),
    otype = c(
      "returns",
      "weights"
    )) {

  momo_type <- match.arg(
    momo_type
  )

  otype <- match.arg(
    otype
  )

  rebalance_i <- endpoints(
    returns,
    "weeks"
  )

  rebalance_i <- rebalance_i[
    rebalance_i > n_days
  ]

  weights <- returns * NA

  for (i in rebalance_i) {

    n_day_returns <- returns[
      (i - n_days):i,
      ,
      drop = FALSE
    ]

    momentum_returns <- apply(
      1 + n_day_returns,
      2,
      prod
    ) - 1

    weights[i, ] <- 0

    top_cols <-
      .find_top_momo_columns(
        momentum_returns,
        n_assets,
        momo_type
      )

    if (length(top_cols) >= 2) {

      weights[
        i,
        top_cols
      ] <-
        portf_wts_equal_risk_local(
          n_day_returns[
            ,
            top_cols,
            drop = FALSE
          ],
          n_days_vol
        )

    } else if (length(top_cols) == 1) {

      weights[
        i,
        top_cols
      ] <- 1
    }
  }

  weights <- lag(
    weights,
    1
  )

  weights <- zoo::na.locf(
    weights,
    na.rm = FALSE
  )

  Rp <- xts(
    rowSums(
      returns *
        weights,
      na.rm = FALSE
    ),
    index(returns),
    weights = weights
  )

  colnames(Rp) <-
    "R_momo_eq_risk_weekly"

  if (otype == "returns") {
    return(Rp)
  }

  weights
}


### ASSET-FAMILY-AWARE SIGNAL FUNCTIONS ----------------------------------------

compound_window_return <- function(
    returns,
    i,
    n_days,
    asset) {

  x <- returns[
    (i - n_days):i,
    asset,
    drop = FALSE
  ]

  as.numeric(
    prod(
      1 + x
    ) - 1
  )
}


asset_family <- function(asset) {

  if (asset %in%
      c(
        "SPY",
        "VGK",
        "EEM"
      )) {

    return("Equity")
  }

  if (asset %in%
      c(
        "IEF",
        "TLT"
      )) {

    return("Rates")
  }

  if (asset == "ICF") {
    return("REIT")
  }

  if (asset == "GLD") {
    return("Gold")
  }

  if (asset == "Cash") {
    return("Cash")
  }

  "Other"
}


universal_rank_score <- function(
    returns,
    i,
    horizons =
      c(
        63,
        126,
        189
      )) {

  momentum_matrix <- do.call(
    cbind,
    lapply(
      horizons,
      function(h) {

        x <- returns[
          (i - h):i,
          ,
          drop = FALSE
        ]

        apply(
          1 + x,
          2,
          prod
        ) - 1
      }
    )
  )

  colnames(momentum_matrix) <-
    paste0(
      "mom_",
      horizons
    )

  rownames(momentum_matrix) <-
    colnames(returns)

  rank_matrix <- apply(
    momentum_matrix,
    2,
    function(x) {

      rank(
        x,
        ties.method = "average",
        na.last = "keep"
      ) /
        sum(
          !is.na(x)
        )
    }
  )

  if (is.null(
    dim(rank_matrix)
  )) {

    rank_matrix <- matrix(
      rank_matrix,
      ncol = 1,
      dimnames = list(
        colnames(returns),
        paste0(
          "mom_",
          horizons
        )
      )
    )
  }

  score <- rowMeans(
    rank_matrix,
    na.rm = TRUE
  )

  list(
    score = score,
    momentum = momentum_matrix,
    ranks = rank_matrix
  )
}


family_gate_pass <- function(
    returns,
    i,
    asset) {

  fam <- asset_family(
    asset
  )

  if (fam %in%
      c(
        "Cash",
        "REIT"
      )) {

    return(TRUE)
  }

  if (fam == "Equity") {

    mom <- sapply(
      family_equity_gate_horizons,
      function(h) {

        compound_window_return(
          returns,
          i,
          h,
          asset
        )
      }
    )

    return(
      sum(
        mom > 0,
        na.rm = TRUE
      ) >= 2
    )
  }

  if (fam == "Rates") {

    mom <- sapply(
      family_rates_gate_horizons,
      function(h) {

        compound_window_return(
          returns,
          i,
          h,
          asset
        )
      }
    )

    return(
      all(
        mom > 0
      )
    )
  }

  if (fam == "Gold") {

    mom <- sapply(
      family_gold_gate_horizons,
      function(h) {

        compound_window_return(
          returns,
          i,
          h,
          asset
        )
      }
    )

    return(
      all(
        mom > 0
      )
    )
  }

  TRUE
}


select_family_aware_assets <- function(
    returns,
    i,
    n_assets = 5) {

  signal <- universal_rank_score(
    returns = returns,
    i = i,
    horizons =
      family_universal_horizons
  )

  score <- signal$score

  eligible <- score >
    mean(
      score,
      na.rm = TRUE
    )

  ordered <- order(
    score,
    decreasing = TRUE,
    na.last = NA
  )

  ordered <- ordered[
    eligible[
      ordered
    ]
  ]

  selected <- integer()

  n_equity <- 0
  n_rates  <- 0

  exclusion_reason <- rep(
    "",
    NCOL(returns)
  )

  names(exclusion_reason) <-
    colnames(returns)

  for (j in ordered) {

    asset <- colnames(
      returns
    )[j]

    fam <- asset_family(
      asset
    )

    if (!family_gate_pass(
      returns,
      i,
      asset
    )) {

      exclusion_reason[
        asset
      ] <- "family_gate"

      next
    }

    if (
      fam == "Equity" &&
      n_equity >=
        family_max_equity_positions
    ) {

      exclusion_reason[
        asset
      ] <- "equity_cap"

      next
    }

    if (
      fam == "Rates" &&
      n_rates >=
        family_max_rates_positions
    ) {

      exclusion_reason[
        asset
      ] <- "rates_cap"

      next
    }

    selected <- c(
      selected,
      j
    )

    if (fam == "Equity") {
      n_equity <- n_equity + 1
    }

    if (fam == "Rates") {
      n_rates <- n_rates + 1
    }

    if (length(selected) >=
        n_assets) {

      break
    }
  }

  list(
    selected = selected,
    score = score,
    momentum =
      signal$momentum,
    ranks =
      signal$ranks,
    exclusion_reason =
      exclusion_reason
  )
}


portf_return_asset_family_aware <- function(
    returns,
    n_assets = 5,
    n_days_vol = 42,
    otype = c(
      "returns",
      "weights"
    )) {

  otype <- match.arg(
    otype
  )

  required_history <- max(
    family_universal_horizons,
    family_equity_gate_horizons,
    family_rates_gate_horizons,
    family_gold_gate_horizons
  )

  month_end_i <- endpoints(
    returns,
    "months"
  )

  month_end_i <- month_end_i[
    month_end_i >
      required_history
  ]

  weights <- returns * NA

  for (i in month_end_i) {

    signal <-
      select_family_aware_assets(
        returns = returns,
        i = i,
        n_assets = n_assets
      )

    selected <-
      signal$selected

    weights[
      i,
    ] <- 0

    if (length(selected) >= 2) {

      risk_window <- returns[
        (i - n_days_vol + 1):i,
        selected,
        drop = FALSE
      ]

      weights[
        i,
        selected
      ] <-
        portf_wts_equal_risk_local(
          risk_window,
          n_days_vol =
            n_days_vol
        )

    } else if (
      length(selected) == 1
    ) {

      weights[
        i,
        selected
      ] <- 1

    } else {

      # If every candidate is rejected, use Cash.
      if ("Cash" %in%
          colnames(weights)) {

        weights[
          i,
          "Cash"
        ] <- 1
      }
    }
  }

  weights <- lag(
    weights,
    1
  )

  weights <- zoo::na.locf(
    weights,
    na.rm = FALSE
  )

  Rp <- xts(
    rowSums(
      returns *
        weights,
      na.rm = FALSE
    ),
    index(returns),
    weights = weights
  )

  colnames(Rp) <-
    "R_asset_family_aware"

  if (otype == "returns") {
    return(Rp)
  }

  weights
}


### LOAD LONG-HISTORY DATA ------------------------------------------------------

data(
  aaa_returns,
  package = "ftblog"
)


### DOWNLOAD ETF DATA -----------------------------------------------------------

etfs <- c(
  "SPY",
  "VGK",
  "EWJ",
  "EEM",
  "ICF",
  "RWX",
  "IEF",
  "TLT",
  "DBC",
  "GLD"
)

assets <- lapply(
  etfs,
  function(x) {

    cat(
      "Downloading",
      x,
      "...\n"
    )

    df <- getSymbols(
      x,
      auto.assign = FALSE
    )

    # Preserve original implementation:
    # use Yahoo Close rather than Adjusted.
    df <- df[, 4]

    names(df) <-
      gsub(
        ".Close",
        "",
        names(df)
      )

    df <- zoo::na.locf(
      df,
      na.rm = FALSE
    )

    Return.calculate(
      df
    )
  }
)

assets <- do.call(
  cbind,
  assets
)


### MAP LONG HISTORY TO ETF PROXIES --------------------------------------------

asset_names <- c(
  "SPY",
  "VGK",
  "EWJ",
  "EEM",
  "ICF",
  "RWX",
  "IEF",
  "TLT",
  "DBC",
  "GLD"
)

if (use_cash) {

  returns <- aaa_returns

  assets$Cash <- 0

  assets <- assets[
    ,
    c(
      "Cash",
      asset_names
    )
  ]

  names(returns) <-
    c(
      "Cash",
      asset_names
    )

} else {

  returns <- aaa_returns[
    ,
    -1
  ]

  names(returns) <-
    asset_names

  assets <- assets[
    ,
    asset_names
  ]
}


### SPLICE ETF ERA --------------------------------------------------------------

assets <- assets[
  index(assets) >
    etf_splice_date,
]


### COMBINE HISTORY -------------------------------------------------------------

returns <- rbind(
  returns,
  assets
)

returns <- returns[
  order(
    index(returns)
  ),
]


### STRATEGY UNIVERSE -----------------------------------------------------------

strategy_universe <- c(
  "Cash",
  "SPY",
  "VGK",
  "EEM",
  "ICF",
  "IEF",
  "TLT",
  "GLD"
)

if (!use_cash) {

  strategy_universe <-
    strategy_universe[
      strategy_universe !=
        "Cash"
    ]
}

r_full <- returns[
  ,
  strategy_universe
]

# Preserve the tiny positive Cash return from the original script.
if ("Cash" %in%
    colnames(r_full)) {

  r_full$Cash <-
    0.000000000001
}


# ==============================================================================
# RUN ORIGINAL STRATEGY
# ==============================================================================

original_returns <-
  portf_return_momo_equal_risk(
    r_full,
    n_assets =
      original_n_assets,
    n_days =
      original_n_days,
    n_days_vol =
      original_n_days_vol,
    momo_type =
      original_momo_type,
    otype = "returns"
  )

original_weights <-
  portf_return_momo_equal_risk(
    r_full,
    n_assets =
      original_n_assets,
    n_days =
      original_n_days,
    n_days_vol =
      original_n_days_vol,
    momo_type =
      original_momo_type,
    otype = "weights"
  )

original_3x <-
  original_returns *
  strategy_leverage

colnames(original_3x) <-
  "Original_Josh_3x"


# ==============================================================================
# RUN ASSET-FAMILY-AWARE STRATEGY
# ==============================================================================

family_returns <-
  portf_return_asset_family_aware(
    r_full,
    n_assets =
      family_n_assets,
    n_days_vol =
      family_n_days_vol,
    otype = "returns"
  )

family_weights <-
  portf_return_asset_family_aware(
    r_full,
    n_assets =
      family_n_assets,
    n_days_vol =
      family_n_days_vol,
    otype = "weights"
  )

family_3x <-
  family_returns *
  strategy_leverage

colnames(family_3x) <-
  "Asset_Family_Aware_3x"


# ==============================================================================
# ANALYTICS
# ==============================================================================

comparison <- merge(
  original_3x,
  family_3x,
  r_full$SPY,
  join = "inner"
)

colnames(comparison) <- c(
  "Original Josh",
  "Asset Family Aware",
  "S&P 500"
)

comparison <-
  comparison[
    complete.cases(
      comparison
    ),
  ]


cat(
  "\n\n================ FULL PERIOD ================\n"
)

print(
  table.AnnualizedReturns(
    comparison
  )
)

cat(
  "\nAnnualized Sharpe:\n"
)

print(
  SharpeRatio.annualized(
    comparison
  )
)

cat(
  "\nMaximum Drawdown:\n"
)

print(
  maxDrawdown(
    comparison
  )
)

cat(
  "\nCumulative Return:\n"
)

print(
  Return.cumulative(
    comparison
  )
)


charts.PerformanceSummary(
  comparison,
  main =
    "Josh Strategy: Original vs Asset-Family-Aware"
)


### RESEARCH SUBPERIODS ---------------------------------------------------------

research_periods <- list(
  Discovery =
    "1997-03-03/2014-12-31",
  Validation =
    "2015-01-01/2020-12-31",
  Holdout =
    "2021-01-01/"
)

for (nm in names(
  research_periods
)) {

  x <- comparison[
    research_periods[[nm]]
  ]

  cat(
    "\n\n================ ",
    nm,
    " ================\n",
    sep = ""
  )

  print(
    table.AnnualizedReturns(
      x
    )
  )

  print(
    SharpeRatio.annualized(
      x
    )
  )

  print(
    maxDrawdown(
      x
    )
  )
}


### CURRENT WEIGHTS -------------------------------------------------------------

cat(
  "\n\n================ CURRENT ORIGINAL WEIGHTS ================\n"
)

print(
  round(
    tail(
      original_weights,
      1
    ),
    4
  )
)


cat(
  "\n\n================ CURRENT FAMILY-AWARE WEIGHTS ================\n"
)

print(
  round(
    tail(
      family_weights,
      1
    ),
    4
  )
)


### CURRENT FAMILY SIGNAL DIAGNOSTICS ------------------------------------------

month_end_i <- endpoints(
  r_full,
  "months"
)

required_history <- max(
  family_universal_horizons,
  family_equity_gate_horizons,
  family_rates_gate_horizons,
  family_gold_gate_horizons
)

month_end_i <- month_end_i[
  month_end_i >
    required_history
]

latest_signal_i <- tail(
  month_end_i,
  1
)

latest_signal <-
  select_family_aware_assets(
    r_full,
    latest_signal_i,
    family_n_assets
  )

diagnostic <- data.frame(
  Asset =
    colnames(r_full),

  Family =
    sapply(
      colnames(r_full),
      asset_family
    ),

  Universal_Rank_Score =
    as.numeric(
      latest_signal$score
    ),

  Gate_Pass =
    sapply(
      colnames(r_full),
      function(x) {

        family_gate_pass(
          r_full,
          latest_signal_i,
          x
        )
      }
    ),

  Exclusion_Reason =
    latest_signal$exclusion_reason,

  stringsAsFactors =
    FALSE
)

diagnostic$Selected <-
  diagnostic$Asset %in%
  colnames(r_full)[
    latest_signal$selected
  ]

cat(
  "\nLatest family-aware signal date:",
  as.character(
    index(r_full)[
      latest_signal_i
    ]
  ),
  "\n"
)

print(
  diagnostic[
    order(
      diagnostic$Universal_Rank_Score,
      decreasing = TRUE
    ),
  ]
)


# ==============================================================================
# EXPORT TO PORTFOLIO AGGREGATION
# ==============================================================================

if (
  export_outputs &&
  exists(
    "export_strategy_output",
    mode = "function"
  )
) {

  leveraged_name_map <- c(
    Cash = "cash",
    SPY = "UPRO",
    VGK = "EURL",
    EEM = "EDC",
    ICF = "DRN",
    IEF = "TYD",
    TLT = "TMF",
    GLD = "SHNY"
  )


  ### EXPORT ORIGINAL JOSH STRATEGY --------------------------------------------

  original_export_returns <-
    original_returns *
    strategy_leverage

  colnames(
    original_export_returns
  ) <- "Josh_Strat"

  original_export_returns <-
    original_export_returns[
      complete.cases(
        original_export_returns
      ),
    ]

  original_export_weights <-
    original_weights[
      index(
        original_export_returns
      ),
    ]

  colnames(
    original_export_weights
  ) <-
    unname(
      leveraged_name_map[
        colnames(
          original_export_weights
        )
      ]
    )

  export_strategy_output(
    strategy_name =
      "Josh_Strat",

    returns_xts =
      original_export_returns,

    weights_xts =
      original_export_weights,

    output_dir =
      aggregation_output_dir
  )


  ### EXPORT ASSET-FAMILY-AWARE STRATEGY ---------------------------------------

  family_export_returns <-
    family_returns *
    strategy_leverage

  colnames(
    family_export_returns
  ) <-
    "Josh_Asset_Family"

  family_export_returns <-
    family_export_returns[
      complete.cases(
        family_export_returns
      ),
    ]

  family_export_weights <-
    family_weights[
      index(
        family_export_returns
      ),
    ]

  colnames(
    family_export_weights
  ) <-
    unname(
      leveraged_name_map[
        colnames(
          family_export_weights
        )
      ]
    )

  export_strategy_output(
    strategy_name =
      "Josh_Asset_Family",

    returns_xts =
      family_export_returns,

    weights_xts =
      family_export_weights,

    output_dir =
      aggregation_output_dir
  )

} else {

  cat(
    "\nexport_strategy_output() was not found, so aggregation exports were skipped.\n"
  )
}


# ==============================================================================
# OPTIONAL QUICK CHECKS
# ==============================================================================

cat(
  "\n\n================ QUICK CHECKS ================\n"
)

cat(
  "\nOriginal 3x annualized return:\n"
)

print(
  Return.annualized(
    original_3x
  )
)

cat(
  "\nFamily-aware 3x annualized return:\n"
)

print(
  Return.annualized(
    family_3x
  )
)

cat(
  "\nOriginal 3x annualized Sharpe:\n"
)

print(
  SharpeRatio.annualized(
    original_3x
  )
)

cat(
  "\nFamily-aware 3x annualized Sharpe:\n"
)

print(
  SharpeRatio.annualized(
    family_3x
  )
)

cat(
  "\nOriginal 3x max drawdown:\n"
)

print(
  maxDrawdown(
    original_3x
  )
)

cat(
  "\nFamily-aware 3x max drawdown:\n"
)

print(
  maxDrawdown(
    family_3x
  )
)

cat(
  "\n\nDone.\n"
)
