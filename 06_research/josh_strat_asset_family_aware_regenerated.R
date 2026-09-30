# ==============================================================================
# JOSH MULTI-ASSET MOMENTUM: ASSET-FAMILY-AWARE OVERLAY
# ==============================================================================
#
# METHODOLOGY NOTE -------------------------------------------------------------
#
# Purpose:
#   Keep the successful universal 63/126/189-day cross-sectional momentum
#   ensemble, then apply asset-family-specific absolute-momentum gates and
#   concentration limits before ERC portfolio construction.
#
# Universal signal:
#   - Calculate cumulative returns over 63, 126 and 189 trading days.
#   - Cross-sectionally rank each asset at each horizon.
#   - Average the three rank scores.
#   - Assets must rank above the cross-sectional mean to be eligible.
#
# Family rules:
#   Equity family: SPY, VGK, EEM
#     - At least 2 of 63/126/252-day returns must be positive.
#     - Maximum 2 regional-equity positions at one time.
#
#   Rates family: IEF, TLT
#     - Both 126-day and 252-day returns must be positive.
#     - Maximum 1 Treasury-duration position at one time.
#
#   Gold: GLD
#     - Both 126-day and 252-day returns must be positive.
#
#   U.S. REITs: ICF
#     - No extra absolute-momentum gate in the preferred model.
#
#   Cash:
#     - Remains in the ranking universe.
#
# Portfolio construction:
#   - Process candidates from highest universal rank score downward.
#   - Apply family gates and concentration caps.
#   - Retain up to 5 assets.
#   - Equal-risk-contribution (ERC) weight selected assets using trailing
#     42 trading days.
#   - Calculate at month-end, lag one trading day, hold until next rebalance.
#
# Leverage:
#   - Default research comparison = 3x strategy return.
#   - Leverage overlays are intentionally kept separate from signal research.
#
# Research snapshot from the Python study:
#   Universal 63/126/189 ensemble, gross 3x:
#     CAGR ~35.0%, Sharpe ~1.166, Max DD ~-41.8%
#
#   Family-aware constrained ensemble, gross 3x:
#     CAGR ~38.1%, Sharpe ~1.179, Max DD ~-38.2%
#
# Important data note:
#   The source dataset used for the original study spliced Yahoo Close rather
#   than Adjusted prices after 2023. This script intentionally consumes r_full
#   exactly as supplied so it remains comparable to the original research.
#
# ============================================================================== 

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(FRAPO)
})

### PARAMETERS -----------------------------------------------------------------

universal_horizons <- c(63, 126, 189)
equity_gate_horizons <- c(63, 126, 252)
rates_gate_horizons  <- c(126, 252)
gold_gate_horizons   <- c(126, 252)

max_equity_positions <- 2
max_rates_positions  <- 1

n_assets_max <- 5
n_days_vol   <- 42
strategy_leverage <- 3

export_outputs <- TRUE
output_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"


### REQUIRED UNIVERSE ----------------------------------------------------------

required_assets <- c(
  "Cash",
  "SPY",
  "VGK",
  "EEM",
  "ICF",
  "IEF",
  "TLT",
  "GLD"
)

if (!exists("r_full")) {
  stop("This script expects an existing xts object named r_full.")
}

missing_assets <- setdiff(required_assets, colnames(r_full))

if (length(missing_assets) > 0) {
  stop(
    paste(
      "r_full is missing required columns:",
      paste(missing_assets, collapse = ", ")
    )
  )
}

r_family <- r_full[, required_assets]


### HELPERS --------------------------------------------------------------------

family_erc_weights <- function(returns, n_days_vol = 42) {

  x <- tail(returns, n_days_vol)
  sigma <- cov(x)

  capture.output({
    fit <- FRAPO::PERC(
      sigma,
      percentage = FALSE
    )
  })

  w <- as.numeric(FRAPO::Weights(fit))
  names(w) <- colnames(x)

  w
}


compound_window_return <- function(returns, i, n_days, asset) {

  x <- returns[
    (i - n_days):i,
    asset,
    drop = FALSE
  ]

  as.numeric(prod(1 + x) - 1)
}


universal_rank_score <- function(
    returns,
    i,
    horizons = c(63, 126, 189)) {

  momentum_matrix <- do.call(
    cbind,
    lapply(horizons, function(h) {

      x <- returns[
        (i - h):i,
        ,
        drop = FALSE
      ]

      apply(1 + x, 2, prod) - 1
    })
  )

  colnames(momentum_matrix) <- paste0("mom_", horizons)
  rownames(momentum_matrix) <- colnames(returns)

  rank_matrix <- apply(
    momentum_matrix,
    2,
    function(x) {
      rank(
        x,
        ties.method = "average",
        na.last = "keep"
      ) / sum(!is.na(x))
    }
  )

  if (is.null(dim(rank_matrix))) {
    rank_matrix <- matrix(
      rank_matrix,
      ncol = 1,
      dimnames = list(
        colnames(returns),
        paste0("mom_", horizons)
      )
    )
  }

  score <- rowMeans(rank_matrix, na.rm = TRUE)

  list(
    score = score,
    momentum = momentum_matrix,
    ranks = rank_matrix
  )
}


asset_family <- function(asset) {

  if (asset %in% c("SPY", "VGK", "EEM")) return("Equity")
  if (asset %in% c("IEF", "TLT")) return("Rates")
  if (asset == "ICF") return("REIT")
  if (asset == "GLD") return("Gold")
  if (asset == "Cash") return("Cash")

  "Other"
}


family_gate_pass <- function(returns, i, asset) {

  fam <- asset_family(asset)

  if (fam %in% c("Cash", "REIT")) {
    return(TRUE)
  }

  if (fam == "Equity") {

    mom <- sapply(
      equity_gate_horizons,
      function(h) {
        compound_window_return(
          returns,
          i,
          h,
          asset
        )
      }
    )

    return(sum(mom > 0, na.rm = TRUE) >= 2)
  }

  if (fam == "Rates") {

    mom <- sapply(
      rates_gate_horizons,
      function(h) {
        compound_window_return(
          returns,
          i,
          h,
          asset
        )
      }
    )

    return(all(mom > 0))
  }

  if (fam == "Gold") {

    mom <- sapply(
      gold_gate_horizons,
      function(h) {
        compound_window_return(
          returns,
          i,
          h,
          asset
        )
      }
    )

    return(all(mom > 0))
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
    horizons = universal_horizons
  )

  score <- signal$score

  eligible <- score > mean(score, na.rm = TRUE)

  ordered <- order(
    score,
    decreasing = TRUE,
    na.last = NA
  )

  ordered <- ordered[eligible[ordered]]

  selected <- integer()
  n_equity <- 0
  n_rates <- 0

  exclusion_reason <- rep("", NCOL(returns))
  names(exclusion_reason) <- colnames(returns)

  for (j in ordered) {

    asset <- colnames(returns)[j]
    fam <- asset_family(asset)

    if (!family_gate_pass(returns, i, asset)) {
      exclusion_reason[asset] <- "family_gate"
      next
    }

    if (
      fam == "Equity" &&
      n_equity >= max_equity_positions
    ) {
      exclusion_reason[asset] <- "equity_cap"
      next
    }

    if (
      fam == "Rates" &&
      n_rates >= max_rates_positions
    ) {
      exclusion_reason[asset] <- "rates_cap"
      next
    }

    selected <- c(selected, j)

    if (fam == "Equity") n_equity <- n_equity + 1
    if (fam == "Rates") n_rates <- n_rates + 1

    if (length(selected) >= n_assets) break
  }

  list(
    selected = selected,
    score = score,
    momentum = signal$momentum,
    ranks = signal$ranks,
    exclusion_reason = exclusion_reason
  )
}


### FAMILY-AWARE STRATEGY -------------------------------------------------------

portf_return_asset_family_aware <- function(
    returns,
    n_assets = 5,
    n_days_vol = 42,
    otype = c("returns", "weights")) {

  otype <- match.arg(otype)

  required_history <- max(
    universal_horizons,
    equity_gate_horizons,
    rates_gate_horizons,
    gold_gate_horizons
  )

  month_end_i <- endpoints(returns, "months")
  month_end_i <- month_end_i[month_end_i > required_history]

  weights <- returns * NA

  for (i in month_end_i) {

    signal <- select_family_aware_assets(
      returns = returns,
      i = i,
      n_assets = n_assets
    )

    selected <- signal$selected

    weights[i, ] <- 0

    if (length(selected) >= 2) {

      risk_window <- returns[
        (i - n_days_vol + 1):i,
        selected,
        drop = FALSE
      ]

      weights[i, selected] <- family_erc_weights(
        risk_window,
        n_days_vol = n_days_vol
      )

    } else if (length(selected) == 1) {

      weights[i, selected] <- 1

    } else {

      weights[i, "Cash"] <- 1
    }
  }

  # Signal is observed at month-end and traded next row/day.
  weights <- lag(weights, 1)
  weights <- zoo::na.locf(weights, na.rm = FALSE)

  Rp <- xts(
    rowSums(returns * weights),
    index(returns),
    weights = weights
  )

  colnames(Rp) <- "R_asset_family_aware"

  if (otype == "returns") {
    return(Rp)
  }

  weights
}


### UNIVERSAL BENCHMARK ---------------------------------------------------------

portf_return_universal_rank_ensemble <- function(
    returns,
    n_assets = 5,
    n_days_vol = 42,
    otype = c("returns", "weights")) {

  otype <- match.arg(otype)

  month_end_i <- endpoints(returns, "months")
  month_end_i <- month_end_i[month_end_i > max(universal_horizons)]

  weights <- returns * NA

  for (i in month_end_i) {

    signal <- universal_rank_score(
      returns,
      i,
      universal_horizons
    )

    score <- signal$score
    eligible <- score > mean(score, na.rm = TRUE)

    ordered <- order(
      score,
      decreasing = TRUE,
      na.last = NA
    )

    selected <- ordered[eligible[ordered]]
    selected <- head(selected, n_assets)

    weights[i, ] <- 0

    if (length(selected) >= 2) {

      risk_window <- returns[
        (i - n_days_vol + 1):i,
        selected,
        drop = FALSE
      ]

      weights[i, selected] <- family_erc_weights(
        risk_window,
        n_days_vol = n_days_vol
      )

    } else if (length(selected) == 1) {

      weights[i, selected] <- 1

    } else {

      weights[i, "Cash"] <- 1
    }
  }

  weights <- lag(weights, 1)
  weights <- zoo::na.locf(weights, na.rm = FALSE)

  Rp <- xts(
    rowSums(returns * weights),
    index(returns),
    weights = weights
  )

  colnames(Rp) <- "R_universal_rank_ensemble"

  if (otype == "returns") {
    return(Rp)
  }

  weights
}


### RUN MODELS -----------------------------------------------------------------

family_returns <- portf_return_asset_family_aware(
  r_family,
  n_assets = n_assets_max,
  n_days_vol = n_days_vol,
  otype = "returns"
)

family_weights <- portf_return_asset_family_aware(
  r_family,
  n_assets = n_assets_max,
  n_days_vol = n_days_vol,
  otype = "weights"
)

universal_returns <- portf_return_universal_rank_ensemble(
  r_family,
  n_assets = n_assets_max,
  n_days_vol = n_days_vol,
  otype = "returns"
)

universal_weights <- portf_return_universal_rank_ensemble(
  r_family,
  n_assets = n_assets_max,
  n_days_vol = n_days_vol,
  otype = "weights"
)


### LEVERAGED RESEARCH RETURNS --------------------------------------------------

family_3x <- family_returns * strategy_leverage
universal_3x <- universal_returns * strategy_leverage

comparison <- merge(
  universal_3x,
  family_3x
)

colnames(comparison) <- c(
  "Universal_Ensemble_3x",
  "Family_Aware_3x"
)

comparison <- comparison[complete.cases(comparison), ]


### ANALYTICS ------------------------------------------------------------------

print(table.AnnualizedReturns(comparison))
print(SharpeRatio.annualized(comparison))
print(maxDrawdown(comparison))

charts.PerformanceSummary(
  comparison,
  main = "Universal vs Asset-Family-Aware Momentum"
)


### SUBPERIOD REPORT ------------------------------------------------------------

research_periods <- list(
  Discovery = "1997-03-03/2014-12-31",
  Validation = "2015-01-01/2020-12-31",
  Holdout = "2021-01-01/2026-09-23"
)

for (nm in names(research_periods)) {

  period_returns <- comparison[research_periods[[nm]]]

  cat(
    "\n\n================ ",
    nm,
    " ================\n",
    sep = ""
  )

  print(table.AnnualizedReturns(period_returns))
  print(SharpeRatio.annualized(period_returns))
  print(maxDrawdown(period_returns))
}


### CURRENT PORTFOLIO -----------------------------------------------------------

cat("\n\nCURRENT FAMILY-AWARE WEIGHTS\n")
print(round(tail(family_weights, 1), 4))


### CURRENT SIGNAL DIAGNOSTIC ---------------------------------------------------

month_end_i <- endpoints(r_family, "months")

month_end_i <- month_end_i[
  month_end_i > max(
    universal_horizons,
    equity_gate_horizons,
    rates_gate_horizons,
    gold_gate_horizons
  )
]

latest_signal_i <- tail(month_end_i, 1)

latest_signal <- select_family_aware_assets(
  r_family,
  latest_signal_i,
  n_assets_max
)

diagnostic <- data.frame(
  Asset = colnames(r_family),
  Family = sapply(colnames(r_family), asset_family),
  Universal_Rank_Score = as.numeric(latest_signal$score),
  Gate_Pass = sapply(
    colnames(r_family),
    function(x) {
      family_gate_pass(
        r_family,
        latest_signal_i,
        x
      )
    }
  ),
  Exclusion_Reason = latest_signal$exclusion_reason,
  stringsAsFactors = FALSE
)

diagnostic$Selected <- diagnostic$Asset %in%
  colnames(r_family)[latest_signal$selected]

cat(
  "\nLatest signal date:",
  as.character(index(r_family)[latest_signal_i]),
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


### OPTIONAL EXPORT TO AGGREGATION ---------------------------------------------

if (
  export_outputs &&
  exists("export_strategy_output", mode = "function")
) {

  returns_xts <- family_returns * strategy_leverage
  colnames(returns_xts) <- "Josh_Asset_Family"
  returns_xts <- returns_xts[complete.cases(returns_xts), ]

  weights_xts <- family_weights[index(returns_xts), ]

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

  colnames(weights_xts) <- unname(
    leveraged_name_map[colnames(weights_xts)]
  )

  export_strategy_output(
    strategy_name = "Josh_Asset_Family",
    returns_xts = returns_xts,
    weights_xts = weights_xts,
    output_dir = output_dir
  )
}
