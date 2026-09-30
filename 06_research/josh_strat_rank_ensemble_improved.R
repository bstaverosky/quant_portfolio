# ==============================================================================
# JOSH MULTI-ASSET MOMENTUM: RESEARCH UPGRADE
# ==============================================================================
#
# Intended use:
#   Source/run AFTER the packages and r_full object from the existing Josh
#   strategy script have been created.
#
# Research result:
#   The strongest robust improvement found in the Python replication was a
#   multi-horizon rank-ensemble momentum signal using 63 / 126 / 189 trading
#   days, monthly rebalancing, up to 5 selected assets, and 42-day equal-risk
#   contribution (ERC) weighting.
#
#   A 50/50 blend of the original engine and the ensemble engine is also
#   included because it diversifies signal-model risk and preserved more of
#   the original strategy's recent upside in the 2021-2026 holdout.
#
# ==============================================================================
#
# METHODOLOGY ------------------------------------------------------------------
#
# ORIGINAL ENGINE
#   - Universe: Cash, SPY, VGK, EEM, ICF, IEF, TLT, GLD.
#   - Signal: 120-trading-day cumulative return.
#   - Selection: assets above the cross-sectional average momentum; keep the
#     top 3.
#   - Weighting: equal risk contribution using trailing 42 daily returns.
#   - Rebalance: month-end signal, lagged one trading day.
#   - Cash: retained as the near-zero-return Cash column already used in r_full.
#   - Leverage: strategy return multiplied by 3, matching the existing research
#     convention. Financing, ETF fees, taxes, and market impact are NOT deducted.
#
# ENSEMBLE ENGINE
#   - Calculate cumulative momentum over 63, 126, and 189 trading days.
#   - For EACH horizon separately, rank all assets cross-sectionally from worst
#     to best. Ranks are scaled to (1/N ... 1).
#   - Average each asset's percentile-like rank across the three horizons.
#   - Keep assets whose average rank is above the cross-sectional mean.
#   - From those, retain up to the top 5.
#   - Weight selected assets with ERC using the trailing 42 daily returns.
#   - Rebalance monthly and lag the new portfolio by one trading day.
#   - Cash participates in ranking. When Cash is selected, its approximately
#     zero variance naturally causes ERC to move the portfolio heavily toward
#     cash. Economically this behaves like a multi-horizon absolute-momentum
#     risk-off mechanism.
#
# BLENDED ENGINE
#   - 50% original strategy + 50% ensemble strategy by capital.
#   - Returns and weights are blended directly.
#   - This is deliberately simple rather than optimized.
#
# PYTHON RESEARCH SNAPSHOT (gross, same common start date)
#   Original 3x:
#       CAGR ~32.3%, Sharpe ~0.96, Max DD ~-48.4%
#
#   Pure 63/126/189 rank ensemble, 5 assets, ERC(42), 3x:
#       CAGR ~35.3%, Sharpe ~1.17, Max DD ~-41.8%
#
#   50/50 original + ensemble, 3x:
#       CAGR ~34.3%, Sharpe ~1.10, Max DD ~-44.9%
#
# IMPORTANT:
#   These are research backtests, not expected-return forecasts. The historical
#   panel includes synthetic/pre-ETF asset-class history. The current ETF splice
#   in the parent script uses Yahoo Close rather than Adjusted Close, so the
#   post-2023 section is not a clean dividend-adjusted total-return series.
#
# ==============================================================================


### PARAMETERS -----------------------------------------------------------------

ensemble_horizons   <- c(63, 126, 189)
ensemble_n_assets   <- 5
ensemble_n_days_vol <- 42

strategy_leverage   <- 3

# 0.00 = original only
# 0.50 = equal blend (research default)
# 1.00 = pure ensemble
ensemble_blend_weight <- 0.50

export_outputs <- TRUE

output_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"


### HELPER FUNCTIONS -----------------------------------------------------------

portf_wts_equal_risk_ensemble <- function(returns, n_days_vol = 42) {

  if (!requireNamespace("FRAPO", quietly = TRUE)) {
    stop("Please install the FRAPO package.")
  }

  n_day_returns <- tail(returns, n_days_vol)
  sigma <- cov(n_day_returns)

  capture.output({
    optim_portf <- FRAPO::PERC(
      sigma,
      percentage = FALSE
    )
  })

  w <- as.numeric(FRAPO::Weights(optim_portf))
  names(w) <- colnames(n_day_returns)

  w
}


rank_ensemble_signal <- function(returns,
                                 i,
                                 horizons = c(63, 126, 189),
                                 n_assets = 5) {

  momentum_matrix <- do.call(
    cbind,
    lapply(horizons, function(h) {

      x <- returns[(i - h):i, , drop = FALSE]

      apply(
        1 + x,
        2,
        prod
      ) - 1
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

  ensemble_score <- rowMeans(
    rank_matrix,
    na.rm = TRUE
  )

  eligible <- ensemble_score >
    mean(ensemble_score, na.rm = TRUE)

  ordered <- order(
    ensemble_score,
    decreasing = TRUE,
    na.last = NA
  )

  top_cols <- ordered[eligible[ordered]]
  top_cols <- head(top_cols, n_assets)

  list(
    top_cols = top_cols,
    score = ensemble_score,
    momentum = momentum_matrix,
    ranks = rank_matrix
  )
}


portf_return_momo_rank_ensemble_erc <- function(
    returns,
    horizons = c(63, 126, 189),
    n_assets = 5,
    n_days_vol = 42,
    otype = c("returns", "weights")) {

  otype <- match.arg(otype)

  month_end_i <- endpoints(
    returns,
    "months"
  )

  month_end_i <- month_end_i[
    month_end_i > max(horizons)
  ]

  weights <- returns * NA

  for (i in month_end_i) {

    signal <- rank_ensemble_signal(
      returns = returns,
      i = i,
      horizons = horizons,
      n_assets = n_assets
    )

    top_cols <- signal$top_cols

    weights[i, ] <- 0

    if (length(top_cols) >= 2) {

      # Give the ERC helper enough history; it keeps only n_days_vol rows.
      risk_window <- returns[
        (i - max(horizons)):i,
        top_cols,
        drop = FALSE
      ]

      weights[i, top_cols] <-
        portf_wts_equal_risk_ensemble(
          risk_window,
          n_days_vol = n_days_vol
        )
    }
  }

  # Signal formed at month-end becomes tradable on the following row/day.
  weights <- lag(weights, 1)

  weights <- zoo::na.locf(
    weights,
    na.rm = FALSE
  )

  Rp <- xts(
    rowSums(returns * weights),
    index(returns),
    weights = weights
  )

  colnames(Rp) <- "R_momo_rank_ensemble_erc"

  if (otype == "returns") {
    return(Rp)
  }

  weights
}


### RUN ORIGINAL ENGINE AGAIN --------------------------------------------------
# Recompute fresh objects so this add-on does not depend on whether strat_wts
# was renamed to leveraged ETF tickers later in the parent script.

original_returns_research <- portf_return_momo_equal_risk(
  r_full,
  n_assets = 3,
  n_days = 120,
  n_days_vol = 42,
  momo_type = "above average",
  otype = "returns"
)

original_wts_research <- portf_return_momo_equal_risk(
  r_full,
  n_assets = 3,
  n_days = 120,
  n_days_vol = 42,
  momo_type = "above average",
  otype = "weights"
)


### RUN ENSEMBLE ENGINE --------------------------------------------------------

ensemble_returns <- portf_return_momo_rank_ensemble_erc(
  r_full,
  horizons = ensemble_horizons,
  n_assets = ensemble_n_assets,
  n_days_vol = ensemble_n_days_vol,
  otype = "returns"
)

ensemble_wts <- portf_return_momo_rank_ensemble_erc(
  r_full,
  horizons = ensemble_horizons,
  n_assets = ensemble_n_assets,
  n_days_vol = ensemble_n_days_vol,
  otype = "weights"
)


### BUILD 50/50 (OR USER-DEFINED) MODEL BLEND ---------------------------------

common_returns <- merge(
  original_returns_research,
  ensemble_returns,
  join = "inner"
)

common_returns <- common_returns[
  complete.cases(common_returns),
]

blend_returns <-
  (1 - ensemble_blend_weight) *
  common_returns[, 1] +
  ensemble_blend_weight *
  common_returns[, 2]

colnames(blend_returns) <- "Josh_Strat_Blend"


common_dates <- index(common_returns)

original_wts_common <- original_wts_research[
  common_dates,
]

ensemble_wts_common <- ensemble_wts[
  common_dates,
]

blend_wts <-
  (1 - ensemble_blend_weight) *
  original_wts_common +
  ensemble_blend_weight *
  ensemble_wts_common


### LEVERAGE -------------------------------------------------------------------

original_3x <- original_returns_research * strategy_leverage
ensemble_3x <- ensemble_returns * strategy_leverage
blend_3x    <- blend_returns * strategy_leverage

colnames(original_3x) <- "Original_3x"
colnames(ensemble_3x) <- "Ensemble_3x"
colnames(blend_3x)    <- "Blend_3x"


### ANALYTICS ------------------------------------------------------------------

comparison_1x <- merge(
  original_returns_research,
  ensemble_returns,
  blend_returns,
  join = "inner"
)

colnames(comparison_1x) <- c(
  "Original",
  "Rank_Ensemble",
  "50_50_Blend"
)

comparison_3x <- comparison_1x * strategy_leverage


cat("\n================ 1X PERFORMANCE ================\n")
print(
  round(
    table.AnnualizedReturns(comparison_1x),
    4
  )
)

cat("\n1X MAX DRAWDOWN\n")
print(
  round(
    maxDrawdown(comparison_1x),
    4
  )
)


cat("\n================ 3X PERFORMANCE ================\n")
print(
  round(
    table.AnnualizedReturns(comparison_3x),
    4
  )
)

cat("\n3X MAX DRAWDOWN\n")
print(
  round(
    maxDrawdown(comparison_3x),
    4
  )
)


charts.PerformanceSummary(
  comparison_3x,
  main = "Josh Strategy Research: Original vs Rank Ensemble vs 50/50 Blend"
)


### HOLDOUT CHECK --------------------------------------------------------------
# This slice was NOT used as the primary discovery period in the Python search.

holdout <- comparison_3x["2021/"]

cat("\n================ 2021+ HOLDOUT ================\n")
print(
  round(
    table.AnnualizedReturns(holdout),
    4
  )
)

cat("\n2021+ MAX DRAWDOWN\n")
print(
  round(
    maxDrawdown(holdout),
    4
  )
)


### CURRENT WEIGHTS ------------------------------------------------------------

cat("\n================ CURRENT ORIGINAL WEIGHTS ================\n")
print(
  round(
    last(original_wts_research),
    4
  )
)

cat("\n================ CURRENT ENSEMBLE WEIGHTS ================\n")
print(
  round(
    last(ensemble_wts),
    4
  )
)

cat("\n================ CURRENT BLENDED WEIGHTS ================\n")
print(
  round(
    last(blend_wts),
    4
  )
)


### OPTIONAL EXPORT TO PORTFOLIO AGGREGATOR ------------------------------------

if (export_outputs) {

  leveraged_names <- c(
    "cash",
    "UPRO",
    "EURL",
    "EDC",
    "DRN",
    "TYD",
    "TMF",
    "SHNY"
  )

  ensemble_wts_export <- ensemble_wts
  blend_wts_export <- blend_wts

  names(ensemble_wts_export) <- leveraged_names
  names(blend_wts_export) <- leveraged_names

  ensemble_returns_export <- ensemble_3x[
    complete.cases(ensemble_3x),
  ]

  blend_returns_export <- blend_3x[
    complete.cases(blend_3x),
  ]

  export_strategy_output(
    strategy_name = "Josh_Strat_Ensemble",
    returns_xts = ensemble_returns_export,
    weights_xts = ensemble_wts_export,
    output_dir = output_dir
  )

  export_strategy_output(
    strategy_name = "Josh_Strat_Blend",
    returns_xts = blend_returns_export,
    weights_xts = blend_wts_export,
    output_dir = output_dir
  )
}
