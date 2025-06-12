# ============================================================
# File:        shared_utils.R
# Purpose:     Reusable helper functions for trading strategies
# Author:      Brian Staverosky
# Created:     2025-07-25
# Description: 
#   - Functions shared across strategy scripts and aggregation
#   - Includes export_strategy_weights() and future utilities
# Usage:
#   source("02_strategies/shared_utils.R")
# ============================================================

export_strategy_output <- function(
    strategy_name,
    returns_xts,
    weights_xts,
    output_dir = "03_portfolio_aggregation/strategy_outputs"
) {
  stopifnot(!missing(strategy_name), !missing(returns_xts), !missing(weights_xts))
  stopifnot("xts" %in% class(returns_xts))
  stopifnot("xts" %in% class(weights_xts))
  
  # Extract the most recent weights row
  latest_weights_xts <- tail(weights_xts, 1)
  
  # Construct output list
  output <- list(
    returns = returns_xts,
    weights = weights_xts,
    latest_weights = latest_weights_xts
  )
  
  # Create output dir if needed
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save to RDS
  file_path <- file.path(output_dir, paste0(strategy_name, "_output.rds"))
  saveRDS(output, file = file_path)
  message("Exported strategy output to: ", file_path)
}



