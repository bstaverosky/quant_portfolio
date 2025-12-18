# =====================================================================
# adaptive_mw_asset_alloc.R
# Multi-asset strategy using:
#   - Layer 3: Ensemble Multiplicative Weights (different learning/forgetting)
#   - Layer 4: AdaNormalHedge supervisor across the Layer-3 algos
#   - Layer 5: Live portfolio weights across ETFs (optionally levered)
#
# Designed to be a drop-in replacement for the momentum selector in josh_strat.R
# =====================================================================

rm(list = ls())
suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  source("~/quant_portfolio/02_strategies/utils.R")  # adjust path as needed
})

# -----------------------
# PARAMETERS
# -----------------------
use_cash          <- TRUE
rebalance_freq    <- "weeks"     # "weeks" or "months"
rebalance_k       <- 1           # every k weeks/months
portfolio_leverage<- 3           # match your strat_3x behavior
min_weight_floor  <- 0.00        # allow zero weights
max_weight_cap    <- 1.00        # cap weights (pre leverage). keep 1.0 unless you want concentration limits
weight_smoothing  <- 0.00        # optional: blend current weights with prior weights to reduce turnover [0..1]
verbose           <- FALSE
use_momo_filter      <- TRUE
momo_lookback_days   <- 126     # ~6 months of trading days
min_assets_required  <- 3
cash_fallback_weight <- 0.50    # when < min_assets_required survive

# Universe
etfs <- c("SPY","VGK","EWJ","EEM","ICF","IEF","TLT","GLD")

# If you want commodities back, add "DBC" and adjust leveraged mapping below.
# etfs <- c(etfs, "DBC")

# Leveraged mapping for export weights (same intent as your comments)
# NOTE: You can change these if you actually trade different leveraged proxies.
leveraged_map <- c(
  Cash = "cash",
  SPY  = "UPRO",
  VGK  = "EURL",
  EWJ  = "EZJ",   # you noted EZJ (2x) in comments; keep as placeholder
  EEM  = "EDC",
  ICF  = "DRN",
  IEF  = "TYD",
  TLT  = "TMF",
  GLD  = "SHNY"
  # DBC = "???"
)

# -----------------------
# HELPERS (from your style)
# -----------------------
strat_summary <- function(returns, original_results = NULL) {
  stats <- table.AnnualizedReturns(returns)
  stats <- rbind(stats, "Worst Drawdown" = -maxDrawdown(returns))
  if (!is.null(original_results)) {
    stats <- cbind(original_results, stats)
    colnames(stats)[1] <- "Original"
  }
  round(stats, 3)
}

# -----------------------
# ONLINE LEARNING CORE
# -----------------------

# Convert per-period returns to losses in [0,1] by rank.
# Best return -> loss ~ 0. Worst -> loss ~ 1.
returns_to_losses_rank <- function(r_vec) {
  ok <- is.finite(r_vec)
  losses <- rep(NA_real_, length(r_vec))
  if (sum(ok) <= 1) {
    # Not enough info; return neutral losses
    losses[ok] <- 0.5
    return(losses)
  }
  ranks <- rank(-r_vec[ok], ties.method = "average")  # higher return = smaller rank
  losses[ok] <- (ranks - 1) / (length(ranks) - 1)
  losses
}

# Multiplicative weights update with optional forgetting/regularization to initial weights w0
mw_update <- function(w, losses, eta, lambda = 0, w0 = NULL) {
  # eta=0 -> no learning (static)
  if (eta <= 0) {
    if (!is.null(w0)) return(w0 / sum(w0))
    return(w / sum(w))
  }
  
  ok <- is.finite(losses)
  if (!any(ok)) return(w / sum(w))
  
  # For NA losses, treat as neutral (doesn't move weights much)
  losses2 <- losses
  losses2[!ok] <- mean(losses2[ok])
  
  w_new <- w * exp(-eta * losses2)
  # numerical guard
  w_new[!is.finite(w_new)] <- 0
  if (sum(w_new) <= 0) w_new <- rep(1/length(w_new), length(w_new))
  w_new <- w_new / sum(w_new)
  
  if (!is.null(w0) && lambda > 0) {
    w0 <- w0 / sum(w0)
    w_new <- (1 - lambda) * w_new + lambda * w0
    w_new <- w_new / sum(w_new)
  }
  
  w_new
}

# AdaNormalHedge supervisor across "experts" (here: Layer-3 algos)
# adanormalhedge_step <- function(R, C) {
#   # Implements the weight formula used in the paper’s Algorithm 2 (Luo & Schapire)
#   # R: signed regret vector
#   # C: cumulative absolute regret vector
#   # returns: p (prob distribution over experts), updated weights helper
#   
#   w <- 0.5 * (exp(((R + 1)^2) / (3 * (C + 1))) - exp(((R - 1)^2) / (3 * (C + 1))))
#   w[!is.finite(w)] <- 0
#   if (sum(w) <= 0) {
#     p <- rep(1/length(w), length(w))
#   } else {
#     p <- w / sum(w)
#   }
#   list(p = p, w = w)
# }

adanormalhedge_step <- function(R, C) {
  # Raw AdaNormalHedge weights (can be negative if R < 0)
  w_raw <- 0.5 * (exp(((R + 1)^2) / (3 * (C + 1))) - exp(((R - 1)^2) / (3 * (C + 1))))
  
  # IMPORTANT: enforce valid mixture weights
  w <- pmax(w_raw, 0)          # clip negatives
  w[!is.finite(w)] <- 0
  
  if (sum(w) <= 0) {
    p <- rep(1/length(w), length(w))
  } else {
    p <- w / sum(w)
  }
  
  list(p = p, w = w)
}

six_month_momentum <- function(R_assets, i_now, lookback = 126) {
  # returns vector of trailing compounded returns over lookback window ending at i_now
  start_i <- max(1, i_now - lookback + 1)
  window  <- R_assets[start_i:i_now, , drop = FALSE]
  apply(1 + window, 2, prod, na.rm = TRUE) - 1
}


# -----------------------
# STRATEGY ENGINE
# -----------------------

# Compute rebalance endpoints
get_rebalance_idx <- function(R, freq = c("weeks","months"), k = 1) {
  freq <- match.arg(freq)
  ep <- endpoints(R, on = freq)
  ep <- ep[ep > 0]
  if (length(ep) <= 1) return(integer())
  # take every kth endpoint (skip 0 already removed)
  ep <- ep[seq(1, length(ep), by = k)]
  ep
}

# Main: generate weights + returns using Layer3 ensemble + Layer4 supervisor
run_adaptive_allocation <- function(R_assets,
                                    freq = "weeks",
                                    k = 1,
                                    max_cap = 1.0,
                                    smooth = 0.0,
                                    verbose = FALSE) {
  
  stopifnot(is.xts(R_assets))
  stopifnot(ncol(R_assets) >= 2)
  
  # Layer-3 ensemble params (tune-free-ish; supervisor will adapt)
  # eta: learning rate; lambda: regularization to initial weights (forgetting)
  algo_params <- list(
    fast   = list(eta = 2.0, lambda = 0.00),
    mid    = list(eta = 0.7, lambda = 0.01),
    slow   = list(eta = 0.2, lambda = 0.01),
    defend = list(eta = 0.1, lambda = 0.05),
    static = list(eta = 0.0, lambda = 0.00)  # stays at w0
  )
  algos <- names(algo_params)
  
  nA <- ncol(R_assets)
  w0_assets <- rep(1/nA, nA)
  names(w0_assets) <- colnames(R_assets)
  
  # Layer-3 state: each algo has its own asset weight vector
  W_assets <- lapply(algos, function(a) w0_assets)
  names(W_assets) <- algos
  
  # Layer-4 state: regrets across algos
  R_reg <- rep(0, length(algos))  # signed regret
  C_reg <- rep(0, length(algos))  # abs regret
  names(R_reg) <- algos
  names(C_reg) <- algos
  
  # Outputs at rebalance times
  rb_idx <- get_rebalance_idx(R_assets, freq = freq, k = k)
  if (length(rb_idx) < 2) stop("Not enough data to rebalance with chosen frequency.")
  
  weights_out <- R_assets * NA_real_
  algo_p_out  <- xts(matrix(NA_real_, nrow = nrow(R_assets), ncol = length(algos)),
                     order.by = index(R_assets))
  colnames(algo_p_out) <- paste0("p_", algos)
  
  # We update using the realized returns from the *previous* rebalance window,
  # then apply the new weights starting next period (we will lag later).
  for (j in 2:length(rb_idx)) {
    i_prev <- rb_idx[j-1]
    i_now  <- rb_idx[j]
    
    window <- R_assets[(i_prev+1):i_now, , drop = FALSE]
    if (nrow(window) < 1) next
    
    # Period return per asset (compounded over the window)
    r_period <- apply(1 + window, 2, prod, na.rm = TRUE) - 1
    
    # Update each Layer-3 algo weights from asset losses
    losses_assets <- returns_to_losses_rank(r_period)
    
    for (a in algos) {
      p <- algo_params[[a]]
      W_assets[[a]] <- mw_update(W_assets[[a]], losses_assets, p$eta, p$lambda, w0_assets)
      
      # optional cap
      if (max_cap < 1.0) {
        W_assets[[a]] <- pmin(W_assets[[a]], max_cap)
        W_assets[[a]] <- W_assets[[a]] / sum(W_assets[[a]])
      }
    }
    
    # Compute each algo’s realized return over the same window using *its weights from start of window*.
    # To avoid needing historical weights at i_prev, approximate by using updated weights as proxy is bad.
    # Better: evaluate using weights BEFORE update. So we keep a copy:
    # We'll do this cleanly by re-deriving: use weights from previous iteration stored in prev_W_assets.
    # For j==2, prev weights are w0_assets.
    if (j == 2) {
      prev_W_assets <- lapply(algos, function(a) w0_assets)
      names(prev_W_assets) <- algos
    }
    
    algo_returns <- sapply(algos, function(a) sum(prev_W_assets[[a]] * r_period, na.rm = TRUE))
    
    # Supervisor chooses p over algos based on regret update
    sup <- adanormalhedge_step(R_reg, C_reg)
    p_algos <- sup$p
    
    p_algos <- pmax(p_algos, 0)
    p_algos <- p_algos / sum(p_algos)
    stopifnot(all(p_algos >= -1e-12), abs(sum(p_algos) - 1) < 1e-8)
    
    # Learner return is p-weighted algo return
    learner_return <- sum(p_algos * algo_returns)
    
    # Losses at algo-level (rank based on algo return)
    losses_algos <- returns_to_losses_rank(algo_returns)
    
    # Update regrets per AdaNormalHedge: r = loss_learner - loss_expert
    loss_learner <- sum(p_algos * losses_algos, na.rm = TRUE)
    
    for (a_i in seq_along(algos)) {
      a <- algos[a_i]
      r <- loss_learner - losses_algos[a_i]
      R_reg[a] <- R_reg[a] + r
      C_reg[a] <- C_reg[a] + abs(r)
    }
    
    # Combine asset weights across algos using supervisor p
    w_combo <- Reduce(`+`, lapply(seq_along(algos), function(a_i) {
      a <- algos[a_i]
      p_algos[a_i] * W_assets[[a]]
    }))
    w_combo <- w_combo / sum(w_combo)
    
    w_combo <- pmax(w_combo, 0)
    w_combo <- w_combo / sum(w_combo)
    
    # -----------------------------
    # Post-filter: 6-month momentum gate + cash fallback
    # -----------------------------
    if (use_momo_filter) {
      
      mom <- six_month_momentum(R_assets, i_now, lookback = momo_lookback_days)
      
      # Define risky assets: everything except Cash (or cash-like column)
      is_cash <- names(w_combo) %in% c("Cash", "cash")
      risky_names <- names(w_combo)[!is_cash]
      
      # Assets that pass the momentum filter
      pass <- risky_names[is.finite(mom[risky_names]) & mom[risky_names] >= 0]
      
      # Zero out the ones that fail
      w_combo[risky_names[!risky_names %in% pass]] <- 0
      
      n_pass <- length(pass)
      
      if (n_pass == 0) {
        # Nobody passes -> 100% cash
        w_combo[] <- 0
        w_combo[names(w_combo)[is_cash][1]] <- 1
        
      } else if (n_pass < min_assets_required) {
        # Fewer than 3 pass -> 50% cash, 50% split across survivors
        cash_name <- names(w_combo)[is_cash][1]
        
        # Preserve relative weights among survivors if possible, else equal weight them
        w_surv <- w_combo[pass]
        if (sum(w_surv) <= 0) w_surv <- rep(1 / n_pass, n_pass)
        
        w_surv <- w_surv / sum(w_surv)         # normalize survivors to 1
        w_combo[] <- 0
        w_combo[pass] <- (1 - cash_fallback_weight) * w_surv
        w_combo[cash_name] <- cash_fallback_weight
        
      } else {
        # 3+ pass -> renormalize across cash + survivors (cash may remain >0 if engine assigned it)
        total <- sum(w_combo)
        if (total <= 0) {
          w_combo[] <- 0
          w_combo[names(w_combo)[is_cash][1]] <- 1
        } else {
          w_combo <- w_combo / total
        }
      }
    }
    
    
    
    
    # Optional smoothing to reduce turnover: w_t = (1-s)*w_new + s*w_old
    # Use last published weight as w_old if available, else w0
    if (smooth > 0) {
      w_old <- as.numeric(weights_out[i_prev, ])
      if (any(is.finite(w_old))) {
        w_old[!is.finite(w_old)] <- w0_assets[!is.finite(w_old)]
        w_old <- w_old / sum(w_old)
      } else {
        w_old <- w0_assets
      }
      w_combo <- (1 - smooth) * w_combo + smooth * w_old
      w_combo <- w_combo / sum(w_combo)
    }
    
    # Store weights on rebalance date
    weights_out[i_now, ] <- w_combo
    algo_p_out[i_now, ]  <- p_algos
    
    if (verbose) {
      cat("Rebalance:", as.character(index(R_assets)[i_now]),
          "LearnerRet:", round(learner_return, 4), "\n")
    }
    
    # Save current weights as "prev" for next window evaluation
    prev_W_assets <- W_assets
  }
  
  # Lag weights so they apply AFTER decision date, LOCF
  weights_out <- lag(weights_out, k = 1)
  weights_out <- na.locf(weights_out, na.rm = FALSE)
  weights_out[is.na(weights_out)] <- 0
  weights_out <- weights_out / rowSums(weights_out)
  
  # Portfolio daily returns
  Rp <- xts(rowSums(R_assets * weights_out), order.by = index(R_assets))
  colnames(Rp) <- "Adaptive_MW_Portfolio"
  
  list(returns = Rp, weights = weights_out, algo_p = algo_p_out)
}

# -----------------------
# LOAD DATA (same overall shape as your josh_strat.R)
# -----------------------
data(aaa_returns, package = "ftblog")

assets <- lapply(etfs, FUN = function(x) {
  if (verbose) cat("Loading:", x, "\n")
  df <- getSymbols(x, auto.assign = FALSE)
  df <- df[, 4]
  names(df) <- gsub(".Close", "", names(df))
  Return.calculate(df)
})
assets <- do.call("cbind", assets)

asset_names <- etfs

if (use_cash) {
  returns <- aaa_returns
  assets$Cash <- 0
  assets <- assets[, c("Cash", asset_names)]
  asset_names <- c("Cash", asset_names)
}

# Align / append like your original script
assets  <- assets[(which(index(assets) == "2023-12-29") + 1):nrow(assets), ]
returns <- returns[,names(returns)[!names(returns) %in% c("International.Real.Estate", "Commodities")]]
names(returns) <- asset_names

returns <- rbind(returns, assets)
r_full <- returns

# Keep same reduced set you ended up using
# (You can edit this list; script adapts automatically.)
keep_cols <- c("Cash", "SPY", "VGK", "EEM", "ICF", "IEF", "TLT", "GLD")
r_full <- r_full[, keep_cols]
r_full$Cash <- 1e-12  # tiny positive to avoid divide-by-zero issues

# Drop rows with all NAs
r_full <- r_full[apply(is.finite(r_full), 1, any), ]

# -----------------------
# RUN STRATEGY
# -----------------------
res <- run_adaptive_allocation(
  R_assets = r_full,
  freq     = rebalance_freq,
  k        = rebalance_k,
  max_cap  = max_weight_cap,
  smooth   = weight_smoothing,
  verbose  = verbose
)

strat_returns <- res$returns
strat_wts     <- res$weights

# Apply leverage like your original strat_3x
strat_returns_lev <- strat_returns * portfolio_leverage

# Quick checks
charts.PerformanceSummary(strat_returns_lev)
charts.PerformanceSummary(merge(strat_returns_lev, r_full$SPY))

output <- merge(strat_returns_lev, r_full$SPY)
output <- output[complete.cases(output), ]
names(output) <- c("Adaptive MW (Levered)", "SPY")
charts.PerformanceSummary(output)

Return.annualized(output)
SharpeRatio.annualized(output)
print(strat_summary(output))

# -----------------------
# EXPORT (compatible with your aggregation pipeline)
# -----------------------
returns_xts <- strat_returns_lev
returns_xts <- returns_xts[complete.cases(returns_xts), ]
colnames(returns_xts) <- "Adaptive_MW"

# Map weights to leveraged tickers for export
w_export <- strat_wts
colnames(w_export) <- ifelse(colnames(w_export) %in% names(leveraged_map),
                             leveraged_map[colnames(w_export)],
                             colnames(w_export))

# Standardize cash label to "cash" (as you used)
if ("Cash" %in% colnames(w_export)) colnames(w_export)[colnames(w_export) == "Cash"] <- "cash"

# Ensure same column order each run (optional)
w_export <- w_export[, sort(colnames(w_export))]

# export_strategy_output(
#   strategy_name = "Adaptive_MW",
#   returns_xts   = returns_xts,
#   weights_xts   = w_export,
#   output_dir    = "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
# )
