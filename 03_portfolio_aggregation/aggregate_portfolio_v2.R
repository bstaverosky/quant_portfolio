rm(list=ls())
library(xts)
library(PerformanceAnalytics)
library(quadprog)

# === CONFIG ===
input_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
strategy_files <- list.files(input_dir, pattern = "_output\\.rds$", full.names = TRUE)
window_length <- 504  # lookback window for weighting schemes

# === LOAD STRATEGY DATA ===
strategy_list <- list()
for (f in strategy_files) {
  name <- gsub("_output\\.rds$", "", basename(f))
  out <- readRDS(f)
  strategy_list[[name]] <- out
}

# === ALIGN RETURNS ===
return_list <- lapply(strategy_list, function(x) x$returns)
common_dates <- as.Date(Reduce(intersect, lapply(return_list, index)))
aligned_returns <- do.call(merge, lapply(return_list, function(x) x[common_dates]))

# === ERC via quadprog ===
calc_erc_weights_qp <- function(R, tol = 1e-6, max_iter = 500) {
  covmat <- cov(R, use = "pairwise.complete.obs")
  n <- ncol(covmat)
  w <- rep(1 / n, n)  # initial equal weight
  
  for (i in 1:max_iter) {
    port_var <- as.numeric(t(w) %*% covmat %*% w)
    mrc <- covmat %*% w
    rc <- w * mrc
    target_rc <- mean(rc)
    
    grad <- mrc - target_rc / w
    grad[is.infinite(grad)] <- 0
    
    Dmat <- covmat
    dvec <- rep(0, n)
    Amat <- cbind(rep(1, n), diag(n))
    bvec <- c(1, rep(0, n))
    
    # Safe fallback if matrix isn't positive definite
    sol <- try(solve.QP(Dmat, dvec, Amat, bvec, meq = 1), silent = TRUE)
    
    if (inherits(sol, "try-error")) {
      message("solve.QP failed, falling back to equal weights")
      return(rep(1 / n, n))
    }
    
    w_new <- sol$solution
    w_new <- w_new / sum(w_new)
    
    if (sqrt(sum((w_new - w)^2)) < tol) break
    w <- w_new
  }
  
  w
}

# === Multiplicative Update ===
calc_multiplicative_weights <- function(R, min_weight = 0.02) {
  cum_returns <- apply(1 + R, 2, prod, na.rm = TRUE) - 1
  adj_returns <- pmax(cum_returns, min_weight)
  w <- adj_returns / sum(adj_returns)
  w
}





# === Walk-Forward Backtest ===
start_index <- window_length + 1
n_periods <- nrow(aligned_returns)

erc_returns <- mu_returns <- blend_returns <- aligned_returns[0, 1, drop = FALSE]


# Prepare empty xts objects to store weights
dates <- index(aligned_returns)[start_index:(n_periods - 1)]
strategy_names <- colnames(aligned_returns)

erc_weights_xts   <- xts(matrix(NA, nrow = length(dates), ncol = length(strategy_names)), order.by = dates)
mu_weights_xts    <- erc_weights_xts
blend_weights_xts <- erc_weights_xts
colnames(erc_weights_xts) <- strategy_names
colnames(mu_weights_xts) <- strategy_names
colnames(blend_weights_xts) <- strategy_names




for (i in start_index:(n_periods - 1)) {
  print(i)
  R_window <- aligned_returns[(i - window_length):(i - 1), ]
  R_next <- aligned_returns[i, , drop = FALSE]  # next day's strategy returns
  
  w_erc   <- calc_erc_weights_qp(R_window)
  w_mu    <- calc_multiplicative_weights(R_window)
  w_blend <- 0.5 * w_erc + 0.5 * w_mu
  
  # Store the weights
  erc_weights_xts[i - window_length, ]   <- w_erc
  mu_weights_xts[i - window_length, ]    <- w_mu
  blend_weights_xts[i - window_length, ] <- w_blend
  
  erc_returns <- rbind(erc_returns, xts(R_next %*% w_erc, order.by = index(R_next)))
  mu_returns  <- rbind(mu_returns,  xts(R_next %*% w_mu,  order.by = index(R_next)))
  blend_returns <- rbind(blend_returns, xts(R_next %*% w_blend, order.by = index(R_next)))
}

# Combine for performance evaluation
portfolios <- merge(erc_returns, mu_returns, blend_returns)
colnames(portfolios) <- c("ERC", "Multiplicative", "Blend")

# === PERFORMANCE METRICS ===
cat("\n=== Walk-Forward Performance Summary ===\n")
perf_table <- rbind(
  Return.annualized(portfolios),
  SharpeRatio.annualized(portfolios)
)
rownames(perf_table) <- c("Annual Return", "Annual Sharpe")
print(round(perf_table, 3))

# === PLOT ===
charts.PerformanceSummary(portfolios, legend.loc = "topleft", main = "Walk-Forward Strategy Comparison")

head(erc_weights_xts)
head(mu_weights_xts)
head(blend_weights_xts)
