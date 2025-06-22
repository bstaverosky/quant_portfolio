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

# === PREPARE STRATEGY-LEVEL FUNCTIONS ===
calc_erc_weights_qp <- function(R, tol = 1e-6, max_iter = 500) {
  covmat <- cov(R, use = "pairwise.complete.obs")
  n <- ncol(covmat)
  w <- rep(1 / n, n)
  for (i in 1:max_iter) {
    mrc <- covmat %*% w
    rc <- w * mrc
    target_rc <- mean(rc)
    
    Dmat <- covmat
    dvec <- rep(0, n)
    Amat <- cbind(rep(1, n), diag(n))
    bvec <- c(1, rep(0, n))
    sol <- try(solve.QP(Dmat, dvec, Amat, bvec, meq = 1), silent = TRUE)
    if (inherits(sol, "try-error")) return(rep(1/n, n))
    w_new <- sol$solution / sum(sol$solution)
    if (sqrt(sum((w_new - w)^2)) < tol) break
    w <- w_new
  }
  w
}

calc_multiplicative_weights <- function(R, min_weight = 0.02) {
  cum_returns <- apply(1 + R, 2, prod, na.rm = TRUE) - 1
  adj_returns <- pmax(cum_returns, min_weight)
  adj_returns / sum(adj_returns)
}

# === SETUP WALK-FORWARD ===
start_index <- window_length + 1
n_periods   <- nrow(aligned_returns)
dates       <- index(aligned_returns)[start_index:(n_periods-1)]

# Strategy-level return containers
erc_returns   <- mu_returns   <- blend_returns   <- aligned_returns[0,1,drop=FALSE]

# Strategy-level weight containers
strategy_names     <- colnames(aligned_returns)
erc_weights_xts   <- xts(matrix(NA, nrow=length(dates), ncol=length(strategy_names)), order.by=dates)
mu_weights_xts    <- erc_weights_xts
blend_weights_xts <- erc_weights_xts
colnames(erc_weights_xts)   <- strategy_names
colnames(mu_weights_xts)    <- strategy_names
colnames(blend_weights_xts) <- strategy_names

# ETF-level ticker universe
eft_universe <- unique(unlist(lapply(strategy_list, function(s) colnames(s$weights))))
erc_etf_xts   <- xts(matrix(0, nrow=length(dates), ncol=length(eft_universe)), order.by=dates)
mu_etf_xts    <- erc_etf_xts
blend_etf_xts <- erc_etf_xts
colnames(erc_etf_xts)   <- eft_universe
colnames(mu_etf_xts)    <- eft_universe
colnames(blend_etf_xts) <- eft_universe

# === WALK-FORWARD LOOP ===
for (i in seq(start_index, n_periods-1)) {
  print(i)
  date_index <- index(aligned_returns)[i]
  R_window <- aligned_returns[(i-window_length):(i-1), ]
  R_next   <- aligned_returns[i, , drop=FALSE]
  
  # Strategy-level weights
  w_erc   <- calc_erc_weights_qp(R_window)
  w_mu    <- calc_multiplicative_weights(R_window)
  w_blend <- 0.5 * w_erc + 0.5 * w_mu
  
  erc_weights_xts[i-start_index+1, ]   <- w_erc
  mu_weights_xts[i-start_index+1, ]    <- w_mu
  blend_weights_xts[i-start_index+1, ] <- w_blend
  
  # Strategy returns
  erc_returns   <- rbind(erc_returns,   xts(as.numeric(R_next %*% w_erc),   order.by=index(R_next)))
  mu_returns    <- rbind(mu_returns,    xts(as.numeric(R_next %*% w_mu),    order.by=index(R_next)))
  blend_returns <- rbind(blend_returns, xts(as.numeric(R_next %*% w_blend), order.by=index(R_next)))
  
  # ETF-level aggregation
  # Fetch asset-level weights for each strategy as of date_index
  asset_w_list <- lapply(strategy_list, function(s) {
    wt <- s$weights
    # latest row <= date_index
    dates_avail <- index(wt)[index(wt) <= date_index]
    if (length(dates_avail)==0) return(setNames(rep(0, ncol(wt)), colnames(wt)))
    #as.numeric(wt[dates_avail[length(dates_avail)], ])
    wt[dates_avail[length(dates_avail)], ]
  })
  names(asset_w_list) <- strategy_names
  
  # Helper to build ETF weights
  build_etf <- function(strat_w) {
    etf_w <- setNames(rep(0, length(eft_universe)), eft_universe)
    for (j in seq_along(strat_w)) {
      print(j)
      name_j <- strategy_names[j]
      wj_strat <- strat_w[j]
      aw <- asset_w_list[[name_j]]
      tickers <- names(aw)
      for (k in seq_along(aw)) {
        etf_w[tickers[k]] <- etf_w[tickers[k]] + aw[,k] * wj_strat
      }
    }
    etf_w / sum(etf_w)
  }
  
  erf <- build_etf(w_erc)
  muf <- build_etf(w_mu)
  blf <- build_etf(w_blend)
  
  erc_etf_xts[i-start_index+1, ]   <- erf
  mu_etf_xts[i-start_index+1, ]    <- muf
  blend_etf_xts[i-start_index+1, ] <- blf
}

# === PERFORMANCE ===
portfolios <- merge(
  xts(erc_returns,   order.by=index(erc_returns)),
  xts(mu_returns,    order.by=index(mu_returns)),
  xts(blend_returns, order.by=index(blend_returns))
)
colnames(portfolios) <- c("ERC","Multiplicative","Blend")

cat("\n=== Walk-Forward Performance Summary ===\n")
perf_table <- rbind(
  Return.annualized(portfolios),
  SharpeRatio.annualized(portfolios)
)
rownames(perf_table) <- c("Annual Return","Annual Sharpe")
print(round(perf_table,3))

# === PLOTS ===
charts.PerformanceSummary(portfolios, legend.loc="topleft", main="Walk-Forward Strategy Comparison")

# === HISTORICAL WEIGHTS OUTPUT ===
# Strategy-level:
print(tail(erc_weights_xts))
print(tail(mu_weights_xts))
print(tail(blend_weights_xts))
# ETF-level:
print(tail(erc_etf_xts))
print(tail(mu_etf_xts))
print(tail(blend_etf_xts))
