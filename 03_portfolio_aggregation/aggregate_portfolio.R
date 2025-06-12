rm(list=ls())
library(xts)
library(PerformanceAnalytics)
library(nloptr)
library(quadprog)

# === CONFIG ===
input_dir <- "/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
strategy_files <- list.files(input_dir, pattern = "_output\\.rds$", full.names = TRUE)
window_length <- 60  # lookback window for weighting schemes

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

# === FUNCTION: Equal Risk Contribution ===
calc_erc_weights_qp <- function(R, tol = 1e-6, max_iter = 500) {
  covmat <- cov(R, use = "pairwise.complete.obs")
  n <- ncol(covmat)
  w <- rep(1 / n, n)
  
  for (i in 1:max_iter) {
    port_var <- as.numeric(t(w) %*% covmat %*% w)
    mrc <- (covmat %*% w)
    rc <- w * mrc
    target_rc <- mean(rc)
    
    grad <- mrc - target_rc / w
    grad[is.infinite(grad)] <- 0
    
    Dmat <- covmat
    dvec <- rep(0, n)
    Amat <- cbind(rep(1, n), diag(n))
    bvec <- c(1, rep(0, n))
    
    sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    w_new <- sol$solution
    w_new <- w_new / sum(w_new)
    
    if (sqrt(sum((w_new - w)^2)) < tol) break
    w <- w_new
  }
  
  w
}

# === FUNCTION: Multiplicative Updates ===
calc_multiplicative_weights <- function(R, min_weight = 0.02) {
  cum_returns <- apply(1 + R, 2, prod, na.rm = TRUE) - 1
  
  # Replace negatives with small floor, NOT zero
  adj_returns <- pmax(cum_returns, min_weight)
  
  # Normalize
  w <- adj_returns / sum(adj_returns)
  w
}

# === STRATEGY-LEVEL WEIGHTS ===
R_window <- tail(aligned_returns, window_length)

erc_weights <- calc_erc_weights_qp(R_window)
mu_weights  <- calc_multiplicative_weights(R_window)
blend_weights <- 0.5 * erc_weights + 0.5 * mu_weights

names(erc_weights) <- colnames(aligned_returns)
names(mu_weights)  <- colnames(aligned_returns)
names(blend_weights) <- colnames(aligned_returns)

cat("\n=== Strategy Weights ===\n")
print(round(erc_weights, 4))
print(round(mu_weights, 4))
print(round(blend_weights, 4))


strategy_weights <- erc_weights
name <- "strat"

# === AGGREGATE HOLDINGS ===
aggregate_holdings <- function(strategy_weights) {
  holdings_list <- lapply(names(strategy_weights), function(name) {
    w <- strategy_weights[name]
    lw_xts <- strategy_list[[name]]$latest_weights
    lw <- as.numeric(lw_xts[1, ])
    tickers <- colnames(lw_xts)
    data.frame(ticker = tickers, weighted_weight = lw * w, stringsAsFactors = FALSE)
  })
  
  combined <- do.call(rbind, holdings_list)
  agg <- aggregate(weighted_weight ~ ticker, data = combined, sum)
  agg$weight <- agg$weighted_weight / sum(agg$weighted_weight)
  agg[, c("ticker", "weight")]
}

erc_holdings   <- aggregate_holdings(erc_weights)
mu_holdings    <- aggregate_holdings(mu_weights)
blend_holdings <- aggregate_holdings(blend_weights)

cat("\n=== Aggregated Holdings: ERC ===\n")
print(erc_holdings)
cat("\n=== Aggregated Holdings: Multiplicative Updates ===\n")
print(mu_holdings)
cat("\n=== Aggregated Holdings: 50/50 Blend ===\n")
print(blend_holdings)

# === STRATEGY-LEVEL RETURNS → PORTFOLIO RETURNS ===
weighted_portfolio_returns <- function(R, strategy_weights) {
  xts::xts(R %*% strategy_weights, order.by = index(R))
}

erc_port <- weighted_portfolio_returns(aligned_returns, erc_weights)
mu_port  <- weighted_portfolio_returns(aligned_returns, mu_weights)
blend_port <- weighted_portfolio_returns(aligned_returns, blend_weights)

portfolios <- merge(erc_port, mu_port, blend_port, aligned_returns)
colnames(portfolios) <- c("ERC", "Multiplicative", "Blend", names(aligned_returns))

# === PERFORMANCE METRICS ===
cat("\n=== Performance Summary ===\n")
perf_table <- rbind(
  Return.annualized(portfolios),
  SharpeRatio.annualized(portfolios)
)
rownames(perf_table) <- c("Annual Return", "Annual Sharpe")
print(round(perf_table, 3))

# === PLOT CUMULATIVE PERFORMANCE ===
charts.PerformanceSummary(portfolios, legend.loc = "topleft", main = "Portfolio Strategy Comparison")
charts.PerformanceSummary(portfolios$Blend, legend.loc = "topleft", main = "Portfolio Strategy Comparison")
