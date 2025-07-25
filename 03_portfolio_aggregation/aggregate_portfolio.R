rm(list=ls())
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(quadprog)

# getSymbols("QQQ",
#            src    = "yahoo",
#            method = "libcurl",
#            timeout        = 60,   # total time (secs)
#            connecttimeout = 30,   # DNS+TCP handshake
#            auto.assign   = FALSE)
# 
# 
# 
# getSymbols("QQQ", 
#            from = as.Date("2025-05-29"), 
#            to = as.Date("2025-05-30"),
#            auto.assign = FALSE, 
#            warnings = FALSE, 
#            method = "libcurl", 
#            src = "yahoo",
#            timeout = 60,
#            connecttimeout=30)

# === CONFIG ===
options(timeout = 300)
input_dir <- "03_portfolio_aggregation/strategy_outputs"
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
calc_erc_weights_qp         <- function(R, tol = 1e-6, max_iter = 500) {
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

# generate_trade_list <- function(target_w,
#                                 current_shares = NULL,
#                                 cash_inflow = 0,
#                                 date = Sys.Date()) {
#   require(quantmod)
#   
#   # 1) Ensure all tickers are present
#   all_tickers <- names(target_w)
#   cur_sh      <- setNames(rep(0, length(all_tickers)), all_tickers)
#   if (!is.null(current_shares)) {
#     cur_sh[names(current_shares)] <- current_shares
#   }
#   
#   # 2) Fetch or set prices
#   prices <- sapply(all_tickers, function(tk) {
#     if (tolower(tk) == "cash") {
#       return(1)  # $1 per "share" of cash
#     }
#     # otherwise pull close price for that date
#     data <- try(getSymbols(tk, 
#                            from = date, 
#                            to = date,
#                            auto.assign = FALSE, 
#                            warnings = FALSE, 
#                            method = "libcurl", 
#                            src = "yahoo",
#                            timeout = 60,
#                            connecttimeout=30), silent = TRUE)
#     if (inherits(data, "try-error") || nrow(data) == 0)
#       stop("Price fetch failed for ", tk)
#     as.numeric(Cl(data)[as.character(date), 1])
#   })
#   
#   # 3) Compute current market values and total capital
#   mkt_val    <- cur_sh * prices
#   total_val  <- sum(mkt_val, na.rm = TRUE) + cash_inflow
#   
#   # 4) Compute target dollar allocations
#   target_val    <- total_val * target_w
#   
#   # 5) Compute target shares
#   target_shares <- sapply(all_tickers, function(tk) {
#     if (tolower(tk) == "cash") {
#       return(target_val[tk])      # for cash, #shares = $ amount
#     }
#     # for ETFs: integer shares
#     floor(target_val[tk] / prices[tk])
#   })
#   
#   # 6) Compute trades
#   trade_shares <- target_shares - cur_sh
#   
#   # 7) Assemble trade list
#   trades <- data.frame(
#     ticker         = all_tickers,
#     price          = as.numeric(prices),
#     current_shares = as.numeric(cur_sh),
#     target_shares  = as.numeric(target_shares),
#     trade_shares   = as.numeric(trade_shares),
#     stringsAsFactors = FALSE
#   )
#   
#   # drop zeros if desired
#   trades <- trades[trades$trade_shares != 0, ]
#   rownames(trades) <- NULL
#   trades
# }

#target_w <- tw
#current_shares <- current
#cash_inflow <- 1300
#date <- Sys.Date()

generate_trade_list <- function(target_w,
                                current_shares = NULL,
                                cash_inflow = 1300,
                                date = Sys.Date()) {
  require(quantmod)
  
  # Make sure date is Date
  date <- as.Date(date)-1
  prev_day <- date - 1
  
  # 1) Build full ticker list & initialize current shares
  all_tickers <- names(target_w)
  cur_sh      <- setNames(rep(0, length(all_tickers)), all_tickers)
  if (!is.null(current_shares)) {
    cur_sh[names(current_shares)] <- current_shares
  }
  
  # 2) Fetch or set prices
  prices <- sapply(all_tickers, function(tk) {
    print(tk)
    if (tolower(tk) == "cash") {
      return(1)  # $1 per "share" of cash
    }
    # pull data from 'date' through 'date+1'
    data_xts <- try(
      #getSymbols(tk, from = prev_day, to = date,
      #           auto.assign = FALSE, warnings = FALSE)
      getSymbols(tk,
                 from = as.Date(prev_day),
                 to = as.Date(date),
                 auto.assign = FALSE,
                 warnings = FALSE,
                 method = "libcurl",
                 src = "yahoo",
                 timeout = 60,
                 connecttimeout=30),
      silent = TRUE
    )
    if (inherits(data_xts, "try-error") || nrow(data_xts) == 0) {
      stop("Price fetch failed for ", tk, " on ", date)
    }
    # extract the row exactly matching 'date'
    #price <- as.numeric(Cl(data_xts)[as.character(date), 1])
    price <- as.numeric(data_xts[,4])
    
    if (is.na(price)) {
      stop("No price for ", tk, " on ", date)
    }
    price
  })
  
  # 3) Compute current market values and total capital
  mkt_val   <- cur_sh * prices
  total_val <- sum(mkt_val, na.rm = TRUE) + cash_inflow
  
  # 4) Compute target dollar allocations
  target_val <- total_val * target_w
  
  # 5) Compute target shares
  target_shares <- sapply(all_tickers, function(tk) {
    if (tolower(tk) == "cash") {
      return(target_val[tk])  # dollars of cash
    }
    floor(target_val[tk] / prices[tk])
  })
  
  # 6) Compute trades
  trade_shares <- target_shares - cur_sh
  
  # 7) Assemble and return
  trades <- data.frame(
    ticker         = all_tickers,
    price          = prices,
    current_shares = as.numeric(cur_sh),
    target_shares  = as.numeric(target_shares),
    trade_shares   = as.numeric(trade_shares),
    stringsAsFactors = FALSE
  )
  # drop zero-trades
  trades[trades$trade_shares != 0, , drop = FALSE]
}


# === SETUP WALK-FORWARD ===
start_index <- window_length + 1
n_periods   <- nrow(aligned_returns)
dates       <- index(aligned_returns)[start_index:(n_periods-1)]

# Strategy-level return containers
erc_returns   <- mu_returns   <- blend_returns   <- aligned_returns[0,1,drop=FALSE]

# Strategy-level weight containers
strategy_names     <- colnames(aligned_returns)
erc_weights_xts   <- xts(matrix(NA, nrow = length(dates) + 1, ncol = length(strategy_names)), order.by = c(dates, index(aligned_returns)[n_periods]))
mu_weights_xts    <- erc_weights_xts
blend_weights_xts <- erc_weights_xts
colnames(erc_weights_xts)   <- strategy_names
colnames(mu_weights_xts)    <- strategy_names
colnames(blend_weights_xts) <- strategy_names

# ETF-level ticker universe
eft_universe <- unique(unlist(lapply(strategy_list, function(s) colnames(s$weights))))
erc_etf_xts   <- xts(matrix(0, nrow = length(dates) + 1, ncol = length(eft_universe)), order.by = c(dates, index(aligned_returns)[n_periods]))
mu_etf_xts    <- erc_etf_xts
blend_etf_xts <- erc_etf_xts
colnames(erc_etf_xts)   <- eft_universe
colnames(mu_etf_xts)    <- eft_universe
colnames(blend_etf_xts) <- eft_universe

# === WALK-FORWARD LOOP ===
for (i in seq(start_index, n_periods)) {
  print(i)
  date_index <- index(aligned_returns)[i]
  R_window <- aligned_returns[(i - window_length):(i - 1), ]
  
  # Strategy-level weights
  w_erc   <- calc_erc_weights_qp(R_window)
  w_mu    <- calc_multiplicative_weights(R_window)
  w_blend <- 0.5 * w_erc + 0.5 * w_mu
  
  erc_weights_xts[i - start_index + 1, ]   <- w_erc
  mu_weights_xts[i - start_index + 1, ]    <- w_mu
  blend_weights_xts[i - start_index + 1, ] <- w_blend
  
  # Skip return computation only
  if (i < n_periods) {
    R_next <- aligned_returns[i, , drop = FALSE]
    erc_returns   <- rbind(erc_returns,   xts(as.numeric(R_next %*% w_erc),   order.by = index(R_next)))
    mu_returns    <- rbind(mu_returns,    xts(as.numeric(R_next %*% w_mu),    order.by = index(R_next)))
    blend_returns <- rbind(blend_returns, xts(as.numeric(R_next %*% w_blend), order.by = index(R_next)))
  } else {
    # Optionally compute return for the last date if needed
    R_last <- aligned_returns[i, , drop = FALSE]
    erc_returns   <- rbind(erc_returns,   xts(as.numeric(R_last %*% w_erc),   order.by = index(R_last)))
    mu_returns    <- rbind(mu_returns,    xts(as.numeric(R_last %*% w_mu),    order.by = index(R_last)))
    blend_returns <- rbind(blend_returns, xts(as.numeric(R_last %*% w_blend), order.by = index(R_last)))
  }
  
  
  # ETF-level aggregation
  asset_w_list <- lapply(strategy_list, function(s) {
    wt <- s$weights
    dates_avail <- index(wt)[index(wt) <= date_index]
    if (length(dates_avail) == 0) return(setNames(rep(0, ncol(wt)), colnames(wt)))
    wt[dates_avail[length(dates_avail)], ]
  })
  names(asset_w_list) <- strategy_names
  
  build_etf <- function(strat_w) {
    etf_w <- setNames(rep(0, length(eft_universe)), eft_universe)
    for (j in seq_along(strat_w)) {
      name_j <- strategy_names[j]
      wj_strat <- strat_w[j]
      aw <- asset_w_list[[name_j]]
      tickers <- names(aw)
      for (k in seq_along(aw)) {
        etf_w[tickers[k]] <- etf_w[tickers[k]] + aw[, k] * wj_strat
      }
    }
    etf_w / sum(etf_w)
  }
  
  erf <- build_etf(w_erc)
  muf <- build_etf(w_mu)
  blf <- build_etf(w_blend)
  
  erc_etf_xts[i - start_index + 1, ]   <- erf
  mu_etf_xts[i - start_index + 1, ]    <- muf
  blend_etf_xts[i - start_index + 1, ] <- blf
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
charts.PerformanceSummary(portfolios["2015/"], legend.loc="topleft", main="Walk-Forward Strategy Comparison")
Return.annualized(portfolios["2014/"])


# === HISTORICAL WEIGHTS OUTPUT ===
# Strategy-level:
print(tail(erc_weights_xts))
print(tail(mu_weights_xts))
print(tail(blend_weights_xts))
# ETF-level:
print(tail(erc_etf_xts))
print(tail(mu_etf_xts))
print(tail(blend_etf_xts))

# Get trade list #

# Suppose your last blend weights xts has:
tw <- as.numeric(last(blend_etf_xts))
names(tw) <- colnames(blend_etf_xts)

# And you currently hold:
current <- c(cash = 0,
             QQQ = 0,
             TQQQ = 0,
             SPY = 0,
             UPRO = 0,
             EURL = 0,
             EDC = 0,
             DRN = 0,
             TYD = 0,
             TMF = 0,
             SHNY = 0)

# With $5,000 new cash coming in:
trade_list <- generate_trade_list(
  target_w       = tw,
  current_shares = current,
  cash_inflow    = 5000,
  date           = last(index(blend_etf_xts))
)

print(trade_list)