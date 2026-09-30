# CONCRETUM: A CENTURY OF PROFITABLE INDUSTRY TRENDS ---------------------------
# Independent R translation of Concretum Group's public Python methodology.
# Source: https://concretumgroup.substack.com/p/backtest-a-profitable-trend-following
# Implementation: https://concretumgroup.com/backtest-a-profitable-trend-following-strategy-using-python/
# Not an official Concretum implementation. Industry portfolios are NOT tradable ETFs.

# METHODOLOGY -----------------------------------------------------------------
# {
# PURPOSE AND DATA
#   Long-only trend following on Kenneth French's 48 DAILY industry portfolios.
#   Downloads daily industry returns and daily Fama-French factors from Dartmouth.
#   The market benchmark is Mkt-RF + RF, NOT SPY; idle cash earns daily RF.
#   These are historical industry portfolios, not securities one could have bought.
#   Return values in French files are percentages and are divided by 100.
#   Industry missing-value sentinels (returns <= -99%) become NA. For a synthetic
#   industry price only, missing returns count as 0 (matching Concretum's Python);
#   the missing industry is NOT treated as investable on those dates.
#
# SIGNALS (INDEPENDENTLY FOR EACH INDUSTRY)
#   Reconstruct a total-return price index: P[t] = P[t-1] * (1 + r[t]).
#   Donchian upper = highest CLOSE in the preceding 20 observations INCLUDING t.
#   Donchian lower = lowest CLOSE in the preceding 40 observations INCLUDING t.
#   Keltner upper = EMA(P,20) + 2 * 1.4 * mean(abs(P[t]-P[t-1]),20).
#   Keltner lower = EMA(P,40) - 2 * 1.4 * mean(abs(P[t]-P[t-1]),40).
#   Concretum uses absolute close changes instead of ATR because French files
#   do not contain intraday high/low prices. Upper=min(Donchian,Keltner),
#   lower=max(Donchian,Keltner). Entry at close t is allowed when
#   P[t] >= upper[t-1] AND upper[t-1] > lower[t-1]. The bands at t-1
#   avoid a forward-looking entry threshold.
#
#   Once long, maintain a NONDECREASING trailing stop:
#   stop[t] = max(stop[t-1], lower[t]). Exit at close t if
#   P[t] <= max(stop[t-1], lower[t]); otherwise retain the position.
#   A new position gets stop[t] = lower[t]. There are no short positions.
#   A stopped position can reenter on a later day's breakout.
#
# POSITION SIZING AND PORTFOLIO RETURNS
#   Compute 20-day population standard deviation (ddof=0) of each industry's
#   daily returns. On an active signal: preliminary weight = target_vol / vol.
#   Divide by the NUMBER OF INDUSTRIES WITH AVAILABLE RETURNS that day, not
#   by the number of active breakouts. Cap each weight at max_single_weight;
#   if total gross weight > max_gross_exposure, scale ALL weights pro rata.
#   Recompute target weights every close, even if the set of longs is unchanged.
#   Signals/weights determined at close t earn returns starting t+1.
#   extra_lag_days=1 delays execution AN ADDITIONAL day (t+2 returns).
#   Portfolio return = sum(lagged industry weight * industry daily return)
#       + (1 - sum(lagged industry weights)) * daily Fama-French RF.
#   Hence a portfolio above 100% exposure pays the RF rate on borrowed capital;
#   any exposure below 100% receives RF. Additional financing spread optional.
#   The historic paper's idealized run assumes zero commissions/slippage;
#   trade_bps permits a simple gross-notional trading-cost sensitivity.
#
# REPLICATION LIMITATIONS
#   target_vol=0.015 reflects the prose of the 2026 Substack article, whereas
#   Concretum's older downloadable Python example uses target_vol=0.02.
#   Set target_vol=0.02 to investigate the latter. Python pandas EMA seeds at
#   the initial price: a custom EMA below reproduces that choice. Results can
#   differ modestly because of provider revisions, missing data, and execution
#   conventions; all unvalidated performance outputs must be treated as research.
#   Unlike an actual ETF backtest, these industry series omit tracking error,
#   ETF fees, market impact, borrow frictions, and historic investability.
# }
# END METHODOLOGY -------------------------------------------------------------

# USER SETTINGS ---------------------------------------------------------------
up_days <- 20L
 down_days <- 40L
atr_adjustment <- 1.4
keltner_multiplier <- 2.0
target_vol <- 0.015             # Original article prose. Python notebook: 0.020.
max_gross_exposure <- 2.0
max_single_weight <- 0.20
extra_lag_days <- 0L            # 0 = next-session returns; 1 = one EXTRA day late.
trade_bps <- 0                  # Modeled cost per gross traded notional, in bps.
borrow_spread_annual <- 0       # Extra spread above RF when portfolio > 100%.
include_cash_interest <- TRUE
paper_end_date <- as.Date("2024-03-31")
show_charts <- interactive()
cache_dir <- file.path(getwd(), "concretum_data_cache")

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(TTR)
  library(PerformanceAnalytics)
})
stopifnot(up_days >= 2L, down_days >= 2L, target_vol > 0,
          max_gross_exposure > 0, max_single_weight > 0,
          extra_lag_days >= 0L, extra_lag_days == as.integer(extra_lag_days),
          trade_bps >= 0, borrow_spread_annual >= 0)

# DOWNLOAD AND PARSE THE DAILY BLOCKS ----------------------------------------
industry_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/48_Industry_Portfolios_daily_CSV.zip"
factors_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

get_csv_lines <- function(url, filename) {
  zipfile <- file.path(cache_dir, filename)
  if (!file.exists(zipfile)) {
    message("Downloading: ", url)
    tryCatch(utils::download.file(url, zipfile, mode = "wb", quiet = TRUE,
                                  method = "libcurl"),
             error = function(e) stop("Could not download ", url,
                                     "\nPlease save that ZIP manually as ", zipfile,
                                     "\nUnderlying error: ", conditionMessage(e)))
  }
  if (!file.exists(zipfile) || file.info(zipfile)$size < 100L)
    stop("ZIP missing/empty: ", zipfile, ". Delete a broken ZIP and retry.")
  members <- tryCatch(utils::unzip(zipfile, list = TRUE)$Name,
                      error = function(e) stop("Invalid ZIP ", zipfile, ": ", conditionMessage(e)))
  csv <- members[grepl("\\.csv$", members, ignore.case = TRUE)]
  if (length(csv) != 1L) stop("Expected one CSV inside ", zipfile)
  extracted <- utils::unzip(zipfile, files = csv, exdir = cache_dir, overwrite = TRUE)
  readLines(extracted, warn = FALSE, encoding = "UTF-8")
}

parse_daily_block <- function(lines, n_fields, label) {
  # CSV daily rows start with YYYYMMDD; ignore monthly/annual blocks and notes.
  is_daily <- grepl("^\\s*[0-9]{8}\\s*,", lines)
  start <- which(is_daily)[1]
  if (!is.finite(start)) stop("No YYYYMMDD daily observations in ", label)
  following <- which(!is_daily[start:length(lines)])
  finish <- if (length(following)) start + following[1] - 2L else length(lines)
  tab <- utils::read.csv(text = paste(lines[start:finish], collapse = "\n"),
                         header = FALSE, check.names = FALSE, strip.white = TRUE,
                         na.strings = c("-99.99", "-999", "-999.99"),
                         stringsAsFactors = FALSE)
  if (NCOL(tab) != n_fields)
    stop(label, " expected ", n_fields, " fields, found ", NCOL(tab),
         ". The French source format may have changed.")
  date_string <- trimws(as.character(tab[[1]]))
  dates <- as.Date(date_string, "%Y%m%d")
  if (anyNA(dates) || anyDuplicated(dates) || is.unsorted(dates))
    stop("Malformed, duplicated or unsorted dates in ", label)
  value <- suppressWarnings(as.matrix(data.frame(lapply(tab[-1], as.numeric))))
  if (!NROW(value) || any(dim(value) != c(length(dates), n_fields - 1L)))
    stop("Malformed numeric data in ", label)
  list(date = dates, values = value)
}

ind_lines <- get_csv_lines(industry_url, "48_Industry_Portfolios_daily_CSV.zip")
ff_lines <- get_csv_lines(factors_url, "F-F_Research_Data_Factors_daily_CSV.zip")
industry <- parse_daily_block(ind_lines, 49L, "48 industry portfolios")
factors <- parse_daily_block(ff_lines, 5L, "Fama-French daily factors")

# The first CSV header line before the first YYYYMMDD row holds industry names.
header_line <- tail(ind_lines[seq_len(which(grepl("^\\s*[0-9]{8}\\s*,", ind_lines))[1] - 1L)], 1)
header_names <- trimws(strsplit(header_line, ",", fixed = TRUE)[[1]])
industry_names <- if (length(header_names) == 49L)
  make.unique(header_names[-1L]) else sprintf("Industry_%02d", 1:48)

ind_r <- industry$values / 100
ind_r[!is.finite(ind_r) | ind_r <= -0.99] <- NA_real_
colnames(ind_r) <- industry_names
R_ind <- xts::xts(ind_r, order.by = industry$date)
F <- xts::xts(factors$values / 100, order.by = factors$date)
colnames(F) <- c("Mkt_RF", "SMB", "HML", "RF")
all_dates <- intersect(index(R_ind), index(F))
if (length(all_dates) < 1000L) stop("Too few overlapping daily observations.")
R_ind <- R_ind[all_dates]
F <- F[all_dates]
rf <- as.numeric(F[, "RF"])
market <- as.numeric(F[, "Mkt_RF"]) + rf
if (any(!is.finite(rf)) || any(!is.finite(market)))
  stop("Missing market or RF factor after date alignment: inspect source files.")
dates <- index(R_ind)
R <- as.matrix(R_ind)
N <- NCOL(R); TT <- NROW(R)

# INDICATORS ------------------------------------------------------------------
roll_mean_min <- function(x, n, min_obs = n - 1L) {
  ok <- is.finite(x)
  xx <- ifelse(ok, x, 0)
  s <- c(0, cumsum(xx)); cts <- c(0, cumsum(as.integer(ok)))
  i <- seq_along(x); left <- pmax(1L, i - n + 1L)
  cnt <- cts[i + 1L] - cts[left]
  ans <- (s[i + 1L] - s[left]) / pmax(cnt, 1L)
  ans[cnt < min_obs] <- NA_real_
  ans
}
roll_sd_population <- function(x, n) {
  ok <- is.finite(x); xx <- ifelse(ok, x, 0)
  sx <- c(0, cumsum(xx)); sx2 <- c(0, cumsum(xx^2))
  ct <- c(0, cumsum(as.integer(ok)))
  i <- seq_along(x); left <- pmax(1L, i - n + 1L)
  count <- ct[i + 1L] - ct[left]
  mu <- (sx[i + 1L] - sx[left]) / pmax(count, 1L)
  var <- (sx2[i + 1L] - sx2[left]) / pmax(count, 1L) - mu^2
  out <- sqrt(pmax(var, 0))
  out[count < n] <- NA_real_
  out
}
pandas_ema <- function(x, n) {
  ans <- numeric(length(x)); ans[1] <- x[1]
  alpha <- 2 / (n + 1)
  if (length(x) > 1L) for (i in 2:length(x))
    ans[i] <- alpha * x[i] + (1 - alpha) * ans[i - 1L]
  ans
}

P <- apply(R, 2, function(z) cumprod(1 + ifelse(is.finite(z), z, 0)))
colnames(P) <- colnames(R)
VOL <- UP <- DOWN <- matrix(NA_real_, TT, N,
                           dimnames = list(NULL, colnames(R)))
for (j in seq_len(N)) {
  p <- P[, j]
  abs_delta <- c(NA_real_, abs(diff(p)))
  donc_upper <- as.numeric(TTR::runMax(p, n = up_days))
  donc_lower <- as.numeric(TTR::runMin(p, n = down_days))
  kelt_upper <- pandas_ema(p, up_days) + keltner_multiplier * atr_adjustment *
    roll_mean_min(abs_delta, up_days)
  kelt_lower <- pandas_ema(p, down_days) - keltner_multiplier * atr_adjustment *
    roll_mean_min(abs_delta, down_days)
  UP[, j] <- pmin(donc_upper, kelt_upper)
  DOWN[, j] <- pmax(donc_lower, kelt_lower)
  VOL[, j] <- roll_sd_population(R[, j], up_days)
}

# STATE MACHINE: CLOSE-T DECISIONS, RATCHETING STOPS --------------------------
exposure <- matrix(FALSE, TT, N, dimnames = list(NULL, colnames(R)))
trailing_stop <- matrix(NA_real_, TT, N, dimnames = list(NULL, colnames(R)))
for (t in 2:TT) {
  valid <- is.finite(R[t, ]) & is.finite(UP[t, ]) &
    is.finite(DOWN[t, ]) & is.finite(VOL[t, ]) & VOL[t, ] > 0
  enter <- valid & !exposure[t-1L, ] &
    is.finite(UP[t-1L, ]) & is.finite(DOWN[t-1L, ]) &
    P[t, ] >= UP[t-1L, ] & UP[t-1L, ] > DOWN[t-1L, ]
  keep <- valid & exposure[t-1L, ] &
    P[t, ] > pmax(trailing_stop[t-1L, ], DOWN[t, ])
  enter[is.na(enter)] <- FALSE
  keep[is.na(keep)] <- FALSE
  exposure[t, enter | keep] <- TRUE
  trailing_stop[t, enter] <- DOWN[t, enter]
  trailing_stop[t, keep] <- pmax(trailing_stop[t-1L, keep], DOWN[t, keep])
}

# DAILY TARGET WEIGHTS, LIMITS, AND ADJUSTABLE EXECUTION LAG ------------------
number_available <- rowSums(is.finite(R))
raw_w <- target_vol / VOL
raw_w[!is.finite(raw_w)] <- 0
raw_w <- raw_w * exposure / pmax(number_available, 1)
raw_w <- pmin(raw_w, max_single_weight)
gross <- rowSums(raw_w)
scale_down <- pmin(1, max_gross_exposure / pmax(gross, 1e-12))
target_w <- raw_w * scale_down
stopifnot(all(is.finite(target_w)), all(target_w >= 0),
          all(rowSums(target_w) <= max_gross_exposure + 1e-9))

delay <- 1L + as.integer(extra_lag_days)
held_w <- matrix(0, TT, N, dimnames = list(NULL, colnames(R)))
if (delay < TT) held_w[(delay + 1L):TT, ] <- target_w[1L:(TT - delay), ]
R_usable <- R
missing_while_held <- which(!is.finite(R_usable) & held_w > 0, arr.ind = TRUE)
if (NROW(missing_while_held))
  warning(NROW(missing_while_held), " held industry returns are missing; filled with zero like the original notebook.")
R_usable[!is.finite(R_usable)] <- 0
held_total <- rowSums(held_w)
industry_component <- rowSums(held_w * R_usable)
cash_component <- if (include_cash_interest) (1 - held_total) * rf else rep(0, TT)
borrow_cost <- pmax(held_total - 1, 0) * borrow_spread_annual / 252

# Approximate commission at each close on changes from drifted weights.
# Initial deployment and subsequent daily reweights are included.
trade_notional <- numeric(TT)
execution_close <- matrix(0, TT, N)
if (extra_lag_days < TT)
  execution_close[(extra_lag_days + 1L):TT, ] <-
    target_w[1L:(TT - extra_lag_days), ]
for (t in seq_len(TT)) {
  nav_before_trade <- 1 + industry_component[t] + cash_component[t] - borrow_cost[t]
  if (!is.finite(nav_before_trade) || nav_before_trade <= 0)
    stop("Portfolio wealth nonpositive on ", dates[t], "; cannot calculate drift.")
  pretrade_w <- held_w[t, ] * (1 + R_usable[t, ]) / nav_before_trade
  trade_notional[t] <- sum(abs(execution_close[t, ] - pretrade_w))
}
transaction_cost <- trade_notional * trade_bps / 10000
strategy_r <- industry_component + cash_component - borrow_cost - transaction_cost
if (any(1 + strategy_r <= 0)) stop("Portfolio reached zero or negative wealth.")

strategy <- xts(strategy_r, order.by = dates)
benchmark <- xts(market, order.by = dates)
cash_rate <- xts(rf, order.by = dates)
gross_exposure <- xts(held_total, order.by = dates)
turnover <- xts(trade_notional, order.by = dates)
colnames(strategy) <- "Concretum_Industry_Trend"
colnames(benchmark) <- "FF_Market"
colnames(cash_rate) <- "RF"
colnames(gross_exposure) <- "Gross_Exposure"
colnames(turnover) <- "Traded_Notional"

summarize_period <- function(x, market_x, rf_x, label) {
  z <- as.numeric(x); m <- as.numeric(market_x); r <- as.numeric(rf_x)
  if (length(z) < 42L || any(!is.finite(z))) stop("Insufficient data for ", label)
  metric <- function(v) {
    wealth <- cumprod(1 + v)
    drawdown <- wealth / cummax(c(1, wealth))[-1L] - 1
    excess <- v - r
    c(CAGR = 100 * (tail(wealth, 1)^(252 / length(v)) - 1),
      Sharpe = if (sd(excess) > 0) sqrt(252) * mean(excess) / sd(excess) else NA_real_,
      Volatility = 100 * sd(v) * sqrt(252),
      MaxDD = 100 * min(drawdown))
  }
  out <- rbind(Strategy = metric(z), FF_Market = metric(m))
  cat("\n", label, " (", as.character(first(index(x))), " through ",
      as.character(last(index(x))), "; ", NROW(x), " days)\n", sep = "")
  print(round(out, 2))
  invisible(out)
}

# Original paper's time window; the live Kenneth French files may extend later.
paper_ix <- dates <= paper_end_date
if (sum(paper_ix) > 42L)
  paper_stats <- summarize_period(strategy[paper_ix], benchmark[paper_ix],
                                  cash_rate[paper_ix], "PAPER-END SAMPLE")
full_stats <- summarize_period(strategy, benchmark, cash_rate, "FULL AVAILABLE HISTORY")
cat("\nMean gross exposure:", round(mean(held_total), 3),
    "| Mean daily gross traded notional:", round(mean(trade_notional), 4),
    "| Signal execution lag (trading days):", delay, "\n")

if (show_charts) {
  comparison <- merge(strategy, benchmark)
  PerformanceAnalytics::charts.PerformanceSummary(comparison,
    main = "Concretum industry trend vs Fama-French market")
  plot(gross_exposure, main = "Gross industry exposure", ylab = "x NAV")
}

# Inspect objects without writing to your portfolio aggregation directory.
backtest <- list(strategy = strategy, benchmark = benchmark, cash_rate = cash_rate,
                 target_weights = xts(target_w, order.by = dates),
                 held_weights = xts(held_w, order.by = dates),
                 active = xts(exposure * 1, order.by = dates),
                 trailing_stops = xts(trailing_stop, order.by = dates),
                 gross_exposure = gross_exposure, traded_notional = turnover,
                 paper_stats = if (exists("paper_stats")) paper_stats else NULL,
                 full_stats = full_stats)
