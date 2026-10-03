rm(list = ls())

# ==============================================================================
# ADAPTIVE LEVERAGE V1 + V2
# ==============================================================================
# [METHODOLOGY] ----------------------------------------------------------- #
# V1 reproduces the original 3-signal Adaptive Leverage model:
#   SMA signal:  SMA(21) / SMA(200) > 1.00
#   VOL signal:  short vol / long vol < 1.00
#   P2H signal:  Close / prior 252-day high > 0.90
#   Score = SMA + VOL + P2H, lagged 1 trading day.
#   Exposure: score 0/1/2/3 -> 0.0x / 0.5x / 0.9x / 3.0x.
#
# V2 keeps the same signals but preserves WHICH signals are active:
#   state = SMA/VOL/P2H, e.g. 110 = SMA on, VOL on, P2H off.
#   000=0.0x, 001=0.5x, 010=0.5x, 100=0.0x,
#   011=0.5x, 101=0.9x, 111=3.0x.
#   For 110:
#       if 200DMA > its level 20 trading days ago -> 3.0x
#       otherwise                              -> 1.5x
#
# All signals AND the 200DMA-slope gate are lagged one trading day before
# earning returns. No forward return is used in signal construction.
#
# Synthetic backtest:
#   S&P 500 daily return * target exposure.
#
# Tradeable ETF backtest:
#   0.0-1.0x -> cash + SPY
#   1.0-3.0x -> SPY + UPRO, where effective exposure is
#               SPY_weight + 3 * UPRO_weight.
#   Example: 1.5x = 75% SPY + 25% UPRO; 3.0x = 100% UPRO.
#   Cash earns a lagged 13-week T-bill proxy (^IRX).
#   Optional transaction costs are charged on each dollar traded.
#
# Research intent:
#   V1 remains the untouched benchmark. V2 is the frozen research candidate
#   developed from state-identity + long-term-trend-slope robustness work.
# ----------------------------------------------------------------------- #

# ---- PACKAGES ---------------------------------------------------------------
pkgs <- c("quantmod", "PerformanceAnalytics", "xts", "TTR")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- USER INPUTS ------------------------------------------------------------
INDEX_TICKER <- "SPY"
FROM_DATE    <- "1900-01-01"

sma_short <- 21
sma_long  <- 200
sma_thres <- 1.00

vol_short <- 65
vol_long  <- 252
vol_thres <- 1.00

p2h_days  <- 252
p2h_thres <- 0.90

gate_ma_days    <- 200
gate_slope_days <- 20

tc_bps <- 0
chart_from <- "2000-01-01"

EXPORT_OUTPUTS <- TRUE
UTILS_PATH <- "~/quant_portfolio/02_strategies/utils.R"
OUTPUT_DIR <- "~/quant_portfolio/03_portfolio_aggregation/strategy_outputs"

# ---- HELPERS ----------------------------------------------------------------
lag1 <- function(x) stats::lag(x, 1)

exposure_to_weights <- function(exposure, prefix = "") {
  e <- as.numeric(exposure)
  cash <- ifelse(e <= 1, 1 - e, 0)
  upro <- ifelse(e <= 1, 0, (e - 1) / 2)
  spy  <- 1 - cash - upro
  out <- xts(cbind(cash, spy, upro), order.by = index(exposure))
  names(out) <- paste0(prefix, c("cash", "SPY", "UPRO"))
  out
}

perf <- function(x) {
  x <- na.omit(x)
  data.frame(
    CAGR   = as.numeric(Return.annualized(x, geometric = TRUE)),
    AnnVol = as.numeric(StdDev.annualized(x)),
    Sharpe = as.numeric(SharpeRatio.annualized(x)),
    MaxDD  = -as.numeric(maxDrawdown(x))
  )
}

apply_costs <- function(ret, w, bps = 0) {
  w <- na.locf(w, na.rm = FALSE)
  traded <- xts(rowSums(abs(w - lag1(w))), order.by = index(w))
  traded[1] <- 0
  ret - traded * (bps / 10000)
}

# ---- MARKET DATA ------------------------------------------------------------
asset <- getSymbols(INDEX_TICKER, src = "yahoo", from = FROM_DATE,
                    auto.assign = FALSE, warnings = FALSE)
asset <- Cl(asset)
names(asset) <- "Close"
asset$Close <- na.locf(asset$Close)

# ---- SIGNAL ENGINE ----------------------------------------------------------
asset$Return <- dailyReturn(asset$Close)

asset$sma_short <- SMA(asset$Close, sma_short)
asset$sma_long  <- SMA(asset$Close, sma_long)
asset$sma_rat   <- asset$sma_short / asset$sma_long

logret <- diff(log(asset$Close))
asset$stvol   <- runSD(logret, n = vol_short - 1)
asset$ltvol   <- runSD(logret, n = vol_long - 1)
asset$vol_rat <- asset$stvol / asset$ltvol

prior_high <- lag1(runMax(asset$Close, n = p2h_days))
asset$p2h  <- asset$Close / prior_high

asset$smasig <- ifelse(asset$sma_rat > sma_thres, 1, 0)
asset$volsig <- ifelse(asset$vol_rat < vol_thres, 1, 0)
asset$p2hsig <- ifelse(asset$p2h > p2h_thres, 1, 0)

asset$score_raw <- asset$smasig + asset$volsig + asset$p2hsig
asset$state_raw <- 100 * asset$smasig + 10 * asset$volsig + asset$p2hsig

gate_ma <- SMA(asset$Close, gate_ma_days)
asset$gate_rising_raw <- ifelse(gate_ma > lag(gate_ma, gate_slope_days), 1, 0)

# Lag EVERYTHING used for today's position.
asset$score_trade       <- lag1(asset$score_raw)
asset$state_trade       <- lag1(asset$state_raw)
asset$gate_rising_trade <- lag1(asset$gate_rising_raw)

# ---- V1: ORIGINAL SCORE MODEL -----------------------------------------------
asset$V1_Exposure <- ifelse(asset$score_trade == 0, 0.0,
                     ifelse(asset$score_trade == 1, 0.5,
                     ifelse(asset$score_trade == 2, 0.9,
                     ifelse(asset$score_trade == 3, 3.0, NA))))

# ---- V2: STATE + SLOPE-GATED 110 --------------------------------------------
st <- asset$state_trade
asset$V2_Exposure <- ifelse(st ==   0, 0.0,
                     ifelse(st ==   1, 0.5,
                     ifelse(st ==  10, 0.5,
                     ifelse(st == 100, 0.0,
                     ifelse(st ==  11, 0.5,
                     ifelse(st == 101, 0.9,
                     ifelse(st == 110,
                            ifelse(asset$gate_rising_trade == 1, 3.0, 1.5),
                     ifelse(st == 111, 3.0, NA))))))))

asset$V1_Synthetic <- asset$Return * asset$V1_Exposure
asset$V2_Synthetic <- asset$Return * asset$V2_Exposure

# ---- TRADEABLE SPY / UPRO / CASH BACKTEST ----------------------------------
SPY  <- getSymbols("SPY",  src = "yahoo", from = FROM_DATE, auto.assign = FALSE)
UPRO <- getSymbols("UPRO", src = "yahoo", from = FROM_DATE, auto.assign = FALSE)
IRX  <- getSymbols("^IRX", src = "yahoo", from = FROM_DATE, auto.assign = FALSE)

spy_ret  <- dailyReturn(Ad(SPY));  names(spy_ret)  <- "SPY"
upro_ret <- dailyReturn(Ad(UPRO)); names(upro_ret) <- "UPRO"

irx_yield <- lag1(Cl(IRX)) / 100
cash_ret  <- (1 + irx_yield)^(1 / 252) - 1
names(cash_ret) <- "cash"

w1 <- exposure_to_weights(asset$V1_Exposure, "V1_")
w2 <- exposure_to_weights(asset$V2_Exposure, "V2_")

trade_data <- merge(spy_ret, upro_ret, w1, w2, join = "inner")
trade_data <- merge(trade_data, cash_ret, join = "left")
trade_data$cash <- na.locf(trade_data$cash, na.rm = FALSE)
trade_data <- trade_data[complete.cases(trade_data), ]

trade_data$V1_Return <- trade_data$V1_cash * trade_data$cash +
                        trade_data$V1_SPY  * trade_data$SPY +
                        trade_data$V1_UPRO * trade_data$UPRO

trade_data$V2_Return <- trade_data$V2_cash * trade_data$cash +
                        trade_data$V2_SPY  * trade_data$SPY +
                        trade_data$V2_UPRO * trade_data$UPRO

trade_data$V1_Return_Net <- apply_costs(
  trade_data$V1_Return,
  trade_data[, c("V1_cash", "V1_SPY", "V1_UPRO")],
  tc_bps
)

trade_data$V2_Return_Net <- apply_costs(
  trade_data$V2_Return,
  trade_data[, c("V2_cash", "V2_SPY", "V2_UPRO")],
  tc_bps
)

# ---- PERFORMANCE ------------------------------------------------------------
synthetic <- na.omit(asset[, c("Return", "V1_Synthetic", "V2_Synthetic")])
names(synthetic) <- c("SP500", "V1_Original", "V2_Slope_Gated")

actual <- trade_data[, c("SPY", "V1_Return_Net", "V2_Return_Net")]
names(actual) <- c("SPY", "V1_Original", "V2_Slope_Gated")

synthetic_perf <- do.call(
  rbind,
  lapply(seq_len(ncol(synthetic)), function(i) perf(synthetic[, i]))
)
actual_perf <- do.call(
  rbind,
  lapply(seq_len(ncol(actual)), function(i) perf(actual[, i]))
)

rownames(synthetic_perf) <- colnames(synthetic)
rownames(actual_perf)    <- colnames(actual)

cat("\nSYNTHETIC INDEX BACKTEST\n")
print(round(synthetic_perf, 4))

cat("\nACTUAL ETF BACKTEST (SPY/UPRO/T-BILL)\n")
cat("Transaction cost =", tc_bps, "bps per dollar traded\n")
print(round(actual_perf, 4))

# ---- CURRENT POSITION -------------------------------------------------------
latest <- tail(
  asset[, c(
    "score_trade",
    "state_trade",
    "gate_rising_trade",
    "V1_Exposure",
    "V2_Exposure"
  )],
  1
)

latest_w1 <- tail(w1, 1)
latest_w2 <- tail(w2, 1)

cat("\nLATEST SIGNAL / EXPOSURE\n")
print(latest)

cat("\nLATEST V1 TRADEABLE WEIGHTS\n")
print(round(latest_w1, 4))

cat("\nLATEST V2 TRADEABLE WEIGHTS\n")
print(round(latest_w2, 4))

# ---- CHARTS -----------------------------------------------------------------
charts.PerformanceSummary(
  synthetic[paste0(chart_from, "/"), c("V1_Original", "V2_Slope_Gated")],
  main = "Adaptive Leverage: V1 vs V2 (Synthetic)"
)

charts.PerformanceSummary(
  actual[paste0(chart_from, "/"), ],
  main = "Adaptive Leverage: Actual ETF Implementation"
)

# Export unprefixed ticker columns; keep prefixed w1/w2 for comparison analytics.
# ---- OPTIONAL PORTFOLIO-AGGREGATION EXPORT ---------------------------------
if(EXPORT_OUTPUTS && file.exists(path.expand(UTILS_PATH))) {
  source(path.expand(UTILS_PATH))

  if(exists("export_strategy_output")) {
    dir.create(path.expand(OUTPUT_DIR), recursive = TRUE, showWarnings = FALSE)

    export_strategy_output(
      strategy_name = "Adaptive_Leverage_V1",
      returns_xts   = trade_data$V1_Return_Net,
      weights_xts   = exposure_to_weights(asset$V1_Exposure)[index(trade_data)],
      output_dir    = path.expand(OUTPUT_DIR)
    )

    export_strategy_output(
      strategy_name = "Adaptive_Leverage_V2",
      returns_xts   = trade_data$V2_Return_Net,
      weights_xts   = exposure_to_weights(asset$V2_Exposure)[index(trade_data)],
      output_dir    = path.expand(OUTPUT_DIR)
    )
  }
}
