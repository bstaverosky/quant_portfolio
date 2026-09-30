# Three-day leveraged ETF reversal: extended SYNTHETIC and REAL-ETF backtests.
# Independent approximation of FinLab's strategy, NOT a FinLab-engine reproduction.
# https://finlab.finance/en/blog/us-mean-reversion-strategy
# Month-end close signal -> next trading day's CLOSE; touched 8% stop thereafter.
# Synthetic 3x DAILY reset: QQQ -> TQQQ, XLK -> TECL, with financing + fund fees.
# No strategy-output exports. Requires internet from YOUR R session for Yahoo / FRED.

suppressPackageStartupMessages({library(quantmod); library(TTR); library(PerformanceAnalytics)})
options(timeout=300)

### SETTINGS ###
from <- "1998-01-01"; article_start <- as.Date("2016-06-01"); article_end <- as.Date("2026-06-12")
trade_bps <- 5; funding_spread <- 0.01; synthetic_fee <- c(TQQQ=0.0095, TECL=0.0095)
trend_days <- 200; trend_min <- 100; momentum_days <- 126; reversal_days <- 3
stop_pct <- 0.08; use_stop <- TRUE; show_charts <- interactive()
# Set TRUE only with complete, accurate holdings and after checking today's signal and stop history.
run_trade_list <- FALSE; current_shares <- c(Cash=0, TQQQ=0, TECL=0, IEF=0, GLD=0, SHY=0)
cash_inflow <- 0

### DOWNLOAD: REAL ETF OHLC AND A LAGGED FINANCING-RATE PROXY ###
tickers <- c("QQQ", "XLK", "TQQQ", "TECL", "IEF", "GLD", "SHY", "SPY")
fetch <- function(s) {
  message("Downloading ", s)
  x <- try(getSymbols(s, src="yahoo", from=from, auto.assign=FALSE, warnings=FALSE), silent=TRUE)
  if (inherits(x, "try-error") || !NROW(x)) stop("Yahoo download failed: ", s)
  x
}
raw <- setNames(lapply(tickers, fetch), tickers)
tbill <- try(getSymbols("DTB3", src="FRED", from=from, auto.assign=FALSE), silent=TRUE)
if (inherits(tbill, "try-error") || !NROW(tbill)) stop("FRED DTB3 download failed")
last_common <- min(as.Date(vapply(raw, function(x) as.character(last(index(x))), character(1))))
idx <- index(raw$QQQ); idx <- idx[idx <= last_common]
if (length(idx) < trend_days + 5) stop("Insufficient QQQ history")
P <- do.call(merge, lapply(tickers, function(s) { z <- Ad(raw[[s]]); colnames(z) <- s; z }))
P <- zoo::na.locf(P[idx], na.rm=FALSE) # Only fills AFTER a ticker's first observation; never backfills inception.
R <- P / lag(P, 1) - 1
adj_field <- function(s, field) {
  x <- raw[[s]]; factor <- Ad(x) / Cl(x)
  z <- switch(field, open=Op(x), low=Lo(x)) * factor; colnames(z) <- s
  as.numeric(merge(P[,"QQQ"], z, join="left")[,2])
}
open_real <- xts(do.call(cbind, lapply(tickers, function(s) adj_field(s, "open"))), order.by=idx)
low_real  <- xts(do.call(cbind, lapply(tickers, function(s) adj_field(s, "low"))), order.by=idx)
colnames(open_real) <- colnames(low_real) <- tickers
# FRED's DTB3 is the 3-month T-bill discount yield, an APPROXIMATION to borrowing costs.
rate <- merge(P[,"QQQ"], tbill, join="left")[,2]
rf_daily <- as.numeric(lag(zoo::na.locf(rate, na.rm=FALSE), 1)) / 100 / 252
if (!is.finite(tail(rf_daily,1))) stop("Missing financing-rate data near end of sample")

### DAILY-RESET SYNTHETIC PRICES AND INTRADAY STOP-PRICE PROXIES ###
# Close-to-close: 3 * underlying adjusted return - 2 * (lagged T-bill + spread) - fee.
# Open/low: apply 3x to same-day underlying overnight/intraday path from prior close.
# These OHLC values are approximate; they are NOT synthetic leveraged ETF market quotes.
synth_3x <- function(base, ticker) {
  u <- as.numeric(P[,base]); o <- adj_field(base,"open"); lo <- adj_field(base,"low")
  n <- length(u); close <- op <- low <- rep(NA_real_, n)
  valid <- which(is.finite(u) & u > 0 & is.finite(rf_daily))
  if (!length(valid)) stop("No synthetic history for ", ticker)
  initial <- valid[1]; close[initial] <- op[initial] <- low[initial] <- 100
  if (initial < n) for (i in seq.int(initial+1L,n)) {
    if (!all(is.finite(c(u[i-1],u[i],o[i],lo[i],rf_daily[i],close[i-1])))) stop("Missing synthetic input for ",ticker," on ",idx[i])
    drag <- 2*(rf_daily[i]+funding_spread/252)+synthetic_fee[ticker]/252
    daily <- 1+3*(u[i]/u[i-1]-1)-drag
    overnight <- 1+3*(o[i]/u[i-1]-1)
    intralow <- 1+3*(lo[i]/u[i-1]-1)
    if (min(daily,overnight,intralow) <= 0) stop("3x synthetic price <= 0 for ",ticker," on ",idx[i])
    close[i] <- close[i-1]*daily; op[i] <- close[i-1]*overnight
    low[i] <- close[i-1]*min(intralow,overnight,daily)
  }
  z <- xts(cbind(close=close,open=op,low=low),order.by=idx); z
}
syn <- list(TQQQ=synth_3x("QQQ","TQQQ"), TECL=synth_3x("XLK","TECL"))
P_syn <- P; open_syn <- open_real; low_syn <- low_real
for (s in c("TQQQ","TECL")) {
  P_syn[,s] <- syn[[s]][,"close"]; open_syn[,s] <- syn[[s]][,"open"]; low_syn[,s] <- syn[[s]][,"low"]
}

### ONE SIGNAL RECIPE FOR BOTH PRICE SOURCES ###
# The QQQ regime and defensive rankings use REAL adjusted ETF prices in both modes.
# Only TQQQ / TECL's 3-day rankings differ: modeled synthetic vs actual ETF.
make_signals <- function(prices) {
  qqq <- P[,"QQQ"]; sma <- SMA(qqq,n=trend_days)
  if (trend_min < trend_days && NROW(qqq) >= trend_min) for (i in trend_min:min(trend_days-1L,NROW(qqq))) sma[i] <- mean(as.numeric(qqq[1:i]))
  risk_on <- as.numeric(qqq) > as.numeric(sma) & as.numeric(qqq / lag(qqq,momentum_days)-1) > 0
  lev <- prices[,c("TQQQ","TECL")] / lag(prices[,c("TQQQ","TECL")],reversal_days)-1
  defensive <- P[,c("IEF","GLD","SHY")] / lag(P[,c("IEF","GLD","SHY")],63) - P[,c("IEF","GLD","SHY")] / lag(P[,c("IEF","GLD","SHY")],21)
  e <- endpoints(P,"months"); e <- e[e > 0 & e < NROW(P)] # Never assume final incomplete month is month-end.
  target <- rep(NA_character_,NROW(P)); names(target) <- as.character(idx)
  for (i in e) {
    if (!is.finite(as.numeric(sma[i])) || !is.finite(as.numeric(lag(qqq,momentum_days)[i]))) next
    candidates <- if (isTRUE(risk_on[i])) c("TQQQ","TECL") else c("IEF","GLD","SHY")
    scores <- if (isTRUE(risk_on[i])) as.numeric(lev[i,]) else as.numeric(defensive[i,])
    if (any(!is.finite(scores))) next
    target[i] <- candidates[if (isTRUE(risk_on[i])) which.min(scores) else which.max(scores)]
  }
  target
}
signal_synthetic <- make_signals(P_syn); signal_actual <- make_signals(P)

### ONE EXECUTION ENGINE: REAL OR SYNTHETIC, OPTIONAL ACTUAL-SIGNAL LOCK ###
# When stopped, move to 0%-return cash until the next monthly rebalance.
# Adjusted OHLC approximates touched stops; next-close fills are NOT FinLab's exact fills.
run_bt <- function(mode=c("synthetic","actual"), start, end=last_common, bps=trade_bps,
                   stop_enabled=use_stop, signal_mode=mode[1]) {
  mode <- match.arg(mode); signal_mode <- match.arg(signal_mode,c("synthetic","actual"))
  price <- if (mode=="synthetic") P_syn else P
  op <- if (mode=="synthetic") open_syn else open_real
  lo <- if (mode=="synthetic") low_syn else low_real
  signals <- if (signal_mode=="synthetic") signal_synthetic else signal_actual
  first_i <- which(idx >= as.Date(start))[1]; end_i <- tail(which(idx <= as.Date(end)),1)
  if (is.na(first_i) || !length(end_i) || end_i < first_i+5) stop("Invalid backtest dates")
  if (!is.finite(as.numeric(price[first_i,"GLD"]))) stop("Defensive data not available at start")
  ret <- nav <- rep(NA_real_,NROW(P)); held <- rep(NA_character_,NROW(P))
  trades <- data.frame(date=as.Date(character()),action=character(),ticker=character(),price=numeric(),stringsAsFactors=FALSE)
  holding <- "Cash"; entry <- NA_real_; wealth <- 1; rate_bps <- bps/10000
  for (i in first_i:end_i) {
    previous <- wealth; stopped <- FALSE
    if (holding != "Cash" && i > first_i) {
      prior <- as.numeric(price[i-1,holding]); today <- as.numeric(price[i,holding])
      if (!all(is.finite(c(prior,today))) || min(prior,today) <= 0) stop("Missing/invalid holding prices on ",idx[i])
      open_i <- as.numeric(op[i,holding]); low_i <- as.numeric(lo[i,holding]); level <- entry*(1-stop_pct)
      if (stop_enabled && (!all(is.finite(c(open_i,low_i))) || min(open_i,low_i) <= 0)) stop("Missing/invalid stop OHLC on ",idx[i])
      if (stop_enabled && low_i <= level) {
        fill <- min(open_i,level); wealth <- wealth*(fill/prior)*(1-rate_bps)
        trades <- rbind(trades,data.frame(date=as.Date(idx[i]),action="STOP SELL",ticker=holding,price=fill))
        holding <- "Cash"; entry <- NA_real_; stopped <- TRUE
      } else wealth <- wealth*today/prior
    }
    # At NEXT session close, implement PRIOR session's month-end decision.
    if (i > 1 && !is.na(signals[i-1]) && !stopped) {
      next_holding <- signals[i-1]
      if (next_holding != holding) {
        if (holding != "Cash") {
          trades <- rbind(trades,data.frame(date=as.Date(idx[i]),action="SELL",ticker=holding,price=as.numeric(price[i,holding])))
          wealth <- wealth*(1-rate_bps)
        }
        if (next_holding != "Cash") {
          buy <- as.numeric(price[i,next_holding]); if (!is.finite(buy) || buy <= 0) stop("Unavailable buy price: ",next_holding," on ",idx[i])
          trades <- rbind(trades,data.frame(date=as.Date(idx[i]),action="BUY",ticker=next_holding,price=buy))
          wealth <- wealth*(1-rate_bps)
        }
        holding <- next_holding; entry <- if (holding!="Cash") as.numeric(price[i,holding]) else NA_real_
      }
    }
    ret[i] <- wealth/previous-1; nav[i] <- wealth; held[i] <- holding
  }
  dates <- idx[first_i:end_i]
  out <- xts(ret[first_i:end_i],order.by=dates); colnames(out) <- paste(mode,signal_mode,sep="_")
  list(returns=out,equity=xts(nav[first_i:end_i],order.by=dates),holding=xts(held[first_i:end_i],order.by=dates),trades=trades)
}

### BACKTEST WINDOWS: EXTENDED, ACTUAL, MATCHED, ARTICLE'S 2016-2026 ###
# Actual 3x series start in 2010; synthetic can extend back once GLD and warm-up exist.
# Actual backtest starts AFTER the later 3x ETF's first close, to avoid an NA first-day benchmark return.
first_date <- function(s) as.Date(first(index(raw[[s]])))
synthetic_start <- max(first_date("GLD"),first_date("IEF"),first_date("SHY"),as.Date(idx[trend_days+1]))
# First listed close does NOT have a daily return. Start only after BOTH 3x ETFs
# have finite adjusted close-to-close returns, so the strategy and TQQQ benchmark
# are evaluated on exactly the same valid dates.
actual_inception <- max(synthetic_start,first_date("TQQQ"),first_date("TECL"))
valid_actual <- which(idx > actual_inception & complete.cases(R[,c("QQQ","TQQQ","TECL")]))
if (!length(valid_actual)) stop("No common valid daily returns for QQQ, TQQQ and TECL")
actual_start <- as.Date(idx[valid_actual[1]])
if (actual_start > last_common-30) stop("Insufficient overlapping actual ETF history")
if (article_start > last_common) stop("Article period begins after last available data")
article_cutoff <- min(article_end,last_common)

extended <- run_bt("synthetic",synthetic_start)
actual <- run_bt("actual",actual_start)
if (any(!is.finite(as.numeric(actual$returns)))) stop("Actual strategy has non-finite daily returns")
# Both matched runs start from CASH on the same trading day and use the SAME actual-ETF signals.
# This isolates synthetic-vs-real execution return differences more cleanly.
synthetic_matched <- run_bt("synthetic",actual_start,signal_mode="actual")
synthetic_own_matched <- run_bt("synthetic",actual_start,signal_mode="synthetic")
article_actual <- run_bt("actual",article_start,article_cutoff)
article_actual_zero_cost <- run_bt("actual",article_start,article_cutoff,bps=0) # FinLab reports no trading costs.
article_synthetic <- run_bt("synthetic",article_start,article_cutoff,signal_mode="actual")

### PERFORMANCE TABLES / DIAGNOSTICS ###
metrics <- function(x) {
  z <- as.numeric(x); z <- z[is.finite(z)]
  if (length(z) < 10 || any(z <= -1)) stop("Invalid return series")
  c(CAGR=100*(prod(1+z)^(252/length(z))-1),Sharpe=sqrt(252)*mean(z)/sd(z),
    Vol=100*sd(z)*sqrt(252),MaxDD=-100*as.numeric(maxDrawdown(x)))
}
bench <- function(symbol, dates) {
  x <- R[dates,symbol]; bad <- which(!is.finite(as.numeric(x)))
  if (length(bad)) stop("Missing benchmark return for ",symbol," on ",as.character(index(x)[bad[1]]),
                        " (",length(bad)," missing rows). Check price history rather than silently filling returns.")
  x
}
report <- function(named) {
  x <- do.call(rbind,lapply(named,function(z) { a <- metrics(z); c(Start=as.character(first(index(z))),End=as.character(last(index(z))),
                                                          N=NROW(z),round(a,2)) }))
  print(x,quote=FALSE)
}
cat("\n========== EXTENDED SYNTHETIC (pre-inception years; NOT realized ETF prices) ==========\n")
report(list(Synthetic=extended$returns,QQQ=bench("QQQ",index(extended$returns))))
cat("\n========== ACTUAL ETFs (since both 3x ETFs existed) ==========\n")
report(list(Actual=actual$returns,QQQ=bench("QQQ",index(actual$returns)),TQQQ=bench("TQQQ",index(actual$returns))))
cat("\n========== MATCHED DATES (same actual-ETF signals, same starting cash) ==========\n")
report(list(Synthetic_same_signals=synthetic_matched$returns,Actual=actual$returns,
            Synthetic_own_signals=synthetic_own_matched$returns,QQQ=bench("QQQ",index(actual$returns))))
cat("\n========== FINLAB ARTICLE WINDOW (actual ETFS, 2016-06 through 2026-06) ==========\n")
report(list(Actual_with_5bp_costs=article_actual$returns,Actual_zero_trade_costs=article_actual_zero_cost$returns,
            Synthetic_article_window=article_synthetic$returns,
            QQQ=bench("QQQ",index(article_actual$returns)),TQQQ=bench("TQQQ",index(article_actual$returns))))
cat("\nActual trades:",NROW(actual$trades),"| stop exits:",sum(actual$trades$action=="STOP SELL"),"\n")
cat("Synthetic extended trades:",NROW(extended$trades),"| stop exits:",sum(extended$trades$action=="STOP SELL"),"\n")
cat("Latest modeled ACTUAL holding (not a verified account holding):\n"); print(last(actual$holding))
cat("Latest FULL month-end signal:",tail(na.omit(signal_actual),1),"on",tail(names(signal_actual)[!is.na(signal_actual)],1),"\n")
if (show_charts) {
  charts.PerformanceSummary(merge(extended$returns,bench("QQQ",index(extended$returns))),main="Extended: modeled 3x vs QQQ")
  z <- merge(synthetic_matched$returns,actual$returns,bench("QQQ",index(actual$returns)))
  colnames(z) <- c("Synthetic / actual signals","Actual ETFs","QQQ")
  charts.PerformanceSummary(z,main="Same dates and signals: synthetic vs actual")
}

### OPTIONAL INDICATIVE TRADE LIST: DISABLED UNTIL FULL HOLDINGS ARE PROVIDED ###
if (run_trade_list) {
  if (is.null(names(current_shares)) || !all(c("Cash","TQQQ","TECL","IEF","GLD","SHY") %in% names(current_shares)) ||
      any(!is.finite(current_shares)) || any(current_shares < 0)) stop("Enter ALL holdings and Cash dollars")
  if (!is.finite(cash_inflow) || current_shares["Cash"]+cash_inflow < 0) stop("Invalid cash flow")
  # This is merely a current MODEL holding, not a live order; manually reconcile stop-outs.
  target <- as.character(last(actual$holding)); if (target == "Cash") target <- "Cash"
  names_now <- c("TQQQ","TECL","IEF","GLD","SHY")
  px_now <- setNames(vapply(names_now,function(s) as.numeric(last(Cl(raw[[s]]))),numeric(1)),names_now)
  nav_now <- current_shares["Cash"]+cash_inflow+sum(current_shares[names_now]*px_now)
  if (nav_now <= 0 || any(!is.finite(px_now))) stop("Invalid prices or account value")
  desired <- setNames(rep(0,length(names_now)),names_now)
  if (target != "Cash") desired[target] <- floor(nav_now/px_now[target])
  delta <- desired-current_shares[names_now]
  indicative_trades <- data.frame(ticker=names_now,action=ifelse(delta>0,"BUY",ifelse(delta<0,"SELL","HOLD")),
                                  shares=as.numeric(delta),reference_close=as.numeric(px_now))
  indicative_trades <- indicative_trades[indicative_trades$shares!=0,,drop=FALSE]
  cat("\nINDICATIVE ONLY: reconcile real holdings, stop-outs, quotes and execution before trading:\n")
  print(indicative_trades,row.names=FALSE)
}
# Objects retained: extended, actual, synthetic_matched, synthetic_own_matched,
# article_actual, article_actual_zero_cost, article_synthetic, signal_actual, signal_synthetic, P, P_syn, R.
