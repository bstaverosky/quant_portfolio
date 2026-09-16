#remotes::install_github("joshuaulrich/ftblog")
rm(list=ls())

suppressPackageStartupMessages({
  library(ftblog)
  library(PerformanceAnalytics)
  library(FRAPO)
  library(quantmod)
  source("~/quant_portfolio/02_strategies/utils.R")
})

### PARAMETERS ###
use_cash <- TRUE
export_results <- FALSE

vol_short <- 20
vol_long <- 63
vol_normal_window <- 756
trend_sma <- 200

### FUNCTIONS ###
{
  strat_summary <- function(returns,original_results=NULL) {
    stats <- table.AnnualizedReturns(returns)
    stats <- rbind(stats,"Worst Drawdown"=-maxDrawdown(returns))
    if (!is.null(original_results)) {
      stats <- cbind(original_results,stats)
      colnames(stats)[1] <- "Original"
    }
    round(stats,3)
  }
  
  chart_performance <- function(R,title="Performance") {
    stopifnot(all(c("Replication","OOS") %in% colnames(R)))
    r <- R[,c("Replication","OOS")]
    p <- chart.CumReturns(r,main=title,main.timespan=FALSE,yaxis.right=TRUE)
    p <- addLegend("topleft",lty=1,lwd=1)
    p <- addSeries(r[,1],type="h",main="Return")
    p <- addSeries(r[,2],type="h",on=0,col="red")
    p <- addSeries(Drawdowns(r),main="Drawdown")
    p
  }
  
  .find_top_momo_columns <- function(returns,n_assets=5,type=c("relative","positive","above average")) {
    type <- match.arg(type)
    include_cols <- switch(type,"relative"=rep(TRUE,length(returns)),"positive"=returns>0,"above average"=returns>mean(returns,na.rm=TRUE))
    which_cols <- which(include_cols)
    
    if (length(which_cols)>0) {
      momo_rank <- order(returns,decreasing=TRUE)
      top_cols <- momo_rank[momo_rank %in% which_cols]
      top_cols <- head(top_cols,n_assets)
    } else top_cols <- integer()
    
    top_cols
  }
  
  portf_return_momo_equal_risk <- function(returns,n_assets=5,n_days=120,n_days_vol=60,momo_type=c("relative","positive","above average"),otype=c("returns","weights")) {
    month_end_i <- endpoints(returns,"months")
    month_end_i <- month_end_i[month_end_i>n_days]
    weights <- returns*NA
    momo_type <- match.arg(momo_type)
    
    for (i in month_end_i) {
      n_day_returns <- returns[(i-n_days):i,]
      momentum_returns <- apply(1+n_day_returns,2,prod)-1
      weights[i,] <- 0
      top_cols <- .find_top_momo_columns(momentum_returns,n_assets,momo_type)
      if (length(top_cols)>=2) weights[i,top_cols] <- portf_wts_equal_risk(n_day_returns[,top_cols],n_days_vol)
    }
    
    weights <- lag(weights)
    weights <- na.locf(weights)
    Rp <- xts(rowSums(returns*weights),index(returns),weights=weights)
    colnames(Rp) <- "R_momo_eq_risk"
    
    if (otype=="returns") Rp else weights
  }
  
  portf_return_momo_erc_brian <- function(returns,n_assets=5,n_days=120,n_days_vol=60,momo_type=c("relative","positive","above average"),otype=c("returns","weights")) {
    month_end_i <- endpoints(returns,"weeks")
    month_end_i <- month_end_i[month_end_i>n_days]
    weights <- returns*NA
    momo_type <- match.arg(momo_type)
    
    for (i in month_end_i) {
      n_day_returns <- returns[(i-n_days):i,]
      momentum_returns <- apply(1+n_day_returns,2,prod)-1
      weights[i,] <- 0
      top_cols <- .find_top_momo_columns(momentum_returns,n_assets,momo_type)
      if (length(top_cols)>=2) weights[i,top_cols] <- portf_wts_equal_risk(n_day_returns[,top_cols],n_days_vol)
    }
    
    weights <- lag(weights)
    weights <- na.locf(weights)
    Rp <- xts(rowSums(returns*weights),index(returns),weights=weights)
    colnames(Rp) <- "R_momo_eq_risk"
    
    if (otype=="returns") Rp else weights
  }
  
  port_wts_equal_risk <- function(returns,n_days_vol=60) {
    if (!requireNamespace("FRAPO",quietly=TRUE)) stop("please install the FRAPO package")
    n_day_returns <- last(returns,n_days_vol)
    sigma <- cov(n_day_returns)
    capture.output({ optim_portf <- FRAPO::PERC(sigma,percentage=FALSE) })
    FRAPO::Weights(optim_portf)
  }
  
  get_yahoo_prices <- function(tickers,from="1990-01-01") {
    x <- lapply(tickers,function(s) {
      print(s)
      px <- getSymbols(s,src="yahoo",from=from,auto.assign=FALSE,warnings=FALSE)
      px <- Ad(px)
      colnames(px) <- s
      px
    })
    do.call("merge",x)
  }
  
  get_yahoo_returns <- function(tickers,from="1990-01-01") {
    px <- get_yahoo_prices(tickers,from)
    px <- zoo::na.locf(px,na.rm=FALSE)
    Return.calculate(px)
  }
  
  xts_min <- function(x,y) {
    z <- merge(x,y,join="inner")
    out <- z[,1]
    out[] <- pmin(as.numeric(z[,1]),as.numeric(z[,2]))
    out
  }
  
  apply_leverage <- function(R,lev,name) {
    z <- merge(R,lev,join="inner")
    out <- z[,1]*z[,2]
    colnames(out) <- name
    out
  }
  
  apply_actual_leverage <- function(R3x,lev,name) {
    z <- merge(R3x,lev,join="inner")
    out <- z[,1]*(z[,2]/3)
    colnames(out) <- name
    out
  }
  
  make_vol_leverage <- function(R,short_n=20,long_n=63,normal_n=756,min_lev=1,max_lev=3) {
    v_short <- zoo::rollapply(R,short_n,sd,fill=NA,align="right")*sqrt(252)
    v_long <- zoo::rollapply(R,long_n,sd,fill=NA,align="right")*sqrt(252)
    
    v_ref <- v_short
    v_ref[] <- pmax(as.numeric(v_short),as.numeric(v_long))
    
    v_normal <- zoo::rollapply(v_ref,normal_n,median,fill=NA,align="right",na.rm=TRUE)
    
    lev <- v_ref
    lev[] <- max_lev*as.numeric(v_normal)/as.numeric(v_ref)
    lev[] <- pmin(max_lev,pmax(min_lev,as.numeric(lev)))
    
    colnames(lev) <- "Vol_Leverage"
    lag(lev,1)
  }
  
  make_breadth_cap <- function(returns,risk_assets=c("SPY","VGK","EEM","ICF"),sma_n=200) {
    r <- returns[,risk_assets]
    r[is.na(r)] <- 0
    
    px <- xts(apply(1+coredata(r),2,cumprod),index(r))
    colnames(px) <- risk_assets
    
    sma <- zoo::rollapply(px,sma_n,mean,fill=NA,align="right",by.column=TRUE)
    above <- px>sma
    
    breadth <- rowSums(coredata(above),na.rm=FALSE)
    spy_above <- as.logical(above[,"SPY"])
    
    cap <- rep(NA_real_,nrow(px))
    valid <- !is.na(breadth) & !is.na(spy_above)
    
    cap[valid] <- 2
    cap[valid & spy_above & breadth>=3] <- 3
    cap[valid & !spy_above & breadth<=1] <- 1
    
    cap <- xts(cap,index(px))
    colnames(cap) <- "Breadth_Cap"
    lag(cap,1)
  }
  
  make_canary_cap <- function(canary_prices,template) {
    ep <- endpoints(canary_prices,"months")
    ep <- ep[ep>0]
    pm <- canary_prices[ep,]
    x <- coredata(pm)
    
    score <- matrix(NA_real_,nrow=nrow(x),ncol=ncol(x))
    colnames(score) <- colnames(pm)
    
    for (i in 13:nrow(x)) {
      r1 <- x[i,]/x[i-1,]-1
      r3 <- x[i,]/x[i-3,]-1
      r6 <- x[i,]/x[i-6,]-1
      r12 <- x[i,]/x[i-12,]-1
      score[i,] <- (12*r1+4*r3+2*r6+r12)/19
    }
    
    bad <- rowSums(score<=0,na.rm=FALSE)
    cap <- rep(NA_real_,nrow(score))
    cap[bad==0] <- 3
    cap[bad==1] <- 2
    cap[bad==2] <- 1
    
    cap_me <- xts::xts(cap,order.by=index(pm))
    colnames(cap_me) <- "Canary_Cap"
    
    cap_daily <- merge(xts::xts(rep(NA_real_,nrow(template)),order.by=index(template)),cap_me,join="left")[,2]
    cap_daily <- zoo::na.locf(cap_daily,na.rm=FALSE)
    colnames(cap_daily) <- "Canary_Cap"
    
    lag(cap_daily,1)
  }
  
  make_actual_weights <- function(base_wts,lev) {
    z <- merge(base_wts,lev,join="inner")
    z <- z[complete.cases(z),]
    
    n <- ncol(base_wts)
    W <- coredata(z[,1:n])
    L <- as.numeric(z[,n+1])/3
    
    W <- W*L
    cash_col <- which(colnames(base_wts)=="Cash")
    W[,cash_col] <- W[,cash_col]+(1-L)
    
    out <- xts(W,index(z))
    colnames(out) <- colnames(base_wts)
    out
  }
  
  worst_rolling_return <- function(R,n) {
    R <- na.omit(R)
    if (nrow(R)<n) return(NA_real_)
    rr <- zoo::rollapply(R,n,function(x) prod(1+x)-1,fill=NA,align="right")
    min(rr,na.rm=TRUE)
  }
  
  strategy_stats <- function(R,lev_list) {
    out <- lapply(colnames(R),function(n) {
      z <- merge(R[,n],lev_list[[n]],join="inner")
      z <- z[complete.cases(z),]
      
      x <- z[,1]
      lev <- as.numeric(z[,2])
      
      c(
        CAGR=100*as.numeric(Return.annualized(x)),
        AnnVol=100*as.numeric(StdDev.annualized(x,scale=252)),
        Sharpe=as.numeric(SharpeRatio.annualized(x,Rf=0,scale=252)),
        MaxDD=-100*as.numeric(maxDrawdown(x)),
        Calmar=as.numeric(Return.annualized(x))/as.numeric(maxDrawdown(x)),
        WorstMonth=100*min(apply.monthly(x,Return.cumulative),na.rm=TRUE),
        Worst1D=100*min(x,na.rm=TRUE),
        Worst5D=100*worst_rolling_return(x,5),
        Worst20D=100*worst_rolling_return(x,20),
        AvgLev=mean(lev),
        PctLow=100*mean(lev<=1.25),
        PctMid=100*mean(lev>1.25 & lev<2.25),
        PctHigh=100*mean(lev>=2.25)
      )
    })
    
    out <- as.data.frame(do.call(rbind,out))
    rownames(out) <- colnames(R)
    round(out,2)
  }
  
  crisis_stats <- function(R) {
    crises <- list(
      DotCom=c("2000-03-24","2002-10-09"),
      GFC=c("2007-10-09","2009-03-09"),
      EuroCrisis2011=c("2011-04-29","2011-10-03"),
      Q4_2018=c("2018-09-20","2018-12-24"),
      COVID=c("2020-02-19","2020-03-23"),
      Bear2022=c("2022-01-03","2022-10-12")
    )
    
    out <- matrix(NA_real_,nrow=length(crises),ncol=ncol(R),dimnames=list(names(crises),colnames(R)))
    
    for (i in seq_along(crises)) {
      d <- crises[[i]]
      
      for (j in 1:ncol(R)) {
        x <- na.omit(R[paste0(d[1],"/",d[2]),j])
        
        if (nrow(x)>0) {
          first_date <- as.Date(index(x)[1])
          last_date <- as.Date(index(x)[nrow(x)])
          
          if (first_date<=as.Date(d[1])+7 && last_date>=as.Date(d[2])-7) out[i,j] <- 100*as.numeric(Return.cumulative(x))
        }
      }
    }
    
    round(as.data.frame(out),2)
  }
  
  implementation_gap_stats <- function(synthetic,actual) {
    out <- lapply(colnames(actual),function(n) {
      z <- merge(synthetic[,n],actual[,n],join="inner")
      z <- z[complete.cases(z),]
      
      syn <- as.numeric(Return.annualized(z[,1]))
      act <- as.numeric(Return.annualized(z[,2]))
      
      c(
        Synthetic_CAGR=100*syn,
        Actual_CAGR=100*act,
        Gap_Per_Year=100*(act-syn)
      )
    })
    
    out <- as.data.frame(do.call(rbind,out))
    rownames(out) <- colnames(actual)
    round(out,2)
  }
}

### LOAD BASE DATA ###
data(aaa_returns,package="ftblog")

etfs <- c("SPY","VGK","EWJ","EEM","ICF","RWX","IEF","TLT","DBC","GLD")
assets <- get_yahoo_returns(etfs)

asset_names <- c("SPY","VGK","EWJ","EEM","ICF","RWX","IEF","TLT","DBC","GLD")

if (use_cash) {
  returns <- aaa_returns
  assets$Cash <- 0
  assets <- assets[,c("Cash",asset_names)]
  asset_names <- c("Cash",asset_names)
} else {
  returns <- aaa_returns[,-1]
}

assets <- assets[(which(index(assets)=="2023-12-29")+1):nrow(assets),]
names(returns) <- asset_names

returns <- rbind(returns,assets)

r_full <- returns[,c("Cash","SPY","VGK","EEM","ICF","IEF","TLT","GLD")]
r_full$Cash <- 0.000000000001


### BASE MOMENTUM / ERC STRATEGY ###
strat_returns <- portf_return_momo_equal_risk(r_full,n_assets=3,n_days=120,n_days_vol=42,momo_type="above average",otype="returns")
strat_wts <- portf_return_momo_equal_risk(r_full,n_assets=3,n_days=120,n_days_vol=42,momo_type="above average",otype="weights")


### LEVERAGE SIGNAL A: CONSTANT 3X ###
lev_constant <- strat_returns*0+3
colnames(lev_constant) <- "Constant_3x"


### LEVERAGE SIGNAL B: VOLATILITY TARGET ###
lev_vol <- make_vol_leverage(strat_returns,short_n=vol_short,long_n=vol_long,normal_n=vol_normal_window,min_lev=1,max_lev=3)
colnames(lev_vol) <- "Vol_Target"


### LEVERAGE SIGNAL C: TREND + BREADTH REGIME ###
lev_breadth <- make_breadth_cap(returns,risk_assets=c("SPY","VGK","EEM","ICF"),sma_n=trend_sma)
lev_breadth <- lev_breadth[index(strat_returns)]
colnames(lev_breadth) <- "Trend_Breadth"


### LEVERAGE SIGNAL D: VOL TARGET + TREND/BREADTH CAP ###
lev_vol_breadth <- xts_min(lev_vol,lev_breadth)
colnames(lev_vol_breadth) <- "Vol_Trend_Breadth"


### LEVERAGE SIGNAL E: VOL TARGET + DAA CANARY CAP ###
# DAA canaries: EEM + AGG used as longer-history proxies for VWO + BND
canary_prices <- get_yahoo_prices(c("EEM","AGG"),from="1990-01-01")
lev_canary <- make_canary_cap(canary_prices,r_full)
lev_canary <- lev_canary[index(strat_returns)]
colnames(lev_canary) <- "DAA_Canary"

lev_vol_canary <- xts_min(lev_vol,lev_canary)
colnames(lev_vol_canary) <- "Vol_DAA_Canary"


### STORE ALL LEVERAGE RULES ###
lev_list <- list(
  Constant_3x=lev_constant,
  Vol_Target=lev_vol,
  Trend_Breadth=lev_breadth,
  Vol_Trend_Breadth=lev_vol_breadth,
  Vol_DAA_Canary=lev_vol_canary
)


### SYNTHETIC BACKTESTS ###
synthetic_variants <- merge(
  apply_leverage(strat_returns,lev_constant,"Constant_3x"),
  apply_leverage(strat_returns,lev_vol,"Vol_Target"),
  apply_leverage(strat_returns,lev_breadth,"Trend_Breadth"),
  apply_leverage(strat_returns,lev_vol_breadth,"Vol_Trend_Breadth"),
  apply_leverage(strat_returns,lev_vol_canary,"Vol_DAA_Canary")
)


### ACTUAL 3X ETF BACKTESTS ###
# SPY -> UPRO
# VGK -> EURL
# EEM -> EDC
# ICF -> DRN
# IEF -> TYD
# TLT -> TMF
# GLD -> SHNY

leveraged_etfs <- c("UPRO","EURL","EDC","DRN","TYD","TMF","SHNY")
actual_names <- c("Cash",leveraged_etfs)

leveraged_returns <- get_yahoo_returns(leveraged_etfs,from="1990-01-01")
leveraged_returns$Cash <- 0
leveraged_returns <- leveraged_returns[,actual_names]

leveraged_wts <- strat_wts
colnames(leveraged_wts) <- actual_names

W <- leveraged_wts
R <- leveraged_returns

colnames(W) <- paste0("W_",actual_names)
colnames(R) <- paste0("R_",actual_names)

actual_data <- merge(W,R,join="inner")
actual_data <- actual_data[complete.cases(actual_data),]

actual_wts <- actual_data[,paste0("W_",actual_names)]
actual_rets <- actual_data[,paste0("R_",actual_names)]

colnames(actual_wts) <- actual_names
colnames(actual_rets) <- actual_names

actual_3x_base <- xts(rowSums(coredata(actual_wts)*coredata(actual_rets)),index(actual_data))
colnames(actual_3x_base) <- "Actual_3x_Base"

actual_variants <- merge(
  apply_actual_leverage(actual_3x_base,lev_constant,"Constant_3x"),
  apply_actual_leverage(actual_3x_base,lev_vol,"Vol_Target"),
  apply_actual_leverage(actual_3x_base,lev_breadth,"Trend_Breadth"),
  apply_actual_leverage(actual_3x_base,lev_vol_breadth,"Vol_Trend_Breadth"),
  apply_actual_leverage(actual_3x_base,lev_vol_canary,"Vol_DAA_Canary")
)


### COMMON-SAMPLE PERFORMANCE ###
synthetic_common <- synthetic_variants[complete.cases(synthetic_variants),]
actual_common <- actual_variants[complete.cases(actual_variants),]

cat("\nSYNTHETIC COMMON PERIOD:\n")
print(c(start=as.character(start(synthetic_common)),end=as.character(end(synthetic_common))))

cat("\nACTUAL ETF COMMON PERIOD:\n")
print(c(start=as.character(start(actual_common)),end=as.character(end(actual_common))))


### PERFORMANCE TABLES ###
synthetic_summary <- strategy_stats(synthetic_common,lev_list)
actual_summary <- strategy_stats(actual_common,lev_list)

cat("\nSYNTHETIC LEVERAGE STRATEGIES:\n")
print(synthetic_summary)

cat("\nACTUAL 3X ETF LEVERAGE STRATEGIES:\n")
print(actual_summary)


### CRISIS PERFORMANCE ###
# Uses each strategy's available synthetic history individually.
# DAA will show NA where EEM/AGG history is unavailable.

crisis_table <- crisis_stats(synthetic_variants)

cat("\nCRISIS RETURNS (%):\n")
print(crisis_table)


### ACTUAL ETF IMPLEMENTATION GAP ###
implementation_gap <- implementation_gap_stats(synthetic_variants,actual_variants)

cat("\nACTUAL ETF MINUS SYNTHETIC IMPLEMENTATION:\n")
print(implementation_gap)


### LEVERAGE SIGNAL CHART ###
leverage_chart <- merge(lev_constant,lev_vol,lev_breadth,lev_vol_breadth,lev_vol_canary)
chart.TimeSeries(leverage_chart,main="Strategy Leverage Through Time",legend.loc="bottomleft",yaxis.right=TRUE)


### SYNTHETIC PERFORMANCE CHART ###
charts.PerformanceSummary(synthetic_common,main="Synthetic Leverage Regimes")


### ACTUAL ETF PERFORMANCE CHART ###
charts.PerformanceSummary(actual_common,main="Actual 3x ETF Leverage Regimes")


### FOCUS: CONSTANT 3X VS COMBINED VOL + BREADTH ###
focus_synthetic <- synthetic_common[,c("Constant_3x","Vol_Trend_Breadth")]
charts.PerformanceSummary(focus_synthetic,main="Constant 3x vs Vol + Trend/Breadth")


### FOCUS: CONSTANT 3X VS COMBINED VOL + CANARY ###
focus_canary <- synthetic_common[,c("Constant_3x","Vol_DAA_Canary")]
charts.PerformanceSummary(focus_canary,main="Constant 3x vs Vol + DAA Canary")


### ACTUAL IMPLEMENTABLE CAPITAL WEIGHTS ###
# Example: desired leverage = 2x means 2/3 in the 3x ETF basket + 1/3 idle cash.

actual_weight_list <- lapply(names(lev_list),function(n) make_actual_weights(leveraged_wts,lev_list[[n]]))
names(actual_weight_list) <- names(lev_list)


### OPTIONAL EXPORT ###
if (export_results) {
  for (n in names(lev_list)) {
    
    syn_ret <- synthetic_variants[,n]
    syn_wts <- actual_weight_list[[n]]
    syn_data <- merge(syn_ret,syn_wts,join="inner")
    syn_data <- syn_data[complete.cases(syn_data),]
    
    export_strategy_output(
      strategy_name=paste0("Josh_",n,"_Synthetic"),
      returns_xts=syn_data[,1],
      weights_xts=syn_data[,-1],
      output_dir="/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
    )
    
    act_ret <- actual_variants[,n]
    act_wts <- actual_weight_list[[n]]
    act_data <- merge(act_ret,act_wts,join="inner")
    act_data <- act_data[complete.cases(act_data),]
    
    export_strategy_output(
      strategy_name=paste0("Josh_",n,"_Actual"),
      returns_xts=act_data[,1],
      weights_xts=act_data[,-1],
      output_dir="/home/brian/quant_portfolio/03_portfolio_aggregation/strategy_outputs"
    )
  }
}