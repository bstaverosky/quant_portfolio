# 33% static UPRO + candidate diversifying sleeves
# Compact, reproducible experiment; xts/PerformanceAnalytics; no dplyr/plyr.
# Actual ETF total returns already include fund fees/financing. Trading costs are added below.

rm(list=ls())
suppressPackageStartupMessages({library(quantmod); library(PerformanceAnalytics); library(xts); library(zoo); library(ftblog); library(FRAPO)})

### SETTINGS ##################################################################
START <- "1990-01-01"; UPRO_W <- .33; SLEEVE_W <- 1-UPRO_W; TC_BPS <- 5
BETA_WIN <- 252; BETA_MIN <- 126; BETA_CLIP <- c(.25,1.75); RF_FALLBACK <- .02
SYN_UPRO_ER <- .0091; SYN_UPRO_SWAP_SPREAD <- .0030; SYN_SH_ER <- .0089
MOMO_N <- 3; MOMO_LOOKBACK <- 120; MOMO_VOL <- 42
OUT <- "upro_33_sleeve_results.csv"

### HELPERS ###################################################################
get_ret <- function(x, from=START) {
  z <- suppressWarnings(getSymbols(x, src="yahoo", from=from, auto.assign=FALSE))
  r <- Return.calculate(Ad(z)); names(r) <- x; r
}
get_many <- function(x, from=START) Reduce(function(a,b) merge(a,b,all=TRUE), lapply(x,get_ret,from=from))
cc <- function(x) x[complete.cases(x),]
ann_turn <- function(w) { z <- .5*abs(w-lag(w)); as.numeric(sum(rowSums(z,na.rm=TRUE),na.rm=TRUE)/as.numeric(diff(range(index(w)))/365.25)) }
net_of_tc <- function(r,w,bps=TC_BPS) { cost <- .5*rowSums(abs(w-lag(w)),na.rm=TRUE)*bps/10000; out <- r-cost; names(out) <- names(r); out }
roll_beta <- function(x,mkt,win=BETA_WIN,minobs=BETA_MIN) {
  z <- merge(x,mkt,join="inner"); b <- rep(NA_real_,nrow(z))
  for(i in seq_len(nrow(z))) if(i>=minobs) { q <- z[max(1,i-win+1):i,]; b[i] <- cov(q[,1],q[,2])/var(q[,2]) }
  xts(pmin(pmax(b,BETA_CLIP[1]),BETA_CLIP[2]),index(z))
}
beta_hedge <- function(long,sh,mkt,label) {
  z <- cc(merge(long,sh,mkt,join="inner")); b <- lag(roll_beta(z[,1],z[,3])); ep <- endpoints(z,"months")
  bm <- b*NA; bm[ep[ep>0]] <- b[ep[ep>0]]; b <- na.locf(bm,na.rm=FALSE)
  wl <- 1/(1+b); ws <- b/(1+b); w <- merge(wl,ws); names(w) <- c("Long","SH")
  r <- z[,1]*wl+z[,2]*ws; r <- net_of_tc(r,w); names(r) <- label; list(r=r,w=w,b=b,turn=ann_turn(w))
}
scale_sleeve <- function(upro,sleeve,label) {
  z <- cc(merge(upro,sleeve,join="inner")); target <- c(UPRO_W,SLEEVE_W); w <- target; r <- z[,1]*NA; mon <- format(index(z),"%Y-%m")
  for(i in seq_len(nrow(z))) { cost <- 0; if(i>1 && mon[i]!=mon[i-1]) { cost <- .5*sum(abs(target-w))*TC_BPS/10000; w <- target }; r[i] <- sum(w*as.numeric(z[i,]))-cost; w <- w*(1+as.numeric(z[i,])); w <- w/sum(w) }
  names(r) <- label; r
}
capm_beta <- function(r,mkt) { z <- cc(merge(r,mkt,join="inner")); as.numeric(cov(z[,1],z[,2])/var(z[,2])) }
metrics <- function(R,mkt,turn=NULL) {
  f <- function(r) { r <- r[complete.cases(r)]; c(Start=as.numeric(format(first(index(r)),"%Y%m%d")),Years=as.numeric(diff(range(index(r)))/365.25),
    CAGR=as.numeric(Return.annualized(r,geometric=TRUE)),Vol=as.numeric(StdDev.annualized(r)),Sharpe=as.numeric(SharpeRatio.annualized(r,Rf=0)),
    MaxDD=as.numeric(maxDrawdown(r)),Beta=capm_beta(r,mkt),Turnover=if(is.null(turn)) NA_real_ else turn) }
  out <- t(sapply(seq_len(ncol(R)),function(i) f(R[,i]))); rownames(out) <- colnames(R); out
}
rank_table <- function(R,mkt,turn=NULL) { x <- metrics(R,mkt,turn); x[order(x[,"Sharpe"],decreasing=TRUE),] }

### ORIGINAL MOMENTUM BENCHMARK ################################################
.find_top <- function(x,n=5,type=c("relative","positive","above average")) {
  type <- match.arg(type); ok <- switch(type,relative=rep(TRUE,length(x)),positive=x>0,`above average`=x>mean(x,na.rm=TRUE)); head(order(x,decreasing=TRUE)[order(x,decreasing=TRUE)%in%which(ok)],n)
}
erc <- function(r,n=MOMO_VOL) { s <- cov(tail(r,n),use="pairwise.complete.obs"); o <- FRAPO::PERC(s,percentage=FALSE); as.numeric(FRAPO::Weights(o)) }
momo <- function(r,n_assets=MOMO_N,n_days=MOMO_LOOKBACK,n_vol=MOMO_VOL) {
  ep <- endpoints(r,"months"); ep <- ep[ep>n_days]; w <- r*NA
  for(i in ep) { x <- r[(i-n_days):i,]; j <- .find_top(apply(1+x,2,prod,na.rm=TRUE)-1,n_assets,"above average"); w[i,] <- 0; if(length(j)>=2) w[i,j] <- erc(x[,j],n_vol) }
  w <- na.locf(lag(w),na.rm=FALSE); ret <- xts(rowSums(r*w,na.rm=TRUE),index(r)); ret[rowSums(!is.na(w))==0] <- NA; names(ret) <- "Momentum"; list(r=ret,w=w,turn=ann_turn(w))
}

data(aaa_returns,package="ftblog")
base_tickers <- c("SPY","VGK","EWJ","EEM","ICF","RWX","IEF","TLT","DBC","GLD")
live <- get_many(base_tickers,"2023-12-29"); live <- live["2023-12-30/"]; live$Cash <- 0; live <- live[,c("Cash",base_tickers)]
hist <- aaa_returns; names(hist) <- c("Cash",base_tickers); all_base <- rbind(hist,live)
r_full <- all_base[,c("Cash","SPY","VGK","EEM","ICF","IEF","TLT","GLD")]; r_full$Cash <- 1e-12
mom <- momo(r_full); momentum <- mom$r

### ETF DATA, SYNTHETIC LEVERAGE, AND PROXIES ##################################
tickers <- c("UPRO","SH","QUAL","MTUM","VLUE","BTAL","KMLM","DBMF","MNA","SPHQ","PDP","RPV",
  "XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY","BIL")
px <- get_many(tickers); spy <- all_base$SPY; names(spy) <- "SPY"

# Daily financing proxy: prior-day 13-week T-bill yield if available, otherwise fixed fallback.
irx <- try(getSymbols("^IRX",src="yahoo",from=START,auto.assign=FALSE),silent=TRUE)
if(inherits(irx,"try-error")) rf <- spy*0+RF_FALLBACK/252 else { rf <- lag(na.locf(Cl(irx)/100,na.rm=FALSE))/252; rf <- merge(spy,rf,join="left")[,2]; rf <- na.locf(rf,na.rm=FALSE); rf[is.na(rf)] <- RF_FALLBACK/252 }
syn_upro <- 3*spy-2*rf-(SYN_UPRO_ER+SYN_UPRO_SWAP_SPREAD)/252; names(syn_upro) <- "UPRO_Synthetic"
syn_sh <- -spy-rf-(SYN_SH_ER/252); names(syn_sh) <- "SH_Synthetic"

# Longer proxy histories are sensitivity tests, not reconstructions: SPHQ~QUAL, PDP~MTUM, RPV~VLUE.
actual_upro <- px$UPRO; names(actual_upro) <- "UPRO_Actual"
control_actual <- scale_sleeve(actual_upro,momentum,"Control_Actual")
control_synth <- scale_sleeve(syn_upro,momentum,"Control_Synthetic")

### LAGGED SECTOR MOMENTUM ######################################################
sector_mom <- function(x,n=3,lookback=252) {
  x <- x[,c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")]; ep <- endpoints(x,"months"); ep <- ep[ep>lookback]; w <- x*NA
  for(i in ep) { score <- apply(1+x[(i-lookback+1):i,],2,prod,na.rm=TRUE)-1; j <- head(order(score,decreasing=TRUE),n); w[i,] <- 0; w[i,j] <- 1/n }
  w <- na.locf(lag(w),na.rm=FALSE); r <- xts(rowSums(x*w,na.rm=TRUE),index(x)); r[rowSums(!is.na(w))==0] <- NA; names(r) <- "SectorMom_Long"; r <- net_of_tc(r,w); list(r=r,w=w,turn=ann_turn(w))
}
sm <- sector_mom(px); smh <- beta_hedge(sm$r,px$SH,spy,"SectorMom_SH")

### ACTUAL CANDIDATE SLEEVES ####################################################
qh <- beta_hedge(px$QUAL,px$SH,spy,"QUAL_SH"); mh <- beta_hedge(px$MTUM,px$SH,spy,"MTUM_SH"); vh <- beta_hedge(px$VLUE,px$SH,spy,"VLUE_SH")
sleeves_actual <- merge(qh$r,mh$r,vh$r,smh$r,px$BTAL,px$KMLM,px$DBMF,px$MNA,momentum,join="outer")
names(sleeves_actual) <- c("QUAL_SH","MTUM_SH","VLUE_SH","SectorMom_SH","BTAL","KMLM","DBMF","MNA","Momentum")
ports_actual <- merge(control_actual,
  scale_sleeve(actual_upro,qh$r,"UPRO33_QUAL_SH"),scale_sleeve(actual_upro,mh$r,"UPRO33_MTUM_SH"),
  scale_sleeve(actual_upro,vh$r,"UPRO33_VLUE_SH"),scale_sleeve(actual_upro,smh$r,"UPRO33_SectorMom_SH"),
  scale_sleeve(actual_upro,px$BTAL,"UPRO33_BTAL"),scale_sleeve(actual_upro,px$KMLM,"UPRO33_KMLM"),
  scale_sleeve(actual_upro,px$DBMF,"UPRO33_DBMF"),scale_sleeve(actual_upro,px$MNA,"UPRO33_MNA"),join="outer")

### LONGER PROXY/SYNTHETIC SENSITIVITY #########################################
qp <- beta_hedge(px$SPHQ,syn_sh,spy,"SPHQ_SH_proxy"); mp <- beta_hedge(px$PDP,syn_sh,spy,"PDP_SH_proxy"); vp <- beta_hedge(px$RPV,syn_sh,spy,"RPV_SH_proxy")
sm_syn <- beta_hedge(sm$r,syn_sh,spy,"SectorMom_SH_synthetic")
ports_proxy <- merge(control_synth,scale_sleeve(syn_upro,qp$r,"SynUPRO33_SPHQ_SH"),scale_sleeve(syn_upro,mp$r,"SynUPRO33_PDP_SH"),
  scale_sleeve(syn_upro,vp$r,"SynUPRO33_RPV_SH"),scale_sleeve(syn_upro,sm_syn$r,"SynUPRO33_SectorMom_SH"),join="outer")

### RESULTS: EACH OWN HISTORY + FAIR COMMON PERIOD #############################
turn_actual <- c(Control_Actual=mom$turn,UPRO33_QUAL_SH=qh$turn,UPRO33_MTUM_SH=mh$turn,UPRO33_VLUE_SH=vh$turn,
  UPRO33_SectorMom_SH=smh$turn+sm$turn,UPRO33_BTAL=0,UPRO33_KMLM=0,UPRO33_DBMF=0,UPRO33_MNA=0)
own_actual <- rank_table(ports_actual,spy,turn_actual[colnames(ports_actual)])
common_actual <- cc(ports_actual); common_actual_stats <- rank_table(common_actual,spy)
own_proxy <- rank_table(ports_proxy,spy)
common_proxy <- cc(ports_proxy); common_proxy_stats <- rank_table(common_proxy,spy)

cat("\n===== ACTUAL ETFs: EACH AVAILABLE HISTORY (ranked by after-trading-cost Sharpe) =====\n"); print(round(own_actual,3))
cat("\n===== ACTUAL ETFs: STRICT COMMON HISTORY =====\n"); print(round(common_actual_stats,3))
cat("\n===== LONGER PROXY/SYNTHETIC: EACH AVAILABLE HISTORY =====\n"); print(round(own_proxy,3))
cat("\n===== LONGER PROXY/SYNTHETIC: STRICT COMMON HISTORY =====\n"); print(round(common_proxy_stats,3))

### MATCHED ACTUAL-vs-SYNTHETIC UPRO DIAGNOSTIC ###############################
u <- cc(merge(actual_upro,syn_upro)); names(u) <- c("Actual_UPRO","Synthetic_UPRO")
cat("\n===== UPRO MODEL: MATCHED PERIOD =====\n"); print(round(metrics(u,spy),3)); print(round(cor(u),3))

### SAVE FLAT RESULT TABLE + PLOTS #############################################
result <- rbind(data.frame(Sample="Actual_Own",Strategy=rownames(own_actual),own_actual,check.names=FALSE),
  data.frame(Sample="Actual_Common",Strategy=rownames(common_actual_stats),common_actual_stats,check.names=FALSE),
  data.frame(Sample="Proxy_Own",Strategy=rownames(own_proxy),own_proxy,check.names=FALSE),
  data.frame(Sample="Proxy_Common",Strategy=rownames(common_proxy_stats),common_proxy_stats,check.names=FALSE))
write.csv(result,OUT,row.names=FALSE)
charts.PerformanceSummary(common_actual,main="33% UPRO + 67% candidate sleeve: common actual history")
charts.PerformanceSummary(common_proxy,main="33% synthetic UPRO + 67% proxy sleeve: common proxy history")

cat("\nSaved:",normalizePath(OUT),"\n")
cat("\nInterpretation notes:\n",
  "1) Actual ETF adjusted returns are net of fund expenses and internal financing; do not subtract expense ratios again.\n",
  "2) TC_BPS applies to observable allocation turnover. Set it to your expected one-way spread/slippage.\n",
  "3) Betas, factor rankings, and portfolio weights are lagged. SH hedge weights update monthly.\n",
  "4) Synthetic UPRO charges 2x prior-day cash financing plus stated ER and an assumed swap spread.\n",
  "5) SPHQ/PDP/RPV are imperfect longer-history proxies for QUAL/MTUM/VLUE; treat only as robustness checks.\n",
  "6) KMLM, DBMF, BTAL, and MNA are tested only with actual returns; no unsupported synthetic history is invented.\n",
  "7) Sharpe uses Rf=0 because returns are total portfolio returns; change metrics() if you prefer excess-return Sharpe.\n",sep="")
