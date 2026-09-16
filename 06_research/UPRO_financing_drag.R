library(quantmod)
library(PerformanceAnalytics)

# ============================================================
# 1. Download SPY and UPRO
# ============================================================

getSymbols(
  c("SPY", "UPRO"),
  src = "yahoo",
  from = "2010-01-01",
  auto.assign = TRUE
)

# Use adjusted prices
prices <- merge(
  SPY = Ad(SPY),
  UPRO = Ad(UPRO)
)

prices <- na.omit(prices)


# ============================================================
# 2. Calculate daily returns
# ============================================================

returns <- Return.calculate(
  prices,
  method = "discrete"
)

returns <- na.omit(returns)

colnames(returns) <- c("SPY", "UPRO")


# ============================================================
# 3. Create hypothetical frictionless 3x SPY return
# ============================================================

SPY_3x <- 3 * returns[, "SPY"]


# ============================================================
# 4. Calculate daily UPRO implementation drag
#
# Positive number means:
# 3x SPY outperformed UPRO that day
# ============================================================

drag <- SPY_3x - returns[, "UPRO"]

analysis <- merge(
  returns,
  SPY_3x,
  drag
)

colnames(analysis) <- c(
  "SPY",
  "UPRO",
  "SPY_3x",
  "Drag"
)


# ============================================================
# 5. Average daily and annualized implementation drag
# ============================================================

daily_drag <- mean(analysis[, "Drag"])

annual_drag <- daily_drag * 252

cat(
  "Average daily drag:",
  round(daily_drag * 10000, 4),
  "basis points\n"
)

cat(
  "Annualized implementation drag:",
  round(annual_drag * 100, 2),
  "%\n"
)


# ============================================================
# 6. Crude implied financing rate
#
# A 3x ETF needs roughly 2x additional exposure.
#
# Therefore:
#
# financing rate ~= total drag / 2
#
# This includes expense ratio and other implementation costs.
# ============================================================

implied_rate_raw <- annual_drag / 2

cat(
  "Raw implied financing rate:",
  round(implied_rate_raw * 100, 2),
  "%\n"
)


# ============================================================
# 7. Estimate financing after removing expense ratio
# ============================================================

UPRO_expense_ratio <- 0.0089

non_expense_drag <-
  annual_drag - UPRO_expense_ratio

implied_financing_rate <-
  non_expense_drag / 2

cat(
  "Implied financing rate after removing expense ratio:",
  round(implied_financing_rate * 100, 2),
  "%\n"
)


# ============================================================
# 8. Regression approach
#
# UPRO_t = alpha + beta * SPY_t + error
#
# Beta should be approximately 3.
# Alpha should generally be negative.
# ============================================================

regression_data <- data.frame(
  SPY = as.numeric(analysis[, "SPY"]),
  UPRO = as.numeric(analysis[, "UPRO"])
)

model <- lm(
  UPRO ~ SPY,
  data = regression_data
)

print(summary(model))

alpha_daily <- coef(model)[1]
beta <- coef(model)[2]

alpha_annual <- alpha_daily * 252

cat("\nRegression results:\n")

cat(
  "Beta:",
  round(beta, 4),
  "\n"
)

cat(
  "Annualized alpha:",
  round(alpha_annual * 100, 2),
  "%\n"
)

cat(
  "Implied financing rate from regression:",
  round(
    (-alpha_annual - UPRO_expense_ratio) / 2 * 100,
    2
  ),
  "%\n"
)


# ============================================================
# 9. Calculate results separately for each calendar year
# ============================================================

dates <- index(analysis)
years <- format(dates, "%Y")

unique_years <- unique(years)

annual_results <- data.frame(
  Year = unique_years,
  Days = NA,
  Average_Daily_Drag = NA,
  Annualized_Drag = NA,
  Implied_Financing_Rate = NA
)

for (i in seq_along(unique_years)) {
  
  yr <- unique_years[i]
  
  idx <- which(years == yr)
  
  year_drag <- as.numeric(
    analysis[idx, "Drag"]
  )
  
  avg_daily_drag <- mean(
    year_drag,
    na.rm = TRUE
  )
  
  ann_drag <- avg_daily_drag * 252
  
  implied_financing <-
    (ann_drag - UPRO_expense_ratio) / 2
  
  annual_results$Days[i] <-
    length(year_drag)
  
  annual_results$Average_Daily_Drag[i] <-
    avg_daily_drag
  
  annual_results$Annualized_Drag[i] <-
    ann_drag
  
  annual_results$Implied_Financing_Rate[i] <-
    implied_financing
}


# ============================================================
# 10. Display annual results as percentages
# ============================================================

annual_results$Annualized_Drag_Pct <-
  annual_results$Annualized_Drag * 100

annual_results$Implied_Financing_Rate_Pct <-
  annual_results$Implied_Financing_Rate * 100

print(
  annual_results[
    ,
    c(
      "Year",
      "Days",
      "Annualized_Drag_Pct",
      "Implied_Financing_Rate_Pct"
    )
  ]
)


# ============================================================
# 11. Plot implied financing rate by year
# ============================================================

plot(
  as.numeric(annual_results$Year),
  annual_results$Implied_Financing_Rate_Pct,
  type = "b",
  xlab = "Year",
  ylab = "Implied Financing Rate (%)",
  main = "UPRO Implied Financing Rate"
)

abline(h = 0)


# ============================================================
# 12. Plot total UPRO drag by year
# ============================================================

plot(
  as.numeric(annual_results$Year),
  annual_results$Annualized_Drag_Pct,
  type = "b",
  xlab = "Year",
  ylab = "Annualized Drag (%)",
  main = "UPRO vs. 3x SPY: Annual Implementation Drag"
)

abline(h = 0)

returns$SPY3x <- returns$SPY * 3
charts.PerformanceSummary(returns)
Return.annualized(returns)
