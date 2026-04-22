# FINAL MODEL COMPARISON
# Author: Lim Feng Zhi, Tham Zhen Hern, Tho Hui Yee, Ng Chiao Han

model_comparison <- data.frame(
  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "ETS(A,A,A) alpha=0.05 beta=0.01 gamma=0.05",
    "HW Additive alpha=0.02 beta=0.005 gamma=0.65",
    "TBATS (Trend + ARMA)",
    "Seasonal Naive"
  ),
  Train_MAPE = c(acc_arima ["Training set", "MAPE"],
                 acc_ets   ["Training set", "MAPE"],
                 acc_hw    ["Training set", "MAPE"],
                 acc_tbats ["Training set", "MAPE"],
                 acc_snaive["Training set", "MAPE"]),
  Test_MAPE  = c(acc_arima ["Test set", "MAPE"],
                 acc_ets   ["Test set", "MAPE"],
                 acc_hw    ["Test set", "MAPE"],
                 acc_tbats ["Test set", "MAPE"],
                 acc_snaive["Test set", "MAPE"]),
  LjungBox_p = c(ljung_arima $p.value,
                 ljung_ets   $p.value,
                 ljung_hw    $p.value,
                 ljung_tbats $p.value,
                 ljung_snaive$p.value)
)

# Add MAPE gap
model_comparison$MAPE_Gap <- abs(model_comparison$Test_MAPE - model_comparison$Train_MAPE)

# Round numeric columns for clean display
model_comparison$Train_MAPE <- round(model_comparison$Train_MAPE, 2)
model_comparison$Test_MAPE  <- round(model_comparison$Test_MAPE,  2)
model_comparison$MAPE_Gap   <- round(model_comparison$MAPE_Gap,   2)
model_comparison$LjungBox_p <- round(model_comparison$LjungBox_p, 4)

cat("\n============ 5-MODEL COMPARISON TABLE ============\n\n")
print(model_comparison, row.names = FALSE)