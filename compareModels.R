# FINAL MODEL COMPARISON

model_comparison <- data.frame(
  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "ETS(A,A,A) alpha=0.05 beta=0.01 gamma=0.05",
    "HW Additive alpha=0.02 beta=0.005 gamma=0.65",
    "TBATS (Trend + ARMA)",
    "Seasonal Naive"
  ),
  Train_RMSE = c(acc_arima ["Training set", "RMSE"],
                 acc_ets   ["Training set", "RMSE"],
                 acc_hw    ["Training set", "RMSE"],
                 acc_tbats ["Training set", "RMSE"],
                 acc_snaive["Training set", "RMSE"]),
  Test_RMSE  = c(acc_arima ["Test set", "RMSE"],
                 acc_ets   ["Test set", "RMSE"],
                 acc_hw    ["Test set", "RMSE"],
                 acc_tbats ["Test set", "RMSE"],
                 acc_snaive["Test set", "RMSE"]),
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

cat("\n============ 5-MODEL COMPARISON TABLE ============\n\n")
print(model_comparison, row.names = FALSE)