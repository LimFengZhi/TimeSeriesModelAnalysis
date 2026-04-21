comparison <- data.frame(
  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "ETS(A,A,A) alpha=0.05 Beta=0.01 Gamma=0.95",
    "HW Additive alpha=0.15 Beta=0.01 Gamma=0.80",
    "TBATS (Box-Cox + Damped + ARMA)",
    "Seasonal Naive"
  ),
  Train_RMSE = c(arima_acc["Training set","RMSE"],
                 ets_acc   ["Training set","RMSE"],
                 hw_acc    ["Training set","RMSE"],
                 tbats_acc ["Training set","RMSE"],
                 snaive_acc["Training set","RMSE"]),
  Test_RMSE  = c(arima_acc["Test set","RMSE"],
                 ets_acc   ["Test set","RMSE"],
                 hw_acc    ["Test set","RMSE"],
                 tbats_acc ["Test set","RMSE"],
                 snaive_acc["Test set","RMSE"]),
  Train_MAPE = c(arima_acc["Training set","MAPE"],
                 ets_acc   ["Training set","MAPE"],
                 hw_acc    ["Training set","MAPE"],
                 tbats_acc ["Training set","MAPE"],
                 snaive_acc["Training set","MAPE"]),
  Test_MAPE  = c(arima_acc["Test set","MAPE"],
                 ets_acc   ["Test set","MAPE"],
                 hw_acc    ["Test set","MAPE"],
                 tbats_acc ["Test set","MAPE"],
                 snaive_acc["Test set","MAPE"])
)

cat("\n============ 5-MODEL COMPARISON TABLE ============\n\n")
print(comparison, row.names = FALSE)