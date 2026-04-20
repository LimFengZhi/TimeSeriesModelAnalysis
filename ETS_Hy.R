library(forecast)

# =========================
# 1. Model Fitting 
# =========================
ets_auto <- ets(train)

ets_AAA <- ets(train, model = "AAA")
ets_AAN <- ets(train, model = "AAN")
ets_ANN <- ets(train, model = "ANN")

# =========================
# 2. Forecast
# =========================
h <- length(test)

fc_auto <- forecast(ets_auto, h = h)
fc_AAA  <- forecast(ets_AAA, h = h)
fc_AAN  <- forecast(ets_AAN, h = h)
fc_ANN  <- forecast(ets_ANN, h = h)

# =========================
# 3. Accuracy
# =========================
acc_auto <- accuracy(fc_auto, test)
print(acc_auto)
acc_AAA  <- accuracy(fc_AAA, test)
print(acc_AAA)
acc_AAN  <- accuracy(fc_AAN, test)
print(acc_AAN)
acc_ANN  <- accuracy(fc_ANN, test)
print(acc_ANN)

# =========================
# 4. Ljung-Box Test
# =========================
lb_auto <- Box.test(residuals(ets_auto), lag = 20, type = "Ljung-Box")
lb_AAA  <- Box.test(residuals(ets_AAA), lag = 20, type = "Ljung-Box")
lb_AAN  <- Box.test(residuals(ets_AAN), lag = 20, type = "Ljung-Box")
lb_ANN  <- Box.test(residuals(ets_ANN), lag = 20, type = "Ljung-Box")

# =========================
# 5. Model Comparison Table
# =========================
comparison <- data.frame(
  Model = c("AUTO", "AAA", "AAN", "ANN"),
  
  RMSE = c(acc_auto["Test set", "RMSE"],
           acc_AAA["Test set", "RMSE"],
           acc_AAN["Test set", "RMSE"],
           acc_ANN["Test set", "RMSE"]),
  
  MAE = c(acc_auto["Test set", "MAE"],
          acc_AAA["Test set", "MAE"],
          acc_AAN["Test set", "MAE"],
          acc_ANN["Test set", "MAE"]),
  
  MAPE = c(acc_auto["Test set", "MAPE"],
           acc_AAA["Test set", "MAPE"],
           acc_AAN["Test set", "MAPE"],
           acc_ANN["Test set", "MAPE"]),
  
  LjungBox_pvalue = c(lb_auto$p.value,
                      lb_AAA$p.value,
                      lb_AAN$p.value,
                      lb_ANN$p.value)
)

print(comparison)

# =========================
# 6. Best Model
# =========================
valid_models <- comparison[comparison$LjungBox_pvalue > 0.05, ]

if (nrow(valid_models) == 0) {
  best_model <- comparison[which.min(comparison$RMSE), ]
} else {
  best_model <- valid_models[which.min(valid_models$RMSE), ]
}

cat("\nBEST MODEL:\n")
print(best_model)

# =========================
# 7. Plot Best Model
# =========================

if (best_model$Model == "AUTO") {
  best_fc <- fc_auto
} else if (best_model$Model == "AAA") {
  best_fc <- fc_AAA
} else if (best_model$Model == "AAN") {
  best_fc <- fc_AAN
} else {
  best_fc <- fc_ANN
}

plot(best_fc,
     main = paste("Best ETS Model:", best_model$Model),
     xlab = "Time",
     ylab = "Forecast")

lines(test, col = "red")
legend("topleft",
       legend = c("Forecast", "Actual"),
       col = c("blue", "red"),
       lty = 1)