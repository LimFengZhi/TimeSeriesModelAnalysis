library(forecast)

# =========================
# 1. Model Fitting 
# =========================

# 1. Forced Trend
tbats_trend <- tbats(train, 
                     use.box.cox = FALSE, 
                     use.trend = TRUE, 
                     use.damped.trend = FALSE, 
                     seasonal.periods = c(12), 
                     use.arma.errors = TRUE)

# 2. Damped Trend
tbats_damped <- tbats(train, 
                      use.box.cox = FALSE, 
                      use.trend = TRUE, 
                      use.damped.trend = TRUE, 
                      seasonal.periods = c(12), 
                      use.arma.errors = TRUE)

# =========================
# 2. Forecast
# =========================
h <- length(test)

fc_trend  <- forecast(tbats_trend, h = h)
fc_damped <- forecast(tbats_damped, h = h)

# =========================
# 3. Accuracy Calculations
# =========================
acc_trend  <- accuracy(fc_trend, test)
acc_damped <- accuracy(fc_damped, test)

print(acc_trend)
print(acc_damped)

# =========================
# 4. Ljung-Box Test
# =========================
lb_trend  <- Box.test(residuals(tbats_trend), lag = 20, type = "Ljung-Box")
lb_damped <- Box.test(residuals(tbats_damped), lag = 20, type = "Ljung-Box")

# =========================
# 5. Model Comparison Table
# =========================
comparison <- data.frame(
  Model = c("TREND", "DAMPED"),
  
  RMSE = c(acc_trend["Test set", "RMSE"],
           acc_damped["Test set", "RMSE"]),
  
  MAE = c(acc_trend["Test set", "MAE"],
          acc_damped["Test set", "MAE"]),
  
  MAPE = c(acc_trend["Test set", "MAPE"],
           acc_damped["Test set", "MAPE"]),
  
  LjungBox_pvalue = c(lb_trend$p.value,
                      lb_damped$p.value)
)

print(comparison)

# =========================
# 6. Best Model Selection
# =========================
valid_models <- comparison[comparison$LjungBox_pvalue > 0.05, ]

if (nrow(valid_models) == 0) {
  best_model <- comparison[which.min(comparison$RMSE), ]
  cat("\nWARNING: Neither model passed Ljung-Box. Selecting fallback by min RMSE.\n")
} else {
  best_model <- valid_models[which.min(valid_models$RMSE), ]
}

cat("\nBEST MODEL:\n")
print(best_model)

# =========================
# 7. Plot Best Model
# =========================
if (best_model$Model == "TREND") {
  best_fc <- fc_trend
} else {
  best_fc <- fc_damped
}

plot(best_fc,
     main = paste("Best TBATS Model:", best_model$Model),
     xlab = "Time",
     ylab = "Sales Forecast")

lines(test, col = "red", lwd = 2)
legend("topleft",
       legend = c("Forecast", "Actual Test Data"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2)

cat("\n=== Winning Model Architecture Summary ===\n")
if(best_model$Model == "TREND") print(tbats_trend)
if(best_model$Model == "DAMPED") print(tbats_damped)