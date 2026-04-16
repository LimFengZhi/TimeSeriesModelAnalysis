library(forecast)

# Model fitting
tbats_model <- tbats(
  train,
  use.box.cox = FALSE,
  use.trend = TRUE,
  use.damped.trend = FALSE,
  seasonal.periods = c(12),
  use.arma.errors = TRUE
)

print("=== Model Summary ===")
print(tbats_model)

# Diagnostic checking
res_tbats <- residuals(tbats_model)

# Ljung-Box test
lb_test <- Box.test(res_tbats, lag = 8, fitdf = 0, type = "Ljung-Box")
print("=== Ljung-Box Test ===")
print(lb_test)

# Residual plot
checkresiduals(tbats_model)

# Forecast and evaluation
tbats_fc <- forecast(tbats_model, h = h)

# Plot forecast vs actual test data
plot(tbats_fc, main = "TBATS Forecast vs Actual Sales", ylab = "Sales", xlab = "Year")
lines(test, col = "red", lwd = 2)
legend("topleft",
       legend = c("Forecast", "Actual Test Data"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2)

# Accuracy metrics
accuracy_tbats <- accuracy(tbats_fc, test)

print("=== Forecast Evaluation Metrics ===")
print(accuracy_tbats)

# MAPE difference
train_mape <- accuracy_tbats["Training set", "MAPE"]
test_mape <- accuracy_tbats["Test set", "MAPE"]
mape_diff <- abs(train_mape - test_mape)

print("=== MAPE Check ===")
print(paste("Train MAPE:", round(train_mape, 2)))
print(paste("Test MAPE:", round(test_mape, 2)))
print(paste("Absolute MAPE Difference:", round(mape_diff, 2)))