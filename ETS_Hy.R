library(forecast)

# 1. MODEL FITTING
ets_model <- ets(train, model="AAA") 
summary(ets_model)

# 2. DIAGNOSTIC CHECKING
checkresiduals(ets_model)

# 3. FORECASTING
ets_forecast <- forecast(ets_model, h = h)
plot(ets_forecast, main="ETS Forecast vs Actual Test Data", ylab="Sales", xlab="Year")
lines(test, col="red", lwd=2) # Overlay the actual test data in red
legend("topleft", legend=c("Forecast", "Actual Test Data"), col=c("blue", "red"), lty=1, lwd=2)


# 4. FORECAST EVALUATION (Accuracy & MAPE)
# Evaluate the forecast against the test dataset
accuracy_results <- accuracy(ets_forecast, test)
print(accuracy_results)

test_mape <- accuracy_results["Test set", "MAPE"]
cat("\nTest Set MAPE:", round(test_mape, 2), "%\n")