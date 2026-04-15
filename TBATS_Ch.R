# 1. MODEL IDENTIFICATION
library(forecast)

tbats_model <- tbats(train)
tbats_model
summary(tbats_model)

# 2. MODEL FITTING
plot(tbats_model)

## Fitted values vs Actual
fitted_values <- fitted(tbats_model)

plot(train, main="Actual vs Fitted (TBATS Model)", ylab="Sales")
lines(fitted_values, col="blue")
legend("topleft", legend=c("Actual","Fitted"),
       col=c("black","blue"), lty=1)

accuracy(tbats_model)

# 3. DIAGNOSTIC CHECKING
## plot + ACF + histogram
checkresiduals(tbats_model)

## Ljung-Box test
lb_test <- Box.test(residuals(tbats_model), lag=12, type="Ljung-Box")
lb_test

mean_res <- mean(residuals(tbats_model))
mean_res

# 4. FORECASTING & EVALUATION
## Forecast for test period
forecast_tbats <- forecast(tbats_model, h = h)

## Plot forecast vs actual
plot(forecast_tbats, main="TBATS Forecast vs Actual")
lines(test, col="red")

legend("topleft", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=1)

acc_test <- accuracy(forecast_tbats, test)
acc_test

autoplot(forecast_tbats) +
  autolayer(test, series="Actual")

# 5. CHECK TRAIN VS TEST MAPE DIFFERENCE
acc_train <- accuracy(tbats_model)

train_mape <- acc_train["Training set","MAPE"]
test_mape  <- acc_test["Test set","MAPE"]
diff_mape  <- abs(train_mape - test_mape)

cat("Train MAPE:", train_mape, "\n")
cat("Test MAPE :", test_mape, "\n")
cat("MAPE Difference:", diff_mape, "\n")
cat("Ljung-Box p-value:", lb_test$p.value, "\n")