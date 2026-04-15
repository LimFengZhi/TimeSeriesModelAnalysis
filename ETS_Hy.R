# 1. MODEL IDENTIFICATION
library(forecast)

ets_model <- ets(train)
ets_model
summary(ets_model) # Show model parameters

# 2. MODEL FITTING
plot(ets_model)

## Fitted values vs Actual
fitted_values <- fitted(ets_model)

plot(train, main="Actual vs Fitted (ETS Model)", ylab="Sales")
lines(fitted_values, col="blue")
legend("topleft", legend=c("Actual","Fitted"),
       col=c("black","blue"), lty=1)

accuracy(ets_model)

# 3. DIAGNOSTIC CHECKING
## plot + ACF + histogram
checkresiduals(ets_model)

## Ljung-Box test
Box.test(residuals(ets_model), lag=20, type="Ljung-Box")

mean(residuals(ets_model))

# 4. FORECASTING & EVALUATION
## Forecast for test period
forecast_ets <- forecast(ets_model, h = h)

## Plot forecast vs actual
plot(forecast_ets, main="ETS Forecast vs Actual")
lines(test, col="red")

legend("topleft", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=1)

accuracy(forecast_ets, test)

autoplot(forecast_ets) +
  autolayer(test, series="Actual")