# Model
library(forecast)

# Model Fitting
hw_mul <- HoltWinters(train, seasonal="multiplicative")

# Diagnostic Checking
checkresiduals(hw_mul)

# Forecasting
f1 <- forecast(hw_mul, h=length(test))

# Forecast evaluation
accuracy(f1, test)

forecast_vals <- predict(hw_mul, n.ahead=length(test),prediction.interval=TRUE, level=0.95)

# Visualisation
plot(hw_mul, forecast_vals, ylim=c(100,620))
lines(test, col="turquoise2")