library(forecast)

fit <- auto.arima(train, ic = "aic", trace = TRUE)
summary(fit)
checkresiduals(fit)

fr <- forecast(fit, h = h)
plot(fr)
lines(test, col = "turquoise2")

accuracy(fr, test)

library(forecast)
library(tseries)
# Check number of differencing, need to define frequency for time series
ndiffs(train)
nsdiffs(train)
# Seasonal differencing
SDiff <- ts(diff(Y, lag=12))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot.ts(SDiff)
acf(SDiff)
pacf(SDiff)
adf.test(SDiff)

# Non-seasonal differencing
Diff <- ts(diff(SDiff, lag=1))
plot.ts(Diff)
acf(Diff)
pacf(Diff)
adf.test(Diff)

fit <- arima(train, order=c(0,1,1), seasonal=list(order =c(1,1,0), period=12))
summary(fit)
checkresiduals(fit)
fr <- forecast(fit, h=h)
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)


fit2 <- arima(train, order=c(2,1,2), seasonal=list(order =c(1,1,1), period=12))
summary(fit2)
checkresiduals(fit2)
fr <- forecast(fit2, h=h)
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)