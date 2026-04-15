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

fit <- arima(train, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12))
summary(fit)
checkresiduals(fit)
fr <- forecast(fit, h=h)
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)