library(forecast)

# Model 1: auto.arima
fit1 <- auto.arima(train, ic = "aic", trace = TRUE)
summary(fit1)
checkresiduals(fit1)
fr1 <- forecast(fit1, h = h)
accuracy(fr1, test)
plot(fr1, main = "auto.arima")
lines(test, col = "turquoise2", lwd = 2)

# Model 2: SARIMA(0,1,1)(0,1,1)[12]
fit2 <- Arima(train, order = c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))
summary(fit2)
checkresiduals(fit2)
fr2 <- forecast(fit2, h = h)
accuracy(fr2, test)
plot(fr2, main = "SARIMA(0,1,1)(0,1,1)[12]")
lines(test, col = "turquoise2", lwd = 2)