library(forecast)

# Model: Seasonal Naive
fit_snaive <- snaive(train, h = h)
summary(fit_snaive)
checkresiduals(fit_snaive)
fr_snaive <- forecast(fit_snaive, h = h)
accuracy(fr_snaive, test)
plot(fr_snaive, main = "Seasonal Naive")
lines(test, col = "turquoise2", lwd = 2)
