library(forecast)

# Model: TBATS
fit_tbats <- tbats(train)
summary(fit_tbats)
checkresiduals(fit_tbats)
fr_tbats <- forecast(fit_tbats, h = h)
accuracy(fr_tbats, test)
plot(fr_tbats, main = "TBATS")
lines(test, col = "turquoise2", lwd = 2)