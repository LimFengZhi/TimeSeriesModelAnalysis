#ETS
library(forecast)
fit1 <- ets(train, model = "AAA",
            alpha = 0.05, beta = 0.01, gamma = 0.05)
plot(train, main = "Actual vs Fitted (ETS Model)", ylab = "Value", xlab = "Time")
lines(fitted_values, col = "blue", lwd = 2)
summary(fit1)
checkresiduals(fit1)
fr1 <- forecast(fit1, h = h)
ets_acc <-accuracy(fr1, test)
print(ets_acc)
plot(fr1, main = "ETS(A,A,A)")
lines(test, col = "turquoise2", lwd = 2)