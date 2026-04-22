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

plot(fr1, main = "ETS(A,A,A) Forecast vs Actual",
     ylab = "Sales", xlab = "Year", fcol = "blue", flwd = 2)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast","Actual"),
       col = c("blue","red"), lty = 1, lwd = 2, cex = 0.7)