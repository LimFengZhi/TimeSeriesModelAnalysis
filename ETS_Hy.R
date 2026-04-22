#ETS
library(forecast)

# MODEL FITTING
fit_ets <- ets(train, model = "AAA",
            alpha = 0.05, beta = 0.01, gamma = 0.05)

autoplot(fit_ets)

# DIAGNOSIS CHECKING
summary(fit_ets)
checkresiduals(fit_ets)
ljung_ets <- Box.test(residuals(fit_ets),
                      lag = 12, type = "Ljung-Box")
# FORECAST EVALUATION
fc_ets <- forecast(fit_ets, h = h)
acc_ets <-accuracy(fc_ets, test)
print(acc_ets)

plot(fr1, main = "ETS(A,A,A)",
     ylab = "Sales", xlab = "Year", fcol = "blue", flwd = 2)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast","Test"),
       col = c("blue","red"), lty = 1, lwd = 2, cex = 0.7)