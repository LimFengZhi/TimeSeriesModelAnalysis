library(forecast)

# MODEL FITTING
fit_tbats <- tbats(train,
                   use.box.cox       = FALSE,
                   use.trend         = TRUE,
                   use.damped.trend  = FALSE,
                   use.arma.errors   = TRUE,
                   seasonal.periods  = c(12, 6))

# DIAGNOSIS CHECKING
summary(fit_tbats)
checkresiduals(fit_tbats)
ljung_tbats <- Box.test(residuals(fit_tbats), lag =12, type = "Ljung-Box")

# FORECAST EVALUATION
fc_tbats  <- forecast(fit_tbats, h = h)
acc_tbats <- accuracy(fc_tbats, test)
print(tbats_acc)

plot(fr_tbats, main = "TBATS (Trend + ARMA)",
     ylab = "Sales", xlab = "Year", fcol = "blue", flwd = 2)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast","Test"),
       col = c("blue","red"), lty = 1, lwd = 2, cex = 0.7)