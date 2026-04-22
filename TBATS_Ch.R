library(forecast)

# TBATS (tuned)
fit_tbats <- tbats(train,
                   use.box.cox       = FALSE,
                   use.trend         = TRUE,
                   use.damped.trend  = FALSE,
                   use.arma.errors   = TRUE,
                   seasonal.periods  = c(12, 6))

summary(fit_tbats)
checkresiduals(fit_tbats)

fr_tbats  <- forecast(fit_tbats, h = h)
tbats_acc <- accuracy(fr_tbats, test)
print(tbats_acc)

plot(fr_tbats, main = "TBATS Forecast vs Actual",
     ylab = "Sales", xlab = "Year", fcol = "blue", flwd = 2)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast","Actual"),
       col = c("blue","red"), lty = 1, lwd = 2, cex = 0.7)