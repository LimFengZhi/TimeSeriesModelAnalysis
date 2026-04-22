library(forecast)

# MODEL FITTING
fit_snaive <- snaive(train, h = h)
summary(fit_snaive)

# Fitted vs Actual 
plot(train, main = "SNaive Fitted vs Training Data",
     ylab = "Sales", xlab = "Year")
lines(fitted(fit_snaive), col = "blue", lwd = 2)
legend("topleft",
       legend = c("Actual", "SNaive Fitted"),
       col = c("black", "blue"),
       lty = 1, lwd = 2, cex = 0.7)

# DIAGNOSTIC CHECKING (3-in-1 plot)
checkresiduals(fit_snaive, lag = 12)

# Ljung-Box Test
res <- na.omit(residuals(fit_snaive))
ljung_snaive <- Box.test(res, lag = 12, type = "Ljung-Box")
print(ljung_snaive)

# FORECAST EVALUATION
plot(fit_snaive, main = "SNaive Forecast vs Actual",
     ylab = "Sales", xlab = "Year",
     fcol = "blue", flwd = 2)

lines(test, col = "red", lwd = 2)

legend("topleft",
       legend = c("Forecast", "Actual"),
       col = c("blue", "red"),
       lty = 1, lwd = 2, cex = 0.7)

snaive_acc <- accuracy(fit_snaive, test)
print(snaive_acc)
