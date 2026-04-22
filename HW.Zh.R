library(forecast)


# MODEL FITTING
fit_hw <- hw(train, seasonal = "additive", h = h,
              alpha = 0.02, beta = 0.005, gamma = 0.65)

autoplot(fit_hw$model)

# DIAGNOSIS CHECKING
summary(fit_hw)
checkresiduals(fit_hw)
ljung_hw <- Box.test(residuals(fit_hw),
                     lag = 12, type = "Ljung-Box")



hw_acc<- accuracy(fit_hw, test)
print(hw_acc)
plot(fit_hw, main = "HW Additive alpha = 0.02, beta = 0.005, gamma = 0.65",
     ylab = "Sales", xlab = "Year", fcol = "blue", flwd = 2)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast","Test"),
       col = c("blue","red"), lty = 1, lwd = 2, cex = 0.7)