library(forecast)

fit_hw3 <- hw(train, seasonal = "additive", h = h,
              alpha = 0.02, beta = 0.005, gamma = 0.65)
summary(fit_hw3)
checkresiduals(fit_hw3)
hw_acc<- accuracy(fit_hw3, test)
print(hw_acc)
plot(fit_hw3, main = "HW Additive Forecast vs Actual",
     ylab = "Sales", xlab = "Year", fcol = "blue", flwd = 2)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast","Actual"),
       col = c("blue","red"), lty = 1, lwd = 2, cex = 0.7)