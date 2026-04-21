library(forecast)


# -------------------------------------------------------------

# -------------------------------------------------------------
# HW Variant 3:  alpha = 0.15, beta = 0.01, gamma = 0.80
# Train_RMSE=63.81  Test_RMSE=67.78  RMSE_Gap=3.98
# Train_MAPE=41.92  Test_MAPE=32.16  MAPE_Gap=9.76
# -------------------------------------------------------------
fit_hw3 <- hw(train, seasonal = "additive", h = h,
              alpha = 0.15, beta = 0.01, gamma = 0.80)
summary(fit_hw3)
checkresiduals(fit_hw3)
hw_acc<- accuracy(fit_hw3, test)
print(hw_acc)
plot(fit_hw3, main = "HW Additive  (α=0.15, β=0.01, γ=0.80)")
lines(test, col = "turquoise2", lwd = 2)