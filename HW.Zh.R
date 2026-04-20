library(forecast)


# -------------------------------------------------------------
# HW Variant 1:  alpha = 0.05, beta = 0.01, gamma = 0.95
# Train_RMSE=64.39  Test_RMSE=69.29  RMSE_Gap=4.90
# Train_MAPE=37.57  Test_MAPE=31.22  MAPE_Gap=6.35
# -------------------------------------------------------------
fit_hw1 <- hw(train, seasonal = "additive", h = h,
              alpha = 0.05, beta = 0.01, gamma = 0.95)
summary(fit_hw1)
checkresiduals(fit_hw1)
accuracy(fit_hw1, test)
plot(fit_hw1, main = "HW Additive  (α=0.05, β=0.01, γ=0.95)")
lines(test, col = "turquoise2", lwd = 2)

# -------------------------------------------------------------
# HW Variant 2:  alpha = 0.05, beta = 0.01, gamma = 0.80   ** best RMSE gap **
# Train_RMSE=61.40  Test_RMSE=63.96  RMSE_Gap=2.56
# Train_MAPE=39.49  Test_MAPE=30.53  MAPE_Gap=8.99
# -------------------------------------------------------------
fit_hw2 <- hw(train, seasonal = "additive", h = h,
              alpha = 0.05, beta = 0.01, gamma = 0.80)
summary(fit_hw2)
checkresiduals(fit_hw2)
accuracy(fit_hw2, test)
plot(fit_hw2, main = "HW Additive  (α=0.05, β=0.01, γ=0.80)")
lines(test, col = "turquoise2", lwd = 2)


# -------------------------------------------------------------
# HW Variant 3:  alpha = 0.15, beta = 0.01, gamma = 0.80
# Train_RMSE=63.81  Test_RMSE=67.78  RMSE_Gap=3.98
# Train_MAPE=41.92  Test_MAPE=32.16  MAPE_Gap=9.76
# -------------------------------------------------------------
fit_hw3 <- hw(train, seasonal = "additive", h = h,
              alpha = 0.15, beta = 0.01, gamma = 0.80)
summary(fit_hw3)
checkresiduals(fit_hw3)
accuracy(fit_hw3, test)
plot(fit_hw3, main = "HW Additive  (α=0.15, β=0.01, γ=0.80)")
lines(test, col = "turquoise2", lwd = 2)