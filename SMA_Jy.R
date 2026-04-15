library(forecast)
library(TTR)

plot(train, main="Monthly Sales - Training Data",
     ylab="Sales", xlab="Year")

components <- decompose(train, type="additive")
plot(components)

acf(train,  lag.max=36, main="ACF of Training Data")
pacf(train, lag.max=36, main="PACF of Training Data")

sma_3  <- SMA(train, n=3)
sma_6  <- SMA(train, n=6)
sma_12 <- SMA(train, n=12)

plot(train, main="SMA Comparison on Training Data",
     ylab="Sales", xlab="Year", col="black", lwd=1.5)
lines(ts(sma_3,  frequency=12, start=start(train)), col="blue",  lwd=2)
lines(ts(sma_6,  frequency=12, start=start(train)), col="green", lwd=2)
lines(ts(sma_12, frequency=12, start=start(train)), col="red",   lwd=2)
legend("topleft",
       legend=c("Actual", "SMA(3)", "SMA(6)", "SMA(12)"),
       col=c("black","blue","green","red"),
       lty=1, lwd=2, cex=0.7)

fitted_12 <- ts(na.omit(sma_12), frequency=12, start=c(2015,12))
train_12  <- window(train, start=c(2015,12))

error_3  <- na.omit(as.numeric(train) - as.numeric(sma_3))
error_6  <- na.omit(as.numeric(train) - as.numeric(sma_6))
error_12 <- na.omit(as.numeric(train) - as.numeric(sma_12))

plot(error_3,  type="l", main="Residuals - SMA(3)",  ylab="Residuals")
abline(h=0, col="red", lty=2)

plot(error_6,  type="l", main="Residuals - SMA(6)",  ylab="Residuals")
abline(h=0, col="red", lty=2)

plot(error_12, type="l", main="Residuals - SMA(12)", ylab="Residuals")
abline(h=0, col="red", lty=2)

acf(error_3,  main="ACF of Residuals - SMA(3)")
acf(error_6,  main="ACF of Residuals - SMA(6)")
acf(error_12, main="ACF of Residuals - SMA(12)")

ljung_3  <- Box.test(error_3,  lag=12, type="Ljung-Box")
ljung_6  <- Box.test(error_6,  lag=12, type="Ljung-Box")
ljung_12 <- Box.test(error_12, lag=12, type="Ljung-Box")

get_sma_forecast <- function(train, test, h, n) {
  smoothed  <- SMA(train, n=n)
  last_sma  <- tail(na.omit(smoothed), 1)
  predicted <- ts(rep(last_sma, h),
                  frequency=12,
                  start=start(test))
  return(predicted)
}

f_3  <- get_sma_forecast(train, test, h, n=3)
f_6  <- get_sma_forecast(train, test, h, n=6)
f_12 <- get_sma_forecast(train, test, h, n=12)

plot(train, main="SMA(3) Forecast vs Actual",
     xlim=c(2015, 2020), ylim=c(0, 400), ylab="Sales")
lines(test, col="red",  lwd=2)
lines(f_3,  col="blue", lwd=2, lty=2)
legend("bottomright", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=c(2,1), lwd=2, cex=0.7)

plot(train, main="SMA(6) Forecast vs Actual",
     xlim=c(2015, 2020), ylim=c(0, 400), ylab="Sales")
lines(test, col="red",  lwd=2)
lines(f_6,  col="blue", lwd=2, lty=2)
legend("bottomright", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=c(2,1), lwd=2, cex=0.7)

plot(train, main="SMA(12) Forecast vs Actual",
     xlim=c(2015, 2020), ylim=c(0, 400), ylab="Sales")
lines(test, col="red",  lwd=2)
lines(f_12, col="blue", lwd=2, lty=2)
legend("bottomright", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=c(2,1), lwd=2, cex=0.7)

test_acc  <- accuracy(f_12, test)
train_acc <- accuracy(fitted_12, train_12)

mape_train <- train_acc[,"MAPE"]
mape_test  <- test_acc[,"MAPE"]
mape_diff  <- abs(mape_train - mape_test)

print(ljung_12)
cat("Mean Residuals SMA(12):", mean(error_12), "\n")
print(test_acc)
cat("Train MAPE     :", mape_train, "\n")
cat("Test MAPE      :", mape_test,  "\n")
cat("MAPE Difference:", mape_diff,  "\n")
cat("Ljung-Box p-value:", ljung_12$p.value, "\n")