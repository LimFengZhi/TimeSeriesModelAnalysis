library(forecast)

plot(train, main="Monthly Sales - Training Data",
     ylab="Sales", xlab="Year")

components <- decompose(train, type="additive")
plot(components)

acf(train,  lag.max=36, main="ACF of Training Data")
pacf(train, lag.max=36, main="PACF of Training Data")

fit_snaive <- snaive(train, h=h)
summary(fit_snaive)

plot(fit_snaive, main="SNaive Forecast vs Actual",
     xlim=c(2015, 2022), ylim=c(0, 450), ylab="Sales")
lines(test, col="red", lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=1, lwd=2, cex=0.7)

ljung_snaive <- Box.test(residuals(fit_snaive), lag=12, type="Ljung-Box")
print(ljung_snaive)

acc <- accuracy(fit_snaive, test)
print(acc)

mape_train <- acc["Training set","MAPE"]
mape_test  <- acc["Test set","MAPE"]
mape_diff  <- abs(mape_train - mape_test)

cat("Train MAPE:", mape_train, "\n")
cat("Test MAPE:", mape_test, "\n")
cat("MAPE Diff:", mape_diff, "\n")
cat("Ljung-Box p:", ljung_snaive$p.value, "\n")
