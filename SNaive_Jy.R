library(forecast)

# MODEL IDENTIFICATION
plot(train, main="Monthly Sales - Training Data",
     ylab="Sales", xlab="Year")

components <- decompose(train, type="additive")
plot(components)

acf(train, lag.max=36, main="ACF of Training Data")
pacf(train, lag.max=36, main="PACF of Training Data")

# MODEL FITTING
fit_snaive <- snaive(train, h = length(test))
summary(fit_snaive)

plot(train, main="SNaive Fitted vs Training Data",
     ylab="Sales", xlab="Year")
lines(fitted(fit_snaive), col="blue", lwd=2)
legend("topleft",
       legend=c("Actual","SNaive Fitted"),
       col=c("black","blue"),
       lty=1, lwd=2, cex=0.7)

# DIAGNOSTIC CHECKING
checkresiduals(fit_snaive, lag = 12)

ljung_snaive <- Box.test(residuals(fit_snaive),
                         lag=12, type="Ljung-Box")
print(ljung_snaive)

# FORECAST EVALUATION
plot(fit_snaive, main="SNaive Forecast vs Actual",
     ylab="Sales", xlab="Year",
     fcol="blue", flwd=2)

lines(test, col="red", lwd=2)

legend("topleft", legend=c("Forecast","Actual"),
       col=c("blue","red"), lty=1, lwd=2, cex=0.7)

snaive_acc <- accuracy(fit_snaive, test)
print(snaive_acc)

mape_train <- snaive_acc["Training set","MAPE"]
mape_test  <- snaive_acc["Test set","MAPE"]
mape_diff  <- abs(mape_train - mape_test)

cat("Train MAPE:", mape_train, "\n")
cat("Test MAPE:",  mape_test,  "\n")
cat("MAPE Diff:",  mape_diff,  "\n")
cat("Ljung-Box p:", ljung_snaive$p.value, "\n")
