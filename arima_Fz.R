library(forecast)
library(tseries)
library(uroot)


# Test ORIGINAL series
ndiffs(train, test="kpss")
nsdiffs(train, test="ch")

# Apply REGULAR difference
train_d1 <- diff(train, differences = 1)

# Test after REGULAR difference
kpss.test(train_d1) # p > 0.05 (stationary)

# Apply SEASONAL difference
train_d1_D1 <- diff(train_d1, lag =12)

# Test after seasonal difference
ch.test(train_d1_D1)

# Estimate the parameters
ggtsdisplay(train,main = "Monthly Sales Training Data (2015 - 2019): ACF and PACF")
ggtsdisplay(train_d1,main = "Monthly Sales After Regular Differencing (d=1): ACF and PACF")
ggtsdisplay(train_d1_D1,main = "Monthly Sales After Regular and Seasonal Differencing (d=1, D=1): ACF and PACF")


# Arima Manual Fitting
arima_fit1 <- Arima(train, order=c(0,1,1), seasonal=c(0,1,1))
arima_fit2 <- Arima(train, order=c(1,1,0), seasonal=c(1,1,0))
arima_fit3 <- Arima(train, order=c(1,1,1), seasonal=c(0,1,1))
arima_fit4 <- Arima(train, order=c(0,1,1), seasonal=c(1,1,0))
arima_fit5 <- Arima(train, order=c(1,1,0), seasonal=c(0,1,1))

# Benchmark Auto Arima
auto_arima_fit <- auto.arima(train,d=1, D=1, stepwise =FALSE, approximation = FALSE, ic ="aic", trace = TRUE)
summary(auto_arima_fit)

# Compare AIC & BIC
arima_fitting_result <- data.frame(
  Model = c("SARIMA(0,1,1)(0,1,1)[12]",
            "SARIMA(1,1,0)(1,1,0)[12]",
            "SARIMA(1,1,1)(0,1,1)[12]",
            "SARIMA(0,1,1)(1,1,0)[12]",
            "SARIMA(1,1,0)(0,1,1)[12]",
            paste0("AUTO: SARIMA (", 
                   arimaorder(auto_arima_fit)[1], ",",
                   arimaorder(auto_arima_fit)[2], ",",
                   arimaorder(auto_arima_fit)[3], ")(",
                   arimaorder(auto_arima_fit)[4], ",",
                   arimaorder(auto_arima_fit)[5], ",",
                   arimaorder(auto_arima_fit)[6], ")[12]")),
  AIC = c(AIC(arima_fit1), AIC(arima_fit2), AIC(arima_fit3),
          AIC(arima_fit4), AIC(arima_fit5), AIC(auto_arima_fit)),
  BIC = c(BIC(arima_fit1), BIC(arima_fit2), BIC(arima_fit3),
          BIC(arima_fit4), BIC(arima_fit5), BIC(auto_arima_fit))
)

# Print AIC & BIC
cat("=== SARIMA Model Comparison: AIC and BIC ===\n\n")
print(arima_fitting_result)

# Check Residual 
checkresiduals(arima_fit1)
# siginificant model p > 0.05

# Forecast on test set
arima_fr1 <- forecast(arima_fit1, h=h)

arima_acc <- accuracy(arima_fr1, test)

# Plotting
plot(arima_fr1, main = "SARIMA(0,1,1)(0,1,1)[12]")
lines(test, col = "turquoise2", lwd = 2)