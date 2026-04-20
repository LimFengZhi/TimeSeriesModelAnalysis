library(smooth)
library(forecast)

# One-time helper function (paste this once at the top)
sma_accuracy <- function(fit, train, test, h) {
  fr <- forecast(fit, h = h)
  tr_act <- as.numeric(train); tr_fit <- as.numeric(fitted(fit))
  te_act <- as.numeric(test);  te_fit <- as.numeric(fr$mean)
  keep <- !is.na(tr_fit)
  tr_act <- tr_act[keep]; tr_fit <- tr_fit[keep]
  tr_err <- tr_act - tr_fit; te_err <- te_act - te_fit
  round(rbind(
    "Training set" = c(ME = mean(tr_err), RMSE = sqrt(mean(tr_err^2)),
                       MAE = mean(abs(tr_err)), MPE = mean(tr_err/tr_act)*100,
                       MAPE = mean(abs(tr_err/tr_act))*100),
    "Test set"     = c(ME = mean(te_err), RMSE = sqrt(mean(te_err^2)),
                       MAE = mean(abs(te_err)), MPE = mean(te_err/te_act)*100,
                       MAPE = mean(abs(te_err/te_act))*100)
  ), 4)
}


# Model 1: SMA(3)
fit1 <- sma(train, order = 3, h = h)
summary(fit1)
checkresiduals(fit1)
fr1 <- forecast(fit1, h = h)
sma_accuracy(fit1, train, test, h)       # <-- only this line changed from your SARIMA version
plot(fr1, main = "SMA(3)")
lines(test, col = "turquoise2", lwd = 2)


# Model 2: SMA(6)
fit2 <- sma(train, order = 6, h = h)
summary(fit2)
checkresiduals(fit2)
fr2 <- forecast(fit2, h = h)
sma_accuracy(fit2, train, test, h)       # <-- only this line changed
plot(fr2, main = "SMA(6)")
lines(test, col = "turquoise2", lwd = 2)


# Model 3: SMA(12)
fit3 <- sma(train, order = 12, h = h)
summary(fit3)
checkresiduals(fit3)
fr3 <- forecast(fit3, h = h)
sma_accuracy(fit3, train, test, h)       # <-- only this line changed
plot(fr3, main = "SMA(12)")
lines(test, col = "turquoise2", lwd = 2)