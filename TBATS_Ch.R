library(forecast)

# -------------------------------------------------------------
# TBATS Variant 1 (BEST):  BoxCox + Trend + Damped + ARMA errors
# Train_RMSE=49.06  Test_RMSE=51.12  RMSE_Gap=2.06
# Train_MAPE=44.04  Test_MAPE=24.61  MAPE_Gap=19.43
# -------------------------------------------------------------
fit_tbats1 <- tbats(train,
                    use.box.cox      = TRUE,
                    use.trend        = TRUE,
                    use.damped.trend = TRUE,
                    use.arma.errors  = TRUE)
summary(fit_tbats1)
checkresiduals(fit_tbats1)
fr_tbats1 <- forecast(fit_tbats1, h = h)
accuracy(fr_tbats1, test)
plot(fr_tbats1, main = "TBATS  (BoxCox + Trend + Damped + ARMA)")
lines(test, col = "turquoise2", lwd = 2)


# -------------------------------------------------------------
# TBATS Variant 2:  No BoxCox + Trend + Auto damping + ARMA errors
# Same test metrics as Variant 1 (grid output identical)
# -------------------------------------------------------------
fit_tbats2 <- tbats(train,
                    use.box.cox      = FALSE,
                    use.trend        = TRUE,
                    use.damped.trend = NULL,    # auto
                    use.arma.errors  = TRUE)
summary(fit_tbats2)
checkresiduals(fit_tbats2)
fr_tbats2 <- forecast(fit_tbats2, h = h)
accuracy(fr_tbats2, test)
plot(fr_tbats2, main = "TBATS  (No BoxCox + Trend + Auto damp + ARMA)")
lines(test, col = "turquoise2", lwd = 2)


# -------------------------------------------------------------
# TBATS Variant 3:  BoxCox + No trend + No ARMA errors (simpler model)
# Train_RMSE=51.24  Test_RMSE=55.16  RMSE_Gap=3.92
# Train_MAPE=43.97  Test_MAPE=24.25  MAPE_Gap=19.72
# -------------------------------------------------------------
fit_tbats3 <- tbats(train,
                    use.box.cox      = TRUE,
                    use.trend        = FALSE,
                    use.damped.trend = FALSE,
                    use.arma.errors  = FALSE)
summary(fit_tbats3)
checkresiduals(fit_tbats3)
fr_tbats3 <- forecast(fit_tbats3, h = h)
accuracy(fr_tbats3, test)
plot(fr_tbats3, main = "TBATS  (BoxCox, no trend, no ARMA)")
lines(test, col = "turquoise2", lwd = 2)
