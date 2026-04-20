fit1 <- ets(train, model = "AAA",
            alpha = 0.15, beta = 0.01, gamma = 0.80)
summary(fit1)
checkresiduals(fit1)
fr1 <- forecast(fit1, h = h)
accuracy(fr1, test)
plot(fr1, main = "ETS(A,A,A) α=0.05 β=0.01 γ=0.95")
lines(test, col = "turquoise2", lwd = 2)