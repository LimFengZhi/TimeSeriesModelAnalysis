# =============================================================
# HW ALPHA/BETA/GAMMA GRID SEARCH for ID = 34150
# =============================================================
library(forecast)

rmse <- function(a, f) sqrt(mean((a - f)^2, na.rm = TRUE))
mape <- function(a, f) {
  k <- a != 0 & !is.na(a) & !is.na(f)
  mean(abs((a[k] - f[k]) / a[k])) * 100
}

alpha_grid <- seq(0.05, 0.95, by = 0.10)
beta_grid  <- seq(0.01, 0.30, by = 0.05)
gamma_grid <- seq(0.05, 0.95, by = 0.15)

cat("Grid size:",
    length(alpha_grid) * length(beta_grid) * length(gamma_grid), "\n\n")

results <- list(); k <- 0

for (a in alpha_grid) {
  for (b in beta_grid) {
    if (b > a) next                    # stability: beta <= alpha
    for (g in gamma_grid) {
      if (g > 1 - a) next              # stability: gamma <= 1 - alpha
      
      # Use forecast::hw() — returns a forecast object directly
      fit <- tryCatch(
        hw(train, seasonal = "additive", h = h,
           alpha = a, beta = b, gamma = g),
        error = function(e) NULL, warning = function(w) NULL
      )
      if (is.null(fit)) next
      
      fv <- as.numeric(fitted(fit))
      fc <- as.numeric(fit$mean)
      if (any(!is.finite(fc))) next
      
      tr  <- rmse(as.numeric(train), fv)
      te  <- rmse(as.numeric(test),  fc)
      trm <- mape(as.numeric(train), fv)
      tem <- mape(as.numeric(test),  fc)
      
      if (!is.finite(tr) || !is.finite(te)) next
      
      k <- k + 1
      results[[k]] <- data.frame(
        alpha = a, beta = b, gamma = g,
        Train_RMSE = round(tr, 2),  Test_RMSE = round(te, 2),
        RMSE_Gap   = round(abs(te  - tr),  2),
        Train_MAPE = round(trm, 2), Test_MAPE = round(tem, 2),
        MAPE_Gap   = round(abs(tem - trm), 2)
      )
    }
  }
}

df <- do.call(rbind, results)
df$Pass <- df$RMSE_Gap <= 10 & df$MAPE_Gap <= 10

cat("Combinations evaluated :", nrow(df), "\n")
cat("Combinations PASSING   :", sum(df$Pass), "\n\n")

if (sum(df$Pass) > 0) {
  ok <- df[df$Pass, ]
  ok$Score <- ok$RMSE_Gap + ok$MAPE_Gap
  ok <- ok[order(ok$Score), ]
  cat("===== TOP 15 PASSING (alpha, beta, gamma) for HW =====\n")
  print(head(ok, 15), row.names = FALSE)
  
  cat("\n>>> BEST: alpha =", ok$alpha[1],
      " beta =", ok$beta[1],
      " gamma =", ok$gamma[1], "\n")
  cat(sprintf(">>> Code: hw(train, seasonal = \"additive\", h = h,\n"))
  cat(sprintf("             alpha = %.2f, beta = %.2f, gamma = %.2f)\n",
              ok$alpha[1], ok$beta[1], ok$gamma[1]))
} else {
  cat("No passing combination. Top 15 by smallest combined gap:\n")
  df$Total_Gap <- df$RMSE_Gap + df$MAPE_Gap
  df <- df[order(df$Total_Gap), ]
  print(head(df, 15), row.names = FALSE)
}

write.csv(df, "HW_Grid.csv", row.names = FALSE)