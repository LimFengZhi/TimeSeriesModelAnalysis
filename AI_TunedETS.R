# =============================================================
# ETS ALPHA/BETA/GAMMA GRID SEARCH for ID = 34150
# Tests hundreds of (alpha, beta, gamma) combinations on ETS(A,A,A)
# Needs: train_clean (or train), test, h  already defined
# =============================================================
library(forecast)

if (!exists("train_clean")) train_clean <- tsclean(train)

rmse <- function(a, f) sqrt(mean((a - f)^2, na.rm = TRUE))
mape <- function(a, f) {
  k <- a != 0 & !is.na(a) & !is.na(f)
  mean(abs((a[k] - f[k]) / a[k])) * 100
}

# ---------- Define the grid ----------
# alpha = level smoothing, beta = trend smoothing, gamma = season smoothing
# Small values  => smoother, more weight on history
# Large values  => reactive, more weight on recent observations
alpha_grid <- seq(0.05, 0.95, by = 0.10)   # 10 values
beta_grid  <- seq(0.01, 0.30, by = 0.05)   #  6 values
gamma_grid <- seq(0.05, 0.95, by = 0.15)   #  7 values

# That's 10 * 6 * 7 = 420 combinations (ETS requires beta <= alpha)
cat("Grid size:",
    length(alpha_grid) * length(beta_grid) * length(gamma_grid),
    "combinations\n\n")

results <- list()
k <- 0

for (a in alpha_grid) {
  for (b in beta_grid) {
    if (b > a) next                         # ETS constraint: beta <= alpha
    for (g in gamma_grid) {
      if (g > 1 - a) next                   # ETS constraint: gamma <= 1-alpha
      
      fit <- tryCatch(
        ets(train_clean, model = "AAA",
            alpha = a, beta = b, gamma = g),
        error = function(e) NULL,
        warning = function(w) NULL
      )
      if (is.null(fit)) next
      
      fv <- as.numeric(fitted(fit))
      fc <- tryCatch(as.numeric(forecast(fit, h = h)$mean),
                     error = function(e) NULL)
      if (is.null(fc) || any(!is.finite(fc))) next
      
      tr  <- rmse(as.numeric(train_clean), fv)
      te  <- rmse(as.numeric(test),        fc)
      trm <- mape(as.numeric(train_clean), fv)
      tem <- mape(as.numeric(test),        fc)
      
      if (!is.finite(tr) || !is.finite(te)) next
      
      k <- k + 1
      results[[k]] <- data.frame(
        alpha = a, beta = b, gamma = g,
        Train_RMSE = round(tr,  2), Test_RMSE = round(te,  2),
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

# ---------- Report ----------
if (sum(df$Pass) > 0) {
  ok <- df[df$Pass, ]
  ok$Score <- ok$RMSE_Gap + ok$MAPE_Gap
  ok <- ok[order(ok$Score), ]
  
  cat("===== TOP 15 PASSING (alpha, beta, gamma) =====\n")
  print(head(ok, 15), row.names = FALSE)
  
  cat("\n>>> BEST: alpha =", ok$alpha[1],
      " beta =", ok$beta[1],
      " gamma =", ok$gamma[1], "\n")
  cat(">>> R code:\n")
  cat(sprintf("    fit_ets <- ets(train_clean, model = \"AAA\",\n"))
  cat(sprintf("                   alpha = %.2f, beta = %.2f, gamma = %.2f)\n",
              ok$alpha[1], ok$beta[1], ok$gamma[1]))
} else {
  cat(">>> NO combination passes both gaps <= 10 on ETS(A,A,A).\n\n")
  cat("Top 15 by smallest combined gap:\n")
  df$Total_Gap <- df$RMSE_Gap + df$MAPE_Gap
  df <- df[order(df$Total_Gap), ]
  print(head(df, 15), row.names = FALSE)
  
  cat("\nMinimum MAPE gap achieved:", min(df$MAPE_Gap), "\n")
  cat("Minimum RMSE gap achieved:", min(df$RMSE_Gap), "\n")
}

# ---------- Visualize MAPE gap across the grid ----------
# Heatmap: for each (alpha, gamma) pair, show best MAPE gap over all beta
if (requireNamespace("lattice", quietly = TRUE)) {
  best_per_ag <- aggregate(MAPE_Gap ~ alpha + gamma, data = df, FUN = min)
  print(lattice::levelplot(MAPE_Gap ~ alpha * gamma, data = best_per_ag,
                           col.regions = heat.colors(100),
                           main = "Min MAPE Gap across beta values"))
}

write.csv(df, "ETS_AlphaBetaGamma_Grid.csv", row.names = FALSE)