# =============================================================
# TBATS TUNING GRID for ID = 34150
# =============================================================
library(forecast)

rmse <- function(a, f) sqrt(mean((a - f)^2, na.rm = TRUE))
mape <- function(a, f) {
  k <- a != 0 & !is.na(a) & !is.na(f)
  mean(abs((a[k] - f[k]) / a[k])) * 100
}

# ---- TBATS has fewer direct knobs than ETS, but these matter most ----
# use.box.cox     : apply Box-Cox transformation   (TRUE/FALSE/NULL=auto)
# use.trend       : include trend component        (TRUE/FALSE/NULL=auto)
# use.damped.trend: damp the trend                 (TRUE/FALSE/NULL=auto)
# use.arma.errors : model residuals with ARMA      (TRUE/FALSE)
# You can also pre-specify Box-Cox lambda directly

boxcox_opts <- list(TRUE, FALSE)
trend_opts  <- list(TRUE, FALSE)
damped_opts <- list(TRUE, FALSE, NULL)    # NULL = only makes sense with trend
arma_opts   <- list(TRUE, FALSE)

results <- list(); k <- 0

for (bc in boxcox_opts) {
  for (tr_opt in trend_opts) {
    for (dm in damped_opts) {
      # skip impossible combos (damping needs trend)
      if (isTRUE(dm) && !isTRUE(tr_opt)) next
      for (ar in arma_opts) {
        
        fit <- tryCatch(
          tbats(train,
                use.box.cox      = bc,
                use.trend        = tr_opt,
                use.damped.trend = dm,
                use.arma.errors  = ar),
          error = function(e) NULL, warning = function(w) NULL
        )
        if (is.null(fit)) next
        
        fv <- as.numeric(fitted(fit))
        fc_obj <- tryCatch(forecast(fit, h = h), error = function(e) NULL)
        if (is.null(fc_obj)) next
        fc <- as.numeric(fc_obj$mean)
        if (any(!is.finite(fc))) next
        
        tr  <- rmse(as.numeric(train), fv)
        te  <- rmse(as.numeric(test),  fc)
        trm <- mape(as.numeric(train), fv)
        tem <- mape(as.numeric(test),  fc)
        if (!is.finite(tr) || !is.finite(te)) next
        
        k <- k + 1
        results[[k]] <- data.frame(
          box_cox     = ifelse(is.null(bc), "auto", as.character(bc)),
          trend       = ifelse(is.null(tr_opt), "auto", as.character(tr_opt)),
          damped      = ifelse(is.null(dm), "auto", as.character(dm)),
          arma_errors = ifelse(is.null(ar), "auto", as.character(ar)),
          Train_RMSE = round(tr,  2), Test_RMSE = round(te,  2),
          RMSE_Gap   = round(abs(te  - tr),  2),
          Train_MAPE = round(trm, 2), Test_MAPE = round(tem, 2),
          MAPE_Gap   = round(abs(tem - trm), 2),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

df <- do.call(rbind, results)
df$Pass <- df$RMSE_Gap <= 10 & df$MAPE_Gap <= 10

cat("Variants tested :", nrow(df), "\n")
cat("Variants PASSING:", sum(df$Pass), "\n\n")

if (sum(df$Pass) > 0) {
  ok <- df[df$Pass, ]
  ok$Score <- ok$RMSE_Gap + ok$MAPE_Gap
  ok <- ok[order(ok$Score), ]
  cat("===== PASSING TBATS VARIANTS (best first) =====\n")
  print(ok, row.names = FALSE)
} else {
  cat("No TBATS variant passes both gaps <= 10.\n\n")
  cat("Top 10 by smallest combined gap:\n")
  df$Total_Gap <- df$RMSE_Gap + df$MAPE_Gap
  df <- df[order(df$Total_Gap), ]
  print(head(df, 10), row.names = FALSE)
  
  cat("\nMinimum MAPE_Gap achieved:", min(df$MAPE_Gap), "\n")
  cat("Minimum RMSE_Gap achieved:", min(df$RMSE_Gap), "\n")
}

write.csv(df, "TBATS_Grid.csv", row.names = FALSE)