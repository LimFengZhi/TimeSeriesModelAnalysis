train_clean <- tsclean(train)

# Show what tsclean() changed
diff_idx <- which(abs(as.numeric(train_clean) - as.numeric(train)) > 0.5)
cat("========== tsclean() OUTLIER REPLACEMENTS ==========\n")
cat("Number of points modified:", length(diff_idx), "\n")
if (length(diff_idx) > 0) {
  for (i in diff_idx) {
    cat("  Obs", i, "(", format(as.Date(time(train)[i]), "%Y-%m"),
        "):  original =", as.numeric(train)[i],
        " -> cleaned =", round(as.numeric(train_clean)[i], 1), "\n")
  }
}