# BDA400 - A1 - Kulwinder Singh - display and comparison charts
source("stock_functions.R")

portfolio <- load_stock_data("portfolio.txt")
results   <- all_statistics(portfolio)

cat("Data loaded for:", paste(names(portfolio), collapse = ", "), "\n\n")
for (tk in names(portfolio)) {
  cat("---", tk, "---\n")
  print(head(portfolio[[tk]], 3))
}

cat("\nStatistics for the whole portfolio\n")
print(results, digits = 4, row.names = FALSE)

# Compare stocks at very different price levels by rebasing each to 100.
# Trim every series to the shortest one so the lines share an x axis.
shortest <- min(sapply(portfolio, nrow))
rebased  <- sapply(portfolio, function(d) {
  px <- d$Close[seq_len(shortest)]
  100 * px / px[1]
})
matplot(portfolio[[1]]$Date[seq_len(shortest)], rebased,
        type = "l", lty = 1, lwd = 2, xlab = "Date", ylab = "Index (start = 100)",
        main = "Portfolio comparison, rebased to 100")
legend("topleft", legend = names(portfolio), col = seq_along(portfolio), lty = 1, bty = "n")
