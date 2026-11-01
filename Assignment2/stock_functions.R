# BDA400 - A1 - Kulwinder Singh
library(quantmod)
library(TTR)

load_stock_data <- function(portfolio_file = "portfolio.txt") {
  tickers <- Filter(nzchar, trimws(readLines(portfolio_file, warn = FALSE)))
  store   <- new.env()
  getSymbols(tickers, src = "yahoo", from = "2025-01-01",
             env = store, auto.assign = TRUE)

  result <- list()
  for (tk in tickers) {
    if (!exists(tk, envir = store, inherits = FALSE)) next
    x <- get(tk, envir = store)
    d <- data.frame(Date = index(x), coredata(x))
    names(d) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    result[[tk]] <- d
  }
  result
}

calculate_statistics <- function(df, n = 20) {
  close    <- df$Close
  counts   <- table(floor(close))                 # mode taken over $1 wide bins
  mode_bin <- as.numeric(names(counts)[which.max(counts)])

  data.frame(
    mean       = mean(close),
    median     = median(close),
    mode_bin   = mode_bin,
    sd         = sd(close),
    sma_last   = tail(SMA(close, n), 1),
    rolling_sd = tail(runSD(close, n), 1),
    pct_change = (tail(close, 1) / close[1] - 1) * 100
  )
}

all_statistics <- function(stock_list) {
  tab <- do.call(rbind, lapply(stock_list, calculate_statistics))
  tab$ticker <- rownames(tab)
  rownames(tab) <- NULL
  tab[, c("ticker", setdiff(names(tab), "ticker"))]
}
