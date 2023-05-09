zscore <- function(x) {
  (x - mean(x)) / stats::sd(x)
}
