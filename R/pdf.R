pdf <- function(x) {
  with(stats::density(x), list(x = x, y = cumsum(y * (x[2] - x[1]))))
}
