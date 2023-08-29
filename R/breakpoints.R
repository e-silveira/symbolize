breakpoints <- function(x, alpha) {
  UseMethod("breakpoints")
}

breakpoints.sax <- function(x, alpha) {
  stats::qnorm(cuts(alpha))
}

breakpoints.dwsax <- function(x, alpha) {
  with(pdf(x), stats::approx(y, x, cuts(alpha))$y)
}

breakpoints.qsax <- function(x, alpha) {
  unname(stats::quantile(x, cuts(alpha), na.rm = TRUE))
}
