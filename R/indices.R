#' Title
#'
#' @param x The vector to be symbolized.
#' @param len.out The length of x after dimensionality reduction.
#'
#' @return The indices ...
#' @export
#'
#' @examples
#' indices(rnorm(100), 10)
indices <- function(x, len.out) {
  len <- length(x)
  seq(0, len.out) * floor(len / len.out) + remainder_increment(len, len.out)
}
