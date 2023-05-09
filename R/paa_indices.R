#' PAA Indices
#'
#' @param len The length of the vector to be symbolized.
#' @param len.out The length of x after dimensional reduction.
#'
#' @return The indices for doing Piecewise Aggregate Approximation.
#' @export
#'
#' @examples
#' paa_indices(100, 10)
paa_indices <- function(len, len.out) {

  if (len == 0 || len.out == 0) {
    stop("Invalid zero length.")
  }

  if (len.out > len) {
    stop("Output length greater than input length.")
  }

  remainder <- len %% len.out
  increment <- rep(0, len.out)

  if (remainder != 0) {
    increment_indices <- seq(len.out - remainder + 1, len.out)
    increment[increment_indices] <- seq(1, remainder)
  }

  return(seq(0, len.out) * floor(len / len.out) + increment)
}
