#' Increment Remainder
#'
#' @param len The length of the original vector.
#' @param len.out The length of the vector after reduction.
#'
#' @return A vector with the increments.
#' @export
#'
#' @examples
#' remainder_increment(1000, 100)
remainder_increment <- function(len, len.out) {

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

  return(increment)
}
