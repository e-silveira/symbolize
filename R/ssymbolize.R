#' SSymbolize
#'
#' @param x The vector to be symbolized.
#' @param alpha The size of the alphabet.
#' @param len.out The output length.
#' @param type The SAX method.
#' @param from The beginning of the desired interval.
#' @param to The end of the desired interval.
#'
#' @return A symbolic vector.
#' @export
#'
ssymbolize <- function(x, alpha, len.out, type, from, to) {
  if (alpha < 3) {
    stop("Invalid alphabet size.")
  }
  type <- tolower(type)
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  x <- zscore(x)
  x <- paa(x, len.out)
  class(x) <- c(type, class(x))
  bp <- numeric()
  if (alpha == 3) {
    bp <- c(from, to)
  } else if (type == "sax") {
    bp <- sax_segmented_breakpoints(alpha, from, to)
  } else {
    bp <- sax_adjust_closer_to(breakpoints(x, alpha), from, to)
  }
  sym <- discretize(x, alpha, bp, letters)
  structure(sym, "bp" = bp * sd_x + mean_x, "paa" = x * sd_x + mean_x, class = c("symbolic", "character"))
}
