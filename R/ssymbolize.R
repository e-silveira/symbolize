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
  structure(sym, "bp" = bp, "paa" = x, class = c("symbolic", "character"))
}
