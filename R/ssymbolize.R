ssymbolize <- function(x, alpha, len.out, type, from, to) {
  if (alpha < 3) {
    stop("Invalid alphabet size.")
  }
  x <- paa(x, len.out)
  segment <- x[x >= from & x <= to]
  class(segment) <- c(type, class(segment))
  class(x) <- c(type, class(x))
  bp <- numeric()
  if (alpha == 3) {
    bp <- c(from, to)
  } else if (type == "sax") {
    bp <- sax_segmented_breakpoints(alpha, from, to)
  } else {
    bp <- c(from, breakpoints(segment, alpha - 2), to)
  }
  sym <- discretize(x, alpha, bp, letters)
  structure(sym, "bp" = bp, "paa" = x, class = c("symbolic", "character"))
}
