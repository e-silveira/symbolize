symbolize <- function(x, alpha, w, type, alphabet = letters) {
  x <- paa(x, w)
  if (alpha == 1) {
    structure(rep(alphabet[1], w), "bp" = NA, class = c("symbolic", "character"))
  } else {
    class(x) <- c(type, class(x))
    bp <- breakpoints(x, alpha)
    sym <- discretize(x, alpha, bp, alphabet)
    structure(sym, "bp" = bp, "paa" = x, class = c("symbolic", "character"))
  }
}
