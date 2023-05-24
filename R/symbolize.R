#' Symbolize
#'
#' @param x The vector to be symbolized.
#' @param alpha The size of the alphabet.
#' @param w The output length.
#' @param type The SAX method.
#' @param alphabet The alphabet to be used.
#'
#' @return A symbolic vector.
#' @export
#'
symbolize <- function(x, alpha, w, type, alphabet = letters) {
  type <- tolower(type)
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  x <- zscore(x)
  x <- paa(x, w)
  if (alpha == 1) {
    structure(rep(alphabet[1], w), "bp" = NA, class = c("symbolic", "character"))
  } else {
    class(x) <- c(type, class(x))
    bp <- breakpoints(x, alpha)
    sym <- discretize(x, alpha, bp, alphabet)
    structure(sym, "bp" = bp * sd_x + mean_x, "paa" = x * sd_x + mean_x, class = c("symbolic", "character"))
  }
}
