discretize <- function(x, alpha, bp, alphabet) {
  sym <- character()
  alpha <- length(bp) + 1
  for (i in seq(from = alpha - 1, to = 1, by = -1)) {
    sym[x < bp[i]] <- alphabet[i]
  }
  sym[x >= bp[alpha - 1]] <- alphabet[alpha]
  sym
}
