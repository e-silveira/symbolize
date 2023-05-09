paa <- function(x, len.out) {
  indices <- indices(length(x), len.out)
  sapply(seq(1, length(indices) - 1),
         function(j) mean(x[(indices[j] + 1):indices[j + 1]]))
}
