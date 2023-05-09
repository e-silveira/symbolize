sax_segmented_breakpoints <- function(alpha, from, to) {
  bp <- breakpoints.sax(NA, alpha)
  closer_to_from <- closer_to(bp, from)
  closer_to_to <- closer_to(bp, to)
  if (closer_to_from == closer_to_to) {
    if (closer_to_from + 1 != alpha) {
      bp[closer_to_from] <- from
      bp[closer_to_from + 1] <- to
    } else {
      bp[closer_to_to] <- to
      bp[closer_to_to - 1] <- from
    }
  } else {
    bp[closer_to_from] <- from
    bp[closer_to_to] <- to
  }
  bp
}
