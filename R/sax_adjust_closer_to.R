sax_adjust_closer_to <- function(bp, from, to) {
  closer_to_from <- closer_to(bp, from)
  closer_to_to <- closer_to(bp, to)
  if (closer_to_from == closer_to_to) {
    if (closer_to_from + 1 != length(bp)) {
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
