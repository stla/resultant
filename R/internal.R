nvariables <- function(qspray) {
  suppressWarnings(max(lengths(qspray@powers)))
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x > 0
}

integerRange <- function(start, stop) {
  if(stop >= start) {
    start:stop
  } else {
    integer(0L)
  }
}

makePermutation <- function(n, var) {
  p <- c(integerRange(1L, var-1L), integerRange(var+1L, n), var)
  p[p] <- seq_along(p)
  p
}
