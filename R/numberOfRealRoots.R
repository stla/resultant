#' @title Number of real roots
#' @description Number of distinct real roots of a univariate polynomial.
#'
#' @param qspray a univariate \code{qspray} polynomial
#'
#' @return An integer, the number of real roots of the polynomial.
#' @export
#' @importFrom qspray numberOfVariables isQzero
#'
#' @examples
#' library(resultant)
#' library(qspray)
#' x <- qlone(1)
#' P <- 2*x^4 + x^3 - 3*x^2 - x + 1
#' numberOfRealRoots(P)
numberOfRealRoots <- function(qspray) {
  n <- numberOfVariables(qspray)
  if(n == 0L) {
    if(isQzero(qspray)) {
      Inf
    } else {
      0L
    }
  } else if(n == 1L) {
    coeffs <- qspray@coeffs
    pows <- vapply(qspray@powers, function(pwrs) {
      out <- integer(1L)
      out[seq_along(pwrs)] <- pwrs
      out
    }, integer(1L))
    numberOfRealRootsCPP(pows, coeffs)
  } else {
    stop("The polynomial is not univariate.")
  }
}
