#' @title Resultant of two polynomials
#' @description Resultant of two polynomials with rational coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials with at more two
#'   variables
#'
#' @return For univariate polynomials, this returns a fraction given as a
#'   string. For bivariate polynomials, this returns a univariate
#'   \code{qspray} polynomial.
#' @export
#'
#' @examples
#' library(resultant)
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' f <- x^4 - x^3 + x^2 - 2*x*y^2 + y^4
#' g <- x - 2*y^2
#' resultant(f, g)
resultant <- function(qspray1, qspray2) {
  n1 <- nvariables(qspray1)
  n2 <- nvariables(qspray2)
  n <- max(n1, n2)
  if(n == 0L) {
    stop("The two polynomials are constant.")
  }
  if(n >= 3L) {
    stop("The implementation only allows univariate and bivariate polynomials.")
  }
  coeffs1 <- qspray1@coeffs
  coeffs2 <- qspray2@coeffs
  pows1 <- vapply(qspray1@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  pows2 <- vapply(qspray2@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  if(n == 1L) {
    resultantCPP1(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else {
    coeffs <- resultantCPP2(
      pows1, coeffs1,
      pows2, coeffs2
    )
    d <- length(coeffs) - 1L
    qsprayMaker(powers = as.list(0L:d), coeffs = coeffs)
  }
}
