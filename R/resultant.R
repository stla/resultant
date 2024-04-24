#' @title Resultant of two polynomials
#' @description Resultant of two polynomials with rational coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials with at more nine
#'   variables
#' @param var integer indicating with respect to which variable the resultant
#'   is desired (e.g. \code{1} for \code{x} and \code{2} for \code{y})
#'
#' @return For univariate polynomials, this returns a fraction given as a
#'   string.
#'   For bivariate polynomials, this returns a univariate
#'   \code{qspray} polynomial.
#'   For trivariate polynomials, this returns a bivariate
#'   \code{qspray} polynomial. And so on.
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables
#'
#' @examples
#' library(resultant)
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' f <- x^4 - x^3 + x^2 - 2*x*y^2 + y^4
#' g <- x - 2*y^2
#' Rx <- resultant(f, g, var = 1)
#' prettyQspray(Rx, "y")
#' Ry <- resultant(f, g, var = 2)
#' prettyQspray(Ry, "x")
resultant <- function(qspray1, qspray2, var = 1) {
  n1 <- numberOfVariables(qspray1)
  n2 <- numberOfVariables(qspray2)
  n <- max(1L, n1, n2)
  # if(n == 0L) {
  #   stop("The two polynomials are constant.")
  # }
  if(n >= 10L) {
    stop(
      "Only polynomials with at more nine variables are allowed."
    )
  }
  stopifnot(isPositiveInteger(var))
  if(var > n) {
    stop("Too large value of `var`.")
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
  } else if(n == 2L){
    coeffs <- resultantCPP2(
      pows1, coeffs1,
      pows2, coeffs2,
      var == 1L
    )
    d <- length(coeffs) - 1L
    qsprayMaker(powers = as.list(0L:d), coeffs = coeffs)
  } else {
    permutation <- makePermutation(n, var)
    if(n == 3L) {
      R <- resultantCPP3(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    } else if(n == 4L) {
      R <- resultantCPP4(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    } else if(n == 5L) {
      R <- resultantCPP5(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    } else if(n == 6L) {
      R <- resultantCPP6(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    } else if(n == 7L) {
      R <- resultantCPP7(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    } else if(n == 8L) {
      R <- resultantCPP8(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    } else if(n == 9L) {
      R <- resultantCPP9(
        pows1, coeffs1,
        pows2, coeffs2,
        permutation
      )
    }
    qsprayMaker(
      powers = apply(R[["Powers"]], 2L, identity, simplify = FALSE),
      coeffs = R[["Coeffs"]]
    )
  }
}
