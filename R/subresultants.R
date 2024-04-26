#' @title Subresultants of two polynomials
#' @description Subresultants of two polynomials with rational coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials with at more nine
#'   variables
#' @param var integer indicating with respect to which variable the resultant
#'   is desired (e.g. \code{1} for \code{x} and \code{2} for \code{y})
#'
#' @return For univariate polynomials, this returns a vector of fractions
#'   given as strings.
#'   For bivariate polynomials, this returns a list of univariate
#'   \code{qspray} polynomials. And so on.
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables permuteVariables
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' p <- x^2 * y * (y^2 - 5*x + 6)
#' q <- x^2 * y * (3*y + 2)
#' subresultants(p, q, var = 1) # should be 0, 0, non-zero, ...
#' subresultants(p, q, var = 2) # should be 0, non-zero, ...
subresultants <- function(qspray1, qspray2, var = 1) {
  n1 <- numberOfVariables(qspray1)
  n2 <- numberOfVariables(qspray2)
  n <- max(1L, n1, n2)
  # if(n == 0L) {
  #   stop("The two polynomials are constant.")
  # }
  if(n >= 10L) {
    stop(
      "Only polynomials with at most nine variables are allowed."
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
    subresultantsCPP1(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else {
    permutation <- makePermutation(n, var)
    ipermutation <- inversePermutation(permutation) - 1L
    if(n == 2L) {
      subres <- subresultantsCPP2(
        pows1, coeffs1,
        pows2, coeffs2,
        var == 1
      )
    } else if(n == 3L) {
      subres <- subresultantsCPP3(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 4L) {
      subres <- subresultantsCPP4(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 5L) {
      subres <- subresultantsCPP5(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 6L) {
      subres <- subresultantsCPP6(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 7L) {
      subres <- subresultantsCPP7(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 8L) {
      subres <- subresultantsCPP8(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    } else if(n == 9L) {
      subres <- subresultantsCPP9(
        pows1, coeffs1,
        pows2, coeffs2,
        ipermutation
      )
    }
    lapply(subres, function(S) {
      qspray <- qsprayMaker(
        powers = Columns(S[["Powers"]]),
        coeffs = S[["Coeffs"]]
      )
      permuteVariables(qspray, permutation)
    })
  }
}
