#' @title Greatest common divisor of two polynomials
#' @description Greatest common divisor of two polynomials with rational
#'   coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials with at more nine
#'   variables
#'
#' @return A \code{qspray} polynomial.
#'
#' @export
#' @importFrom qspray qsprayMaker
#'
#' @examples
#' library(resultant)
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' g <- x^2 + 2*x*y + 1
#' p <- g * (y^2 + x^2)
#' q <- g * (y + x^3 + 2)
#' gcd(p, q)
gcd <- function(qspray1, qspray2) {
  n1 <- nvariables(qspray1)
  n2 <- nvariables(qspray2)
  n <- max(1L, n1, n2)
  if(n >= 10L) {
    stop(
      "Only polynomials with at more nine variables are allowed."
    )
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
    D <- gcdCPP1(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 2L) {
    D <- gcdCPP2(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 3L) {
    D <- gcdCPP3(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 4L) {
    D <- gcdCPP4(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 5L) {
    D <- gcdCPP5(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 6L) {
    D <- gcdCPP6(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 7L) {
    D <- gcdCPP7(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 8L) {
    D <- gcdCPP8(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 9L) {
    D <- gcdCPP9(
      pows1, coeffs1,
      pows2, coeffs2
    )
  }
  qsprayMaker(
    powers = apply(D[["Powers"]], 2L, identity, simplify = FALSE),
    coeffs = D[["Coeffs"]]
  )
}
