#' @title Sturm-Habicht sequence of a polynomial
#' @description Sturm-Habicht sequence of a polynomial with rational
#'   coefficients.
#'
#' @param qspray a \code{qspray} polynomial having at most nine variables
#'
#' @return A list of \code{qspray} polynomials, the Sturm-Habicht sequence of
#'   \code{qspray}, starting with the \code{0}-th Sturm-Habicht polynomial.
#'
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' SturmHabicht(x^3*y^2 + 2*x*y + 1)
SturmHabicht <- function(qspray) {
  n <- max(1L, numberOfVariables(qspray))
  if(n >= 10L) {
    stop(
      "Only polynomials with at most nine variables are allowed."
    )
  }
  coeffs <- qspray@coeffs
  pows <- vapply(qspray@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  if(n == 1L) {
    SHsequence <- SturmHabichtCPP1(
      rbind(pows), coeffs
    )
  } else if(n == 2L) {
    SHsequence <- SturmHabichtCPP2(
      pows, coeffs
    )
  } else if(n == 3L) {
    SHsequence <- SturmHabichtCPP3(
      pows, coeffs
    )
  } else if(n == 4L) {
    SHsequence <- SturmHabichtCPP4(
      pows, coeffs
    )
  } else if(n == 5L) {
    SHsequence <- SturmHabichtCPP5(
      pows, coeffs
    )
  } else if(n == 6L) {
    SHsequence <- SturmHabichtCPP6(
      pows, coeffs
    )
  } else if(n == 7L) {
    SHsequence <- SturmHabichtCPP7(
      pows, coeffs
    )
  } else if(n == 8L) {
    SHsequence <- SturmHabichtCPP8(
      pows, coeffs
    )
  } else if(n == 9L) {
    SHsequence <- SturmHabichtCPP9(
      pows, coeffs
    )
  }
  lapply(SHsequence, function(sh) {
    qsprayMaker(
      powers = Columns(sh[["Powers"]]),
      coeffs = sh[["Coeffs"]]
    )
  })
}
