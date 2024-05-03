#' @title Integral division of two polynomials
#' @description Integral division (division without remainder) of two
#'   polynomials with rational coefficients.
#'
#' @param qspray1,qspray2 two \code{qspray} polynomials having at most nine
#'   variables
#' @param  Boolean, whether to  that \code{qspray2} divides
#'   \code{qspray1}
#'
#' @return If \code{=TRUE}, this returns \code{NULL} if \code{qspray2}
#'   does not divide \code{qspray1}, otherwise this returns a \code{qspray}
#'   polynomial, the quotient of \code{qspray1} by \code{qspray2}.
#'   If \code{=FALSE}, this always returns a \code{qspray} polynomial,
#'   which is the quotient of \code{qspray1} by \code{qspray2} if
#'   \code{qspray2} divides \code{qspray1}, otherwise it is an undefined
#'   polynomial. So you can use \code{=FALSE} only when you are sure that
#'   \code{qspray2} divides \code{qspray1}.
#'
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables
#'
#' @seealso \code{\link{univariateDivision}},
#'   \code{\link[qspray]{qsprayDivision}}.
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' SHsequence <- x^2 + 2*x*y + 1
#' qspray1 <- q * (x^4 + y^2 + 2)
#' qspray2 <- x^4 + y^2 + 2
#' SturmHabicht(qspray1, qspray2) == q # should be TRUE
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
