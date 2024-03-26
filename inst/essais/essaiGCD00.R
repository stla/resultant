gcd <- function(qspray1, qspray2) {
  n1 <- resultant:::nvariables(qspray1)
  n2 <- resultant:::nvariables(qspray2)
  n <- max(1L, n1, n2)
  if(n >= 3L) {
    stop(
      "Only polynomials with at more two variables are allowed."
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
    D <- resultant:::gcdCPP1(
      pows1, coeffs1,
      pows2, coeffs2
    )
  } else if(n == 2L) {
    D <- resultant:::gcdCPP2(
      pows1, coeffs1,
      pows2, coeffs2
    )
  }
  qsprayMaker(
    powers = apply(D[["Powers"]], 2L, identity, simplify = FALSE),
    coeffs = D[["Coeffs"]]
  )
}

library(qspray)
x <- qlone(1)
y <- qlone(2)

g <- x^2 + x*y + 1
p <- g * (y^2 + x^2)
q <- g * (y + x^3 + 2)

gcd(p, q)


