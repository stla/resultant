library(resultant)

x <- qlone(1)
y <- qlone(2)

g <- x^2 + x*y + 1
p <- g * (y^2 + x^2)
q <- g * (y + x^3 + 2)

G <- gcd(p, q)

qspray1 <- q
qspray2 <- G
coeffs1 <- qspray1@coeffs
coeffs2 <- qspray2@coeffs
n <- 2L
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


Q <- resultant:::integralDivisionCPP2(
  pows1, coeffs1, pows2, coeffs2, TRUE
)

qsprayMaker(
  powers = resultant:::Columns(Q[["Powers"]]),
  coeffs = Q[["Coeffs"]]
)
