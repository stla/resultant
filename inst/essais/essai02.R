library(qspray)
x <- qlone(2)
y <- qlone(1)
z <- qlone(3)
f <- x^4 - x^3 + x^2 - 2*x*y^2 + y^4 + z
g <- x - 2*y^2 + z
n <- 3L
powsf <- vapply(f@powers, function(pwrs) {
  out <- integer(n)
  out[seq_along(pwrs)] <- pwrs
  out
}, integer(n))
powsg <- vapply(g@powers, function(pwrs) {
  out <- integer(n)
  out[seq_along(pwrs)] <- pwrs
  out
}, integer(n))

R <- resultant:::resultantCPP3(
  powsf, f@coeffs,
  powsg, g@coeffs,
  1L
)

qsprayMaker(
  powers = apply(R$Powers, 2L, identity, simplify = FALSE),
  coeffs = R$Coeffs
)
