# f(x) = x^2 - 2x - 1,
# g(x) = x^2 - 3.
# resultant:::resultantCPP(
#   c(2L, 1L, 0L), c(1L, -2L, -1L),
#   c(2L, 0L), c(1L, -3L)
# )

resultant:::resultantCPP(
  c(2L, 1L, 0L), c("1", "-2", "-1"),
  c(2L, 0L), c("1", "-3")
)

x <- qlone(1)
f <- x^2 - 2*x - 1
g <- x^2 - 3


# y^4 - y^3 + y^2 - 2*x^2*y + x^4
# y - 2*x^2
library(qspray)
x <- qlone(1)
y <- qlone(2)
f <- x^4 - x^3 + x^2 - 2*x*y^2 + y^4
g <- x - 2*y^2
powsf <- vapply(f@powers, function(pwrs) {
  out <- integer(2L)
  out[seq_along(pwrs)] <- pwrs
  out
}, integer(2L))
powsg <- vapply(g@powers, function(pwrs) {
  out <- integer(2L)
  out[seq_along(pwrs)] <- pwrs
  out
}, integer(2L))

R <- resultant:::resultantCPP2(
  powsf, f@coeffs,
  powsg, g@coeffs
)

16*(x - "1/2")^4*x^4

d <- length(R)-1L
qsprayMaker(powers = as.list(0L:d), coeffs = R)
