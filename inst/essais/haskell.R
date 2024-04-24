library(qspray)
library(resultant)
x <- qlone(1)
y <- qlone(2)
z <- qlone(3)

p = x^4 - x^3 + x^2 - 2* (x * y^2) + z^4
q = x - (2* y^2) * z^2 * 1

resultant(p, q, 1)
