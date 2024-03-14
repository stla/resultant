library(resultant)
library(qspray)
x <- qlone(1)
y <- qlone(2)
z <- qlone(3)
f <- x^2 + y^2 + z^2 + 2
g <- x*y + y*z

R <- resultant(f, g, 3L)
prettyQspray(R, c("x", "y"))
