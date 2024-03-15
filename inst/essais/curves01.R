f <- function(x, y) {
  y^4 - y^3 + y^2 - 2*x^2*y + x^4
}

g <- function(x, y) {
  y - 2*x^2
}

# contour line for f(x,y)=0
x <- seq(-1.2, 1.2, len = 2000)
y <- seq(0, 1, len = 2000)
zf <- outer(x, y, f)
crf <- contourLines(x, y, zf, levels = 0)
# contour line for g(x,y)=0
x <- seq(-1, 1, len = 2000)
y <- seq(0, 1.5, len = 2000)
zg <- outer(x, y, g)
crg <- contourLines(x, y, zg, levels = 0)

intercalateNA <- function(xs) {
  if(length(xs) == 1L) {
    xs[[1L]]
  } else {
    c(xs[[1L]], NA, intercalateNA(xs[-1L]))
  }
}
contourData <- function(cr) {
  xs <- lapply(cr, `[[`, "x")
  ys <- lapply(cr, `[[`, "y")
  data.frame("x" = intercalateNA(xs), "y" = intercalateNA(ys))
}

datf <- contourData(crf)
datg <- contourData(crg)

library(ggplot2)
ggplot() +
  geom_path(aes(x, y), data = datf, linewidth = 1, color = "blue") +
  geom_path(aes(x, y), data = datg, linewidth = 1, color = "green")

###
# define the two polynomials
library(qspray)
x <- qlone(1)
y <- qlone(2)
p <- f(x, y)
q <- g(x, y)

Rx <- resultant::resultant(p, q, var = 1) # var=1 <=> var="x"
prettyQspray(Rx, vars = "x")
#coeffs <- sapply(0:8, function(k) gmp::asNumeric(getCoefficient(R, k)))


library(giacR)
giac <- Giac$new()
command <- sprintf("roots(%s)", prettyQspray(Rx, vars = "x"))
giac$execute(command)
giac$close()

px <- substituteQspray(p, c(NA, "0"))
qx <- substituteQspray(q, c(NA, "0"))
prettyQspray(px, "x")
prettyQspray(qx, "x")

px <- substituteQspray(p, c(NA, "1/2"))
qx <- substituteQspray(q, c(NA, "1/2"))
prettyQspray(px, "x")
prettyQspray(qx, "x")


