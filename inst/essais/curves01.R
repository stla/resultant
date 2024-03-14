f <- function(x, y) {
  y^4 - y^3 + y^2 - 2*x^2*y + x^4
}

g <- function(x, y) {
  y - 2*x^2
}

x <- seq(-1.2, 1.2, len = 2000)
y <- seq(0, 1, len = 2000)
zf <- outer(x, y, f)
crf <- contourLines(x, y, zf, levels = 0)

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

xs <- lapply(crf, `[[`, "x")
ys <- lapply(crf, `[[`, "y")
datf <- data.frame(x = intercalateNA(xs), y = intercalateNA(ys))
xs <- lapply(crg, `[[`, "x")
ys <- lapply(crg, `[[`, "y")
datg <- data.frame(x = intercalateNA(xs), y = intercalateNA(ys))

library(ggplot2)
ggplot() +
  geom_path(aes(x, y), data = datf, linewidth = 1, color = "blue") +
  geom_path(aes(x, y), data = datg, linewidth = 1, color = "green")

###
library(qspray)
library(resultant)
library(gmp)
x <- qlone(1)
y <- qlone(2)
p <- f(x, y)
q <- g(x, y)

R <- resultant(p, q, var = 1)
coeffs <- sapply(0:8, function(k) asNumeric(getCoefficient(R, k)))
unique(polyroot(coeffs))
prettyQspray(R, vars = "x")

library(giacR)
giac <- Giac$new()
command <- sprintf("roots(%s)", prettyQspray(R, vars = "x"))
giac$execute(command)
giac$close()

substituteQspray(p, c(NA, "1/2"))

