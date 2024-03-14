test_that("Bivariate example", {
  library(qspray)
  x <- qlone(1)
  y <- qlone(2)
  f <- x^4 - x^3 + x^2 - 2*x*y^2 + y^4
  g <- x - 2*y^2
  r <- resultant(f, g)
  expect_true(r == 16 * (x - "1/2")^4 * x^4)
})