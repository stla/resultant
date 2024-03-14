test_that("Trivariate example", {
  library(qspray)
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  f <- x^2 + y^2 + z^2 + 2
  g <- x*y + y*z
  R <- resultant(f, g, var = 3)
  expect_equal(prettyQspray(R, c("x", "y")), "y^4 + 2*x^2*y^2 + 2*y^2")
})
