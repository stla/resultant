test_that("Univariate example", {
  x <- qlone(1)
  f <- x^2 - 2*x - 1
  g <- x^2 - 3
  r <- resultant(f, g)
  expect_equal(r, "-8/1")
})
