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
