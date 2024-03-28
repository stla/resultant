resultantCPPX <- function(X) {
  lines <- readLines("resultantCPPX.txt")
  codeX <- paste0(lines, collapse = "\n")
  sprintf(codeX, X, X, X, X, X, X, X, X, X, X, X, X-1, X-1, X-1, X-1, X-1)
}

code <-vapply(4:9, resultantCPPX, character(1L))
writeLines(code, "resultantCPPX.cpp")

resultant.hX <- function(X) {
  lines <- readLines("resultant.h.txt")
  codeX <- paste0(lines, collapse = "\n")
  sprintf(codeX, X, X, X, X, X, X)
}

code <-vapply(4:9, resultant.hX, character(1L))
writeLines(code, "resultantX.h")

#
gcdCPPX <- function(X) {
  lines <- readLines("gcdCPPX.txt")
  codeX <- paste0(lines, collapse = "\n")
  sprintf(codeX, X, X, X, X, X)
}

code <- vapply(4:9, gcdCPPX, character(1L))
writeLines(code, "gcdCPPX.cpp")
