library(qspray)
x <- qlone(1)
p <- x^2 - 5*x + 6
q <- x^2 - 3*x + 2

resultant:::subresultants(p, q)

x <- qlone(1)
y <- qlone(2)

p <- x^2*y^2 - 5*x + 6
q <- x^2 - 3*y + 2
resultant:::subresultants(p, q)

p <- x^2 * y * (y^2 - 5*x + 6)
q <- x^2 * y * (3*y + 2)
Sx <- subresultants(p, q, var = 1) # should be 0, 0, non-zero, ...
lapply(Sx, function(s) prettyQspray(s, "y"))
Sy <- subresultants(p, q, var = 2) # should be 0, non-zero, ...
lapply(Sy, function(s) prettyQspray(s, "x"))

