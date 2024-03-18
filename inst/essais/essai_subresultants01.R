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

p <- y * (y^2 - 5*x + 6)
q <- y * (3*y + 2)
resultant:::subresultants(p, q)


