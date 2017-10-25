
d = 0.2

m = read.table("citeseer.rtable")

n = ncol(m)
pr = rep(1,n)

m

m.matrix <- data.matrix(m)

(pr <- (1-d)/n + (d * (m.matrix %*% (pr/colSums(m.matrix)))))

pr <- rep(1,n)
length(pr/colSums(m.matrix))

m.matrix
m.matrix %*% (pr/colSums(m.matrix))
