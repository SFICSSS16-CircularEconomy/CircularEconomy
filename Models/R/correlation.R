
library(sp)

n = 20

coords = matrix(runif(2*n),ncol = 2)

d = as.matrix(dist(coords))
d = d + matrix(rnorm(length(d),sd = 0.01),nrow = nrow(d))
diag(d)<-1

c = chol(d,pivot=T)
c%*%t(c)
