# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- dist(mydata) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
  main="Metric	MDS",	 type="n")
text(x, y, labels = row.names(mydata), cex=.7)




# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d <- dist(mydata) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
  main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata), cex=.7)


# Sammon's mapping (focus on small distances in original space)
fit <- sammon(d, y = cmdscale(d, k = 2), k = 2)