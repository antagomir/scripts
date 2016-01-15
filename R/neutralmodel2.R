library(untb)
a <- untb(start=rep(5, 100), prob=0.01, gens=1000, keep=TRUE)
plot(species.count(a),type="b")
matplot(species.table(a),type="l",lty=1)
