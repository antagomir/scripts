library(mgcv)

# testaa esim jotain tallasta (scelluksen vinkki)
m <- gam(y ~ c1 + ... + s(x1, k=10) + s(x2, by=c2, k=10) + x3 
                 + ..., method="REML", family=jotain)
