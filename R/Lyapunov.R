library(fractal)

#x <- beamchaos
#x <- df$tax

ly <- lyapunov(x)
print(ly)
summary(ly)
plot(ly)

# Defaults:
# lyapunov(x, tlag=NULL, dimension=5, local.dimension=3,
#         reference=NULL, n.reference=NULL, olag=2,
#         sampling.interval=NULL, polynomial.order=3, metric=Inf, scale=NULL)

# -----------------------


library(tseriesChaos)
#output <-lyap_k(lorenz.ts, m=3, d=2, s=200, t=40, ref=1700, k=2, eps=4)
#plot(output)
#lyap(output, 0.73, 2.47)

x <- df$tax
output <-lyap_k(as.vector(x), m=3, d=2, s=200, t=40, ref=1700, k=2, eps=4)
plot(output)
#lyap(output, 0.73, 2.47)
     
# ---------------------------------------