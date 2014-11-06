library(sde)

# Estimate volatility change point
tau0 <- 0.6
k0 <- ceiling(1000*tau0)
set.seed(123)
X1 <- sde.sim(X0=1, N=2*k0, t0=0, T=tau0, model="CIR",
theta=c(6,2,1))
X2 <- sde.sim(X0=X1[2*k0+1], N=2*(1000-k0), t0=tau0,
T=1, model="CIR", theta=c(6,2,3))
Y <- ts(c(X1,X2[-1]), start=0, deltat=deltat(X1))
X <- window(Y,deltat=0.01)
DELTA <- deltat(X)
n <- length(X)
mu <- function(x) 6-2*x
sigma <- function(x) sqrt(x)
cp <- cpoint(X,mu,sigma)
cp
plot(X)
abline(v=tau0,lty=3)
abline(v=cp$tau0,col="red")
# nonparametric estimation
cpoint(X)

# ---------------------------------------------

# Cluster time series 
#De Gregorio, A. Iacus, S.M. (2008) Clustering of discretely observed
#diffusion processes, http: //arxiv.org/abs/0809.3902
#Analysis of both synthetic data and real financial data from
#NYSE/NASDAQ stocks, give evidence that this distance seems capable to
#catch differences in both the drift and diffusion coefficients
#contrary to other commonly used metrics.

data(quotes)
plot(quotes)
d <- MOdist(quotes)
cl <- hclust( d )
groups <- cutree(cl, k=4)
cmd <- cmdscale(d)
plot( cmd, col=groups)
text( cmd, labels(d) , col=groups)
plot(quotes, col=groups)
plot(quotes, col=groups,ylim=range(quotes))

# ---------------------------

# Simulate SDEs

# Ornstein-Uhlenbeck process
set.seed(123)
d <- expression(-5 * x)
s <- expression(3.5)
sde.sim(X0=10,drift=d, sigma=s) -> X
plot(X,main="Ornstein-Uhlenbeck")

# Multiple trajectories of the O-U process
set.seed(123)
sde.sim(X0=10,drift=d, sigma=s, M=3) -> X
plot(X,main="Multiple trajectories of O-U")

# Cox-Ingersoll-Ross process
# dXt = (6-3*Xt)*dt + 2*sqrt(Xt)*dWt
set.seed(123)
d <- expression( 6-3*x )
s <- expression( 2*sqrt(x) )
sde.sim(X0=10,drift=d, sigma=s) -> X
plot(X,main="Cox-Ingersoll-Ross")

# Cox-Ingersoll-Ross using the conditional distribution "rcCIR"
set.seed(123)
sde.sim(X0=10, theta=c(6, 3, 2), rcdist=rcCIR,
method="cdist") -> X
plot(X, main="Cox-Ingersoll-Ross")

set.seed(123)
sde.sim(X0=10, theta=c(6, 3, 2), model="CIR") -> X
plot(X, main="Cox-Ingersoll-Ross")

# Exact simulation
set.seed(123)
d <- expression(sin(x))
d.x <- expression(cos(x))
A <- function(x) 1-cos(x)
sde.sim(method="EA", delta=1/20, X0=0, N=500,
drift=d, drift.x = d.x, A=A) -> X
plot(X, main="Periodic drift")

