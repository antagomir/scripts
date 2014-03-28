# Compare running times for different functions

library(compiler)
library(rbenchmark)

dat <- matrix(rnorm(100), 10, 10)
	
f <- function (dat) {colSums(dat)}
fc <- cmpfun(f)
    
g <- function (dat) {apply(dat, 2, sum)}
gc <- cmpfun(g)

benchmark(f(dat), g(dat), fc(dat), gc(dat),
	  columns=c("test", "replications", "elapsed", "relative"),
	  order="relative", replications=1e3)
					 
############

# NOTES:
#
# colSums(x) wins apply(x,2,sum)
# x^2 wins x*x
# compiled x={1/{1+x}} wins other ways to calculate 1/(1+x)
#
#
#
