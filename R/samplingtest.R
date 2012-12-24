#source("slicesampling.R")
#uni.slice(0, g = function (x) {x^3 - 2*x + 1}))

library(diversitree)
set.seed(1)
lik <- function (x) {x^3 - 2*x + 1}
samples <- mcmc(lik, c(0, 0), 10000, 1, print.every = 1000)