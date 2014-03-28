logp <- seq(-12, 0, 1)
p <- 10^logp
expected.N <- 10 # EX = n*p 
n <- expected.N / p # required number of reads
variance.N <- n*p*(1-p)
logn <- log10(expected.N) - logp
logvar <- log10(n) + log10(p) + log10(1-p)
logstd <- 0.5 * logvar
logerror <- log10(1.96) * logstd # 1.96*std
logerror.min <- log10(n - 1.96 * n * p * (1-p))
logerror.max <- log10(n + 1.96 * n * p * (1-p))

library(ggplot2)

plot(logp, logn, type = "l")
points(logp, logerror.min, type = "l")
points(logp, logerror.max, type = "l")

########################################################

n <- 1e6
logn <- log10(n)
p <- 10^seq(-12,0,.1); 
plot(log10(p), log10(n) + log10(p), type = "l")
points(log10(p), log10(n*p - 1.96 * sqrt(n * p * (1-p))), type = "l", lty = 2)
points(log10(p), log10(n*p + 1.96 * sqrt(n * p * (1-p))), type = "l", lty = 2)

########################################################

n <- 10^seq(1,6,.01)
logn <- log10(n)
p <- 1e-6
int <- 1.96 # 95%
#int <- 3.29 # 99,9%

pdf("~/depth.pdf")
plot(log10(n), log10(n) + log10(p), type = "l", ylab = "Expected hits (log10)", xlab = "Read count (log10)", main = paste("Concentration", p))
points(log10(n), log10(n*p - int * sqrt(n * p * (1-p))), type = "l", lty = 2)
points(log10(n), log10(n*p + int * sqrt(n * p * (1-p))), type = "l", lty = 2)
abline(0, 0, lty = 3)
dev.off()
