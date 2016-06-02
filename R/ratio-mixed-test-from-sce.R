library(lme4)

scale <- 50 # Scale of counts (max mean will be 3*scale)
d <- data.frame(cbind(rpois(20, scale), rpois(20, 3*scale)))
d$X1[1:10] <- d$X1[1:10]+rpois(10, 2*scale)
# The latent ratio is 0.5 for the first half, 0.25 for the rest.

d$obs <- as.factor(1:nrow(d))
m <- glmer(cbind(X1, X2) ~ (1|obs), family=binomial, data=d)

ilogit <- function (x) {1/(1+exp(-x))}

p <- ilogit(fixef(m)+ranef(m)$obs[,1])

plot(p)
points(d$X1/(d$X1+d$X2), col="red")
