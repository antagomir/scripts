# Mod From google.github.io/CausalImpact/CausalImpact.html

#install.packages("devtools")
#library(devtools)
#devtools::install_github("google/CausalImpact")

library(CausalImpact)

# create intervention effect by lifting the response variable by 10
# units after timepoint 71.
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

matplot(data, type = "l")

pre.period <- c(1, 70)
post.period <- c(71, 100)

impact <- CausalImpact(data, pre.period, post.period)

plot(impact)

summary(impact)

summary(impact, "report")

# Adjusting..
# impact <- CausalImpact(..., model.args = list(niter = 5000, nseasons = 7))

