# Unified Neutral Theory on Biodiversity
# http://finzi.psych.upenn.edu/R/library/untb/html/untb.package.html

library(untb)

# start: initial community
# prob: speciation probability
# gens: generations
# keep: keep the whole time series
a <- untb(start=rep(1,100),prob=0.005,gens=5000,keep=TRUE)


# Statistics
#preston(a)
#no.of.spp(a)

# Animation of community evolution
# display.untb(start=rep(1,100),prob=0.1,gens=1000)

#data(butterflies)
#plot(butterflies,uncertainty=TRUE)


# https://github.com/echen/dirichlet-process
# Return a vector of weights drawn from a stick-breaking process
# with dispersion `α`.
#
# Recall that the kth weight is
#   \beta_k = (1 - \beta_1) * (1 - \beta_2) * ... * (1 - \beta_{k-1}) * beta_k
# where each `beta_i` is drawn from a Beta distribution
#   \beta_i ~ Beta(1, α)
stick_breaking_process = function(num_weights, alpha) {
  betas = rbeta(num_weights, 1, alpha)
  remaining_stick_lengths = c(1, cumprod(1 - betas))[1:num_weights]
  weights = remaining_stick_lengths * betas
  weights
}
