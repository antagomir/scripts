# Unified Neutral Theory on Biodiversity
# http://finzi.psych.upenn.edu/R/library/untb/html/untb.package.html

library(untb)
a <- untb(start=rep(1,100),prob=0.005,gens=5000,keep=FALSE)
preston(a)
no.of.spp(a)

display.untb(start=rep(1,100),prob=0.1,gens=1000)

data(butterflies)
plot(butterflies,uncertainty=TRUE)
