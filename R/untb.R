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
