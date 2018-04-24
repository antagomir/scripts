# https://www.fromthebottomoftheheap.net/2011/06/10/what-is-ordisurf-doing/

require(vegan)
data(dune)
data(dune.env)

dune.pca <- rda(dune)
biplot(dune.pca, scaling = 3)

set.seed(17)
dune.ev <- envfit(dune.pca ~ A1, data = dune.env)
plot(dune.ev)

dune.sf <- ordisurf(dune.pca ~ A1, data = dune.env, plot = FALSE, scaling = 3)
biplot(dune.pca, scaling = 3)
plot(dune.ev)
plot(dune.sf, col = "forestgreen", add = TRUE)

