#http://learnr.wordpress.com/2009/07/02/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-3/

library(lattice)
library(ggplot2)
data(Oats, package = "MEMSS")

p <- ggplot(faithful, aes(eruptions))

# Smooth
pg <- p + stat_density(geom = "path", position = "identity") +
     geom_point(aes(y = 0.05), position = position_jitter(height = 0.005),
         alpha = 0.25)
print(pg)

# Shrugged
pg <- p + stat_density(geom = "path", kernel = "rect",
     position = "identity", bw = 0.2) + geom_rug()
print(pg)


# Faceted
library("latticeExtra")
data(gvhd10)
pg <- p + stat_density(geom = "path", position = "identity") + facet_wrap(~Days, ncol = 2, as.table = FALSE)
print(pg)

# Filled facets
pg <- p + geom_histogram(aes(y = ..density..), binwidth = diff(range(log2(gvhd10$FSC.H)))/50) + facet_wrap(~Days, ncol = 2, as.table = FALSE) + xlab("log Forward Scatter")
print(pg)

