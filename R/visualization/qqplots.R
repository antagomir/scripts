#http://learnr.wordpress.com/2009/07/02/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-3/

# Faceted qq-plot
data(Chem97, package = "mlmRev")
p <- ggplot(Chem97)
pg <- p + geom_point(aes(sample = gcsescore), stat = "qq", quantiles = ppoints(100)) + facet_wrap(~score)
print(pg)


# Colors, multiple lines in same figure
pg <- p + geom_point(aes(sample = gcsescore, colour = factor(score)),
     stat = "qq", quantiles = ppoints(100)) + facet_grid(~gender) +
     opts(aspect.ratio = 1) + scale_x_continuous("Standard Normal Quantiles") +
     scale_y_continuous("Average GCSE Score")
print(pg)
