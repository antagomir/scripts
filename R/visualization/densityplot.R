# Data (samples x features)
mydat <- matrix(rnorm(1000), 100, 10)

# PCA projection
pca <- princomp(mydat)$scores

# Arrange the data into data frame
df <- as.data.frame(pca)

# Determine bandwidth for density estimation
library(MASS)
bw <- c(bandwidth.nrd(df[["Comp.1"]]), bandwidth.nrd(df[["Comp.2"]]))

# Construct the figure
library(ggplot2)
theme_set(theme_bw(15))
p <- ggplot(df) 
p <- p + stat_density2d(aes(x = Comp.1, y = Comp.2, fill=..density..), geom="raster", stat_params = list(h = bw, contour = F), geom_params = list()) 
p <- p + scale_fill_gradient(low="white", high="black") 
p <- p + geom_point(aes(x = Comp.1, y = Comp.2), size = 1.5) 
p <- p + xlab("PCA 1") + ylab("PCA 2") + ggtitle("Density plot")

# Plot the figure
print(p)

