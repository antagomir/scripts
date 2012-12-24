# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 

# Biplot visualization
fit <- princomp(mydata, cor=TRUE)
biplot(fit)

# PCA Variable Factor Map 
library(FactoMineR)
result <- PCA(mydata) # graphs generated automatically

#######################################

# Sparse PCA

library(mixOmics)
data(liver.toxicity)
X <- liver.toxicity$gene # Using one data set only

## PCA example: data were centered but not scaled
result <- pca(X, ncomp = 3, center = TRUE, scale. = FALSE)
  
## sPCA example: we are selecting 50 variables on each of the PCs
result <- spca(X, ncomp = 3, center = TRUE, scale. = TRUE, keepX = rep(50, 3))

# optimal number of components 
#pcatune(X, ncomp = 10, center = TRUE, scale. = FALSE)

# Visualize
plotVar(result, comp = 1:2, Y.label = TRUE)
plotIndiv(result, comp = 1:2, ind.names = FALSE, rep.space = "X-variate",  col = col, cex = cex, pch = pch)

#######################################