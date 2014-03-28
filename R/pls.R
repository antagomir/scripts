library(mixOmics)

## We consider that we have the results of a sPLS computation on liver.toxicity 
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic

# Sparse PLS
result <- spls(X, Y, ncomp = 3, keepX = c(50, 50, 50), keepY = c(10, 10, 10))

#######################

#Several frameworks are proposed in PLS:
# * regression mode: this is to model a causal relationship between the two data sets, i.e. PLS will predict Y from X
# * canonical mode: similar to CCA, this mode is used to model a bi-directionnal relationship between the two data sets
# * invariant mode: performs a redundancy analysis (the Y matrix is not deflated)
# * classic mode: is the classical PLS as proposed in Tenenhaus (1998)

## PLS
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic 
result <- pls(X, Y, ncomp = 3) # where ncomp is the number of dimensions or components 
                               # to choose
  
## sPLS mode can be "regression" or "canonical"
## keepX and keepY are the number of variable to select on each component
result <- spls(X, Y, ncomp = 3, mode = "regression", 
               keepX = c(50, 50, 50), keepY = c(10, 10, 10))

# The computation of the Root Mean Squared Error Prediction (RMSEP) can be done using the valid function as displayed below:
## Using spls with 10-fold CV
error.spls <- valid(X, Y, mode = "regression", method = 'spls', ncomp = 3, M = 10, validation = 'Mfold')                    
## Where methods can be 'pls' or 'spls' and validation "loo" or "Mfold" and M is the 
## number of folds when using Mfold
#error.spls$rmsep

##########################################

# Visualize
  
## Choose the color for the different samples depending on the type of dose the 
## rats undergo
liver.toxicity$treatment[, 3]
col <- rep(c("blue", "red", "darkgreen", "darkviolet"), c(16, 16, 16, 16))
  
## Choose different size of points 
cex <- rep(c(1, 1.2, 1, 1.4), c(16, 16, 16, 16))
## Choose the form of the points (square, circle, triangle and diamond-shaped
pch <- rep(c(15, 16, 17, 18), c(16, 16, 16, 16))
  
## comp is the two dimensions you want to display
## ind.names is wether you want labels on each points
## rep.space determines the subspace to project the individuals ("X-variate", 
## "Y-variate" or "XY-variate")
plotIndiv(result, comp = 1:2, ind.names = FALSE, rep.space = "X-variate",  col = col, cex = cex, pch = pch)

# Legends
col <- c("blue", "red", "darkgreen", "darkviolet")
cex <- c(1, 1.2, 1, 1.4)
pch <- c(15, 16, 17, 18)

## The first two parameters are the x and y coordinates of the legend on the graph
## The third one is the text of the legend
legend(0.25, 0.4, c("50 mg/kg", "150 mg/kg", "1500 mg/kg", "2000 mg/kg"), col = col, 
       pch = pch, pt.cex = cex, title = "Treatment")

# Variable plot
plotVar(result, comp = 1:2, Y.label = TRUE)


# 3-dimensional plots

## Same purpose as in 2D plots
col <- rep(c("blue", "red", "darkgreen", "darkviolet"), c(16, 16, 16, 16))
pch <- rep(c("c", "s", "t", "o"), c(16, 16, 16, 16))
## axes.box is responsible for the type of box ("box", "bbox" or "both")
plot3dIndiv(result, ind.names = FALSE, col = col, cex = 0.3, pch = pch, axes.box = "box")
plot3dVar(result, rad.in = 1, Y.label = TRUE, cex = c(1, 0.8))

# Network visualization

# Check connections between the two data sets based  on links given by SPLS
## 'threshold' is the limit to consider a link between to variable. Is directly changeable
## with interactive = TRUE
network(result, comp = 1:3, threshold = 0.8, 
    X.names = NULL, Y.names = NULL, keep.var = TRUE,
    color.node = c("mistyrose", "lightcyan"),
    shape.node = c("rectangle", "circle"),
    color.edge = c("red", "blue"),
    lty.edge = c("solid", "solid"), lwd.edge = c(1, 1), 
    show.edge.labels = FALSE, interactive = FALSE)
