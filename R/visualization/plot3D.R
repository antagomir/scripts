# Visualize

# Interpolate potential grid
library(akima)
intp <- interp(as.vector(pars), as.vector(xis), as.vector(pots), extrap = TRUE, linear = FALSE) 
xy <- expand.grid(intp$x, intp$y)
df <- data.frame(list(x = xy[,1], y = xy[,2], z = as.vector(intp$z)))

# Interactive 
library(Rcmdr)
pars <- as.vector(res$pars) 
xis <- as.vector(res$xis)
pots <- as.vector(res$pots)
scatter3d(pars, pots, xis)

# Interactive 2
library(rgl)
data(volcano)
z <- intp$z
x <- intp$x
y <- intp$y
zlim <- range(y)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen,alpha=0) # height color lookup table
col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
open3d()
rgl.surface(x, y, z, color=col, alpha=01, back="lines")


# Static 0
library(akima)
intp <- interp(as.vector(pars), as.vector(xis), as.vector(pots)) 
xy <- expand.grid(intp$x, intp$y)
z <- as.vector(intp$z)
z[is.na(z)] <- 1 # Check this is ok
df <- data.frame(list(pars = xy[,1], xis = xy[,2], pots = z))
library(ggplot2)
v <- ggplot(df, aes(pars, xis, z = pots)) + geom_tile(aes(fill = pots)) + stat_contour(binwidth = 0.1)
print(v)

# Static 1
persp(x,y,z, theta=30, phi=30, expand=0.6, col='lightblue', shade=0.75, ltheta=120, ticktype='detailed')

# Static 2
library(lattice)
cloud(z~x*y)

# Static 3
scatterplot3d(x[keep], y[keep], z[keep], highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="", pch=20)

# Static 3, not so good
dpoints   <- data.frame(list(x = as.vector(res$pars), y = as.vector(res$xis), z = as.vector(res$pots)))
minpoints <- data.frame(list(x = as.vector(res$pars[mins]), y = as.vector(res$xis[mins]), z = as.vector(res$pots[mins])+0.001))
maxpoints <- data.frame(list(x = as.vector(res$pars[maxs]), y = as.vector(res$xis[maxs]), z = as.vector(res$pots[maxs])+0.001))
df <- rbind(dpoints, minpoints, maxpoints)
classvec <- c(rep("datapoints", nrow(dpoints)), rep("minpoints", nrow(minpoints)), rep("maxpoints", nrow(maxpoints)))
classcol <- "gray"
if (nrow(minpoints)>0) {classcol <- c(classcol, "blue")}
if (nrow(maxpoints)>0) {classcol <- c(classcol, "red")}
library(made4)
do3d(df, classvec = classvec, classcol = classcol, cex.lab = 1, cex.symbols = 0.01)




########################

library(rgl)
data(volcano)
z <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
zlim <- range(y)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen,alpha=0) # height color lookup table
col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
open3d()
rgl.surface(intp$x, intp$y, intp$z, color=col, alpha=01, back="lines")

################

source("chartSeries3d.alpha.R")
library(xts)
TR <- getUSTreasuries("2008")
chartSeries3d0(TR) 

################

#source("~/scripts/R/visualization.R")
#chartSeries3d0(intp) 

plot3d(seq(0, 1, len=length(pot)), xi, pot)
