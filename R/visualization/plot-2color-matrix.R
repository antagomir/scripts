# This script generates a random matrix and visualizes it using
# blue-black-red palette used in expression studies.
# Also the color scale is plotted in the end
# 
# Leo Lahti (2009)
# License: Artistic 2.0
# http://www.opensource.org/licenses/artistic-license-2.0.php
#

# Create random matrix
nrow = 30
ncol = 10
mat = array(rnorm(nrow*ncol),dim=c(nrow,ncol))

# R functions from group directory
source("/share/mi/scripts/visualization.R")

# Define color scale
mypalette <- colorRampPalette(c("blue", "black", "red"),space = "rgb")

# Break points for color shades
mybrs = set.breaks(1, interval=.02)

# Uncomment postscript and dev.off()
# to produce eps figure
#postscript("myplot.eps")
plotMatrix.2way(mat,mybreaks=mybrs,maintext=paste("",sep=""),xlab="",ylab="",cexlab=1,mypalette)
axis(2, at = seq(0,1,length=nrow(mat)),labels=rev(paste("lab-",1:nrow(mat))), cex.axis=.9, las=2)
#dev.off()

# Plot color scale that was used
X11()
plotScale(mybrs, mypalette, cex.axis = 2, label.start =  round(sort(mybrs, dec=T)[[2]],1), Nlab = 3)
