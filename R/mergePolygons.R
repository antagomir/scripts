# Joonan gist
# https://gist.github.com/1886707

library(sorvi)
library(rgeos)
library(maptools)
library(gpclib)
if (!rgeosStatus()) gpclibPermit()


# Read in the data
data(MML)
fi.kunnat <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]
#fi.kunnat <- readShapePoly("~/tmp/etrs-tm35fin/kunta1_p.shp")
#fi.kunnat <- PreprocessShapeMML(readShapePoly("~/tmp/etrs-tm35fin/kunta1_p.shp"))

fi.kunnat$Kunta.FI

# Get the polygon coordinates (not sure what these are really)
lps <- coordinates(fi.kunnat)

# Divide the coordinates into 4 bins
ID <- cut(lps[,1], quantile(lps[,1]), include.lowest=TRUE)

# Dissolve the original SpatialPolygonsDataFrame into 4 bins
reg4 <- unionSpatialPolygons(fi.kunnat, ID, avoidGEOS=T)

# Plot image
pic <- PlotShape(sp, "Kunta")
