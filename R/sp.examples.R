# Varita Suomen kunnat pinta-alan mukaan
#val <- "SHAPE_Area"
#brks <- quantile(sp[[val]], seq(0, 1, length = 10))
#cols <- rev(grey((length(brks):2)/length(brks)))
#plot(sp, col=cols[findInterval(sp[[val]], brks, all.inside = TRUE)])
#finmap <- fortify(sp, region="Maakunnat")

#shp.headers <- getinfo.shape(shape.file)
# Finland Uniform Zone 3:
#http://mapref.org/CoordinateReferenceSystemsFI.html#Zweig588
#dens <- (2:length(brks))*3
## plot of SpatialPolygonsDataFrame, using grey shades
sp <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +datum=NAD27"))
names(sp)
rrt <- sp$SID74/sp$BIR74
brks <- quantile(rrt, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
plot(sp, col=cols[findInterval(rrt, brks, all.inside=TRUE)])
sp <- readShapePoly(shape.file, proj4string=CRS("+proj=utm +datum=WGS84 +zone=3")); plot(sp)

sp <- readShapePoly(shape.file, proj4string=CRS("+init=epsg:26978 +proj=utm +datum=NAD27 +zone=3 +units=m +ellps=bessel")); plot(sp)
+proj=longlat # sterea, utm
+init=epsg:4267 # 26978
+datum=NAD27 # WGS84
+zone=3
#+lat_0=52.15616055555555 
#+lon_0=5.38763888888889 
#+k=0.999908 
#+x_0=155000 
#+y_0=463000 
#+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 

###################################

# plot for SpatialPolygons, with county name at label poin

library(sp)

## plot for SpatialPolygons, with county name at label point
library(maptools)
nc2 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +datum=NAD27"))
plot(nc2)
invisible(text(getSpPPolygonsLabptSlots(nc2), labels=as.character(nc2$NAME), cex=0.4))

##########################################


#      IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
#     plot(xx, border="blue", axes=TRUE, las=1)
#     text(coordinates(xx), labels=row.names(xx), cex=0.6)
#     as(xx, "data.frame")[1:5, 1:6]
#     xxx <- xx[xx$SID74 < 2,]
#     plot(xxx, border="red", add=TRUE)
#     tmpfl <- paste(tempdir(), "xxpoly", sep="/")
#     writePolyShape(xxx, tmpfl)
#     getinfo.shape(paste(tmpfl, ".shp", sep=""))
#     axx <- readShapePoly(tmpfl, proj4string=CRS("+proj=longlat +ellps=clrk66"))
#     plot(xxx, border="black", lwd=4)
#     plot(axx, border="yellow", lwd=1, add=TRUE)
#     unlink(paste(tmpfl, ".*", sep=""))
     
####################################



#plot of SpatialPolygonsDataFrame, using grey shades 

library(sp)

## plot of SpatialPolygonsDataFrame, using grey shades
library(maptools)
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +datum=NAD27"))
names(nc1)
rrt <- nc1$SID74/nc1$BIR74
brks <- quantile(rrt, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
plot(nc1, col=cols[findInterval(rrt, brks, all.inside=TRUE)])


##########################

# plot of SpatialPolygonsDataFrame, using line densities

library(sp)

## plot of SpatialPolygonsDataFrame, using line densities
library(maptools)
nc <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +datum=NAD27"))
names(nc)
rrt <- nc$SID74/nc$BIR74
brks <- quantile(rrt, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
plot(nc, density=dens[findInterval(rrt, brks, all.inside=TRUE)])
