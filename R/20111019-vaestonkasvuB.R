
# (C) Leo Lahti <leo.lahti@iki.fi>
# FreeBSD License (keep this notice)
# Esimerkki Tilastokeskuksen PC Axis-datan 
# kuvaamisesta Maanmittauslaitoksen karttadatalla
# Vaestonkasvu kuntatasolla vuonna 2010.

# Load libraries
library(maptools)
library(rgdal)
library(mapproj)
library(sp)
library(gpclib)
library(ggplot2)
library(maps)
library(sorvi)

# Read Finland map file and convert to SpatialPolygon
# (C) MML 2011
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta

shape.file <- system.file("extdata/Maanmittauslaitos/1-milj-Shape-etrs-shape/kunta1_p.shp", package = "sorvi")

#coord_map(project= "gilbert")
# Read Shape file to SpatialPolygon
#sp <- readShapePoly(shape.file, proj4string=CRS(“+init=epsg:3047"))

# More information on CRS projection definitions at:
# http://spatialreference.org/ref/epsg/3047/
sp <- readShapePoly(shape.file, proj4string=CRS("+init=epsg:3047"))

# Joonan Suomi-shape ilman merialueita
#sp <- readShapePoly("~/Rpackages/louhos/data.sorvi/Shape/kunta1_p_maa_alue", proj4string=CRS("+init=epsg:3047"))

# Projection transformations can be done with spTransform:
sp2 <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
#sp <- spTransform(sp, CRS("+proj=merc"))
#q <- spplot(sp, "Suuralue", lwd = .4, col = "black")
#print(q)

# Preprocess MML Shape file
sp <- putsaa.shape.mml(sp) 

#################################################

# Tilastokeskuksen StatFin-tietokanta
# http://www.stat.fi/tup/statfin/index.html

# Read data from PC Axis format
library(pxR)
pxf <- "http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px"
px <- read.px(pxf)

# Pick variables of interest
pxs <- subset(as.data.frame(px), Väestönmuutos.ja.väkiluku == "Luonnollinen väestönlisäys" & Vuosi == 2010)

# Preprocess
vaestonkasvu <- putsaa.px(pxs)

################################################

# Lisaa informaatio kartalle
sp@data$vaestonkasvu <- vaestonkasvu$dat[match(sp$Kunta.FI, vaestonkasvu$Alue)]

# Plotting does not go through if there are NAs
sp@data$vaestonkasvu[is.na(sp@data$vaestonkasvu)] <- 0

# Define color palette
my.palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
ncol <- 10 # Number of colors

# Plot vaestonkasvu
varname <- "vaestonkasvu"
int <- max(abs(sp[[varname]]))
q <- spplot(sp, varname,
       col.regions = my.palette(ncol),
       main = "Väestönkasvu 2010",
       colorkey = TRUE, 
       lwd = .4,
       col = "black", 
       at = seq(0 - int, 0 + int, length = ncol)
)

print(q)


#################################################

