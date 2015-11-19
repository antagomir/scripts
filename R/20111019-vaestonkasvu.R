
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

# Read Shape file to SpatialPolygon
sp <- readShapePoly(shape.file)

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
png("vaestonkasvu.png")
print(q)
dev.off()

#################################################

# Alternative trial with ggplot2-paketilla
# see http://had.co.nz/ggplot2/coord_map.html

# slower than above

# may require gpclibPermit()
# NOTE: if rgeos is installed this should work without permit
# when rgeos is not available, polygon geometry computations in
# 	maptools depend on gpclib, which has a restricted licence. It
# 	is disabled by default; to enable gpclib, type gpclibPermit() 
if (!gpclibPermitStatus()) {gpclibPermit()}

# Convert SpatialPolygon to data.frame
finmap  <- fortify(sp, region = "Kunta.FI") 

# Add information
finmap$vaestonkasvu <- as.numeric(sp$vaestonkasvu)[match(finmap$id, sp$Kunta.FI)]

# NOTE: color scale ends are mistaken here?
p <- ggplot(finmap, aes(x = long, y = lat)) + geom_polygon(aes(group=id, fill=vaestonkasvu, col=vaestonkasvu), colour="black") + opts(title="Vaestonkasvu")
print(p)

# How to get projections working?
# e.g. p <- p + coord_map(project= "gilbert") 
