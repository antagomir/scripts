# Author: Joona Lehtomäki <joona.lehtomaki@gmail.com>
# Updated: 13.11.2011
# Version: 0.0.1

if (!require("rgdal")) {
install.packages("rgdal")
}

if (!require("raster")) {
install.packages("raster")
}

if (!require("XML")) {
install.packages("XML")
}

if (!require("sorvi")) {
install.packages("sorvi", repos="http://R-Forge.R-project.org",
type = "source", dependencies = TRUE)
}

# URLs to OIVA site (http://wwwp2.ymparisto.fi/scripts/oiva.asp) WMS servers.
# Servers registered here include two data sets so far:
# 1. Maanpeite (Corine 2000 / 2006) - land use categorization
# 2. Suojelualueet (suojelualueet + Natura2000) - protected areas
OIVA.URLS <- c(Corine='http://paikkatieto.ymparisto.fi/ArcGIS/services/INSPIRE/SYKE_Maanpeite/MapServer/WMSServer?',
               Suojelu='http://paikkatieto.ymparisto.fi/ArcGIS/services/INSPIRE/SYKE_SuojellutAlueet/MapServer/WMSServer?')

#' Create a WMS service description that is used to load data from WMS server.
#'
#' WMS service description file is a XML file that describes required and
#' optional information on how to retrieve an exisiting WMS raster over the
#' web. The extent of the raster tile from the data source is defined by the
#' extent of a SpatialPolygonsDataFrame object (no other ways of
#' providing extent are implemented yet). Raster resolution (pixel size is
#' also provided as a parameter.
#'
#' @param data.source string describing the name of the data source
#' @param layer string name of the layer to be fetched from the data source
#' @param extent SpatialPolygonsDataFrame object to be used to define the extent
#' @param resolution integer value of the resolution (CRS dependent)
#'
#' @return A character string of XML
#'
#' @note function does not check whether data source or the layer actually exist
#'
#' @references
#' \url{http://www.gdal.org/frmt_wms.html}
#' @author Joona Lehtomäki \email{joona.lehtomaki@@gmail.org}

WMSserviceDesc <- function(data.source, layer, extent, resolution) {
  
  # Check if data.source is included in OIVA.URLS
  if (is.na(OIVA.URLS[data.source])) {
    stop(paste('Data source', data.source, 'not found'))
  }
  
  # Extent is defined by the bounding box of the SpatialPolygonsObject provided
  # as extent parameter.
  # TODO: implement other ways of providing the extent
  if (class(extent) == 'SpatialPolygonsDataFrame') {
    bbox.extent <- bbox(extent)
# Number of columns and rows (i.e. resolution) is defined by the real
# width and height of the raster divided by the resolution parameter
# (all this depends on the CRS, not very tested)
    ncols <- round((bbox.extent[1, 2] - bbox.extent[1, 1]) / resolution, 0)
    nrows <- round((bbox.extent[2, 2] - bbox.extent[2, 1]) / resolution, 0)
# Set the extent corners
    ulx <- bbox.extent[1, 1]
    uly <- bbox.extent[2, 2]
    lrx <- bbox.extent[1, 2]
    lry <- bbox.extent[2, 1]
  } else {
    stop('Function only supports SpatialPolygonDataFrames')
  }
  
  # Create the XML structure based on the GDAL specification at
  # http://www.gdal.org/frmt_wms.html
  
  # Root level
  root <- newXMLNode('GDAL_WMS')
  # Service level
  service <- newXMLNode('Service', attrs=c(name='WMS'), parent=root)
  version <- newXMLNode('Version', text='1.1.1', parent=service)
  serverUrl <- newXMLNode('ServerUrl', text=OIVA.URLS[data.source], parent=service)
  # TODO: CRS should not be hard coded
  srs <- newXMLNode('SRS', text='EPSG:3067', parent=service)
  # Not sure if really needed
  imageFormat <- newXMLNode('ImageFormat', text='image/tiff', parent=service)
  layers <- newXMLNode('Layers', text=layer, parent=service)
  # Style is needed even if empty
  style <- newXMLNode('Style', parent=service)
  
  # DataWindow level
  dataWindow <- newXMLNode('DataWindow', parent=root)
  # Note that the following notation is minX, maxY, maxX, minY
  upperLeftX <- newXMLNode('UpperLeftX', text=ulx, parent=dataWindow)
  upperLeftY <- newXMLNode('UpperLeftY', text=uly, parent=dataWindow)
  lowerRightX <- newXMLNode('LowerRightX', text=lrx, parent=dataWindow)
  lowerRightY <- newXMLNode('LowerRightY', text=lry, parent=dataWindow)
  # TODO: although size is set here, it is not completely clear how the
  # native raster resolution on the WMS server is related to resolution
  # requested
  sizeX <- newXMLNode('SizeX', text=ncols, parent=dataWindow)
  sizeY <- newXMLNode('Sizey', text=nrows, parent=dataWindow)
  yOrigin <- newXMLNode('YOrigin', text='top', parent=dataWindow)
  
  # Back to the root level
  projection <- newXMLNode('Projection', text='EPSG:3067', parent=root)
  # Optional, this is also the default. Seems to be required in case where the
  # the raster requested is 3-band RGB raster.
  bandsCount <- newXMLNode('BandsCount', text='3', parent=root)
  # Optional, probably not needed here
  cache <- newXMLNode('Cache', parent=root)
  
  # Save the created XML object, not providing a file path converts the object
  # into string.
  return(saveXML(root))
}

# Use soRvi to get some reference data (municipalities)
data(MML)
sp <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]

# Preprocess MML Shape file
sp <- preprocess.shape.mml(sp)

# Select 2 example locations: Helsinki and Tampere

# Helsinki - Corine 2006 land use classes

# Extract only the polygon for Helsinki
sp.helsinki <- sp[which(sp@data$Kunta_ni1 == "Helsinki"),]
# Create the WMS description XML string
wms.xmlDesc.helsinki <- WMSserviceDesc('Corine', 'CorineLandCover2006_25m',
                                       sp.helsinki, 25)

# Use GDAL to read in the value, readGDAL returns a SpatialObject
wms.img.helsinki <-readGDAL(wms.xmlDesc.helsinki)
# Use brick function from package raster to squeeze the 3 RGB band into a
# RasterBrick object for easy plotting
b.wms.img.helsinki <- brick(wms.img.helsinki)
# Use plotRGB from package raster to plot the RGB raster
plotRGB(b.wms.img.helsinki)
# Add municipality borders
plot(sp.helsinki, add=TRUE, lwd=2)

# Helsinki - Natura 2000 polygons
wms.xmlDesc.helsinki <- WMSserviceDesc('Suojelu',
                                   'ProtectedSites.Natura2000Polygons',
                                   sp.helsinki, 25)

wms.img.helsinki <- readGDAL(wms.xmlDesc.helsinki)
b.wms.img.helsinki <- brick(wms.img.helsinki)
plotRGB(b.wms.img.helsinki)
plot(sp.helsinki, add=TRUE, lwd=2)

# Tampere - Corine 2006
sp.tampere <- sp[which(sp@data$Kunta_ni1 == "Tampere"),]
wms.xmlDesc.tampere <- WMSserviceDesc('Corine', 'CorineLandCover2006_25m',
                                   sp.tampere, 25)

wms.img.tampere <- readGDAL(wms.xmlDesc.tampere)
b.wms.img.tampere <- brick(wms.img.tampere)
plotRGB(b.wms.img.tampere)
plot(sp.tampere, add=TRUE, lwd=2)

# Tampere - Natura 2000 polygons
wms.xmlDesc.tampere <- WMSserviceDesc('Suojelu',
                                   'ProtectedSites.Natura2000Polygons',
                                   sp.tampere, 25)

wms.img.tampere <- readGDAL(wms.xmlDesc.tampere)
b.wms.img.tampere <- brick(wms.img.tampere)

png("tmp.png"); 
plotRGB(b.wms.img.tampere)
plot(sp.tampere, add=TRUE, lwd=2); 
dev.off()

