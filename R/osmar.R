library(osmar)

# Get Turku OSM 
tku <- get_osm(center_bbox(center_lon = 22.2686900, center_lat = 60.4514800, width = 1000, height = 1000))
plot(tku)

