# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html

library(googleVis)
#demo(googleVis)

df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

Intensity <- gvisIntensityMap(df)
plot(Intensity)

# ----------------------------

Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

# --------------------------

require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# -----------------------------

GeoMarker <- gvisGeoChart(Andrew, "LatLong", 
                          sizevar='Speed_kt',
                          colorvar="Pressure_mb", 
                          options=list(region="US"))
plot(GeoMarker)

# --------------------------------

PopTable <- gvisTable(Population, 
                      formats=list(Population="#,###",
                                   '% of World Population'='#.#%'),
                      options=list(page='enable'))
plot(PopTable)

# ---------------------------------
