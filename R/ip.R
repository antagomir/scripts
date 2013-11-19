# Purpose: Get geographic coordinates for a given IP-address
# Author: Kay Cichini 
# Modified by Leo Lahti
# Output: Latitude and longitude as a numeric vector
IPtoXY <- function(x) {
   URL_IP <- paste("http://www.datasciencetoolkit.org//ip2coordinates/",
                   x, sep = "")

   api_return <- readLines(URL_IP, warn = F)
   lon1 <- api_return[grep("longitude", api_return)]
   lon <- gsub("[^[:digit:].]", "", lon1)
   lat1 <- api_return[grep("latitude", api_return)]
   lat <- gsub("[^[:digit:].]", "", lat1)
   return(c(lat, lon))
}


# Example:
ip <- "137.224.252.10"
iploc <- IPtoXY(ip)

# OpenStreetMap Leaflet
#require(devtools)
#install_github('rCharts', 'ramnathv')
library(rCharts)
map <- Leaflet$new()
map$setView(iploc, zoom = 17)
map$marker(iploc, bindPopup = "<p> Leo was here! </p>")
print(map)

