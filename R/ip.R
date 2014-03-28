library(sorvi)

# Define your IP
# Use this to check on your own computer
# x <- system("ifconfig", intern=TRUE) # use ipconfig in Windows
# ip <- gsub(".*? ([[:digit:]])", "\\1", x[grep("IPv4", x)])
ip <- "137.224.252.10"

# Check IP location
iploc <- ip_location(ip)

# OpenStreetMap Leaflet
#require(devtools)
#install_github('rCharts', 'ramnathv')
library(rCharts)
map <- Leaflet$new()
map$setView(iploc, zoom = 16)
map$marker(iploc, bindPopup = "<p> rOpenGov was here! </p>")
print(map)

