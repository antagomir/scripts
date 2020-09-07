# rChart Examples from 
# https://github.com/ramnathv/rCharts

require(devtools)
install_github('ramnathv/rCharts')

require(rCharts)

## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

## Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

# Polychart
r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = 'point', color = 'gear')
r1

#Couldn't get this working?
#graph_chart1.addHandler(function(type, e){var data <- e.evtData; if (type == 'click'){alert("You clicked on car with mpg: " + data.mpg.in[0])}})


# Standard economics longitudinal chart
data(economics, package = 'ggplot2')
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = 'date', y = c('psavert', 'uempmed'), type = 'Line', data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1

# Interactive barplots
# Select which variables to show and group or stack !
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
  type = 'multiBarChart')
n1

# xCharts  - just a stacked line plot with nice colorings on 
require(reshape2)
uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c('category', 'year')
x1 <- xPlot(value ~ year, group = 'category', data = uspexp, 
  type = 'line-dotted')
x1

# highcharts
# lineplots with highlighting and selecting which variables to plot
h1 <- Highcharts$new()
h1$chart(type = "spline")
h1$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
h1$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 4), dashStyle = "shortdot")
h1$legend(symbolWidth = 80)
h1

# OpenStreetMap Leaflet; Turku!
map3 <- Leaflet$new()
map3$setView(c(60.45, 22.266667), zoom = 15)
map3$marker(c(60.45, 22.267), bindPopup = "<p> Leo was here! </p>")
map3

# Colored lineplot chart
usp = reshape2::melt(USPersonalExpenditure)
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = 'Var1', data = usp, type = 'area')
p4
