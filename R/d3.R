#get the latest version of gridSVG
#install.packages("gridSVG", repos="http://R-Forge.R-project.org")

require(ggplot2)
require(gridSVG)

#draw a ggplot2 graph
#thanks http://sharpstatistics.co.uk/r/ggplot2-guide/
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
p + facet_grid(. ~ Species) + stat_smooth(method = "lm")

#define a simple html head template
htmlhead <- 
'<!DOCTYPE html>
<head>
  <meta charset = "utf-8">
  <script src = "http://d3js.org/d3.v3.js"></script>
</head>

<body>
'

#use gridSVG to export our plot to SVG
mysvg <- grid.export("panzoom1.svg")


#define a simple pan zoom script using d3
panzoomScript <-
'  <script>
    var svg = d3.selectAll("#gridSVG");
    svg.call(d3.behavior.zoom().scaleExtent([1, 8]).on("zoom", zoom))
    
    function zoom() {
      svg.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
    } 
  </script>
</body>
'

#combine all the pieces into an html file
sink("panzoom_ggplot2.html")
cat(htmlhead,saveXML(mysvg$svg),panzoomScript)
#close our file
sink(file=NULL)
