# Adapting Richie Cotton's gWidgettcltk example to RStudio's manipulate
# C. Ladroue
# See also http://rstudio.org/docs/advanced/manipulate
# manipulate works in RStudio
 
library("ggplot2")
library("manipulate")
 
chromium <- read.csv("chromium.csv")
nickel <- read.csv("nickel.csv")
 
manipulate({
  p<- ggplot(data, aes(air, bm)) + geom_point()
  p+facet_grid(facet)+scale_x_continuous(trans=xScale)+scale_y_continuous(trans=yScale)
  },
  yScale=picker("Linear"="identity","Log"="log10",label="Y Scale Transformation"),
  xScale=picker("Linear"="identity","Log"="log10",label="X Scale Transformation"),
  facet=picker( "None" = ". ~ .", "RPE" = ". ~ rpe","Welding type" = ". ~ welding.type","RPE and Welding type" = "rpe ~ welding.type",initial="None",label="Faceting"),
  data=picker("Chromium"=chromium,"Nickel"=nickel,label="Datasets")
)
