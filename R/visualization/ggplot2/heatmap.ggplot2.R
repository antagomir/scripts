# From Learning R blog

require(ggplot2)

# Data
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
#nba <- read.csv("ppg2008.csv")
#The players are ordered by points scored, and the Name variable
#converted to a factor that ensures proper sorting of the plot.
nba$Name <- with(nba, reorder(Name, PTS))



#Whilst FlowingData uses heatmap function in the stats-package that
#requires the plotted values to be in matrix format, ggplot2 operates
#with dataframes. For ease of processing, the dataframe is converted
#from wide format to a long format.
nba.m <- melt(nba)

#game statistics have very different ranges, so to make them
#comparable all the individual statistics are rescaled
nba.m <- ddply(nba.m, .(variable), transform, rescale = rescale(value))

#no specific heatmap plotting function in ggplot2, but
#combining geom_tile with a smooth gradient fill does the job well
p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
     colour = "white") + scale_fill_gradient(low = "white",
     high = "steelblue")


#finishing touches to the formatting
base_size <- 9
p + theme_grey(base_size = base_size) + labs(x = "",
     y = "") + scale_x_discrete(expand = c(0, 0)) +
     scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none",
     axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size *
     0.8, angle = 330, hjust = 0, colour = "grey50"))
