library(ggplot2)

p <- ggplot(data = iris, aes(y = Sepal.Length, x = Species)) 
p <- p + geom_dotplot(aes(fill = Species),
                 binaxis = "y",
                 binwidth = 0.1, stackdir = "center") 
p <- p + stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5)
print(p)
