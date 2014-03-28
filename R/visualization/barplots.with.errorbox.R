library(ggplot2)

df <- data.frame( 
   trt = factor(c(1, 2)), 
   resp = c(5, 3), 
   se = c(0.3, 0.2)) 
  
# Define the top and bottom of the errorbars 
limits <- aes(ymax = resp + se, ymin=resp - se) 
 
p <- ggplot(df, aes(y=resp, x=trt)) 
p + geom_crossbar(limits, width=0.2) 
