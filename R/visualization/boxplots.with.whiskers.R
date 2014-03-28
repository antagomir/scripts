data.m <- matrix(c(75,34,19, 39,90,41), nrow = 2, ncol=3, byrow=TRUE,
               dimnames = list(c("Factor 1", "Factor 2"),
                                c("Class A", "Class B", "Class C")))

# Create the standard error matrix

error.m <- matrix(c(12,10,7, 4,7,3), nrow = 2, ncol = 3, byrow=TRUE)

# Join the data and s.e. matrices into a data frame

data.fr <- data.frame(data.m, error.m) 

# load library {gplots}

library(gplots)

# Plot the bar graph, with standard errors

with(data.fr,
     barplot2(data.m, beside=TRUE, axes=T, las=1, ylim = c(0,120),  
                main=" ", sub=" ", col=c("gray20",0),
                    xlab="Class", ylab="Total amount (Mean +/- s.e.)",
                plot.ci=TRUE, ci.u=data.m+error.m, ci.l=data.m-error.m, ci.lty=1))

# Now, give it a legend:

legend("topright", c("Factor 1", "Factor 2"), fill=c("gray20",0),box.lty=0)