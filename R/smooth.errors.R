# 4.9.2012
# http://www.nicebread.de/visually-weighted-regression-in-r-a-la-solomon-hsiang/

#source("visualization.R")
library(microbiome)

#    Compute smoothers from 1000 bootstrap samples of the original sample (this results in a spaghetti plot)
#    Calculate a density estimate for each vertical cut through the bootstrapped smoothers. The area under the density curve always is 1, so the ink is constant for each y-slice.
#    Shade the figure according to these density estimates.
# -> Implemented in p4 below
 
# build a demo data set
set.seed(1)
x <- rnorm(200, 0.8, 1.2) 
e <- rnorm(200, 0, 2)*(abs(x)^1.5 + .5)
y <- 8*x - x^3 + e
df <- data.frame(x, y)
 
p1 <- vwReg(y~x, df)
p2 <- vwReg(y~x, df, shade=FALSE, spag=TRUE)
p3 <- vwReg(y~x, df, shade=TRUE, spag=FALSE, mweight=TRUE, show.CI=TRUE, show.lm=TRUE)
p4 <- vwReg(y~x, df, shade=FALSE, spag=TRUE, show.lm=TRUE)

