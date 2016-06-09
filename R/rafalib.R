# http://simplystatistics.org/2015/08/10/rafalib-package-now-on-cran/

library(rafalib)

data(iris)

# Basic plot
library(ggplot2)
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
print(p)

mypar()
print(p)

mypar(3,2) # par(mfrow=c(3,2))

# bigpar() is optimized for R presentations or PowerPoint slides

