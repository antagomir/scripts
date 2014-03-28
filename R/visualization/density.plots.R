df <- as.data.frame(t(dat2[taxs, ]))
names(df) <- c("x", "y")
k <- with(df, MASS:::kde2d(x, y, h = bw))
filled.contour(k)
image(k)
persp(k, phi = 30, theta = 20, d = 5)

# via ggplot (geom_density2d() calls kde2d())
library(ggplot2)
p <- ggplot(df,aes(x=x,y=y)) + stat_density2d(aes(fill=..level..), geom="polygon") 
print(p)

smoothScatter(df, nrpoints=1*nrow(df), colramp=colorRampPalette(c("black", "white")), pch=19, cex=.8, las = 1)

#dens <- kde2d(df$x, df$y, n = 1039, lims = seq(min(df), max(df), 0.5))
#densdf <- data.frame(expand.grid(x = df$x, y = df$y), z = as.vector(dens$z)) 
#m <- ggplot(df, aes(x = x, y = y)) + geom_point() + xlim(min(df), max(df)) + ylim(min(df), max(df)) 
#m <- m + geom_density2d()
#m <- m + geom_contour(aes(z=z), data=densdf)
#print(m)

#ggplot(df,aes(x=x,y=y))+geom_density2d()
#p <- ggplot(df,aes(x=x,y=y)) + stat_density2d(aes(alpha=..level..), geom="polygon") 
#p <- p + scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))
#p <- p + scale_fill_gradient(low="blue", high="green")
#p <- p + geom_point(colour="red",alpha=0.02)

# ------------------------------

df <- as.data.frame(projection[[which.projection]][,1:2])
names(df) <- c("x", "y")
bw <- 0.8*c(bandwidth.nrd(df$x), bandwidth.nrd(df$y))
#dens <- MASS:::kde2d(df$x, df$y, h = bw)
#dens <- kde2d(df$x, df$y, n = nrow(df), lims = seq(min(df), max(df), 0.1), h = bw)
dens <- kde2d(df$x, df$y, n = nrow(df), h = bw)
k <- with(df, dens)
filled.contour(k)
#image(k)
#persp(k, phi = 30, theta = 20, d = 5)

dens <- kde2d(df$x, df$y, n = nrow(df), lims = seq(min(df), max(df), 0.1))
densdf <- data.frame(expand.grid(x = df$x, y = df$y), z = as.vector(dens$z)) 
p <- ggplot(densdf, aes(x, y, z = z)) + stat_contour(bins = 2)

