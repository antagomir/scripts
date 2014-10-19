
```r
library(ggplot2)
library(scales)
theme_set(theme_bw(20))

data(iris)
d <- iris
d$t <- 1:nrow(d)
for (s in unique(iris$Species)) {
  d$t[iris$Species == s] <- 1:sum(iris$Species == s)
}
d$av <- iris$Sepal.Length
d$N <- round(1000*runif(nrow(d)))
d <- d[, c("t", "av", "N", "Species")]
for (ts in seq(min(d$t), max(d$t), length = 100)) {
  p <- ggplot(subset(d, t <= ts), aes(t, av, colour = Species)) 
  p <- p + geom_line() 
  p <- p + ylim(0, max(d$av*1.05)) 
  #p <- p + labs(x = "", y = "Radiation", colour = "Species") 
  p <- p + geom_point(aes(size = av), data = subset(d, t==ts), show_guide = F)
  print(p)
}
```

```
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
```

```
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
```

```
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
```

<video   controls="controls" loop="loop"><source src="figure/rad.ogg" />video of chunk rad</video>
