
```r
library(ggplot2)
library(scales)

data(iris)
d <- iris
d$t <- 1:nrow(iris)
d$av <- iris$Sepal.Length
d$N <- round(1000*runif(nrow(d)))

d <- d[, c("t", "av", "N")]

tts <- sort(unique(d$t))
for (ts in tts[rev(seq(length(tts), 1, by = -12))][-1]) {
  p <- ggplot(subset(d, t <= ts), aes(t, av, colour = N)) 
  p <- p + geom_line() 
  p <- p + ylim(0, max(d$av*1.05)) 
  p <- p + labs(x = "", y = "Radiation (uGy/h)", colour = "blue") 
  p <- p + geom_point(aes(size = av), data = subset(d, t==ts), show_guide = F)
  print(p)
}
```

<video   controls="controls" loop="loop"><source src="figure/rad.ogg" />video of chunk rad</video>
