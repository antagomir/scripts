
```r
quartzFonts(mei = quartzFont(rep("Meiryo", 4)))
par(family="mei")
load("tokyog2.Rda") # データ読み込み
```

```
## Warning: cannot open compressed file 'tokyog2.Rda', probable reason 'No
## such file or directory'
```

```
## Error: cannot open the connection
```

```r
library(ggplot2)
library(scales)

tts <- sort(unique(d$t))
```

```
## Error: object 'd' not found
```

```r
for (ts in tts[rev(seq(length(tts), 1, by = -12))][-1]) {
  p <- ggplot(subset(d, t <= ts), aes(t, av, colour = N)) + geom_line() +
    scale_x_datetime(limits = range(d$t),  labels = date_format("%m月%d日")) +
    ylim(0, max(d$av*1.05)) +
    theme_bw(base_family = "mei") +
    labs(x = "", y = "Radiation (uGy/h)", colour = "計測地") +
    geom_point(aes(size = av), data = subset(d, t==ts), show_guide = F)
  print(p)
  }
```

```
## Error: object 'tts' not found
```
