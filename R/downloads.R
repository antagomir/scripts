library("ggplot2")
library("dlstats")

x <- cran_stats(c("eurostat", "pxweb"))
head(x)

theme_set(theme_bw(20))
p <- ggplot(x, aes(end, downloads, group=package, color=package)) +
    geom_line() + geom_label(aes(label=downloads))

print(p)

