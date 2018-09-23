library("ggplot2")
library("dlstats")
library(dplyr)

x <- cran_stats(c("eurostat", "sotkanet", "dmt", "pxweb", "earlywarnings", "fmi"))
x$year <- format(as.Date(x$start), format="%Y")
x$month <- format(as.Date(x$start), format="%M")

theme_set(theme_bw(20))
p1 <- ggplot(x, aes(end, downloads, group=package, color=package)) +
    geom_line() + geom_label(aes(label=downloads))

x2 <- x %>% group_by(year, package) %>% summarise(n = sum(downloads))
p2 <- ggplot(x2, aes(year, n, group=package, color=package)) +
    geom_line() + geom_label(aes(label=n))

library(gridExtra)
grid.arrange(p1, p2, nrow = 2)


df <- x %>% group_by(package) %>% summarise(total = sum(downloads))

df2017 <- x %>% filter(year == 2017) %>% group_by(package, month) %>% summarise(total = sum(downloads), monthly = sum(downloads)/n())
df2018 <- x %>% filter(year == 2018) %>% group_by(package, month) %>% summarise(total = sum(downloads), monthly = sum(downloads)/n())


library(knitr)
kable(df)

