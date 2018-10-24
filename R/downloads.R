library("ggplot2")
library("dlstats")
library(dplyr)

#x <- cran_stats(c("eurostat", "sotkanet", "dmt", "pxweb", "earlywarnings", "fmi"))

# ropengov selected
pkgs <- sort(unique(c("eurostat", "sotkanet", "pxweb", "fmi", "helsinki", "gisfin", "osmar", "sorvi", "dkstat", "hansard", "pollstR", "recalls", "usbroadband", "rtimes", "rsunlight", "rqog", "enigma", "RPublica", "federalregister", "mpg")))
x <- cran_stats(pkgs)


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


df <- x %>% group_by(package) %>% summarise(total = sum(downloads)) %>% arrange(desc(total))

df2017 <- x %>% filter(year == 2017) %>% group_by(package, month) %>% summarise(total = sum(downloads), monthly = sum(downloads)/n()) %>% select(package, total, monthly)  %>% arrange(desc(total))
df2018 <- x %>% filter(year == 2018) %>% group_by(package, month) %>% summarise(total = sum(downloads), monthly = sum(downloads)/n()) %>% select(package, total, monthly)  %>% arrange(desc(total))


library(knitr)
kable(df)



df2017$package <- factor(df2017$package, levels = rev(unique(df2017$package)))
p <- ggplot(df2017, aes(x = package, y = total)) +
       geom_bar(stat = "identity") +
       labs(x = "", y = "Downloads (2017)",
         title = paste0("CRAN downloads (", sum(df2017$total), ")")) + 
       coord_flip() 
print(p)

png("ropengov2017dl.png")
print(p)
dev.off()

