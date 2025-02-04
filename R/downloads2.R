library("ggplot2")
library("dlstats")
library(dplyr)
library(knitr)
# kable(df)

# ropengov selected
pkgs <- sort(unique(c("dmt", "earlywarnings")))
# c("eurostat", "sotkanet", "dmt", "pxweb", "earlywarnings", "fmi")

x <- cran_stats(pkgs)
x$year <- as.numeric(format(as.Date(x$start), format="%Y"))
x$month <- as.numeric(gsub("^0+", "", format(as.Date(x$start), format="%m")))
x <- dplyr::rename(x, Package = package)

theme_set(theme_bw(20))

# Downloads per month
p1 <- ggplot(x, aes(end, downloads, group=Package, color=Package)) +
    geom_line() + geom_label(aes(label=downloads))

# Downloads per year
x2 <- x %>% group_by(year, Package) %>%
            summarise(n = sum(downloads)) %>%
	    # Exclude current year (non-complete)
	    filter(year < as.numeric(format(Sys.time(), "%Y")))

p2 <- ggplot(x2, aes(year, n, group=Package, color=Package)) +
    geom_line(size = 3) +
    scale_y_log10() +
    geom_label(aes(label=n))

library(gridExtra)
grid.arrange(p1, p2, nrow = 2)

df <- x %>% group_by(Package) %>%
            summarise(total = sum(downloads)) %>%
	    arrange(desc(total))

df2021 <- x %>% filter(year == 2021) %>% group_by(Package) %>% summarise(total = sum(downloads)) %>% arrange(desc(total))

df2021$Package <- factor(df2021$Package, levels = rev(unique(df2021$Package)))
p <- ggplot(df2021, aes(x = Package, y = total)) +
       geom_bar(stat = "identity") +
       labs(x = "", y = "Downloads (2021)",
         title = paste0("CRAN downloads (", sum(df2021$total), ")")) + 
       coord_flip() 
print(p)

png("ropengov2021dl.png")
print(p)
dev.off()

