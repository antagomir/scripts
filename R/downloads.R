library("ggplot2")
library("dlstats")
library(dplyr)

#x <- cran_stats(c("eurostat", "sotkanet", "dmt", "pxweb", "earlywarnings", "fmi"))

# ropengov selected
pkgs <- sort(unique(c("eurostat", "sotkanet", "pxweb", "fmi", "osmar", "dmt", "dkstat", "hansard", "pollstR", "recalls", "usbroadband", "rtimes", "rsunlight", "rqog", "enigma", "RPublica", "federalregister", "mpg", "hetu", "earlywarnings")))

#pkgs <- sort(unique(c("eurostat", "sotkanet")))
#pkgs <- sort(unique(c("pxR", "pxweb")))
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
            summarise(total = sum(downloads)) %>% arrange(desc(total))

df2020 <- x %>% filter(year == 2020) %>%
                group_by(Package, month) %>%
		summarise(total = sum(downloads),
		          monthly = sum(downloads)/n()) %>%
	        select(Package, total, monthly)  %>%
		arrange(desc(total))

df2020total <- x %>% filter(year == 2020) %>%
                group_by(Package) %>%
		summarise(total = sum(downloads)) %>%
		arrange(desc(total))

library(knitr)
kable(df)

df2020$Package <- factor(df2020$Package, levels = rev(unique(df2020$Package)))
p <- ggplot(df2020, aes(x = Package, y = total)) +
       geom_bar(stat = "identity") +
       labs(x = "", y = "Downloads (2020)",
         title = paste0("CRAN downloads (", sum(df2020$total), ")")) + 
       coord_flip() 
print(p)

p3 <- ggplot(x, aes(x = start, y = downloads, color = Package)) +
          geom_point() +
	  geom_smooth()
print(p3)

#png("ropengov2020dl.png")
#print(p)
#dev.off()

