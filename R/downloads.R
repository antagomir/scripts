library("ggplot2")
library("dlstats")
library(dplyr)

#x <- cran_stats(c("eurostat", "sotkanet", "dmt", "pxweb", "earlywarnings", "fmi"))

# ropengov selected
#pkgs <- sort(unique(c("eurostat", "sotkanet", "pxweb", "fmi", "osmar", "dmt", "dkstat", "hansard", "pollstR", "recalls", "usbroadband", "rtimes", "rsunlight", "rqog", "enigma", "RPublica", "federalregister", "mpg", "hetu", "earlywarnings")))

# ropengov all
pkgs <- sort(unique(c(
  "dkstat",
  "digitransit",  
  "enigma",  
  "eurostat",
  "europarl",  
  "federalregister",  
  "fmi2",
  "geofi",  
  "hansard",
  "helsinki",
  "hetu",
  "iotables",      
  "mpg",
  "openthl",
  "ogdindiar",  
  "osmar",  
  "pollstR",
  "psData",  
  "pxweb",  
  "recalls",
  "regions",  
  "rqog",
  "rwfs",  
  "RPublica",  
  "rsunlight",  
  "rtimes",
  "sorvi",
  "sotkanet",
  "sweidnumbr",  
  "usbroadband",
  "vipunen"  
  )))


other <- c("dmt")


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
            filter(Package %in% c("eurostat", "sotkanet", "pxweb", "helsinki")) %>%
            summarise(n = sum(downloads)) %>%
	    # Exclude current year (non-complete)
	    filter(year < as.numeric(format(Sys.time(), "%Y")))

p2 <- ggplot(x2, aes(year, n, group=Package, color=Package)) +
    geom_line(size = 2) +
    scale_y_log10() +
    geom_label(aes(label=n)) +
    labs(y = "Downloads (n)", x = "Year")


jpeg("ropengov_comp.jpg", width=700, height=500, quality=100)
print(p2)
dev.off()

pdf("ropengov_comp.pdf", width=10, height=7)
print(p2)
dev.off()

library(gridExtra)
grid.arrange(p1, p2, nrow = 2)


df <- x %>% group_by(Package) %>%
            summarise(total = sum(downloads)) %>% arrange(desc(total))


df2020 <- x %>% filter(year == 2020) %>%
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

pdf("ropengov2020dl.pdf")
print(p)
dev.off()

pdf("ropengov2020dl.pdf")
print(p)
dev.off()

p3 <- ggplot(x, aes(x = start, y = downloads, color = Package)) +
          geom_point() +
	  geom_smooth()
print(p3)



df4 <- x2 %>% group_by(year) %>% summarize(n=sum(n))
p4 <- df4 %>% ggplot(aes(x = year, y = n)) + geom_line() + geom_point() + labs(x = "Year", y = "Downloads (n)", title="rOpenGov package downloads 2016-2020")


pdf("ropengov2020dl_timeline.pdf")
print(p4)
dev.off()