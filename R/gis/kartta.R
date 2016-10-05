library(tidyverse)
library(eurostat)
map.df <- get_eurostat_geospatial(output_class = "df", resolution = "60")

# Pelkän maiden rajat
map.df %>% filter(nchar(as.character(NUTS_ID)) == 2) %>% 
  ggplot(., aes(long,lat,group=group)) + 
  geom_polygon(fill=NA,colour="dim grey",size = .2) + 
  coord_map(project="orthographic", xlim=c(-22,34), ylim=c(35,70))


# Hatusta vedetty painoindeksi maittain
NUTS_ID <- c("AT", "CY", "CZ", "IE", "LI", "LT", "LU", "LV", "ME", "MK", "MT", "NL", "IS", "CH", "IT", "PT", "RO", "NO", "PL", "SE", "SK", "BE", "TR", "UK", "SI", "DE", "DK", "BG", "EE", "EL", "ES", "HR", "HU", "FI", "FR")

bmi <- rnorm(length(NUTS_ID), mean = 20, 5)

d <- data_frame(NUTS_ID,bmi)

p <- map.df %>% filter(nchar(as.character(NUTS_ID)) == 2) %>%
  left_join(.,d) %>% 
  ggplot(., aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=bmi),colour="white",size = .2) + 
  coord_map(project="orthographic", xlim=c(-22,34), ylim=c(35,70))

print(p)
