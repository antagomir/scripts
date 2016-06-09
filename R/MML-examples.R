# Lataa soRvi
library(sorvi)

###############################################

# Lue Suomen kuntarajat SpatialPolygon-muodossa
# (C) Maanmittauslaitos 2011
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
# Koodien selitykset: http://www.maanmittauslaitos.fi/sites/default/files/1mkood.txt

data(MML)
sp <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]

# Kunnan pääkieli
q <- spplot(sp, "Kieli_ni1")
print(q)

# Syvyys -40m ja merisaaret
#spplot(MML[["1_milj_Shape_etrs_shape"]][["dcont_p"]], "Z", col.regions = levels(MML[["1_milj_Shape_etrs_shape"]][["dcont_p"]]))

# Suomi taynna metsaa
#field <- "forest"; varname <- "IGDS_LEVEL"; spplot(MML[["1_milj_Shape_etrs_shape"]][[field]], varname, col.regions = levels(MML[["1_milj_Shape_etrs_shape"]][[field]]))

# Suomen korkeuskayra
field <- "hcont_p"; varname <- "Z"; 
sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
sp$Z[sp$Z == 9999] <- 0 # Valtakunnan raja, merkkaa 0-korkeuskayralla.
at <- seq(0, max(sp$Z), 100)
PlotShape(sp, varname, type = "oneway", at = at, main = "Suomen korkeus (m)")

# Suomen jarvet
#field <- "lake_p"; varname <- "IGDS_LEVEL"; 
#sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
#sp$IGDS_LEVEL[sp$IGDS_LEVEL %in% c(35,37)] <- "Järvi" 
#sp$IGDS_LEVEL[!sp$IGDS_LEVEL %in% c(35,37)] <- "Muu"
#sp$IGDS_LEVEL <- factor(sp$IGDS_LEVEL)
#spplot(sp, varname, col.regions = levels(sp$IGDS_LEVEL))

# Suomen joet
#field <- "rivera_p"; varname <- "IGDS_LEVEL"; 
#sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
#sp$IGDS_LEVEL <- factor(sp$IGDS_LEVEL)
#spplot(sp, varname, col.regions = levels(sp$IGDS_LEVEL))

# Suomen pellot
field <- "pelto"; varname <- "GRID_CODE"; 
sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
sp[[varname]] <- factor(sp[[varname]])
spplot(sp, varname, col.regions = levels(sp[[varname]]))

# Suomen suot
field <- "suot"; varname <- "GRID_CODE"; 
sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
sp[[varname]] <- factor(sp[[varname]])
sp$name <- rep("", nrow(sp))
sp$name[sp[[varname]] == 1] <- "Suo"
sp$name[sp[[varname]] == 2] <- "Avosuo"
sp$name <- factor(sp$name)
spplot(sp, "name", col.regions = levels(sp[[varname]]))


# Taajamien asukasluku 1999
#field <- "taajama"; varname <- "ASULKM1999"; 
#sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
#at <- seq(0, max(sp[[varname]]), 1000)
#PlotShape(sp, varname, at = at)
# Poimi taajamien asukasluvut 1999
#tab <- as.data.frame(sp)[, c("TEXT1", "ASULKM1999")]
#tab <- tab[!duplicated(tab),]
# Jarjesta suuruusjarjestykseen
#tab <- dfsort(tab, -ASULKM1999)
#plot(0, type = "n", xlim = range(log10(tab$ASULKM1999)), ylim = c(0, nrow(tab)+1))
#text(log10(tab$ASULKM1999), 1:nrow(tab), labels = tab$TEXT1, cex = 0.8)


# Rantaviivaa, saisko zoomatuksi Turun saaristoon?
spplot(MML[["1_milj_Shape_etrs_shape"]][["coast_p"]], "TEXT2")




