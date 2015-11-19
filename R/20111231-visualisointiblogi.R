# (C) 2011 Leo Lahti <leo.lahti@iki.fi> All rights reserved.
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses

# Tama esimerkki on testattu sorvi-paketin versiolla 0.1.42
# Asennusohjeet: http://sorvi.r-forge.r-project.org/asennus.html

# Esimerkki Suomen kuntatason vaestonkasvutilastojen (Tilastokeskus)
# visualisoinnista Maanmittauslaitoksen karttadatalla (vuonna 2010)

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

# Suomen korkeuskayra
field <- "hcont_p"; varname <- "Z"; 
sp <- MML[["1_milj_Shape_etrs_shape"]][[field]]
sp$Z[sp$Z == 9999] <- 0 # Valtakunnan raja, merkkaa 0-korkeuskayralla.
at <- seq(0, max(sp$Z), 100)
PlotShape(sp, varname, type = "oneway", at = at, main = "Suomen korkeus (m)")

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


# Rantaviivaa, saisko zoomatuksi Turun saaristoon?
spplot(MML[["1_milj_Shape_etrs_shape"]][["coast_p"]], "TEXT2")


#################################################

# Lue kuntatason vaestonkasvutiedot tilastokeskuksen StatFin-tietokannasta
# http://www.stat.fi/tup/statfin/index.html
# PC Axis-muodossa ja muunna data.frameksi
px <- GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")

# Poimi taulukosta halutut tiedot
vaestonkasvu <- subset(px,
Väestönmuutos.ja.väkiluku == "Luonnollinen väestönlisäys" &
Vuosi == 2010)

################################################

# Lisaa tiedot karttaobjektiin
sp@data$vaestonkasvu <- vaestonkasvu$dat[match(sp$Kunta.FI, vaestonkasvu$Alue)]
# Korvaa puuttuvat arvot nollalla
sp[["vaestonkasvu"]][is.na(sp[["vaestonkasvu"]])] <- 0

################################################

# Piirra kuva
varname <- "vaestonkasvu"
int <- max(abs(sp[[varname]]))
q <- PlotShape(sp, varname, type = "twoway",
main = "Väestönkasvu 2010",
at = seq(0 - int, 0 + int, length = 11))

#png("vaestonkasvu.png")
jpeg("vaestonkasvu.jpg")
print(q)
dev.off()

# Esimerkkikoodi, joka hakee rasterimuotoisia kartta-aineistoja Suomen
# ympäristökeskuksen (SYKE) OIVA-palvelusta (WMS)
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2011 Joona Lehtomäki, joona.lehtomaki@gmail.com.

library(sorvi)
library(raster)
library(rgdal)

# MML:n kuntadata ladataan soRvista
data(MML)
sp <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]

# Listaa kaikki soRvi:ssa olevat WMS urlit
ListWMSurls()

# Jokaiselle WMS URLille (ts. palvelulle) on luotava oma WMS-olio.
# Corine 2006 maankäyttöluokat
corine.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Corine"))
# Natura2000-suojelualueet
suojelu.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Suojelu"))
# Pohjavesialueet
pohjavesi.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Pohjavesi"))

# Ortoilmakuvat
ortoilma.wms <- PreprocessWMS(url=LoadWMSurl(provider="OIVA", service="Image"))

# Erotellaan kunta-aineistosta Lahden polygoni
sp.lahti <- sp[which(sp@data$Kunta_ni1 == "Lahti"),]

# Haetaan Corine rasteri Lahden alueelta
# HUOM: jos aineistoa haetaan suuren tai useamman kunnan alueelta, on syytä
# käyttää pienempää resoluutiota kuin 25x25m (esim 100x100m)
# HUOM: toistaiseksi WMS-palvelun karttakerroksia (layer) ei voi kysellä soRvin
# kautta, vaan kerroksen nimi pitää tietää (ominaisuus tulossa)
corine.lahti <- GetWMSraster(WMS=corine.wms,
                             layer='CorineLandCover2006_25m',
                             extent=sp.lahti,
                             resolution=25)

# Liitetään rasterin 3 kaistaa yhdeksi (RGB)
brick.corine.lahti <- brick(corine.lahti)
# raster-paketin plotRGB sopii plottaamiseen
plotRGB(brick.corine.lahti)
# Lisätään kuntaraja
plot(sp.lahti, add=TRUE, lwd=2)

# Toistetaan sama ortoilmakuville
image.lahti <- GetWMSraster(WMS=ortoilma.wms,
                            layer='Image2006mosaiikki',
                            extent=sp.lahti,
                            resolution=25)

brick.image.lahti <- brick(image.lahti)
plotRGB(brick.image.lahti)
plot(sp.lahti, add=TRUE, lwd=2)

# Toistetaan sama Natura2000-alueille
natura.lahti <- GetWMSraster(WMS=suojelu.wms,
                             layer='ProtectedSites.Natura2000Polygons',
                             extent=sp.lahti,
                             resolution=25)

brick.natura.lahti <- brick(natura.lahti)
plotRGB(brick.natura.lahti)
plot(sp.lahti, add=TRUE, lwd=2)

# Toistetaan sama pohjavesialueille
pohjavesi.lahti <- GetWMSraster(WMS=pohjavesi.wms,
                                layer='Pohjavesialue',
                                extent=sp.lahti,
                                resolution=25)

brick.pohjavesi.lahti <- brick(pohjavesi.lahti)
plotRGB(brick.pohjavesi.lahti)
plot(sp.lahti, add=TRUE, lwd=2)

