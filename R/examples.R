# Asenna ja lataa sorvi-paketti
install.packages("sorvi", repos="http://R-Forge.R-project.org", dependencies = TRUE)
library(sorvi)

# vaestorekisterin asukasluvut kunnittain
vrek <- hae.vaestorekisteri("http://vrk.fi/default.aspx?docid=5127&amp;site=3&amp;id=0")

# hae suomen kartta ja kuntarajat gadm-muodossa
gadm <- hae.gadm("FIN_adm", "kunnat")

# Liita vaestorekisterin tiedot karttaobjektiin ja
# aseta nollaan asukasluku kunnissa joiden osalta se ei ole tiedossa
gadm$asukkaita <- log10(rowSums(vrek[gadm$kunnat, c("Miehet", "Naiset")]))
gadm$asukkaita[is.na(gadm$asukkaita)] <- 0
# Laske myos sukupuolten suhteellinen osuus
gadm$miehet.osuus <- vrek[gadm$kunnat, "Miehet"]/vrek[gadm$kunnat, "Yhteensa"]
gadm$naiset.osuus <- vrek[gadm$kunnat, "Naiset"]/vrek[gadm$kunnat, "Yhteensa"]
# Aseta arvoon 50% miesten/naisten osuus
# kunnissa joiden osalta vakiluku ei ole tiedossa
gadm$miehet.osuus[is.na(gadm$miehet.osuus)] <- 0.5
gadm$naiset.osuus[is.na(gadm$naiset.osuus)] <- 0.5

# Piirra Suomen kartta varitettyna miesten suhteellisen osuuden nojalla
varname <- "miehet.osuus" # tarkasteltava muuttuja
interval <- max(abs(gadm[[varname]] - 0.5)) # paletin rajat
ncol <- 100 # varien maara
my.palette <- colorRampPalette(c("red", "white", "blue"), space = "rgb")
print(spplot(gadm, varname,
col.regions = my.palette(ncol),
main = "Suomen kuntien miehitys",
colorkey = TRUE,
lwd = .4,
col = "white",
at = seq(0.5 - interval, 0.5 + interval, length = ncol)
))

dev.print(device=png, file = "Suomen.kuntien.miehitys.png",  width = 600, height = 600)

