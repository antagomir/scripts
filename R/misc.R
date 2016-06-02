
mml.koodit <- function (which = "1_milj_koodit.txt") { 

  # Lue MML-datan tulkintakoodit
  # 1_milj_koodit.txt / 4_5_milj_koodit.txt

  tab <- readLines(system.file(paste("extdata/Maanmittauslaitos/1_milj_Shape_etrs_shape/", which, sep = ""), package = "sorvi"))


}


#p <- p + coord_map(project = "mercator") 
# projection = ... see ?mapproject for complete list
#p <- p + coord_map(project="cylindrical") 
#p <- p + coord_map(project='azequalarea',orientation=c(-36.92,174.6,0)) 
#p <- p + coord_map(project= "gilbert") 
#p <- p + coord_map(project= "lagrange") 
#p <- p + coord_map(project= "orthographic") 
#p <- p + coord_map(project= "stereographic") 
#p <- p + coord_map(project= "conic", lat0 = 30)
#p <- p + coord_map(project= "bonne", lat0 = 50) 
#p <- p + coord_map(project= "gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")
print(p)

#################################################

# Create a lat-long dataframe from the maps package 
# http://had.co.nz/ggplot2/coord_map.html
#(finmap <- qplot(x, y, data=nz, geom="path")) 
#qplot(x = long, y = lat, col = mediaanitulo, data = finmap, geom = "path")
#try_require("maps") 
#nz <- data.frame(map("nz", plot=FALSE)[c("x","y")]) 
#(nzmap <- qplot(x, y, data=nz, geom="path")) 
#p <- ggplot(finmap, aes(x = long, y = lat)) 
#df <- data.frame(list(x = finmap$long, y = finmap$lat))
#p <- qplot(x, y, data = df) + geom_polygon(aes(group=group, fill = x))
#q <- p + geom_polygon(aes(group=group, fill=mediaanitulo), colour="black")
#q <- p + coord_map(project = "gilbert") 
#print(q)

#################################################
nams <- list()
for (shape.file in c("AVI1_p.shp", "dcont_p.shp", "hcont_p.shp", "lake_p.shp", "pelto.shp", "suot.shp", "coast_p.shp", "forest.shp", "kunta1_p.shp", "maaku1_p.shp", "rivera_p.shp", "taajama.shp")) {
  sp2 <- readShapePoly(system.file(paste("extdata/Maanmittauslaitos/1_milj_Shape_etrs_shape/", shape.file, sep = ""), package = "sorvi"))

  print(shape.file)
  #print(names(sp2))
  print(head(sp2@data))
  nams[[shape.file]] <- names(sp2)

}

