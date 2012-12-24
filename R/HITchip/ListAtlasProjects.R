
library(microbiome)

# Database projects                                        
require(RMySQL)
drv <- dbDriver("MySQL")
dbuser <- 'root';
dbpwd <- 'fidipro';
dbname <- 'phyloarray';
host <- '127.0.0.1'
port <- 3307
con <- dbConnect(drv, username = dbuser, password = dbpwd, dbname = dbname, host = host, port = port)
ps <- fetch.projects(con)
d <- sort(unique(ps$projectName))

# Current atlas projects
source("~/Rpackages/scripts/R/HITChip/atlas.R")
a <- ListAtlasProjects()

# New projects to add
d <- sort(unique(ps$projectName))

# project sample numbers
my.projects <- ListAtlasProjects()
project.info <- fetch.sample.info(my.projects, chiptype = "Agilent-016089", dbuser, dbpwd, dbname, host = host, port = port)

# ---------------------------

check.projects <- c(
                    "A011",
                    "BROAD",
                    "BURGERS ZOO",
                    "CARIEN",
                    "CENT",
                    "COLINE",
                    "CREST STUDY",
                    "DIA_IMMUNE_2",
                    "DOCTOR HASSAN STUDY",
                    "DYSBIOSIS",
                    "Danone",
                    "Danone I",
                    "ELECTRODE",
                    "ENDOBARRIER",
                    "ENDOBARRIER 2",
                    "EVOTAR",
                    "Extract",
                    "FONSSTUDY",
                    "FOSMID STUDY",
                    "Gilmor study",
                    "Gilmore",
                    "HBD_CASE",
                    "INRA",
                    "META RESISTANCE PROJECT",
                    "MRP",
                    "MTS",
                    "MULTIRESIST PROJECT",
                    "NAM PROJECT",
                    "Poliphenol study",
                    "RNAlater",
                    "ROTTERDAM COHORT STUDY",
                    "RUBER",
                    "RVU STUDY EDO BRUNNER",
                    "SAG PROJECT",
                    "SAN ANTONIO SAMPLES",
                    "SWAB",
                    "TIMGOS",
                    "TNO",
                    "Trento",
                    "UCM_NEONATES",
                    "UTRECHT MEDICAL CENTRE",
                    "Unilever",
                    "WUR",
                    "classified09032009",
                    "empty",
                    "unilver six")


project.info2 <- fetch.sample.info(check.projects, chiptype = "Agilent-016089", dbuser, dbpwd, dbname, host = host, port = port)

pdf("~/tmp/atlas.projects.samplesizes.pdf", height = 10, width = 7);
par(mar = c(2, 10, 1, 1));
barplot(rev(sort(table(project.info$projectName))), horiz = T, las = 1, cex.names = 0.6, main = "Number of samples");
dev.off()

pdf("~/tmp/new.projects.samplesizes.pdf", height = 10, width = 7);
par(mar = c(2, 10, 1, 1));
barplot(rev(sort(table(project.info2$projectName))), horiz = T, las = 1, cex.names = 0.6, main = "Number of samples");
dev.off()


 #write.table(mat, file = "~/tmp/newprojects.tab", quote = FALSE, col.names = FALSE)
