library(HITChipDB)

# LOCAL
#params <- run.profiling.script(dbuser = "lmlahti", dbpwd = "passu", dbname = "Phyloarray")

# FTP SERVER
params <- run.profiling.script(dbuser = "root", dbpwd = "fidipro", dbname = "phyloarray", host = '127.0.0.1', port = 3307)

#plot(as.vector(finaldata[["L2"]][["rpa"]]), as.vector(finaldata[["L2"]][["frpa"]]));abline(0,1)

level <- "L2"
esets <- list()
for (method in c("sum", "rpa", "frpa")) {
  esets[[method]] <- read.profiling(level = level, method = method, data.dir = "~/tmp/testing/", log10 = TRUE)
}
 
# names(which(is.na(eset[,1])))

load("~/data/Atlas/20120328/atlas.full.RData")

par(mfrow = c(2,2))
for (method in c("sum", "rpa")) {
  eset <- esets[[method]]
  atl <- atlas[["level 2"]][[method]]
  colnames(eset) <- gsub("\\.", "-", colnames(eset))
  coms <- intersect(colnames(atl), colnames(eset))
  plot(as.vector(eset[,coms]), as.vector(atl[, coms]), main = method, xlab = "New profiling", ylab = "Old Atlas")
}
plot(as.vector(esets[["rpa"]]), as.vector(esets[["frpa"]]), main = "RPA vs. FRPA")

# Compare profiling and new atlas
load("~/data/Atlas/20130110/atlas.full.RData")
atl2 <- atlas.data$data$L2$rpa.full
coms <- intersect(colnames(atl), colnames(eset))
plot(as.vector(eset[,coms]), as.vector(atl2[, coms]), main = method, xlab = "New profiling", ylab = "New Atlas")

