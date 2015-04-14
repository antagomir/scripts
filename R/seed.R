# https://github.com/danlbek/Seed
# Install seed dependencies
#source("http://bioconductor.org/biocLite.R")    
#biocLite("impute")
#biocLite("Heatplus")
#biocLite("preprocessCore")
#biocLite("GO.db")
#install.packages(c("shiny","vegan","WGCNA","gplots","cluster"))


library(microbiome)
data.directory <- system.file("extdata", package = "microbiome")
d <- read.profiling("L2", "frpa", data.directory)
d <- t(d)
d <- cbind(SampleID = rownames(d), d)


metadata.file <- paste(data.directory, "/metadata.tab", sep = "")
m <- read.csv(metadata.file, sep = "\t", as.is = TRUE)

write.csv(d, file = "tmpdata.csv", quote = F, row.names = F)
write.csv(m, file = "tmpmeta.csv", quote = F, row.names = F)

library(shiny)
runGitHub("Seed","danlbek")

# -----------------------------------


