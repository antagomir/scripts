source('http://www.bioconductor.org/biocLite.R')
biocLite()

biocLite("oligo")
biocLite("qvalue")
biocLite("Matrix")  # boosting matrix calculations
install.packages("plyr")
biocLite("XML") # libxml2-dev, xml2 with synaptic
install.packages("gdata")

biocLite("RSQLite")
biocLite(c("Rgraphviz")) # graphviz and its dependencies
install.packages("igraph" ) #see http://cneurocvs.rmki.kfki.hu/igraph/download.html

# My ow packages
biocLite("RPA")
biocLite("netresponse")
install.packages("ReadImages")
install.packages(c("outliers", "R2HTML", "svDialogs", "vegan"))
install.packages("devtools")

#system("wget http://cran.r-project.org/src/contrib/RMySQL_0.9-3.tar.gz")
install.packages(c("RMySQL"))
biocLite("DBI") #RMySQL is a dependency?, AnnBuilder
install.packages("ggplot2")

# microbiome
biocLite(c("affy", "limma", "preprocessCore", "qvalue", "gplots","MASS","minet","netresponse","nlme","outliers","plyr","RColorBrewer","reshape2","R2HTML","svDialogs","vegan","WGCNA"))
install_github(repo = "microbiome", username = "microbiome")
install_github(repo = "HITChipDB", username = "microbiome")

library(devtools)
install_github(repo = "sorvi", username = "louhos", ref = "develop")

