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

install.packages("RMySQL")
biocLite("DBI") 
install.packages("ggplot2")

# microbiome
biocLite(c("affy", "limma", "preprocessCore", "fields", "gplots","MASS","minet","netresponse","nlme","outliers","RColorBrewer","reshape","R2HTML","svDialogs","vegan","WGCNA"))

install.packages(fields)

# ReadImages not any more in CRAN, install manually
system("wget http://cran.r-project.org/src/contrib/Archive/ReadImages/ReadImages_0.1.3.3.tar.gz")
install.packages("ReadImages_0.1.3.3.tar.gz", repos = NULL)

library(devtools)
install_github(repo = "microbiome", username = "microbiome")
install_github(repo = "HITChipDB", username = "microbiome")
install_github(repo = "sorvi", username = "louhos", ref = "develop")

