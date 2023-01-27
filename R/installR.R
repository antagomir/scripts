
# GITHUB API limit exceeded. See if number of pkgs can be reduceed.

#source('http://www.bioconductor.org/biocLite.R')
#update.packages()
#biocLite()

install.packages("BiocManager")
library(BiocManager)
source("install_bioc.R")
source("install_cran.R")
source("install_github.R")
source("install_universe.R")
upgrade.packages()
update.packages(checkBuilt=TRUE)