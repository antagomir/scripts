
# Author: Leo Lahti

# This script installs default BioConductor packages to R.  Then it
# installs additional packages

###########################################################

# GOOD PRACTICES
 
# - When installing new packages, always try to install BioConductor
#and R packages with getBioC("myBioCpackage") and
#install.packages("myRpackage") commands as these will get the latest
#versions. Use manual installation only when repository installation
#cannot be used. In this case, place the tarball into 
#"/share/mi/bin/BioCondPackages/" and call it from there.

# - When installing new packages, add the installation command line to
#the end of this script (keep BioC and R packages separately, however)

# - When installing new versions of R and BioC, check this
#script. Replace previously manually installed packages with general
#calls getBioC("myBioCpackage") or install.packages("myRpackage")
#since those packages may have become available. Only if this fails,
#install manually.

# - When installing new versions, please install all previously
#installed packages unless there is a good reason to skip some.

###########################################################

# DEFAULT BIOCONDUCTOR INSTALLATION

#source('http://www.bioconductor.org/getBioC.R')
#getBioC()

source('http://www.bioconductor.org/biocLite.R')
biocLite()

################################################################################

# Microarrays

################################################################################

#### Gene expression arrays ####
#biocLite("hgu95a.db")
biocLite("hgu133a.db")
biocLite("hgu133aprobe")
biocLite("hgu133plus2.db")
biocLite("hgu133plus2probe") 
#biocLite("mgu74a.db")
biocLite("org.Hs.eg.db")
biocLite("hom.Hs.inp.db")
#biocLite("org.Mm.eg.db")

# Orthologs
#biocLite("org.HsMm.ortholog.db")

# Annotations
#install.packages("~/local/R/packages/AnnotationDbi_1.13.13.tar.gz", repos = NULL)
install.packages("/share/mi/data/CMG/integration-review-2010/Mullighan/hgu133ahsentrezg.db_13.0.0.tar.gz")

# Differential expression with SAM                                        
install.packages("samr")

# Misc
install.packages("glmnet")


#### Probe-level analysis ####

#The oligo package handles oligonucleotide
#arrays: expression, tiling, SNP and exon chips. The supported
#manufacturers are Affymetrix and NimbleGen. The package provides tools
#for preprocessing.
#biocLite("oligo")
biocLite("altcdfenvs")
#biocLite("CustomCDF")
biocLite("hgu133ahsensgcdf")

#### CGH analysis ####
biocLite("CGHcall")
biocLite("DNAcopy")
install.packages("packages/CGHcall_2.8.0.tar.gz",repos=NULL)
install.packages("packages/CGHbase_1.6.0.tar.gz",repos=NULL)
biocLite("CGHregions")

#install.packages("/share/mi/bin/BioCondPackages/arrayQuality_1.0.11.tar.gz",repos=NULL)
#install.packages("/share/mi/bin/BioCondPackages/cghMCR_1.14.0.tar.gz",repos=NULL)

#### micro-RNA ####

biocLite("miRNApath")
biocLite("microRNA")
#biocLite("RmiR.Hs.miRNA")
biocLite("RmiR")

#####################################################

# Machine learning

#### Graphical gaussian models ####
install.packages(c("corpcor","longitudinal","fdrtool","locfdr","GeneNet"))  

# multivariate stats
install.packages("mvtnorm") #contains rmvnorm for multivariate normal sampling 
install.packages("mvnormtest")

# Self-organizing map (SOM)
biocLite("kohonen") # better than 'som' package

# Kernel methods #
#install.packages("kernlab") #includes kernelCCA function 'kcca'
install.packages("e1071")   # SVM and many other machine learning tools

# ICA #
install.packages("fastICA")

# MCMC #
install.packages("MCMCpack")

# Clustering and mixture models
install.packages("mclust")     # Gaussian mixture EM and other useful stuff
install.packages("vabayelMix") # some variational mixture code
install.packages("entropy")
install.packages("mixtools")

# Classification
biocLite("pamr")

# Topic models #
biocLite("DPpackage")          # Dirichlet Processes / topic models etc

#### Linear models ####
install.packages("lme4")
biocLite("glmnet")             # Lasso, elastic net, regularized generalized linear models

# Statistics
biocLite("qvalue")

################################################################

#### Utilities

################################################################

#apt-get install revolution-r? # Revolution enhancements

# matrix operations 
biocLite("Matrix")  # boosting matrix calculations

# ddply for generating tables for various statistical summaries of given data
install.packages("plyr")

install.packages("ellipse")

#### Public databases ###
biocLite("Biobase")
biocLite(c("Resourcerer","XML")) # libxml2-dev, xml2 with synaptic

# Requires installing curl and setting curl-config path
# see locate libcurl and locate curl-config and
# http://www.omegahat.org/RCurl/FAQ.html
#~/local/bin/curl-7.20.1> ./configure --prefix=/users/lmlahti/bin
# make && make install

# After adding path to  .cshrc
#(line: set path=($path /home/lmlahti/bin/curl-7.21.3)
# and "source .cshrc"
# and isntalling "libcurl-ocaml-dev" with synaptic
#I got RCurl installed with:
#~/local/R/R-2.12.0/bin/R CMD INSTALL ~/local/R/packages/RCurl_1.5-0.tar.gz

source('http://www.bioconductor.org/biocLite.R')
biocLite("biomaRt")# Requires RCurl

# Pairwise integration
biocLite("tilingArray")
install.packages("~/local/R/packages/edira_1.1.0.tar.gz", repos = NULL)
install.packages("~/local/R/packages/ediraAMLdata_1.0.2.tar.gz")


install.packages("DRI") # DR-Integrator
install.packages("iCluster") 
#source("http://www.xlab.unimo.it/PREDA/PREDAinstall.R") # SODEGIR Bicciato et al
biocLite("SIM")
biocLite("PMA")
biocLite("lokern")
install.packages("packages/PREDA_0.2.14.tar.gz")
install.packages("packages/PREDAsampledata_0.1.7.tar.gz")
install.packages("~/local/R/packages/pint_1.3.67.tar.gz", repos = NULL)

#DRI edira intCNGEan SIM pint PMA

# Excel reading utilities                                        
#install.packages("RODBC")
#install.packages("gregmisc")
install.packages("gdata")


# Get R.matlab using custom commands from the author
source("http://www.braju.com/R/hbLite.R")  
hbLite(c("R.methodsS3","R.oo", "R.matlab")) #hbLite()

######################################################################

# Networks
biocLite("NCIgraph")

#######################################################################

# Visualization / graphics

# Including links and tips to plots, potentially useful in
# delivering end results to biomedical guys
biocLite("RSVGTipsDevice")  
biocLite("tikzDevice")

biocLite("ape") # circular hclust dendrograms etc

#install.packages("ellipse")

#### Graphs ####

biocLite("RSQLite")
biocLite("graph")
biocLite("RbcBook1")
biocLite(c("Rgraphviz","Ruuid")) # graphviz and its dependencies
install.packages("igraph" ) #see http://cneurocvs.rmki.kfki.hu/igraph/download.html

# This required BerkeleyDB and librdf installation to cluster machines   
# before working.
# librdf: http://download.librdf.org/source/redland-1.0.5.tar.gz
# BerkeleyDB: db-4.5.20.tar.gz from 
# http://www.oracle.com/technology/software/products/berkeley-db/index.html
# Also possibly, but probably not, needed to set
# LD_LIBRARY_PATH suitably
#biocLite("Rredland")

############################################################

# Matlab

# Get R.matlab using custom commands from the author
#source("http://www.braju.com/R/hbLite.R")  
#hbLite(c("R.methodsS3","R.oo", "R.matlab")) #hbLite()

################################################################

# Document generation

install.packages("brew")
install.packages("stashR") 
install.packages("cacheSweave")

#install.packages("pgfSweave") # cool graphics for Sweave. Did not install to R211, try later.
#biocLite("pgfSweave")

# Parallel computation #
install.packages("foreach")
install.packages("doMC")

# data for parallel examples #
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("nws")

#####################################################

### Gene Ontologies and pathways ####

biocLite(c("GOstats","goTools","topGO"))
biocLite("KEGGSOAP")
#biocLite("AmpAffyExample")
biocLite("globaltest")

# Tarca 2009 Signaling pathway impact analysis
#install.packages("/share/mi/bin/BioCondPackages/SPIA_0.1.0.tar.gz",repos=NULL)
biocLite("SPIA")

# Gene set enrichment analysis                                        
biocLite("GSA")

# My ow packages
biocLite("RPA")
biocLite("netresponse")

# Pairwise integration
#install.packages("~/local/R/packages/edira_1.0.2.tar.gz", repos = NULL)
install.packages("~/local/R/packages/intCNGEan_0.53.tar.gz", repos = NULL)
#install.packages("DRI") # DR-Integrator

#############################################################

# Geographical information
# required installation of 
# geos-dev with synaptic and 
# PROJ.4 from http://trac.osgeo.org/proj/wiki/WikiStart#Download
# in practice through 
# ~/Louhos/takomo/installation/sorvi-install-dependencies-debian.sh
install.packages("rgdal")

##############################################################

# Misc
biocLite("cMAP")
install.packages("R.utils")
install.packages("gridExtra")
install.packages("RgoogleMaps")
install.packages("ReadImages")
install.packages("sp")
install.packages("maptools")

install.packages(c("outliers", "R2HTML", "svDialogs", "vegan"))


##########################################################

# Venn diagrams
#install.packages("/share/mi/bin/BioCondPackages/venn_1.6.tar.gz",repos=NULL)
#install.packages("venn")

# Study material / book supplements

# Examples and functions for
# http://bayes.bgsu.edu/bcwr/
# Bayesian Computation With R, by Jim Albert
# See also
# http://bayes.bgsu.edu/bcwr/R%20scripts/index.html
#install.packages("LearnBayes")

install.packages("devtools")
install.packages("pxR")
install.packages(c("stringr", "formatR", "roxygen2"))
install.packages("Deducer")
install.packages("randomForest")
install.packages("mixtools")
install.packages(c("gpclib", "maps", "spdep"))
install.packages("rworldmap")
install.packages("Rd2roxygen")

# See also ~/scripts/R/twalk-examples.R
# http://www.cimat.mx/~jac/twalk/examples.R
#system("wget http://www.cimat.mx/%7Ejac/twalk/Rtwalk_1.5.1.tar.gz")
#install.packages("Rtwalk_1.5.1.tar.gz")

install.packages("akima")
install.packages("googleViz")

#########################################################

library(devtools)
install_github('slidify', 'ramnathv')
install.packages("knitr")
install_github('whisker', 'edwindj')
install_github('markdown', 'rstudio')


# Required for rgl which is mixOmics dependency
system("sudo apt-get install libglu1-mesa-dev")
#system("sudo apt-get build-dep r-cran-rgl")
install.packages(c("rgl"))
install.packages(c("flexmix"))

# RMySQL
system("wget http://cran.r-project.org/src/contrib/RMySQL_0.9-3.tar.gz")
install.packages(c("RMySQL"))
biocLite("DBI") #RMySQL is a dependency?, AnnBuilder

install.packages(c("vars"))
install.packages(c("wordcloud"))

install.packages("fields")
install.packages("ggplot2")
install.packages("gplots")
install.packages("mixOmics")
install.packages("WGCNA")
install.packages(c("dmt"))
install.packages(c("ggm"))
install.packages(c("WDI"))
install.packages(c("fastcluster"))

# Remove batch effects:
biocLite(c("sva"))
install.packages(c("ber"))

system("git clone https://github.com/hadley/staticdocs.git")
install.packages(c("testthat", "evaluate", "whisker", "highlight", "parser", "markdown"))
library(devtools)
document("staticdocs")
check("staticdocs")
build("staticdocs")
install("staticdocs")

# microbiome
source("http://www.bioconductor.org/biocLite.R")
biocLite(c("affy", "limma", "preprocessCore", "qvalue", "RPA", "brew","DBI","fields","ggplot2","gplots","MASS","minet","mixOmics","netresponse","nlme","outliers","plyr","RColorBrewer","reshape2","R2HTML","svDialogs","vegan","WGCNA"))
install_github(repo = "microbiome", username = "microbiome")
install_github(repo = "HITChipDB", username = "microbiome")


install.packages("raster")
install.packages("rjson")
install.packages("rgeos") # install GEOS first from http://trac.osgeo.org/geos/

library(devtools)
install_github(repo = "sorvi", username = "louhos", branch = "develop")
#install_github(repo = "sorvi", username = "louhos", type = "source", ref = "develop")
