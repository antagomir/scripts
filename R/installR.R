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

source('http://www.bioconductor.org/biocLite.R')
biocLite()

install.packages("httr")
install.packages("devtools")

# Install all R deps
# system("sudo apt-get build-dep r-base-dev")

################################################################################

# Microarrays

#### Probe-level analysis ####
#The oligo package handles oligonucleotide
#arrays: expression, tiling, SNP and exon chips. The supported
#manufacturers are Affymetrix and NimbleGen. The package provides tools
#for preprocessing.
biocLite("oligo")
#biocLite("altcdfenvs")

# Misc
biocLite("genefilter")
biocLite("BiocStyle")

#####################################################

# Machine learning

#### Graphical gaussian models ####

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
install.packages("entropy")
install.packages("mixtools")

# Classification
biocLite("pamr")

# Topic models #
biocLite("DPpackage")          # Dirichlet Processes / topic models et

#### Linear models ####
install.packages("lme4")
biocLite("glmnet")             # Lasso, elastic net, regularized generalized linear models

biocLite("qvalue")
install.packages("NMFN")  # NMF
install.packages("randomForest")


################################################################

#### Utilities

################################################################

# matrix operations 
biocLite("Matrix")  # boosting matrix calculations

# ddply for generating tables for various statistical summaries of given data
install.packages("ellipse")

#### Public databases ###
biocLite("Biobase")
biocLite("XML") # libxml2-dev, xml2 with synaptic

# Microbiome analysis
biocLite("dada2") 

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
biocLite("biomaRt")# Requires RCurl

# Excel reading utilities                                        
install.packages("RODBC")
install.packages("gdata")

# Networks
biocLite("NCIgraph")

#######################################################################

# Visualization / graphics

# Including links and tips to plots, potentially useful in
# delivering end results to biomedical guys
biocLite("RSVGTipsDevice")  
biocLite("tikzDevice")
biocLite("ape") # circular hclust dendrograms etc
biocLite("RSQLite")
biocLite("graph")
biocLite("RbcBook1")
biocLite("Rgraphviz") # graphviz and its dependencies
install.packages("igraph" ) #see http://cneurocvs.rmki.kfki.hu/igraph/download.html
install.packages("plotrix")

################################################################

# Document generation

install.packages("brew")
install.packages("cacheSweave")
#install.packages("pgfSweave") # cool graphics for Sweave. Did not install to R211, try later.

# Parallel computation #
install.packages("foreach")
install.packages("doMC")

# data for parallel examples #
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("nws")

#####################################################

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
install.packages("sp")
install.packages("maptools")
install.packages(c("gpclib", "maps", "spdep"))
install.packages("outliers")

##########################################################

install.packages("pxR")
install.packages(c("stringr", "formatR", "roxygen2"))
install.packages("rworldmap")
install.packages("rworldxtra")
install.packages("Rd2roxygen")
install.packages("akima")
install.packages("googleViz")

#########################################################

library(devtools)
install_github('slidify', 'ramnathv')
install_github('slidifyLibraries', 'ramnathv')

install.packages("knitr")
install_github('whisker', 'edwindj')
install_github('markdown', 'rstudio')

# Required for rgl which is mixOmics dependency
install.packages("rgl")
install.packages("flexmix")
install.packages("mixOmics")

install.packages(c("vars"))
install.packages("wordcloud")
install.packages("fields")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gplots")
install.packages("WGCNA")
install.packages("dmt")
install.packages("ggm")
install.packages("WDI")
install.packages("ber")
install.packages("raster")
install.packages("xtable")
install.packages("rgeos") # install GEOS first from http://trac.osgeo.org/geos/

# microbiome
biocLite("sva")
biocLite("affy")
biocLite("limma")
biocLite("preprocessCore")
biocLite("MASS")
biocLite("minet")
biocLite("nlme")
biocLite("outliers")
biocLite("RColorBrewer")
biocLite("R2HTML")
biocLite("svDialogs")
biocLite("vegan")
biocLite("WGCNA")

install.packages("devtools")
library(devtools)
install_github("ropengov/gisfin")

#library(devtools)
#install_github(repo = "sorvi", username = "ropengov", ref = "master")
install.packages(c("countrycode", "plotrix"))
install.packages("reshape2")
install.packages("ggthemes")
#install.packages("statfi")

# Needed for earlywarnings
install.packages("moments")
install.packages("nortest")
install.packages("Kendall")
install.packages("som")
install.packages("tseries")
install.packages("fNonlinear")
install.packages("splus2R")
install.packages("ifultools")
install.packages("sapa")
install.packages("wmtsa")
install.packages("scatterplot3d")
library(devtools)
install_github(repo = "earlywarnings-R", username = "earlywarningtoolbox", subdir = "earlywarnings")
install.packages("fpc") # Prediction strength

# RStan
install.packages('inline')
install.packages('Rcpp')
options(repos = c(getOption("repos"), rstan = "http://wiki.rstan-repo.googlecode.com/git/"))
install.packages('rstan', type = 'source')


# Modality tests
install.packages("diptest")
install.packages('shiny')
install.packages('VGAM')
install.packages('GPArotation') # required by psych pkg
install.packages('psych') # factor analysis with BIC & RMSEA
install.packages('mapproj')
install.packages('googleVis')
install.packages('highlight')

require(devtools)
install_github(c('slidify', 'slidifyLibraries'), 'ramnathv', ref = 'dev')
install_github('rCharts', 'ramnathv')
install_github('shiny', 'rstudio')

biocLite("impute")
install.packages("lawstat")
install.packages("animation") 
install.packages(c("psych", "GPArotation")) # factor analyses

# earlywarnings deps
install.packages(c("Kendall", "moments", "nortest", "quadprog", "som", "tgp", "tseries"))

# Unifrac
install.packages("GUniFrac")

install.packages("downloader")
install.packages( c( "memisc" , "httr" ) )

install.packages("abind")

install.packages("RUnit")

# Compositions and its deps
install.packages(c("tensorA", "robustbase", "energy", "bayesm"))
install.packages("compositions")
install.packages("robCompositions")

# Misc
install.packages(c("xlsx", "xlsxjars"))
install.packages("getopt")
install.packages("XLConnect")
install.packages("rJava")
install.packages("tidyr")
install.packages("data.table")
install.packages("multcomp")
install.packages("fNonlinear")

install.packages("Rwave")
install.packages("fractal")
install.packages("fractaldim")
install.packages("pracma")

install.packages("rdryad")
install.packages("knitcitations")
install.packages("rmarkdown")
install.packages("testthat")

install.packages("extrafont")
library(extrafont)
font_import()

devtools::install_github("rstudio/rmarkdown")
devtools::install_github("hadley/babynames")
install.packages("rjson")
install.packages("permute")
install.packages("ade4")
biocLite("df2json")
install.packages("fastcluster")
install.packages("vegan")
biocLite("BiocCheck")
biocLite("phyloseq")
install.packages("tau")
install.packages("ggmap")
install.packages("pROC")
install.packages("plotROC")
install.packages("ggROC")
install.packages("tibble")
install.packages("rvg")
install.packages("FD")

install.packages("scimapClient", repos="http://scisoft-net-map.isri.cmu.edu/static/R")

install.packages("tm")
install.packages("funModeling")
install.packages("GGally")
install.packages("intergraph")
install.packages("ggigraph")
install_github("briatte/ggnet")
install.packages(c("gender", "genderdata"),
                 repos = "http://packages.ropensci.org",
                 type = "source")
install_github("kalimu/genderizeR")

  

# Jupyter

install.packages(c('pbdZMQ', 'repr', 'devtools'))  # repr is already on CRAN

# RMySQL
#system("wget http://cran.r-project.org/src/contrib/RMySQL_0.9-3.tar.gz")
install.packages("RMySQL")
biocLite("DBI") #RMySQL is a dependency?, AnnBuilder

install_github("antagomir/netresponse")
install_github("antagomir/RPA")
install.packages("Cairo")
install.packages("openxlsx")
install.packages("FGN")
install.packages("deSolve")
install.packages("untb")
install.packages("Voss")
install.packages("stinepack")
install.packages("geigen")
install.packages("ggrepel")
install.packages("rio")

# Some problem
#install_github("microbiome/microbiome")
#install_github("microbiome/HITChipDB")
# biocLite("DirichletMultinomial") # DMMs
install_github("ropensci/genderdata")
install.packages("babynames")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gender")
install.packages("knitr")
install.packages("magrittr")
install.packages("sorvi")
install.packages("stringr")
install.packages("tau")
install.packages("tibble")
install.packages("tidyr")
install.packages("tm")
install.packages("DT")
install.packages("cowplot")
install.packages("plotly")
install.packages("readr")
install.packages("flexdashboard")
install.packages("stringdist")
install_github("christophergandrud/networkD3")
install_github("wch/webshot")
install.packages("Cairo")
install_github("zdk123/SpiecEasi")


