# biclust requires installing curl and setting curl-config path
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

# GITHUB API limit exceeded. See if number of pkgs can be reduceed.

#source('http://www.bioconductor.org/biocLite.R')
#update.packages()
#biocLite()

install.packages("BiocManager")
library(BiocManager)
source("installation_pkgs.R")
pkgs <- c(cran.pkgs, bioc.pkgs)

suppressUpdate <- TRUE
update <- FALSE

for (pkg in pkgs) {
  if( !require(pkg) ){
    print(pkg)
    BiocManager::install(pkg, suppressUpdates = suppressUpdate, update=update)
  }
}

library(extrafont, font_import())
# options(repos = c(getOption("repos", rstan = "http://wiki.rstan-repo.googlecode.com/git/")))

library("devtools")
source("install_github.R")
# source("install_universe.R")

#upgrade.packages()
update.packages(checkBuilt=TRUE)