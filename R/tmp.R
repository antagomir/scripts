# Needed for earlywarnings
install.packages("moments")
install.packages("nortest")
install.packages("Kendall")
install.packages("som")
install.packages("tseries")
install.packages("splus2R")
install.packages("ifultools")
install.packages("sapa")
install.packages("wmtsa")
install.packages("scatterplot3d")
library(devtools)
install_github(repo = "earlywarnings-R", username = "earlywarningtoolbox", subdir = "earlywarnings")
install.packages("fpc")

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
install.packages('ade4')
install.packages('highlight')
biocLite("df2json")

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

# Compositions and its deps
install.packages(c("tensorA", "robustbase", "energy", "bayesm"))
install.packages("compositions_1.40-0.tar.gz", repos = NULL)

# Misc
install.packages(c("xlsx", "xlsxjars"))


install.packages("extrafont")
library(extrafont)
font_import()

