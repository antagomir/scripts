#github_pat(1)
#Sys.getenv("GITHUB_PAT")
#Sys.unsetenv("GITHUB_PAT")
#Sys.getenv("GITHUB_PAT")

library(devtools)

github_pkgs <-
c("briatte/ggnet","GuillemSalazar/EcolUtils","edwindj/whisker","ggobi/GGally","MilesMcBain/markdrive","mikemc/speedyseq","microbiome/RPA","microbiome/microbiome",'ramnathv/rCharts','ramnathv/slidify','ramnathv/slidifyLibraries','r-lib/cli',"ropengov/eurostat","ropengov/geofi","ropengov/fmi2","ropengov/helsinki","ropengov/hetu","ropengov/iotables","ropengov/pxweb","ropengov/sotkanet","ropengov/regions","ROpenSci/bibtex",'rstudio/markdown',"rstudio/rmarkdown","thomasp85/patchwork"
,"twbattaglia/MicrobeDS", "fawda123/ggord")


for (pkg in github_pkgs) {
  if( !require(pkg) ){
    print(pkg)
    remotes::install_github(pkg)
  }

}

# install_github('rstudio/shiny')
# install_github("ropensci/gender")
# install_github("wch/webshot")
# install_github("zdk123/SpiecEasi")
# install_github("FelixErnst/SEtup")
# install_github("microbiome/microbiome")
# install_github("cboettig/knitcitations")
# install_github("earlywarningtoolbox/earlywarnings-R/earlywarnings")
# install_github("jsilve24/fido")
# install_github("kalimu/genderizeR")
# install_github("reptalex/phylofactor")
