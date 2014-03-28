#
# Resourcerer fetcher array annotations from TIGR
#

require("Resourcerer")

resourcerer <- getResourcerer("Agilent_HumanGenome.zip", organism = "Human",
            destDir = file.path("/share/work/lmlahti/tmp/"),
            baseUrl = "ftp://occams.dfci.harvard.edu/pub/bio/tgi/data/Resourcerer/",
            clean = TRUE, exten = "zip")

resourcerer[1:3, ]

#This function allows users to create a bioC annotation data package
#for the probes contained in the Resourcerer annotation file.

#resourcerer2BioC(which, organism = c("human", "mouse", "rat"), destDir
#               = file.path(.path.package("Resourcerer"), "temp"),
#               pkgName, pkgPath, srcUrls = getSrcUrl("all",
#               switch(organism, human = "Homo sapiens", mouse =
#               "Mus musculus", rat = "Rattus norvegicus")), otherSrc =
#               NULL, baseMapType = c("gb", "ug", "ll"), version =
#               "1.1.0", makeXML = TRUE, fromWeb = TRUE, baseUrl =
#               "ftp://ftp.tigr.org/pub/data/tgi/Resourcerer", check =
#               FALSE, author = list(author = "Anonymous", maintainer =
#               "anonymous@email.com"), exten = "zip")
#############################################################
  ## The example takes a loooong time (about an hour) to run ##
#############################################################
#  if(interactive()){
#    resourcerer2BioC("Agilent_Human1_cDNA.zip")
#    unlink(file.path(.path.package("Resourcerer"), "temp",
#           "AgilentHuman1cDNA"), TRUE)
#  }    
