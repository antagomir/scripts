# Experimental misc scripts, not working yet

human2mouse.entrez <- function (humanEGIds) {

  # This shows how to convert entrez GeneIDs from human to mouse
  # homologs using inparanoid
  # Note: inparanoid has 35 organisms, and by default does not work with
  # EntrezIDs, hence org.Hs/org.Mm databases used here
  
  require(org.Hs.eg.db)
  require(hom.Hs.inp.db)
  require(org.Mm.eg.db)
  
  #Inparanoid uses ensembl protein IDs so start with those. Notice that
  #there will be many protein IDs returned for #a typical gene since
  #there are many possible translations.
  humanProtIds <- mget(na.omit(as.vector(unlist(humanEGIds))), org.Hs.egENSEMBLPROT)
  humanProtIds <- na.omit(as.vector(unlist(humanProtIds)))
  
  #Map the IDs that we can from inparanoid. Notice that by design,
  #inparanoid only represents each gene product with a single
  #translation product. Therefore your list could slim down a lot during
  #this step.  Also, if the thing you are trying to match up at this
  #step has less than 100% seed status, you will not find it in this
  #step.
  if (length(humanProtIds) > 0) {
    rawMouseProtIds <- mget(na.omit(as.vector(unlist(humanProtIds))), hom.Hs.inpMUSMU,ifnotfound=NA)
  } else {return(NA)}

  #This also means that we need to clean up the NAs from our result
  #mouseProtIds <- rawMouseProtIds[!is.na(rawMouseProtIds)]
  mouseProtIds <- as.vector(na.omit(as.vector(unlist(rawMouseProtIds))))

  #Then use the mouse organism based packages to convert these IDs
  #back to an Entrez Gene ID again (this time for mouse).
  if (length(mouseProtIds) > 0) {
    #mouseEGIds <- mget(mouseProtIds, org.Mm.egMGI2EG, ifnotfound=NA)
    mouseEGIds <- na.omit(as.vector(unlist(mget(mouseProtIds, org.Mm.egENSEMBLPROT2EG, ifnotfound=NA))))
  } else { mouseEGIds <- NA }

  as.vector(unlist(mouseEGIds))

}

#hg <- rownames(su2h)
hg <- c("2741", "9312", "4249")
mg <- lapply(hg, function (eg) {human2mouse.entrez(eg)})

####################

# Alternatively, download HomoloGene data and make mapping with that.
# FIXME: could be optimized a lot for speed.
# More intuitive and easy way would be to use data frames and plyr package for the mapping  

# Example:
source("/share/mi/scripts/homologene.R")
mat <- get.latest.homologene()
hg <- c("7499", "122704", "388419")
mg <- lapply(hg, function (eg) {human2mouse.homologene(eg)})



# See also HomoloGene (homolog.db)
#library(biomaRt)
#ensembl <- useMart("ensembl")
#ensembl <- useDataset("hsapiens_gene_ensembl",mart = ensembl)
#geneids <- c("2741", "9312", "4249")
##geneids <- rownames(su2h)[1:5]
#tmp <- getBM(attributes = "entrezgene", filters = "with_mmusculus_homolog", values = geneids, mart = ensembl)
#mget(mget(gid, org.Hs.egENSEMBLPROT),hom.Hs.inpMUSMU)
#Start with some Human Entrez Gene IDs
#humanEGIds <- c("2741", "9312", "4249")
