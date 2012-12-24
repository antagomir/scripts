


ensembl2affy = function (ensembl.ids, id.type = "gene", platform = "affy_hg_u133_plus_2") {
  if (id.type == "gene") {
    x <- hgu133plus2ENSEMBL
    mapped_genes <- mappedkeys(x)
    e2a <- unlist(as.list(x[mapped_genes]))
  } else  if (id.type == "transcript") {
    #id.type = "transcript"
    #platform = "affy_hg_u133_plus_2"
    #ensembl.ids = targets
	# get data from Ensembl in pieces to follow progress
        # avoid doing this more than only once!
        # For loops are not encouraged in downloading biomaRt data!
    step = 1000 
    e2a = NULL
    for (k in seq(1,length(ensembl.ids),step)) {
      print(k)
      e2a = rbind(e2a,get.ensembl(ensembl.ids[k:min((k+step),length(ensembl.ids))],platform=platform,entity="ensembl_transcript_id"))
    }
    
    #e2a = get.ensembl(ensembl.ids,platform=platform,entity="ensembl_transcript_id")
    e2a.tmp = e2a[,1]
    names(e2a.tmp) = e2a[,2]
    e2a = e2a.tmp
    rm(e2a.tmp)
  }
  e2a
} 





get.ensembl <- function (ensembl.ids,platform="affy_hg_u133_plus_2", entity = c("ensembl_transcript_id")) {

  # Usage: e2a = ensembl2affy(ensembl.ids,platform="affy_hg_u133_plus_2")

  # entity: try also "ensembl_gene_id" etc
  
  # Author: Leo Lahti 2009
  # Licence: Artistic2.0

        # See listAttributes(ensembl)[,1] for more platform options
  require(biomaRt)

  ensembl <- useMart("ensembl")
  ensembl <- useDataset("hsapiens_gene_ensembl",mart=ensembl)
  
  e2a = getBM(attributes = c(entity,platform), filters = entity, values = ensembl.ids, mart = ensembl)

        # Remove empty mappings where EnsemblIDmaps to AffySet and also to empty placeholder
  e2a.parsed = NULL
  for (id in ensembl.ids) {
    inds = which(e2a[,entity] == id)
    sets = e2a[inds,platform]
    if (length(sets)>1) {
      inds2 = (!sets == "")
      e2a.parsed = rbind(e2a.parsed,e2a[inds[inds2],])
    } else {e2a.parsed = rbind(e2a.parsed,e2a[inds,])}
  }
  e2a.parsed
}


