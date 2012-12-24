#
# (c) Leo Lahti 2005-2010
# Licence: GPL >= 2
#

# Compute enrichments of GO (or KEGG) classes in the given gene list
# with respect to the overall list

require("GOstats")

# p-value threshold for significance
th <-0.001 

# set parameters
params <- new("GOHyperGParams",
              geneIds = topgenes, # my gene list 
              universeGeneIds = rownames(exprs), # total gene list
              pvalueCutoff = th,
              annotation = "hgu133a", # expression array platform
              ontology = "BP", # GO category
              conditional = TRUE, # take into account GO categ. dependencies
              testDirection = "over") # test overrepresentation

# calculate enrichment
res <- hyperGTest(params)

# investigate results
summary(res)[,c("Pvalue","Count","Size","Term")]
