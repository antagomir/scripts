# Example for retrieving cytoband information for a gene based on Ensembl Gene Identifiers, using biomaRt
# (c) jussi, 2008 (Paananen) and edited by Leo


# Uncomment lines to get additional info about specific options

library(biomaRt)

# List available marts
#listMarts()

ensembl <- useMart("ensembl")
#ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl"

# List available datasets (species etc.)
#listDatasets(ensembl) 

ensembl <- useDataset("hsapiens_gene_ensembl",mart=ensembl)

# List available filters (input)
#listFilters(ensembl) 

# List available attributes (output)
#listAttributes(ensembl) 

ids <- c("ENSG00000012048")

ensemblGenes <- getBM(attributes = c("ensembl_gene_id","band"), filters = "ensembl_gene_id", values = ids, mart = ensembl) 

# Get chr names and affysets
chr.names <- getBM(attributes = c("ensembl_gene_id","chromosome_name"), filters = "ensembl_gene_id", values = ids, mart = ensembl) 
affy.sets <- getBM(attributes = c("ensembl_gene_id","affy_hg_u133_plus_2"), filters = "ensembl_gene_id", values = ids, mart = ensembl) 


 
