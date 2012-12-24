#
# Get genomic locations for miRNAs
#

# Further examples of using biomaRt at:
#http://www.bioconductor.org/workshops/2009/BioC2009/labs/biomaRtGenomeGraphs/Bioc2009_biomaRt.pdf

require(biomaRt)

ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#ensembl <- useDataset("hsapiens_gene_ensembl", mart = ensembl)
miRNA <- getBM(c("mirbase_id", "ensembl_gene_id", "start_position","end_position","chromosome_name","band"), filters = "with_mirbase", values = TRUE, mart = ensembl)

# Check more possible features with listAttributes(ensembl) 

###########################

# Alternative way

alt = FALSE
if (alt) {

# List available marts                                        
#listMarts()
ensembl <- useMart("ensembl")
# List available datasets (species etc.)
#listDatasets(ensembl) 
ensembl <- useDataset("hsapiens_gene_ensembl",mart=ensembl)
# List available filters (input)
#listFilters(ensembl) 
filts <- c("hsa-mir-486","hsa-mir-147","hsa-mir-197") 
# List available attributes (output)
#listAttributes(ensembl) 
attrs <- c("mirbase_id","chromosome_name","start_position","end_position")
# Retrieve data
ensemblGenes <- getBM(attributes = attrs, filters = "mirbase_id", values = filts, mart = ensembl) 
}
