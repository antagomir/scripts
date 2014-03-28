# Retrieve genomic locations for UniGenes from Ensembl

# Further examples of using biomaRt at:
#http://www.bioconductor.org/workshops/2009/BioC2009/labs/biomaRtGenomeGraphs/Bioc2009_biomaRt.pdf

require(biomaRt)

ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
unigene <- getBM(c("unigene","mirbase_id", "ensembl_gene_id", "start_position","end_position","chromosome_name"), filters = "with_unigene", values = TRUE, mart = ensembl)
