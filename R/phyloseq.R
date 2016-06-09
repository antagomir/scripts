library(phyloseq)
data(enterotype)
enterotype
#> slotNames(enterotype)
#[1] "otu_table" "tax_table" "sam_data"  "phy_tree"  "refseq"   

# OTU x Sample; relative abundances; matrix; class: otu_table
enterotype@otu_table

# OTU x 1: OTU names
enterotype@tax_table

# Samples x variables: sample metadata data.frame
head(enterotype@sam_data)

# Optional (empty in enterotypes data set)
# Phylo tree (how about JSa's version for UniFrac)
# class(esophman@phy_tree) # "phylo"

# Optional (empty in enterotypes data set)
# enterotype@refseq

# ------------------------------------

source("Atlas.R") # meta, data

library(microbiome)
x <- relative.abundance(t(data))

