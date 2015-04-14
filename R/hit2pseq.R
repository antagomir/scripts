library(phyloseq)

#---------------------------

# Construct OTU matrix

source("Atlas.R") # meta, data
# OTU x Sample: absolute 'read counts'
x <- t(data) - 50 # min(data, na.rm = T) = 32.3
x[x < 0] <- 0
# Discretize to get 'counts'
x <- round(x/100)
otumat <- x

# --------------------------

sampledata <- sample_data(meta)

# --------------------------

# We could also add phylotree between OTUs
# Now create a random phylogenetic tree with the ape package, and add it to your # dataset. Make sure its tip labels match your OTU_table.
#library("ape")
#random_tree = rtree(ntaxa(physeq), rooted = TRUE, tip.label = taxa_names(physeq))
#plot(random_tree)

# --------------------------

# Construct taxonomy table
#library(microbiome)
ph <- GetPhylogeny("HITChip")
ph <- unique(ph[, c("L1", "L2")])
ph$L2 <- gsub("Clostridium \\(sensu stricto\\)", "Clostridiales", ph$L2)
ph <- unique(ph[, c("L1", "L2")])
colnames(ph) <- c("Phylum", "Genus")
taxmat <- ph
rownames(taxmat) <- as.character(ph$Genus)
taxmat <- taxmat[rownames(otumat), ]
taxmat <- as.matrix(taxmat)

# rownames(otumat) <- rownames(taxmat) <- paste("OTU", 1:nrow(taxmat), sep = "-")

#----------------------------------------------

# Combine OTU and Taxon matrix into Phyloseq object
library("phyloseq")
OTU <- otu_table(otumat[, 1:10], taxa_are_rows = TRUE)
TAX <- tax_table(taxmat)
physeq <- phyloseq(OTU, TAX)
physeq <- merge_phyloseq(physeq, sampledata)
#physeq <- merge_phyloseq(physeq, random_tree)

save(physeq, file = "tmp.RData")

# -------------------------------------------------------

# For some reason does not work when microbiome is loaded
library("phyloseq")
load("tmp.RData")
plot_bar(physeq, fill = "Phylum")
plot_heatmap(physeq)
plot_heatmap(physeq, taxa.label = "Phylum")