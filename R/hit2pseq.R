#  HITChip phyloseq 

library(phyloseq)

# Construct OTU matrix for the public data
# source("Atlas.R") # meta, data

library(microbiome)
data(peerj32)
data <- peerj32$microbes
meta <- peerj32$meta
physeq <- hitchip2physeq(data, meta)

library("phyloseq")
plot_bar(physeq, fill = "Phylum")
plot_heatmap(physeq, taxa.label = "Phylum")
#plot_heatmap(physeq)
#save(physeq, file = "tmp.RData")


