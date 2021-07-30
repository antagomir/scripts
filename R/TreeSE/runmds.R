 

library(mia); packageVersion("mia")
library(dplyr); packageVersion("dplyr")
library(philr)

data(GlobalPatterns, package = "mia")

## Select prevalent taxa 
tse <-  GlobalPatterns %>% subsetByPrevalentTaxa(
                               detection = 3,
                               prevalence = 20/100,
                               as_relative = FALSE)

## Pick taxa that have notable abundance variation across sammples
variable.taxa <- apply(assays(tse)$counts, 1, function(x) sd(x)/mean(x) > 3.0)
tse <- tse[variable.taxa,]
# Collapse the tree!
# Otherwise the original tree with all nodes is kept
# (including those that were filtered out from rowData)
tree <- ape::keep.tip(phy = rowTree(tse), tip = rowLinks(tse)$nodeNum)
rowTree(tse) <- tree

## Add a new assay with a pseudocount 
assays(tse)$counts.shifted <- assays(tse)$counts + 1 

gp.philr <- philr(tse, abund_values = "counts.shifted",
                  part.weights='enorm.x.gm.counts', 
                  ilr.weights='blw.sqrt')

# Store the phILR transformed abundances into the altExp slot
# Note that the phILR transformed data has n-1 features (n in original data)
altExp(tse, "phILR") <- SummarizedExperiment(SimpleList(philr=t(gp.philr)))

# Euclidean distances between samples with phILR transformed data
# Note that we have stored the phILR data in the altExp slot named "phILR"
# in the TreeSE object; and the data is in the "philr" assay in the altExp.
library(scater)
tse <- runMDS(tse, exprs_values="philr", FUN=dist, altexp="phILR", name="MDS_phILR")

# Visualize the ordinated data (with reduced dimensionality)
scater::plotReducedDim(tse, "MDS_phILR", colour_by = "SampleType") +
  labs(x = "PC1", y = "PC2", title = "Euclidean distances with phILR")


