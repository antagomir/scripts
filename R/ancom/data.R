# OTU data: data frame (sample x OTUs) .
# The first column should be the sample identifier with column name “Sample.ID”.
library(microbiome)
library(dplyr)
data(dietswap)
pseq <- dietswap

# Set baseline to 0 (in this data set it appears to be 1)
pseq <- transform(pseq, "shift", shift = -1)

otu_test <- t(abundances(pseq))
otu_test <- bind_cols(data.frame(Sample.ID = rownames(otu_test)), data.frame(otu_test))
otu_test$Sample.ID <- as.character(otu_test$Sample.ID)

# 2. Metadata: data frame with the
# first columns being the sample identifier with column name
# “Sample.ID” and each following column being the variables
map_test <- meta(pseq)
map_test <- bind_cols(data.frame(Sample.ID = rownames(map_test)),
                       map_test)
map_test$Sample.ID <- as.character(map_test$Sample.ID)


otu <- otu_test
met <- map_test