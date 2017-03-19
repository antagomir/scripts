library(phyloseq)
library(structSSI)
library(plyr)
library(dplyr)
library(reshape2)
library(DESeq2)

# Running the DESeq2 analysis
ds2 <- phyloseq_to_deseq2(pseq, ~ nationality)
dds <- DESeq(ds2)
res <- results(dds)
df <- as.data.frame(res)
df$taxon <- rownames(df)
df <- df %>% arrange(log2FoldChange, padj)
res.deseq <- df




# Just applying the variance stabilizing transformation
#varianceStabilizingTransformation(ds2, blind = TRUE, fitType = "parametric")
#ds2 <- estimateSizeFactors(ds2)
#ds2 <- estimateDispersions(ds2)
#abund <- getVarianceStabilizedData(ds2)