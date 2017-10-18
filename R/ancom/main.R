library(exactRankTests)
library(nlme)
library(ggplot2)
source("funcs.R")

source("data.R")

# For comparison, try DESeq2
source("deseq2.R") # res.deseq

# Standard Test
comparison_test <- ANCOM.main(OTUdat=otu,
                           Vardat=met,
                           adjusted=FALSE,
                           repeated=FALSE, # No repeated measurements
                           main.var="nationality",
             adj.formula=NULL, # adjusted=FALSE so not being used
	     random.formula = NULL, # lme random effects
			   longitudinal = FALSE, 
                           repeat.var=NULL,
                           multcorr=2,
                           sig=0.05,
             prev.cut=0.90)
    # OTUs with proportion of zeroes greater than prev.cut are not
    # included in the analysis

comparison_test$W.taxa

# Test with covariates
comparison_test2 <- ANCOM.main(OTUdat=otu,
                           Vardat=met,
                           adjusted=TRUE,
                           repeated=F,
                           main.var="nationality",
                           adj.formula="bmi_group+sex",
			   longitudinal = FALSE, 
                           repeat.var=NULL,
                           multcorr=2,
                           sig=0.05,
                           prev.cut=0.90)

comparison_test2$W.taxa


# And test the alternative ANCOM implementation
source("ancom_anotherimplementation.R")
padj <- ancom(pseq, "nationality")
ancom3 <- names(which(padj < 0.01))
# Doesnt seem to match very well:
#> mean(gsub(" ", ".", ancom3) %in% ancom1)
#[1] 0.4782609
#> mean(gsub(" ", ".", ancom3) %in% ancom2)
#[1] 0.4782609
#> mean(gsub(" ", ".", ancom3) %in% taxa.deseq)
#[1] 0.06521739
#> mean(gsub(" ", ".", ancom3) %in% taxa.anova)
#[1] 0.02173913



source("~/Rpackages/microbiome/inst/extdata/check_anova.R"); ano <- check_anova(pseq, "nationality"); taxa.anova <- gsub(" ", ".", unique(as.character(subset(ano, padj < 0.01)$taxa)))
#source("~/Rpackages/microbiome/inst/extdata/check_wilcoxon.R"); wil <- check_wilcoxon(pseq, "nationality"); taxa.wilcoxon <- unique(as.character(subset(wil, padj < 0.01)$taxa))
taxa.ancom1 <- as.character(subset(comparison_test$W.taxa, detected_0.9 == TRUE)[, 1:2]$otu.names)
taxa.ancom2 <- as.character(subset(comparison_test2$W.taxa, detected_0.9 == TRUE)[, 1:2]$otu.names)
taxa.deseq <- gsub(" ", ".", subset(res.deseq, padj < 0.01 & abs(log2FoldChange) > log10(10))$taxon)

# Ok Good. Most detected taxa are also detected by standard ANOVA
# thus validating the results. The lists are also shorter than in ANOVA
# indicating potentially lower FDR which is good.
# mean(taxa.deseq %in% taxa.anova)
#1] 0.8333333
# mean(taxa.ancom1 %in% taxa.anova)
#1] 0.9090909
# mean(taxa.ancom2 %in% taxa.anova)
#1] 0.9090909
# mean(taxa.ancom1 %in% taxa.deseq)
#[1] 0.9545455
# length(taxa.anova)
#1] 49
# length(taxa.deseq)
#1] 36
# length(taxa.ancom1)
#1] 22
# length(taxa.ancom2)
#1] 22


# See also ANCOM for longitudinal data:
# source("longitudinal_example.R")
