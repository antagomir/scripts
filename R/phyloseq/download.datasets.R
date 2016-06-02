# http://joey711.github.io/phyloseq/download-microbio.me.html
library(phyloseq)
pseq0 <- microbio_me_qiime(509)
# Some preprocessing
pseq <- prune_taxa(taxa_sums(pseq0) > 0, pseq0)
pseq <- prune_samples(sample_sums(pseq) > 100, pseq)


plot_richness(vagina, x = "NUGENT_SCORE", color = "ETHNICITY") + stat_smooth(method = lm)

