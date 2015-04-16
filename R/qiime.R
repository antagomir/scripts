# http://joey711.github.io/phyloseq/download-microbio.me.html

library("phyloseq")
packageVersion("phyloseq")

library("ggplot2")
packageVersion("ggplot2")
theme_set(theme_bw())

# vagina - vaginal microbiome - Ravel_reproductive_women_vagina
vagina <- microbio_me_qiime(509)

dlist = list(vagina)
dlist = lapply(dlist, function(physeq) {
    physeq = prune_taxa(taxa_sums(physeq) > 0, physeq)
    physeq = prune_samples(sample_sums(physeq) > 100, physeq)
})


plot_richness(vagina, x = "NUGENT_SCORE", color = "ETHNICITY") + stat_smooth(method = lm)
plot_richness(vagina, x = "PH", color = "ETHNICITY") + stat_smooth(method = lm)
plot_heatmap(vagina)

