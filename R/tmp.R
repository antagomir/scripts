x <- subset_samples(phy, body_site == "UBERON:feces" & host_subject_id == "M3")

df <- meta(x)
#cm <- core_members(transform(x, "compositional"), detection = 0.1/100, prevalence = 80/100)
#df$diversity <- microbiome::diversity(x, "shannon")$shannon
cm <- rownames(tax_table(x)[grep("Prevotella", tax_table(x)[, 6]),])
df$abundance <- abundances(transform(x, "clr"))[cm[[k]],]
tax <- paste(tax_table(x)[cm[[k]],6:7], collapse = " ")
p1 <- ggplot(df,
         aes(x = days_since_experiment_start, y = abundance)) +
       geom_line() +
       geom_point()

p2 <- ggplot(df, aes(x = abundance)) +
        geom_density() +
	labs(title = tax)

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)

