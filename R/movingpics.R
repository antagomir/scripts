# devtools::install_github("twbattaglia/MicrobeDS")
library(microbiome)
library(MicrobeDS)
data("MovingPictures")

phy <- MovingPictures
sample_data(phy)$shannon <- microbiome::diversity(phy, "shannon")$shannon
x <- subset_samples(phy, body_site == "UBERON:tongue" & host_subject_id == "F4") # F4 / M3
xc <- transform(x, "clr")

p <- ggplot(meta(x),
         aes(x = days_since_experiment_start, y = shannon)) +
       geom_smooth() +
       geom_point()

pdf("~/tmp/shannon.pdf")
print(p)
dev.off()

cm <- core_members(transform(x, "compositional"), detection = 0.1/100, prevalence = 80/100)
df <- meta(x)
df$abundance <- abundances(x)[cm[[1]],]
p <- ggplot(df,
         aes(x = days_since_experiment_start, y = abundance)) +
       geom_smooth() +
       geom_point()

print(p)

#meta(x)$days_since_experiment_start