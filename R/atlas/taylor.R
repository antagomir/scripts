library(microbiome)
data(atlas1006)

library(reshape2)
x <- abundances(transform(atlas1006, "compositional"))
df <- melt(x)
m <- rowMeans(x)
df$mean <- m[match(df$Var1, names(m))]


p <- ggplot(df, aes(x = mean, y = value)) + geom_jitter() + labs(x = "Mean relative abundance (%)", y = "Relative abundance (%)") + theme_bw(20) + scale_y_continuous(label = scales::percent, trans = "sqrt", breaks = c(1/100, 2/100, 5/100, 10/100, 20/100, 50/100, 75/100)) + scale_x_continuous(label = scales::percent, trans = "sqrt", breaks = c(0.1/100, 0.5/100, 1/100, 2/100, 5/100, 10/100))

jpeg("taylor.jpeg")
print(p)
dev.off()
