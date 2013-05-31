df <- read.csv("timeline.csv", stringsAsFactors = FALSE)
title <- colnames(df)
df$year <- as.numeric(rownames(df))
df$count <- as.numeric(as.character(df[,1]))
df <- df[-1, c("year", "count")]
df <- subset(df, year < 2013)
library(ggplot2)

theme_set(theme_bw(20))
p <- ggplot(df, aes(x = year, y = count)) + geom_line() + ggtitle(paste("Microbiota / PubMed", paste(range(df$year), collapse = "-"))) + geom_point()

pdf("~/pic/omat/HITChip/MicrobiotaPubmed.pdf")
print(p)
dev.off()

