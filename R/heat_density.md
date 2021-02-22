

```r
library(vegan)
library(reshape2)
library(microbiome)
library(scales)
library(ggplot2)
library(MASS)
set.seed(423542)

method <- "PCoA"
trans <- "compositional"
distance <- "bray"

data(atlas1006)

# Distance matrix for samples
mypseq <- atlas1006
ps <- transform(mypseq, trans)
ord <- ordinate(ps, method, distance)
proj <- ord$vectors[, 1:2] # same results as with  cmdscale(vegdist(t(abundances(ps)), "bray")) as expected
myd <- proj[sample_names(ps), 1:2]

group1 <- sample_names(subset_samples(mypseq, gender == "female"))
group2 <- sample_names(subset_samples(mypseq, gender == "male"))

  ddd <- proj[group1,]
  x <- ddd[,1]
  y <- ddd[,2]
  zh <- kde2d(x, y, n = 100, lims = c(range(proj[,1]), range(proj[,2])))

  ddd <- proj[group2,]
  x <- ddd[,1]
  y <- ddd[,2]
  zc <- kde2d(x, y, n = 100, lims = c(range(proj[,1]), range(proj[,2])))

  dif <- (zc$z - zh$z) # * (zc$z + zh$z)

  dfm <- melt(dif)
  colnames(dfm) <- gsub("^X", "Var", colnames(dfm))  

  s <- round(seq(round(min(proj[,1]), 1), round(max(proj[,1]), 1), 0.1), 1)[1:7]
  sx <- 100 * ((s - min(proj[,1]))/diff(range(proj[,1])))

  t <- round(seq(round(min(proj[,2]), 1), round(max(proj[,2]), 1), 0.1), 1)
  tx <- 100 * ((t - min(proj[,2]))/diff(range(proj[,2])))

  p <- ggplot(data = dfm, aes(x=Var1, y=Var2, fill=value)) +
         geom_tile() +
         scale_fill_gradient2(low = muted("blue"),
                              mid = "white",
			      high = muted("red"), midpoint = 0, limits = c(-5.5, 5.5)) 
print(p)
```

![plot of chunk heatdensity](figure/heatdensity-1.png)
