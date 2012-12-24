require(fpc)
require(A2R) # http://addictedtor.free.fr/packages
d.usa <- dist(USArrests, "euc")
hc <- hclust(d.usa, method="ward")

set.seed(1)
some.factor <- letters[1:4][rbinom(50, prob=0.5, size=3)+1]
A2Rplot(hc, k=3, fact.sup=some.factor, boxes = FALSE, col.up = "gray", col.down = c("orange","royalblue","green3"))


# Alternative
library(sparcl)
ColorDendrogram(hc, y = annot$Donor, labels=rownames(annot), main = "", branchlength = 0.1)


