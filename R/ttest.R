# Example data

library("affy")
library("affydata")
data(Dilution)
eset <- rma(Dilution)
emat <- exprs(eset)

###########################

# Fast t-test

# specify class labels for matrix columns
emat.classes <- factor(c("a","a","b","b"))

library("genefilter")
tt <- rowttests(emat, emat.classes)
pvalues <- tt[,"p.value"]

####################################

# An alternative t-test with more options (see ?t.test)

inds.a <- which(emat.classes == "a")
inds.b <- which(emat.classes == "b")
pvalues <- apply(emat, 1, function (x) {t.test(x[inds.a], x[inds.b])$p.value})

####################################

# Calculate q-values

require(qvalue)
qstat <- qvalue(pvalues)
qvalues <- qstat$qvalues
