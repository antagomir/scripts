# Option I (self-made implement)

library(microbiome)
library(flexmix)
jsmat <- matrix(NA, nrow = ncol(dat), ncol = ncol(dat))
rownames(jsmat) <- colnames(jsmat) <- colnames(dat)
rd <- relative.abundance(10^dat[mytax,])
for (i in 1:(ncol(dat)-1)) {
  for (j in (i+1):ncol(dat)) {
    print(c(i,j))
    a <- rd[, i]
    b <- rd[, j]
    s <- (a + b)/2
    kldiv.a <- KLdiv(cbind(a, s), eps=10^-4, overlap=TRUE)
    kldiv.b <- KLdiv(cbind(b, s), eps=10^-4, overlap=TRUE)
    js.ab <- (kldiv.a[1, 2] + kldiv.b[1, 2])/2
    jsmat[i, j] <- js.ab
  }
}

# Option II

source("http://www.bioconductor.org/biocLite.R")
biocLite("cummeRbund")
library(cummeRbund)
jsmat2 <- JSdist(rd)

# -> OK, these give same results except that the eps in KLdiv affects 
# comparison a bit. The latter is much faster too.