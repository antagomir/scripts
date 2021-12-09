# Detect biclusters
library(biclust)

# Generate random example data matrix
set.seed(233452)
cmat <- matrix(rnorm(600), 20, 30)
cmat[2:4, 3:6] <- 10 # artifical bicluster
rownames(cmat) <- paste0("Sample", 1:nrow(cmat))
colnames(cmat) <- paste0("Feature", 1:ncol(cmat))

bc <- biclust(cmat, method=BCPlaid(), fit.model = y ~ m,
              background = TRUE, shuffle = 100, back.fit = 0, max.layers = 10,
              iter.startup = 10, iter.layer = 100, verbose = TRUE)

# y ~ m + a + b

# Investigate biclusters
bicluster.list <- list()

if (bc@Number>0) {   

  for (bci in seq(bc@Number)) {

    # Row and col inds for the bci'th bicluster
    rinds <- which(bc@RowxNumber[, bci])
    cinds <- which(bc@NumberxCol[bci, ])
    
    rnams <- rownames(cmat)[rinds]
    cnams <- colnames(cmat)[cinds]

    bicluster.list[[bci]] <- list(rnams, cnams)

  }
}

print("bicluster sizes")
print(t(sapply(bicluster.list, function (x) {sapply(x, length)})))

##################################################################

bicl.file="tmp.txt"
if (bc@Number > 0) {
 for (bci in 1:bc@Number) {

  rnams <- bicluster.list[[bci]][[1]]
  cnams <- bicluster.list[[bci]][[2]]   

  write(paste("BICLUSTER ", bci, sep = ""), file = bicl.file, append = TRUE) 
  write("\n", file = bicl.file, append = TRUE) 
  write(rnams, file = bicl.file, append = TRUE) 
  write("\n", file = bicl.file, append = TRUE) 
  write(cnams, file = bicl.file, append = TRUE) 
  write("\n", file = bicl.file, append = TRUE) 
  write("------------------------------\n", file = bicl.file, append = TRUE) 

 }
}
