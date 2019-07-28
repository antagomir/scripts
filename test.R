load("AtlasFull.RData")

library(HITChipDB);
pet = read_hitchip("~/proj/pet14/data/")
pet$probedata
petL2A = otu_table(tax_glom(pet$pseq, "L2"))
petL2B = read.csv("~/proj/pet14/data/L2-frpa.tab", sep = "\t", row.names = 1)


comc = intersect(colnames(petL2A), colnames(petL2B))
#comr = intersect(rownames(petL2A), rownames(petL2B))
plot(as.vector(petL2A[, comc]), as.vector(unlist(petL2B[, comc])), pch = "."); abline(0,1)

b = pet$probedata
a = atlas$oligo[, match(colnames(pet$probedata), atlas.metadata[["MySQL-sampleID"]])]
k = sample(nrow(a), 1)
plot(a[k, ], b[k, ])
plot(as.vector(a), log10(as.vector(unlist(b))), pch = "."); abline(0,1)