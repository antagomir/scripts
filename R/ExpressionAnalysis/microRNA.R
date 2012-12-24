# Leo 11/2009. Use cases of microRNA package.

require(microRNA)

data(hsTargets)

names(hsTargets)
#"name"   "target" "chrom"  "start"  "end"    "strand"

# Check first entry
i=1;
sapply(hsTargets,function(x){as.vector(x[[i]])})

# Unique mirs in the data
mirs = unique(as.vector(hsTargets[["name"]]))  

# Get targets for one miR
mir = mirs[[1]]
targets = unique(as.vector(hsTargets[["target"]][as.vector(hsTargets[["name"]])==mir]))

# Map Ensembl IDs to probesets on hgu133plus2 array
#source("/share/mi/scripts/Ensembl.R")
#e2a = ensembl2affy(targets,platform="affy_hg_u133_plus_2")

# Map Ensembl IDs to probesets using array annotation file:
x <- hgu133plus2ENSEMBL
mapped_genes <- mappedkeys(x) # Get the entrez gene IDs that are mapped to an Ensembl ID
e2a = unlist(as.list(x[mapped_genes]))

# List target probesets
sets = unique(e2a[!(e2a[,2] == ""),2])

