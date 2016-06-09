# Now create a random phylogenetic tree with the ape package, and add it to your
# dataset. Make sure its tip labels match your OTU_table.
#library("ape")
#random_tree = rtree(ntaxa(physeq), rooted = TRUE, tip.label = taxa_names(physeq))
#plot(random_tree)

# Ks. HITChip-fylogenian muodostus: Science13/Analyses 
# ~/proj/old_proj/2014/2014-NatComm-TippingElements/Analyses-June2012/unifrac.R
#library(ape)
#library(GUniFrac)
#library(microbiome)
#species.data <- atlas.data.1000$species$frpa[, colnames(datr)]
# Species genbank ID mapping
# species.gid.map=read.csv("/Volumes/h901/fidipro/jarmo/Qiime/HITChip-output_phylogeny2.csv")
# Read phylogenetic tree constructed with Sate
#tre=read.tree("/Volumes/h901/fidipro/jarmo/sate_align_070812_revcomp/satejob.tre")
#tre$tip.label=gsub("'","",toupper(tre$tip.label))
#outgroup="ATCC33500ROOT"
# root tree at outgroup
#root.tre=root(tre,outgroup=toupper(outgroup),resolve.root=T)
#Change labels
#t1=as.character(species.gid.map[match(root.tre$tip.label,species.gid.map[,4]),3])
#t1[is.na(t1)]=outgroup
#root.tre$tip.label=t1
#remove outgroup
#root.tre=drop.tip(root.tre,outgroup)

trefile <- "~/data/HITChip/Phylotree/HITdb_bacteria.newick"
#qiimex <- import_qiime(trefile, showProgress = FALSE)
require(phytools)
source("read.newick.R")
tree <- read.newick(file=trefile)
tree2 <- collapse.singles(tree)
plotTree(tree2,type="fan")

load("~/proj/hitchip-atlas/20150407/phylogeny.info.remap.RData")
phylogeny.info.remap


