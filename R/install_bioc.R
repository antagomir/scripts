suppressUpdate <- TRUE
update=FALSE

BiocManager::install("affy", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("affydata", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ALDEx2", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("alto", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ANCOMBC", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ape", suppressUpdates = suppressUpdate, update=update) # circular hclust dendrograms etc
BiocManager::install("Biobase", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("BiocCheck", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("BiocStyle", suppressUpdates = suppressUpdate, update=update)
# Requires installing curl and setting curl-config path
# see locate libcurl and locate curl-config and
# http://www.omegahat.org/RCurl/FAQ.html
#~/local/bin/curl-7.20.1> ./configure --prefix=/users/lmlahti/bin
# make && make install
# After adding path to  .cshrc
#(line: set path=($path /home/lmlahti/bin/curl-7.21.3)
# and "source .cshrc"
# and isntalling "libcurl-ocaml-dev" with synaptic
#I got RCurl installed with:
#~/local/R/R-2.12.0/bin/R CMD INSTALL ~/local/R/packages/RCurl_1.5-0.tar.gz
BiocManager::install("biclust", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("biomaRt", suppressUpdates = suppressUpdate, update=update)# Requires RCurl
BiocManager::install("bluster", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("cMAP", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("cobiclust", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ComplexHeatmap", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("curatedMetagenomicData", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("dada2", suppressUpdates = suppressUpdate, update=update) 
BiocManager::install("DBI", suppressUpdates = suppressUpdate, update=update) #RMySQL is a dependency?, AnnBuilder
BiocManager::install("DESeq2", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("df2json", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("DirichletMultinomial", suppressUpdates = suppressUpdate, update=update) # DMMs; may require libgsl0-dev
# BiocManager::install("DPpackage", suppressUpdates = suppressUpdate, update=update) # Dirichlet Processes / topic models et
BiocManager::install("EMDomics", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ExperimentHub", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ExperimentHubData", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("FD", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("fido", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("genefilter", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ggbio", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ggtree", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("ggiraph", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("glmnet", suppressUpdates = suppressUpdate, update=update) # Lasso, elastic net, regularized generalized linear models
BiocManager::install("GO.db", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("graph", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("impute", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("kohonen", suppressUpdates = suppressUpdate, update=update) # better than 'som' package
BiocManager::install("limma", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("Maaslin2", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("MASS", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("M3C", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("Matrix", suppressUpdates = suppressUpdate, update=update) # boosting matrix calculations
BiocManager::install("microsim", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("minet", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("mixOmics", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("MultiAssayExperiment", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("NCIgraph", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("nlme", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("oligo", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("outliers", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("pamr", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("pheatmap", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("philr", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("phyloseq", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("preprocessCore", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("qvalue", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("R2HTML", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("RbcBook1", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("RColorBrewer", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("rebook", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("remotes", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("Rgraphviz", suppressUpdates = suppressUpdate, update=update) # graphviz and its dependencies
BiocManager::install("RSQLite", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("RSVGTipsDevice", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("sva", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("svDialogs", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("tikzDevice", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("scater", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("SEtools", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("vegan", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("WGCNA", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("XML", suppressUpdates = suppressUpdate, update=update) # libxml2-dev, xml2 with synaptic
BiocManager::install("netresponse", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("SummarizedExperiment", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("tidySummarizedExperiment", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("TreeSummarizedExperiment", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("SingleCellExperiment", suppressUpdates = suppressUpdate, update=update)
#BiocManager::install("microbiome/microbiomeDataSets@release")
BiocManager::install("miaViz", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("microbiomeDataSets", suppressUpdates = suppressUpdate, update=update)
BiocManager::install("mia", update=update, suppressUpdates = suppressUpdate, dependencies = FALSE)
