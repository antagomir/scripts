

BiocManager::install("affy", suppressUpdates = FALSE)
BiocManager::install("ALDEx2", suppressUpdates = FALSE)
BiocManager::install("ape", suppressUpdates = FALSE) # circular hclust dendrograms etc
BiocManager::install("Biobase", suppressUpdates = FALSE)
BiocManager::install("BiocCheck", suppressUpdates = FALSE)
BiocManager::install("BiocStyle", suppressUpdates = FALSE)
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
BiocManager::install("biomaRt", suppressUpdates = FALSE)# Requires RCurl
BiocManager::install("cMAP", suppressUpdates = FALSE)
BiocManager::install("dada2", suppressUpdates = FALSE) 
BiocManager::install("DBI", suppressUpdates = FALSE) #RMySQL is a dependency?, AnnBuilder
BiocManager::install("DESeq2", suppressUpdates = FALSE)
BiocManager::install("df2json", suppressUpdates = FALSE)
BiocManager::install("DirichletMultinomial", suppressUpdates = FALSE) # DMMs; may require libgsl0-dev
# BiocManager::install("DPpackage", suppressUpdates = FALSE) # Dirichlet Processes / topic models et
BiocManager::install("EMDomics", suppressUpdates = FALSE)
BiocManager::install("ExperimentHub", suppressUpdates = FALSE)
BiocManager::install("ExperimentHubData", suppressUpdates = FALSE)
BiocManager::install("FD", suppressUpdates = FALSE)
BiocManager::install("genefilter", suppressUpdates = FALSE)
BiocManager::install("ggbio", suppressUpdates = FALSE)
BiocManager::install("ggiraph", suppressUpdates = FALSE)
BiocManager::install("glmnet", suppressUpdates = FALSE) # Lasso, elastic net, regularized generalized linear models
BiocManager::install("GO.db", suppressUpdates = FALSE)
BiocManager::install("graph", suppressUpdates = FALSE)
BiocManager::install("impute", suppressUpdates = FALSE)
BiocManager::install("kohonen", suppressUpdates = FALSE) # better than 'som' package
BiocManager::install("limma", suppressUpdates = FALSE)
BiocManager::install("MASS", suppressUpdates = FALSE)
BiocManager::install("M3C")
BiocManager::install("Matrix", suppressUpdates = FALSE) # boosting matrix calculations
BiocManager::install("minet", suppressUpdates = FALSE)
BiocManager::install("MultiAssayExperiment", suppressUpdates = FALSE)
BiocManager::install("NCIgraph", suppressUpdates = FALSE)
BiocManager::install("nlme", suppressUpdates = FALSE)
BiocManager::install("oligo", suppressUpdates = FALSE)
BiocManager::install("outliers", suppressUpdates = FALSE)
BiocManager::install("pamr", suppressUpdates = FALSE)
# BiocManager::install("phyloseq", suppressUpdates = FALSE)
BiocManager::install("preprocessCore", suppressUpdates = FALSE)
BiocManager::install("qvalue", suppressUpdates = FALSE)
BiocManager::install("R2HTML", suppressUpdates = FALSE)
BiocManager::install("RbcBook1", suppressUpdates = FALSE)
BiocManager::install("RColorBrewer", suppressUpdates = FALSE)
BiocManager::install("Rgraphviz", suppressUpdates = FALSE) # graphviz and its dependencies
BiocManager::install("RSQLite", suppressUpdates = FALSE)
BiocManager::install("RSVGTipsDevice", suppressUpdates = FALSE)
BiocManager::install("SummarizedExperiment")
BiocManager::install("sva", suppressUpdates = FALSE)
BiocManager::install("svDialogs", suppressUpdates = FALSE)
BiocManager::install("tikzDevice", suppressUpdates = FALSE)
BiocManager::install("TreeSummarizedExperiment")
BiocManager::install("SingleCellExperiment")
BiocManager::install("scater")
BiocManager::install("vegan", suppressUpdates = FALSE)
BiocManager::install("WGCNA", suppressUpdates = FALSE)
BiocManager::install("XML", suppressUpdates = FALSE) # libxml2-dev, xml2 with synaptic
BiocManager::install("netresponse")