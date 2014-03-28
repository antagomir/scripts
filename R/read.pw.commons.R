read.pw.commons <- function (network.file = "/share/mi/data/pathways/PathwayCommons/homo-sapiens.sif") {

  # (C) Leo Lahti 2010. FreeBSD license (keep this notice).
  # Can be optimized a lot..

  #The data contain pairwise gene interactions from all pathways combined
  #(from Pathway commons). Simplified presentation of BioPAX.See the
  #README.TXT file in data dir.

  #(all pairwise interactions from pathways in PW commons for human)
  netdata <- read.table(network.file)
  # ignore the type of interaction and list each interaction just once
  netdata <- unique(netdata[, -2])
  netdata <- cbind(as.character(netdata[,1]),  as.character(netdata[,2]))
 
  # symbol to geneid mappings
  sym2gid <- read.table("/share/mi/data/pathways/PathwayCommons/homo-sapiens-node-attributes.txt", header = TRUE)
  sym2gid <- subset(sym2gid, NCBI_TAX_ID == 9606)[, c("GENE_SYMBOL", "ENTREZ_GENE_ID")]
  sym2gid <- subset(sym2gid, !GENE_SYMBOL == "NOT_SPECIFIED")
  sym2gid <- subset(sym2gid, !ENTREZ_GENE_ID == "NOT_SPECIFIED")

  # List symbols for the interactions
  syms <- unique(c(as.character(netdata[,1]), as.character(netdata[,2])))

  # Convert symbols to geneids (for compatibility with expression data)
  s2g <- c()
  for (k in 1:length(syms)) {
      print(k/length(syms))
      sym <- syms[[k]]
      gid <- unique(subset(sym2gid, GENE_SYMBOL == sym)[, "ENTREZ_GENE_ID"])
      # only store cases where unique geneid match is found
      if (length(gid) == 1) {
        s2g[[sym]] <- gid
      }	       
   }

   # Network with geneids 
   net <- cbind(as.character(s2g[netdata[,1]]), as.character(s2g[netdata[,2]]))
   # Remove NA's
   net <- net[which(rowSums(is.na(net)) == 0),]
   syms <- as.character(unique(as.vector(net)))

   # Form network matrix
   netw <- array(0, dim = c(length(syms), length(syms)), dimnames = list(syms,syms)) 
   for (i in 1:nrow(net)) {
       print(i/nrow(net))
       # symmetric network
       netw[net[i,1],net[i,2]] <- netw[net[i,2],net[i,1]] <- 1
   }
  
  netw

} 