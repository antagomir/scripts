# mapping orthologs between species: here human vs. mouse implemented, extend later
# download HomoloGene data and make mapping with that.
# FIXME: could be optimized a lot for speed.
# More intuitive and easy way would be to use data frames and plyr package for the mapping  
# Also check if homologene access via BioC. In v2.7 of bioc the homolog.db was not included.

# Example:                                        
# source("/share/mi/scripts/homologene.R")
# mat <- get.latest.homologene()
# hg <- c("7499", "122704", "388419")
# mg <- lapply(hg, function (eg) {human2mouse.homologene(eg)})

get.latest.homologene <- function () {
  df <- "/share/work/lmlahti/tmp/homologene.tab"
  download.file(url = "ftp://ftp.ncbi.nih.gov/pub/HomoloGene/current/homologene.data", destfile = df)
  li <- readLines(df)
  mat <- array(NA, dim = c(length(li), 3))
  # see source file for more stuff
  # Doc at ftp://ftp.ncbi.nih.gov/pub/HomoloGene/README
  colnames(mat) <- c("HomoloGene","Taxonomy","GeneID") 
  tmp <- lapply(li, function(x){unlist(strsplit(x, "\t"))[1:3]})
  for (i in 1:length(tmp)) {mat[i, ] <- tmp[[i]]}

  mat <- data.frame(mat)

  mat[["GeneID"]] <- as.character(mat[["GeneID"]])
  
  mat
  
}
  
human2mouse.ortholog.table <- function (homologene) {

  human <- subset(homologene, Taxonomy == 9606)
  mouse <- subset(homologene, Taxonomy == 10090)

  # Remove HomoloGene clusters with multiple entries
  # i.e. no unique orthologue between species found
  human <- subset(human, !duplicated(HomoloGene))
  mouse <- subset(mouse, !duplicated(HomoloGene))
  
  # Pick only genes that have homologues in the other species
  human <- subset(human, HomoloGene %in% mouse[, "HomoloGene"])
  mouse <- subset(mouse, HomoloGene %in% human[, "HomoloGene"])
  
  # Order by HomoloGene cluster
  human <- sort.data.frame(human, key = "HomoloGene")
  mouse <- sort.data.frame(mouse, key = "HomoloGene")

  # Provide human2mouse mapping
  human2mouse <- mouse[, "GeneID"]
  names(human2mouse) <- human[, "GeneID"]

  human2mouse
}






sort.data.frame <- function(x, key, ...) {
    if (missing(key)) {
        rn <- rownames(x)
        if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
        x[order(rn, ...), , drop=FALSE]
    } else {
        x[do.call("order", c(x[key], ...)), , drop=FALSE]
    }
}


