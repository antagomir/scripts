download_david2014 <- function (...) {

    message("Downloading data set from David et al. Genome Biology, 2014: http://genomebiology.com/2014/15/7/R89")

    # Some preliminary code to convert the data in phyloseq format
    skip <- T
    if (!skip) {
    data.dir <- system.file("extdata/David2014", package = "microbiome")
    f <- paste0(data.dir, "/David_OTU_meta.RData")
    load(f, .GlobalEnv)

    # OTU data
    library(phyloseq)
    otumat <- t(rbind(stool.A, stool.B))
    OTU <- otu_table(otumat, taxa_are_rows = TRUE)

    # OTU taxonomy
    #f <- paste0(data.dir, "/david2014-otu_table.RData")
    #gb-2014-15-7-r89-s10-SubjectATaxonomy.xls
    #load(f, .GlobalEnv)
    taxonomy <- otu
    TAX <- tax_table(as.matrix(taxonomy[rownames(otumat), ]))

    # Combine OTU and Taxon matrix into Phyloseq object
    physeq <- phyloseq(OTU, TAX)

    # Metadata and harmonize field names
    meta <- rbind(meta.A, meta.B)
    colnames(meta) <- harmonize_fieldnames(colnames(meta))
    data.david2014 <- merge_phyloseq(physeq, meta)

    save(data.david2014, file = "David2014-physeq.Rdata")
    }

    data.dir <- system.file("extdata/David2014", package = "microbiome")
    f <- paste0(data.dir, "/David2014-physeq.Rdata")
    load(f, .GlobalEnv)
   
}

