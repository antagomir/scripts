
# Write log10 HITChip data matrix taxa x samples into a file
# in the sparCC format
sparcc.write <- function (dat, file) {

   # Mimic absolute counts
    datc <- round(10^dat)
    rownames(datc) <- gsub(" ", "_", rownames(datc))
    datc <- cbind(rownames(datc), datc)
    colnames(datc)[[1]] <- "OTU_id"
    datc <- rbind(colnames(datc), datc)
    rownames(datc) <- colnames(datc) <- NULL
    write.table(datc, file = file, quote = F, col.names = F, row.names = F, sep = "\t")

}