run.sam <- function (dat, lab, nperms, ctrl.group = NULL, log = "log10", resp.type = "Two class unpaired") {

  if (log == "log10") {
    # Convert the data in log2 domaine
    dat <- log2(10^dat)
  } else if (log == "ln") {
    # Convert the data in log2 domaine
    dat <- log2(exp(dat))
  } else if (log == "log2") {
    # Convert the data in log2 domaine
    dat <- dat
  } else if (log == "none") {
    # Convert the data in log2 domain
    dat <- log2(dat)
  }

  require(samr)

  # Important: data needs to be imported in original (non-log) domain
  data <- list(x = dat, y = lab, geneid = rownames(dat), genenames = rownames(dat), logged2 = TRUE)

  samr.obj <- samr(data, resp.type = resp.type, nperms = nperms)

  # Select delta by median FDR
  delta.tab <- samr.compute.delta.table(samr.obj)     
  delta <- max(na.omit(delta.tab[, "delta"]))

  # Identify significant features
  sig.tab <- samr.compute.siggenes.table(samr.obj, delta, data, delta.tab, all.genes = TRUE)
 
  # Provide output
  tablist <- list()
  for (res in c("genes.up", "genes.lo")) {
    tab <- NA
    if (nrow(sig.tab[[res]]) > 0) { tab <- format.sam.table(sig.tab[[res]]) }
    tablist[[res]] <- tab    
  }
  
  tab <- rbind(tablist[[1]], tablist[[2]])
  tab <- tab[order(tab[, "q-value(%)"]),]
  tab[,"q-value(%)"] <- tab[,"q-value(%)"]/100
  colnames(tab)[[2]] <- "qvalue"  
  colnames(tab)[[1]] <- paste("AbsoluteFoldChange", sep = "")

  # All lipids are downregulated??
  # dat <- t(datasets[["lipids"]][subset(ann, time == 2)$sampleID,])
  # for (i in 1:nrow(dat)) {print(sign(2^((mean(dat[i, lab == "Placebo"]) - mean(dat[i, lab == "LGG"])))))}
  #lgg.samples <- subset(ann, time == 2 & lipids.group == "LGG" )$sampleID
  #pla.samples <- subset(ann, time == 2 & lipids.group == "Placebo" )$sampleID
  # k <- 174; boxplot(list(dat[k, lgg.samples], dat[k, pla.samples])) # here lgg upregulated why not in SAM?

  # Ensure fold change calculated correct way (treatment / control, not control / treatment)
  if (lab[[1]] == ctrl.group) { tab[, "AbsoluteFoldChange"] <- 1/tab[, "AbsoluteFoldChange"] }

  # Log10 fold change
  tab <- cbind(tab, log10(tab[, "AbsoluteFoldChange"]))
  colnames(tab)[[3]] <- "Log10FoldChange"

  # SimpleFoldChange: convert to easily interpretable form: 
  # for instance 6/2 -> 3-fold change; 2/6 -> -3-fold-change 
  sf <- tab[, "AbsoluteFoldChange"]
  sf[sf < 1] <-  -1/sf[sf < 1]
  tab <- cbind(tab, sf)  
  colnames(tab)[[4]] <- "SimpleFoldChange"

  # Sort by qvalue and absolute value of signed fold change 
  # signed is made symmetric around 0
  tab <- esort(as.data.frame(tab), qvalue, -abs(Log10FoldChange))

  tab <- tab[, c("qvalue", "AbsoluteFoldChange","Log10FoldChange", "SimpleFoldChange")]

  tab

}


format.sam.table <- function (tab.orig) {

  if (is.null(dim(tab.orig))) {
    tab <- tab.orig[c("Fold Change", "q-value(%)")]
    colnams <- names(tab)
    rownams <- tab.orig["Gene Name"]
  } else if (nrow(tab.orig) == 1) {
    tab <- tab.orig[, c("Fold Change", "q-value(%)")]
    colnams <- names(tab)
    rownams <- tab.orig[, "Gene Name"]
  } else if (nrow(tab.orig) > 1) {
    tab <- tab.orig[, c("Fold Change", "q-value(%)")]
    colnams <- colnames(tab)
    rownams <- tab.orig[, "Gene Name"]
  }

  tab <- matrix(tab, ncol = length(colnams))
  tab <- matrix(apply(tab, 2, as.numeric), nrow = length(rownams))
  colnames(tab) <- colnams
  rownames(tab) <- rownams 

  tab
}


esort <- function(x, sortvar, ...) {
  #Sort data frame dd by columns like: esort(dd, -z, b)

  attach(x)
  x <- x[with(x,order(sortvar,...)),]
  return(x)
  detach(x)
}
