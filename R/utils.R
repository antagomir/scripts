#' Description: Sort data frame dd by columns like: esort(dd, -z, b)
#'
#' Arguments:
#'   @param x data frame to sort
#'   @param sortvar sorted variable/s
#'   @param ... further parameters to pass
#'
#' Returns:
#'   @return sorted data frame
#'
#' @export
#' @examples data(peerj32)
#'           esort(peerj32$meta, -time, gender)
#'
#' @references See citation('microbiome') 
#' @author Contact: Leo Lahti \email{microbiome-admin@@googlegroups.com}
#' @keywords utilities

esort <- function(x, sortvar, ...) {
    
    attach(x, warn.conflicts = FALSE)
    x <- x[with(x, order(sortvar, ...)), ]
    return(x)
    detach(x)

}

#' Description: Perform pairwise comparison between factor levels
#'              
#' Arguments:
#'   @param x data matrix: samples x features
#'   @param y factor levels for the samples
#'   @param qth q-value threshold for included features 
#'   @param optional. If given, output files will be written in the result directory
#'
#' Returns:
#'   @return List with top findings from pairwise comparisons and their q-values
#'
#' @export
#'
#' @references See citation("microbiome") 
#' @author Contact: Leo Lahti \email{leo.lahti@@iki.fi}
#' @keywords utilities

pairwise.comparisons <- function (x, y, qth = 0.05, resdir = NULL) {

  y <- factor(y)

  library(limma)

  # Remove categories with NA annotations and very small sample size
  keep <- !is.na(y) & (y %in% names(which(table(y) > 2)))
  y <- y[keep]
  x <- x[keep, ]

  top.findings <- list()
  top.findings.qvals <- list()

  # Compare each group to other groups
  for (i in 1:(length(unique(y))-1)) {

    qval.list <- list()

    for (j in (i+1):(length(unique(y)))) {
   
      ai <- as.character(unique(y)[[i]])
      aj <- as.character(unique(y)[[j]])

      inds1 <- which(y == ai)
      inds2 <- which(y == aj)

      lab <- factor(y[c(inds1, inds2)])
      ann <- data.frame(list(varname = lab))
      dat <- x[c(inds1, inds2),]

      # Here y refers to internal lm.matrix response variable(!)
      qvals <- lm.matrix(y ~ varname,  dat, ann, type = "qval")
      coefs <- lm.matrix(y ~ varname - 1,  dat, ann, type = "coef") # remove intercept

      nams <- names(sort(qvals))
      
      fc <- colMeans(x[inds1, nams]) - colMeans(x[inds2, nams])
    
      tab <- cbind(nams, coefs[nams, paste("varname", ai, sep = "")], coefs[nams, paste("varname", aj, sep = "")])
      colnames(tab) <- c("phylotype", ai, aj)
      tab <- as.data.frame(tab)
      tab$fold.change <- fc[nams]
      tab$qvalue <- qvals[nams]

      top.findings[[paste(ai, aj, sep = "-")]] <- subset(tab, qvalue < qth)
      qval.list[[aj]] <- qvals
      top.findings.qvals[[ai]] <- qval.list

      if (!is.null(resdir)) {

        resfile <- paste(resdir,"/limma-", ai, "-", aj, "-results.tab", sep = "")
        message(paste("Writing comparison statistics in: ",  resfile))
        write.table(tab, file = resfile, sep = "\t", quote = FALSE, row.names = FALSE)

        if (nrow(tab) > 0) {
    
          df <- as.data.frame(t(rbind(group = as.character(lab), t(dat)[rownames(tab)[1:min(nrow(tab), 9)],])))
          df$group <- factor(df$group)
          dfm <- melt.data.frame(df, id.vars = c("group"))
          dfm$value <- as.numeric(as.character(dfm$value))
          dfm$title <- apply(cbind(as.character(dfm$variable), round(as.numeric(as.character(tab$qvalue[match(dfm$variable, names(tab$qvalue))])),3)),1,function(x){paste(x, collapse = "/ qval: ")})
          p <- ggplot(dfm, aes(group, value)) + geom_boxplot() + facet_wrap(~title) + opts(title = "Top hits")
          pdfname <- paste(resdir,"/limma-", ai, "-", aj, "-tophit.boxplots.pdf", sep = "")
          print(pdfname)
          pdf(pdfname)
          print(p)
          dev.off()
  
        }
      }
    }
  }

  list(qval = top.findings.qvals, top = top.findings)

}

#' Description:  Computes the given lm model for each variable (row) in the given matrix/data frame
#'  		 Designed for HITchip matrices, but is applicable to any other matrix also.
#'  		 NOTE: does not take log automatically! 
#' FIXME: merge with lm.matrix2
#'
#' Arguments:
#'   @param formula formula
#'   @param d.matrix data matrix
#'   @param d.info information 
#'   @param type information to return
#'
#' Returns:
#'   @return matrix
#'
#' @export
#'
#' @references See citation("microbiome") 
#' @author Contact: Leo Lahti \email{leo.lahti@@iki.fi}
#' @keywords utilities


lm.matrix <- function(formula, d.matrix, d.info, type = "coef") {

  require(limma)

  lm.res <- NULL

  for (v in colnames(d.matrix)) {

      df <- data.frame(list(d.info, y = as.vector(d.matrix[,v])))
      lmfit <- lm(formula, df)
      if (type == "coef") {
        lm.res <- rbind(lm.res, lmfit$coefficients)
      } else if (type == "qval") {
        lm.res <- c(lm.res, anova(lmfit)[["Pr(>F)"]][[1]])
      }
  }

  if (type == "coef") {
    rownames(lm.res) <- colnames(d.matrix)
  } else if (type == "qval") {
    names(lm.res) <- colnames(d.matrix)
    require(qvalue)
    lm.res[is.na(lm.res)] <- 1 # pvalue = 1 for missing vals
    lm.res <- qvalue(lm.res, pi0.method = "bootstrap")$qvalue
  }

  lm.res

}

#' Description: Barplot of top findings from pairwise.comparisons function
#'              
#' Arguments:
#'   @param top.findings Ouput from pairwise.comparisons function
#'   @param topN number of top findings to plot
#'   @param annot annotation matrix    
#'
#' Returns:
#'   @return List with top findings from pairwise comparisons and their q-values
#'
#' @export
#'
#' @references See citation("microbiome") 
#' @author Contact: Leo Lahti \email{leo.lahti@@iki.fi}
#' @keywords utilities

top.barplots <- function (top.findings, topN = 5, annot) {

  # Investigate top findings
  specs <- unique(unlist(sapply(top.findings, rownames)))[1:topN] # phylotypes

  if (length(specs)>0) {
    specs.shortnames <- sapply(specs, function(x) {ss <- strsplit(x, " "); if (length(ss[[1]]) > 1) { paste(paste(substr(ss[[1]][[1]], 1, 1), ".", sep = ""), paste(ss[[1]][-1], collapse = " "), sep = " ")} else {ss[[1]][[1]]}})
    names(specs.shortnames) <- specs
    s <- rownames(smat) # samples
    df <- cbind(smat[s, specs], annot[s,])
    means <- melt(aggregate(df[specs], by=list(varname = df[[varname]]), FUN=mean))
    stds <- melt(aggregate(df[specs], by=list(varname = df[[varname]]), FUN=sd))
    Nsqrt <- sqrt(as.numeric(melt(aggregate(df[specs], by = list(varname = df$varname), FUN = length))$value))
    dfm <- cbind(means, mean = means$value, sd = stds$value, sd.of.mean = as.numeric(stds$value) / Nsqrt)
    dfm$shortnames <- as.factor(specs.shortnames[dfm$variable])

    # Create the barplot component
    p <- ggplot(dfm, aes(x = shortnames, y = value, fill = varname)) 
    dodge <- position_dodge(width = 0.9)
    p <- p + geom_bar(position="dodge") 
    p <- p + geom_errorbar(aes(x = shortnames, ymax = mean + 1.96*sd.of.mean, ymin = mean - 1.96*sd.of.mean), position = dodge, width=0.25)
    p <- p + scale_fill_grey() + theme_bw() + ylab("Signal") + xlab("") 
    p <- p + opts(axis.text.x=theme_text(angle=-20)) 

    } else {
      warning("No features available.")
      p <- NULL  
    }

}

#' Description: Visualization of top findings from pairwise.comparisons function
#'              
#' Arguments:
#'   @param x data matrix
#'   @param y annotaction factor
#'   @param phylogeny.info mapping between features
#'   @param color.level feature level to color
#'   @param bar.level feature level for bars
#'   @param top.findings output from pairwise.comparisons function
#'   @param top.findings.qvals output from pairwise.comparisons function
#'   @param qth q-value threshold
#'   @param qth.star q-value threshold for stars
#'   @param mode barplot / heatmap
#' Returns:
#'   @return List with top findings from pairwise comparisons and their q-values
#'
#' @export
#'
#' @references See citation("microbiome") 
#' @author Contact: Leo Lahti \email{leo.lahti@@iki.fi}
#' @keywords utilities

PlotTopComparisons <- function (x, y, phylogeny.info, color.level, bar.level, top.findings, top.findings.qvals, qth = 0.05, qth.star = 0.01, mode = "barplot") {

  # x <- smat; color.level = "level 1"; bar.level = "level 2"; qth <- 0.05; mode = "heatmap"

  if (bar.level == "level 0") {bar.level <- "L0"}
  if (bar.level == "level 1") {bar.level <- "L1"}
  if (bar.level == "level 2") {bar.level <- "L2"}

  if (color.level == "level 0") {color.level <- "L0"}
  if (color.level == "level 1") {color.level <- "L1"}
  if (color.level == "level 2") {color.level <- "L2"}

  flevels <- as.character(unique(y))

  map <- phylogeny.info[[color.level]]
  names(map) <- as.character(phylogeny.info[[bar.level]])

  # Pick the most significant findings only
  nams <- unique(unlist(sapply(top.findings, function(tab){unlist(rownames(subset(tab, qvalue < qth)))})))

  x <- x[, nams]  

  fc <- aggregate(x, by = list(varname = y), mean) # Mean for each group

  # Compare each group to other group
  comparisons <- list()

  for (i in 1:(length(flevels)-1)) {
    for (j in (i+1):length(flevels)) {
      di <- flevels[[i]]
      dj <- flevels[[j]]
      nam <- paste(di, dj, sep = "-")
      comparisons[[nam]] <- as.numeric(subset(fc, varname == di)[-1]) - as.numeric(subset(fc, varname == dj)[-1])
    }
  }

  nams <- names(comparisons)
  df <- data.frame(comparisons)
  colnames(df) <- nams
  rownames(df) <- colnames(x)
  df$bar.level <- colnames(x)
  df$color.level <- factor(map[as.character(df$bar.level)]) # Check L2 for the phylotype species

  if (mode == "barplot") {

    specs.shortnames <- sapply(as.character(df$bar.level), function(x) {ss <- strsplit(x, " "); if (length(ss[[1]]) > 1) { paste(paste(substr(ss[[1]][[1]], 1, 1), ".", sep = ""), paste(ss[[1]][-1], collapse = " "), sep = " ")} else {ss[[1]][[1]]}})
    names(specs.shortnames) <- as.character(df$bar.level)
    df$shortnames <- factor(specs.shortnames[df$bar.level])
    dfm <- melt(df)

    p <- ggplot(dfm) 
    dodge <- position_dodge(width = 0.9) # define the width of the dodge
    p <- p + geom_bar(position = "dodge", width = 0.4, aes(x = shortnames, y = value, fill = color.level))
    p <- p + facet_grid(.~variable, scales = "free", space = "free")
    p <- p + coord_flip()
    p <- p + theme_bw() + ylab("") + xlab("Fold Change")
    p <- p + opts(axis.text.x = theme_text(size = 7))
    # p <- p + opts(legend.position = c(0.15,.15))

  } else if (mode == "heatmap") {

    ncomp <- ncol(df) - 2 # number of comparisons

    dfm <- melt(df[, 1:(ncomp + 1)], variable = "comparison") # melt matrix
    keep <- !is.na(dfm$value)
    dfm <- dfm[keep, ]
    dfm$comparison <- droplevels(dfm$comparison)
    #flevels <- levels(dfm$comparison)

    qmat <- NULL; nams <- c()
    if (length(flevels)>1) {
      for (i in 1:(length(flevels)-1)) {
        for (j in (i+1):(length(flevels))) {
  
          ai <- flevels[[i]]
          aj <- flevels[[j]]
          nam <- paste(ai, aj, sep = "-")

          qmat <- cbind(qmat, unlist(top.findings.qvals[[ai]][[aj]])[df[[bar.level]]])
          nams <- c(nams, nam)
        }
      }
    } else {

          nam <- flevels[[1]]
          qmat <- top.findings.qvals[[1]][[1]][df$bar.level]
	  qmat <- matrix(qmat, length(qmat))
	  rownames(qmat) <- df$bar.level
          nams <- c(nams, nam)

    }

    if (nrow(qmat) > 0) {

      qmat <- data.frame(qmat)
      colnames(qmat) <- nams
 
      qmat[["phylotype"]] <- rownames(qmat)
      dfm.qval <- melt(qmat)
      names(dfm.qval) <- "value"

      cex.xlab = 7; cex.ylab = 12

      p <- ggplot(dfm, aes(x = comparison, y = bar.level, fill = value))
      p <- p + geom_tile() 
      limit <- ceiling(max(abs(na.omit(dfm$value))))
      p <- p + scale_fill_gradientn("Fold Change", breaks=seq(from=limit, to=-limit, by=-0.2), colours=c("darkblue", "blue", "white", "red", "darkred"), limits=c(-limit,limit)) 
      p <- p + theme_bw() 
      p <- p + opts(axis.text.x=theme_text(angle = 0, size = cex.xlab)) 
      p <- p + opts(axis.text.y=theme_text(size = cex.ylab))

      # Merkkaa merkitsevat tahdella
      p <- p + geom_text(data=subset(dfm.qval, value < qth.star), aes(x = variable, y = value, label = "+"), col = "white", size = 3)
      p <- p + xlab("") + ylab("")
    }
  }

  p

}

