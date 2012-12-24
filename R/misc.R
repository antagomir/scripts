#* top.barplots: Barplot of top findings from pairwise.comparisons function
#* PlotTopComparisons: Visualize top findings from pairwise.comparisons 

na.filter <- function (mat, ind) {

  # Remove rows or columns with exlusively NAs
  library(plyr)
  all.nas <- function(x) all(is.na(x))
  mat[!rowwise(all.nas)(mat), !colwise(all.nas)(mat)]

}

variance.contribution <- function (x, subset) {

  m <- rowMeans(x)
  total.variance <- sum((x - m)^2)
  subset.variance <- sum((x[, subset] - m)^2)

  subset.variance/total.variance
}



parseBands <- function (bands,uniq=FALSE) {

        #Go through list of chromosomal band names of form '1q11.1' and convert them to form '1q11

        #if uniq, return unique list of parsed bands
        parsedBands<-c()
        for (band in bands) {
                parsedBands<-c(parsedBands,strsplit(band,"\\.")[[1]][[1]])
        }
        if (uniq) {parsedBands<-unique(parsedBands)}
        parsedBands
}

source.dir <- function (dir = NULL, pattern = NULL) {

  # Run source for R scripts (*.R) in given directory.
  # By default, working dir.

  if (is.null(dir)) {dir = getwd()}

  fils <- list.files(dir, full.names = TRUE, pattern = pattern)
  print(fils)
  
  tmp <- lapply(fils, function (f) {source(f)})
  
}


month2numeric = function (month) {

	months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

	which(months == month)

}

omit.na = function (vec) {vec[!is.na(vec)]}


printCounter = function (ind,seqlength,interval) {
	# print counter information at each interval 
	myseq = seq(0,1,interval)
	frac1 = ind/seqlength
	frac0 = (ind-1)/seqlength
	if (!sum(frac1>myseq)==sum(frac0>myseq))
	{print(ind/seqlength)}
}

split.date = function (d) {

	# Author: Leo Lahti

	dd = unlist(strsplit(d," "))

	day = dd[[1]]
	year = dd[[5]]
	month = dd[[2]]
	datum = dd[[3]]
	
	ddd = unlist(strsplit(dd[[4]],":"))
	hour = ddd[[1]]
	min = ddd[[2]]
	sec = ddd[[3]]

	list(day=day, year=year, month=month, datum=datum, hour=hour, min=min, sec=sec)

}

