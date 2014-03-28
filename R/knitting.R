knitting <- function (knitid) {

  library(knitr)
  knit(paste(knitid, ".Rtex", sep = ""))
  system(paste("pdflatex ", knitid, ".tex", sep = ""))
  system(paste("evince ", knitid, ".pdf &", sep = ""))
  NULL
}

