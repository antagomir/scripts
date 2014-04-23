knitting <- function (knitid, show.pdf = TRUE) {

  library(knitr)
  knit(paste(knitid, ".Rtex", sep = ""))
  system(paste("pdflatex ", knitid, ".tex", sep = ""))

  if (show.pdf) {
    system(paste("evince ", knitid, ".pdf &", sep = ""))
  }

  NULL

}

