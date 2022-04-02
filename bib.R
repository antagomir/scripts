library(RefManageR)
bib<- ReadBib("leo.bib",check= FALSE)
ref <- RefManageR::PrintBibliography(bib, .opts = list(bib.style = "apa", sorting = ""))
print(ref)