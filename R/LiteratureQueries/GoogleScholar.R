# http://www.r-bloggers.com/an-r-function-to-analyze-your-google-scholar-citations-page/
# 24.11.2011

if (!require(png)) {install.packages("png")}
if (!require(klaR)) {install.packages("klaR")}

source("googleCite.R")
#wordcloud, tm, sendmailR, RColorBrewer

# Google Scholar Citations data
out <- searchCite("Leo Lahti")
#out <- googleCite("http://scholar.google.com/citations?user=mjjV-AoAAAAJ&hl=en",
#		pdfname="Leo_wordcloud.pdf")
#out <- googleCite("http://scholar.google.com/citations?user=mjjV-AoAAAAJ&hl=en",
#		pdfname="Leo_wordcloud.pdf")

# Citation indices
print(gcSummary(out))

# Are you a data scientist?
# http://simplystatistics.tumblr.com/post/11271228367/datascientist
#source("dataScientist.R")
#dataScientist(names=c(“D.Scientist”),
#              skills=matrix(rep(1/3,3),nrow=1), 
#	      addSS=TRUE, just=NULL
