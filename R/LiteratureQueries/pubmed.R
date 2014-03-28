# From http://brainchronicle.blogspot.nl/2012/05/using-r-to-graph-subject-trend-in.html

pubmed_trend <- function(search.str = 'Sex+Characteristics[mh] AND Pain[mh]', year.span=1970:2011) {
require(XML)
require(RCurl)
results <- NULL
tmpf <- "./tempfile.xml"
## clean before
system(paste("rm", tmpf))
 
for(i in year.span){
queryString <- paste(search.str, ' AND ', i, '[dp]', sep="")
print(paste('queryString:', queryString))
sysString <- paste('./pubmed_trend.pl "', queryString,'"', sep="")
system(sysString)
 
xml <- xmlTreeParse(tmpf, useInternalNodes=TRUE)
pubTerm <- as.numeric(xmlValue(getNodeSet(xml, "//Count")[[1]]))
print(paste("#______num pub for",i,":",pubTerm))
rm(xml)
results <- append(results, pubTerm)
## avoid being kicked out!
Sys.sleep(1)
}
names(results) <- year.span
## clean after
system(paste("rm", tmpf))
return(results)
}

plot_bar <- function(x=sex.pub, linecol="royalblue", cols, addArg=TRUE) {
bp <- barplot(x, col=cols, add=addArg)
fit <- stats::lowess(x, f=1/3)
lines(x=bp, fit$y, col=linecol, lwd=3)
}

sex.pub <- pubmed_trend(search.str = 'Sex+Characteristics[mh] AND Pain[mh]', year.span=1970:2011)
analgesic.pub <- pubmed_trend(search.str = 'Sex+Characteristics[mh] AND Analgesics[mh]', year.span=1970:2011)
 
source('plot_bar.r')
library("RColorBrewer")
 
#pdf(file='sex_pain.pdf', height=8, width=8)
par(las=1)
colorfunction = colorRampPalette(brewer.pal(9, "Reds"))
mycolors = colorfunction(length(sex.pub))
plot_bar(x=sex.pub, linecol="#525252", cols=mycolors, addArg=FALSE)
 
colorfunction = colorRampPalette(brewer.pal(9, "Blues"))
mycolors = colorfunction(length(analgesic.pub))
plot_bar(x=analgesic.pub, linecol='black', cols=mycolors, addArg=TRUE)
title('Number of publication per year')
legend('topleft',
legend=c('Sex and Pain', 'Sex and Analgesics'),
fill=c("red", "blue"),
bty="n",
cex=1.1
)
#dev.off()