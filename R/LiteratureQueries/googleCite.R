#########################################################################################
#   Some functions to quantify your Google Scholar citations page. 
#   R functions Copyright (C) 2011 John Muschelli (jmuschel@jhsph.edu), Andrew Jaffe (ajaffe@jhsph.edu),
#   Jeffrey Leek (jtleek@gmail.com), and the Simply Statistics Blog
#   (http://simplystatistics.tumblr.com, http://twitter.com/simplystats)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details, see <http://www.gnu.org/licenses/>.
#
#
#   These functions depend on the packages: wordcloud, tm, sendmailR, and RColorBrewer. It will
#   attempt to install them if they are not installed when you source this function. 
#
#
#   How to use: 
#       # Source the function
#       source("http://biostat.jhsph.edu/~jleek/code/googleCite.r")
#
#       # Get the url for a scholar (this is the one for Rafa Irizarry: http://scholar.google.com/citations?user=nFW-2Q8AAAAJ&hl=en)
#       # and run the googleCite function. You can choose to plot word clouds of the co-authors and paper titles by setting plotIt=TRUE
#       # it will automatically produce a pdf file, if you want to set the name/location of this pdffile, set the pdfname="yourname_wordcloud.pdf"
#       # When you run this function, your Google Scholar data will be sent to our email account, so that we can see who is running the function
#       # and to perform population-level analyses. The variable out will contain a table with data from your Google Scholar citation page. 
#        
#       out <- googleCite("http://scholar.google.com/citations?user=nFW-2Q8AAAAJ&hl=en", pdfname="rafa_cloud.pdf")
# 
#
#       # To calculate some popular citation indices you can now apply gcSummary to the output
#       gcSummary(out)
#
#
#       # You can also search for a specific individual by name using the function searchCite
#
#       out2 <- searchCite("Rafa Irizarry", pdfname="rafa_cloud.pdf")
#
########################################################################################


getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(wordcloud))
if(!pckg) {
cat("Installing 'wordcloud' from CRAN\n")
getPckg("wordcloud")
require(wordcloud)
}
pckg = try(require(tm))
if(!pckg) {
cat("Installing 'tm' from CRAN\n")
getPckg("tm")
require("tm")
}
pckg = try(require(sendmailR))
if(!pckg) {
cat("Installing 'sendmailR' from CRAN\n")
getPckg("sendmailR")
require("sendmailR")
}
pckg = try(require(RColorBrewer))
if(!pckg) {
cat("Installing 'RColorBrewer' from Bioconductor\n")
getPckg("RColorBrewer")
require("RColorBrewer")
}

# helper functions
googleCite <- function(theurl, plotIt = TRUE,pdfname=NULL) {

  theurl = strsplit(theurl,"&hl")[[1]][1]
  alldata <- NULL

  author = getAuthor(paste(theurl,"&view_op=list_works&pagesize=100&cstart=",0,sep=""))
  
  for (ipage in 0:1000){
    checker <- ipage * 100
    page = paste(theurl, "&view_op=list_works&pagesize=100&cstart=", checker, sep="")
    temper <- getcites(page, checkcite=checker)
    alldata <- rbind(alldata, temper$data)
    if (temper$stopit == 1) break
  }
  
  alldata$"First Author" <- NA
  alldata$"Second Author" <- NA
  alldata$"Last Author" <- NA
  alldata$"N Authors" <- NA
  
  for(irow in 1:nrow(alldata)){
    tmp = strsplit(alldata$Author[irow], ",")[[1]]
    
    alldata$"First Author"[irow] <- tmp[1]
    alldata$"Second Author"[irow] <- tmp[2]
    alldata$"Last Author"[irow] <- tmp[length(tmp)]
    alldata$"N Authors"[irow] <- length(tmp)
    
  }
 
  alldata$Is_First <- grepl(alldata$"First Author", pattern=author)
  alldata$Is_Second <- grepl(alldata$"Second Author",pattern=author)
  alldata$Is_Last <- grepl(alldata$"Last Author",pattern=author)

  alldata$"First Author" <- NULL
  alldata$"Second Author" <- NULL
  alldata$"Last Author" <- NULL
  
  
  if(plotIt) {
    if (!is.null(pdfname)) pdf(pdfname, h = 6, w = 12)
    
    par(mfrow = c(1,2))
    makeAuthorCloud(alldata)
    makePaperCloud(alldata)
    
    if (!is.null(pdfname)) dev.off()
  }

  from <- sprintf("<sendmailR@%s>", Sys.info()[4])
  to <- "<simplystatisticsgs@gmail.com>"
  subject <- author
  body <- list(theurl, mime_part(alldata))
  tmpEmail = try(email <- sendmail(from, to, subject, body, control=list(smtpServer="ASPMX.L.GOOGLE.COM")),silent=T)
  
  return(alldata)
}

getAuthor <- function(webpage) {
  options(warn = -1)
  con <- url(webpage)
  x <- readLines(con,encoding="UTF-8")
  y <- strsplit(x, split="<")
  z <- y[[1]]
  tmp = z[6]
  tmp2 = strsplit(tmp, " ")[[1]]
  ind = grep("-", tmp2)
  out = tmp2[ind-1]
  close(con)
  return(out)
}

getcites <- function(page, checkcite){
  old.locale <- Sys.getlocale()
  Sys.setlocale(locale="C")
  options(warn = -1)
  con <- url(page)
  x <- readLines(con)
  x <- strsplit(x, split="<")
  x <- x[[1]]
  
### grab the end of citations
  endcites <- x[grep(pattern="margin: 0 0.5em 0 0.5em;\">", x=x)[1]]
  endcites <- strsplit(endcites, split="margin: 0 0.5em 0 0.5em;\">")[[1]][2]
  endcites <- as.numeric(strsplit(endcites, split="-")[[1]][2])
  
  stopit <- 0
  # print(checkcite)
  # print(endcites)
  if (is.na(endcites)) return(list(data=NULL, stopit=1))
  if (endcites < checkcite) stopit <- 1
  
  keepers <- grep(pattern="cit-table", x)
  keepers <- keepers[-1]
  keepers <- keepers[-1]
  keepers <- c(keepers, length(x))
  x <- x[keepers[2]:keepers[length(keepers)]]
  cites <- grep(x, pattern="cit-table item")
  cites <- unique(c(cites, length(x)))
  cit <- vector(mode="list", length=length(cites)-1)
  ncites <- length(cites)-1
  
  data <- NULL
  
  for(icite in 1:(length(cites) -1) ){
                                        # print(icite)
    temp_data <- data.frame(matrix(nrow=1, ncol=5))
    temp <- x[ cites[icite]:cites[icite+1] ]
    tites <- grep(pattern="cit-dark-large-link", temp)
    if (length(tites) > 0) temp_data[1, 1] <- strsplit(temp[tites], split="cit-dark-large-link\">")[[1]][2]
    
    tites <- grep(pattern="cit-gray", temp)
    temp2 <- strsplit(temp[tites], split="\"cit-gray\">")
    if (length(tites) > 0) temp_data[1, 2] <- temp2[[1]][2]	
    if (length(temp2) > 1) temp_data[1, 3] <- temp2[[2]][2]
    
    tites <- grep(pattern="col-year", temp)
    if (length(tites) > 0) temp_data[1, 4] <- strsplit(temp[tites], split="col-year\">")[[1]][2]
    
    tites <- grep(pattern="col-citedby", temp)+1
    if (length(tites) > 0) temp_data[1, 5] <- strsplit(temp[tites], split="\">")[[1]][2]
    data <- rbind(data, temp_data)
  }
  colnames(data) <- c("Paper", "Author", "Journal", "Year", "Citations")
  
  data[, "Paper"] <- gsub(x=data[, "Paper"], pattern="\227", replacement="--", fixed=TRUE)
  data[, "Paper"] <- gsub(x=data[, "Paper"], pattern="&#8208;", replacement="-", fixed=TRUE)
  data[, "Paper"] <- gsub(x=data[, "Paper"], pattern="&#39;", replacement="'", fixed=TRUE)
  
  data[, "Author"] <- gsub(x=data[, "Author"], pattern="\227", replacement="--", fixed=TRUE)
  data[, "Author"] <- gsub(x=data[, "Author"], pattern="&#8208;", replacement="-", fixed=TRUE)
  data[, "Author"] <- gsub(x=data[, "Author"], pattern="&#39;", replacement="'", fixed=TRUE)
  data[, "Author"] <- gsub(x=data[, "Author"], pattern="\305", replacement="A", fixed=TRUE)
  
  close(con)
  return(list(data=data, stopit=stopit))
  Sys.setlocale(locale=old.locale)
  
}

getPckg = function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

makeAuthorCloud = function(tab) {

  colIndex = which(names(tab) == "Author")
  
  tmp = strsplit(as.character(tab[,colIndex]), ", ")
  out = sapply(tmp, function(x) {
	x = strsplit(x, " ")
	x = sapply(x, function(x) x[2])
	x = tolower(x)
	return(x)})
  out = unlist(out)
  tmp2 = table(out)
  tmp2 = tmp2[!(names(tmp2) == "...")]
  d = data.frame(word = names(tmp2), freq = tmp2, row.names = NULL)
  d = d[order(d$freq, decreasing = TRUE),]
  d = d[-1,]
  
  pal = brewer.pal(9, "BuGn") 
  pal <- pal[-(1:4)]
  
  wordcloud(words = d$word, freq = d$freq, 
            min.freq = 1, max.words = Inf,
            random.order = FALSE, 
			colors = pal,vfont=c("sans serif","plain"))
}

makePaperCloud = function(tab) {

  colIndex = which(names(tab) == "Paper")
  
  corpus <- Corpus(DataframeSource(data.frame(tab[, colIndex])))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
  
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  pal = brewer.pal(9, "RdPu")
  pal <- pal[-(1:4)]
  
  wordcloud(words = d$word, freq = d$freq, 
            min.freq = 1, max.words = Inf,
            random.order = FALSE, colors = pal,vfont=c("sans serif","plain"))
}

searchCite <- function(Author, ...){
  auth.names <- strsplit(Author, " ")[[1]]
  auth.names <- paste(auth.names[1:length(auth.names)], sep="", collapse="+")
  
  search.page <- paste("http://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=", auth.names, sep="")
  thepage <- url(search.page)
  x <- readLines(thepage)
  x <- strsplit(x[[1]], split="user=")[[1]]
  if (length(x) > 1){
    ### if they have someone for a hit
    ##grab the first hit
    x <- x[2]
    x <- strsplit(x, split="&amp;")[[1]][1]
    theurl <- paste("http://scholar.google.com/citations?hl=en&user=", x, sep="")
    print(theurl)
    return(googleCite(theurl, ...))
  } else stop("No Author found")
  close(thepage)
}

gcSummary <- function(alldata){
  citations = as.numeric(alldata$Citations)
  citations[is.na(citations)] = 0
  nauthors = as.numeric(alldata$"N Authors")
  n = dim(alldata)[1]
  nF = sum(alldata$Is_First)
  nL = sum(alldata$Is_Last)
  nFL = sum(alldata$Is_Last | alldata$Is_First)
  nFS = sum(alldata$Is_First | alldata$Is_Second)

  totalPapers = dim(alldata)[1]
  totalCites = sum(citations,na.rm=T)
  medianCites = median(citations,na.rm=T)
  medianAuthorCites = median(citations/nauthors,na.rm=T)
  
  hindex = sum(citations > 1:n,na.rm=T)
  hindexF = sum(citations[alldata$Is_First]> 1:nF,na.rm=T)
  hindexL = sum(citations[alldata$Is_Last] > 1:nL,na.rm=T)
  hindexFL = sum(citations[alldata$Is_Last | alldata$Is_First] > 1:nFL,na.rm=T)
  hindexFS = sum(citations[alldata$Is_First | alldata$Is_Second] > 1:nFL,na.rm=T)
  
  tmp = cumsum(citations)
  
  gindex = sum(tmp >= (1:n)^2)

  nyears =  as.numeric(format(Sys.time(), "%Y")) - min(as.numeric(out$Year),na.rm=T)
  mindex = hindex/nyears
  
  cat("Total papers = ")
  cat(totalPapers)
  cat("\n")
  cat("Median citations per paper = ")
  cat(medianCites)
  cat("\n")
  cat("Median (citations / # of authors) per paper = ")
  cat(medianAuthorCites)
  cat("\n")
  cat("H-index = ")
  cat(hindex)
  cat("\n")
  cat("G-index = ")
  cat(gindex)
  cat("\n")
  cat("M-index = ")
  cat(mindex)
  cat("\n")
  cat("First author H-index = ")
  cat(hindexF)
  cat("\n")
  cat("Last author H-index = ")
  cat(hindexL)
  cat("\n")
  cat("First or last author H-index = ")
  cat(hindexFL)
  cat("\n")
  cat("First or second author H-index = ")
  cat(hindexFS)
  cat("\n")
  
}
