#install.packages("rfigshare")
library(rfigshare)
url <- fs_download(855494)
data <- read.csv(url)
articles <- fs_search("SciFund")
ids <- fs_ids(articles)
fs_download(ids, urls_only=FALSE)

