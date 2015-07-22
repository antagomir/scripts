# http://lincolnmullen.com/projects/dh-r/topic-modeling.html

library(mallet)
library(tm)
library(dplyr)
tracts <- mallet.read.dir("data/tracts-for-the-times/")