# http://lincolnmullen.com/projects/dh-r/topic-modeling.html

library(mallet)
library(tm)
library(dplyr)

# Read tracts
tracts <- mallet.read.dir("dh-methods-in-r/data/tracts-for-the-times/")

# Create stopwords
stops <- stopwords("english")
stops_file <- file("dh-methods-in-r/data/stopwords.txt")
writeLines(stops, stops_file)
close(stops_file)

#Create mallet instances
inst <- mallet.import(tracts$id, tracts$text, "dh-methods-in-r/data/stopwords.txt")

#Create a topic modeler and load docs
topic_model <- MalletLDA(30)
topic_model$loadDocuments(inst)

# What do we have?
# topic_model$getVocabulary()[1:100]
# topic_model$getDocumentNames()
freq <- mallet.word.freqs(topic_model)
freq %>%
  arrange(-term.freq) %>%
  top_n(20)

# Now to do the topic generation
topic_model$train(500)
doc_topics <- mallet.doc.topics(topic_model, smoothed=T, normalized=T)
topic_words <- mallet.topic.words(topic_model, smoothed=T, normalized=T)
topic_docs <- t(doc_topics)
mallet.top.words(topic_model, topic_words[4,], num.top.words = 20)

topics <- mallet.topic.labels(topic_model, topic_words, num.top.words = 100)
topic_docs <- topic_docs %>%
  as.data.frame() 
names(topic_docs) <- tracts$id
clust <- hclust(dist(topic_words))
plot(clust)

