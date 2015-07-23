# Natural Language Processing in R
# http://lincolnmullen.com/projects/dh-r/nlp.html

library(rJava)
library("NLP")
library("openNLP")
library("RWeka")
library("qdap")
library(magrittr)

if(!require("openNLPmodels.en")) {
  install.packages("openNLPmodels.en",
                   repos = "http://datacube.wu.ac.at/",
                   type = "source")
}

# Example test
bio <- readLines("dh-methods-in-r/data/nlp/anb-jarena-lee.txt")
bio <- paste(bio, collapse = " ")
print(bio)
# Format required for NLP
bio <- as.String(bio)

# Annotate words and sentences
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_ann, word_ann))
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

# Get sentences and words
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

#OpenNLP can find dates, locations, money, organizations, percentages,
#people, and times. (Acceptable values are "date", "location", "money",
#"organization", "percentage", "person", "misc"
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(bio_doc, kind = "person")
entities(bio_doc, kind = "location")
entities(bio_doc, kind = "organization")
