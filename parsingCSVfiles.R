# setup
getwd()
setwd("~/GitHub/NYTtextanalysis/data")

options(stringsAsFactors = FALSE)

library(tm)

# load .csv file containing text and metadata
# (created with my edited version of Neal Caren's Python script split_ln.py)
nyt <- read.csv('nyt3.7b.csv', header = TRUE, stringsAsFactors = FALSE)

# create a corpus from text and metadata
getReaders()
nytcorpus <- VCorpus(DataframeSource(nyt))
class(nytcorpus) #just making sure it's a corpus
inspect(nytcorpus) #gives info about all "documents" in corpus
inspect(nytcorpus[2]) #gives info about only the 2nd "document" in corpus
summary(nytcorpus)

# create a corpus with only text in it
onlytext <- paste(nyt$TEXT, collapse = " ", stringsAsFactors = FALSE)
class(onlytext)
summary(onlytext)
onlytextvs <- VectorSource(onlytext)
onlytextcorpus <- Corpus(onlytextvs)
class(onlytextcorpus)

# clean onlytextcorpus
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})


getTransformations()
otc <- tm_map(onlytextcorpus, content_transformer(tolower)) #need to use content_transformer with tolower because of bug in newer version of tm package
otc <- tm_map(otc, removePunctuation)
otc <- tm_map(otc, toSpace, "-")
otc <- tm_map(otc, toSpace, ":")
otc <- tm_map(otc, stripWhitespace)
otc <- tm_map(otc, removeWords, stopwords("english"))
#otc <- tm_map(otc, removeWords, c("x")) #insert words that you want to remove from corpus where "x" is
inspect(otc)
class(otc)

# make a document term matrix (dtm)
dtm <- DocumentTermMatrix(otc)
dtm2 <- as.matrix(dtm)
dtm

# find most frequent terms
freq <- colSums(dtm2)
str(freq)
freq <- sort(freq, decreasing = TRUE)
head(freq)

# create a wordcloud
library(wordcloud)
words <- names(freq)
wordcloud(words[1:100], freq[1:100])
