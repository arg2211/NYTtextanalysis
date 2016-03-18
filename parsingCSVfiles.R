# setup
getwd()
setwd("~/GitHub/NYTtextanalysis/data")

options(stringsAsFactors = FALSE)

library(tm)

# load .csv file containing text and metadata as corpus
# (created with my edited version of Neal Caren's Python script split_ln.py)
nyt <- read.csv('nyt3.7b.csv', header = TRUE)
nyttext <- VCorpus
getReaders()
nytcorpus <- VCorpus(DataframeSource(nyt))
class(nytcorpus)
inspect(nytcorpus)
inspect(nytcorpus[2])
summary(nytcorpus)
