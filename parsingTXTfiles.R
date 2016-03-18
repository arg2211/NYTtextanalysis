  # setup
getwd()
setwd("./GitHub/NYTtextanalysis")

options(stringsAsFactors = FALSE)

library(tm)

  # load plain text files (articles from lexis nexis)
NYTtest2 = readLines("NYTtest2.txt") #reads .txt file in as a character
length(NYTtest2) #number of lines in character
head(NYTtest2, n = 10) #first 10 lines
tail(NYTtest2, n = 10) #last 10 lines
class(NYTtest2) #what kind of value
NYTtest2.df <- data.frame(NYTtest2) #makes character vector into a dataframe

  # use tm package to read in plain text file and convert to vector source
getReaders() #see what reader functions are available
vs <- VectorSource(NYTtest2) #create a vector source
plain <- readPlain(vs, lang = "en", id = "id1") #read in a plain text (.txt) document
NYTtext <- readPlain(NYTtest2, lang = "en", id = "id1") #read in a plain text (.txt) document
class(plain) #what kind of value

  # create corpus from vs
nytcorpus <- VCorpus(vs)
inspect(nytcorpus) #this tells us that nytcorpus has 269 "documents" bc each line is treated as a document

  # clean corpus
stopwords("english") #view stopwords
inspect(nytcorpus) #see what corpus looks like

nytcorpus <- tm_map(nytcorpus, stripWhitespace) #gets rid of extra white space
nytcorpus <- tm_map(nytcorpus, tolower) #makes all letters lowercase
nytcorpus
