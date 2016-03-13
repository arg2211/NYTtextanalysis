NYTtest2 = readLines("NYTtest2.txt") #reads .txt file in as a character
length(NYTtest2) #number of lines in character
head(NYTtest2, n = 10) #first 10 lines
tail(NYTtest2, n = 10) #last 10 lines
class(NYTtest2) #what kind of value
NYTtest2.df <- data.frame(NYTtest2) #makes character vector into a dataframe

library(tm) #load tm package
getReaders() #see what reader functions are available

vs <- VectorSource(NYTtest2) #create a vector
plain <- readPlain(vs, lang = "en", id = "id1")

# create corpus
nytcorpus <- Corpus(DirSource("file"))

fileName <- "NYTtest2.txt"
readChar(fileName, file.info(fileName)$size)
