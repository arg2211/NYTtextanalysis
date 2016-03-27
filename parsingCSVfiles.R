# setup
getwd()
setwd("~/GitHub/NYTtextanalysis/data/random2015dates")

options(stringsAsFactors = FALSE)

# ----------------------- # clean .csv files and combine into one data frame # --------------------------- #

library(tm)

# load .csv file containing text and metadata
# (created with my edited version of Neal Caren's Python script split_ln.py)
# nyt <- read.csv('<filename>.csv', header = TRUE, stringsAsFactors = FALSE)
#NEED TO FIND A MORE SUCCINCT WAY TO DO THIS
nyt01 <- read.csv('nyt01-02-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt02 <- read.csv('nyt02-02-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt03 <- read.csv('nyt03-20-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt04 <- read.csv('nyt04-11-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt05 <- read.csv('nyt05-12-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt06 <- read.csv('nyt06-06-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt07 <- read.csv('nyt07-26-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt08 <- read.csv('nyt08-30-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt09 <- read.csv('nyt09-18-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt10 <- read.csv('nyt10-17-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt11 <- read.csv('nyt11-10-15.csv', header = TRUE, stringsAsFactors = FALSE)
nyt12 <- read.csv('nyt12-10-15.csv', header = TRUE, stringsAsFactors = FALSE)

colnames(nyt08)

#keep only the columns I want (also makes it easier to merge data frames)
df01 <- subset(nyt01, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df02 <- subset(nyt02, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df03 <- subset(nyt03, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df04 <- subset(nyt04, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df05 <- subset(nyt05, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df06 <- subset(nyt06, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df07 <- subset(nyt07, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df08 <- subset(nyt08, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df09 <- subset(nyt09, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df10 <- subset(nyt10, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df11 <- subset(nyt11, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))
df12 <- subset(nyt12, select = c(SEARCH_ROW, PUBLICATION, SECTION, DATE, TITLE, BYLINE, COUNTRY, STATE, CITY, PERSON, SUBJECT, LENGTH, TEXT))

nyt.merged <- rbind(df01, df02, df03, df04, df05, df06, df07, df08, df09, df10, df11, df12)
save(nyt.merged, file = "nyt.merged.rda")
write.csv(nyt.merged, file = "nyt.merged.csv")

# ------------------------------ # load cleaned data file # ---------------------------------- #

load("nyt.merged.rda")  
# OR
nyt.merged <- read.csv("nyt.merged.csv")

# ------------------------------ # split by sentences before preprocessing # --------------------------------- #

install.packages("quanteda")
require(quanteda)

class(inaugTexts) #inaugTexts is used in example in documentation

# f uses VCorpus object
f <- as.character(nytcorpus) #takes VCorpus object made with tm package and treats it as a character object, f
str(f) #displays internal STRucture of an R object (similar to summary())
myCorpus <- corpus(f)  # build the corpus using quanteda's corpus() function
summary(f, n = 5) #look at 5 docs in f

sentences <- tokenize(f, what = "sentence") #split f corpus by sentences
sentences.n <- as.data.frame(unlist(sentences)) #

# uses onlytext, which is a character object created before from just TEXT column in nyt.merged df
class(onlytext)
sentences2 <- tokenize(onlytext, what = "sentence")
sentences.n2 <- as.data.frame(unlist(sentences2))

# DO NOT DO THIS - DO NOT make corpus lowercase before splitting by sentences... it doesn't work!
f3 <- as.character(otc) #uses only TEXT corpus (created from TEXT column) that has been changed to all lowercase & stripWhitespace
str(f3)
myCorpus3 <- corpus(f3)  # build the corpus
summary(f3)
sentences3 <- tokenize(f3, what = "sentence")
sentences.n3 <- as.data.frame(unlist(sentences3))


# --------------------- # preprocessing & creating a corpus # ---------------------- #

# create a corpus from text and metadata
getReaders()
nytcorpus <- VCorpus(DataframeSource(nyt.merged))
class(nytcorpus) #just making sure it's a corpus
inspect(nytcorpus) #gives info about all "documents" in corpus
inspect(nytcorpus[2]) #gives info about only the 2nd "document" in corpus
summary(nytcorpus)

# create a corpus with only text in it
onlytext <- paste(nyt.merged$TEXT, collapse = " ", stringsAsFactors = FALSE)
class(onlytext)
summary(onlytext)
onlytextvs <- VectorSource(onlytext)
onlytextcorpus <- Corpus(onlytextvs)
class(onlytextcorpus)

# clean onlytextcorpus
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))}) #creates function "toSpace" using gsub that replaces characters with a space

getTransformations()
otc <- tm_map(onlytextcorpus, content_transformer(tolower)) #need to use content_transformer with tolower because of bug in newer version of tm package
otc <- tm_map(otc, stripWhitespace)

otc <- tm_map(otc, removePunctuation)
otc <- tm_map(otc, toSpace, "-")
otc <- tm_map(otc, toSpace, ":")
otc <- tm_map(otc, removeWords, stopwords("english"))
otc <- tm_map(otc, removeWords, c("url")) #insert words that you want to remove from corpus where "x" is
otcstem <- tm_map(otc, stemDocument) #uses tm package stemming

#library(SnowballC)
#otcstem0 <- tm_map(otc, content_transformer(wordStem), language="eng") #uses SnowballC package stemming
#inspect(otcstem0)
#class(otcstem0)

# make a document term matrix (dtm)
dtm <- DocumentTermMatrix(otc) # for original corpus w/o stopwords
dtm2 <- as.matrix(dtm)
dtm # gives number of terms in documents 

dtmstem <- DocumentTermMatrix(otcstem) # for corpus w/o stopwords and w/ tm stemming
dtmstem2 <- as.matrix(dtmstem)

#dtmstem0 <- DocumentTermMatrix(otcstem0) # for corpus w/o stopwords and w/ SnowballC stemming
#dtmstem02 <- as.matrix(dtmstem0)

# find most frequent terms
freq <- colSums(dtm2)
count <- rowSums(dtm2)
count
#str(freq)
freq <- sort(freq, decreasing = TRUE)
head(freq, 30)
tail(freq, 10)
head(table(freq), 30) #shows you a table of frequencies (how many words [bottom row] appear this frequently [top row])

# for tm stemming
freqstem <- colSums(dtmstem2)
countstem <- rowSums(dtmstem2)
countstem
freqstem <- sort(freqstem, decreasing = TRUE)
head(freqstem, 30)
tail(freqstem, 10)

# for SnowballC stemming - NOT WORKING FOR SOME REASON, LOOK INTO 
#freqstem0 <- colSums(dtmstem02)
#countstem0 <- rowSums(dtmstem02)
#countstem0
#freqstem0 <- sort(freqstem0, decreasing = TRUE)
#head(freqstem0, 30)
#tail(freqstem0, 10)


# create a wordcloud
library(wordcloud)
library(RColorBrewer)

words <- names(freq)
wordcloud(words[1:100], freq[1:100])
wordcloud(names(freq), freq, max.words=100) #creates word cloud of words, by frequency (larger text = more), with max of 100 words displayed
wordcloud(names(freq), freq, min.freq=600, colors=brewer.pal(8, "Dark2")) #creates word cloud of words, by frequency (larger text = more), with only words that occur 1000+ times

wordcloud(names(freqstem), freqstem, min.freq=600, colors=brewer.pal(8, "Dark2")) #creates word cloud of words, by frequency (larger text = more), with only words that occur 1000+ times


# descriptive stats on articles
# NEED TO FIX
art <- c(141, 145, 255, 167, 204, 171, 354, 341, 216, 169, 189, 230, 215)
mean(art)
hist(art,
     breaks = 11,
     freq = TRUE, probability = FALSE,
     labels = c("Jan-2", "Feb-2", "Mar-20", "Apr-11", "May-12", "Jun-6", "Jul-26", "Aug-30", "Sept-18", "Oct-17", "Nov-10", "Dec-10", "Avg"),
     col = "green", border = "white",
     main = "Number of NYT Articles Published per Day",
     xlab = "Randomly-Selected Days",
     ylab = "Number of Articles")
     #ylim = c(0, 400)

# try using ggplot for graphing
library(ggplot2)   
wf <- data.frame(word=names(freqstem), freq=freqstem)   
p <- ggplot(subset(wf, freq>500), aes(word, freqstem))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  #error, fix later

#------------# trying to split corpus by sentences #---------------#
# Load Packages
require(tm)
require(NLP)
require(openNLP)
require(qdap)

install.packages('rJava')

# set up function to convert text to sentences
convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector 
  # employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang) #from original
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}

# reshape corpus hack *NOTE* will lose metadata unless recode
reshape_corpus <- function(onlytextcorpus, FUN, ...) {
  # Extract the text from each document in the corpus and put into a list
  text <- lapply(onlytextcorpus, content_transformer)
  
  # Basically convert the text
  docs <- lapply(text, FUN, ...)
  docs <- as.vector(unlist(docs))
  
  # Create a new corpus structure and return it
  new.corpus <- Corpus(VectorSource(docs))
  return(new.corpus)
}

# then use function just created to reshape the corpus into sentences (use onlytextcorpus which has not been preprocessed yet)
onlytextcorpus2 <- reshape_corpus(onlytextcorpus, convert_text_to_sentences)

# ----------------------------------------------------------------------#

# try using gender dictionary #

head(NAMES, 20)
tail(NAMES, 20)
head(NAMES)