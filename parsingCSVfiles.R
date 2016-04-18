# setup
getwd()
setwd("~/GitHub/NYTtextanalysis/data/random2015dates")

options(stringsAsFactors = FALSE)

# ----------------------- # clean .csv files and combine into one data frame # --------------------------- #
library(tm)

# load .csv file containing text and metadata
# (created with my edited version of Neal Caren's Python script split_ln.py)
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

# keep only the columns I want and move those into new dataframes
# (also makes it easier to merge data frames)
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

# merge all 12 days of articles together into one dataframe
nyt.merged <- rbind(df01, df02, df03, df04, df05, df06, df07, df08, df09, df10, df11, df12)

# write new merged dataframe to a .csv file
write.csv(nyt.merged, file = "nyt.merged.csv")

# --------------------- # NEED TO FIX # ------------------------------ #
# create graph of number of articles in sample, per day
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

#try barplot
# schinria's code:
muslim.docs <- tm_map(muslimCorpus, stemDocument)
muslim.dtm <- TermDocumentMatrix(muslim.docs)
muslim.m <- as.matrix(muslim.dtm)
muslim.v <- sort(rowSums(muslim.m),decreasing=TRUE)
muslim.d <- data.frame(word = names(muslim.v),freq=muslim.v)
head(muslim.d, 10)
findFreqTerms(muslim.dtm, lowfreq = 100)

barplot(muslim.d[1:15,]$freq, las = 2, names.arg = muslim.d[1:15,]$word,
        col ="lightblue", main ="Most frequent words Tweeted by Muslim-Names",
        ylab = "Word frequencies")

# my code:
# her muslim.m = my dtmstem2
dtmstem <- DocumentTermMatrix(otcstem) # for corpus w/o stopwords and w/ tm stemming
dtmstem2 <- as.matrix(dtmstem)
dtmstem2.a <- sort(rowSums(dtmstem2),decreasing=TRUE)
dtmstem2.d <- data.frame(word = names(dtmstem2.a),freq=dtmstem2.a)
head(dtmstem2.d, 10)
findFreqTerms(dtmstem, lowfreq = 600)

barplot(dtmstem2.d[1:15,]$freq, las = 2, names.arg = dtmstem2.d[1:15,]$word,
        col ="lightblue", main ="Most Frequently-Used Words in Sample",
        ylab = "Word Frequencies")


# --------------------- # load cleaned data file # ------------------------ #
nyt.merged <- read.csv("nyt.merged.csv")

# --------------------- # creating a corpus # ----------------------------- #
library(tm)

# create a corpus only using text data from dataframe
onlytext <- paste(nyt.merged$TEXT, collapse = " ", stringsAsFactors = FALSE)
onlytextcorpus <- Corpus(VectorSource(onlytext))

# creates function "toSpace" using gsub that replaces characters with a space
# from http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))}) 

# clean onlytextcorpus
# create otc - lowercase + no extra white space
otc <- tm_map(onlytextcorpus, content_transformer(tolower)) #need to use content_transformer with tolower because of bug in newer version of tm package
otc <- tm_map(otc, stripWhitespace)
# create otc2 - no stopwords
otc2 <- tm_map(otc, removePunctuation)
otc2 <- tm_map(otc2, removeNumbers)
otc2 <- tm_map(otc2, toSpace, "-")
otc2 <- tm_map(otc2, toSpace, ":")
otc2 <- tm_map(otc2, toSpace, "%")
otc2 <- tm_map(otc2, removeWords, stopwords("english"))
otc2 <- tm_map(otc2, removeWords, c("url")) #insert words that you want to remove from corpus where "x" is
otc2 <- tm_map(otc2, stripWhitespace)
# create otcstem - no stopwords + stemmed
otcstem <- tm_map(otc2, stemDocument) #uses tm package stemming

# make a document term matrix (dtm)
# for otc
dtm.otc <- DocumentTermMatrix(otc)
dtm.m.otc <- as.matrix(dtm.otc)
dtm.otc # gives number of terms in documents # 131,986 terms
# for otc2
dtm.otc2 <- DocumentTermMatrix(otc2)
dtm.m.otc2 <- as.matrix(dtm.otc2)
dtm.otc2 # 69,525 terms
# for otcstem
dtm.stem <- DocumentTermMatrix(otcstem)
dtm.m.otcstem <- as.matrix(dtm.stem)
dtm.stem # 49,377 terms

# find most frequent terms in document term matrices
# for otc
freq.otc <- colSums(dtm.m.otc)
freq.otc <- sort(freq.otc, decreasing = TRUE)
head(freq.otc, 30) # gives top 30 most used words
count.otc <- rowSums(dtm.m.otc)
count.otc # 1,400,486 words
# for otc2
freq.otc2 <- colSums(dtm.m.otc2)
freq.otc2 <- sort(freq.otc2, decreasing = TRUE)
head(freq.otc2, 30) # gives top 30 most used words
count.otc2 <- rowSums(dtm.m.otc2)
count.otc2 # 945,003 words
# for otcstem
freq.otcstem <- colSums(dtm.m.otcstem)
freq.otcstem <- sort(freq.otcstem, decreasing = TRUE)
head(freq.otcstem, 30) # gives top 30 most used words
count.stem <- rowSums(dtm.m.otcstem)
count.stem # 942,968

# create a wordcloud just for otcstem (not for otc or otc2)
library(wordcloud)
library(RColorBrewer)
# create word cloud by frequency with only words that occur 1000+ times
# the larger the font, the more the word occurs
wordcloud(names(freq.otcstem), freq.otcstem, min.freq=1000, colors=brewer.pal(8, "Dark2"), random.order = FALSE) 


# ------------------------------ # split by sentences # --------------------------------- #
# see quanteda documentation for help
class(inaugTexts) #inaugTexts is used in example in documentation

require(quanteda)
class(onlytext) # need to begin with a character object
sentences <- tokenize(onlytext, what = "sentence")
sentences <- toLower(sentences, keepAcronyms = FALSE) # make all sentences lowercase
sentences.df <- as.data.frame(unlist(sentences)) # create dataframe of lowercase sentences
colnames(sentences.df) = c("all") # rename column in dataframe to "all"

# use grepl / regex to categorize sentences by gender
# NEED TO ADD POSSESSIVES
# if any of these words appear in a sentence, assign "1" to new variable 'men'
sentences.df$men <- ifelse(grepl("\\b(guys?|spokesm[ae]n|chairm[ae]n|m[ae]n
|him|he|his|boys?|boyfriends?|brothers?|dads?|dudes?|fathers?|gentlem[ae]n|gods?
|grandfathers?|grandpas?|grandsons?|grooms?|himself|hisself|husbands?|kings?|males?
|mr|nephews?|priests?|princes?|sons?|uncles?|widowers?)\\b", 
                                       sentences.df$all, ignore.case = TRUE), 1, 0)

# COME BACK TO THIS - WANT TO ACCOUNT FOR APOSTRPHES - |he\\'s|dad\\'s|father\\'s|gentlem[ae]n\\'s

# if any of these words appear in a sentence, assign "1" to new variable 'women'
sentences.df$women <- ifelse(grepl("\\b(heroines?|spokeswom[ae]n|chairwom[ae]n
|wom[ae]n|actress|actresses|she|her|aunts?|brides?|daughters?|females?|girls?|girlfriends?
|goddess|goddesses|granddaughters?|grandmas?|grandmothers?|herself|ladies|lady|moms?
|mothers?|mrs|ms|nieces?|priestess|priestesses|princess|princesses|queens?|sisters?
|waitress|waitresses|widows?|wife|wives)\\b",
                                         sentences.df$all, ignore.case = TRUE), 1, 0)
# create new variables 'both' and 'none' from previous variables 'men' and 'women'
sentences.df$both <- ifelse((sentences.df$men==1 & sentences.df$women==1), 1, 0)
sentences.df$none <- ifelse((sentences.df$men==0 & sentences.df$women==0), 1, 0)

# count number of sentences that fall into each category
sum(sentences.df$none) # 56,237 sentences that are about neither men nor women
sum(sentences.df$both) # 2,544 sentences that contain words about both men and women
sum(sentences.df$men) # 23,111 sentences that contain a 'man' word
sum(sentences.df$women) # 9,511 sentences that contain a 'woman' word

# testing if proportion of man-sentences:all-sentences is significantly 
# different from woman-sentences:all-sentences
t.test(sentences.df$men,sentences.df$women)
prop.test(x=c(23111,9511),n=c(86315,86315))

# create subsets of sentences only about 'men' and only about 'women'
s.men <- subset(sentences.df, men==1 & women!=1)
s.women <- subset(sentences.df, women==1 & men!=1)
# check to make sure there are no words that fit the 'none' or 'both' category
sum(s.women$both) # 0
sum(s.women$none) # 0
sum(s.men$both) # 0
sum(s.men$none) # 0

# ----------- # now make corpus & look at word freqs # ----------- #
library(tm)

# create a corpus with only text in it for each gender's sentences
corpus.men <- paste(s.men$all, collapse = " ", stringsAsFactors = FALSE)
corpus.men <- Corpus(VectorSource(corpus.men))
corpus.women <- paste(s.women$all, collapse = " ", stringsAsFactors = FALSE)
corpus.women <- Corpus(VectorSource(corpus.women))

# write only text corpuses to disk
writeCorpus(corpus.women, path = "./sentences", filenames = "women.txt")
writeCorpus(corpus.men, path = "./sentences", filenames = "men.txt")

# # example of writing corpus to disk in .txt format
# data("crude")
# str(crude)
# class(crude)
# writeCorpus(crude, path = ".",filenames = paste(seq_along(crude), ".txt", sep = ""))

# ---------------- # use new gendered text files, make into one corpus w/ 2 docs # ------------- #
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
library(tm)
library(SnowballC)
library(Rcampdf)

# create a filepath
fp <- file.path(".", "sentences")
dir(fp) # tells you what files are in the filepath directory

# create a corpus from the files in the filepath
docs <- Corpus(DirSource(fp))
docs2 <- Corpus(DirSource(fp))

# now clean docs, just like before
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "â")
docs <- tm_map(docs, toSpace, "ã")
docs <- tm_map(docs, toSpace, "%")
docs <- tm_map(docs, removeWords, c("url"))
docs <- tm_map(docs, removeWords, stopwords("english")) # NEED TO FIX THIS - WANT TO KEEP HE,SHE,ETC.
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
# for docs2 - DO NOT REMOVE STOPWORDS
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, toSpace, "-")
docs2 <- tm_map(docs2, toSpace, ":")
docs2 <- tm_map(docs2, toSpace, "â")
docs2 <- tm_map(docs2, toSpace, "ã")
docs2 <- tm_map(docs2, toSpace, "%")
docs2 <- tm_map(docs2, toSpace, ",")
docs2 <- tm_map(docs2, stemDocument)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, PlainTextDocument)

# and create a dtm, just like before
docs.dtm <- DocumentTermMatrix(docs)
docs.dtm # 34% sparsity, 2 documents, 28,341 terms
docs.dtm.df = as.data.frame(as.matrix(docs.dtm))
# for docs2
docs2.dtm <- DocumentTermMatrix(docs2)
docs2.dtm # 34% sparsity, 2 documents, 28,414 terms
docs2.dtm[1,] #23,586 non-sparse + 4,828 sparse terms for men
docs2.dtm[2,] #14,121 non-sparse + 14,293 sparse terms for women
docs2.dtm.df = as.data.frame(as.matrix(docs2.dtm))

# find most common words in this corpus
findFreqTerms(docs.dtm, lowfreq=500) # lowest freq = 500 times
# find most common words in each doc
findFreqTerms(docs.dtm[1,], lowfreq=500) # lowest freq = 500 times
findFreqTerms(docs.dtm[2,], lowfreq=500) # lowest freq = 500 times

# find associations with words
findAssocs(docs.dtm, "mother", 0.9)

# for docs
freq.docs <- colSums(docs.dtm.df)
freq.docs <- sort(freq.docs, decreasing = TRUE)
head(freq.docs, 30) # gives top 30 most used words
count.docs <- rowSums(docs.dtm.df)
count.docs #

# for docs2
freq.docs2 <- colSums(docs2.dtm.df)
freq.docs2 <- sort(freq.docs2, decreasing = TRUE)
head(freq.docs2, 30) # gives top 30 most used words
count.docs2 <- rowSums(docs2.dtm.df)
count.docs2 #

# separate into in docs.m and docs.w
rownames(docs.dtm.df) = c("men","women")
docs.m <- docs.dtm.df[1,]
docs.m <- sort(docs.m, decreasing = TRUE)
docs.w <- docs.dtm.df[2,]
docs.w <- sort(docs.w, decreasing = TRUE)

freq.docs.m <- colSums(docs.m)
freq.docs.m <- sort(freq.docs.m, decreasing = TRUE)
head(freq.docs.m, 30) # gives top 30 most used words
count.docs.m <- rowSums(docs.m)
count.docs.m # 276,357

freq.docs.w <- colSums(docs.w)
freq.docs.w <- sort(freq.docs.w, decreasing = TRUE)
head(freq.docs.w, 30) # gives top 30 most used words
count.docs.w <- rowSums(docs.w)
count.docs.w # 91,989

# separate into in docs2.m and docs2.w
rownames(docs2.dtm.df) = c("men","women")
docs2.m <- docs2.dtm.df[1,]
docs2.m <- sort(docs2.m, decreasing = TRUE)
docs2.w <- docs2.dtm.df[2,]
docs2.w <- sort(docs2.w, decreasing = TRUE)

freq.docs2.m <- colSums(docs2.m)
freq.docs2.m <- sort(freq.docs2.m, decreasing = TRUE)
head(freq.docs2.m, 30) # gives top 30 most used words
count.docs2.m <- rowSums(docs2.m)
count.docs2.m # 388,336

freq.docs2.w <- colSums(docs2.w)
freq.docs2.w <- sort(freq.docs2.w, decreasing = TRUE)
head(freq.docs2.w, 30) # gives top 30 most used words
count.docs2.w <- rowSums(docs2.w)
count.docs2.w # 131,375

# for fun, create word clouds
library(wordcloud)
wordcloud(names(freq.docs), freq.docs, min.freq=300, colors=brewer.pal(8, "Dark2"), random.order = FALSE)
wordcloud(names(freq.docs.m), freq.docs.m, min.freq=300, colors=brewer.pal(8, "Dark2"), random.order = FALSE)
wordcloud(names(freq.docs.w), freq.docs.w, min.freq=120, colors=brewer.pal(8, "Dark2"), random.order = FALSE)

# transpose dataframe so columns are docs and rows are words
docs.tdm.df <- t(docs.dtm.df)
head(docs.tdm.df)
docs2.tdm.df <- t(docs2.dtm.df)
head(docs2.tdm.df)

# correlate docs
cor(docs.tdm.df)
cor(docs2.tdm.df)

# cosine docs
library(lsa)
cosine(docs.tdm.df)
cosine(docs2.tdm.df)

# chi-square test
chitable <- table(docs.tdm.df)
chisq.test(chitable)
chitable3 <- table(docs2.tdm.df)
chisq.test(chitable3)


# ----------------- # less sparse dtm # ---------------- #
# make dtm less sparse
docs.dtm.s <- removeSparseTerms(docs.dtm, 0.20) # This makes a matrix that is 20% empty space, maximum.
docs.dtm.s # 9,219 terms, 2 docs, 0% sparse

# transpose dataframe so columns are docs and rows are words
docs.dtm.s.df <- as.data.frame(as.matrix(docs.dtm.s))
docs.dtm.s.df.t <- t(docs.dtm.s.df)
head(docs.dtm.s.df.t)

# correlate docs
cor(docs.dtm.s.df.t)

# cosine docs
library(lsa)
cosine(docs.dtm.s.df.t)

# chi-square test
chitable2 <- table(docs.dtm.s.df.t)
chisq.test(chitable2)


# ----------------- # new # ------------------ #
# create dtm in a different way
# weight matrix by TdIdf
terms <-DocumentTermMatrix(docs,control=list(weighting=function(x) weightTfIdf(x,normalize=FALSE)))
terms # 2 docs, 41,082 terms, 66% sparse
terms.df <- as.data.frame(as.matrix(terms))
terms.df.t <- t(terms.df)

# find rarest words by doc
# words most uniquely associated with men
findFreqTerms(terms[1,], lowfreq=40)
# words most uniquely associated with women
findFreqTerms(terms[2,], lowfreq=40)

# develop relative frequencies, thx Neal Caren
summing <- function(x) x/sum(x, na.rm=T)
docs.dtm.df.t_new <- apply(docs.dtm.df.t, 2, summing)
head(docs.dtm.df.t_new) # this is good stuff

# more on relative freq's ...
docs.dtm.df.t_new <- as.data.frame(docs.dtm.df.t_new)
names(docs.dtm.df.t_new)[names(docs.dtm.df.t_new)=="1"] <- "men"
names(docs.dtm.df.t_new)[names(docs.dtm.df.t_new)=="2"] <- "women"

# which words are most similar to men and least similar to women?
docs.dtm.df.t_new$ratio = docs.dtm.df.t_new$men - docs.dtm.df.t_new$women
sort.men <- docs.dtm.df.t_new[order(-docs.dtm.df.t_new$ratio) , ]
sort.men[1:15, ]

# which words are most similar to women and least similar to men?
docs.dtm.df.t_new$ratio = docs.dtm.df.t_new$men - docs.dtm.df.t_new$women
sort.women <- docs.dtm.df.t_new[order(docs.dtm.df.t_new$ratio) , ]
sort.women[1:15, ]

# ------------------ # n-grams # ---------------------- #
install.packages("RWeka")
require(RWeka)
library(tm)

#data("crude")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


# for docs (no stopwords)
docs.tdm.bi <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))
docs.tdm.bi
inspect(docs.tdm.bi[10000:10010, ])

docs.tdm.tri <- TermDocumentMatrix(docs, control = list(tokenize = TrigramTokenizer))
docs.tdm.tri
inspect(docs.tdm.tri[200:210, ])
docs.tdm.tri.s <- removeSparseTerms(docs.tdm.tri, 0.40)
docs.tdm.tri.s
inspect(docs.tdm.tri.s[200:210, ])

# for docs2 (with stopwords)
docs2.tdm.bi <- TermDocumentMatrix(docs2, control = list(tokenize = BigramTokenizer))
docs2.tdm.bi
inspect(docs2.tdm.bi[104100:104110, ])

docs2.tdm.bi.df <- as.data.frame(as.matrix(docs2.tdm.bi))
colnames(docs2.tdm.bi.df) <- c("men","women")

count.bi <- colSums(docs2.tdm.bi.df)
count.bi # 490,282 bigrams for men + 160,870 bigrams for women
freq.bi <- rowSums(docs2.tdm.bi.df)
freq.bi <- sort(freq.bi, decreasing = TRUE)
head(freq.bi, 30) # gives top 30 most used words

docs2.tdm.tri <- TermDocumentMatrix(docs2, control = list(tokenize = TrigramTokenizer))
docs2.tdm.tri
inspect(docs2.tdm.tri[200:210, ])

docs2.tdm.tri.df <- as.data.frame(as.matrix(docs2.tdm.tri))
colnames(docs2.tdm.tri.df) <- c("men","women")

count.tri <- colSums(docs2.tdm.tri.df)
count.tri # 490,281 trigrams for men + 160,869 trigrams for women
freq.tri <- rowSums(docs2.tdm.tri.df)
freq.tri <- sort(freq.tri, decreasing = TRUE)
head(freq.tri, 30) # gives top 30 most used words

# docs2.tdm.tri.s <- removeSparseTerms(docs2.tdm.tri, 0.40)
# docs2.tdm.tri.s
# inspect(docs2.tdm.tri.s[200:210, ])

# find most frequent bigrams for men
findFreqTerms(docs2.tdm[,1], lowfreq=400) # lowest freq = 400/
# find most frequent bigrams for women
findFreqTerms(docs2.tdm[,2], lowfreq=400) # lowest freq = 500 times

# ------------------ # find proportion of specific n-grams # ------------------- #
count(docs.w[,"said"])

count(docs2.tdm.bi.df["he said", 1]) #1790
count(docs2.tdm.bi.df["he say", 1]) #58
count(docs2.tdm.bi.df["they said", 1]) #15
count(docs2.tdm.bi.df["they say", 1]) #21

count(docs2.tdm.bi.df["was said", 1]) #12
count(docs2.tdm.bi.df["been said", 1]) #1
count(docs2.tdm.bi.df["is said", 1]) #9
count(docs2.tdm.bi.df["it said", 1]) #19
count(docs2.tdm.bi.df["be said", 1]) #4
count(docs2.tdm.bi.df["were said", 1]) #1
count(docs2.tdm.bi.df["?", 1])

# 490,282 bigrams for men + 160,870 bigrams for women
# he said vs she said
prop.test(x=c(1790,634),n=c(490282,160870))
# he say vs she say
prop.test(x=c(58,29),n=c(490282,160870))
# was/is/been/it/be said in men vs women
prop.test(x=c((12+1+9+19+4+1),(1+1+1+3+1+1)),n=c(490282,160870))
# 

count(docs2.tdm.bi.df["she said", 2]) #634
count(docs2.tdm.bi.df["she say", 2]) #29
count(docs2.tdm.bi.df["they said", 2]) #5
count(docs2.tdm.bi.df["they say", 2]) #6
count(docs2.tdm.bi.df["has said", 2]) #8
count(docs2.tdm.bi.df["had said", 2]) #1
count(docs2.tdm.bi.df["have said", 2]) #3
count(docs2.tdm.bi.df["is say", 2]) #1
count(docs2.tdm.bi.df["are say", 2]) #2
count(docs2.tdm.bi.df["will say", 2]) #0
count(docs2.tdm.bi.df["was say", 2]) #0
count(docs2.tdm.bi.df["were say", 2]) #1
count(docs2.tdm.bi.df["been say", 2]) #1

count(docs2.tdm.bi.df["was said", 2]) #1
count(docs2.tdm.bi.df["been said", 2]) #1
count(docs2.tdm.bi.df["is said", 2]) #1
count(docs2.tdm.bi.df["it said", 2]) #3
count(docs2.tdm.bi.df["be said", 2]) #1
count(docs2.tdm.bi.df["were said", 2]) #1
count(docs2.tdm.bi.df["?", 2])
