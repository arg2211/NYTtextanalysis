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

# for onlytextcorpus, replace abbreviated months with full words
onlytext.repl <- lapply(onlytext, function(x) {
                    gsub(pattern = "jan.", replacement = "january", x) 
                    })
a <- c("jan.", "Jan.", "feb.", "Feb.", "mar.", "Mar.", "apr.", "Apr.", "jun.", "Jun.", 
       "jul.", "Jul.", "aug.", "Aug.", "sept.", "Sept.", "oct.", "Oct.", "nov.", "Nov.", "dec.", "Dec.")
b <- c("january","january", "february","february", "march","march", "april","april", "june","june", 
       "july","july", "august","august", "september","september", "october","october", 
       "november","november", "december","december")
onlytext.repl <- mapply(gsub, a, b, onlytext, ignore.case = TRUE)

# clean onlytextcorpus
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))}) #creates function "toSpace" using gsub that replaces characters with a space

getTransformations()
otc <- tm_map(onlytextcorpus, content_transformer(tolower)) #need to use content_transformer with tolower because of bug in newer version of tm package
otc <- tm_map(otc, stripWhitespace)

otc2 <- tm_map(otc, removePunctuation)
otc2 <- tm_map(otc2, toSpace, "-")
otc2 <- tm_map(otc2, toSpace, ":")
otc2 <- tm_map(otc2, removeWords, stopwords("english"))
otc2 <- tm_map(otc2, removeWords, c("url")) #insert words that you want to remove from corpus where "x" is
otcstem <- tm_map(otc2, stemDocument) #uses tm package stemming

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


# ------------------------------ # split by sentences before preprocessing # --------------------------------- #

require(quanteda)

class(inaugTexts) #inaugTexts is used in example in documentation

# f uses VCorpus object
f <- as.character(nytcorpus) #takes VCorpus object made with tm package and treats it as a character object, f
str(f) #displays internal STRucture of an R object (similar to summary())
myCorpus <- corpus(f)  # build the corpus using quanteda's corpus() function
summary(f, n = 5) #look at 5 docs in f

sentences <- tokenize(f, what = "sentence") #split f corpus by sentences
sentences.n <- as.data.frame(unlist(sentences)) #

# THIS IS THE BEST OPTION
# uses onlytext.repl, which is a character object created before from just TEXT column in nyt.merged df
class(onlytext.repl)
sentences2 <- tokenize(onlytext.repl, what = "sentence")
sentences.n2 <- as.data.frame(unlist(sentences2)) #create data frame of split sentences
sentences2.lower <- toLower(sentences2, keepAcronyms = FALSE) #make all sentences lowercase
sentences.n2.lower <- as.data.frame(unlist(sentences2.lower)) # create dataframe of lowercase sentences

kwic(sentences2.lower, "terror", valuetype = "regex")

# ------------------------------------------------------------------------------------- #

# ------------------------- # try using gender dictionary # ---------------------------- #
library(quanteda)
NCgenderDict <- dictionary(list(
  man = c('guy', 'spokesman','chairman',"men's",'men','him',"he's",'his','boy',
          'boyfriend','boyfriends','boys','brother','brothers','dad','dads','dude',
          'father','fathers','fiance','gentleman','gentlemen','god','grandfather',
          'grandpa','grandson','groom','he','himself','husband','husbands','king',
          'male','man','mr','nephew','nephews','priest','prince','son','sons',
          'uncle','uncles','waiter','widower','widowers'),
  woman = c('heroine','spokeswoman','chairwoman',"women's",'actress','women',
            "she's",'her','aunt','aunts','bride','daughter','daughters','female',
            'fiancee','girl','girlfriend','girlfriends','girls','goddess',
            'granddaughter','grandma','grandmother','herself','ladies','lady',
            'lady','mom','moms','mother','mothers','mrs','ms','niece','nieces',
            'priestess','princess','queens','she','sister','sisters','waitress',
            'widow','widows','wife','wives','woman')))
class(NCgenderDict)
NCgenderDict$man

class(sentences2.lower)
sentences2.lower.vs <- VectorSource(sentences2.lower)
sentences2.lower.corpus <- Corpus(sentences2.lower.vs)

g <- as.character(sentences2.lower)
nyt.dfm2 <- dfm(g, dictionary = NCgenderDict)
head(nyt.dfm, 30)
nyt.dfm2
g.df <- as.data.frame(unlist(nyt.dfm2))

class(onlytext)
nyt.dfm <- dfm(onlytext, dictionary = NCgenderDict)
head(nyt.dfm, 30)
nyt.dfm

#try grepl - not working

colnames(sentences.n2.lower) = c("all")
sentences.n2.lower$men <- ifelse(grepl("\\b(guy|guys|spokesman|spokemsmen|chairman|chairmen|man
|men|him|he|his|boy|boyfriend|boyfriends|boys|brother|brothers|dad|dads|dude|dudes
|father|fathers|fiance|gentleman|gentlemen|god|gods|grandfather|grandfathers|grandpa|grandson
|grandsons|groom|grooms|he|himself|husband|husbands|king|kings|male|man|men|mr|nephew|nephews
|priest|priests|prince|princes|son|sons|uncle|uncles|widower|widowers)\\b", 
                                       sentences.n2.lower$all, ignore.case = TRUE), 1, 0)

sentences.n2.lower$women <- ifelse(grepl("\\b(heroine|heroines|spokeswoman|spokeswomen|chairwoman
|chairwomen|woman|women's|actress|actresses|women|she|she's|her|aunt|aunts|bride|brides
|daughter|daughters|female|fiancee|girl|girlfriend
|girlfriends|girls|goddess|granddaughter|grandma|grandmother|herself|ladies|lady|lady|mom
|moms|mother|mothers|mrs|ms|niece|nieces|priestess|princess|queens|she|sister|sisters
|waitress|widow|widows|wife|wives|woman)\\b",
                                         sentences.n2.lower$all, ignore.case = TRUE), 1, 0)

sentences.n2.lower$both <- ifelse(sentences.n2.lower$men==1 & sentences2.lower$women==1, 1, 0)

ifelse (ex1[1] == ex1[2] & ex1[1] == ex1$qit, 1,
        ifelse ( ex1[1] == ex1$qit | ex1[2] == ex1$qit, 0.5,
                 NA))

# try regex - attempt #2

lists <- c("men", "women", "both", "none")

regexes <- list(c("guy|guys|spokesman|spokemsmen|chairman|chairmen|man
|man\\'s|men|him|he\\'s|his|boy|boyfriend|boyfriends|boys|brother|brothers|dad|dads|dude|dudes
                |father|fathers|fiance|gentleman|gentlemen|god|gods|grandfather|grandfathers|grandpa|grandson
               |grandsons|groom|grooms|he|himself|husband|husbands|king|kings|male|man|men|mr|nephew|nephews
                 |priest|priests|prince|princes|son|sons|uncle|uncles|widower|widowers","men"),
                   c("heroine|heroines|spokeswoman|spokeswomen|chairwoman
                     |chairwomen|woman|women's|actress|actresses|women|she|she's|her|aunt|aunts|bride|brides
                     |daughter|daughters|female|fiancee|girl|girlfriend
                     |girlfriends|girls|goddess|granddaughter|grandma|grandmother|herself|ladies|lady|lady|mom
                     |moms|mother|mothers|mrs|ms|niece|nieces|priestess|princess|queens|she|sister|sisters
                     |waitress|widow|widows|wife|wives|woman", "women")
                   )

#Create a vector, the same length as the df
output_vector <- character(nrow(sentences.n2.lower))

#For each regex..
for(i in seq_along(regexes)){
  
  #Grep through d$name, and when you find matches, insert the relevant 'tag' into
  #The output vector
  output_vector[grepl(x = sentences.n2.lower$all,ignore.case = TRUE, 
                      pattern = regexes[[i]][1])] <- regexes[[i]][2]
  
} 
#Insert that now-filled output vector into the dataframe
sentences.n2.lower$list <- output_vector
sentences.n2.lower <- subset(sentences.n2.lower, sentences.n2.lower$list != "")

men.sub <- subset(sentences.n2.lower, sentences.n2.lower$list == "men")
women.sub <- subset(sentences.n2.lower, sentences.n2.lower$list == "women")

