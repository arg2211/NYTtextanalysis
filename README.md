# NYTtextanalysis
This is my thesis project! I want to conduct a text analysis of gender related to sentence structure.

#### Steps for Analysis:
1. Find data
  * chosen corpus = articles published in The New York Times in the last year
  * articles for newspapers, magazines, etc. available through LexisNexis Academic (access through school)
    + link here, need to sign in: http://www.columbia.edu/cgi-bin/cul/resolve?AND3603
    + randomly select 1 day from each month in 2015 and download all NYT times from those 12 days
    + download articles in .txt format & select "documents with indexing"
    + merge all .txt files into one .txt file using command line:
```type *.txt > <filename>.txt```
  * another option - scrape text using NYT API
    + good article/tutorial: http://brooksandrew.github.io/simpleblog/articles/new-york-times-api-to-mongodb/#accessing-nyt-api

2. Clean Data
  * found Neal Caren's code here: http://nealcaren.web.unc.edu/cleaning-up-lexisnexis-files/
  * converts plain text files to csv files with text and metadata, where each row is one article
    + modified code slightly to create my own split_ln.py that works with my computer

3. Preprocess Text Data
  * first create corpus of words
  * then clean corpus by:
    + making all letter lowercase
    + removing extra white space
    + removing characters like : and _
    + stemming words (reduce to root)
    + removing stopwords (gets rid of words like "the", "and", "have")
    [NOTE: probably will not want to do this for gender-related words]
  * make a document term matrix (dtm)
  * can run frequencies words (how often they appear), clustering, etc.
  * helpful beginner tutorials, articles, videos, etc. include:
    + A Gentle Introduction to Text Mining Using R https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
    + Basic Text Mining in R https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
    + tm package documentation https://cran.r-project.org/web/packages/tm/index.html
    + Text Mining in R Tutorial: Term Frequency & Word Clouds https://www.youtube.com/watch?v=lRTerj8fdY0

4. Split By Sentences
  * use ```quanteda``` package, see documentation here:
    + https://cran.rstudio.com/web/packages/quanteda/vignettes/quickstart.html
  * for tokenizing, can use:
    + VCorpus object (created using tm package)
    + character object
    + NOTE: DO NOT make corpus all lowercase before tokenizing, the function relies on uppercase letters to help it split sentences more accurately

5. Separate Sentences by Gender
  * first create dictionary of words that are gender-related (list of words that indicate a woman is talked about and another list of words that indicate a man in being referenced)
    + followed in Neal Caren's footsteps (http://nbviewer.jupyter.org/gist/nealcaren/5105037) and used gender-specific words from Danielle Sucher's "Jailbreak the Patriarchy" (https://raw.githubusercontent.com/DanielleSucher/Jailbreak-the-Patriarchy/master/myscript.js)
  * 
  * 
