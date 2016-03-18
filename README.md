# NYTtextanalysis
This is my thesis project! I want to conduct a text analysis of gender related to sentence structure.

#### Steps for Analysis:
1. Find data
  * chosen corpus = articles published in The New York Times in the last year
  * articles for newspapers, magazines, etc. available through LexisNexis Academic (access through school)
    + link here, need to sign in: http://www.columbia.edu/cgi-bin/cul/resolve?AND3603
    + download articles in .txt format & select "documents with indexing"
  * another option - scrape text using NYT API
    + good article/tutorial: http://brooksandrew.github.io/simpleblog/articles/new-york-times-api-to-mongodb/#accessing-nyt-api
2. Clean Data
  * found Neal Caren's code here: http://nealcaren.web.unc.edu/cleaning-up-lexisnexis-files/
  * converts plain text files to csv files with text and metadata, where each row is one article
    + modified code slightly to create my own split_ln.py that works with my computer
3. Preprocess Text Data
  * 
