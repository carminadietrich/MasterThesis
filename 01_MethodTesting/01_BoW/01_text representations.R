library(readtext)
library(quanteda)
path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/Test_Data/Texts"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files))

###BOW###
corpus<-corpus(data)
tokens<-tokens(corpus,
               what = "word",
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE)
tokens <- tokens %>%
  tokens_tolower() %>%
  tokens_remove(stopwords()) %>%
  tokens_keep(min_nchar = 3)  %>%
  tokens_wordstem()

featnames(dfm(tokens))
mydfm <-dfm(tokens, tolower = FALSE)

#tf idf BOW
mydfm <- dfm_tfidf(mydfm,
                  scheme_tf = "count",
                  scheme_df = "inverse")

###n-gram BOW (just insert n)
tokens <- tokens_ngrams(tokens, n=1:5)
head(tokens[[1]], 30)
tail(tokens[[1]], 30)
featnames(dfm(tokens))
mydfm <-dfm(tokens, tolower = FALSE)

#tf idf bigram BOW
mydfm <- dfm_tfidf(mydfm,
                   scheme_tf = "count",
                   scheme_df = "inverse")

###skip-gram
tokens<-tokens_skipgrams(tokens, n= 1:2, skip = 1:3)
head(tokens[[1]], 30)
tail(tokens[[1]], 30)
mydfm <-dfm(tokens, tolower = FALSE)

#tf idf bigram BOW
mydfm <- dfm_tfidf(mydfm,
                   scheme_tf = "count",
                   scheme_df = "inverse")
