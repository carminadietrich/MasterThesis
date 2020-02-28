library(readtext)
getwd()
setwd("/Users/carminadietrich/Desktop/EUPData_new/Test Data/Texts")

path_data <- "/Users/carminadietrich/Desktop/EUPData_new/Test Data/Texts"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files))


#BOW
corpus<-corpus(data)
tok<-tokens(corpus,
            what = "word",
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_separators = TRUE)
tok <- tokens_remove(tok, stopwords("english"))
featnames(dfm(tok))
mydfm <-dfm(tok, tolower = TRUE)

#TF IDF BOW
mydfm <- dfm_tfidf(mydfm,
                  scheme_tf = "count",
                  scheme_df = "inverse")

#bigram BOW
tok<-tokens(corpus,
            what = "word",
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_separators = TRUE)
tok <- tokens_remove(tok, stopwords("english"))
tok <- tokens_ngrams(tok, 2)
featnames(dfm(tok))
mydfm <-dfm(tok, tolower = TRUE)

#tf idf bigram BOW
mydfm <- dfm_tfidf(mydfm,
                   scheme_tf = "count",
                   scheme_df = "inverse")
#5-gram BOW
tok<-tokens(corpus,
            what = "word",
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_separators = TRUE)
tok <- tokens_remove(tok, stopwords("english"))
tok <- tokens_ngrams(tok, 5)
featnames(dfm(tok))
mydfm <-dfm(tok, tolower = TRUE)

#tf idf 5-gram BOW
mydfm <- dfm_tfidf(mydfm,
                   scheme_tf = "count",
                   scheme_df = "inverse")

#8-gram BOW
tok<-tokens(corpus,
            what = "word",
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_separators = TRUE)
tok <- tokens_remove(tok, stopwords("english"))
tok <- tokens_ngrams(tok, 8)
featnames(dfm(tok))
mydfm <-dfm(tok, tolower = TRUE)

#tf idf 8-gram BOW
mydfm <- dfm_tfidf(mydfm,
                   scheme_tf = "count",
                   scheme_df = "inverse")

                        