library(dplyr)
library(quanteda)
library(readtext)
library(stringi)

#exploring
glimpse(data)
names(data)
typeof(data)
class(data)

?corpus

#create corpus
corp<-corpus(data)
#docvars(corp, "type") <- "Automotive Lobby"
#docvars(corp$communication, "type") <- "KOM"
summary(corp)

#corpus_subset
kwic(corp, "commiss", valuetype = "regex")

#tokenize corpus to remove one letter words 
tok<-tokens(corp)
tok<-tokens_select(tok, min_nchar = 2)

mydfm <-dfm(tok, 
             remove = stopwords("en", source = "snowball"),
             remove_numbers = TRUE,
             remove_punct = TRUE,
            remove_hyphens = TRUE,
             tolower = TRUE, 
             stem = TRUE)
mydfm1<- dfm_trim(mydfm, min_termfreq = 10, min_docfreq = 5)
             
topfeatures(mydfm, 100)
topfeatures(mydfm1,100)

textplot_wordcloud(mydfm1)
textplot_wordcloud(mydfm, docs = "acea.txt")

#save dfm
getwd()
setwd("/Users/carminadietrich/Desktop/EUPData_new/CarminaObjects")
save(mydfm1, file = "mydfm_unweighted.RData")

