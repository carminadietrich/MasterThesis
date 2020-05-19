library(dplyr) 
library(tidyr) 
library(purrr) 
library(readr) 
library(stringr) 
library(quanteda)
library(ggplot2)
library(readtext)
path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/EU_Data/Texts/Final"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files))

corpus <- corpus(data$text)
docvars(corpus, field = "doc_id") <- corpus$doc_id

# extract features for embedding
news_tokens <- tokens(x = corpus, 
                      what = "word", 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_url = TRUE,
                      remove_separators = TRUE)


#Normalize tokens (set them to lower case), remove stopwords and tokens with < 2 letters
news_tokens <- news_tokens %>%
  tokens_tolower() %>%
  tokens_remove(stopwords()) %>%
  tokens_keep(min_nchar = 3) 

# Create DTM
news_dtm <- dfm(news_tokens, tolower=FALSE)
news_dtm

# Extract words from the DTM for matching against words in the pre-trained GloVe model
news_words <- featnames(news_dtm)
head(news_words, n = 100)

#save file
getwd()
save(news_words, file = "news_words")
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/01_MethodTesting/WordEmbeddings/Pre-Trained/Glove/files")
load("g6b_300d_df")
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis")

# match words from the GloVe model against the words from corpus
glove_words <- colnames(g6b_300d_df)
save(glove_words, file = "glove_words")
# keep only those words that are present in both 
words_to_keep <- intersect(news_words, glove_words)
# check the 'level' of matching
length(words_to_keep)/length(news_words)
# 96,79% words from DTM have their vectors in GloVe
# inspect words from post_dtm that are not in GloVe
setdiff(news_words, glove_words)[1:500]
#There are 111 words from post_dtm that are not in Glove. Mostly abbreviations, 
#misspelled words, and compound words but also 'regular' words in the posessive form. 
#Improve the matching level by substituting posessive
# forms with their 'regular' counterparts (eg. one's -> one)
tokens_to_replace <- tokens_keep(x = news_tokens,
                                 pattern = "[a-z]+'s",
                                 valuetype = "regex", verbose = TRUE)
tokens_to_replace <- unlist(tokens_to_replace) %>% unique()
replacements <- gsub(pattern = "([a-z]+)'s", replacement = "\\1", 
                     x = tokens_to_replace)
# Now, re-create DTM
news_dtm <- dfm_replace(news_dtm, 
                        pattern = tokens_to_replace,
                        replacement = replacements, verbose = TRUE)


# Again, get the words that are present both in the DTM and GloVe model
words_to_keep <- intersect(featnames(news_dtm), glove_words)
setdiff(words_to_keep, glove_words)[1:100]
length(words_to_keep)/length(news_words)
# 97.39% - a slight improvement and now there are zero words that are not in Glove

# Create a new DTM that will keep only those words (columns) from the original DTM 
#(post_dtm) that are present in the GloVe model  
dtm_reduced <- dfm_keep(news_dtm, 
                        pattern=words_to_keep, 
                        valuetype="fixed", 
                        verbose=TRUE)
save(dtm_reduced, file = "dtm_reduced")

# Likewise, from GloVe, select word vectors that will be used for building vectors
#of the words present in the dtm_reduced
glove_to_keep_indices <- which(glove_words %in% words_to_keep)
g6b_300d_df_reduced <- g6b_300d_df[,glove_to_keep_indices]

# Order the columns (words) in the g6b_300d_df_reduced, to be the same as in
# the dtm_reduced
g6b_300d_df_reduced <- g6b_300d_df_reduced[,colnames(dtm_reduced)]
save(g6b_300d_df_reduced, file = "g6b_300d_df_reduced")
remove(g6b_300d_df)

# remove large objects that are no longer needed
remove(glove_words, glove_to_keep_indices, words_to_keep, glove_6B_300d_dir, data, stats, news_tokens, news_words, news_dtm)

# Create a Relaxed WMD (RWMD) object for computing the dissimilarity of documents
library(text2vec)
wv = t(g6b_300d_df_reduced)
dim(wv)
dim(dtm_reduced)
rwmd_model = RWMD$new(dtm_reduced, wv)

#calculate Relaxed Word Mover’s Distance
rwmd_glove = rwmd_model$dist2(dtm_reduced[1:27, ])
dim(rwmd_glove)
head(sort(rwmd_glove[1, ], decreasing = T))
head(sort(rwmd_glove[1, ], decreasing = F))
save(rwmd_glove, file = "rwmd_glove")
save(wv, file = "wv")



