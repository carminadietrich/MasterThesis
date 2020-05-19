library(dplyr) 
library(tidyr) 
library(purrr) 
library(readr) 
library(stringr) 
library(quanteda)
library(ggplot2)

path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/Test_Data/Texts"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files))
data.frame(data)
glimpse(data)

# checking length of the selected posts and removing overly long ones. 
corpus <- corpus(data$text)
# Add post id as document level variables
docvars(corpus, field = "doc_id") <- corpus$doc_id

# Compute some basic statistics about the token counts
# to get an idea about the distribution of the post length:
corpus %>% 
  summary(n = nrow(data)) %>% 
  summarize(doc_cnt = n(),
            avg_token = mean(Tokens),
            median_token = median(Tokens),
            Q3_token = quantile(Tokens, probs = 0.75),
            min_token =min(Tokens),
            max_token = max(Tokens),)

#doc_cnt avg_token median_token Q3_token min_token max_token
#      50     90.42         89.5    103.5        48       138

# According to the examined token-related statistics, there are no overly long posts

# Start by computing summary statistics for each document (post) in the corpus
stats<-summary(corpus, n = 50)
glimpse(stats)
boxplot(stats$Tokens) #no outliers

# extract features for embedding

# Extract tokens from the corpus and filter out all those tokens that are
# not expected to be useful for classification purposes. The rationale:
# since we will do the averaging over word vectors within a document, we
# should try to assure that only semantics-bearing words are kept.

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

# tokens are not stemmed, since words in the GloVe model were also not stemmed

# Create DTM
news_dtm <- dfm(news_tokens, tolower=FALSE)
news_dtm

# Extract words from the DTM for matching against words in the pre-trained GloVe model
news_words <- featnames(news_dtm)
# ... and examine them
head(news_words, n = 100)
tail(news_words, n = 100)
#save file
getwd()
save(news_words, file = "news_words")


