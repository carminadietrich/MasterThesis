# Load the pre-trained GloVe word vectors

glove_6B_300d_dir <- "/Users/kathrindietrich/Desktop/041620MasterThesis/01_MethodTesting/WordEmbeddings/Pre-Trained/Glove/glove.6B/"
g6b_300d <- scan(file = paste0(glove_6B_300d_dir, "glove.6B.300d.txt"), what="", sep="\n")

# Create a data frame out of the large vector read from the file
get_word_vectors_df <- function(m_glove, verbose = FALSE) {
  n_words <- length(m_glove)
  vals <- list()
  names <- character(n_words)
  for(i in 1:n_words) {
    if (verbose) {
      if(i %% 5000 == 0) {print(i)}
    }
    this_vec <- m_glove[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  
    this_vec_name <- this_vec_unlisted[1]
    
    vals[[i]] <- this_vec_values
    names[i] <- this_vec_name
  }
  
  # convert the list to a data frame and attach the names
  glove_df <- data.frame(vals)
  names(glove_df) <- names
  
  glove_df
}

g6b_300d_df <- get_word_vectors_df(g6b_300d, verbose = TRUE)

# Remove g6b_300d to release memory
remove(g6b_300d)
save(g6b_300d_df, file = "g6b_300d_df")

# Take words from the GloVe model to match them against the words from corpus
glove_words <- colnames(g6b_300d_df)
save(glove_words, file = "glove_words")
# match and keep only those words that are present both in corpus and in the GloVe model. 
words_to_keep <- intersect(news_words, glove_words)
# check the 'level' of matching
length(words_to_keep)/length(news_words)
# 96,79% words from DTM have their vectors in GloVe

# inspect words from post_dtm that are not in GloVe
setdiff(news_words, glove_words)[1:100]

#There are 48 words from post_dtm that are not in Glove. Mostly abbreviations, 
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
#by specifying 2 input parameters:
# - word vector matrix with words given in rows and dimensions of the 
#   embedding space in columns; rows should have word names.
# - the method to be used for computing the distance between word vectors
library(text2vec)
wv = t(g6b_300d_df_reduced)
dim(wv)
dim(dtm_reduced)
rwmd_model = RWMD$new(dtm_reduced, wv)

#calculate Relaxed Word Mover’s Distance
rwmd_glove = rwmd_model$dist2(dtm_reduced[1:50, ])
dim(rwmd_glove)
head(sort(rwmd_glove[1, ], decreasing = T))
head(sort(rwmd_glove[1, ], decreasing = F))

save(rwmd_glove, file = "rwmd_glove")
save(wv, file = "wv")

#evaluation
cor(c(as.matrix(cons_goldsim)), c(as.matrix(rwm_glove)), method = "pearson") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(rwm_glove)), method = "pearson") 
#[1] -0.6997315


#with tfidf weighting
tf_idf_dtm <- dfm_tfidf(dtm_reduced, 
                        scheme_tf = "prop") # for TF, use normalized counts (ie. proportions)
tf_idf_rwmd_model = RWMD$new(tf_idf_dtm, wv)
tf_idf_rwmd_glove = tf_idf_rwmd_model$dist2(tf_idf_dtm[1:50, ])

save(tf_idf_rwmd_glove, file = "tf_idf_rwmd_glove")
save(tf_idf_rwmd_model, file = "tf_idf_rwmd_model")
save(tf_idf_dtm, file = "tf_idf_dtm")

#evaluation
cor(c(as.matrix(cons_goldsim)), c(as.matrix(tf_idf_rwmd_glove)), method = "pearson") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(tf_idf_rwmd_glove)), method = "pearson") 
#[1] -0.6953039
