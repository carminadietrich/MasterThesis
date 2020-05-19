library(text2vec)
library(rsparse)
library(readtext)
library(quanteda)

path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/Test_Data/Texts"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files), encoding = "UTF-8")
data$text<-removeNumbers(data$text)
data$text<-removePunctuation(data$text)
data$text<-removeWords(data$text, stopwords("en") )
gsub("[\t]", "", data$text)
data$text<-stemDocument(data$text, language = "english")
tokens = word_tokenizer(tolower(data$text))

v = create_vocabulary(itoken(tokens))
v = prune_vocabulary(v, term_count_min = 2) 
it = itoken(tokens)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, skip_grams_window = 5)
tcm = create_tcm(it, vectorizer, skip_grams_window = 5) #https://srdas.github.io/MLBook/Text2Vec.html#n-grams
print(dim(tcm))

#fit the word embeddings using glove
glove_model = GloVe$new(rank = 50, x_max = 10)
wv = glove_model$fit_transform(tcm, n_iter = 5)
wv = wv + t(glove_model$components) # get average of main and context vectors as proposed in GloVe paper

###calculate RWMD
rwmd_model = RelaxedWordMoversDistance$new(dtm, wv)
rwmd = rwmd_model$dist2(dtm)

#Spearman
cor(c(as.matrix(cons_goldsim)), c(as.matrix(rwmd)), method = "spearman") 
#-0.3466092
#Pearson
cor(c(as.matrix(cons_goldsim)), c(as.matrix(rwmd)), method = "pearson") 
#-0.6806991

###calculate RWMD with tf idf
tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm, tfidf)
dtm_test_tfidf = transform(dtm, tfidf)
tcm_test_tfidf = transform(tcm, tfidf)

#fit the word embeddings using glove
glove_model = GloVe$new(rank = 50, x_max = 10)
wv = glove_model$fit_transform(tcm_test_tfidf, n_iter = 5)
wv = wv + t(glove_model$components) # get average of main and context vectors as proposed in GloVe paper

#calculate RWMD
rwmd_model = RelaxedWordMoversDistance$new(dtm_test_tfidf, wv)
rwmd = rwmd_model$dist2(dtm_test_tfidf)

#Spearman
cor(c(as.matrix(cons_goldsim)), c(as.matrix(rwmd)), method = "spearman") 
#[1] -0.3021944
#Pearson
cor(c(as.matrix(cons_goldsim)), c(as.matrix(rwmd)), method = "pearson") 
#[1] -0.6778304


###calculate euclidean and cosine distance
document_embeddings <- function(doc_tokens,
                                embedding_matrix) {
  
  # allocate a vector to embed the document in:
  doc_embedding <- rep(0,ncol(embedding_matrix))
  
  # remove blank tokens
  rem <- which(doc_tokens == "")
  if (length(rem) > 0) {
    doc_tokens <- doc_tokens[-rem]
  }
  
  # determine which tokens in the document match the vocaubulary in the
  # word embeddings matrix:
  inds <- match(doc_tokens,rownames(embedding_matrix))
  
  # remove any terms that do not appear in the embeddings matrix:
  rem <- which(is.na(inds))
  if(length(rem) > 0) {
    inds <- inds[-rem]
  }
  
  # take the column sums to find the vector addition of the words. Deal with
  # case of multiple matching words, only one, and none:
  if (length(inds) > 1) {
    doc_embedding <- colSums(embedding_matrix[inds,])
  } else if(length(inds) > 0) {
    doc_embedding <- embedding_matrix[inds,]
  } else {
    doc_embedding <- rep(0,ncol(embedding_matrix))
  }
  # return the document embedding vector
  return(doc_embedding)
}


embedded_docs <- matrix(0, nrow = length(tokens),
                        ncol = ncol(wv))

# loop through your documents and generate document embeddings from word
# embeddings:
for (i in 1:length(tokens)) {
  if (i %% 10 == 0) {
    print(i)
  }
  embedded_docs[i,] <- document_embeddings(
    tokens[[i]],
    embedding_matrix = wv)
}

embedded_docs

# make sure that the rownames are the text of the documents:
rownames(embedded_docs) <- data$text

# finding similar documents based on their embedding
similarities <- embedded_docs[data$text, , drop = FALSE] 
cos<-dist2(embedded_docs, similarities, method = "cosine", norm ="l2")
euc<-dist2(embedded_docs, similarities, method = "euclidean", norm ="l2")

#Spearman
cor(c(as.matrix(cons_goldsim)), c(as.matrix(cos)), method = "spearman") 
#[1] -0.2033892
#Pearson
cor(c(as.matrix(cons_goldsim)), c(as.matrix(cos)), method = "pearson") 
#-0.5057808

#Spearman
cor(c(as.matrix(cons_goldsim)), c(as.matrix(euc)), method = "spearman")
# -0.20339
#Pearson
cor(c(as.matrix(cons_goldsim)), c(as.matrix(euc)), method = "pearson") 
#-0.578006







