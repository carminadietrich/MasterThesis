library(quanteda)

#Cosine Similarity
doc_simil <- textstat_simil(mydfm,
                            margin = "documents", 
                            method = "cosine")


#Jaccard Similarity
doc_simil <- textstat_simil(mydfm,
                            margin = "documents", 
                            method = "jaccard")

#Euclidean Distance
doc_simil <- textstat_dist(mydfm,
                           margin = "documents", 
                           method = "euclidean")

#Convert into DF
data_doc_simil <- as.data.frame(as.matrix(doc_simil))


