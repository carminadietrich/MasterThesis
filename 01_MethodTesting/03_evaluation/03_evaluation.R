#Spearman
cor(c(as.matrix(cons_goldsim)), c(as.matrix(doc_simil)), method = "spearman") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(doc_simil)), method = "spearman") 

#Pearson
cor(c(as.matrix(cons_goldsim)), c(as.matrix(doc_simil)), method = "pearson") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(doc_simil)), method = "pearson") 

