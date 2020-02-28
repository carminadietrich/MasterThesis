#Kendall's Tau
cor(c(as.matrix(cons_goldsim)), c(as.matrix(data_doc_simil)), method = "kendall") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(data_doc_simil)), method = "kendall") 

#Spearman
cor(c(as.matrix(cons_goldsim)), c(as.matrix(data_doc_simil)), method = "spearman") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(data_doc_simil)), method = "spearman") 
#0.2281123
#Pearson
cor(c(as.matrix(cons_goldsim)), c(as.matrix(data_doc_simil)), method = "pearson") 
cor.test(c(as.matrix(cons_goldsim)), c(as.matrix(data_doc_simil)), method = "pearson") 