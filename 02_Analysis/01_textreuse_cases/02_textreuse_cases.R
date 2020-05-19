setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/files")
load("rwmd_glove")
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/01_textreuse_cases")
library(ggplot2)
library(RColorBrewer)
rownames(rwmd_glove) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "EUcomm2", "EUcomm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
colnames(rwmd_glove) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "EUcomm2", "EUcomm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")

final <- (t(rwmd_glove) + rwmd_glove)/2
save(final, file="final")
###visualization###
heatmap <- heatmap(final, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

legend(x="bottomright", legend=c("similar", "dissimilar"), 
              fill=colorRampPalette(brewer.pal(8, "Spectral"))(3))


#basic statistics
max(as.numeric(unlist(final)))
median(as.numeric(unlist(final)))
min(as.numeric(unlist(final)))
mean(as.numeric(unlist(final)))
boxplot(as.numeric(unlist(final)))


#filter out most/least similar cases
library(dplyr)
list<-rwmd_glove %>%                               
  as.table() %>% as.data.frame() %>%       
  subset(Var1 != Var2 & abs(Freq)<0.5) %>% # omit diagonal and keep significant correlations (optional...)
  filter(!duplicated(paste0(pmax(as.character(Var1), as.character(Var2)), pmin(as.character(Var1), as.character(Var2))))) %>% # keep only unique occurrences, as.character because Var1 and Var2 are factors
  arrange(Freq)

#most similar (smallest distance) are avele & adts, avere & avele, avere & adts, t_and_e & etsc 
#least similar (largest distance) are rai & faep, etsc & ebb, rai & etrma


# use a Barnes-Hut t-Distributed Stochastic Neighbor Embedding algorithm to reduce the dimensionality to 2 
#dimensions so that we can plot submissions against eachother:
library(Rtsne)
library(ggplot2)
library(ggrepel)
# Set seed:
set.seed(12345)
# Generate the 2-dimenstional reduced embeddings:
tsne <- Rtsne(final, perplexity= 3, pca = FALSE, is_distance = TRUE, dims =3)
# take the results and put them in a data frame for easy plotting:
results <- data.frame(names = rownames(final),
                      x = tsne$Y[,1],
                      y = tsne$Y[,2],
                      stringsAsFactors = FALSE)

ggplot(results, aes(x = x, y = y, label = names)) +
  geom_point(color=custom.col, size=6) +
  geom_text_repel()

custom.col <- c("#D16103",
                "#4E84C4",
                "#4E84C4",
                "#4E84C4",
                "#4E84C4",
                "#C4961A",
                "#C4961A",
                "#4E84C4",
                "#4E84C4",
                "#C4961A",
                "#C4961A",
                "#C4961A",
                "#C4961A",
                "#52854C",
                "#52854C",
                "#52854C",
                "#D16103",
                "#D16103",
                "navyblue",
                "navyblue",
                "#D16103",
                "#52854C",
                "#D16103",
                "#52854C",
                "#C4961A",
                "#D16103",
                "#52854C")


dev.off()


#implementing SW-algorithm
library(textreuse)
path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/EU_Data/Texts/Final"
corpus <- TextReuseCorpus(dir = path_data)

alignment<-align_local(corpus[["avele"]], corpus[["adts"]])
str(alignment)

alignment<-align_local(corpus[["press release feb"]], corpus[["avele"]])
str(alignment)
alignment[[1]]
alignment[[2]]


