#prep data for WF
library(readtext)
library(quanteda.textmodels)
library(quanteda)
library(ggplot2)
library(ggrepel)

path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/EU_Data/Texts/Final"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files))
corpus<-corpus(data)
tokens<-tokens(corpus,
               what = "word",
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE)
tokens <- tokens %>%
  tokens_tolower() %>%
  tokens_remove(stopwords()) %>%
  tokens_keep(min_nchar = 3)  #%>%
  #tokens_wordstem()

featnames(dfm(tokens))
#dfmstem <-dfm(tokens, tolower = FALSE)
dfm<-dfm(tokens, tolower = FALSE)

#run WF

tm<-textmodel_wordfish(dfm, dir = c(19,20))
tm$theta
wf<-dist(tm$theta, upper = TRUE, diag = TRUE)
wf<-as.matrix(wf)
wf<-as.data.frame(as.matrix(wf))
colnames(wf) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "comm2", "comm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
rownames(wf) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "comm2", "comm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")

wf <- wf[-c(19:20), -c(1:18, 21:27)] # delete all columns apart from the commission statements, delete commission statements in the rows

#save(wf, file = "wf")
#write.csv(wf, "wf.csv")

#prep for merge with text reuse file
wf["name"] <- NA #create new column
wf$name <- c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
colnames(wf)[2] <- "wf_comm1"
colnames(wf)[1] <- "wf_comm2"

#prep glove file for merge 
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/files")
load("rwmd_glove")
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/03_comparisonWF")
rwmd<-as.data.frame(rwmd_glove)
rwmd<-(t(rwmd) + rwmd)/2
colnames(rwmd) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "comm2", "comm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
rownames(rwmd) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "comm2", "comm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")

rwmd <- rwmd[-c(19:20), -c(1:18, 21:27)] # delete columns apart from commisssion
name <- rownames(rwmd)
rownames(rwmd) <- NULL
rwmd <- cbind(name,rwmd)

colnames(rwmd)[2] <- "rwmd_comm2"
colnames(rwmd)[3] <- "rwmd_comm1"

#merge
object <- merge(rwmd, wf, by="name")  #convert names into rowlabels
rownames(object) = c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
object <- object[, -c(1)]
col_order <- c("rwmd_comm1", "rwmd_comm2", "wf_comm1", "wf_comm2")
mergedobject <- object[, col_order]
#save(mergedobject, file = "mergedobject")
#write.csv(mergedobject, "mergedobject.csv")

#normalize/standardize
#z-scores
z_scores<-scale(mergedobject, center = TRUE, scale = TRUE)
is(z_scores)
z_scores<-as.data.frame(z_scores)

#plot communication 1
library(ggplot2)

ggplot(z_scores, aes(x=wf_comm1, y=rwmd_comm1)) + 
  geom_point(colour = "#1380A1", size = 3) +
  geom_abline(intercept = 0, slope = 1)+
  geom_text(label=rownames(z_scores)) +
  labs(#title="Comparison WF and Text Reuse",
          # subtitle = "Policy Draft",
       x="Wordfish", y = "Text Reuse")

#plot communication 2
ggplot(z_scores, aes(x=wf_comm2, y=rwmd_comm2)) + 
  geom_point(colour = "#1380A1", size = 3) +
  geom_abline(intercept = 0, slope = 1)+
  geom_text(label=rownames(z_scores)) +
  labs(#title="Comparison WF and Text Reuse",
       #subtitle = "Final Legislation",
       x="Wordfish", y = "Text Reuse")

cor(c(as.matrix(z_scores$rwmd_comm1)), c(as.matrix(z_scores$wf_comm1)), method = "pearson") 
cor(c(as.matrix(z_scores$rwmd_comm2)), c(as.matrix(z_scores$wf_comm2)), method = "pearson") 

#do they move in the same direction?
library(dplyr)
will<-mergedobject %>% #change to z-scores to unscale
  mutate(diff_rwmd = rwmd_comm1 - rwmd_comm2) %>%
  mutate(diff_wf = wf_comm1 - wf_comm2)

cor(will$diff_rwmd, will$diff_wf)

ggplot(will, aes(x=diff_rwmd, y=diff_wf)) + 
  geom_point(colour = "#1380A1", size = 3) +
  geom_text(label=rownames(z_scores)) +
  geom_text_repel(label =rownames(z_scores)) +
  geom_abline(intercept = 0, slope = 1)+
  labs(#title="Comparison WF and Text Reuse",subtitle = "",
       x="Text Reuse", y = "Wordfish")



###generate pure WF graph (re-add column 19 for the graph)
library(ggplot2)
library(bbplot)
library(ggalt)
library(tidyr)
library(dplyr)
library(tibble)

wf$names<- NA
wf$names <- c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "EUcomm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
col_order <- c("names", "comm1", "comm2")
wf<- wf[, col_order]


#plot
d<-wf %>% 
  mutate(diff = comm2 - comm1)
d$names <- factor(d$names, levels = d$names[order(d$comm1)])
d$diff<-round(d$diff, 3)


hi<-ggplot(d, aes(x=comm1, xend=comm2, y=names)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=comm1, 
                   xend=comm2, 
                   y=names, 
                   yend=names), 
               color="#b2b2b2", size=1.5)+
  theme_minimal()+
  geom_dumbbell(color="grey60", 
                size=3,
                size_x=3.5, 
                size_xend = 3.5,
                colour_x="#F7BC08",
                colour_xend = "#395B74") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

hi + geom_text(aes(x=comm1, label="EU1", fontface = "bold"),
                     color="#F7BC08", size=2, hjust=1.5, show.legend = TRUE) +
  geom_text(aes(x=comm2, label="EU2", fontface="bold"), 
            color="#395B74", size=2, hjust=-1.5, show.legend = TRUE) #alt just put "black" for black labels
  

#labs(x="Change from Draft to Final Policy as generated by Wordfish ", y=NULL)









