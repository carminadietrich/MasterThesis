library(ggplot2)
library(bbplot)
library(ggalt)
library(tidyr)
library(dplyr)
library(tibble)

setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/files")
load("final")
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/02_policyanalysis")

final <- final[-c(19), -c(1:18, 21:27)]
final<-as.data.frame(final)
final<-as.data.frame(as.matrix(final))

final$names<- NA
final$names <- c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "EUcomm1", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
col_order <- c("names", "EUcomm1", "EUcomm2")
final<- final[, col_order]


#plot
d<-final %>% 
  mutate(diff = EUcomm2 - EUcomm1)
d$names <- factor(d$names, levels = d$names[order(d$EUcomm1)])
d$diff<-round(d$diff, 3)

plot<-ggplot(d, aes(x=EUcomm1, xend=EUcomm2, y=names)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=EUcomm1, 
                   xend=EUcomm2, 
                   y=names, 
                   yend=names), 
               color="#b2b2b2", size=1.5)+
  theme_minimal()+
  geom_dumbbell(color="grey60", 
                size=3,
                size_x=3.5, 
                size_xend = 3.5,
                colour_x="#F7BC08",
                colour_xend = "#395B74")+
  labs(x="Change from Draft to Final Policy (in RWMD) ", y=NULL)+
  geom_text(aes(x=EUcomm1, label="EU1", fontface = "bold"),
            color="#F7BC08", size=2, hjust=1.5, show.legend = TRUE)+  #alt just put "black" for black labels
  geom_text(aes(x=EUcomm2, label="EU2", fontface="bold"), 
            color="#395B74", size=2, hjust=-1.5, show.legend = TRUE) #alt just put "black" for black labels


plot + annotate(geom = "rect",
              xmin = .2,
              xmax = .379,
              ymin = as.numeric(d$names[d$names =="acea"]) - 0.3,
              ymax = as.numeric(d$names[d$names =="acea"]) + 0.3,
              alpha = .3,
              fill = "firebrick")+
  annotate(geom = "rect",
           xmin = .2,
           xmax = .379,
           ymin = as.numeric(d$names[d$names =="avere"]) - 0.3,
           ymax = as.numeric(d$names[d$names =="avere"]) + 0.3,
           alpha = .3,
           fill = "firebrick")+
  annotate(geom = "rect",
           xmin = .2,
           xmax = .379,
           ymin = as.numeric(d$names[d$names =="aegpl"]) - 0.3,
           ymax = as.numeric(d$names[d$names =="aegpl"]) + 0.3,
           alpha = .3,
           fill = "firebrick")+
  annotate(geom = "rect",
           xmin = .2,
           xmax = .379,
           ymin = as.numeric(d$names[d$names =="engva"]) - 0.3,
           ymax = as.numeric(d$names[d$names =="engva"]) + 0.3,
           alpha = .3,
           fill = "firebrick")+
  annotate(geom = "rect",
           xmin = .2,
           xmax = .379,
           ymin = as.numeric(d$names[d$names =="vda"]) - 0.3,
           ymax = as.numeric(d$names[d$names =="vda"]) + 0.3,
           alpha = .3,
           fill = "firebrick")+
  annotate(geom = "rect",
           xmin = .2,
           xmax = .379,
           ymin = as.numeric(d$names[d$names =="wwf"]) - 0.3,
           ymax = as.numeric(d$names[d$names =="wwf"]) + 0.3,
           alpha = .3,
           fill = "firebrick")


