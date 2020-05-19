library(ggplot2)
library(bbplot)
library(ggalt)
library(tidyr)
library(dplyr)
library(tibble)

setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/files")
load("final")
setwd("/Users/kathrindietrich/Desktop/041620MasterThesis/02_Analysis/02_policyanalysis")

final <- final[-c(19:20), -c(1:19, 21:27)]
final<-as.data.frame(final)
final<-as.data.frame(as.matrix(final))

final$names<- NA
final$names <- c("acea", "adts", "aegpl", "avele", "avere", "beuc", "bvrla", "ebb", "engva", "etrma", "etsc", "etuc", "faep", "fanc", "foe_uk", "greenpeace", "jama", "kama", "rai", "rspb", "smmt", "t_and_e", "uk_aa", "vda", "wwf")
col_order <- c("names", "final")
final<- final[, col_order]

final$names <- factor(final$names, levels = final$names[order(final$final)])
custom.col <-c("#D16103",
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
               "#D16103",
               "#52854C",
               "#D16103",
               "#52854C",
               "#C4961A",
               "#D16103",
               "#52854C")
bp<-ggplot(final, aes(x=names, y=final)) +
  geom_segment( aes(x=names, xend=names, y=0, yend=final), color="grey") +
  geom_point( color=custom.col, size=4) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("Interest Groups") +
  ylab("Distance to Policy Draft (RWMD)")

bp<-bp + ylim(0, 0.4)
bp<-bp + theme(axis.text.x = element_text(angle = 40))
bp






