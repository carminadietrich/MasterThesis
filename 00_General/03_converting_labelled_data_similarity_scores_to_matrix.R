library(tidyverse)
lp <- read.csv("LeePincombeWelshData.csv")
lp <- group_by(lp, Document1, Document2)
gold <- summarize(lp, av_sim = mean(Similarity), 
                  se = sqrt(var(Similarity)/n()))

goldsim <- matrix(1, nrow = 50, ncol = 50)
for (i in 1:nrow(gold))
  goldsim[gold$Document1[i], gold$Document2[i]] <- gold$av_sim[i]

cons_goldsim <- (t(goldsim) + goldsim)/2

#Dataset available at https://webfiles.uci.edu/mdlee/LeePincombeWelsh.zip