setwd("/Users/carminadietrich/Desktop/EUPData1/CarminaObjects")
setwd("C:/Users/174454/Desktop/EUPData_new/CarminaObjects")
rm(list = ls())
install.packages("ggplot2")
library(quanteda)
library(ggplot2)
load("mydfm.RData")


#estimating wordfish
wf <- textmodel_wordfish(mydfm, dir = c(17,28))
#looking at results
sort(predict(wf))
summary(wf)
str(wf)
textplot_scale1d(wf, margin = "documents")
textplot_scale1d(wf, margin = "features", 
                 highlighted = c("co2", "commission", "environment", "car", "economy"))

#groups = docvars(wf, "party")) #add in category of what it is to see it by lobby

#internal validation: frequent words (top of eiffel tower) should not discriminate between
#manifestos because they do no contain any political meaning. Thus they have large fixed effects associated with weights
#close to zero.More infrequently mentioned words are more likely part of politically relevant language and discriminate 
#between organisations. they have smaller positive or negative weights. Words with large weights have larger politically 
#relevant connotation

# extracting rest of parameters
str(coef(wf))

#look at the word-level parameters
sw <- data.frame( word=wf$features, beta=wf$beta, psi=wf$psi)
sw <- sw[order(sw$beta),] # discrimination
head(sw, n=20) #might show the more environmental side of the argument (e.g. methan)
tail(sw, n=20) #faep is the European Federation of Magazine Publishers (unclear what their participation in this con-
#sultation is about)

sw <- sw[order(sw$psi),] # frequency, psi	= estimated word fixed effects
head(sw, n=20)
tail(sw, n=20)


theta <- wf[["theta"]]
beta <- wf[["beta"]]
alpha <- wf[["alpha"]]
psi <- wf[["psi"]]
words <- wf[["features"]]

sum(theta[which(theta>0)])
sum(theta[which(theta<0)])
sum(theta)

hist(theta, breaks=30)
hist(alpha, breaks=30)
hist(beta, breaks=50)
hist(psi, breaks=50)

# Plot the estimated two document parameters: Theta against Alpha
ggplot(mapping = aes(x = wf$theta, y = wf$alpha, label = wf$docs)) + 
  geom_text(size = 1.5) + 
  labs(x = "Theta", y = "Alpha") +
  guides(size = "none", color = guide_legend(""))

