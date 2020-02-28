# fitting correspondence analysis
?textmodel_ca
wca <- textmodel_ca(mydfm)#, nd=2) # two dimensions
summary(wca)
textplot_scale1d(wca)

#If you want to plot documents on multi-dimensional scale, you use coef() to obtain coordinates of lower dimensions.
dat_ca <- data.frame(dim1 = coef(wca, doc_dim = 1)$coef_document, 
                     dim2 = coef(wca, doc_dim = 2)$coef_document)
head(dat_ca)

plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2')
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))


# looking at organisations' estimated positions
wca$rowcoord

# words' positions
head(wca$colcoord)

# comparing wordfish and CA results
plot(wf$theta, wca$rowcoord[,1], 
     xlab="Wordfish theta-hat", ylab="CA dim 1 coordinate",
     main="Wordfish v CA dim 1", pch=19, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
abline(lm(wca$rowcoord[,1] ~ wf$theta), col="grey50", lty="dotted")


install.packages("ca")
library(ca)
mod <- ca(as.matrix(mydfm))
mod$rowcoord
plot(mod$rowcoord[,1:2]) #plots 2 dimensions and you can see that there is something weird going on, which would
#not have been visible if I plotted it only on one dimension (i.e. Wordfish). It is FAEP (you can see this by putting in mod$rowcoord,
#looking at DIM2 and at around 8 you can see FAEP
summary(mod) #words that have a really positive k=2 have a part in pushing that up, e.g. the word adeq is at 1097.
