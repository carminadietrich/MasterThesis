#install.packages("quanteda")
#install.packages("readtext")

library(quanteda)
library(readtext)


getwd()
setwd("C:/Users/174454/Desktop/EUPData/Texts/Wordfish")
                       
                       
path_data <- "C:/Users/174454/Desktop/EUPData/Texts/Wordfish"


files <- list.files(path = ".", recursive = TRUE,
         pattern = "\\.txt", 
         full.names = TRUE)


data <- readtext(paste0(path_data, files))
