#install.packages("quanteda")
#install.packages("readtext")

library(quanteda)
library(readtext)
library(stringi)

getwd()
setwd("/Users/carminadietrich/Desktop/EUPData_new/Texts/Wordfish")
                       
                       
path_data <- "/Users/carminadietrich/Desktop/EUPData_new/Texts/Wordfish"


files <- list.files(path = ".", recursive = TRUE,
         pattern = "\\*", 
         full.names = TRUE)

data <- readtext(paste0(path_data, files))


