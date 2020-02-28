library(textreuse)
#'vignette("textreuse-alignment", package = "textreuse")
#path_data <- "/Users/carminadietrich/Desktop/EUPData_new/Texts/Wordfish"
#path_data <- "C:/Users/174454/Desktop/EUPData_new/Texts/Wordfish"

corpus <- TextReuseCorpus(dir = path_data)

align_local(corpus[["press release dec"]], corpus[["press release feb"]])
alignment <- align_local(corpus[["press release dec"]], corpus[["press release feb"]])
str(alignment)

#score of 71