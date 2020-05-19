library(quanteda)
library(readtext)
library(dplyr)
path_data <- "/Users/kathrindietrich/Desktop/041620MasterThesis/03_Data/EU_Data/Texts/Final"
files <- list.files(path = ".", recursive = TRUE,
                    pattern = "\\*", 
                    full.names = TRUE)
data <- readtext(paste0(path_data, files))
corpus<-corpus(data)

corpus %>% 
  summary(n = nrow(data)) %>% # include all posts in the summary
  summarize(doc_cnt = n(),
            avg_token = mean(Tokens),
            median_token = median(Tokens),
            Q3_token = quantile(Tokens, probs = 0.75),
            max_token = max(Tokens))
#doc_cnt avg_token median_token Q3_token max_token
#      27  1937.741         1109     2404      8027

docvars(corpus, field = "Category") <-c("Industry Groups",	"Alternative Industry Groups",	"Alternative Industry Groups",	"Alternative Industry Groups",	"Alternative Industry Groups",	"Other",	"Other",	"Alternative Industry Groups",	"Alternative Industry Groups", 	"Other",	"Other",	"Other", "Other",	"Environmental Groups",	"Environmental Groups",	"Environmental Groups",	"Industry Groups",	"Industry Groups",	"Commission",	"Commission",	"Industry Groups",	"Environmental Groups",	"Industry Groups",	"Environmental Groups",	"Other",	"Industry Groups", "Environmental Groups")
summary(corpus)

# wordcloud
dfmat <- dfm(corpus_subset(corpus, Category %in% c("Industry Groups", "Alternative Industry Groups", "Environmental Groups", "Other")),
              remove = stopwords("english"), stem=TRUE, remove_punct = TRUE, remove_numbers = TRUE, group = "Category") %>%
  dfm_trim(min_termfreq = 3, max_termfreq = 50)
textplot_wordcloud(dfmat2, max_size=3, comparison = TRUE, max_words = 600, labelsize=1.1)

