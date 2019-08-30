library(readtext)
library(quanteda)

## ----size="scriptsize"---------------------------------------------------
txts <- readtext(paste0(".", "/KDEN.csv", sep=""),
                 text_field = "remarks")

## ----size="scriptsize"---------------------------------------------------
data(txts) # US presidential inaugural address texts corpus
dat <- dfm(txts)
dat

## ----size="scriptsize"---------------------------------------------------
dat <- dfm(data_corpus_inaugural, 
           remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
dat

## ----size="tiny",fig.width=7,fig.height=7,out.width="0.6\\textwidth"-----
set.seed(123)
textplot_wordcloud(dat, rotation = .25, max_words = 50)