## ----eval=FALSE,size="scriptsize"----------------------------------------
## install.packages(c("readtext","stringr","quanteda"))

## ----eval=FALSE,size="scriptsize"----------------------------------------
## ## A series of interesting text corpora (sets of documents)
## devtools::install_github("quanteda/quanteda.corpora")
## ## Some dictionaries
## devtools::install_github("kbenoit/quanteda.dictionaries")

## ------------------------------------------------------------------------
library(readtext)
# The data folder
dataF <- system.file("extdata/", package = "readtext")

## ------------------------------------------------------------------------
txts <- readtext(paste0(dataF, "/txt/UDHR/*"))

## ----size="scriptsize"---------------------------------------------------
readtext(paste0(dataF, "/txt/EU_manifestos/*.txt"),
         docvarsfrom = "filenames", 
         docvarnames = c("unit", "context", "year", "language", "party"),
         dvsep = "_", encoding = "ISO-8859-1")

## ----size="scriptsize"---------------------------------------------------
readtext(paste0(dataF, "/csv/inaugCorpus.csv"),
         text_field = "texts")

## ----size="scriptsize"---------------------------------------------------
readtext(paste0(dataF, "/json/inaugural_sample.json"),
         text_field = "texts")

## ----size="scriptsize"---------------------------------------------------
txts <- readtext(paste0(dataF, "/pdf/UDHR/*.pdf"),
                 docvarsfrom = "filenames", 
                 docvarnames = c("document", "language"),
                 sep = "_")

## ----size="scriptsize"---------------------------------------------------
readtext(paste0(dataF, "/word/*.docx"))

## ----size="scriptsize"---------------------------------------------------
x <- c("This is a text", 
       "This other contains <font size='10'>special</font> html code")
x
library(stringr)
x <- str_replace_all(x,"<.*?>","")
x

## ----size="scriptsize", message=FALSE------------------------------------
library(quanteda)
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt)

## ----size="scriptsize"---------------------------------------------------
tokens(txt, remove_numbers = TRUE, remove_punct = TRUE)

## ----size="scriptsize"---------------------------------------------------
x <- "This is an Important Statement"
tokens(x)
tokens_tolower(tokens(x))

## ----size="scriptsize"---------------------------------------------------
tokens_wordstem(tokens("He is running with higher intensity"))

## ----size="scriptsize"---------------------------------------------------
tokens_remove(tokens("He is running with higher intensity"),
              stopwords("english"))

## ----echo=FALSE,message=FALSE--------------------------------------------
library(readtext)
library(quanteda)
# The data folder
dataF <- system.file("extdata/", package = "readtext")

## ----size="scriptsize"---------------------------------------------------
txts <- readtext(paste0(dataF, "/txt/EU_manifestos/*.txt"),
                 docvarsfrom = "filenames", 
                 docvarnames = c("unit", "context", "year", "language", "party"),
                 dvsep = "_", encoding = "ISO-8859-1")
cp <- corpus(txts)
summary(cp,2) # Information on the first 2 documents 

## ----size="scriptsize"---------------------------------------------------
texts(cp)[1] # showing the text of the first document of the corpus 

## ----size="scriptsize"---------------------------------------------------
cp
corpus_subset(cp,language == "en") 

## ----size="tiny"---------------------------------------------------------
kwic(cp,"foreign") 

## ----size="scriptsize"---------------------------------------------------
tokens('an example of n-grams',ngrams=2)

## ----size="scriptsize"---------------------------------------------------
data("data_corpus_inaugural") # US presidential inaugural address texts corpus
dat <- dfm(data_corpus_inaugural)
dat

## ----size="scriptsize"---------------------------------------------------
dat <- dfm(data_corpus_inaugural, 
           remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
dat

## ----size="tiny"---------------------------------------------------------
topfeatures(dat,5)  # count
topfeatures(dat,5,scheme = "docfreq")  # by document
tf <- topfeatures(dat,5,groups="President")  # by docvar
tf[c("Clinton","Obama", "Trump")]

## ----size="tiny",fig.width=7,fig.height=7,out.width="0.6\\textwidth"-----
set.seed(123)
textplot_wordcloud(dat, rotation = .25, max_words = 50)

## ----size="tiny"---------------------------------------------------------
dat
dat <- dfm_trim(dat,min_termfreq = 2)  # eliminating terms occurrying in few docs
dat

## ----size="tiny"---------------------------------------------------------
dat[1:4,1:4]
dat <- dfm_tfidf(dat) # from absolute frequencies to TF-IDF weights
dat[1:4,1:4]

## ----eval=FALSE,size="scriptsize"----------------------------------------
## ## note that you need to have package devtools installed before to do this
## devtools::install_github("kbenoit/quanteda.dictionaries")

## ----message=FALSE,size="tiny"-------------------------------------------
library(quanteda)
library(quanteda.dictionaries)

## ----message=FALSE,size="tiny"-------------------------------------------
sent <- liwcalike(data_corpus_inaugural,data_dictionary_HuLiu)
head(sent)

## ----message=FALSE,size="tiny"-------------------------------------------
sent$balance <- sent$positive-sent$negative
head(sent[order(sent$balance,decreasing = TRUE),c("docname","balance")])
head(sent[order(sent$balance,decreasing = FALSE),c("docname","balance")])

## ----message=FALSE,size="tiny",fig.width=10,fig.height=9,out.height="0.6\\textheight"----
library(ggplot2)
ggplot(sent, aes(x=positive,y=negative,label=docname,color=balance)) +
  geom_text() + geom_abline(slope = 1, intercept = 0, color = "red")

## ----size="tiny"---------------------------------------------------------
library(quanteda.corpora)
head(summary(data_corpus_irishbudgets),2)
table(docvars(data_corpus_irishbudgets)$party)

## ----message=FALSE,size="tiny"-------------------------------------------
corp <- corpus_subset(data_corpus_irishbudgets, ! party %in% c("PBPA","SOC", "WUAG"))
dat <- dfm(corp, 
           remove = stopwords("english"), 
           stem = TRUE, 
           remove_punct = TRUE,
           remove_numbers = TRUE)
dat <- dfm_trim(dat, min_termfreq = 10)
dat <- dfm_tfidf(dat)
ds <- convert(dat, to = "data.frame")[,-1]
ds$party <- factor(docvars(dat)$party)

## ----message=FALSE,size="tiny"-------------------------------------------
library(e1071)
library(performanceEstimation)
res <- performanceEstimation(
  PredTask(party ~ ., ds),
  c(workflowVariants(learner="svm",learner.pars=list(cost=c(1,5,10))),
    workflowVariants(learner="naiveBayes",learner.pars=list(laplace=c(0,1)))),
  EstimationTask(metrics = "err", method = CV()))

## ----message=FALSE,size="tiny",fig.width=10,fig.height=7,out.width="0.7\\textwidth"----
plot(res)