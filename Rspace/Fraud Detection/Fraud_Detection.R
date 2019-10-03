## ----message=FALSE,warning=FALSE,size="scriptsize"-----------------------
library(dplyr)
data(sales,package="DMwR2")  # load the data without loading the pack.
sales



## ----size="tiny"---------------------------------------------------------
summary(sales)
nrow(filter(sales,is.na(Quant), is.na(Val)))
table(sales$Insp)/nrow(sales)*100


## ----size="scriptsize"---------------------------------------------------
sales <- mutate(sales, Uprice = Val / Quant)
summary(sales$Uprice)


## ----out.width="0.8\\textwidth",size="tiny",tidy=FALSE-------------------
library(ggplot2)
ids <- group_by(sales, ID)
ggplot(summarize(ids, nTrans=n()), 
       aes(x=ID,y=nTrans)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x=element_blank()) + 
  xlab("Salesmen") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Salesman")


## ----out.width="0.8\\textwidth",size="tiny",tidy=FALSE-------------------
prods <- group_by(sales, Prod)
ggplot(summarize(prods, nTrans=n()),
       aes(x=Prod,y=nTrans)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x=element_blank()) + 
  xlab("Product") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Product")

## ----size="scriptsize",tidy=FALSE----------------------------------------
nrow(summarise(prods,nTrans=n()) %>% filter(nTrans < 20))

## ----size="scriptsize",tidy=FALSE----------------------------------------
## The following is not part of the slides but you may be interested
## in trying this top_n() function
mpProds <- summarize(prods, medianPrice=median(Uprice,na.rm=TRUE))
top_n(mpProds,1,medianPrice)
top_n(mpProds,-1,medianPrice)

## ----size="scriptsize",tidy=FALSE----------------------------------------
mpProds <- summarize(prods, medianPrice=median(Uprice,na.rm=TRUE))
bind_cols(mpProds %>% arrange(medianPrice) %>% slice(1:5),
          mpProds %>% arrange(desc(medianPrice)) %>% slice(1:5))


## ----out.width="\\textwidth",echo=FALSE,warning=FALSE, message=FALSE-----
library(ggplot2)
library(forcats)
ggplot(filter(sales, Prod %in% c("p3689","p560")),
       aes(x=fct_drop(Prod),y=Uprice)) + geom_boxplot() + 
  scale_y_log10() + 
  xlab("") + ylab("log10(UnitPrice)")


## ----size="scriptsize",tidy=FALSE,warning=FALSE,message=FALSE------------
nrow(summarise(ids,nTrans=n()) %>% filter(nTrans < 10))


## ----size="scriptsize",tidy=FALSE----------------------------------------
tvIDs <- summarize(ids,totalVal=sum(Val,na.rm=TRUE))
top_n(tvIDs,1,totalVal)
top_n(tvIDs,-1,totalVal)


## ----size="scriptsize",tidy=FALSE,warning=FALSE,message=FALSE------------
tvIDs <- summarize(ids,totalVal=sum(Val,na.rm=TRUE))
bind_cols(tvIDs %>% arrange(totalVal) %>% slice(1:5),
          tvIDs %>% arrange(desc(totalVal)) %>% slice(1:5))

## ----size="scriptsize",tidy=FALSE----------------------------------------
arrange(tvIDs,desc(totalVal)) %>% slice(1:100) %>% 
  summarize(t100=sum(totalVal)) / 
  (summarize(tvIDs,sum(totalVal))) * 100
arrange(tvIDs,totalVal) %>% slice(1:2000) %>% 
  summarize(b2000=sum(totalVal)) / 
  (summarize(tvIDs,sum(totalVal))) * 100

## ----size="scriptsize",tidy=FALSE----------------------------------------
nouts <- function(x) length(boxplot.stats(x)$out)
noutsProds <- summarise(prods,nOut=nouts(Uprice))
arrange(noutsProds,desc(nOut))

## ----size="scriptsize",tidy=FALSE----------------------------------------
summarize(noutsProds,totalOuts=sum(nOut))
summarize(noutsProds,totalOuts=sum(nOut))/nrow(sales)*100


######################################################
## UNKNOWN's
######################################################


## ----echo=FALSE,message=FALSE--------------------------------------------
## This is not necessary, it is just if you want to start from here
## It is essentially re-calculating and re-loading the basic
## objects that are necessary for the following code
## If you started from the begining you can safly skip this chunck
library(dplyr)
data(sales,package="DMwR2")  # load the data without loading the pack.
sales <- mutate(sales,Uprice = Val / Quant)
prods <- group_by(sales,Prod)
ids <- group_by(sales,ID)

## ----size="scriptsize"---------------------------------------------------
nas <- filter(sales,is.na(Quant), is.na(Val)) %>% select(ID,Prod)

## ----size="tiny",tidy=FALSE,tidy.opts=list(width.cutoff=80)--------------
prop.naQandV <- function(q,v) 
  100*sum(is.na(q) & is.na(v))/length(q)
summarise(ids,nProbs=prop.naQandV(Quant,Val)) %>%
  arrange(desc(nProbs))

## ----size="tiny",tidy=FALSE,tidy.opts=list(width.cutoff=80)--------------
summarise(prods,nProbs=prop.naQandV(Quant,Val)) %>%
  arrange(desc(nProbs))

## ----size="scriptsize"---------------------------------------------------
sales <- filter(sales,!(is.na(Quant) & is.na(Val)))

## ----size="tiny",tidy=FALSE,tidy.opts=list(width.cutoff=80)--------------
prop.nas <- function(x) 100*sum(is.na(x))/length(x)
propNAsQp <- summarise(prods,Proportion=prop.nas(Quant))
arrange(propNAsQp,desc(Proportion))

## ----size="scriptsize"---------------------------------------------------
nlevels(sales$Prod)
sales <- droplevels(filter(sales,!(Prod %in% c("p2442", "p2443"))))
nlevels(sales$Prod)

## ----size="tiny",tidy=FALSE,tidy.opts=list(width.cutoff=80)--------------
summarise(ids,Proportion=prop.nas(Quant)) %>% 
  arrange(desc(Proportion))

## ----size="tiny",tidy=FALSE,tidy.opts=list(width.cutoff=80)--------------
summarise(prods,Proportion=prop.nas(Val)) %>% 
  arrange(desc(Proportion))

## ----size="tiny",tidy=FALSE,tidy.opts=list(width.cutoff=80)--------------
summarise(ids,Proportion=prop.nas(Val)) %>% 
  arrange(desc(Proportion))

## ----size="scriptsize",tidy=FALSE----------------------------------------
tPrice <- filter(sales, Insp != "fraud") %>% 
  group_by(Prod) %>% 
  summarise(medianPrice = median(Uprice,na.rm=TRUE))

## ----size="scriptsize",tidy=FALSE, message=FALSE-------------------------
noQuantMedPrices <- filter(sales, is.na(Quant)) %>% 
  inner_join(tPrice) %>% select(medianPrice)
noValMedPrices <- filter(sales, is.na(Val)) %>% 
  inner_join(tPrice) %>% select(medianPrice)

noQuant <- which(is.na(sales$Quant))
noVal <- which(is.na(sales$Val))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val']/noQuantMedPrices)
sales[noVal,'Val'] <- sales[noVal,'Quant'] * noValMedPrices

sales$Uprice <- sales$Val/sales$Quant 

save(sales, file="SalesReady.Rdata")

######################################################
## EVAL
######################################################

## ----echo=FALSE,message=FALSE--------------------------------------------
## This code chunck is only necessary if you want to pick up from here
## This involves downloading the file "SalesReady.Rdata" from the 
## course Web site. This file contains the "sales" object with all
## NAs removed.
library(dplyr)
load("SalesReady.Rdata")

## ----size="small"--------------------------------------------------------
100*sum(sales$Insp == "unkn")/nrow(sales)

## ----size="tiny",message=FALSE-------------------------------------------
library(ROCR)
data(ROCR.simple)
head(ROCR.simple$predictions)
head(ROCR.simple$labels)

## ----size="tiny",tidy=FALSE,out.width="\\textwidth",message=FALSE--------
library(DMwR2)
PRcurve(ROCR.simple$predictions, ROCR.simple$labels,
        main="Precision/Recall Curve")

## ----size="tiny",message=FALSE-------------------------------------------
library(ROCR)
data(ROCR.simple)
head(ROCR.simple$predictions)
head(ROCR.simple$labels)

## ----size="tiny",tidy=FALSE,out.width="\\textwidth"----------------------
CRchart(ROCR.simple$predictions, ROCR.simple$labels , 
        main='Cumulative Recall Chart')

## ----size="tiny",tidy=FALSE----------------------------------------------
avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats)) 
    stop('Provide either the training data or the product stats')
  if (missing(stats)) {
    stats <- as.matrix(filter(train,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
    rownames(stats) <- levels(train$Prod)
    stats[which(stats[,'iqr']==0),'iqr'] <- stats[which(stats[,'iqr']==0),'median']
  }
  
  return(mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
                stats[toInsp$Prod,'iqr']))
}

## ----size="scriptsize",tidy=FALSE----------------------------------------
evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds,...) 
{
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[1:nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}


######################################################
## EVAL
######################################################

## ----echo=FALSE,message=FALSE--------------------------------------------
library(dplyr)
load("SalesReady.Rdata")

## ----size="tiny",tidy=FALSE----------------------------------------------
globalStats <- as.matrix(filter(sales,Insp != 'fraud') %>%
                           group_by(Prod) %>% 
                           summarise(median=median(Uprice),iqr=IQR(Uprice)) %>% 
                           select(median,iqr))
rownames(globalStats) <- levels(sales$Prod)
globalStats[which(globalStats[,'iqr']==0), 'iqr'] <- 
  globalStats[which(globalStats[,'iqr']==0), 'median']
head(globalStats,3)

######################################################
## UNSUPERVISED
######################################################

## ----echo=FALSE,message=FALSE--------------------------------------------
library(DMwR2)
library(dplyr)
load("SalesReady.Rdata")
source(paste("./", "evaluationCode.R", sep = ""), encoding = "utf-8")  # evaluationNew2c.R at code

## ----size="scriptsize",tidy=FALSE----------------------------------------
BPrule.wf <- function(form,train,test,...) {
  require(dplyr, quietly=TRUE)
  ms <- as.matrix(filter(train,Insp != 'fraud') %>%
                    group_by(Prod) %>%
                    summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                    select(median,iqr))
  rownames(ms) <- levels(train$Prod)
  ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
  ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
    ms[test$Prod,'iqr']
  rankOrder <- order(ORscore,decreasing=T)
  res <- list(testSet=test,rankOrder=rankOrder,
              probs=matrix(c(ORscore,ifelse(test$Insp=='fraud',1,0)),
                           ncol=2))
  res
}

## ----echo=FALSE,size="scriptsize",tidy=FALSE,message=FALSE,results="hide"----
library(performanceEstimation)
bp.res <- performanceEstimation(
  PredTask(Insp ~ .,sales),
  Workflow("BPrule.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1,
                                     statsProds=globalStats))
)

## ----size="scriptsize"---------------------------------------------------
summary(bp.res)

## ----out.width="0.7\\textwidth",fig.width=10,fig.height=6,size="tiny",tidy=FALSE----
par(mfrow=c(1,2))
ps.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,1])
ts.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",avg="vertical")
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',avg='vertical')



## ----size="tiny",tidy=FALSE----------------------------------------------
LOF.wf <- function(form, train, test, k, ...) {
  require(DMwR2, quietly=TRUE)  
  ntr <- nrow(train)
  all <- as.data.frame(rbind(train,test))
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  
  res <- list(testSet=test,
              rankOrder=order(all[(ntr+1):N,'lof'],decreasing=TRUE),
              probs=as.matrix(cbind(all[(ntr+1):N,'lof'],
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}

## ----size="scriptsize",tidy=FALSE,message=FALSE,results="hide"-----------
lof.res <- performanceEstimation(
  PredTask(Insp ~ .,sales),
  Workflow("LOF.wf",k=7),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1,
                                     statsProds=globalStats))
)

## ----size="scriptsize"---------------------------------------------------
summary(lof.res)


## ----echo=FALSE,out.width="0.9\\textwidth",fig.width=10,fig.height=6,size="scriptsize",tidy=FALSE----
par(mfrow=c(1,2))
ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])
ts.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,xlim=c(0,1),
        ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))


######################################################
## CLASSIF
######################################################

## ----echo=FALSE,message=FALSE--------------------------------------------
library(DMwR2)
library(dplyr)
## In the end it should be the following
library(performanceEstimation)

load("SalesReady.Rdata")
load("resUnsupNew2.Rdata")
source("evaluationCode.R")
source("workflowsCode.R")  

## ----size="scriptsize",tidy=FALSE----------------------------------------
table(sales[sales$Insp != 'unkn','Insp'])/
  nrow(sales[sales$Insp != 'unkn',]) 

## ----size="scriptsize",tidy=FALSE----------------------------------------
NBsm.wf <- function(form,train,test,C.perc=list(ok=0.9, fraud=0.1),dist="HEOM",...) {
  require(e1071,quietly=TRUE)
  require(UBL,quietly=TRUE)
  
  sup <- which(train$Insp != 'unkn')
  data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  # newData <- SmoteClassif(Insp ~ .,data,C.perc=C.perc,dist=dist,...)
  model <- naiveBayes(Insp ~ .,data)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
  rankOrder <- order(preds[,'fraud'],decreasing=T)
  rankScore <- preds[,'fraud']
  
  res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}

## ----size="scriptsize",tidy=FALSE,message=FALSE,results="hide"-----------
nbs.res <- performanceEstimation(
  PredTask(Insp ~ ., sales),
  Workflow("NBsm.wf"),
  EstimationTask(metrics = c("Precision","Recall","avgNDTP"),
                 method = Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                 evaluator = "evalOutlierRanking",
                 evaluator.pars = list(Threshold=0.1,
                                       statsProds=globalStats))
)

## ----size="scriptsize"---------------------------------------------------
summary(nbs.res)


## ----echo=FALSE,out.width="0.9\\textwidth",fig.width=10,fig.height=6,size="scriptsize",tidy=FALSE----
par(mfrow=c(1,2))
ps.nbs <- sapply(getIterationsInfo(nbs.res), function(i) i$probs[,1])
ts.nbs <- sapply(getIterationsInfo(nbs.res), function(i) i$probs[,2])
PRcurve(ps.nbs,ts.nbs,main="PR curve",lty=1,xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.bp,ts.bp,add=T,lty=1,col="red",avg='vertical')
legend('topright',c('smote Naive Bayes','BP rule'),lty=c(1,1),col=c("black","red"))
CRchart(ps.nbs,ts.nbs,main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.bp,ts.bp,add=T,lty=1,col="red",avg='vertical')
legend('bottomright',c('smote Naive Bayes','BP rule'),lty=c(1,1),col=c("black","red"))


## ----size="scriptsize",tidy=FALSE----------------------------------------
rf.wf <- function(form,train,test,C.perc="extreme",dist="HEOM",ntrees=5000,...) {
  require(ranger,quietly=TRUE)
  sup <- which(train$Insp != 'unkn')
  data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  newData <- SmoteClassif(Insp ~ .,data,C.perc=C.perc,dist=dist,...)
  model <- ranger(Insp ~ .,newData,
                  num.trees = ntrees, probability=TRUE)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')])$predictions
  rankOrder <- order(preds[,"fraud"],decreasing=TRUE)
  rankScore <- preds[,"fraud"]
  
  res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}

## ----size="scriptsize",tidy=FALSE,message=FALSE,results="hide"-----------
rf.res <- performanceEstimation(
  PredTask(Insp ~ .,sales),
  Workflow("rf.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1,
                                     statsProds=globalStats))
)

## ----size="scriptsize"---------------------------------------------------
summary(rf.res)


## ----echo=FALSE,out.width="0.9\\textwidth",fig.width=10,fig.height=6,size="tiny",tidy=FALSE----
par(mfrow=c(1,2)) 
ps.rf <- sapply(getIterationsInfo(rf.res), function(i) i$probs[,1])
ts.rf <- sapply(getIterationsInfo(rf.res), function(i) i$probs[,2])
PRcurve(ps.nbs,ts.nbs,main="PR curve",lty=1,xlim=c(0,1),
        ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=T,lty=1,col="red",avg='vertical')
PRcurve(ps.rf,ts.rf,add=T,lty=1,col="green",avg='vertical')
legend('topright',c('smote Naive Bayes','LOF','RFs'),
       lty=1,col=c("black","red","green"))
CRchart(ps.nbs,ts.nbs,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=1,col="red",avg='vertical')
CRchart(ps.rf,ts.rf,add=T,lty=1,col="green",avg='vertical')
legend('bottomright',c('smote Naive Bayes','LOF','RFs'),
       lty=1,col=c("black","red","green"))

######################################################
## SEMISUP
######################################################

## ----echo=FALSE,message=FALSE--------------------------------------------
library(DMwR2)
library(dplyr)
## In the end it should be the following
library(performanceEstimation)

# load("SalesReady.Rdata")
# load("resUnsupNew2.Rdata")
# load("resSupNew3.Rdata")
# source("evaluationCode.R")
# source("workflowsCode.R")

## ----size="tiny",tidy=FALSE----------------------------------------------
pred.rf <- function(m,d) {
  p <- predict(m,d)$predictions
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}

rf.st.wf <- function(form,train,test,ntrees=1000,...) {
  require(ranger,quietly=TRUE)
  require(DMwR2,quietly=TRUE)
  train <- as.data.frame(train[,c('ID','Prod','Uprice','Insp')])
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- DMwR2::SelfTrain(form,train,
                            learner='ranger',
                            learner.pars=list(num.trees=ntrees, probability=TRUE),
                            pred='pred.rf')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')])$predictions
  
  rankOrder <- order(preds[,'fraud'],decreasing=T)
  rankScore <- preds[,"fraud"]
  
  res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}

## ----size="scriptsize",tidy=FALSE,message=FALSE,results="hide"-----------
rf.st.res <- performanceEstimation(
  PredTask(Insp ~ .,sales),
  Workflow("rf.st.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1,
                                     statsProds=globalStats))
)

## ----size="scriptsize"---------------------------------------------------
summary(rf.st.res)


## ----echo=FALSE,out.width="0.9\\textwidth",fig.width=10,fig.height=6,size="tiny",tidy=FALSE----
par(mfrow=c(1,2))
ps.rf.st <- sapply(getIterationsInfo(rf.st.res), function(i) i$probs[,1])
ts.rf.st <- sapply(getIterationsInfo(rf.st.res), function(i) i$probs[,2])

PRcurve(ps.nbs,ts.nbs,main="PR curve",lty=1,xlim=c(0,1),
        ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=T,lty=1,col="red",avg='vertical')
PRcurve(ps.rf,ts.rf,add=T,lty=1,col="green",avg='vertical')
PRcurve(ps.rf.st,ts.rf.st,add=T,lty=1,col="blue",avg='vertical')
legend('topright',c('smote Naive Bayes','LOF','RFs','RFs-ST'),
       lty=1,col=c("black","red","green","blue"))

CRchart(ps.nbs,ts.nbs,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=1,col="red",avg='vertical')
CRchart(ps.rf,ts.rf,add=T,lty=1,col="green",avg='vertical')
CRchart(ps.rf.st,ts.rf.st,add=T,lty=1,col="blue",avg='vertical')
legend('bottomright',c('smote Naive Bayes','LOF','RFs','RFs-ST'),
       lty=1,col=c("black","red","green","blue"))
