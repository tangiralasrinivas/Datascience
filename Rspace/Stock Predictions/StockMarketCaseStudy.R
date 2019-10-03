## ----size="tiny", message=FALSE, warning=FALSE---------------------------
library(xts)  # extra package to install
data(GSPC,package="DMwR2")
xts::first(GSPC)
xts::last(GSPC)
dim(GSPC)

## ----size="tiny",out.width="0.9\\textwidth",size="scriptsize",message=FALSE----
library(quantmod) # extra package you need to install
plot(Cl(GSPC),main="Close SP500")

## ----size="scriptsize",message=FALSE,tidy=FALSE,warning=FALSE,eval=FALSE----
getSymbols('MSFT',from='2010-01-01')
getFX("USD/EUR")
getMetals("Gold")


## ----size="scriptsize",tidy=FALSE----------------------------------------
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)
  v[1] <- Cl(quotes)[1] 
  
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x)
  
  x <- apply(r,1,function(x) 
    sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

## ----eval=FALSE,size="tiny",message=FALSE--------------------------------
candleChart(last(GSPC,'3 months'),theme='white',TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()


## ----size="tiny",message=FALSE,tidy=FALSE--------------------------------
library(TTR)
myATR        <- function(x) ATR(HLC(x))[,'atr']
mySMI        <- function(x) SMI(HLC(x))[, "SMI"]
myADX        <- function(x) ADX(HLC(x))[,'ADX']
myAroon      <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator
myEMV        <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]
myMACD       <- function(x) MACD(Cl(x))[,2]
myMFI        <- function(x) MFI(HLC(x),  Vo(x))
mySAR        <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1]
myVolat      <- function(x) volatility(OHLC(x),calc="garman")[,1]

## ----size="scriptsize", tidy=FALSE, message=FALSE, warning=FALSE---------
data.model <- specifyModel(T.ind(GSPC) ~ myATR(GSPC) + mySMI(GSPC) +  
                             myADX(GSPC) + myAroon(GSPC) + myEMV(GSPC) + 
                             myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + mySAR(GSPC) + 
                             runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

Tdata.train <- as.data.frame(modelData(data.model,
                                       data.window=c('1970-01-02','2005-12-30')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                                              data.window=c('2006-01-01','2016-01-25'))))
Tform <- as.formula('T.ind.GSPC ~ .')  # the formula to be used in models

## ----size="tiny"---------------------------------------------------------
head(Tdata.train)



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


## ----size="scriptsize",message=FALSE,results="hide",tidy=FALSE,cache=FALSE----
library(performanceEstimation)
library(e1071)
library(plm)
library(randomForest)
exp <- performanceEstimation(
  PredTask(Tform, Tdata.train[1:2000,], 'SP500'),   
  c(Workflow('standardWF', wfID="standSVM",
             learner='svm',learner.pars=list(cost=10,gamma=0.01)),
    Workflow(learner='randomForest', learner.pars=list(ntree=500)),
    # Workflow(learner='plm', learner.pars=list(effects='twoways')),
    Workflow('timeseriesWF', wfID="slideSVM", 
             type="slide", relearn.step=90,
             learner='svm',learner.pars=list(cost=10,gamma=0.01))
  ),
  EstimationTask(metrics="theil",
                 method=MonteCarlo(nReps=10,szTrain=0.5,szTest=0.25)))

## ----size="tiny",cache=FALSE---------------------------------------------
summary(exp)   

## ----size="scriptsize",fig.width=8,fig.height=8,out.width="0.5\\textwidth",cache=FALSE----
plot(exp)   

## ----message=FALSE,size="small"------------------------------------------
library(earth)  # extra package to install
data(Boston, package="MASS") 
sp <- sample(1:nrow(Boston),as.integer(0.7*nrow(Boston)))
tr <- Boston[sp,]
ts <- Boston[-sp,]
mars <- earth(medv ~ .,tr)
preds <- predict(mars,ts)
(mae <- mean(abs(ts$medv - preds)))

## ----size="tiny"---------------------------------------------------------
summary(mars) 


## ----size="scriptsize",message=FALSE,tidy=FALSE--------------------------
library(DMwR2)
library(quantmod)
data(GSPC, package="DMwR2")
## Train and test periods used in this illustration
start <- 1
len.tr <- 1000  # first 1000 for training models
len.ts <- 500   # next 500 for testing them
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
## The market quotes during this "testing period"
## This will be used by the simulator
## Note: you need the training data created previously!
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date,'/',sep='')][1:len.ts]

## ----echo=FALSE----------------------------------------------------------
source("twoPolicies.R")

## ----eval=FALSE,size="scriptsize"----------------------------------------
## ## First we need the code implementing the 2 policies
## source("twoPolicies.R")  # file to be downloaded from course site

## ----size="scriptsize",message=FALSE,tidy=FALSE--------------------------
library(e1071)
# s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
r <- randomForest(Tform,Tdata.train[tr,],ntree = 500)
p <- predict(r,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1) # predictions to signals
## now using the simulated trader
t1 <- trading.simulator(market,sig,
                        'policy.1',  # the policy function name
                        list(exp.prof=0.05,bet=0.2,hold.time=30))
t1

## ----size="scriptsize"---------------------------------------------------
summary(t1)

## ----size="scriptsize",warning=FALSE,out.width="0.55\\textwidth"---------
plot(t1,market,theme='white',name='SP500') 

## ----size="scriptsize",message=FALSE,tidy=FALSE--------------------------
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))

## ----size="scriptsize",message=FALSE,tidy=FALSE--------------------------
summary(t2)

## ----size="scriptsize",message=FALSE,tidy=FALSE--------------------------
tradingEvaluation(t2) 

## ----size="scriptsize",warning=FALSE,out.width="0.55\\textwidth"---------
plot(t2,market,theme='white',name='SP500')


## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
library(DMwR2)
library(quantmod)
library(performanceEstimation)
library(e1071)
## The following files should be in your current
## working directory after you download them from
## the course web page
load("SP.train.eval.Rdata")
source("twoPolicies.R")
source("tradingWF.R")

## ----size="tiny",tidy=FALSE,eval=FALSE-----------------------------------
## NOTE: The following code should take a bit of time
## (depending on your hardware) to run. You may avoid
## that by skipping the code and just load the object
## "mc.res" from the file "mcresObj.Rdata" availabe
## at the course web site (check the following code
## chunk to see the loading instruction).
mc.res <- performanceEstimation(
  PredTask(Tform,Tdata.train),
  workflowVariants('tradingWF',
                   quotes='GSPC',
                   learner='svm',
                   pred.target="indicator",
                   learner.pars=list(cost=c(1,10),gamma=0.01),
                   b.t=c(0.01,0.05),s.t=c(-0.01,-0.05),
                   policy='policy.2',
                   policy.pars=list(bet=c(0.2,0.5),
                                    exp.prof=0.05,max.loss=0.05)
  ),
  ## Monte Carlo repeating 5 times: 10y for training and 5y for testing
  EstimationTask(method=MonteCarlo(nReps=5,szTrain=2540,szTest=1270,seed=1234),
                 evaluator="tradingEval")
)

## ----echo=FALSE,message=FALSE--------------------------------------------
## In case you have not executed the above code of the
## model selection just load the object from this file
## after downloading the file from the course web site
load("mcresObj.Rdata")

## ----size="scriptsize",tidy=FALSE,warning=FALSE--------------------------
mc.res2 <- subset(mc.res, 
                  metrics=c("PercProf", "Ret",  "MaxDD"),
                  partial=FALSE)

topPerformers(mc.res2, maxs=c(TRUE,TRUE,FALSE))

## ----size="scriptsize",out.width="0.5\\textwidth"------------------------
plot(subset(mc.res2,metrics="Ret"))


## ----size="scriptsize",tidy=FALSE,message=FALSE,warning=FALSE------------
getWorkflow("tradingWF.v5",mc.res)

## ----size="tiny",tidy=FALSE----------------------------------------------
summary(subset(mc.res2, workflows="tradingWF.v5"))


## ----size="tiny",tidy=FALSE----------------------------------------------
t <- tradingWF(Tform,Tdata.train[(nrow(Tdata.train)-2540+1):nrow(Tdata.train),],
               Tdata.eval,pred.target="indicator",
               learner="svm",learner.pars=list(cost=1,gamma=0.01),
               quotes="GSPC",
               b.t=0.01,s.t=-0.05,
               policy="policy.2",
               policy.pars=list(bet=0.2,exp.prof=0.05,max.loss=0.05))
tradingEval(t$trueSigs,t$predSigs,t$tradeRec)


## ----echo=FALSE,size="tiny",tidy=FALSE,out.width="0.5\\textwidth",message=FALSE----
tr <- t$tradeRec
market <- GSPC[paste(start(tr@trading),end(tr@trading),sep='/'),]
plot(tr,market,theme="white",name="SP500")

