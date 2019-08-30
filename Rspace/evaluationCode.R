CRchart <- function(preds, trues, ...) {
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "rec", "rpp")
  plot(pf, ...)
}

PRcurve <- function(preds, trues, ...) {
  require(ROCR, quietly = TRUE)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "prec", "rec")
  pf@y.values <- lapply(pf@y.values, 
                        function(x) rev(cummax(rev(x))))
  plot(pf, ...)
}

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


evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds,...) {
   ordTS <- testSet[rankOrder,]
   N <- nrow(testSet)
   nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
   cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
   prec <- cm['fraud','fraud']/sum(cm['fraud',])
   rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
   AVGndtp <- avgNDTP(ordTS[1:nF,],stats=statsProds)
   return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}


globalStats <- as.matrix(filter(sales,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
rownames(globalStats) <- levels(sales$Prod)
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
    globalStats[which(globalStats[,'iqr']==0),'median']

