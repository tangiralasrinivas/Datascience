#######################################################################
## The function to evaluate the workflows
#######################################################################
tradingEval <- function(trueSigs,predSigs,tradeRec,...) 
{
  ## Signals evaluation
  st <- sigs.PR(predSigs,trueSigs)
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec','rec'),each=3),c('s','b','sb'),sep='.')
  
  ## Trading record evaluation
  tradRes <- tradingEvaluation(tradeRec)
  return(c(st,tradRes))
}


#######################################################################
## Our trading workflow
#######################################################################
tradingWF <- function(form, train, test, 
                      quotes, pred.target="signals",
                      learner, learner.pars=NULL,
                      predictor.pars=NULL,
                      learn.test.type='fixed', relearn.step=30,
                      b.t, s.t,
                      policy, policy.pars,
                      trans.cost=5, init.cap=1e+06)
{
  ## obtain the model(s) and respective predictions for the test set
  if (learn.test.type == 'fixed') {  # a single fixed model
    m <- do.call(learner,c(list(form,train),learner.pars))
    preds <- do.call("predict",c(list(m,test),predictor.pars))
  } else {  # either slide or growing window strategies
    data <- rbind(train,test)
    n <- NROW(data)
    train.size <- NROW(train)
    sts <- seq(train.size+1,n,by=relearn.step)
    preds <- vector()
    for(s in sts) {  # loop over each relearn step
      tr <- if (learn.test.type=='slide') data[(s-train.size):(s-1),] 
      else data[1:(s-1),]
      ts <- data[s:min((s+relearn.step-1),n),]
      
      m <- do.call(learner,c(list(form,tr),learner.pars))
      preds <- c(preds,do.call("predict",c(list(m,ts),predictor.pars)))
    }    
  } 
  
  ## Getting the trading signals
  if (pred.target != "signals") {  # the model predicts the T indicator
    predSigs <- trading.signals(preds,b.t,s.t)
    tgtName <- all.vars(form)[1]
    trueSigs <- trading.signals(test[[tgtName]],b.t,s.t)
  } else {  # the model predicts the signals directly
    tgtName <- all.vars(form)[1]
    if (is.factor(preds))
      predSigs <- preds
    else {
      if (preds[1] %in% levels(train[[tgtName]]))
        predSigs <- factor(preds,labels=levels(train[[tgtName]]),
                           levels=levels(train[[tgtName]]))
      else 
        predSigs <- factor(preds,labels=levels(train[[tgtName]]),
                           levels=1:3)
    }
    trueSigs <- test[[tgtName]]
  }
  
  ## obtaining the trading record from trading with the signals
  date <- rownames(test)[1]
  market <- get(quotes)[paste(date,"/",sep='')][1:length(preds),]
  tradeRec <- trading.simulator(market,predSigs,
                                policy.func=policy,policy.pars=policy.pars,
                                trans.cost=trans.cost,init.cap=init.cap)
  
  return(list(trueSigs=trueSigs,predSigs=predSigs,tradeRec=tradeRec))
}
