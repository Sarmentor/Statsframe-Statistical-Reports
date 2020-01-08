##Linear Regression - BEGIN##

reg <- function(data,dep.vars,ind.var,predict.data){
  
  res.list <- list()
  
  newdata <- data
  form <- as.formula(eval(paste(paste(dep.vars," ~ "),paste(ind.var,collapse= " + "))))
  
  #browser()
  
  lm = lm(formula = form , data=newdata, na.rm = TRUE)
  tidy.lm <- tidy(lm)
  
  res.list$summ <- summary(lm,signif.stars=TRUE)
  
  res.list$coeffs <- coefficients(lm)
  
  res.list$lm <- tidy.lm
  
  res.list$pvalue <- pf(res.list$summ$fstatistic[1], res.list$summ$fstatistic[2], res.list$summ$fstatistic[3], lower=FALSE)
  
  if(is.null(predict.data)){
      
  }else{
    res.list$predone <- predict(lm, predict.data)
    #predict with confidence interval 95%
    res.list$predtwo <- predict(lm, predict.data, interval="confidence")
  }
  
  ###TODO MAKE LIST WITH RESULTS###
  return(res.list)
}

##Linear Regression - END##