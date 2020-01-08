

classifyfunc <- function(data,pred.vars,classvar){
  
  
  
  #j48 <- WOW("J48")
  #ibk <- WOW("IBk")
  
  #some algorithms do not support strings, only factors, remove NAs from data
  newdata <<- as.data.frame(na.omit(data[,c(pred.vars,classvar)]),stringsAsFactors =TRUE)
  names(newdata) <<- names(data[,c(pred.vars,classvar)])
  
  classvar <<- classvar
  pred.vars <<- pred.vars
  ## Identify a decision tree.
  
  #initialize storage variable df
  df <- data.frame(classifier = c(0), pctCorrect = c(0), pctIncorrect=c(0), pctUnclassified= c(0), kappa= c(0), meanAbsoluteError=c(0), rootMeanSquaredError=c(0), relativeAbsoluteError=c(0), rootRelativeSquaredError=c(0))#, var=c(0))
 
  classifier.names <<- c("AdaBoostM1","Bagging","DecisionStump","JRip","LMT","Logistic","OneR","PART","SMO","Stacking","LogitBoost","J48","IBk")
   
  c1 <<- AdaBoostM1(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))), data = newdata)
  c2 <<- Bagging(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))), data = newdata)
  c3 <<- DecisionStump(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c4 <<- JRip(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c5 <<- LMT(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c6 <<- Logistic(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  #c7 <<- LBR(eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c7 <<- OneR(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c8 <<- PART(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c9 <<- SMO(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c10 <<- Stacking(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c11 <<- LogitBoost(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  #c12 <<- M5P(eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  #c12 <<- M5Rules(eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  #c12 <<- MultiBoostAB(eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)
  c12 <<- J48(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)#,control = Weka_control(J = TRUE, M= var))
  c13 <<- IBk(formula = eval(paste(paste(classvar," ~ "),paste(pred.vars,collapse= " + "))),data = newdata)#,control = Weka_control(K= var))
 
  #browser()
  
  
  # Use 10 fold cross-validation.
  for(i in 1:13){
    name <- paste("e", i, sep = "")
    classifier <- paste("c", i, sep = "")
    assign(name,evaluate_Weka_classifier(get(classifier),
                                         numFolds = 10, complexity = TRUE,
                                         seed = 123, class = TRUE))
    
    nome <- paste("e",i,sep="")
    aux <- get(nome)
    res <- c()
    for(j in 1:8){
      res <- c(res,round(as.numeric(unlist(aux[2])[j]),3))
    }
    
    df <- rbind(df,c(paste(classifier.names[i]),as.numeric(res)))
  }
  
  df <- df[-1,]
  df[order(df[,2], decreasing=TRUE),]
  
  return(df)
  
}