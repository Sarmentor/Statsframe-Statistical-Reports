
#without scientific notation
#options(scipen=999)

#scientific notation
options(scipen=0)

options(digits=8)

##Confirmatory Analysis - Begin##

library(foreign)
library(sem)
library (psych)

cfa <- function(data,vars,pca){
  
  
  pca.coefs <- matrix(as.numeric(pca.varimax$loadings[1:length(pca.varimax$loadings)]),nrow=21,ncol=4,byrow=FALSE) 
  
  nfactor <- apply(pca.coefs,1,FUN=function(x){
    which.max(x)
  })
  
  crave <- data.frame(matrix(ncol=2,nrow=0),stringsAsFactors=FALSE)
  
  
  sapply(unique(nfactor), function(x){
    coeff  <- pca.coefs[which(x==nfactor),x]
    coeff2 <- pca.coefs[which(x==nfactor),x]^2
    e2 <- 1-coeff2
    cr <- sum(coeff)^2 / (sum(coeff)^2+sum(e2))
    ave <- sum(coeff2) / (sum(coeff2)+sum(e2))
    crave <<- rbind(crave,c(cr,ave))
  })
  
  names(crave) <- c("CR","AVE") 
  crave
  
  #matriz correlacional com AVE na diagonal
  library(psych)
  library(GPArotation)
  m.cor.ave <- fa(correlation, nfactors=4)$score.cor
  
  for (i in 1:4){
    m.cor.ave[i,i] <- sqrt(crave[i,"AVE"])
  }
  m.cor.ave
  
  
  ##
  dataCov <- cov(data.df)
  
  ## CFA with Lavaan package 
  library(lavaan)
  
  Q.model <-'FACTOR1 =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9
  FACTOR2 =~ Q10 + Q11 + Q12 + Q13 + Q14
  FACTOR3 =~ Q15 + Q16 + Q17 + Q18
  FACTOR4 =~ Q19 + Q20 + Q21'
  
  
  fit <- sem(Q.model, data = data.df)
  summary(fit, fit.measures=TRUE)
  
  #Every combination of variables (SEM package)
  cfa.model <- specifyModel(file="measurementsmodel.txt")
  cfaOut<-sem(cfa.model,S=dataCov,N=204)
  summary(cfaOut)
  
  #Our specified model
  #with lavaan 
  
  library(lavaan)
  
  Q.model.2 <-'FACTOR1 =~ Q1 + Q2 + Q3 + Q4 + Q6 + Q7 + Q8 + Q9
  FACTOR2 =~ Q12 + Q13 + Q14
  FACTOR3 =~ Q15 + Q16 + Q17 + Q18
  FACTOR4 =~ Q20 + Q21'
  
  
  fit <- sem(Q.model.2, data = data.df)
  summary(fit, fit.measures=TRUE)
  
  
  
  #with sem
  library(sem)
  cfa.model.2<-specifyModel("adjustedmodel.txt")
  cfaOut.2<-sem(cfa.model.2,S=dataCov,N=204)
  summary(cfaOut.2)
  
  #with lavaan
  library(lavaan)
  
  Q.model.3 <-'FACTOR1 =~ Q1 + Q2 + Q3 + Q4 + Q6 + Q7 + Q8 + Q9
FACTOR2 =~ Q12 + Q13 + Q14
FACTOR3 =~ Q15 + Q16 + Q17 + Q18
FACTOR4 =~ Q20 + Q21'
  
  cfaOut.3<-sem(Q.model.3,data=data.df)
  summary(cfaOut.3, fit.measures=TRUE)
  parameterEstimates(cfaOut.3, standardized=TRUE)
  
  library(dplyr) 
  library(tidyr)
  library(knitr)
  
  parameterEstimates(cfaOut.3, standardized=TRUE) %>% filter(op == "~~", lhs %in% c("FACTOR1", "FACTOR2", "FACTOR3","FACTOR4"), !is.na(pvalue)) %>% mutate(stars = ifelse(pvalue < .001, "***", ifelse(pvalue < .01, "**", ifelse(pvalue < .05, "*", "")))) %>% select('Factor 1'=lhs,'Factor 2'=rhs, Correlation=est,sig=stars) %>% kable(digits = 3, format="pandoc", caption="Table: Latent Factor Correlations")
  
}

go.conffactor(data,vars,pca){
  
  if(is.null(pca)){
    source("efa.R")
    pca <- go.expfactor(data,vars)
  }
  
  res.conffactor <- cfa(data,vars,pca)
  
  return(res.conffactor)
  
}

go.conffactor(data,vars,pca=NULL)

##Confirmatory Analysis - End##