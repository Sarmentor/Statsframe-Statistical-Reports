
#without scientific notation
#options(scipen=999)

#scientific notation
options(scipen=0)

options(digits=8)

##Factorial Analysis - BEGIN##

efa <- function(data,vars){
  
  res.efa <- list()
  
  res.efa$vars <- vars
  # Identification of the variables used in factor analysis
  newdata <- na.omit(data[,c(vars)])
  
  # Descriptive analysis for each variable
  summary(newdata)
  
  ### Correlation between variables in vars
  correlation <- cor(newdata, method="spearman")
  res.efa$correlation <- correlation
  
  ### Bartlett Sphericity test
  
  res.efa$bartlett <- cortest.bartlett(correlation, n=nrow(newdata))
  
  ### KMO Measure
  
  res.efa$KMO <- KMO(correlation)
  
  ### Kaiser criterion
  
  res.efa$eigen <- eigen(correlation)
  
  ### Scree plot criterion
  
  scree(correlation, hline=-1) # hline=-1 draw a horizontal line at -1
  
  ### Explained variance for each component
  pc <- prcomp(newdata,scale.=F, na.action="exclude")
  res.efa$summary <- summary(pc)
  
  ### Principal Component method
  my.nfactors.vss <- vss(newdata,n=ncol(newdata))
  my.nfactors.vec <- c(which.min(na.omit(my.nfactors.vss$map)),which.min(na.omit(my.nfactors.vss$vss.stats$BIC)),which.min(na.omit(my.nfactors.vss$vss.stats$SABIC)))

  #MODA
  # obtem a tabela com frequencia das variaveis (testes de número de Componentes, para map, BIC e SABIC a minimos)
  # obtem o nome da variavel
  # se empate então opta pela opção de mais componentes
  my.nfactors <- ifelse(length(aux.res <- as.numeric(names(sort(table(my.nfactors.vec),decreasing=TRUE))))==3,max(my.nfactors.vec,na.rm = TRUE),aux.res[1])
                             
  #PCA
  res.efa$pca <- principal(correlation,nfactors=my.nfactors, rotate="none")
  
  ### Principal Component method with varimax rotation
  res.efa$pca.varimax <- principal(correlation,nfactors=my.nfactors, rotate="varimax")
  
  ### Internal consistency PCs ###
  ################################
  #library (psych)
  
  #for(pc in 1:length(PCs)){
  #  alpha(newdata[,c(index.vars)])
  #}
  
  return(res.efa)
}

##Factorial Analysis - END##