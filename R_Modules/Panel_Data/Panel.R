#correlation
library(Hmisc)
library(lsdv)
library(plm)
library(Paneldata)
library(lavaan)

panel <- function(data, ind.var, dep.vars, predict.data, nint=2){
  
  newdata <- data
  
  head(newdata)
  
 
  
  correlation_matrix <- rcorr(as.matrix(newdata[,c(dep.vars)]))
  
  correlation_matrix
  
  ###########################################################################
  ############################ LSDV package #################################
  ###########################################################################
  
  fe.panel <- Lsdv(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, n=nrow(newdata), t=nint)
  summary(fe.panel)

  
  ###########################################################################
  ####################### Paneldata package #################################
  ###########################################################################
  
 
  
  fe.panel <- Paneldata(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, n=nrow(newdata), t=nint, model="fe")
  summary(fe.panel)

  
  re.panel <- Paneldata(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, n=nrow(newdata), t=nint, model="re")
  summary(re.panel)
  
  
  ###########################################################################
  ############################# plm package #################################
  ###########################################################################
  

  
  inst.fixed.lm = lm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel)
  summary(inst.fixed.lm)
  
  #ONE in nlogit
  CONST <- rep(as.numeric(inst.fixed.lm$coefficients["(Intercept)"]),nrow(Panel))
  CONST <- rep(1,nrow(Panel))
  
  #One-way fixed effects
  inst.fixed = plm(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, index = c("Institution"), model = "within")
  summary(inst.fixed)
  
  year.fixed = plm(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, index = c("Year"), model = "within")
  summary(year.fixed)
  
  #farm.fixed = plm(MILK ~ LABOR + LAND + COWS + FEED, data = Milk, index = c("FARM"), model = "within")
  #summary(farm.fixed)
  
  #Two-way error
  two.fixed = plm(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, index = c("Institution", "Year"),effect = "twoways", model = "within") 
  summary(two.fixed)
  
  #One-way random effects
  inst.random = plm(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, index = c("Institution"), model = "random")
  summary(inst.random)
  
  year.random = plm(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, index = c("Year"), model = "random")
  summary(year.random)
  
  #Two-way error with random effects
  two.random = plm(eval(paste(paste(ind.var," ~ "),paste(dep.vars,collapse= " + "))), data = newdata, index = c("Institution", "Year"),effect = "twoways", model = "random") 
  summary(two.random)
  
  
  
  #######statistical tests############
  
  #Chow F-test
  pFtest(inst.fixed, inst.fixed.lm )
  pFtest(year.fixed, inst.fixed.lm )
  pFtest(two.fixed, inst.fixed.lm )
  
  ##Lagrange Multiplier
  plmtest(inst.random, type=c("bp"))
  plmtest(year.random, type=c("bp"))
  plmtest(two.random, type=c("bp"))
  
  ##Hausman test
  phtest(inst.fixed, inst.random)
  phtest(year.fixed, year.random)
  phtest(two.fixed, two.random)
  
  ###################
  
  
  
  phtest(random, fixed)

  head(Demo.growth)
  
  model = "i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
  s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4"
  
  fit = growth(model, data = Demo.growth)
  summary(fit)
 
}

go.panel <- function(data, ind.var, dep.vars, predict.data, nint=2){
  
}



