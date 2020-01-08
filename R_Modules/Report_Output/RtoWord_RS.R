#defining the document name
if(grepl("windows",tolower(my.OS))){
  my.time <<- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  aux.file.path <<- rep.file.path
  
  dir.create(file.path(Sys.getenv("TEMP"), "data"), showWarnings = FALSE)
  aux.file.rdata <<- file.path(Sys.getenv("TEMP"), "data", paste(my.time,"-Statsframe.DATA",sep=""))
  my_doc <<- read_docx()
}else if(grepl("linux",tolower(my.OS))){
  my.time <<- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  aux.file.path <<- rep.file.path
  aux.file.report <<- file(aux.file.path, open="wt", blocking = FALSE, encoding ="UTF-8")
  
  dir.create("/tmp/data/", showWarnings = FALSE)
  aux.file.rdata <<- paste("/tmp/data/", my.time,"-Statsframe.DATA",sep="")
  my_doc <<- read_docx()
}else{
  my.time <<- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  aux.file.path <<- rep.file.path
  
  dir.create("/tmp/data/", showWarnings = FALSE)
  aux.file.rdata <<- paste("/tmp/data/", my.time,"-Statsframe.DATA",sep="")
  my_doc <<- read_docx()
}



histPlot <- function(ip,vars.name.pass){
  src0 <- tempfile(fileext = ".png")
  png(filename = src0, width = 5, height = 6, units = 'in', res = 300)
  my.breaks <- quantile(ip,seq(0,1,by=0.1),na.rm = TRUE)
  labels = 1:(length(my.breaks)-1)
  # hist(ip,breaks = "Sturges", probability=TRUE ,xlim = extendrange(my.breaks,r=range(my.breaks,na.rm=TRUE),f=0.05), ylim=extendrange(ip,r=range(ip,na.rm=TRUE),f=0.05),
  #      col=c("green","darkgreen","blue","darkblue","purple","pink"),
  #      main=paste("Histogram of",vars.name.pass,"variable",sep=" "))
  hist(na.omit(ip),breaks = "Sturges", probability=FALSE ,
       col=c("green","darkgreen","blue","darkblue","purple","pink"),
       main=paste("Histogram of",vars.name.pass,"variable",sep=" "),xlab = paste(vars.name.pass))
  dev.off()
  src0 <<- src0
}

barPlot <- function(ip,vars.name.pass){
  src1 <- tempfile(fileext = ".png")
  png(filename = src1, width = 5, height = 6, units = 'in', res = 300)
  barplot(table(na.omit(ip)),col=1:10, cex.names = 0.70,cex.axis = 0.70,
          main=paste("Bar Plot of",vars.name.pass, "Variable\n (with sample sizes)",sep=" "))
  dev.off()
  src1 <<- src1
}

boxPlot <- function(ip,vars.name.pass){
  src2 <- tempfile(fileext = ".png")
  png(filename = src2, width = 5, height = 6, units = 'in', res = 300)
  boxplot(ip)
  title(paste("Box plot of",vars.name.pass,"variable",sep = " "))
  dev.off()
  src2 <<- src2
}

scatterPlot <- function(ip,vars.name.pass){
  src3 <- tempfile(fileext = ".png")
  png(filename = src3, width = 5, height = 6, units = 'in', res = 300)
  scatter.smooth(ip)
  dev.off()
  src3 <<- src3
}

piePlot <- function(ip,vars.name.pass){
  src4 <- tempfile(fileext = ".png")
  png(filename = src4, width = 5, height = 6, units = 'in', res = 300)
  lbls <- paste(names(table(ip)), "\n", table(ip), sep="")
  pie3D(table(ip), labels = lbls,explode=0.1,labelcex=0.8,
        main=paste("Pie Chart of",vars.name.pass, "Variable\n (with sample sizes)",sep=" "))
  dev.off()
  src4 <<- src4
}


writing <- function(what="descriptive",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  
  
  
  if(what=="descriptive"){ 
    
    if(analysis=="header1"){
      
      val.header1 <<-  paste("Descriptive Analysis for All Variables:", sep = "")
      body_add_par(my_doc, val.header1, style = "heading 1",pos = "after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="header2"){
      
      val.header2 <<-  paste('Variable "', vars.name.pass, '" - This is the Descriptive Analysis:', sep = "")
      body_add_par(my_doc, val.header2, style = "heading 2",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="summ"){
      
      val.summ <- summary(na.omit(data[,vars.name.pass]))
      
      my.min <- as.numeric(val.summ["Min."])
      my.max <- as.numeric(val.summ["Max."])
      my.mean <- as.numeric(val.summ["Mean"])
      my.median <- as.numeric(val.summ["Median"])
      my.1stQu <- as.numeric(val.summ["1st Qu."])
      my.3rdQu <- as.numeric(val.summ["3rd Qu."])
      my.sd <- sd(na.omit(data[,vars.name.pass]))
      
      #output text with values for variable summary
      val.summ.text <<- paste('"', vars.name.pass, '" variable varies between ',my.min ,' and ',my.max,'. The mean is ',round(my.mean,3),'. The standard deviation is ',round(my.sd,3),', that is, on average, the "',vars.name.pass,'" varies about ',round(my.sd,3),' of the mean.',' The first and third quartiles are ',my.1stQu,' and ',my.3rdQu,' respectively. This means that 50% (half) of elements of the sample have "', vars.name.pass,'" between ', my.1stQu,' and ',my.3rdQu,'.', sep="")
      body_add_par(my_doc, val.summ.text, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="freqs"){
      data[,vars.name.pass] <- as.factor(data[,vars.name.pass])
      val.freqs <- as.numeric(freq <- table(na.omit(data[,vars.name.pass])))
      
      #browser()
      #TODO
      my.Abs.Freq <- val.freqs 
      my.Rel.Freq <- as.numeric(prop.table(table(na.omit(data[,vars.name.pass])))[vars.name.pass])
      my.CumAbs.Freq <- cumsum(val.freqs) 
      my.CumRel.Freq <- cumsum(my.Rel.Freq)
      
      ncats <- length(unique(data[,vars.name.pass]))
      
      if(ncats > 10){
        local.info.text <- paste('NOTE: Variable "',vars.name.pass,'"',' has more than 10 categories. Thus, it should be advised to divide this variable data in intervals or groups.\n','NONETHELESS, this software will continue with the analysis, at your own risk.', sep="")
        #TODO - call shiny dialog windows with message, with only a OK button, no close possibility.
      }
      
      val.freqs.text <- paste('Categorical variables are qualitative variables and cannot be represented by a number. Categorical variables could be nominal and ordinal categorical variables. The difference is that, regarding their presentation, while nominal variables may be presented randomly or in the preferred order of the analyst, the ordinal variables must be presented in the order that is more easily understood (lowest to high, for example). In case of categorical variables, the analysis that can be done is the frequency of each category.')
      body_add_par(my_doc, val.freqs.text, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      for(cat in 1:ncats){
        
        #text with substitution of values for each category
        val.freqs.text <- paste('Analyses show that there are ',val.freqs[cat],' counts of category "', levels(data[,vars.name.pass])[cat] ,'", corresponding to ',round(val.freqs[cat]/sum(val.freqs),3)*100,'%. In total, there are ', length(na.omit(data[,paste(vars.name.pass)])) ,' elements in the study.',sep="")
        body_add_par(my_doc, val.freqs.text, style = "Normal",pos="after") 
        body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
        
      }
      
      # obtem a tabela com frequencia das variaveis
      # obtem o nome da variavel
      name = names(freq)[freq == max(freq)]
      
      #se a contagem for igual para a variavel não faz
      if(length(unique(table(data[,vars.name.pass])))>1){
        
        val.freqs.text <- paste('The mode (most common element) of this variable is ', name,', with ',max(freq),' counts.',sep="")
        body_add_par(my_doc, val.freqs.text, style = "Normal",pos="after") 
        body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
        
      }
      
   }
    
    if(analysis == "na"){
      trigger <- is.na(data[,vars.name.pass])
      
      #if(any(trigger)){
      val <- length(which(trigger==TRUE))
      #}
      
      #output text with variable information about NA count
      val.na.text <<-  paste(
        "Results show that there are ", val,' missing values in variable "',vars.name.pass,'"',sep="")
      body_add_par(my_doc, val.na.text, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="mean"){
      val.mean <<-  paste(vars.name.pass, " mean is:", mean(data[,vars.name.pass]),sep="")
      body_add_par(my_doc, val.mean, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="sd"){
      
      val.sd <<-  paste(vars.name.pass, " sd (standard deviation) is:", sd(data[,vars.name.pass]))
      body_add_par(my_doc, val.sd, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="median"){
      
      val.median <<-  paste(vars.name.pass, " median is:", median(data[,vars.name.pass]),sep="")
      body_add_par(my_doc, val.median, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
  }
  
  if(what=="scatter"){
    val.scatter <<- scatterPlot(data[,paste(vars.name.pass)],vars.name.pass)
    body_add_par(my_doc, "Scatter Plot is: ", style = "Normal",pos="after") # blank paragraph
    body_add_img(my_doc, src = val.scatter, width = 5, height = 6, style = "centered",pos="after")
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
  }
  
  if(what=="bar"){
    val.bar <<- barPlot(data[,paste(vars.name.pass)],vars.name.pass)
    body_add_par(my_doc, "Bar Plot is: ", style = "Normal",pos="after") # blank paragraph
    
    #to avoid dense bar plots
    if(length(unique(data[,paste(vars.name.pass)]))<=30){
      body_add_img(my_doc, src = val.bar, width = 5, height = 6, style = "centered",pos="after")
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }else{
      body_add_par(my_doc, "Analysis not possible due to data constraints! (unique values > 30)", style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
    
  
  }
  
  if(what=="box"){
    val.box <<- boxPlot(data[,paste(vars.name.pass)],vars.name.pass)
    body_add_par(my_doc, "Box Plot is: ", style = "Normal",pos="after") # blank paragraph
    body_add_img(my_doc, src = val.box, width = 5, height = 6, style = "centered",pos="after")
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
  }
  
  if(what=="pie"){
    val.pie <<- piePlot(data[,paste(vars.name.pass)],vars.name.pass)
    body_add_par(my_doc, "Pie Plot is: ", style = "Normal",pos="after") # blank paragraph
   
     #to avoid dense pie charts
    if(length(unique(data[,paste(vars.name.pass)]))<=30){
      body_add_img(my_doc, src = val.pie, width = 5, height = 6, style = "centered",pos="after")
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }else{
      body_add_par(my_doc, "Analysis not possible due to data constraints! (unique values > 30)", style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
  }
  
  if(what=="hist"){
    if(class(data[,paste(vars.name.pass)])=="Date"){
      return({0})
    }else{
      val.hist <<- histPlot(data[,paste(vars.name.pass)],vars.name.pass)
      body_add_par(my_doc, "Histogram Plot is: ", style = "Normal",pos="after") # blank paragraph
      body_add_img(my_doc, src = val.hist, width = 5, height = 6, style = "centered",pos="after")
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
  }
  
  if(what=="table"){
    
    if(analysis=="head"){
      #browser()
      
      new.column <- data.frame("..." = rep("...",nrow(data)), stringsAsFactors = TRUE)
      new.table <<- as.data.frame(head(cbind(data[,1:ifelse(ncol(data<5),2,3)],new.column,data[,(ncol(data)-ifelse(ncol(data<5),1,3)):ncol(data)])))
      
      val.head <- flextable(new.table) %>% theme_booktabs() %>% fontsize() %>% autofit()
      body_add_par(my_doc, "A short example of your data to analyze: ", style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      body_add_flextable(my_doc, val.head, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
    if(analysis=="freqs"){
      val.freqs <<- flextable(Freq(data[,vars.name.pass]))
      body_add_par(my_doc, "The table for frequencies is the following: ", style = "Normal",pos="after") # blank paragraph
      body_add_flextable(my_doc, autofit(fontsize(val.freqs, size = 6, part = "all")), pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
  }
}

inf.writing <- function(what="inference",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  if(what=="inference"){
    
    if(analysis=="header1"){
      
      val.header1 <<-  paste("Inference Analysis for All Variables Combination in Dataset:", sep = "")
      body_add_par(my_doc, val.header1, style = "heading 1",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <<- paste("An assessment of the normality of data is a prerequisite for many statistical tests because normal data is an underlying assumption in parametric tests. The variables used to test the normality need to be numerical. Additionally, to numerically test the normality, the length of the sample should be taken into account. If the length is smaller than 50 records we use “Shapiro-Wilk test” or else we use “Kolmogorov-Smirnov test”.",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <<- paste("Thus, normality test was done for the numerical variables. The results of the tests used -- Shapiro-Wilk or Kolmogorov-Smirnov test - allows us concluding that, with a 95% of confidence, the null hypothesis is rejected if p<0.05/non-rejected if p>0.05, i.e., there is evidence/there is no evidence to reject the null hypothesis and it may not be/ be considered the existence of normality, respectively.",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <<- paste("In this sense, the numeric variable has not/has normal distribution and, consequently, non-parametric/parametric tests should be used with that variable.",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <<- paste("To compare two or more categorical variables, a cross-tabulation (also called the contingency table) is the most adequate option. However, to analyze the statistical differences, the chi-squared test or fisher test, for independence, should be applied to the crosstab.",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <<- paste("To analyze two numerical variables, correlations analysis is applied. If variables have/have not normal distribution, Pearson’s / Spearman’s correlation is more appropriate, respectively.",sep="") 
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    } 
    
    if(analysis=="header2"){
      
      val.header2 <<-  paste('Inference Analysis between Variable "', vars.name.pass[1], '" and "',vars.name.pass[2],'":', sep = "")
      body_add_par(my_doc, val.header2, style = "heading 2",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="res"){
      
      var1type <- vars.name.pass[1]
      var2type <- vars.name.pass[2]
      
      
      if(class(vars.val.pass[,var1type])=="factor" && class(vars.val.pass[,var2type])=="factor"){
        if(length(unique(vars.val.pass[,var1type]))==1 || length(unique(vars.val.pass[,var2type]))==1){
          warning(Sys.time()," - var1:",paste(var1type)," - var2:",paste(var2type)," - [ERROR][INF] Error in chisq.test: 'x' must at least have 2 elements\n\n")
          return({0})
        }else{
          
          crosstbl <- table(vars.val.pass[,var1type],vars.val.pass[,var2type],dnn=c(var1type,var2type))
          crosstbl_matrix <-as.matrix(crosstbl)
          count_1<-which(crosstbl_matrix<5)
          count_2<-which(crosstbl_matrix>=5)
          ratio1 <- length(count_1)/(nrow(crosstbl_matrix)*ncol(crosstbl_matrix))
          ratio2 <-length(count_2)/(nrow(crosstbl_matrix)*ncol(crosstbl_matrix))
          #browser()
          
          #ATTENTION: Crosstable can get big!!
          if(nrow(crosstbl_matrix)*ncol(crosstbl_matrix)<100){
            val.test.inf <- flextable(as.data.frame.table(crosstbl_matrix))
            body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
          }else{
            body_add_par(my_doc, paste("NOTE: TEST Crosstable of high dimension. Therefore, not available in this report!"),style = "Normal",pos="after") 
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
          }
          
          
          #if there are zeros and 80% or less of the values are less than 5
          #do Fisher exact test
          if(any(crosstbl == 0 )){
            if(ratio1>0.20 && ratio1 < 0.80){
              
              
              val.test.inf <- flextable(tidy(my.fisher <- fisher.test(x = crosstbl, alternative = "two.sided",simulate.p.value = TRUE)))
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("The results of the analysis shows that Fisher test presents a p-value of ",round(my.fisher$p.value,3)," ",ifelse(my.fisher$p.value<0.05,paste("(p<0.05)."),paste("(p>0.05)."))," Thus, the null hipothesis is ",ifelse(my.fisher$p.value<0.05,paste("rejected"),paste("non-rejected"))," with a confidence level of 95%. We can conclude that both variables are ",ifelse(my.fisher$p.value<0.05,paste("dependent."),paste("independent.")),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              
              body_add_par(my_doc, paste("NOTE: TEST not robust due to high percentage of values inferior to 5 in crosstable: ",ratio1*100,"%\n\n",sep=""), style = "Normal",pos="after") 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }else if(ratio1 < 0.20){
              val.test.inf <- flextable(tidy(my.fisher <- fisher.test(x = crosstbl, alternative = "two.sided",simulate.p.value = TRUE)))
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("The results of the analysis shows that Fisher test presents a p-value of ",round(my.fisher$p.value,3)," ",ifelse(my.fisher$p.value<0.05,paste("(p<0.05)."),paste("(p>0.05)."))," Thus, the null hipothesis is ",ifelse(my.fisher$p.value<0.05,paste("rejected"),paste("non-rejected"))," with a confidence level of 95%. We can conclude that both variables are ",ifelse(my.fisher$p.value<0.05,paste("dependent."),paste("independent.")),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }else{
              return({0})
            } 
          }else{
            if(ratio1>0.20 && ratio1 < 0.80){
              val.test.inf <- flextable(tidy(my.chisq <- chisq.test(x = crosstbl)))####what happens if there are values in the crosstbl equal to 5
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("The results of the analysis shows that chi-squared test presents a p-value of ",round(my.chisq$p.value,3)," ",ifelse(my.chisq$p.value<0.05,paste("(p<0.05)."),paste("(p>0.05)."))," Thus, the null hipothesis is ",ifelse(my.chisq$p.value<0.05,paste("rejected"),paste("non-rejected"))," with a confidence level of 95%. We can conclude that both variables are ",ifelse(my.chisq$p.value<0.05,paste("dependent."),paste("independent.")),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              body_add_par(my_doc, paste("NOTE: TEST not robust due to high percentage of values inferior to 5 in crosstable: ",ratio1*100,"%\n\n",sep=""), style = "Normal",pos="after") 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }else if(ratio1 < 0.20){
              val.test.inf <- flextable(tidy(my.chisq <- chisq.test(x = crosstbl)))####what happens if there are values in the crosstbl equal to 5
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("The results of the analysis shows that chi-squared test presents a p-value of ",round(my.chisq$p.value,3)," ",ifelse(my.chisq$p.value<0.05,paste("(p<0.05)."),paste("(p>0.05)."))," Thus, the null hipothesis is ",ifelse(my.chisq$p.value<0.05,paste("rejected"),paste("non-rejected"))," with a confidence level of 95%. We can conclude that both variables are ",ifelse(my.chisq$p.value<0.05,paste("dependent."),paste("independent.")),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }else{
              return({0})
            } 
          }
        }
      }else if(class(vars.val.pass[,var1type])== "factor" && (class(vars.val.pass[,var2type]) == "numeric"|| class(vars.val.pass[,var2type]) == "integer" || class(vars.val.pass[,var2type]) == "double")){
        
        #check normality for quantitative variable
        #with Kolmogorov-Smirnov (lilliefors)
        if(length(unique(na.omit(vars.val.pass[,var2type])))<2 || length(na.omit(vars.val.pass[,var2type]))<5){
          return({0})
        }else{
          p.value <-lillie.test(na.omit(vars.val.pass[,var2type]))$p.value###The K-S test is for a continuous distribution and so MYDATA
        }
        
        aux.data <- vars.val.pass[complete.cases(vars.val.pass[,c(var1type,var2type)]),c(var1type,var2type)]
        
        
         
        #browser()
        #should not contain any ties (repeated values). 
        #https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r
        if(p.value < 0.05){#not Normal
          
          
          if(length(unique(aux.data[,var2type]))==1){
            warning(Sys.time()," - var2:",paste(var2type)," - [ERROR][INF] Mann-Whitney-Wilcoxon or Kruskal cannot be applied to numeric vectors with only 1 unique value\n\n")
            return({0})
          }
          if(length(unique(aux.data[,var1type]))==2){###What do you mean with unique(var1)==2?
            
            #browser()
            
            #Mann-Whitney
            test <-  tidy(my.wilcox <- wilcox.test(aux.data[,var2type]~aux.data[,var1type]))  #wilcox.test(y~A) where y is numeric and A is A binary factor (independent)
            test$data.name<-paste(var2type,"by",var1type,sep = " ")
            val.test.inf <- flextable(test)
            body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
            aux.text.inf <- paste('Analyses show that p-value is ',round(my.wilcox$p.value,3),ifelse(round(my.wilcox$p.value,3)<0.05,paste(' (p<0.05). This means that null hypothesis is rejected (p<0.05) and, therefore, there are statistical differences. Thus, it is possible to assume that groups of "',var2type,'" are very different regarding "',var1type,'" variable.',sep=""),paste(' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, the distributions are similar. Thus, it is possible to assume that groups of "',var2type,'" are similar regarding "',var1type,'" variable.',sep="")),sep="") 
            body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
          }else if(length(unique(aux.data[,var1type]))>2){
            
            #browser()
            
            #Kruskal-Wallis
            if(length(unique(aux.data[,var1type]))==1 || length(unique(aux.data[,var2type]))<=2){
              warning(Sys.time()," - var1:",paste(var1type)," - [ERROR][INF] Error in kruskal.test.default: all observations are in the same group\n\n")
              return({0})
            }else{
              test <- tidy(my.kruskal <- kruskal.test(x=aux.data[,var2type],g=aux.data[,var1type]))#https://www.statmethods.net/stats/nonparametric.html #kruskal.test(y~A) # where y1 is numeric and A is a factor
              test$data.name<-paste(var2type,"by",var1type,sep = " ")
              val.test.inf <- flextable(test)
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste('Analyses show that p-value is ',round(my.kruskal$p.value,3),ifelse(round(my.kruskal$p.value,3)<0.05,paste(' (p<0.05). This means that null hypothesis is rejected (p<0.05) and, therefore, there are statistical differences. Thus, it is possible to assume that groups are very different regarding "',var1type,'" variable.',sep=""),paste(' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, there are no statistical differences. Thus, it is possible to assume that groups have similar distribution regarding "',var1type,'" variable.',sep="")),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }
          }else{
            return({0})
          }
        }else{#normal
          vars.val.pass <- na.omit(vars.val.pass)
          
          if(length(unique(vars.val.pass[,var1type]))==2){###What do you mean with unique(var1)==2?
            #ttest
            tryCatch(stop(val.test.inf <<- flextable(tidy(my.t.test <- t.test(vars.val.pass[,var2type]~vars.val.pass[,var1type], var.equal=TRUE)))), error = function(e) {warning(Sys.time()," - var1:",paste(var1type)," - var2:",paste(var2type)," - [ERROR][INF] ",e,"\n\n")}, finally = {
              # t.test(y~x) where y is numeric and x is a binary factor
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste('Analyses show that p-value is ',round(my.t.test$p.value,3),' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, there are no statistical differences. Thus, it is possible to assume that groups of "',var1type,'" have the same distribution regarding variable "',var2type,'".',sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            })
            
          }else{##What do you mean with unique(var1)>2?
            #Anova
            if(length(unique(vars.val.pass[,var1type]))<2){
              warning(Sys.time()," - var1:",paste(var1type)," - [ERROR][INF] Error in contrasts<-: contrasts can be applied only to factors with 2 or more levels\n\n")
              return({0})
            }else{
              test<-tidy(my.aov <- aov(vars.val.pass[,var2type] ~ vars.val.pass[,var1type]))     #"Anova"
              my.aov$p.value <- summary.aov(my.aov)[[1]][["Pr(>F)"]][1]
              
              if(is.null(my.aov$p.value)){
                return({0})
              }
              
              test$call <- paste("aov(formula = ",var2type," ~ ",var1type,")",sep = "")
              val.test.inf <- flextable(test)
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste('Analyses show that p-value is ',round(my.aov$p.value,3),ifelse(round(my.aov$p.value,3)>=0.05,paste(' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, there are no statistical differences. Thus, it is possible to assume that groups have the same distribution regarding "',var2type,'" variable.',sep=""),paste(' (p<0.05). This means that null hypothesis is rejected (p<0.05) and, therefore, there are statistical differences. Thus, it is possible to assume that groups do not have the same distribution regarding "',var2type,'" variable.',sep="")),sep = "") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }
          }
        }
      }else if(
        
        ##################################################################################################################
        class(vars.val.pass[,var2type])== "factor" && (class(vars.val.pass[,var1type]) == "numeric"|| class(vars.val.pass[,var1type]) == "integer" || class(vars.val.pass[,var1type]) == "double")){
        
        #check normality for quantitative variable
        #with Kolmogorov-Smirnov (lilliefors)
        if(length(unique(na.omit(vars.val.pass[,var1type])))<2 || length(na.omit(vars.val.pass[,var1type]))<5){
          return({0})
        }else{
          p.value <-lillie.test(na.omit(vars.val.pass[,var1type]))$p.value###The K-S test is for a continuous distribution and so MYDATA
        }
        
        aux.data <- vars.val.pass[complete.cases(vars.val.pass[,c(var1type,var2type)]),c(var1type,var2type)]
        
        
        #should not contain any ties (repeated values). 
        #https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r
        if(p.value < 0.05){#not Normal
          
         
          
          if(length(unique(aux.data[,var2type]))==2){###What do you mean with unique(var1)==2?
            
            #browser()
            #Mann-Whitney
            test <-  tidy(my.wilcox <- wilcox.test(aux.data[,var1type]~aux.data[,var2type]))  #wilcox.test(y~A) where y is numeric and A is A binary factor (independent)
            test$data.name<-paste(var1type,"by",var2type,sep = " ")
            val.test.inf <- flextable(test)
            body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
            aux.text.inf <- paste('Analyses show that p-value is ',round(my.wilcox$p.value,3),ifelse(round(my.wilcox$p.value,3)<0.05,paste(' (p<0.05). This means that null hypothesis is rejected (p<0.05) and, therefore, there are statistical differences. Thus, it is possible to assume that groups of "',var2type,'" are very different regarding "',var1type,'" variable.',sep=""),paste(' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, the distributions are similar. Thus, it is possible to assume that groups of "',var2type,'" are similar regarding "',var1type,'" variable.',sep="")),sep="") 
            body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
          }else if(length(unique(aux.data[,var2type]))>2){
            #Kruskal-Wallis
            if(length(unique(aux.data[,var2type]))==1 || length(unique(aux.data[,var1type]))==1){
              warning(Sys.time()," - var2:",paste(var2type)," - [ERROR][INF] Error in kruskal.test.default: all observations are in the same group\n\n")
              return({0})
            }else{
              test <- tidy(my.kruskal <- kruskal.test(aux.data[,var1type]~aux.data[,var2type]))#https://www.statmethods.net/stats/nonparametric.html #kruskal.test(y~A) # where y1 is numeric and A is a factor
              test$data.name<-paste(var1type,"by",var2type,sep = " ")
              val.test.inf <- flextable(test)
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste('Analyses show that p-value is ',round(my.kruskal$p.value,3),ifelse(round(my.kruskal$p.value,3)<0.05,paste(' (p<0.05). This means that null hypothesis is rejected (p<0.05) and, therefore, there are statistical differences. Thus, it is possible to assume that groups are very different regarding "',var1type,'" variable.',sep=""),paste(' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, there are no statistical differences. Thus, it is possible to assume that groups have similar distribution regarding "',var1type,'" variable.',sep="")),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }
          }else{
            return({0})
          }
        }else{#normal
          #take out NAs
          vars.val.pass <- na.omit(vars.val.pass)
          if(length(unique(vars.val.pass[,var2type]))==2){###What do you mean with unique(var1)==2?
            #ttest
            tryCatch(stop(val.test.inf <<- flextable(tidy(my.t.test <- t.test(vars.val.pass[,var1type]~vars.val.pass[,var2type], var.equal=TRUE)))), error = function(e) {warning(Sys.time()," - var1:",paste(var1type)," - var2:",paste(var2type)," - [ERROR][INF] ",e,"\n\n")}, finally = {
              # t.test(y~x) where y is numeric and x is a binary factor
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste('Analyses show that p-value is ',round(my.t.test$p.value,3),' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, there are no statistical differences. Thus, it is possible to assume that groups of "',var1type,'" have the same distribution regarding variable "',var2type,'".',sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            })
          }else if(length(unique(vars.val.pass[,var2type]))>2){##What do you mean with unique(var1)>2?
            #Anova
            if(length(unique(vars.val.pass[,var2type]))==1){
              warning(Sys.time()," - ",paste(var2type)," - [ERROR][INF] Error in contrasts<-: contrasts can be applied only to factors with 2 or more levels\n\n")
              return({0})
            }else{
              
              test<-tidy(my.aov <- aov(vars.val.pass[,var1type] ~ vars.val.pass[,var2type]))     #"Anova"
              my.aov$p.value <- summary.aov(my.aov)[[1]][["Pr(>F)"]][1]
               
              if(is.null(my.aov$p.value)){
                 return({0})
               }
              
              test$call <- paste("aov(formula = ",var1type," ~ ",var2type,")",sep = "")
              val.test.inf <- flextable(test)
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste('Analyses show that p-value is ',round(my.aov$p.value,3),ifelse(round(my.aov$p.value,3)>=0.05,paste(' (p>0.05). This means that null hypothesis is non-rejected (p>0.05) and, therefore, there are no statistical differences. Thus, it is possible to assume that groups have the same distribution regarding "',var2type,'" variable.',sep=""),paste(' (p<0.05). This means that null hypothesis is rejected (p<0.05) and, therefore, there are statistical differences. Thus, it is possible to assume that groups do not have the same distribution regarding "',var2type,'" variable.',sep="")),sep = "") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }  
          }else{
            return({0})
          }
        }
      }else{
        
        #################################################################################################################
        
        if((class(vars.val.pass[,var1type]) == "numeric"|| class(vars.val.pass[,var1type]) == "integer" || class(vars.val.pass[,var1type]) == "double") &&
           (class(vars.val.pass[,var2type]) == "numeric"|| class(vars.val.pass[,var2type]) == "integer" || class(vars.val.pass[,var2type]) == "double")){
          
          vars.val.pass <- vars.val.pass[complete.cases(vars.val.pass[,c(var1type,var2type)]),c(var1type,var2type)]
          
          #check normality for quantitative variable
          #with Kolmogorov-Smirnov (lilliefors)
          if(length(unique(na.omit(vars.val.pass[,var1type])))<2 || length(na.omit(vars.val.pass[,var1type]))<5){
            return({0})
          }else{
            p.value.v1 <-lillie.test(na.omit(vars.val.pass[,var1type]))$p.value###The K-S test is for a continuous distribution and so MYDATA
          }
          #check normality for quantitative variable
          #with Kolmogorov-Smirnov (lilliefors)
          if(length(unique(na.omit(vars.val.pass[,var2type])))<2 || length(na.omit(vars.val.pass[,var2type]))<5){
            return({0})
          }else{
            p.value.v2 <-lillie.test(na.omit(vars.val.pass[,var2type]))$p.value###The K-S test is for a continuous distribution and so MYDATA
          }
          
         
          #check if the variables are continuous or discrete
          if((class(vars.val.pass[,var1type]) == "numeric"|| class(vars.val.pass[,var1type]) == "double") &&
             (class(vars.val.pass[,var2type]) == "numeric"|| class(vars.val.pass[,var2type]) == "double")){
            
            
            if(p.value.v1>0.05 && p.value.v2>0.05){
              
              #Pearson Correlation
              test <- tidy(my.pearson <- cor.test(vars.val.pass[,var1type],vars.val.pass[,var2type],method = "pearson"))
              test$call <- paste(var1type,"and",var2type,")",sep = " ")  
              val.test.inf <- flextable(test)
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("To analyze two numerical variables, correlations analysis is applied. Since ",var1type ,'" and "',var2type,'" have normal distribution, Pearson correlation is more appropriate.',sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("Pearson correlation presents a p-value of ",round(my.pearson$p.value,3),". This means ",ifelse(my.pearson$p.value<0.05,"a significant correlation between both variables exists."," that the independence of both variables exists."),". The value of the correlation is ",round(my.pearson$estimate,3),". Since the correlation result is ",ifelse(my.pearson$estimate>=0,"positive","negative"),", both variables are ",ifelse(my.pearson$estimate>=0,"positively","negatively")," correlated, i.e., when ",var1type," increases, ",var2type," ",ifelse(my.pearson$estimate>=0,"increases.","decreases."),sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }else{
              #Spearman Correlation
              test <- tidy(my.Spearman <- cor.test(vars.val.pass[,var1type],vars.val.pass[,var2type],method = "spearman"))
              test$call <- paste(var1type,"and",var2type,sep = " ")  
              val.test.inf <- flextable(test)
              body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("To analyze two numerical variables, correlations analysis is applied. Since ",'"',var1type ,'" and/or "',var2type,'" do not have normal distribution, Spearman correlation is more appropriate.',sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
              aux.text.inf <- paste("Spearman correlation presents a p-value of ",round(my.Spearman$p.value,3),". This means that ",ifelse(my.Spearman$p.value<0.05,"a significant correlation between both variables exists.","the variables are independent.")," The value of the correlation is ",round(my.Spearman$estimate,3),". Since the correlation result is ",ifelse(my.Spearman$estimate>=0,"positive","negative"),", both variables are ",ifelse(my.Spearman$estimate>=0,"positively","negatively")," correlated, i.e., when ",var1type," increases, ",var2type," ",ifelse(my.Spearman$estimate>=0,"increases","decreases"),".",sep="") 
              body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
              body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
              
            }
            
          }else if(class(vars.val.pass[,var1type]) == "integer" || class(vars.val.pass[,var2type]) == "integer"){
            
            #Spearman Correlation (one integer variable)
            test <- tidy(my.Spearman <- cor.test(vars.val.pass[,var1type],vars.val.pass[,var2type],method = "spearman"))
            test$call <- paste(var1type,"and",var2type,sep = " ")  
            val.test.inf <- flextable(test)
            body_add_flextable(my_doc, autofit(fontsize(val.test.inf, size = 6, part = "all")), pos="after",split = TRUE) 
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
            aux.text.inf <- paste('To analyze two numerical variables (in this case, one of them is an integer), correlations analysis is applied. Since we have an integer variable, Spearman correlation is more appropriate.',sep="") 
            body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
            aux.text.inf <- paste("Spearman correlation presents a p-value of ",round(my.Spearman$p.value,3),". This means that ",ifelse(my.Spearman$p.value<0.05,"a significant correlation between both variables exists."," the independence of both variables exists.")," The value of the correlation is ",round(my.Spearman$estimate,3),". Since the correlation result is ",ifelse(my.Spearman$estimate>=0,"positive","negative"),", both variables are ",ifelse(my.Spearman$estimate>=0,"positively","negatively")," correlated, i.e., when ",var1type," increases, ",var2type," ",ifelse(my.Spearman$estimate>=0,"increases","decreases"),".",sep="") 
            body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
            body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
            
          }else{
            return({0})
          }
        }
      }
      
      
      
    }
  }
}


efa.writing <- function(what="factor",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  if(what=="factor"){
    
    if(analysis=="header1"){
      
      val.header1.efa <<-  paste('Exploratory Factor Analysis:', sep = "")
      body_add_par(my_doc, val.header1.efa, style = "heading 1",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="header2"){
      
      val.header2.efa <<-  paste('', sep = "")
      body_add_par(my_doc, val.header2.efa, style = "heading 2",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="res"){
      val.efa <<- reg.efa
      
      aux.text.inf <- paste("Factor analysis is a statistical method used to describe variability among observed, correlated variables. The goal of performing factor analysis is to search for some unobserved variables called factors.") 
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      my.max <- which(val.efa$correlation == max(val.efa$correlation),arr.ind = TRUE)
      row.max <- my.max[1]
      col.max <- my.max[2]
      
      my.min <- which(val.efa$correlation == min(val.efa$correlation),arr.ind = TRUE)
      row.min <- my.min[1]
      col.min <- my.min[2]
      
      aux.text.inf <- paste("The correlation values (between different variables in study) varies from a minimum ",round(min(val.efa$correlation),3)," to ",round(max(val.efa$correlation[which(val.efa$correlation<1)]),3),". These differences suggests the variables could be reduced down to at least two underlying variables or factors.",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("Sampling Adequacy",sep="")
      body_add_par(my_doc, aux.text.inf, style = "heading 2",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("Exploratory factor analysis is only useful if the matrix of population correlation is statistically different from the identity matrix. If these are equal, the variables are few interrelated, i.e., the specific factors explain the greater proportion of the variance and the common factors are unimportant. Therefore, it should be defined when the correlations between the original variables are sufficiently high. Thus, the factor analysis is useful in estimation of common factors. With this in mind, the Bartlett Sphericity test can be used. The hypotheses are:\n\n",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("H0: the matrix of population correlations is equal to the identity matrix",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("H1: the matrix of population correlations is different from the identity matrix.",sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
      val.efa$my.bartlett <- data.frame(Chi_Squared = val.efa$bartlett$chisq, pvalue=val.efa$bartlett$p.value, df=val.efa$bartlett$df)
      
      #browser()
      
      ft <- flextable(val.efa$my.bartlett,col_keys = names(val.efa$my.bartlett)) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_par(my_doc, "Bartlett Test: ", style = "heading 3",pos="after") # blank paragraph
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
     
      aux.text.inf <- paste("Based on the results, it is possible to verify that p-value is ",round(val.efa$my.bartlett$pvalue,3),ifelse(round(val.efa$my.bartlett$pvalue,3)<0.05,paste(" (p<0.05), which allow us to conclude the null hypothesis is rejected. Thus, the matrix of population correlations is different from the identity matrix. This difference suggests that factor analysis is appropriate to our data.",sep=""),paste(" (p>0.05), which allow us to conclude the null hypothesis is not rejected. Thus, the matrix of population correlations is similar from the identity matrix. This difference suggests that factor analysis is not appropriate to our data.",sep="")),sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #KMO
      
      aux.text.inf <- paste("KMO Test",sep="")
      body_add_par(my_doc, aux.text.inf, style = "heading 3",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("KMO (Kaiser-Meyer-Olkin) is a widely method to measure the adequacy of sampling. KMO checks if it is possible to factorize the primary variables efficiently. For reference, Kaiser suggested the following classification of the results:\n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("•	0 to 0.49 unacceptable\n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("•	0.50 to 0.59 miserable\n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("•	0.60 to 0.69 mediocre\n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("•	0.70 to 0.79 middling \n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("•	0.80 to 0.89 meritorious\n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("•	0.90 to 1 marvelous\n\n")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      data.kmo.value = data.frame(KMO_value=val.efa$KMO$MSA)
      
      ft <- flextable(data.kmo.value,col_keys = "KMO_value") %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #aux.text.inf <- paste("SOME TEXT ABOUT KMO MSAi") 
      #body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      #body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      data.kmo.values = data.frame(Variable = val.efa$vars,KMO_value_per_variable=val.efa$KMO$MSAi)
      
      ft <- flextable(data.kmo.values,col_keys = c("Variable","KMO_value_per_variable")) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      degrees <- c("unacceptable","miserable","mediocre","middling","meritorious","marvelous")  
      my.kmo <- round(as.numeric(data.kmo.value),3)
      my.vars <- data.kmo.values[which(data.kmo.values[,"KMO_value_per_variable"]<0.5),"Variable"]
      
      if(my.kmo<0.49){
        my.degree <- "unacceptable"
      }else if(my.kmo>0.49 && my.kmo<0.6){
        my.degree <- "miserable"
      }else if(my.kmo>0.59 && my.kmo<0.7){
        my.degree <- "mediocre"
      }else if(my.kmo>0.69 && my.kmo<0.8){
        my.degree <- "middling" 
      }else if(my.kmo>0.79 && my.kmo<0.9){
        my.degree <- "meritorious"
      }else if(my.kmo>0.9 && my.kmo<=1){
        my.degree <- "marvelous"
      } 
      
      aux.text.inf <- paste("The analysis show a KMO equal to ",my.kmo,'. This results suggest that the degree of common variance in our dataset is "',my.degree,'". ',ifelse(length(paste(my.vars))==0,paste(" Additionally, all variables have KMO higher than 0.5, and therefore, the factor analysis is appropriate to this data.",sep=""),paste("Additionally, variables (",paste(my.vars,collapse=", "),") have KMO less than 0.5, and therefore, a detailed discussion of these variables should be made. To continue to perform analysis, being conscientious of the weaknesses or to remove variables with KMO less than 0.5, are valid options.",sep="")),sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #aux.text.inf <- paste("SOME TEXT ABOUT KMO Image Table") 
      #body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      #body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #ft <- flextable(data = tidy(as.data.frame(val.efa[["KMO"]][["Image"]],row.names = "KMO.Image"))) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      #body_add_par(my_doc, "Results KMO Test: ", style = "heading 2",pos="after") # blank paragraph
      #body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      #body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #aux.text.inf <- paste("SOME TEXT ABOUT KMO ImCov Table") 
      #body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      #body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #ft <- flextable(data = tidy(as.data.frame(val.efa[["KMO"]][["ImCov"]],row.names = "KMO.ImCov"))) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      #body_add_par(my_doc, "Results KMO Test: ", style = "heading 2",pos="after") # blank paragraph
      #body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      #body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #eigen
      
      aux.text.inf <- paste("Retained Factors") 
      body_add_par(my_doc, aux.text.inf, style = "heading 2",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("After deciding about the adequacy or not of the model, the next step is to decide the number of factors to retain.") 
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      data.eigen = data.frame(Variable = val.efa$vars,Eigen_values=val.efa$eigen$values)
      
      ft <- flextable(data.eigen, col_keys = c("Variable","Eigen_values")) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_par(my_doc, "Results Eigen Values: ", style = "heading 3",pos="after") # blank paragraph
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("The Kaisen criteria considers that only factors with eigenvalues greater than one should be retained for interpretation. Thus, the table shows that the analyzed data have ",length(which(data.eigen>1))," eigenvalues higher than 1, and therefore, ",length(which(data.eigen>1))," factors should be retained.")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      #summary
      
      
      ft <- flextable(tidy(val.efa$summary$importance)) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_par(my_doc, "Results Summary: ", style = "heading 3",pos="after") # blank paragraph
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      n.factors <- which(val.efa$summary$importance[3,]>0.5)[1]
      
      aux.text.inf <- paste("Other method oftentimes used is the variance explained criteria. This is a method based on to retain the number of factors that account for a certain percent of extracted variance. The literature varies on how much variance should be explained before the number of factors is sufficient. However, there is a consensus that should be more than 50%. Considering the minimum acceptable, at least ",n.factors," factors should be retained according the variance explained criteria.") 
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
      #pca
      
      aux.text.inf <- paste("PCA") 
      body_add_par(my_doc, aux.text.inf, style = "heading 2",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("Principal Component Analysis (PCA) is a dimension-reduction tool that can be used to reduce a large set of variables to a small set that still contains most of the information in the large set. PCA is a mathematical procedure that transforms a number of correlated variables into a smaller number of uncorrelated variables called principal components.") 
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("Communalities Values") 
      body_add_par(my_doc, aux.text.inf, style = "heading 3",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("The following table show the values of communalities. Communality is the proportion of each variables variance that can be explained by the factors.")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      ft <- flextable(tidy(val.efa$pca$communality)) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      my.vars.comm <- names(val.efa$pca$communality[which(val.efa$pca$communality<0.5)])
      
      aux.text.inf <- paste(ifelse(!any(val.efa$pca$communality<0.5),paste("Analyzing the communality values, it is possible to verify that all values are higher than 0.5. This means that the percentage of the variance of each variable explained by common factors is greater than 50% and all of them could be considered in the model.",sep=""),paste("Analyzing the communality values, it is possible to verify that all values are higher than 0.5, except (",paste(my.vars.comm,collapse = ", "),"). Variables with communality values less than 0.5 should be eliminated and the analysis performed without them. NOTE: Although the following analysis might not be ideal, we will keep all variables. PLEASE consider to remove these variables and repeat Factor analysis.",sep="")),sep="")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      my.row.names <- attr(val.efa$pca$loadings,which="dimnames")[[1]]
      my.col.names <- attr(val.efa$pca$loadings,which="dimnames")[[2]]
      my.loadings.matrix <- matrix( as.numeric(reg.efa$pca$loadings),nrow = length(my.row.names), ncol = length(my.col.names),dimnames = list(my.row.names,my.col.names))
      
      my.pca.loadings <- data.frame(Variable = val.efa$vars,Loadings = my.loadings.matrix)
      
      ft <- flextable(my.pca.loadings) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_par(my_doc, "Results PCA:", style = "heading 3",pos="after") # blank paragraph
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("Analyzing the weights of each variable in the factors it is possible to decide the factor with greater weight for the variable. This is the factor that a specific variable belongs to.")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #browser()
      for(i in 2:ncol(my.pca.loadings)){
      assign(paste("PC",i-1,sep=""),c())
      }
      
      for(my.row in 1:nrow(my.pca.loadings)){
        aux <- c()
        i.pc <- which.max(my.pca.loadings[my.row,2:ncol(my.pca.loadings)])
        #browser()
        assign(paste("PC",i.pc,sep=""),c(get(paste("PC",i.pc,sep="")),paste(row.names(my.pca.loadings[my.row,]))))
      }
      
      #first column is not accounted (var names)
      for(i in 2:ncol(my.pca.loadings)){
        aux.text.inf <- paste("Component ",i-1," has variables ",paste(get(paste("PC",i-1,sep="")),collapse = ", "),". ",sep="")
        body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
        body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      }
      
      
      #pca varimax
      body_add_par(my_doc, "Results for PCA VARIMAX: ", style = "heading 3",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.inf <- paste("If some doubts regarding the previous model remains, the results should be analyzed after a factor rotation (Varimax rotation, the most popular rotation method due to its simplicity).") 
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #ft <- flextable(data = tidy(val.efa$pca.varimax$communality)) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      #body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
    
      my.row.names.var <- attr(val.efa$pca.varimax$loadings,which="dimnames")[[1]]
      my.col.names.var <- attr(val.efa$pca.varimax$loadings,which="dimnames")[[2]]
      my.loadings.matrix.var <- matrix( as.numeric(reg.efa$pca.varimax$loadings),nrow = length(my.row.names.var), ncol = length(my.col.names.var),dimnames = list(my.row.names.var,my.col.names.var))
      
      ft <- flextable(data.frame(Variable = val.efa$vars,Loadings.Varimax=my.loadings.matrix.var)) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      my.pca.loadings <- data.frame(Variable = val.efa$vars,Loadings = my.loadings.matrix.var)
      #browser()
      for(i in 2:ncol(my.pca.loadings)){
        assign(paste("PC",i-1,sep=""),c())
      }
      
      for(my.row in 1:nrow(my.pca.loadings)){
        aux <- c()
        i.pc <- which.max(my.pca.loadings[my.row,2:ncol(my.pca.loadings)])
        #browser()
        assign(paste("PC",i.pc,sep=""),c(get(paste("PC",i.pc,sep="")),paste(row.names(my.pca.loadings[my.row,]))))
      }
      
      #first column is not accounted (var names)
      for(i in 2:ncol(my.pca.loadings)){
        aux.text.inf <- paste("Component ",i-1," has variables ",paste(get(paste("PC",i-1,sep="")),collapse = ", "),". ",sep="")
        body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
        body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      }
      
      aux.text.inf <- paste("Now, the analyst should pay attention to two suggestions (without and with rotation) and decide which one makes the most sense for their data.")
      body_add_par(my_doc, aux.text.inf, style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
    
  }
}


reg.writing <- function(what="regression",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  if(what=="regression"){
    
    if(analysis=="header1"){
      
      val.header1.reg <<-  paste('Linear Regression Analysis:', sep = "")
      body_add_par(my_doc, val.header1.reg, style = "heading 1",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="header2"){
      
      val.header2.reg <<-  paste('', sep = "")
      body_add_par(my_doc, val.header2.reg, style = "heading 2",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="res"){
      val.reg <<- reg.res
      
      aux.text.reg <<- paste('Regression analysis is a statistical process for estimating the relationships among variables. It includes many techniques for modeling and analyzing several variables when the focus is on the relationship between a dependent variable and one or more independent variables i.e. the predictors.')
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.reg <<- paste('Please check the following table: ')
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      val.reg$my.summ <- data.frame(Multiple.R_Squared = val.reg$summ$r.squared, Adjusted.R_Squared=val.reg$summ$adj.r.squared, F.Statistic=val.reg$summ$fstatistic[1], pvalue=val.reg$pvalue)
      
      aux.val.reg <<- flextable(as.data.frame(val.reg$my.summ))
      body_add_flextable(my_doc, autofit(fontsize(aux.val.reg, size = 6, part = "all")), pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.reg <<- paste('The Adjusted R-squared value (',round(val.reg$summ$adj.r.squared,3)*100,'%) means the % of the total variability in "',vars.name.pass,'" that is explained by the independent variables used in the linear regression model.',sep = "")
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.reg <<- paste('The F-statistic is ',round(val.reg$summ$fstatistic[1],3),', and has a p-value equal to ',round(val.reg$pvalue,3),ifelse(round(val.reg$pvalue,3)<0.05,paste(' (p<0.05). This means that the null hypothesis should be rejected and, consequently, the model is highly significant.',sep=""),paste(' (p>0.05). This means that the null hypothesis is not rejected and, consequently, the model should be revised.',sep="")),sep="")
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.reg <<- paste('However, it is important to know, if all the studied variables significantly contribute to the linear regression model.',sep="")
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
   
      aux.text.reg <<- paste('Please check the following table: ')
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.val.reg <<- flextable(as.data.frame(val.reg$lm))
      body_add_flextable(my_doc, autofit(fontsize(aux.val.reg, size = 6, part = "all")), pos="after",split = TRUE) 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      #browser()
      significative.vars <- c()
      my.coeffs <- val.reg$summ$coefficients
      for(row in 1:nrow(my.coeffs)){
        if(as.numeric(my.coeffs[row,ncol(my.coeffs)])<0.05){
          significative.vars <- c(significative.vars, row.names(my.coeffs)[row]) 
        }
      }
     
      aux.text.reg <<- paste("The table presents the estimation of each variable’s coefficient. Looking at the p-values,",ifelse(length(significative.vars>0),paste(" it is possible to verify that variables ",paste(significative.vars,collapse = ", ")," are significant (p<0.05).",sep=""),paste("there are no significant variables (p>0.05).",sep="")),sep="")
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
      aux.text.reg <<- paste('Additionally, variables with positive coefficient (see "estimate" column in the table) are directly related to an increase in the dependent variable values. Variables with negative coefficient are directly related to a decrease in the dependent variable values.',sep="") # If this independent variable is boolean, i.e. true or false, it means its value TRUE is correlated to higher values of the dependent variable (BOOLEAN BEHAVIOUR, if p-value < 0.05),sep="")
      body_add_par(my_doc, aux.text.reg, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
  }
}


clus.writing <- function(what="cluster",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  if(what=="cluster"){
    
    if(analysis=="header1"){
      
      val.header1.clus <<-  paste('Cluster Analysis:', sep = "")
      body_add_par(my_doc, val.header1.clus, style = "heading 1",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="header2"){
      
      val.header2.clus <<-  paste(vars.val.pass, sep = "")
      body_add_par(my_doc, val.header2.clus, style = "heading 2",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    if(analysis=="header3"){
      
      val.header3.clus <<-  paste(vars.val.pass, sep = "")
      body_add_par(my_doc, val.header3.clus, style = "heading 3",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    
    if(analysis=="res"){
      val.clus <<- reg.clust
      
      clus.writing(what = "cluster",analysis="header2", vars.val.pass = "Hierarchical Clustering")
    
      aux.text.clus <- paste('Since the “best” method of performing hierarchical clustering does not exist, some literature suggest the use of various methods, simultaneously. Hence, if all methods produce similarly  interpretable solutions, it is possible to conclude that data matrix has natural groupings. Thus, two methods are applied: single-linkage clustering and completelinkage clustering. The respective dendrograms are shown below for the selected variables.',sep="")
      body_add_par(my_doc, aux.text.clus, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      hc.sing.img <- tempfile(fileext = ".png")
      png(filename = hc.sing.img, width = 5, height = 6, units = 'in', res = 300)
      plot(val.clus$HierarchicalClusteringSingle, hang = -1, cex = 0.6)
      rect.hclust(reg.clust$HierarchicalClusteringSingle, k = best.k)
      dev.off()
      hc.sing.img <<- hc.sing.img
      
      clus.writing(what = "cluster",analysis="header3", vars.val.pass = "Hierarchical Clustering Single Method")
      body_add_img(my_doc, hc.sing.img, width = 5, height = 6, style = "centered",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.clus <- paste('Single linkage (also called connectedness or minimum method) is one of the simplest agglomerative hierarchical clustering methods. In single linkage, the distance between groups is defined as the distance between the closest pair of objects, where only pairs consisting of one object from each group are considered. Thus, the distance between two clusters is given by the value of the shortest link between the clusters.',sep="")
      body_add_par(my_doc, aux.text.clus, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
      hc.comp.img <- tempfile(fileext = ".png")
      png(filename = hc.comp.img, width = 5, height = 6, units = 'in', res = 300)
      plot(val.clus$HierarchicalClusteringComplete,hang = -1, cex = 0.6)
      rect.hclust(reg.clust$HierarchicalClusteringComplete, k = best.k)
      dev.off()
      hc.comp.img <<- hc.comp.img
      
      clus.writing(what = "cluster",analysis="header3", vars.val.pass = "Hierarchical Clustering Complete Method")
      body_add_img(my_doc,hc.comp.img,width = 5, height = 6, style = "centered",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.clus <- paste('In complete linkage (also called farthest neighbor), the clustering method is the opposite of single linkage. The distance between groups is defined as the distance between the most distant pair of objects, one from each group. Thus, the distance between two clusters is given by the value of the longest link between clusters.',sep="")
      body_add_par(my_doc, aux.text.clus, style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
      clus.writing(what = "cluster",analysis="header2", vars.val.pass = "KMeans")
      
      aux.text.clus <- paste('Non-hierarchical clustering methods are intended in grouping items (and not variables) in a set of  clusters whose number is defined a-priori. These methods quickly apply to arrays of large data because it is not necessary to calculate and store a new dissimilarity matrix in each step of the algorithm. There are various non-hierarchical methods that differ primarily in the way it unfolds the first aggregation of items in clusters and how the new distances between the centroids of the clusters and the item are calculated. One of the standard methods in most statistical software is the K-means.',sep="")
      body_add_par(my_doc, aux.text.clus , style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      ft <- flextable(tidy(val.clus$KMeans)) %>% theme_booktabs() %>% fontsize( size = 6, part = "all") %>% autofit()
      body_add_flextable(my_doc, ft, pos="after",split = TRUE)
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      kmeans.img <- tempfile(fileext = ".jpeg")
      jpeg(filename = kmeans.img, width = 5, height = 6, units = 'in', res = 300)
      ggsave(kmeans.img,plot=fviz_cluster(val.clus$KMeans.bestk,data = val.clus$data))
      kmeans.img <<- kmeans.img
      
      body_add_img(my_doc,kmeans.img,width = 5, height = 6, style = "centered",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      clus.writing(what = "cluster",analysis="header2", vars.val.pass = "Conclusions")
      
      aux.text.clus <- paste('After using Non-hierarchical clustering methods as K-means and Hierarchical ones, we can conclude, after optimization of both methods, that the presented data clustering is best approached with the use of ',val.clus$bestk,' clusters.',sep="")
      body_add_par(my_doc, aux.text.clus , style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
    }
    
  }
}

classifiers.writing <- function(what="classifiers",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  if(analysis=="header1"){
    
    val.header1.class <<-  paste('Classifiers Analysis:', sep = "")
    body_add_par(my_doc, val.header1.class, style = "heading 1",pos="after") 
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
  }
  
  if(analysis=="header2"){
    
    val.header2.class <<-  paste('', sep = "")
    body_add_par(my_doc, val.header2.class, style = "heading 2",pos="after") 
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
  }
  
  if(analysis=="table"){
    
    body_add_par(my_doc, "About the Classifiers: ", style = "heading 2",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Classification is a task of supervised learning that conveys one or more attributes in order to group subpopulations into different labels or classes. The function that maps the attribution of these classes is called a classifier and its output is therefore discrete. The output is what differentiates classification from the other supervised learning method – regression output is continuous and its mapping function is called estimator."," Knowing the underlying assumptions of the most used classification algorithms may be very useful when the analyst wants to deepen its data understanding. Prediction accuracy for each problem is usually the most important feature. But it may also be useful to have a better computational efficiency. Transparency is another issue that may arise because sometimes algorithms that provide the most accurate models do not reveal how their models are generated."," In this section some of the most popular classification algorithms available in the Weka software application and that will be used in this work are presented.",sep="\n\r")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("The simplest algorithm to be tested is the OneR method that stands for one rule, i.e. based on a unique attribute. Attributes are ranked based on the training set error rate (Holte, 1993). Even considering its simplicity, it can be a relatively accurate method. Rule sets have several advantages. They are easier to understand and may be used as a first order logic. But have some disadvantages too, like poor scaling and noisy data susceptibility.",sep="\n\r")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("JRip implements the propositional rule learner Repeated Incremental Pruning to Produce Error Reduction (RIPPER) proposed by (Cohen, 1995). It is an improved method that grows rules to 100% accuracy and then prunes over fitting rules until accuracy starts to decrease.",sep="")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("In a decision tree, each node is either a decision node for an attribute or a leaf node corresponding to a classification. In contrast to rules setting based on error rates, decision trees rely on entropy-based measures (Holte, 1993). The decision stump is simply a one level tree resulting in the same configuration of the OneR algorithm over a single attribute.",sep="")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste(" Haykin (1999) defines an Artificial Neural Network (ANN) as a massively parallel processor, distributed, consisting of simple processing units, which have a natural propensity for storing experiential knowledge and making it available for use.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste(" The Support Vector Machines (SVM) algorithm represents instances as points in space creating clear gaps between different classes (Chandan Kolvankar et al., 2012)."," http://www.svms.org/anns.html",sep="")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste(" Contrary to the previous algorithms, Instance-based learning (IBl) does not record abstractions from instances, but predicts based on specific instances (Aha, Kibler & Albert, 1991). This is why it is very easy to interpret its results.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste(" From the assumption that combining classifiers has better results than each classifier isolated resulted different ensemble methods. Bagging is an acronym for bootstrap aggregation (Breiman, 1996). It generates multiple versions of a predictor in order to use them into an aggregated average to predict a class. Boosting “works by sequentially applying a classification algorithm to re-weighted versions of the training data and then taking a weighted majority vote of the sequence of classifiers thus produced” (Friedman et al., 2000). It improves the performance of any weak learning algorithm by running on various distributions over the training data and combining the resulting models in a composite classifier (Freund & Schapire, 1996).",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste(" Stacking differs from boosting because it combines different learning algorithms and tries to balance their strengths and weaknesses.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    
    val.classifiers <<- reg.class
    is.num <- sapply(val.classifiers, is.numeric)
    val.classifiers[is.num] <- lapply(val.classifiers[is.num], round, 3)
    
    ft <- flextable(val.classifiers) %>% theme_booktabs() %>% fontsize( size = 5, part = "all") %>% autofit()
    body_add_par(my_doc, "Classifiers Tests Results: ", style = "heading 3",pos="after") # blank paragraph
    body_add_flextable(my_doc, ft, pos="after",split = TRUE) 
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Now, the analyst should pay attention to these results metrics and decide which classifier makes the most sense to be used for their data.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Additional Readings",sep = "")
    body_add_par(my_doc, aux.text.class, style = "heading 2",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Aha, D. W., Kibler, D., & Albert, M. K. (1991). Instance-Based Learning Algorithms. Machine Learning, (6), 37–66.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Breiman, L. (1996). Bagging Predictors. Machine Learning, 24(2), 123–140.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Chandan Kolvankar, Trivedi, J., Mani, B., Ramanathan, R., & Kadam, S. (2012). Support Vector Machine for Learning in Artificial Intelligence Systems. International Journal of Engineering Research and Applications (IJERA), 409–412.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Cohen, W. W. (1995). Fast Effective Rule Induction. In In Proceedings of the Twelfth International Conference on Machine Learning (pp. 115–123). Morgan Kaufmann.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Freund, Y., & Schapire, R. E. (1996). Experiments with a New Boosting Algorithm. In Proceedings of the Thirteenth International Conference (pp. 148–156).",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Haykin, S. (1999). Neural Networks: A Comprehensive Foundation (2nd ed.). Prentice-Hall.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Holte, R. C. (1993). Very Simple Classification Rules Perform Well on Most Commonly Used Datasets. In Machine Learning (pp. 63–91).",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
    aux.text.class <<- paste("Jerome Friedman, Hastie, T., & Tibshirani, R. (2000). Additive Logistic Regression: a Statistical View of Boosting. Annals of Statistics, 28(2), 337–407.",sep = "")
    body_add_par(my_doc, aux.text.class, style = "Normal",pos="after") # blank paragraph
    body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    
  }
  
  
}




cloud.writing <- function(what="cloud",vars.name.pass=NULL,vars.val.pass=NULL,analysis=NULL){
  
  if(what=="cloud"){
    
    if(analysis=="header1"){
      
      val.header1.cloud <<-  paste('Word Cloud Text Analysis:', sep = "")
      body_add_par(my_doc, val.header1.cloud, style = "heading 1",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
      
    }
    
    if(analysis=="header2"){
      
      val.header2.cloud <<-  paste('', sep = "")
      body_add_par(my_doc, val.header2.cloud, style = "heading 2",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
    }
    
    #ver isto melhor (milestone 0.2)
    if(analysis=="bar"){
      #browser()
      #barplot(, las = 2, names.arg = ,
      #        col ="lightblue", main ="Most frequent words",
      #        ylab = "Word frequencies")
      val.bar.word <<- barPlot(d[1:10,]$freq, vars.name.pass = d[1:10,]$word.freq)
      body_add_par(my_doc, "Bar Plot is: ", style = "Normal",pos="after") # blank paragraph
      body_add_img(my_doc, src = val.bar.word, width = 5, height = 6, style = "centered",pos="after")
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
    
    #REVER ISTO POR CAUSA DO SVG
    if(analysis=="cloud"){
     
      body_add_par(my_doc, "Word Cloud (with Word Freq.) is: ", style = "heading 2",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      body_add_par(my_doc, 'Suppose we have a set of English text documents and wish to rank which document is most relevant to the query, "the white SUV". A simple way to start out is by eliminating documents that do not contain all three words "the", "white", and "SUV", but this still leaves many documents. To further distinguish them, we might count the number of times each term occurs in each document; the number of times a term occurs in a document is called its term frequency. However, in the case where the length of documents varies greatly, adjustments are often made.', style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      cloud.img.1 <- tempfile(fileext = ".jpeg")
      jpeg(filename =cloud.img.1, width = 5, height = 6, units = 'in', res = 300)
      ggsave(cloud.img.1,plot=wordcloud(words = d$word.freq, freq = d$freq, min.freq = 1,
                                        max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2")))
      cloud.img.1 <<- cloud.img.1
      
      body_add_img(my_doc, src = cloud.img.1, width = 5, height = 6, style = "centered",pos="after")
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
     
      body_add_par(my_doc, "Word Cloud (with Word TF-IDF Weight) is: ", style = "heading 2",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      body_add_par(my_doc, "In information retrieval, tf–idf or TFIDF (short for term frequency–inverse document frequency), is a numerical metric that is intended to reflect how important a word is to a document in a collection or corpus. It is often used as a weighting factor (More complex than previous and simple Word Frequency) in searches of information retrieval, text mining, and user modeling. The tf–idf value increases proportionally to the number of times a word appears in the document and is offset by the number of documents in the corpus that contain the word, which helps to adjust for the fact that some words appear more frequently in general, therefore, these words are less important in searching for keywords. tf–idf is one of the most popular term-weighting schemes of today information retrieval systems for text documents."
                   , style = "Normal",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
      cloud.img.2 <- tempfile(fileext = ".jpeg")
      jpeg(filename =cloud.img.2, width = 5, height = 6, units = 'in', res = 300)
      ggsave(cloud.img.2,plot= wordcloud(words = d$word.tfidf, freq = d$weight.tf.idf, min.freq = 1,
                                         max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2")))
      cloud.img.2 <<- cloud.img.2
      
      body_add_img(my_doc, src = cloud.img.2, width = 5, height = 6, style = "centered",pos="after")
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      
    }
    
    if(analysis=="table"){
      
      body_add_par(my_doc, "About the Text Variables: ", style = "heading 2",pos="after") # blank paragraph
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      aux.text.clus <- paste('Text variable analysis can make sense when we analyze them as a whole. In this report about variable "',vars.name.pass,'", we analyze it regarding word frequency and weighted word frequency. These analysis give the reader a better notion about the keywords provided on all instances in the data. Sometimes, these variables arise from survey respondents answers, it is clear that such analysis can be complemented with the grouping of answers. Thus, to provide a better look at this variable behavior and influence in other possible variables in the dataset, grouping is advisable. If you have doubts or need further analysis, please contact us and to ask for help, please feel free to use our ticket system.',sep="")
      body_add_par(my_doc, aux.text.clus , style = "Normal",pos="after") 
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      
      val.cloud.df <<- d
      ft <- flextable(data = val.cloud.df[1:100,]) %>% theme_booktabs() %>% fontsize() %>% autofit()
      body_add_par(my_doc, "Results: ", style = "heading 2",pos="after") # blank paragraph
      body_add_flextable(my_doc, ft, pos="after",split = TRUE)
      body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
    }
  }
  
  
}


#Example with iris dataset
#data <- iris

data <- data.report

as.class <- FALSE


if(flag.des == 1){
  
  #Descriptive Analysis
  if(as.class==TRUE){
    nvars <- ncol(data)-1
  }else{
    nvars <- ncol(data)
  }
  
  withProgress(message = 'Making Report (Descriptive Analytics)', value = 0, {
    
    for(var in 1:(nvars)){
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/nvars, detail = paste("Doing part ", var))
      
      if(var==1){
        writing(what = "table",vars.name.pass = NULL,analysis = "head")
        writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "header1")
      }
      
      writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "header2")
      #writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "mean")
      #writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "sd")
      #writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "median")
      writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "na")
      
      if((class(data[,paste(names(data)[var])])=="character" || class(data[,paste(names(data)[var])])=="factor") && length(unique(data[,paste(names(data)[var])]))<30){
        writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "freqs")
        writing(what = "bar",vars.name.pass = paste(names(data)[var]),analysis = NULL)  
        writing(what = "pie",vars.name.pass = paste(names(data)[var]),analysis = NULL)
      }else if((class(data[,paste(names(data)[var])])!="character" && class(data[,paste(names(data)[var])])!="factor") && length(unique(data[,paste(names(data)[var])]))<=10){
        writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "freqs")
        writing(what = "bar",vars.name.pass = paste(names(data)[var]),analysis = NULL)  
        writing(what = "pie",vars.name.pass = paste(names(data)[var]),analysis = NULL)
      }else if((class(data[,paste(names(data)[var])])!="character" && class(data[,paste(names(data)[var])])!="factor") && length(unique(data[,paste(names(data)[var])]))>10){
        writing(what = "descriptive",vars.name.pass = paste(names(data)[var]),analysis = "summ")
        writing(what = "hist",vars.name.pass = paste(names(data)[var]),analysis = NULL)
        #integer but categorical
        x <<- na.omit(data[,var])
        if(any(as.integer(x) != x)){
          writing(what = "scatter",vars.name.pass = paste(names(data)[var]),analysis = NULL)
          writing(what = "box",vars.name.pass = paste(names(data)[var]),analysis = NULL)
        }
      }else{
        body_add_par(my_doc, "Analysis not possible due to data constraints!", style = "Normal",pos="after") 
        body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
      }  
        
       
      }
      
      
      
      
    })
  #if(grepl("win",tolower(my.OS))){
  print(my_doc, target = paste(aux.file.path))
  #}
  #if(grepl("lin",tolower(my.OS))){
  #  print(my_doc, target = paste(aux.file.path))
  #}
  #if(grepl("mac",tolower(my.OS))){
  #  print(my_doc, target = paste(aux.file.path))
  #}
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed Descriptive\n\n")
}





#Inference Analysis

if(flag.inf == 1){
  
  #variables combinations
  data.vars.vector <- names(data)
  var.comb <- t(combn(data.vars.vector,2))
  
  inf.writing(what = "inference",vars.name.pass=NULL,vars.val.pass=NULL,analysis="header1")
  
  
  withProgress(message = 'Making Report (Inference Module)', value = 0, {
    
    for(comb in 1:nrow(var.comb)){
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/nrow(var.comb), detail = paste("Doing part ", comb))
      
      inf.writing(what = "inference",vars.name.pass=var.comb[comb,],vars.val.pass=data[,var.comb[comb,]],analysis="header2")
      aux.res.inf <- inf.writing(what = "inference",vars.name.pass=var.comb[comb,],vars.val.pass=data[,var.comb[comb,]],analysis="res")
      if(class(aux.res.inf)=="numeric"){
        if(aux.res.inf==0){
          body_add_par(my_doc, "Analysis not possible due to data constraints!", style = "Normal",pos="after") 
          body_add_par(my_doc, "", style = "Normal",pos="after") # blank paragraph
        }
      }
    }
    
  })
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed Inference\n\n")
}


if(flag.reg == 1){
  
  #Linear Regression Analysis
  if(exists("reg.res")){
    
    
    withProgress(message = 'Making Report (Linear Regression Module)', value = 100, {
      
      reg.writing(what = "regression",vars.name.pass=NULL,vars.val.pass=NULL,analysis="header1")
      reg.writing(what = "regression",vars.name.pass=depvar,vars.val.pass=NULL,analysis="res")
      
    })
    
  }
  
  
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed Linear Regression\n\n")
  
}

if(flag.efa==1){
  #EFA Analysis
  if(exists("reg.efa")){
    
    withProgress(message = 'Making Report (Factor Analysis Module)', value = 100, {
      
      efa.writing(what = "factor",vars.name.pass=NULL,vars.val.pass=NULL,analysis="header1")
      efa.writing(what = "factor",vars.name.pass=NULL,vars.val.pass=NULL,analysis="res")
      
    })
    
  }
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed EFA\n\n")
  
}


if(flag.clus==1){
  
  #Cluster Analysis
  if(exists("reg.clust")){
    
    withProgress(message = 'Making Report (Clustering Module)', value = 100, {
      
      clus.writing(what = "cluster",vars.name.pass=NULL,vars.val.pass=NULL,analysis="header1")
      clus.writing(what = "cluster",vars.name.pass=NULL,vars.val.pass=NULL,analysis="res")
      
    })
    
  }
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed Cluster Analysis\n\n")
  
}

if(flag.class==1){
  
  #Classifier Analysis
  if(exists("reg.class")){
    
    withProgress(message = 'Making Report (Classifier Module)', value = 100, {
      
      classifiers.writing(what = "classifiers",vars.name.pass=NULL,vars.val.pass=NULL,analysis="header1")
      classifiers.writing(what = "classifiers",vars.name.pass=NULL,vars.val.pass=NULL,analysis="table")
      
    })
    
  }
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed Classifiers\n\n")
  
  
}

if(flag.word==1){
  
  #Cloud Analysis
  if(exists("d")){
    
    withProgress(message = 'Making Report (Word Cloud Module)', value = 100, {
      
      cloud.writing(what = "cloud",vars.name.pass=d$Variable,vars.val.pass=NULL,analysis="header1")
      cloud.writing(what = "cloud",vars.name.pass=d$Variable,vars.val.pass=NULL,analysis="table")
      #cloud.writing(what = "cloud",vars.name.pass=NULL,vars.val.pass=NULL,analysis="bar")
      cloud.writing(what = "cloud",vars.name.pass=d$Variable,vars.val.pass=NULL,analysis="cloud")
      
    })
    
  }
  save.image(file = paste(aux.file.rdata))
  warning(Sys.time(),"Passed Cloud\n\n")
  
  
}


#if(grepl("win",tolower(my.OS))){
print(my_doc, target = paste(aux.file.path))
#}
#if(grepl("lin",tolower(my.OS))){
#  print(my_doc, target = paste(aux.file.path))
#}
#if(grepl("mac",tolower(my.OS))){
#  print(my_doc, target = paste(aux.file.path))
#}

save.image(file = paste(aux.file.rdata))
warning(Sys.time(),"Passed Final Print\n\n")


if(grepl("lin",tolower(my.OS))){
  close(aux.file.report)
}
warning("Passed Final Print")

sink(type = "message")
sink()
#end and print
