source("libraries.R")

setwd(WD <<- getwd())
options(digits.secs = 6)

HEADER <- FALSE
keep.id.column <- TRUE

my.OS <<- Sys.info()['sysname']

ui <- dashboardPage(skin='blue',
                    dashboardHeader( title="Statsframe"
                                     
                    ),
                    dashboardSidebar(fileInput(inputId = "data.file", label=h3("Upload your data")),
                                     checkboxInput(inputId = "header", label = h5("Header"),value = HEADER),
                                     checkboxInput(inputId = "idcolumn", label = h5("Keep Column #1"),value = keep.id.column),
                                     checkboxInput(inputId = "stringsAsFactors", label = h5("stringsAsFactors"),value = FALSE),          
                                     radioButtons("sep", 
                                                  h3("Separator"), 
                                                  choices = c(Comma=',',Semicolon=';',Tab='\t',Space=''),
                                                  selected = ";"),
                                     br(),
                                     br(),
                                     br(),
                                     #Group of checkboxes - Language Selection (PLANNED 0.2 MILESTONE)
                                     # checkboxGroupInput("checkGroup", label = h3("Choose Report(s) Language(s)"), 
                                     #                    choices = list("English" = 1, "Spanish" = 2, "Portuguese (EU)" = 3), 
                                     #                    selected = c(1)),
                                     br(),
                                     shinySaveButton('save', label='Save Report', title='Save as...', filetype = list(word = "docx")),
                                     p()
                    ),#cierra el sidebar
                    dashboardBody(
                      tabsetPanel(
                        tabPanel( "Data",#tabpanel data
                                  
                                  
                                  verbatimTextOutput("dataInfo"),box(title= span(h5("Your Input Data:"), style = "color:#3380ff"),
                                                                     width = 1000,status = "primary", DT::dataTableOutput(outputId="tablita")) 
                                  ,
                                  box(title= span(h5("Type of variables"), style = "color:#FF333F"),
                                      width = 50,status = "primary", dataTableOutput(outputId="str"))
                        ),#tabpanel data
                        tabPanel( "Descriptive analysis", 
                                  fluidRow(
                                    column(width = 3,
                                           uiOutput("choose_columns")),
                                    column(width = 2,
                                           imageOutput(outputId="help_desc", hover = hoverOpts(
                                             id = "help_hover",
                                             nullOutside = TRUE),inline = TRUE)
                                    ),
                                    bsTooltip("help_desc", "Select the Variable You need to Explore",
                                              "right", options = list(container = "body"))
                                  ),
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("godes","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('savedes', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  
                                  tabsetPanel(type = "tabs",#tabsetpanel graficos y summary
                                              tabPanel("Plots",#tabpanelfig
                                                       fluidRow(#fluidRow
                                                         
                                                         
                                                         box(
                                                           title = "Fig 1", width = 4, solidHeader = TRUE, status = "primary",
                                                           withSpinner(plotOutput("plot1"), color="#F58F4F")
                                                         ),
                                                         box(
                                                           title = "Fig 2", width = 4,solidHeader = TRUE,status = "primary",
                                                           withSpinner(plotOutput("plot2"), color="#F58F4F")),
                                                         
                                                         
                                                         box(
                                                           title = "Fig 3", width = 4,solidHeader = TRUE,status = "primary",
                                                           withSpinner(plotOutput("plot3"), color="#F58F4F")
                                                         
                                                       )#fluidRow
                                                       
                                              )),#tabpanelfig,
                                              
                                              tabPanel("Summary Table (Numeric Variables Only)", DT::dataTableOutput("sum"))
                                  )#tabsetpanel graficos y summary
                                  
                                  
                                  
                                  
                        ),#setpanel data y tipo de variable,
                        tabPanel( "Inference analysis", 
                                  fluidRow(column(4,uiOutput("choose_columns1")),
                                           column(4,uiOutput("choose_columns2")),
                                           column(width = 2,
                                                  imageOutput(outputId="help_inf", hover = hoverOpts(
                                                    id = "help_hover",
                                                    nullOutside = TRUE),inline = TRUE)
                                           ),
                                           bsTooltip("help_inf", "Select two variables you need to relate to each other. StringAsfactor SHOULD be checked to perform analysis.",
                                                     "right", options = list(container = "body")))
                                  ,
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("goinf","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('saveinf', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  box(verbatimTextOutput("Test"),status = "primary",width = 12)
                        ),
                        
                        tabPanel( "Linear Regression Analysis", 
                                  fluidRow(column(4,uiOutput("choose_independent")),
                                           column(4,uiOutput("choose_dependent")),
                                           column(width = 2,
                                                  imageOutput(outputId="help_reg", hover = hoverOpts(
                                                    id = "help_hover",
                                                    nullOutside = TRUE),inline = TRUE)
                                           ),
                                           bsTooltip("help_reg", "Select the independent variable(s) as predictors - numeric or scale – to verify if they contribute to the dependent variable. To remove one selected variable, select them and click delete on your keyboard. Choose also the dependent variable (only numeric are visible).",
                                                     "right", options = list(container = "body"))),
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("goreg","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('savereg', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  box(verbatimTextOutput("Regression"),status = "primary",width = 12)
                        ),
                        
                        tabPanel( "Factor Analysis", 
                                  fluidRow(column(4,uiOutput("choose_fact_vars")),
                                           column(width = 2,
                                                  imageOutput(outputId="help_fact", hover = hoverOpts(
                                                    id = "help_hover",
                                                    nullOutside = TRUE),inline = TRUE)
                                           ),
                                           bsTooltip("help_fact", "Select all variables you need to include in factor analysis. To remove one selected variable, select them and click delete on your keyboard.",
                                                     "right", options = list(container = "body"))),
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("goefa","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('saveefa', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  box(verbatimTextOutput("EFA"),status = "primary",width = 12)
                        ),
                        
                        tabPanel( "Cluster Analysis", 
                                  fluidRow(column(4,uiOutput("choose_clu_vars")),
                                           column(width = 2,
                                                  imageOutput(outputId="help_clus", hover = hoverOpts(
                                                    id = "help_hover",
                                                    nullOutside = TRUE),inline = TRUE)
                                           ),
                                           bsTooltip("help_clus", "Select all variables you want to consider in the creation of clusters. To remove one selected variable, select them and click delete on your keyboard.",
                                                     "right", options = list(container = "body"))),
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("goclust","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('saveclust', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  fluidRow(#fluidRow
                                    box(
                                      title = "Fig 1", width = 4,solidHeader = TRUE,status = "primary",
                                      withSpinner(plotOutput("ClusteringHieraSingle"), color="#F58F4F")
                                  ),
                                    box(
                                      title = "Fig 2", width = 4,solidHeader = TRUE,status = "primary",
                                      withSpinner(plotOutput("ClusteringHieraComplete"), color="#F58F4F")
                                  ),
                                    box(
                                      title = "Fig 3", width = 4,solidHeader = TRUE,status = "primary",
                                      withSpinner(plotOutput("kmeans"), color="#F58F4F")
                                  )
                        )),
                        
                        tabPanel( "Classifier Analysis", 
                                  fluidRow(column(4,uiOutput("choose_class_vars")),
                                           column(4,uiOutput("choose_class")),
                                           column(width = 2,
                                                  imageOutput(outputId="help_class", hover = hoverOpts(
                                                    id = "help_hover",
                                                    nullOutside = TRUE),inline = TRUE)
                                           ),
                                           bsTooltip("help_class", "Select variable(s) - numeric or scale – you consider contributing to the classification. To remove one selected variable, select it and click delete on your keyboard. Choose also the variable (only non-numerical variables are visible) you have classes.",
                                                     "right", options = list(container = "body"))),
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("goclass","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('saveclass', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  box(title= span(h5("Classifiers Results"), style = "color:#FF333F"),
                                      width = 50,status = "primary", withSpinner(dataTableOutput(outputId="Classifiers"), color="#F58F4F")) 
                        ),
                        
                        tabPanel( "Word Cloud Analysis", 
                                  fluidRow(column(4,uiOutput("choose_text_var")),
                                           column(width = 2,
                                                  imageOutput(outputId="help_cloud", hover = hoverOpts(
                                                    id = "help_hover",
                                                    nullOutside = TRUE),inline = TRUE)
                                           ),
                                           bsTooltip("help_cloud", "Select the text variable you want to obtain keywords.",
                                                     "right", options = list(container = "body"))),
                                  fluidRow(
                                    column(width = 3,
                                           actionButton("gocloud","Click to Analyze First!",width = '100%')),
                                    column(width = 3,
                                           shinySaveButton('saveword', label='Save Partial Report', title='Save as...', filetype = list(word = "docx"),style = "width: 100%;")
                                           ,inline = TRUE)
                                  ),
                                  p(), 
                                  fluidRow(#fluidRow
                                    box(
                                      title = "Fig 1", width = 4, solidHeader = TRUE, status = "primary",
                                      withSpinner(plotOutput("wordbarplot"), color="#F58F4F")
                                    ),
                                    box(
                                      title = "Fig 2", width = 4,solidHeader = TRUE,status = "primary",
                                      withSpinner(plotOutput("cloud"), color="#F58F4F"))
                                  ,#fluidRow
                                  box(
                                    title = "Fig 3", width = 4,solidHeader = TRUE,status = "primary",
                                    withSpinner(plotOutput("cloudtfidf"), color="#F58F4F"))
                        
                                  )    #         box(verbatimTextOutput("Classifier Analysis"),status = "primary",width = 12)
                        ),
                        #tabPanel( "Panel Data Analysis", 
                        #         fluidRow(column(4,uiOutput("choose_panel_vars"))),
                        #         box(verbatimTextOutput("Panel_Anal"),status = "primary",width = 12)
                        #      ),
                        #tabPanel( "Confirmatory Data Analysis", 
                        #         fluidRow(column(4,uiOutput("choose_conf_vars"))),
                        #         box(verbatimTextOutput("Conf_Anal"),status = "primary",width = 12)
                        #)
                        tabPanel( "Q&A",
                                  htmlOutput("QA")
                        ),
                        tabPanel( "Knowledge Base",
                                  htmlOutput("KB")          
                        ),
                        tabPanel( "Support",
                                  htmlOutput("Support")          
                        )
                        
                      )#body 
                    )#dashboardpage
                    
)    




server <- function(input, output, session){##1
  
  ################### var init ###################
  
  flag.des <<- 0
  flag.inf <<- 0
  flag.reg <<- 0
  flag.clus <<- 0
  flag.class <<- 0
  flag.efa <<- 0
  flag.word <<- 0 
  
  
  ##################### Visualizacion de los datos
  
  if(grepl("windows",tolower(my.OS))){
    dir.create(file.path(Sys.getenv("TEMP"), "logs"), showWarnings = FALSE)
    zz <<- file(file.path(Sys.getenv("TEMP"), "logs", paste(format(Sys.time(), "%Y-%m-%d_%H%M%S"),"-Statsframe.log",sep="")), open="wt")
    sink(file=zz,append = TRUE,type = "message")
  }else{
    dir.create("/tmp/logs/", showWarnings = FALSE)
    zz <<- file(paste("/tmp/logs/",format(Sys.time(), "%Y-%m-%d_%H%M%S"),"-Statsframe.log",sep=""), open="wt")
    sink(file=zz,append = TRUE,type = "message")
  }
  
  
  warning("###########################################################\n\n")
  warning(Sys.time(), " - STATSFRAME LOG FILE \n\n")
  warning(Sys.info(),"\n\n")
  warning(Sys.getenv("LOGNAME"),"\n\n")
  warning(sessionInfo(), "\n\n","ENDED LOG FILE HEADER","\n\n")
  warning("###########################################################\n\n")
  
  
  getPageSHOP <-function() {
    return((HTML(readLines('https://statsframe.com/shop/'))))
  }
  output$SHOP<-renderUI({
    
    showModal(modalDialog(span('This feature is only available in other versions of Statsframe Software!'), title ="IMPORTANT NOTICE", footer = tagList(actionButton("ok","OK!")),
                          size = "l", easyClose = TRUE, fade = TRUE))
    getPageSHOP()
    
  })
  
  getPageQA<-function() {
    return((HTML(readLines('https://statsframe.com/qa/'))))
  }
  output$QA<-renderUI({
    #x <- input$test  
    getPageQA()
  })
  
  getPageKB<-function() {
    return((HTML(readLines('https://statsframe.com/knowledgebase/'))))
  }
  output$KB<-renderUI({
    #x <- input$test  
    getPageKB()
  })
  
  getPageSupport<-function() {
    return((HTML(readLines('https://statsframe.com/support/'))))
  }
  output$Support<-renderUI({
    #x <- input$test  
    getPageSupport()
  })
  
  my.data <- reactive({
    file1 <- input$data.file
    
    if(is.null(file1)){return(NULL)}
    file1$datapath
    enc=input$header;factores=input$stringsAsFactors;separadores=input$sep
    if(enc==FALSE && factores==FALSE  && separadores==";"){
      a<-readLines(file1$datapath,n=2)
      b<- nchar(gsub(";", "", a[1]))
      c<- nchar(gsub(",", "", a[1]))
      d<- nchar(gsub("\t", "", a[1]))
      if(b<c&b<d){
        r <- read.table(file1$datapath,sep=";",stringsAsFactors = FALSE,nrows=1)
        
        SEP=";"
      }else if(c<b&c<d){
        r <- read.table(file1$datapath,sep=",",stringsAsFactors = FALSE,nrows=1)
        SEP=","
      }else{
        r <- read.table(file1$datapath,sep="\t",stringsAsFactors = FALSE,nrows=1)
        SEP="\t"
      }
      
      r1<-read.table(file1$datapath,sep=SEP,stringsAsFactors = FALSE,nrows=1,skip = 1)
      aciertos <- 0
      for(i in 1: ncol(r)){
        if(class(r[1,i])==class(r1[1,i])){
          aciertos <- aciertos+1
        }
      }
      
      HEADER=T
      if(aciertos==ncol(r)){
        HEADER=F
      }
      updateCheckboxInput(session, inputId="header", value = HEADER) 
      
      data <- read.table(file1$datapath,sep=SEP,header=HEADER)
      
    }else{
      data <- read.table(file=file1$datapath,sep=input$sep,header=input$header,stringsAsFactors=input$stringsAsFactors)
    }
    
    
    a<-readLines(file1$datapath,n=2)
    b<- nchar(gsub(";", "", a[1]))
    c<- nchar(gsub(",", "", a[1]))
    d<- nchar(gsub("\t", "", a[1]))
    if(b<c&b<d){
      SEP=";"
    }else if(c<b&c<d){
      SEP=","
    }else{
      SEP="\t"
    }
    
    #######################################################################################
    #################################### Date formating ###################################
    #######################################################################################
    
    #r1<-read.table(file1$datapath,sep=SEP,stringsAsFactors = FALSE,nrows=1,skip = 1)
    
    
    #fechas <- c()
    
    #for(i in 1:ncol(r1)){
    #  cat(i)
    #  if(i=11) browser()
    #  if(nchar(r1[1,i])==10 & substr(r1[1,i],5,5) =="-" & substr(r1[1,i],8,8) =="-"){fechas <- c(fechas,i)}
    #}
    
    #fechas
    #for(i in fechas){
    #  data[,i]=as.Date(data[,i],format="%Y-%m-%d")
    #}
    
    
    #######################################################################################
    ################################ END - Date formating #################################
    #######################################################################################
    if(input$idcolumn ==TRUE){
      data.classifier <<- data
      data.report <<- data
      return(data)
    }else{
      keep.id.column = FALSE
      updateCheckboxInput(session, inputId="idcolumn", value = keep.id.column) 
      data.classifier <<- data[,-c(1)]
      data.report <<- data[,-c(1)]
      return(data[,-c(1)])
    }
    
    #   
    # showModal(modalDialog(span('Do you have, in your data, any variable which required user manual input? Consider an example of a survey where the user input their Nationality, and several ambiguous answers occur, answers like "EN", "British", "English"..."'), title ="IMPORTANT NOTICE", footer = tagList(actionButton("yesCheckVars","YES"),actionButton("noCheckVars","NO"),actionButton("donotknow","NOT SURE")),
    #                       size = "l", easyClose = TRUE, fade = TRUE))
    # 
    # 
    
  })
  
  
  
  # observeEvent(input$yesCheckVars,{
  #   
  #   showModal(modalDialog(
  #     
  #     checked.vars <<- checkboxGroupInput('show_vars', 'Columns to show:', names(data),
  #                                         selected = NULL)
  #     ,title ="Choose vars you want to change!", footer = tagList(actionButton("CheckedVars","Check Vars"),actionButton("noCheckVars","Exit")),
  #     size = "l", easyClose = TRUE, fade = TRUE))
  #   
  # })
  # 
  # observeEvent(input$CheckedVars,{
  #   
  #   for(i in 1:length(input$show_vars)){
  #     
  #     #browser()
  #     
  #     showModal(modalDialog( DT::datatable( as.data.frame(data[,paste(input$show_vars[i])]),fillContainer = TRUE, width =  , rownames = TRUE, colnames=TRUE, options = list(scrollX = TRUE, scrolly=TRUE), selection = "multiple",editable = TRUE),title =paste("Change Values for this var named: ",input$show_vars[i], sep=""), footer = tagList(actionButton("nextVar","Go to next variable"),actionButton("Close","Quit changing variables!")),
  #                            size = "l", easyClose = TRUE, fade = TRUE))
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$noCheckVars,{
  #   
  #   showModal(modalDialog(span('No Problem! If you wish you can come back to this edition of variables later! Results can be deviated nonetheless, if you do not take care of data coerence first!'), title ="Please Note", footer = tagList(actionButton("Close","Close Window")),
  #                         size = "l", easyClose = TRUE, fade = TRUE))
  #   
  #   
  #   
  # })
  # 
  # observeEvent(input$donotknow,{
  #   
  #   showModal(modalDialog(span('No Problem! If you wish you can come back to this edition of variables later! Results can be deviated nonetheless, if you do not take care of data coerence first!'), title ="Please Note", footer = tagList(actionButton("Close","Close Window")),
  #                         size = "l", easyClose = TRUE, fade = TRUE))
  #   
  #   
  #   
  #   
  # })
  # 
  # observeEvent(input$Close, {
  #   removeModal()
  # })
  
  #####################################################################
  ##################### HELP HOVER INSTRUCTIONS #######################
  #####################################################################
  
  output$help_desc <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$help_inf <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$help_reg <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$help_fact <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  output$help_clus <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$help_class <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$help_cloud <- renderImage({ # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/Icons/help.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  #####################################################################
  ################## END - HELP HOVER INSTRUCTIONS ####################
  #####################################################################
  
  output$tablita <- renderDataTable({
    if(is.null(my.data())){return()}
    my.data()
    DT::datatable( my.data(), options = list(scrollX = TRUE))
  })
  
  
  output$str <- renderDataTable({
    if(is.null(my.data())){return()}
    tipos <- c()
    for(i in my.data()){
      a <- print(class(i))
      
      tipos <- cbind(tipos,a)
      
      
    }
    
    names <- print(as.vector(names(my.data())) ) 
    junte <- rbind(names,tipos)
    colnames(junte) <- names(my.data())
    junte <- as.data.frame(junte)
    junte <- junte[2,]
    DT::datatable( junte, options = list(scrollX = TRUE,dom = 't'))
  })
  
  
  output$choose_columns1 <- renderUI({
    
    seleccion <- names(my.data())
    
    
    
    # Create the checkboxes and select them all by default
    selectInput(inputId="var1type", "Choose Variable", 
                choices  = seleccion,
                selected = seleccion[1])
  })
  
  ##################################################################
  output$choose_columns2 <- renderUI({
    
    seleccion <- names(my.data())
    
    
    
    # Create the checkboxes and select them all by default
    selectInput(inputId="var2type", "Choose Variable", 
                choices  = seleccion,
                selected = seleccion[2])
  })
  
  
  ##################################################################
  
  output$choose_independent <- renderUI({
    
    enteros <- c()
    
    for(j in 1:ncol(my.data())){
      if(class(my.data()[,j])=="integer" ||class(my.data()[,j])=="numeric"|| class(my.data()[,j])=="double"){
        k=j
        enteros <- c(enteros,k)
      }
    }
    
    
    seleccion <- names(my.data()[,unique(enteros)])
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "indvars", label = "Check Predictor Variables for Linear Regression", multiple = TRUE,
                choices  = seleccion)
  })
  
  
  ##################################################################
  
  output$choose_dependent <- renderUI({
    
    enteros <- c()
    
    for(j in 1:ncol(my.data())){
      if(class(my.data()[,j])=="integer" ||class(my.data()[,j])=="numeric"|| class(my.data()[,j])=="double"){
        k=j
        enteros <- c(enteros,k)
      }
    }
    
    seleccion <- names(my.data()[,unique(enteros)])
    
    
    
    # Create the checkboxes and select them all by default
    selectInput(inputId="depvar", "Choose Dependent Variable", 
                choices  = seleccion,
                selected = seleccion[1])
  })
  
  
  ################################################################
  
  output$choose_clu_vars <- renderUI({
    
    
    enteros <- c()
    
    for(j in 1:ncol(my.data())){
      if(class(my.data()[,j])=="integer" ||class(my.data()[,j])=="numeric"|| class(my.data()[,j])=="double"){
        k=j
        enteros <- c(enteros,k)
      }
    }
    
    
    seleccion <- names(my.data()[,unique(enteros)])
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "cluvars", label = "Check Variables for Individual Clustering", multiple = TRUE,
                choices  = seleccion
    )
  })
  
  
  ##################################################################
  
  output$choose_fact_vars <- renderUI({
    
    enteros <- c()
    
    for(j in 1:ncol(my.data())){
      if(class(my.data()[,j])=="integer" ||class(my.data()[,j])=="numeric"|| class(my.data()[,j])=="double"){
        k=j
        enteros <- c(enteros,k)
      }
    }
    
    
    seleccion <- names(my.data()[,unique(enteros)])
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "factvars", label = "Check Variables for Factor Analysis", multiple = TRUE,
                choices  = seleccion
    )
  })
  
  
  ##################################################################
  
  output$choose_class_vars <- renderUI({
    
    
    seleccion <- names(my.data())
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "classvars", label = "Check Predictor Variables for Classifier Analysis", multiple = TRUE,
                choices  = seleccion
    )
    
  })
  
  
  ##################################################################
  
  output$choose_class <- renderUI({
    
    #class can only be non-numerical
    aux.factor <- which(as.character(sapply(my.data(), class))=="factor")
    aux.char <- which(as.character(sapply(my.data(), class))=="character")
    
    strings <- unique(c(aux.factor,aux.char))
    
    seleccion <- names(my.data())[strings]
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "classvar", label = "Check Class Variable for Classifier Analysis (Non-Numerical Variables Only)",
                choices  = seleccion,
                selected = seleccion[1])
    
  })
  
  
  ##################################################################
  
  output$choose_text_var <- renderUI({
    
    #text var can only be non-numerical
    aux.factor <- which(as.character(sapply(my.data(), class))=="factor")
    aux.char <- which(as.character(sapply(my.data(), class))=="character")
    
    strings <- unique(c(aux.factor,aux.char))
    
    seleccion <- names(my.data())[strings]
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "textvar", label = "Choose Text Variable",
                choices  = seleccion,
                selected = seleccion[1])
    
    
  })
  
  
  observeEvent(input$gocloud,{
    
    source("./R_Modules/Word_Cloud/WordCloud.R")
    
    d <<- wordcloudfunc(my.data(),input$textvar)$df
    
    
    output$wordbarplot <- renderPlot({
      
      
      barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word.freq,
              col ="lightblue", main ="Most frequent words",
              ylab = "Word frequencies")
      
    })
    
    output$cloud <- renderPlot({
      
      wordcloud(words = d$word.freq, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
      
      
    })
    
    output$cloudtfidf <- renderPlot({
      
      wordcloud(words = d$word.tfidf, freq = d$weight.tf.idf, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
      
    })
  })
  
  ##################################################################
  
  
  output$choose_columns <- renderUI({
    
    seleccion <- names(my.data())
    
    
    # Create the checkboxes and select them all by default
    selectInput(inputId="columns", "Choose Variable", 
                choices  = seleccion,
                selected = seleccion[1])
    
    
  })
  
  
  #################################################################
  
 
  observeEvent(input$godes,{
    
    if(is.null(my.data())){return()}
    
    output$plot1 <- renderPlot({
      if(is.null(my.data())){return()}
      seleccion <- names(my.data())
      for(i in 1:ncol(my.data())){if(input$columns==seleccion[i]){minumero=i}}
      
      if(class(my.data()[,minumero])=="character" || class(my.data()[,minumero])=="factor"){
        # Pie Chart from data frame with Appended Sample Sizes
        mytable <- table(my.data()[,minumero])
        lbls <- paste(names(mytable), "\n", mytable, sep="")
        pie(mytable, labels = lbls,
            main=paste("Pie Chart of",seleccion[minumero], "Variable\n (with sample sizes)",sep=" ")
        ) 
      }else{
        p<-ggplot(my.data(), aes(x="", y=my.data()[,minumero])) 
        
        p+geom_boxplot(outlier.colour="red", outlier.shape=8,
                       outlier.size=4,fill=cm.colors(1))+
          labs(title=paste("Box plot of",seleccion[minumero],"variable",sep = " "),x="", y = "")
        
        
      }
    })
    
 ##################################################################
    
    
    output$plot2 <- renderPlot({
      if(is.null(my.data())){return()}
      seleccion <- names(my.data())
      for(i in 1:ncol(my.data())){if(input$columns==seleccion[i]){minumero=i}}
      if((class(my.data()[,minumero])=="character" || class(my.data()[,minumero])=="factor") && length(unique(my.data()[,minumero]))<=30){
        barplot(table(na.omit(my.data()[,minumero])),col=1:10, cex.names = 0.70,cex.axis = 0.70,
                main=paste("Bar Plot of",seleccion[minumero], "Variable\n (with sample sizes)",sep=" "))
        }else if((class(my.data()[,minumero])!="character" && class(my.data()[,minumero])!="factor") && length(unique(my.data()[,minumero]))<=10){
        barplot(table(na.omit(my.data()[,minumero])),col=1:10, cex.names = 0.70,cex.axis = 0.70,
                main=paste("Bar Plot of",seleccion[minumero], "Variable\n (with sample sizes)",sep=" "))
      }else if((class(my.data()[,minumero])!="character" && class(my.data()[,minumero])!="factor") && length(unique(my.data()[,minumero]))>10){
        vector <-   na.omit(my.data()[,minumero])
        breaks <- quantile(vector,seq(0,1,by=0.1),na.rm = TRUE)
        labels = 1:(length(breaks)-1)
        den = density(vector)
        # hist(vector,
        #      breaks=breaks,
        #      col=c("green","darkgreen","blue","darkblue","purple","pink"),
        #      probability=TRUE ,xlim = range(breaks), ylim=range(vector, na.rm=TRUE),
        #      main=paste("Histogram of",seleccion[minumero]," variable",sep=" ") )
        hist(vector,breaks = "Sturges", probability=FALSE ,
             col=c("green","darkgreen","blue","darkblue","purple","pink"),
             main=paste("Histogram of",seleccion[minumero],"variable",sep=" "),xlab = seleccion[minumero])
        #lines(den)
      }else{
        showModal(modalDialog(span(paste('Please NOTE (Fig. 2):  This variable has more than 30 possible values, due to data constraints, this analysis will not continue for this variable only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                              size = "l", easyClose = TRUE, fade = TRUE))
        return({})
      }
    }) 
    
    ###############################################################################
    
    output$plot3 <- renderPlot({
      if(is.null(my.data())){return()}
      seleccion <- names(my.data())
      for(i in 1:ncol(my.data())){if(input$columns==seleccion[i]){minumero=i}}
      if((class(my.data()[,minumero])=="character" || class(my.data()[,minumero])=="factor") && length(unique(my.data()[,minumero]))<30){
        mytable <- table(my.data()[,minumero])
        lbls <- paste(names(mytable), "\n", mytable, sep="")
      
        # Pie Chart from data frame with Appended Sample Sizes
        pie3D(mytable, labels = lbls,explode=0.1,labelcex = 0.8,
              main=paste("Pie Chart 3D of",seleccion[minumero], "Variable\n (with sample sizes)",sep=" ")  )
        
      }else if((class(my.data()[,minumero])!="character" && class(my.data()[,minumero])!="factor")){
        qqnorm(my.data()[,minumero]) 
      }else{
        showModal(modalDialog(span(paste('Please NOTE (Fig. 3):  This variable has more than 30 possible values, due to data constraints, this analysis will not continue for this variable only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                              size = "l", easyClose = TRUE, fade = TRUE))
        return({})
      }
    }) 
    #####################################################################
    
    output$sum <- renderDataTable({
      
      if(is.null(my.data())){return()}
      data1 <- my.data()
      enteros <- c()
      
      for(j in 1:ncol(data1)){
        if(class(data1[,j])=="integer" || class(data1[,j])=="numeric"|| class(data1[,j])=="double"){
          k=j
          enteros <- c(enteros,k)
        }
      }
      
      for(j in enteros){
        data1[,j] <- as.numeric(data1[,j])
      }
      
      datanumerica <- data1[,c(enteros)]
      
      
      datanumerica[is.na(datanumerica)] <- 0
      
      tablita<-c()
      for(j in 1:ncol(datanumerica)){tablita <-rbind(tablita,describe(na.omit(datanumerica[,j]))) 
      }
      
      tablita<-t(tablita)
      colnames(tablita) <- c(names(datanumerica))
      
      DT::datatable( tablita, options = list(scrollX = TRUE))
    })  
    
  })
  
  ############################################################################ 
  
  observeEvent(input$goinf,{
    
    
    if(is.null(my.data())){return()}
    
    
    output$Test <- renderPrint({
      if(is.null(my.data())){return()}
      seleccion <- names(my.data())
      for(i in 1:ncol(my.data())){if(input$var1type==seleccion[i]){var1type=i}}
      for(j in 1:ncol(my.data())){if(input$var2type==seleccion[j]){var2type=j}}
      if(class(my.data()[,var1type])=="factor" && class(my.data()[,var2type])=="factor"){
        crosstbl <- table(my.data()[,var1type],my.data()[,var2type])
        crosstbl_matrix <-as.matrix(crosstbl)
        count_1<-which(crosstbl_matrix<5)
        count_2<-which(crosstbl_matrix>=5)
        ratio1 <- length(count_1)/(nrow(crosstbl_matrix)*ncol(crosstbl_matrix))
        ratio2 <-length(count_2)/(nrow(crosstbl_matrix)*ncol(crosstbl_matrix))
        #if there are zeros and 20% or more of the values are less than 5
        #do Fisher exact test
        if(any(crosstbl == 0 )){
          if(ratio1<0.80 && ratio1>0.20){
            print(crosstbl)
            fisher.test(x = crosstbl, alternative = "two.sided")
            #if there are no zeros and 80% or more of the values are more than 5
            #do Chi-square test
          }else if(ratio1<0.20){
            print(crosstbl)
            fisher.test(x = crosstbl, alternative = "two.sided")
          }else{
            print(paste("NOTE: TEST not robust due to high percentage of values inferior to 5 in crosstable: ",ratio1*100,"%\n\n",sep=""))
          }
        }else{
          if(ratio1<0.80 && ratio1>0.20){
            print(crosstbl)
            chisq.test(x = crosstbl)####what happens if there are values in the crosstbl equal to 5
            #if there are no zeros and 80% or more of the values are more than 5
            #do Chi-square test
          }else if(ratio1<0.20){
            print(crosstbl)
            chisq.test(x = crosstbl)####what happens if there are values in the crosstbl equal to 5
          }else{
            print(paste("NOTE: TEST not robust due to high percentage of values inferior to 5 in crosstable: ",ratio1*100,"%\n\n",sep=""))
          }
          
        }
      }else if(class(my.data()[,var1type])== "factor" && (class(my.data()[,var2type]) == "numeric"|| class(my.data()[,var2type]) == "integer" || class(my.data()[,var2type]) == "double")){
        
        #check normality for quantitative variable
        #with Kolmogorov-Smirnov (lilliefors)
        if(length(unique(na.omit(my.data()[,var2type])))<2 || length(na.omit(my.data()[,var2type]))<5){
          showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                size = "l", easyClose = TRUE, fade = TRUE))
          return({})
        }else{
          p.value <-lillie.test(na.omit(my.data()[,var2type]))$p.value###The K-S test is for a continuous distribution and so MYDATA
        }
        
        aux.data <- my.data()[,c(var1type,var2type)]
        names(aux.data) <- names(my.data()[,c(var1type,var2type)])
        aux.data <- aux.data[complete.cases(aux.data),]
        var1type.name <- names(my.data()[var1type])
        var2type.name <- names(my.data()[var2type])
        
       
        
        #should not contain any ties (repeated values). 
        #https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r
        if(p.value < 0.05){#not Normal
          
          
          
         if(length(unique(aux.data[,var1type.name]))==2){
            #Mann-Whitney
            test <-  wilcox.test(aux.data[,var2type.name]~aux.data[,var1type.name])  #wilcox.test(y~A) where y is numeric and A is A binary factor (independent)
            test$data.name<-paste(seleccion[var2type],"by",seleccion[var1type],sep = " ")
            test
          }else if(length(unique(aux.data[,var1type.name]))>2){
          
            #Kruskal-Wallis
            test <- kruskal.test(aux.data[,var2type.name]~aux.data[,var1type.name])#https://www.statmethods.net/stats/nonparametric.html #kruskal.test(y~A) # where y1 is numeric and A is a factor
            test$data.name<-paste(seleccion[var2type],"by",seleccion[var1type],sep = " ")
            test
          }else{
            showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                  size = "l", easyClose = TRUE, fade = TRUE))
            return({})
          }
        }else{#normal
          
          
          if(length(unique(aux.data[,var1type.name]))==2){
            #ttest
            t.test(aux.data[,var2type.name]~aux.data[,var1type.name], var.equal=TRUE) # t.test(y~x) where y is numeric and x is a binary factor
            
          }else if(length(unique(aux.data[,var1type.name]))>2){
            #Anova
            test<-aov(aux.data[,var2type.name] ~ aux.data[,var1type.name])     #"Anova"
            test$call <- paste("aov(formula = ",seleccion[var2type]," ~ ",seleccion[var1type],")",sep = "")
            test
          }else{
            showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                  size = "l", easyClose = TRUE, fade = TRUE))
            return({})
          }
        }
      }else if(

        class(my.data()[,var2type])== "factor" && (class(my.data()[,var1type]) == "numeric"|| class(my.data()[,var1type]) == "integer" || class(my.data()[,var1type]) == "double")){
        
        #check normality for quantitative variable
        #with Kolmogorov-Smirnov (lilliefors)
        if(length(unique(na.omit(my.data()[,var1type])))<2 || length(na.omit(my.data()[,var1type]))<5){
          showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                size = "l", easyClose = TRUE, fade = TRUE))
          return({})
        }else{
          p.value <-lillie.test(na.omit(my.data()[,var1type]))$p.value###The K-S test is for a continuous distribution and so MYDATA
        }
        
        aux.data <- my.data()[,c(var1type,var2type)]
        names(aux.data) <- names(my.data()[,c(var1type,var2type)])
        aux.data <- aux.data[complete.cases(aux.data),]
        var1type.name <- names(my.data()[var1type])
        var2type.name <- names(my.data()[var2type])
        
        #should not contain any ties (repeated values). 
        #https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r
        if(p.value < 0.05){#not Normal
          
          if(length(unique(na.omit(aux.data[,var2type.name])))==2){
            #Mann-Whitney
            test <-  wilcox.test(aux.data[,var1type.name]~aux.data[,var2type.name])  #wilcox.test(y~A) where y is numeric and A is A binary factor (independent)
            test$data.name<-paste(seleccion[var1type],"by",seleccion[var2type],sep = " ")
            test
          }else if(length(unique(na.omit(aux.data[,var2type.name])))>2){
            #Kruskal-Wallis
            test <- kruskal.test(aux.data[,var1type.name]~aux.data[,var2type.name])#https://www.statmethods.net/stats/nonparametric.html #kruskal.test(y~A) # where y1 is numeric and A is a factor
            test$data.name<-paste(seleccion[var1type],"by",seleccion[var2type],sep = " ")
            test
          }else{
            showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                  size = "l", easyClose = TRUE, fade = TRUE))
            return({})
          }
        }else{#normal
          
          if(length(unique(aux.data[,var2type.name]))==2){
            #ttest
            t.test(aux.data[,var1type.name]~aux.data[,var2type.name], var.equal=TRUE) # t.test(y~x) where y is numeric and x is a binary factor
            
          }else if(length(unique(aux.data[,var2type.name]))>2){
            #Anova
            test<-aov(aux.data[,var1type.name] ~ aux.data[,var2type.name])     #"Anova"
            test$call <- paste("aov(formula = ",seleccion[var1type]," ~ ",seleccion[var2type],")",sep = "")
            test
          }else{
            showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                  size = "l", easyClose = TRUE, fade = TRUE))
            return({})
          }
        }
      }else{
        
        if((class(my.data()[,var1type]) == "numeric"|| class(my.data()[,var1type]) == "integer" || class(my.data()[,var1type]) == "double") &&
           (class(my.data()[,var2type]) == "numeric"|| class(my.data()[,var2type]) == "integer" || class(my.data()[,var2type]) == "double")){
          
          aux.data <- my.data()[,c(var1type,var2type)]
          names(aux.data) <- names(my.data()[,c(var1type,var2type)])
          aux.data <- aux.data[complete.cases(aux.data),]
          var1type.name <- names(my.data()[var1type])
          var2type.name <- names(my.data()[var2type])
          
          #with Kolmogorov-Smirnov
          #with Kolmogorov-Smirnov
          
          #check normality for quantitative variable
          #with Kolmogorov-Smirnov (lilliefors)
          if(length(unique(na.omit(aux.data[,var1type.name])))<2 || length(na.omit(aux.data[,var1type.name]))<5){
            showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                  size = "l", easyClose = TRUE, fade = TRUE))
            return({})
          }else{
            p.value.v1 <-lillie.test(na.omit(aux.data[,var1type.name]))$p.value###The K-S test is for a continuous distribution and so MYDATA
          }
          #check normality for quantitative variable
          #with Kolmogorov-Smirnov (lilliefors)
          if(length(unique(na.omit(aux.data[,var2type.name])))<2 || length(na.omit(aux.data[,var2type.name]))<5){
            showModal(modalDialog(span(paste('Please NOTE:  Due to data constraints, this analysis will not continue for this pair of variables only!')),title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                                  size = "l", easyClose = TRUE, fade = TRUE))
            return({})
          }else{
            p.value.v2 <-lillie.test(na.omit(aux.data[,var2type.name]))$p.value###The K-S test is for a continuous distribution and so MYDATA
          }
          
          print(paste("Kolmogorov-Smirnov test gives, for variable ",paste(names(aux.data[var1type.name])),", a pvalue of: ", p.value.v1,sep=""))
          print(paste("Kolmogorov-Smirnov test gives, for variable ",paste(names(aux.data[var2type.name])),", a pvalue of: ", p.value.v2,sep=""))
          
          #check if the variables are continuous or discrete
          if((class(aux.data[,var1type.name]) == "numeric"|| class(aux.data[,var1type.name]) == "double") &&
             (class(aux.data[,var2type.name]) == "numeric"|| class(aux.data[,var2type.name]) == "double")){
            if((p.value.v1>0.05) && (p.value.v2>0.05)){
              #Pearson Correlation
              test <- cor.test(aux.data[,var1type.name],aux.data[,var2type.name],method = "pearson")
              test$call <- paste(seleccion[var1type],"and",seleccion[var2type],")",sep = " ")  
              test
            }else{
              #Spearman Correlation
              test <- cor.test(aux.data[,var1type.name],aux.data[,var2type.name],method = "spearman")
              test$call <- paste(seleccion[var1type],"and",seleccion[var2type],sep = " ")  
              test
            }
            
          }else{
            #Spearman Correlation
            test <- cor.test(aux.data[,var1type.name],aux.data[,var2type.name],method = "spearman")
            test$call <- paste(seleccion[var1type],"and",seleccion[var2type],sep = " ")  
            test
          }
        }
      }
      
    })
    
  })
  
  ############################################################################
  
  observeEvent(input$goreg,{
    
    output$Regression <- renderPrint({
      if(is.null(my.data())){return()}
      
      if(any(input$depvar %in% input$indvars)){
        showModal(modalDialog(span(paste('Please NOTE: You cannot choose the same variable "',input$depvar,'". Please choose another dependent variable or unselect it from the set of independent variables!')), title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                              size = "l", easyClose = TRUE, fade = TRUE))
        return({})
      }else{
        
        #browser()
        source("./R_Modules/Linear_Regression/reg.R")
        depvar <<- input$depvar
        reg.res <<- reg(my.data(),input$depvar,input$indvars,NULL)
        reg.res$summ
      }
      
    })
    
  })
  
  ############################################################################
  
  observeEvent(input$goclust,{
    
    if(length(input$cluvars) < 2){
      showModal(modalDialog(span(paste('Please NOTE: You have to choose some variables!')), title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                            size = "l", easyClose = TRUE, fade = TRUE))
      return({})
    }  
    source("./R_Modules/Clusterization/clust.R")
    
    reg.clust <<- clust(my.data(),input$cluvars)
    
    
     output$ClusteringHieraSingle <- renderPlot({
         if(is.null(my.data())){return()
       
         }else{
           plot(reg.clust$HierarchicalClusteringSingle, hang = -1, cex = 0.6)
           # Draws rectangles around the branches of a dendrogram highlighting the corresponding clusters
           rect.hclust(reg.clust$HierarchicalClusteringSingle, k = best.k)
         }    
     })
    
    output$ClusteringHieraComplete <- renderPlot({
      if(is.null(my.data())){return()
        
      }else{
        plot(reg.clust$HierarchicalClusteringComplete, hang = -1, cex = 0.6)
        rect.hclust(reg.clust$HierarchicalClusteringComplete, k = best.k)
      }
    })
    
    output$kmeans <- renderImage({
      if(is.null(my.data())){return()
        
      }else{
        kmeans.img <- tempfile(fileext = ".jpeg")
        jpeg(filename = kmeans.img, width = 5, height = 6, units = 'in', res = 300)
        ggsave(kmeans.img,plot=fviz_cluster(reg.clust$KMeans.bestk,data = reg.clust$data))
        kmeans.img <<- kmeans.img
        
        # Return a list containing the filename
        list(src =kmeans.img,
             width = '100%',
             height = '400px',
             alt = "This is alternate text")
      }
    },deleteFile = TRUE)
    
  })
  
  ############################################################################
  
  observeEvent(input$goefa,{
    
    output$EFA <- renderPrint({
      if(is.null(my.data())){return()}
      
      if(length(input$factvars) < 2){
        showModal(modalDialog(span(paste('Please NOTE: You have to choose some variables!')), title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                              size = "l", easyClose = TRUE, fade = TRUE))
        return({})
      }else{
        
        source("./R_Modules/Exploratory_Factor_Analysis/efa.R")
        
        reg.efa <<- efa(my.data(),input$factvars)
        reg.efa
      }
      
    })
    
  })
  
  ############################################################################
  
  observeEvent(input$goclass,{
    
    output$Classifiers <-  renderDataTable({
      if(is.null(my.data())){return()}
      
      if(any(input$classvar %in% input$classvars)){
        showModal(modalDialog(span(paste('Please NOTE: You cannot choose the same variable "',input$classvar,'". Please choose another dependent variable or unselect it from the set of independent variables!')), title ="IMPORTANT NOTICE", footer = modalButton("Cancel"),
                              size = "l", easyClose = TRUE, fade = TRUE))
        return({})
      }else{
        
        source("./R_Modules/Classifiers_Tests/classify.R")
        
        reg.class <<- classifyfunc(data=data.classifier,pred.vars=input$classvars,classvar=input$classvar)
        reg.class
      }
      
      
      
    })
    
  })
  
  ############################################################################
  
  observe({ 
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "save",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$save)
  
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$save)$datapath
      flag.des <<- 1
      flag.inf <<- 1
      flag.reg <<- 1
      flag.clus <<- 1
      flag.class <<- 1
      flag.efa <<- 1
      flag.word <<- 1 
      
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
      
      flag.des <<- 0
      flag.inf <<- 0
      flag.reg <<- 0
      flag.clus <<- 0
      flag.class <<- 0
      flag.efa <<- 0
      flag.word <<- 0 
      } 
    }) 
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "savedes",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$savedes)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$savedes)$datapath
      flag.des <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
     
      flag.des <<- 0
    }
  })
  
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "saveinf",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$saveinf)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$saveinf)$datapath
      flag.inf <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",aux.file.path,sep=""))
     
      flag.inf <<- 0
    }
  })  
  
  
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "savereg",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$savereg)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$savereg)$datapath
      flag.reg <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
      
      flag.reg <<- 0
    }
  })  
  
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "saveefa",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$saveefa)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$saveefa)$datapath
      flag.efa <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
      
      flag.efa <<- 0
    }
  })  
  
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "saveclust",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$saveclust)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$saveclust)$datapath
      flag.clus <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
      
      flag.clus <<- 0
    }
  })  
  
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "saveclass",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$saveclass)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$saveclass)$datapath
      flag.class <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
      
      flag.class <<- 0
    }
  })  
  
  
  observe({
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileSave(input, "saveword",roots = volumes, session = session, restrictions = system.file(package = "base"))
    fileinfo <- parseSavePath(volumes, input$saveword)
    
    if (nrow(fileinfo) > 0) {
      rep.file.path <<- parseSavePath(volumes, input$saveword)$datapath
      flag.word <<- 1
      source("./R_Modules/Report_Output/RtoWord_RS.R")
      
      showModal(modalDialog("Your Report Processing is finally complete.","GOOD NEWS!"))
      
      warning(paste("The Report File is in the path: ",rep.file.path,sep=""))
      
      flag.word <<- 0
    }
  })  
  
  
} ################closes server

shinyApp(ui = ui, server = server)
