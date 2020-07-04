##################################
##LIBRARIES#######################
##################################
library(shiny)
library(nlrx)
library(shinydashboard)
library(shinydashboardPlus)
library(stringr)
library(raster)
library(ggplot2)
library(shinyalert)
library(DT)
library(shinymeta)
source('utils.R') #other helper functions
###################################
###################################


###################################
##SHINY DASHBOARD##################
###################################
ui <- dashboardPagePlus(skin = "purple",
                        header = dashboardHeader(title = "NLRXweb"),
                        
                        ##################################
                        ##SIDEBAR NAVIGATION##############
                        ##################################
                        sidebar = dashboardSidebar(
                          sidebarMenu(
                            menuItem("Model Selection", tabName = "Model", icon = icon("check-square")), #select model either from sample or you can upload model. If you want to change the netlogopath go to utils.r and change the variable netlogopath
                            menuItem("Experiment Setup", tabName = "ExperimentSetup", icon = icon("clipboard")), #set the experiment tab
                            menuItem("Generate code", tabName = "GenerateCode", icon = icon("receipt"))
                          )
                        ),
                        ##################################
                        ##MAIN BODY#######################
                        ##################################
                        body = dashboardBody(
                          #style function to inline label with box. Default behaviour is vertically aligned label in shiny
                          tags$head(tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; } #inline .form-group { display: table-row;}")),
                          
                          tabItems
                          (
                            tabItem(
                              tabName = "Model",
                              fluidRow(
                                column(12 ,
                                       box(
                                         wellPanel(
                                           #Choose from sample models or upload the model
                                           selectInput("choose.model","Model type",choices = c('Sample Model','Upload Model')),
                                           #'ui' will dynamically generate view to either choose from models or upload model. Upload model can upload any file but only able to read if file extension ends with .nlogo.
                                           uiOutput('ui'),
                                           #make compulsory to avoid errors
                                           actionButton("modelsetup", "Enter Model")
                                         )
                                       )
                                )
                              ),
                              fluidRow(
                                #PRINT INFOTAB USING NLRX FUNCTION (utils.r)
                                #verbatimTextOutput("infotab")
                              )
                            ),
                            tabItem(
                              tabName = "ExperimentSetup",
                              fluidRow( #First Box: Experiment Name, reptition, tickmetrics, runtime. Have to add evalticks
                                box(width = 12,
                                    splitLayout(  cellWidths = c("15%","15%","15%","15%","30%"),
                                                  textInput("expname", "Experiment Name", value = "", placeholder = "Name your Experiment"),
                                                  numericInput("repetition","Repetition",value=1 ,min=1),
                                                  radioButtons("tickmetrics","Tickmetrics",c( "ON" = "true" ,"OFF" = "false")),
                                                  numericInput("runtime", "RUNTIME", value=10,min=1),
                                                  sliderInput("evalticks","EvaluateTicks",min=1,max=1000,value=c(0,5))
                                    )
                                )
                              ),
                              fluidRow( #Second Box: Netlogo defined SLIDER params as value, min, max, step, qfun and keep constant
                                box(width = 12,
                                    splitLayout(
                                      cellWidths = c("25%","12.5%","12.5%","12.5%","12.5%"),
                                      tags$div(id = "inline", tags$h4('SLIDER VALUE'), uiOutput("slidervalue")),
                                      tags$div(id = "inline", tags$h4('MIN'),uiOutput("slidermin")),
                                      tags$div(id = "inline", tags$h4('MAX'),uiOutput("slidermax")),
                                      tags$div(id = "inline",  tags$h4('STEP'),uiOutput("sliderstep")),
                                      tags$div(id = "inline", tags$h4('QFUN'),uiOutput("sliderqfun")),
                                      tags$div(tags$h4('CONSTANT'),uiOutput("slidercheckbox"))
                                    )
                                )
                              ),
                              fluidRow( #Third Box: Netlogo defined INPUTBOX params. For creating variable, have to defined min,max and other box which donot actually exits 
                                box(width = 12,
                                    splitLayout(
                                      cellWidths = c("35%","12.5%","12.5%","12.5%","12.5%"),
                                      tags$div(id = "inline", tags$h4('INPUTBOX  VALUE'), uiOutput("inputboxvalue")),
                                      tags$div(id = "inline", tags$h4('MIN'),uiOutput("inputboxmin")),
                                      tags$div(id = "inline", tags$h4('MAX'),uiOutput("inputboxmax")),
                                      tags$div(id = "inline",  tags$h4('STEP'),uiOutput("inputboxstep")),
                                      tags$div(id = "inline", tags$h4('QFUN'),uiOutput("inputboxqfun")),
                                      tags$div(tags$h4('CONSTANT'),uiOutput("inputcheckbox"))
                                    )
                                )
                              ),
                              fluidRow(#Metrics for definition of global, pathc, turtle, link
                                box(width = 12,
                                    column(3,
                                           tags$h4("Global metrics"),
                                           actionButton("add_btn", "Add Textbox"),
                                           actionButton("rm_btn", 'remove'),
                                           uiOutput("textbox_ui")
                                    ),
                                    column(3,
                                           tags$h4("Patch metrics"),
                                           actionButton("add_btn_patch", "Add Textbox"),
                                           actionButton("rm_btn_patch", 'remove'),
                                           uiOutput("textbox_ui_patch")
                                    ),
                                    column(3,
                                           tags$h4("Turtle metrics"),
                                           actionButton("add_btn_turtle", "Add Textbox"),
                                           actionButton("rm_btn_turtle", 'remove'),
                                           uiOutput("textbox_ui_turtle")
                                    ),
                                    column(3,
                                           tags$h4("Link metrics"),
                                           actionButton("add_btn_link", "Add Textbox"),
                                           actionButton("rm_btn_link", 'remove'),
                                           uiOutput("textbox_ui_link")
                                    ),
                                )
                                
                                
                                
                                
                              ),
                              fluidRow( #Simulation setup. Still have to implement actual function most of them dont work
                                box(width = 12,
                                    column(5,
                                           selectInput("simdesign","simdesign",
                                                       choices=c("simple","ff",'lhs',"sobol", "sobol2007", 
                                                                 "soboljansen","morris","eFast","GenSA","GenAlg",
                                                                 "ABCmcmc_Marjoram", "ABCmcmc_Marjoram_original", 
                                                                 "ABCmcmc_Wegmann")
                                           ),
                                           useShinyalert(),
                                           actionButton('experimentsetup', 'experimentsetup'),
                                           actionButton('execute','execute')
                                           
                                           
                                    ),
                                    column(7,
                                           uiOutput("simdesign.helper"),
                                           br(),
                                           helpText("Note: while the data view will show only
                                                                                             the specified number of observations, the
                                                                                            summary will be based on the full dataset."),
                                    )
                                )
                                
                              )
                            ),
                            tabItem( 
                              tabName = "GenerateCode",
                              fluidRow( #Generate code using shinymeta
                                outputCodeButton(verbatimTextOutput("code"))
                              )
                              
                              
                            ),
                            tabItem(
                              tabName = "About",
                              fluidRow(
                                column(2,),
                                column(8,
                                       wellPanel(
                                         tags$h3("References"),
                                         tags$h4("Salecker J, Sciaini M, Meyer KM, Wiegand K (2019). “The nlrx r package: A next-generation framework for reproducible NetLogo model analyses.” Methods in Ecololgy and Evolution, 00, 2041–210X. doi: 10.1111/2041-210X.13286, https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13286."),
                                         tags$h4("Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.ci"),
                                         tags$h4("R Core Team (2020). R: A language and environment for statistical computing. R Foundation for
                                                                                            Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."),
                                         br(),
                                         tags$h3("Support"),
                                         tags$h4("Github student pack"),
                                         tags$h4("Amazon web service (AWS) for free hosting of application using AWS educate support"),
                                         tags$h3("contacts"),
                                         tags$h4("Github repository coming soon..."),
                                         tags$h4("Email: rahulsamrat299@gmail.com")
                                       )
                                ),
                                column(2, )
                              )
                            )
                          )
                          
                        ),
)
##############################################
##SERVER SIDE#################################
##############################################
server <- function(input, output, session) 
{
  
  ##Take input from the session and convert into a dataframe. Used to access input values
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
    eval.ticks <- unlist(x['evalticks'])
    x['evalticks'] <- eval.ticks[1]
    x['sidebarItemExpanded'] <- TRUE
    c <- x['file1']
    
    if(x['file1'] == "NULL"){
      x['file1'] <- 'NA'
    }
    else{
      x['file1'] <- c$file1$datapath
    }
    names <- names(x)
    values <- unlist(x, use.names = FALSE)
    b <- data.frame(names, values, stringsAsFactors = F)
  })
  
  ##If sample model is selected then show list of sample models to choose from in case of upload model show button to upload netlogo file
  gui <- reactive({
    if(input$choose.model == "Sample Model")
    {a <- gui_element(input$type)}
    else{
      a <- input$file1$datapath
      if (is.null(a))
      {return(NULL)}
      else{ b <- nldoc_table_gui(a)}
    }
  })
  
  ##Create the Experiment setup page by comparing netlogo file params and AllInput params which is generated by shiny UI
  exp.setup <- reactive({
    experiment_setup <- create_experiment_setup(gui(),AllInputs())
  })
  
  ##Dyanimically generate choose models
  output$ui <- renderUI({
    switch (input$choose.model,
            "Sample Model" = selectInput(inputId = "type",
                                         label = strong("Select from Sample models"),
                                         choices = unique(models.list),
                                         selected = "Ants"),
            "Upload Model" = fileInput("file1", "upload netlogo file")
    )
  })
  
  ##Dynamically generate SLIDER BOX value,min,max etc
  ##SLIDER#########################################
  output$slidervalue <- renderUI({
    
    a <- gui()
    lapply(seq(a), function(i)
    {
      if((as.character(a[[i]][1]))=="SLIDER")
      {
        numericInput(paste0(as.character(a[[i]][2]),'_value'),as.character(a[[i]][2]),value =as.numeric(a[[i]][3]))
      }
    })
  })
  output$slidermin <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      if((as.character(a[[i]][1]))=="SLIDER")
      {
        numericInput(paste0(as.character(a[[i]][2]),'_min'),NULL,value = as.numeric(a[[i]][4]))
      }
    })
  })
  output$slidermax <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="SLIDER")
      {
        numericInput(paste0(as.character(a[[i]][2]),'_max'),NULL,value = as.numeric(a[[i]][5]))
      }
    })
  })
  output$sliderstep <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="SLIDER")
      {
        numericInput(paste0(as.character(a[[i]][2]),'_step'),NULL,value = as.numeric(a[[i]][6]) )
      }
    })
  })
  output$sliderqfun <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="SLIDER")
      {
        textInput(paste0(as.character(a[[i]][2]),'_qfun'),NULL,value ="qunif" )
      }
    })
  })
  output$slidercheckbox <- renderUI({
    a <- gui()
    lapply(seq(a), function(i){
      if(a[[i]][1] == "SLIDER"){
        checkboxInput(paste0(as.character(a[[i]][2]),"1"),
                      as.character(a[[i]][2]),
                      TRUE)
      }
    })
  })
  #################################################
  
  ##Dynamically generate INPUT BOX; since most of the new models uses inputbox due to
  ##reproduciblity purpose. It will generate similar boxes as slider box with default value in value,min,max
  output$inputboxvalue <- renderUI({
    
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="INPUTBOX")
      {
        numericInput(paste0(as.character(a[[i]][2]),'_inputvalue'),as.character(a[[i]][2]),value =as.numeric(a[[i]][3]))
      }
    })
  })
  output$inputboxmin <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="INPUTBOX")
      {
        #print(a[[i]][1])
        numericInput(paste0(as.character(a[[i]][2]),'_inputmin'),
                     NULL, 
                     as.character(a[[i]][3]))
      }
    })
    
  })
  output$inputboxmax <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="INPUTBOX")
      {
        #print(a[[i]][1])
        numericInput(paste0(as.character(a[[i]][2]),'_inputmax'),
                     NULL, 
                     as.character(a[[i]][3]))
      }
    })
    
  })
  output$inputboxstep <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="INPUTBOX")
      {
        numericInput(paste0(as.character(a[[i]][2]),'_inputstep'),NULL,value = 1 )
      }
    })
  })
  output$inputboxqfun <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="INPUTBOX")
      {
        textInput(paste0(as.character(a[[i]][2]),'_inputqfun'),NULL,value ="qunif" )
      }
    })
  })
  output$inputcheckbox <- renderUI({
    a <- gui()
    lapply(seq(a), function(i){
      if(a[[i]][1] == "INPUTBOX"){
        checkboxInput(paste0(as.character(a[[i]][2]),"1"),
                      paste0(as.character(a[[i]][2]),"1"),
                      TRUE)
      }
    })
  })
  
  ##output definition (metrics)
  ##global metrics
  counter <- reactiveValues(n = 0)
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn,{
    if (counter$n > 0){
      counter$n <- counter$n - 1
    }
  })
  textboxes <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("global.", i),
                    label = paste0("global.", i), 
                    value = AllInputs()[[paste0("global.", i)]])
        })
      })
    }
    
  })
  output$textbox_ui <- renderUI({textboxes()})
  metrics.global <- reactive({
    add_global <- c()
    element_global <- AllInputs()
    element_global_name <- element_global$name
    element_global_value <- element_global$value
    
    for(i in seq(length(element_global_name)))
    {
      if(str_detect(element_global_name[i],'global.'))
      {
        add_global <- c(add_global, element_global_value[i])
      }
    }
    if(length(add_global > 0))
    {
      add_global
    }
    else
    {
      add_global <- NA_character_
    }
  })
  
  ##patch metrics
  counter.p <- reactiveValues(n = 0)
  observeEvent(input$add_btn_patch, {counter.p$n <- counter.p$n + 1})
  observeEvent(input$rm_btn_patch,{
    if (counter.p$n > 0){
      counter.p$n <- counter.p$n - 1
    }})
  textboxes_patch <- reactive({
    
    n <- counter.p$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("patch.", i),
                    label = paste0("patch.", i), 
                    value = AllInputs()[[paste0("patch.", i)]])
        })
      })
    }
    
  })
  output$textbox_ui_patch <- renderUI({ textboxes_patch() })
  metrics.patch <- reactive({
    add_patch <- c()
    element_patch <- AllInputs()
    element_patch_name <- element_patch$name
    element_patch_value <- element_patch$value
    
    for(i in seq(length(element_patch_name)))
    {
      if(str_detect(element_patch_name[i],'patch.'))
      {
        add_patch <- c(add_patch, element_patch_value[i])
      }
    }
    if(length(add_patch > 0))
    {
      add_patch
    }
    else
    {
      add_patch <- NA_character_
    }
  })
  
  ##turtle metrics
  counter.t <- reactiveValues(n = 0)
  observeEvent(input$add_btn_turtle, {counter.t$n <- counter.t$n + 1})
  observeEvent(input$rm_btn_turtle,{
    if (counter.t$n > 0){
      counter.t$n <- counter.t$n - 1
    }})
  textboxes_turtle <- reactive({
    
    n <- counter.t$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("turtle.", i),
                    label = paste0("turtle.", i), 
                    value = AllInputs()[[paste0("turtle.", i)]])
        })
      })
    }
    
  })
  output$textbox_ui_turtle <- renderUI({textboxes_turtle()})
  ##implement metrics.turtle here
  
  ##link metrics
  counter.l <- reactiveValues(n = 0)
  observeEvent(input$add_btn_link, {counter.l$n <- counter.l$n + 1})
  observeEvent(input$rm_btn_link,{
    if (counter.l$n > 0){
      counter.l$n <- counter.l$n - 1
    }})
  textboxes_link <- reactive({
    
    n <- counter.l$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("link.", i),
                    label = paste0("link.", i), 
                    value = AllInputs()[[paste0("link.", i)]])
        })
      })
    }
    
  })
  output$textbox_ui_link <- renderUI({ textboxes_link() })
  ##implement link.metrics here
  
  ##separate constants and variables from slider and inputbox
  ##Function which seprate constan and variables
  separate.consvari <- reactive({
    separate.func <- separate.function(exp.setup(), AllInputs())
  })
  ##Take constants list and prepare it for use in nlrx experiment
  constants <- reactive({
    b <- separate.consvari()
    for(i in seq(length(b[[1]])))
    {
      check <- b[[1]]
      logical <- check[[i]]
      if(str_detect(logical, pattern="\'"))
      {
        log_split <- str_split(logical, pattern="\'")
        log_split1 <- log_split[[1]][2]
        choose1 <- paste0("\"",log_split1,"\"")
        b[[1]][i] <- choose1
      }
    }
    b[[1]]
  })
  ##Take variables list adn ready it to use in nlrx variable experiment slot
  variables <- reactive({
    b <- separate.consvari()
    b[[2]]
  })
  
  ##CHECK EVALUATE TICKS
  eval.ticks.exp <- reactive({
    i <- unlist(input$evalticks)
    eval.ticks.e <- seq(i[1],i[2])
  })
  
  ###############################################
  ##NLRX SETUP###################################
  ###############################################
  
  ###############################################
  ##NLRX NL CLASS SETUP##########################
  nl_model <- metaReactive2({
    req(input$modelsetup)
    modelpaths <- file.path(netlogopath, "app/models/Sample Models/Biology//")
    modeltype <- paste0(input$type, ".nlogo")
    modelfile <- paste0(modelpaths,modeltype)
    isolate
    (
      metaExpr
      ({
        nl <- nl(nlversion = "6.1.1",
                 nlpath = ..(netlogopath),
                 modelpath = ..(file.path(modelfile)),
                 jvmmem = 1024)
      })
    )
    
    
    
  })
  ###############################################
  ##NLRX NL EXPERIMENT SETUP#####################
  nl_experiment <- metaReactive2({
    req(input$experimentsetup)
    isolate(metaExpr({
      nl <- ..(nl_model())
      nl@experiment <- experiment(expname=..(input$expname),
                                  outpath=..(outpath),
                                  repetition=..(input$repetition),
                                  tickmetrics=..(input$tickmetrics),
                                  idsetup="setup",
                                  idgo="go",
                                  idfinal=NA_character_,
                                  idrunnum=NA_character_,
                                  runtime=..(input$runtime),
                                  evalticks= ..(eval.ticks.exp()),
                                  metrics.patches = ..(metrics.patch()),
                                  #metrics.turtles = matrics.turtles(),
                                  metrics= ..(metrics.global()),
                                  variables = ..(variables()),
                                  constants = ..(constants())
      )
      nl
    }))
  })
  ###############################################
  ##NLRX NL SIMDESIGN SETUP######################
  ##Display simdesign options####################
  output$simdesign.helper <- renderUI({
    switch(input$simdesign,
           
           "simple" = wellPanel(  tags$h4("Simdesign Simple"),
                                  numericInput("nseeds", "nseeds", value = 3, min=1)),
           "distinct" = wellPanel( tags$h4("Simdesign Distinct"),
                                   numericInput("nseeds", "nseeds", value = 3, min=1)),
           "ff" = wellPanel( tags$h4("Simdesign Full-factorial"),
                             numericInput("nseeds", "nseeds", value = 3, min=1)),
           "lhs" = wellPanel( tags$h4("Simdesign Latin-hypercube "),
                              numericInput("samples","samples", value =1,min=1),
                              numericInput("nseeds", "nseeds", value = 3, min=1),
                              numericInput("precision", "precision", value =2, min=0)),
           "sobol" = wellPanel( tags$h4("Simdesign sobol"),
                                numericInput("samples","samples", value =1,min=1),
                                numericInput("sobolorder", "sobolorder", value = 2, min=1),
                                numericInput("sobolnboot", "sobolnboot", value = 3, min=1),
                                numericInput("sobolconf", "sobolconf", value =0.95, step=0.01, min=0,max=1),
                                numericInput("nseeds", "nseeds", value = 3, min=1),
                                numericInput("precision", "precision", value =2, min=0)),
           "sobol2007" = wellPanel( tags$h4("Simdesign sobol2007"),
                                    numericInput("samples","samples", value =1,min=1),
                                    numericInput("sobolnboot", "sobolnboot", value = 3, min=1),
                                    numericInput("sobolconf", "sobolconf", value =0.95, step=0.01, min=0,max=1),
                                    numericInput("nseeds", "nseeds", value = 3, min=1),
                                    numericInput("precision", "precision", value =2, min=0),
                                    helpText("The sobol2007 simdesign uses the sensitivity package to set up a sobol2007 sensitivity analysis,
                                              including a simobject of class sobol and a input tibble for simulations. For details on method specific
                                              sensitivity analysis function parameters see ?sobol2007 Finally, the function reports a simdesign
                                              object.")),
           "soboljansen" = wellPanel( tags$h4("Simdesign Sobol-jansen"),
                                      numericInput("samples","samples", value =1,min=1),
                                      numericInput("sobolorder", "sobolorder", value = 2, min=1),
                                      numericInput("sobolnboot", "sobolnboot", value = 3, min=1),
                                      numericInput("sobolconf", "sobolconf", value =0.95, step=0.01, min=0,max=1),
                                      numericInput("nseeds", "nseeds", value = 3, min=1),
                                      numericInput("precision", "precision", value =2, min=0)),
           "morris" = wellPanel( tags$h4("Simdesign Morris"),
                                 textInput("morristype", "morristype",value="oat"),
                                 numericInput("morrislevels", "morrislevels", value = 4, min=0),
                                 numericInput("morrisr", "morrisr",value = 20, min=0),
                                 numericInput("morrisgridjump", "morrisgridjump", value = 2, min=0),
                                 numericInput("nseeds", "nseeds", value =3, min=0)),
           
           "eFast" = wellPanel( tags$h4("Simdesign eFast"),
                                numericInput("samples", "samples", value = 3, min=1),
                                numericInput("nseeds", "nseeds", value = 3, min=1)),
           "GenSA" = wellPanel( tags$h4("Simdesign GenSA"),
                                helpText("Work in progress")),
           "GenAlg" = wellPanel( tags$h4("Simdesign GenAlg"),
                                 helpText("Work in progress")),
           "ABCmcmc_Marjoram"= wellPanel( tags$h4("Simdesign ABCmcmc_Marjoram"),
                                          helpText("Work in progress")),
           "ABCmcmc_Marjoram_original"= wellPanel( tags$h4("Simdesign ABCmcmc_Marjoram_original"),
                                                   helpText("Work in progress")),
           "ABCmcmc_Wegmann" = wellPanel( tags$h4("Simdesign ABCmcmc_Wegmann"),
                                          helpText("Work in progress")),
    )
    
  })
  ##NL simdesign
  nl_simdesign <- metaReactive2({
    req(input$execute)
    nl <- nl_experiment()
    if(input$simdesign ==  'simple'){nl@simdesign <- simdesign_simple(nl,nseeds = input$nseeds)}
    if(input$simdesign ==  'distinct'){nl@simdesign <- simdesign_distinct(nl,nseeds = input$nseeds)}
    if(input$simdesign ==  "ff"){nl@simdesign <- simdesign_ff(nl,nseeds = input$nseeds)}
    if(input$simdesign ==  'lhs'){nl@simdesign <- simdesign_lhs(nl,samples=input$samples,nseeds = input$nseeds,precision=input$precision)}
    if(input$simdesign ==  "sobol"){nl@simdesign <- simdesign_sobol(nl,samples = input$samples,sobolorder=input$sobolorder,sobolnboot=input$sobolnboot,sobolconf=input$sobolconf,nseeds = input$nseeds,precision=input$precision)}
    if(input$simdesign ==  "sobol2007"){nl@simdesign <- simdesign_sobol2007(nl,samples = input$samples,sobolnboot=input$sobolnboot,sobolconf=input$sobolconf,nseeds = input$nseeds,precision=input$precision)}
    if(input$simdesign ==  "soboljansen"){nl@simdesign <- simdesign_soboljansen(nl,samples = input$samples,sobolnboot=input$sobolnboot,sobolconf=input$sobolconf,nseeds = input$nseeds,precision=input$precision)}
    if(input$simdesign ==  "morris"){nl@simdesign <- simdesign_morris(nl,morristype = input$morristype, morrislevels = input$morrislevels, morrisr=input$morrisr, morrisgridjump = input$morrisgridjump,nseed = input$nseeds)}
    if(input$simdesign ==  "eFast"){nl@simdesign <- simdesign_eFast(nl,samples=input$samples,nseeds = input$nseeds)}
    # if(input$simdesign ==  "GenSA"){nl@simdesign <- simdesign_simple(nl,nseeds = input$nseeds)}
    # if(input$simdesign ==  "GenAlg"){nl@simdesign <- simdesign_simple(nl,nseeds = input$nseeds)}
    # if(input$simdesign ==  "ABCmcmc_Marjoram"){nl@simdesign <- simdesign_simple(nl,nseeds = input$nseeds)}
    # if(input$simdesign ==  "ABCmcmc_Marjoram_original"){nl@simdesign <- simdesign_simple(nl,nseeds = input$nseeds)}
    # if(input$simdesign ==  "ABCmcmc_Wegmann"){nl@simdesign <- simdesign_simple(nl,nseeds = input$nseeds)}
    isolate(metaExpr({
      nl@simdesign <- ..(nl@simdesign)
      nl
    }))
  })
  ###############################################
  
  
  ###############################################
  ##DISPLAY CODE IN CODE TAB
  ###############################################
  output$code <- renderPrint({
    expandChain(
      quote(library(nlrx)),
      nl_experiment(),
      nl_simdesign()
    )
  })
  ###############################################
  
  ###############################################
  ##WARNING MESSAGES#############################
  observeEvent(input$modelsetup,{
    if(input$modelsetup == TRUE)
    {
      showNotification(
        ui = "Model selected successfully proceed to Experiment tab",
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  observeEvent(input$experimentsetup,{
    if(input$expname == "")
    {
      shinyalert("Experiment Name error", "experiment name must be defined", type= "error")
    }
    if(length(variables()) == 0)
    {
      shinyalert("Variable absent!", "Uncheck at least one box from Experiment constants box (experiment setup is not possibe without any variable)", type= "error")
    }
    if(is.na(metrics.global()))
    {
      shinyalert("Metrics error", "Define one global metrics to setup simdesign", type= "error")
    }
    if(input$expname != "" && length(variables()) != 0 && is.na(metrics.global()) != TRUE)
    {
      showNotification(
        ui = "Experiment setup success!",
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  observeEvent(input$execute,{
    if(input$experimentsetup == FALSE)
    {
      shinyalert("Simdesign is not allowed without experimentsetup correctly", type= "error")
    }
    if(input$execute == TRUE)
    {
      showNotification(
        ui = "Simdesign success! proceed to code tab to generate code",
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  
}


##############################################
##RUN APP#####################################
##############################################
shinyApp(ui, server)
##############################################
##END#########################################
##############################################


