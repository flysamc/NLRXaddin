## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(nlrx)
library(stringr)
library(raster)
library(ggplot2)
library(shinyalert)
library(DT)
source('utils.R')


ui <- dashboardPagePlus(skin = "purple",
                        header = dashboardHeader(title = "NLRXweb"),
                        
                        sidebar = dashboardSidebar(
                          sidebarMenu(
                            menuItem("Info", tabName = "Info", icon = icon("info")),
                            menuItem("Model Selection", tabName = "Model", icon = icon("check-square")),
                            menuItem("Experiment Setup", tabName = "ExperimentSetup", icon = icon("clipboard")),
                            menuItem("Simulation Design", tabName = "SimulationDesign", icon = icon("cog")),
                            menuItem("Results", tabName = "Results", icon = icon("check-circle")),
                            menuItem("Analysis", tabName = "Analysis", icon = icon("file-alt")),
                            menuItem("Report", tabName = "Report", icon = icon("receipt")),
                            menuItem("Help", tabName = "Help", icon = icon("question")),
                            menuItem("About", tabName = "About", icon = icon("user-circle"))
                          )
                        ),
                        body = dashboardBody(
                          tabItems
                          (
                            tabItem(
                              tabName = "Model",
                              fluidRow(
                                column(12 ,
                                       box(
                                         wellPanel(
                                           selectInput("choose.model","Model type",
                                                       choices = c('Sample Model', 'Upload Model')),
                                           
                                           uiOutput('ui'),
                                           #uiOutput("fileInputB"),
                                           actionButton("modelsetup", "Enter Model"))
                                       )
                                )
                              ),
                              fluidRow(
                                verbatimTextOutput("infotab")
                              )
                            ),
                            tabItem(
                              tabName = "ExperimentSetup",
                              fluidRow(
                                column(4, 
                                       wellPanel(
                                         textInput("expname", "Experiment Name", value = "", placeholder = "Name your Experiment"),
                                         numericInput("repetition","Repetition",value=1 ,min=1),
                                         radioButtons("tickmetrics","Tickmetrics",c( "ON" = "true" ,"OFF" = "false")),
                                         numericInput("runtime", "RUNTIME", value=10,min=1)),
                                       wellPanel(
                                         sliderInput("evalticks","EvaluateTicks",min=1,max=10000,value=c(0,5))),
                                       wellPanel(
                                         tags$h4("Switch"),
                                         uiOutput("switch"),
                                         helpText('Contain netlogo switch options if defined')),
                                       wellPanel(
                                         tags$h4("InputBox"),
                                         uiOutput("inputbox"),
                                         helpText('Contain netlogo input boxes options if defined')),
                                       wellPanel(
                                         tags$h4("Choose"),
                                         uiOutput("chooser"),
                                         helpText('Contain netlogo choose options if defined')),
                                       
                                ),
                                
                                
                                column(4,
                                       wellPanel(
                                         tags$h4("Sliders"),
                                         uiOutput("slider"))
                                ),
                                column(4, 
                                       wellPanel(
                                         useShinyalert(),
                                         actionButton("experimentsetup","Experiment Setup"),
                                         helpText("Click to attach experiment to netlogo object")),
                                       wellPanel(
                                         checkboxInput("metrics.patch","Include patches", FALSE),
                                         checkboxInput("metrics.turtles", "include turtles", FALSE)),
                                       wellPanel(
                                         tags$h4("Metrics"),
                                         uiOutput("PLOT")),
                                       br(),
                                       wellPanel(
                                         tags$h4("Experiment Constants"),
                                         uiOutput("checkbox"),
                                         helpText("Unselect the checkbox to make it variable. NoTE:
                                             There must be a single variable in your experiment")),
                                       textOutput("box"),
                                       br(),
                                       tableOutput('allinputs'),
                                       br(),
                                )
                              )
                            ),
                            tabItem(
                              tabName = "SimulationDesign",
                              fluidRow(
                                column(5, wellPanel(
                                  # actionButton("simdesign","simdesign")
                                  selectInput("simdesign","simdesign",
                                              choices=c('simple',"ff",'lhs',"sobol", "sobol2007", 
                                                        "soboljansen","morris","eFast","GenSA","GenAlg",
                                                        "ABCmcmc_Marjoram", "ABCmcmc_Marjoram_original", 
                                                        "ABCmcmc_Wegmann"))
                                  # choices=c('simple',"distinct","ff",'lhs',"sobol", "sobol2007", 
                                  #           "soboljansen","morris","eFast","GenSA","GenAlg",
                                  #           "ABCmcmc_Marjoram", "ABCmcmc_Marjoram_original", 
                                  #           "ABCmcmc_Wegmann"))
                                )
                                ),
                                column(7, wellPanel(
                                  uiOutput("simdesign.helper"),
                                  actionButton('execute','execute'),
                                  br(),
                                  helpText("Note: while the data view will show only",
                                           "the specified number of observations, the",
                                           "summary will be based on the full dataset."),
                                  h4(renderPrint('nl.setup')),
                                )
                                )
                              )
                            ),
                            tabItem(
                              tabName = "Results",
                              fluidRow(
                                column(4, wellPanel(
                                  selectInput("simresult.execute","Execute Simulation",
                                              choices = c('run_nl_all','run_nl_dyn', 'run_nl_one')),
                                  useShinyalert(),
                                  actionButton("perform","Perform Simulation"),
                                  
                                )),
                                column(8,
                                       # wellPanel(
                                       # uiOutput("simresult"),
                                       # shinyjs::useShinyjs(),
                                       # 
                                       # textOutput('msgtext')
                                       # ),
                                       wellPanel(
                                         tags$h3('Patch Plot'),
                                         plotOutput("pplot"),
                                         tags$h3('Turtle Plot'),
                                         plotOutput("tplot")
                                       ),
                                       
                                )
                              )
                            ),
                            
                            tabItem(
                              tabName = "Analysis",
                              fluidRow(
                                column(4, ),
                                column(4, 
                                       titlePanel("Analyze Simulation"),
                                       useShinyalert(),
                                       actionButton('analysis','analysis'),
                                       downloadButton("downloadData", label="Download")),
                                
                              ),
                              fluidRow(
                                column(1, ),
                                column(10, 
                                       #dataTableOutput('analyze.sim'),
                                       DT::dataTableOutput("analyze.sim"),
                                       
                                ),
                                column(1, )
                              )
                              
                            ),
                            tabItem(
                              tabName = "Info",
                              fluidRow(
                                column(1, ),
                                column(10,
                                       tags$h3("The NLRXweb is a web-based graphical user interface tool to setup NetLogo simulations in R.
                                            It is based on nlrx R package. The main function of NLRXweb is to provide rapid prototype platform for NetLogo based
                                            models.
                                            "),
                                       tags$h3("Running Simuation"),
                                       helpText("Go to Results tab and click perform simulation to get started"),
                                       helpText("You can also go to help? tab for step by step tutorial"),
                                       tags$h2("NLRXweb & NetLogo parallel"),
                                       tags$img(src="nl1.PNG",align="center"),
                                       tags$img(src="nl2.PNG",align="center"),
                                       tags$img(src="nl3.PNG",align="center"),
                                       tags$img(src="nl4.PNG",align="center"),
                                ),
                                column(1, ),
                              )
                            ),
                            tabItem(
                              tabName = "Help",
                              fluidRow(
                                column(1, ),
                                column(10, 
                                       tags$h4("Step-1"),
                                       tags$img(src="T_modelsetup.PNG",align="center"),
                                       tags$h4("Step-2"),
                                       tags$img(src="T_experimentsetup.PNG",align="center"),
                                       tags$h4("Step-3"),
                                       tags$img(src="T_simdesign.PNG",align="center"),
                                       tags$h4("Step-4"),
                                       tags$img(src="T_result.PNG",align="center"),
                                       
                                ))
                            ),
                            tabItem(
                              tabName = "Report",
                              fluidRow(
                                
                                verbatimTextOutput("nlobject") 
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
                          
                        )
                        
)
###################################################
##########################SERVERSIDE###############

server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$expname, "zip", sep = ".")
    },
    content = function(fname) {
      nl <- nl_results()
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      #fs <- print(tempdir())
      fs <- file.path(tmpdir, "nlobject.rds")
      saveRDS(nl, fs)
      
      
      zipr(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  
  output$ui <- renderUI({
    # if (is.null(input$choose.model))
    #   return()
    
    switch (input$choose.model,
            "Sample Model" = selectInput(inputId = "type",
                                         label = strong("Select from Sample models"),
                                         choices = unique(models.list),
                                         selected = "Ants"),
            "Upload Model" = fileInput("file1", "upload netlogo file")
    )
  })
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
  output$simresult <- renderUI({
    
    switch(input$simresult.execute,
           'run_nl_all' = wellPanel( tags$h4("Execute all NetLogo simulations from a nl object"),
                                     actionButton("execute.all","execute run_nl_all"),
                                     helpText("Work in progress")),
           'run_nl_dyn' = wellPanel( tags$h4("Execute all NetLogo simulations from a nl object"),
                                     actionButton("execute.dyn","execute run_nl_dyn"),
                                     helpText("Work in progress")),
           'run_nl_one' = wellPanel( tags$h4("Execute all NetLogo simulations from a nl object"),
                                     actionButton("execute.one","execute run_nl_one"),
                                     helpText("Work in progress")),
    )
    
  })
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
    b <- data.frame(names, values)
  })
  #output$allinputs <- renderTable(AllInputs())
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
  exp.setup <- reactive({
    #print(input.names <- AllInputs()["names"][[1]])
    input.values <- AllInputs()["values"][[1]]
    #experiment_setup <- as.data.frame(create_experiment_setup(a,AllInputs()))
    experiment_setup <- create_experiment_setup(gui(),AllInputs())
  })
  output$nl.setup <- renderPrint({
    print(nl_simdesign())
  })
  output$slider <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="SLIDER")
      {
        sliderInput(as.character(a[[i]][2]),
                    as.character(a[[i]][2]),
                    as.numeric(a[[i]][4]),
                    as.numeric(a[[i]][5]),
                    as.numeric(a[[i]][3]))
      }
    })
  })
  output$switch <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      if(as.character(a[[i]][1]) == "SWITCH")
      {
        checkboxInput(as.character(a[[i]][2]), as.character(a[[i]][2]), FALSE)
      }
    })
    
  })
  output$inputbox <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      #print(a[[i]][1])
      if((as.character(a[[i]][1]))=="INPUTBOX")
      {
        #print(a[[i]][1])
        textInput(as.character(a[[i]][2]),
                  as.character(a[[i]][2]), 
                  as.character(a[[i]][3]))
      }
    })
    
  })
  output$chooser <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      if((as.character(a[[i]][1]))=="CHOOSER")
      {
        selected <- radio_button_chooser(a[[i]][4])
        radioButtons(as.character(a[[i]][2]),
                     as.character(a[[i]][2]), 
                     selected)
      }
      
    })
  })
  output$checkbox <- renderUI({
    a <- gui()
    lapply(seq(a), function(i){
      if(a[[i]][1] != "PLOT"){
        checkboxInput(paste0(as.character(a[[i]][2]),"1"),
                      as.character(a[[i]][2]),
                      TRUE)
      }
    })
  })
  output$PLOT <- renderUI({
    a <- gui()
    lapply(seq(a), function(i)
    {
      if(as.character((a[[i]][1])) == "PLOT")
      {
        checkboxInput(as.character(a[[i]][2]),
                      as.character(a[[i]][3]),
                      FALSE) 
      }
    })
  })
  
  matrics.input <- reactive({
    calmatrics <- cal_matrics(gui(),AllInputs())
    if(length(calmatrics) != 0)
    {
      calmatrics <- as.character(calmatrics)
    }
    else{
      calmatrics <- NA_character_
    }
  })
  matrics.patch <- reactive({
    a <- c()
    if(input$metrics.patch == TRUE){
      a <- c("pxcor", "pycor", "pcolor")
    }
    else{
      a <- NA_character_
    }
  }) 
  matrics.turtles <- reactive({
    if(input$metrics.turtles == TRUE){
      a <- list("turtles" = c("who",
                              "pxcor",
                              "pycor"
      ))
    }
    else{
      a <- list()
    }
  })
  eval.ticks.exp <- reactive({
    i <- unlist(input$evalticks)
    eval.ticks.e <- seq(i[1],i[2])
  })
  
  #separate.variables <- reactive
  separate.consvari <- reactive({
    separate.func <- separate.function(exp.setup(), AllInputs())
  })
  constants <- reactive({
    b <- separate.consvari()
    for(i in seq(length(b[[1]])))
    {
      check <- b[[1]]
      logical <- check[[i]]
      # if(logical == 'TRUE')
      # {
      #   b[[1]][[i]] <- paste0("\"",'true',"\"")
      # }
      # if(logical == 'FALSE')
      # {
      #   b[[1]][[i]] <- paste0("\"",'false',"\"")
      # }
      if(str_detect(logical, pattern="\'"))
      {
        log_split <- str_split(logical, pattern="\'")
        log_split1 <- log_split[[1]][2]
        choose1 <- paste0("\"",log_split1,"\"")
        b[[1]][i] <- choose1
      }
      # print(logical)
      
    }
    
    
    b[[1]]
  })
  variables <- reactive({
    b <- separate.consvari()
    b[[2]]
  })
  
  
  nl_model <- eventReactive(input$modelsetup,{
    modelpaths <- file.path(netlogopath, "app/models/Sample Models/Biology//")
    modeltype <- paste0(input$type, ".nlogo")
    modelfile <- paste0(modelpaths,modeltype)
    nl <- nl(nlversion = "6.1.1",
             nlpath = netlogopath,
             modelpath = file.path(modelfile),
             jvmmem = 1024)
    nl
  })
  nl_experiment <- eventReactive(input$experimentsetup,{
    nl <- nl_model()
    nl@experiment <- experiment(expname=input$expname,
                                outpath=outpath,
                                repetition=input$repetition,
                                tickmetrics=input$tickmetrics,
                                idsetup="setup",
                                idgo="go",
                                idfinal=NA_character_,
                                idrunnum=NA_character_,
                                runtime=input$runtime,
                                evalticks= eval.ticks.exp(),
                                metrics.patches = matrics.patch(),
                                metrics.turtles = matrics.turtles(),
                                metrics= matrics.input(),
                                variables = variables(),
                                constants = constants())
    nl
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
    if(is.na(matrics.input()))
    {
      shinyalert("Metrics error", "Select at least one metric from Metric box (experiment with metrics is not possible)", type= "error")
    }
    if(input$expname != "" && length(variables()) != 0 && is.na(matrics.input()) != TRUE)
    {
      showNotification(
        ui = "Experiment setup success! proceed to simulation design",
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  
  nl_simdesign <- eventReactive(input$execute,{
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
    nl
  })
  nl_results <- eventReactive(input$perform,{
    nl <- nl_simdesign()
    results <- nl
    withProgress(message = "Running simulation", detail = "This may take a while...", value = 10, {
      if(input$simresult.execute == 'run_nl_one'){ results <- run_nl_one(nl = nl,
                                                                         seed = getsim(nl, "simseeds")[1],
                                                                         siminputrow = 1)}
      if(input$simresult.execute == 'run_nl_all'){results <- run_nl_all(nl)}
      if(input$simresult.execute == 'run_nl_dyn'){results <- run_nl_all(nl)}
      
    })
    
    
    # Attach results to nl object:
    
    setsim(nl, "simoutput") <- results
    nl
    # Write output to outpath of experiment within nl
    #write_simoutput(nl)
    # Do further analysis:
    #nl.analyze <- analyze_nl(nl)
  })
  nl_analysis <- eventReactive(input$analysis,{
    nl <- nl_results()
    myfuns <- list(mean=mean, sd=sd, min=min, max=max)
    nl.analyze <- nl
    
    
    
    if(input$simdesign == 'ff'){
      nl.analyze <- analyze_nl(nl)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    if(input$simdesign == 'lhs'){nl.analyze <- analyze_nl(nl)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    if(input$simdesign == 'sobol'){nl.analyze <- analyze_nl(nl,myfuns)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    if(input$simdesign == 'sobol2007'){nl.analyze <- analyze_nl(nl,myfuns)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    if(input$simdesign == 'soboljansen'){nl.analyze <- analyze_nl(nl,myfuns)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    if(input$simdesign == 'morris'){nl.analyze <- analyze_nl(nl)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    if(input$simdesign == 'eFast'){nl.analyze <- analyze_nl(nl)
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics."))}
    else{nl.analyze <- nl@simdesign@simoutput
    a.table <- dplyr::select(nl.analyze, -starts_with("metrics.")) 
    }
    
  })
  
  output$analyze.sim <- DT::renderDataTable({
    nl_analysis()
    nl_unsupported <- c('simple',"distinct","GenSA","GenAlg",
                        "ABCmcmc_Marjoram", "ABCmcmc_Marjoram_original", 
                        "ABCmcmc_Wegmann")
    t <- nl_analysis()
    DT::datatable(t, options=list(scrollX = TRUE))
  })
  output$pplot <- renderPlot({
    if(!is.na(matrics.patch()))
    {
      a <- nl_results()
      b <- a@simdesign@simoutput$metrics.patches
      c <- b[[1]]
      ggplot(c, aes(pxcor,pycor, fill=pcolor))+
        geom_raster()
      # ggplot(c, aes(pxcor, pycor, fill=pcolor == 55))+
      #   geom_tile(color = 'black') +
      #   scale_fill_manual(values =c('FALSE' = 'green', 'TRUE' = 'black'))
    }
  })
  output$tplot <- renderPlot({
    if(length(matrics.turtles()) != 0)
    {
      a <- nl_results()
      b <- a@simdesign@simoutput$metrics.turtles
      c <- b[[1]]
      ggplot(c,aes(pxcor,pycor,color=breed))+
        geom_point()
    }
  })
  
  nl_message <- function(){
    message(print(nl_results()))
  }
  
 
  output$nlobject <- renderPrint({
    
    if(input$perform == TRUE)
    {
      x <- nl_results()
      print.nl(x)
    }
    
  })
  output$infotab <- renderPrint({
    nlogocode = ""
    if(input$modelsetup == TRUE)
    {
    if(input$choose.model == "Sample Model")
    {gui <- input$type
    modelpath <- paste0(netlogopath, "/app/models/Sample Models/Biology/",gui)
    modeltype <- paste(modelpath, ".nlogo", sep="")
    modelfiles <- c(modeltype)
      nlogocode <- nldoc_read_nlogo(modelfiles)}
    else{
      modelfiles <- input$file1$datapath
      if (is.null(modelfiles))
      {return(NULL)}
      else{
        nlogocode <- nldoc_read_nlogo(modelfiles)
        }
    }
    noxygen_it <- nlogocode$infotabcode
    print(noxygen_it)
    }
  })
  
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
  
  observeEvent(input$execute,{
    if(input$execute == TRUE)
    {
      showNotification(
        ui = "Simdesign success! proceed to results tab to run model",
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  
  
  
  
  

  observeEvent(input$analysis,{
    if(input$perform == FALSE)
    {
      shinyalert("Simulation Incomplete", "Model is not selected. Click 'Enter model' button in model selection tab to load model", type= "error")
    }
  })
  observeEvent(input$perform,{
    if(input$modelsetup == FALSE)
    {
      shinyalert("Model setup error", "Model is not selected. Click 'Enter model' button in model selection tab to load model", type= "error")
    }
    if(input$experimentsetup == FALSE)
    {
      shinyalert("Experiment setup error", "Experiment is not setup. Click 'Experiment Setup' button in Experiment setup tab to run simulation", type= "error")
    }
    if(input$execute == FALSE)
    {
      shinyalert("Simulation Design error", "Choose appropriate simdesign then click 'execute' button in simulation design tab to add simulation to netlogo object", type= "error")
    }
  })
  observe({
    nl <- nl_experiment()
  })
}

shinyApp(ui, server)