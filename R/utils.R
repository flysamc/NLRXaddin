

nldoc_table_gui <- function(modelfiles) {
  
  ## Filter file names for main model file:
  modelpath <- modelfiles[grepl(pattern=".nlogo", modelfiles)]
  
  ## Open the model as string
  model.code <- readLines(modelpath)
  
  ## Find the line in the NetLogoCode where the interface definiton starts
  ## (separator: @#$#@#$#@)
  model.code.s1 <- grep("@#$#@#$#@", model.code, fixed = TRUE)[1]
  
  ## Remove model code before first separator:
  if (is.na(model.code.s1) == FALSE) {
    model.code <- model.code[(model.code.s1 + 1):length(model.code)]
  }
  ## Find second separator where interface definiton ends:
  model.code.s2 <- grep("@#$#@#$#@", model.code, fixed = TRUE)[1]
  
  ## Remove model code following second separator:
  if (is.na(model.code.s1) == FALSE) {
    model.code <- model.code[1:(model.code.s2 - 1)]
  }
  
  ## Extract the parameters and their values line by line:
  modelparam <- list()
  
  for (i in seq_len(length(model.code)))
  {
    ## Read current line from model code
    l <- model.code[i]
    
    ## Check if l is a definition element and of what kind:
    if (l %in% c("SLIDER", "SWITCH", "INPUTBOX", "CHOOSER", "PLOT", "MONITOR")) 
    {
      if (l == "SLIDER") {
        incr = as.numeric(as.character(model.code[i + 10]))
        max <- 0
        if(is.na(as.numeric(as.character(model.code[i + 8]))))
        {
          
          max = incr*10
        }
        else{
          max = as.numeric(as.character(model.code[i + 8]))
        }
        
        name <- as.character(model.code[i + 5])
        
        entry <- list(
          type = l,
          label = as.character(model.code[i + 5]),
          value = as.numeric(as.character(model.code[i + 9])),
          min = as.numeric(as.character(model.code[i + 7])),
          max = max,
          incr = as.numeric(as.character(model.code[i + 10]))
        )
        modelparam[[name]] <- entry
      }
      if (l == "SWITCH") {
        name <- as.character(model.code[i + 5])
        
        entry <- list(
          type = l,
          label <- as.character(model.code[i + 5]),
          value = ifelse(as.numeric(as.character(model.code[i + 8])) == 1,
                         "true", "false")
        )
        modelparam[[name]] <- entry
      }
      # nocov start
      if (l == "INPUTBOX") {
        name <- as.character(model.code[i + 5])
        
        entry <- list(
          type = l,
          label <- as.character(model.code[i + 5]),
          value = model.code[i + 6],
          entrytype = model.code[i + 9]
        )
        modelparam[[name]] <- entry
      }
      # nocov end
      if (l == "CHOOSER") {
        name <- as.character(model.code[i + 5])
        
        validvalues <- paste0("\'",scan(text = (model.code[i + 7]), what = "", quiet = TRUE),"\'")
        select_id <- (as.numeric(as.character(model.code[i + 8])) + 1)
        selectedvalue <- validvalues[select_id]
        
        entry <- list(
          type = l,
          label <- as.character(model.code[i + 5]),
          value = selectedvalue,
          validvalues = validvalues
        )
        modelparam[[name]] <- entry
      }
      #plot metrics
      if (l == "PLOT") {
        name <- as.character(model.code[i+5])
        counter <- 0
        x <- model.code[i+counter]
        while( x != "")
        {
          x <- model.code[i+counter]
          if(str_detect(x, pattern="plotxy"))
          {
            b <- str_split(x, pattern="plotxy ticks ")
            c <- str_split(b[[1]][2], pattern='"')
            d <- c[[1]][1]
            entry <- list(
              type = l,
              label = name,
              value = d
            )
            name <- str_c(name , as.character(counter))
            modelparam[[name]] <- entry
          }
          if(str_detect(x, pattern="plot "))
          {
            b <- str_split(x, pattern="plot ")
            c <- b[[1]][2]
            d <- str_split(c, pattern = '"')
            e <- d[[1]][1]
            entry <- list(
              type = l,
              label = name,
              value = e
            )
            name <- str_c(name , as.character(counter))
            modelparam[[name]] <- entry
          }
          counter <- counter + 1
          x <- model.code[i + counter]
          
        }
        
      }
    }
  }
  return(modelparam)
}
##################################################
#####################FRONTEND######################

#load model file names
models.list <- list.files(path = "C:/Program Files/NetLogo 6.1.1/app/models/Sample Models/Biology", pattern = ".nlogo")
models.list <- str_remove(models.list, pattern = ".nlogo")

#define netlogo paths
# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.1.1")
outpath <- file.path("C:/Users/rahulsamrat/Desktop/R")



radio_button_chooser <- function(model.chooser){
  model.choose <- list()
  model.chooser <- as.character(model.chooser)
  model.chooser <- unlist(strsplit(model.chooser, split=","))
  for(i in seq(length(model.chooser))){
    a <- stringr::str_match(model.chooser[i], "\"(.*?)\"")[2]
    model.choose[a] <- a}
  return(model.choose)
  
}
gui_element <- function(gui){
  modelpath <- paste0(netlogopath, "/app/models/Sample Models/Biology/",gui)
  modeltype <- paste(modelpath, ".nlogo", sep="")
  modelfiles <- c(modeltype)
  a <- nldoc_table_gui(modelfiles)
  return(a)
}
read_element <- function(gui){
  modelpath <- paste0(netlogopath, "/app/models/Sample Models/Biology/",gui)
  modeltype <- paste(modelpath, ".nlogo", sep="")
  modelfiles <- c(modeltype)
  a <- nldoc_read_nlogo(modelfiles)
  return(a)
}
create_experiment_setup <- function(modelfiles,userinput){
  modeldata.names <- list()
  a <- userinput["names"][[1]]
  b <- userinput["values"][[1]]
  for(i in seq(length(modelfiles)))
  {
    modeldata.names[i] <- modelfiles[[i]][2]  
  }
  
  for(i in seq(length(a))){ 
    for(j in seq(length(modeldata.names)))
    {
      if(modeldata.names[j] == a[i] ){
        modelfiles[[j]][3] <- b[i]
      }
    }
  }
  return(modelfiles)
  
}
separate.function <- function(modelfiles, userinput){
  modeldata.names <- list()
  modeldata.value <- list()
  constants <- list()
  variables <- list()
  a <- userinput["names"][[1]]
  b <- userinput["values"][[1]]
  for(i in seq(length(modelfiles)))
  {
    #modeldata.type [i] <- modelfiles[[i]][1]
    modeldata.names[i] <- modelfiles[[i]][2]
    modeldata.value[i] <- modelfiles[[i]][3]
  }
  
  for(i in seq(length(a)))
  { 
    for(j in seq(length(modeldata.names)))
    {
      if(str_detect(a[i], "1"))
      {
        c1 <- str_remove(a[i],pattern="1" )
        if(modeldata.names[j] == c1)
        {
          if(b[i] == TRUE)
          {
            for(k in seq(length(a)))
            {
              if(a[k] == paste0(as.character(c1),'_value'))
              {
                
                constant.name <- as.character(modeldata.names[j])
                constant.value <- b[k]
                constants[constant.name] = constant.value
                
              }
            }
            
          }
          else
          {
            if(modelfiles[[j]][1] == "SLIDER")
            {
              variable.name <- as.character(modeldata.names[j])
              vari.min = NULL
              vari.max = NULL
              vari.qfun = NULL
              vari.step = NULL
              for(k in seq(length(a))){if(a[k] == paste0(as.character(c1),'_min')){vari.min = as.numeric(b[k])}}
              for(k in seq(length(a))){if(a[k] == paste0(as.character(c1),'_max')){vari.max = as.numeric(b[k])}}
              for(k in seq(length(a))){if(a[k] == paste0(as.character(c1),'_qfun')){vari.qfun = b[k]}}
              for(k in seq(length(a))){if(a[k] == paste0(as.character(c1),'_step')){vari.step = as.numeric(b[k])}}
              entry <- list(
                min = vari.min,
                max = vari.max,
                qfun = vari.qfun,
                step = vari.step)
              variables[[variable.name]] <- entry
            }
          }
        }
      }
    }
  }
  return(list("constants"=constants,"variabes"=variables))
}
cal_matrics <- function(modelfiles,userinput){
  modeldata.type <- list()
  modeldata.label <- list()
  modeldata.value <- list()
  counter <- 1
  matrics <- list()
  a <- userinput["names"][[1]]
  b <- userinput["values"][[1]]
  for(i in seq(length(modelfiles)))
  {
    #modeldata.type [i] <- modelfiles[[i]][1]
    modeldata.type[i] <- modelfiles[[i]][1]
    modeldata.label[i] <- modelfiles[[i]][2]
    modeldata.value[i] <- modelfiles[[i]][3]
  }
  for(i in seq(length(a))){
    for(j in seq(length(modeldata.type)))
    {
      if(as.character(modeldata.type[j]) == "PLOT")
      {
        if(as.character(a[i]) == as.character(modeldata.label[j]))
        {
          if(b[i] == TRUE)
          {
            matrics[counter] <- as.character(modeldata.value[j])
            counter <- counter + 1
          }
        }
      }
    }
  }
  return(matrics)
}

######################################
##############PRINT FUNCTION##########
print.nl <- function(x, ...){
  util_print.nl(x)
  util_print.experiment(x@experiment)
  util_print.simdesign(x@simdesign)
  util_print.summary(x)
}
util_print.nl <- function(x, ...) {
  
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red
  
  
  cat(style_heading(paste0("\n", "   NL OBJECT   ", "\n")))
  
  cat("NetLogo version = ")
  output <- paste0(x@nlversion, "\n")
  cat(ifelse(nchar(x@nlversion) > 0, style_def(output), style_na(output)))
  
  
  cat("NetLogo path    = ")
  output <- paste0(x@nlpath, "\n")
  cat(ifelse(!identical(x@nlpath, character(0)), style_def(output), style_na(output)))
  
  
  cat("Model path      = ")
  output <- paste0(x@modelpath, "\n")
  cat(ifelse(!identical(x@modelpath, character(0)), style_def(output), style_na(output)))
  
  cat("JVM memory      = ")
  output <- paste0(x@jvmmem, "\n")
  cat(ifelse(!is.na(x@jvmmem), style_def(output), style_na(output)))
  
}
util_print.summary <- function(x, ...){
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red
  
  cat(style_heading(paste0("\n", "   SUMMARY   ", "\n")))
  
  cat("supported nlversion: ")
  output <- ifelse(x@nlversion %in% c("5.3.1", "6.0", "6.0.1", "6.0.2", "6.0.3", "6.0.4", "6.1.0", "6.1.1"), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("nlpath exists on local system: ")
  output <- ifelse(dir.exists(x@nlpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("modelpath exists on local system: ")
  output <- ifelse(file.exists(x@modelpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("valid jvm memory: ")
  output <- ifelse(is.numeric(x@jvmmem), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("valid experiment name: ")
  output <- ifelse(is.na(x@experiment@expname) | grepl("\\s", getexp(x, "expname")), style_na("\u2717"), style_def("\u2713"))
  cat(paste0(output, "\n"))
  
  cat("outpath exists on local system: ")
  output <- ifelse(dir.exists(x@experiment@outpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("setup and go defined: ")
  output <- ifelse(!all(is.na(x@experiment@idsetup), is.na(x@experiment@idgo)), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  
  cat("variables defined: ")
  output <- ifelse(length(x@experiment@variables) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  if(!identical(x@modelpath, character(0))){
    if(file.exists(x@modelpath)){
      cat("variables present in model: ")
      output <- ifelse(length(x@experiment@variables) > 0 & all(names(x@experiment@variables) %in% names(report_model_parameters(x))),
                       style_def("\u2713"), style_na("\u2717"))
      cat(paste0(output, "\n"))
    }
  }
  
  cat("constants defined: ")
  output <- ifelse(length(x@experiment@constants) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  if(!identical(x@modelpath, character(0))){
    if(file.exists(x@modelpath)){
      cat("constants present in model: ")
      output <- ifelse(length(x@experiment@constants) > 0 & all(names(x@experiment@constants) %in% names(report_model_parameters(x))),
                       style_def("\u2713"), style_na("\u2717"))
      cat(paste0(output, "\n"))
    }
  }
  
  cat("metrics defined: ")
  output <- ifelse(length(x@experiment@metrics) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("spatial Metrics defined: ")
  output <- ifelse(length(x@experiment@metrics.turtles) > 0 | length(x@experiment@metrics.patches) > 0 | length(x@experiment@metrics.links) > 0,
                   style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  
  
  
  
  cat("simdesign attached: ")
  output <- ifelse(!is.na(x@simdesign@simmethod), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("siminput parameter matrix: ")
  output <- ifelse(nrow(x@simdesign@siminput) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("number of siminputrows: ")
  output <- ifelse(nrow(x@simdesign@siminput) > 0, style_def(nrow(x@simdesign@siminput)), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("number of random seeds: ")
  output <- ifelse(!all(is.na(x@simdesign@simseeds)), style_def(length(x@simdesign@simseeds)), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("estimated number of runs: ")
  output <- ifelse(!all(nrow(x@simdesign@siminput) == 0, is.na(x@simdesign@simseeds)),
                   style_def(nrow(x@simdesign@siminput) * length(x@simdesign@simseeds)),
                   style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("simoutput results attached: ")
  output <- ifelse(nrow(x@simdesign@simoutput) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))
  
  cat("number of runs calculated: ")
  output <- ifelse(nrow(x@simdesign@simoutput) > 0,
                   style_def(nrow(x@simdesign@simoutput)),
                   style_na("\u2717"))
  cat(paste0(output, "\n"))
  
}
util_print.experiment <- function(x, ...){
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red
  
  cat(style_heading(paste0("\n", "   EXPERIMENT   ", "\n")))
  
  cat("Experiment name        = ")
  output <- paste0(x@expname, "\n")
  cat(ifelse(!is.na(x@expname), style_def(output), style_na(output)))
  
  cat("Output path            = ")
  output <- paste0(x@outpath, "\n")
  cat(ifelse(!is.na(x@outpath), style_def(output), style_na(output)))
  
  cat("NetLogo repetitions    = ")
  output <- paste0(x@repetition, "\n")
  cat(ifelse(!is.na(x@repetition), style_def(output), style_na(output)))
  
  cat("Measure on each tick?  = ")
  output <- paste0(x@tickmetrics, "\n")
  cat(ifelse(!is.na(x@tickmetrics), style_def(output), style_na(output)))
  
  cat("Setup procedure(s)     = ")
  output <- paste0(paste(x@idsetup, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@idsetup)), style_def(output), style_na(output)))
  
  cat("Go procedure(s)        = ")
  output <- paste0(paste(x@idgo, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@idgo)), style_def(output), style_na(output)))
  
  cat("Final procedure(s)     = ")
  output <- paste0(paste(x@idfinal, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@idfinal)), style_def(output), style_opt(output)))
  
  cat("Run nr. widget name    = ")
  output <- paste0(x@idrunnum, "\n")
  cat(ifelse(!is.na(x@idrunnum), style_def(output), style_opt(output)))
  
  cat("Runtime (ticks)        = ")
  output <- paste0(x@runtime, "\n")
  cat(ifelse(!is.na(x@runtime), style_def(output), style_na(output)))
  
  cat("Report output on ticks = ")
  output <- paste0(paste(x@evalticks, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@evalticks)), style_def(output), style_opt(output)))
  
  cat("Stop condition         = ")
  output <- paste0(x@stopcond, "\n")
  cat(ifelse(!is.na(x@stopcond), style_def(output), style_opt(output)))
  
  cat("Metrics (output)       = ")
  output <- paste0(paste(x@metrics, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@metrics)), style_def(output), style_na(output)))
  
  cat(paste0("\n", "Turtle metrics (output)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@metrics.turtles)), paste(unlist(x@metrics.turtles), collapse = ", "), sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@metrics.turtles)), style_def(output), style_opt(output)))
  
  cat(paste0("\n", "Patch metrics (output)", "\n"))
  output <- paste0("    ", paste(unlist(x@metrics.patches), collapse = ", "), "\n")
  cat(ifelse(!all(is.na(x@metrics.turtles)), style_def(output), style_opt(output)))
  
  cat(paste0("\n", "Link metrics (output)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@metrics.links)), paste(unlist(x@metrics.links), collapse = ", "), sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@metrics.links)), style_def(output), style_opt(output)))
  
  cat(paste0("\n", "Variable parameters (input)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@variables)), x@variables, collapse="\n", sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@variables)), style_def(output), style_opt(output)))
  
  cat(paste0("\n", "Constant parameters (input)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@constants)), x@constants, sep=" = ", collapse="\n"), "\n")
  cat(ifelse(!all(is.na(x@constants)), style_def(output), style_opt(output)))
  
}
util_print.simdesign <- function(x, ...){
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red
  
  cat(style_heading(paste0("\n", "   SIMDESIGN   ", "\n")))
  
  cat("Simulation method      = ")
  output <- paste0(x@simmethod, "\n")
  cat(ifelse(!identical(x@simmethod, character(0)), style_def(output), style_na(output)))
  
  cat("Simulation object      = ")
  output <- paste0(x@simobject, "\n")
  cat(ifelse(length(x@simmethod) > 0, style_def(output), style_opt(output)))
  
  cat("Generated random seeds = ")
  output <- paste0(paste(x@simseeds, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@simseeds)), style_def(output), style_opt(output)))
  
  cat(paste0("\n", "Parameter matrix (input)", "\n"))
  print(x@siminput, width = Inf)
  
  cat(paste0("\n", "Simulation results (output)", "\n"))
  print(x@simoutput, width = Inf)
  
}
######################################
################END PRINT FUNCTION####


#######################INFO TAB###########
##############################
nldoc_read_nlogo <- function(modelfiles)
{
  # Its possible to select multiple files at once.
  # We have to remove GUI information from the files and bind them together:
  modelcode <- ""
  guicode <- NA
  
  for (i in 1:length(modelfiles))
  {
    # Open the netlogo file and store it in a string vector
    completecode.i <- readLines(modelfiles[i], warn=FALSE)
    
    # Find the line in the NetLogoCode where the interface definiton starts (separator: @#$#@#$#@)
    separator <- grep("@#$#@#$#@", completecode.i, fixed=TRUE)
    
    # Remove all interface definitions from the string vector
    if (length(separator) > 0)
    {
      # 0: sep[1] = modelcode
      modelcode.i <- completecode.i[1:(separator[1] - 1)]
      # 1st sep - 2nd sep = guicode
      guicode <- completecode.i[(separator[1] + 1):(separator[2] - 1)]
      # 2nd sep to 3rd sep = infotab:
      infotabcode <- completecode.i[(separator[2] + 1):(separator[3] - 1)]
      # 3rd sep to 4th sep = shapes
      # 4th sep to 5th sep = netlogo version
      nlversion <- completecode.i[(separator[4] + 1):(separator[5] - 1)]
      # 7th sep to 8th sep = bscode
      bscode <- completecode.i[(separator[7] + 1):(separator[8] - 1)]
    } else {
      # If there are no sepearators in the file, we have a nls file and just take everything as modelcode:
      modelcode.i <- completecode.i
    }
    
    # Bind together in modelcode vector:
    modelcode <- c(modelcode, "\n", modelcode.i)
  }
  
  nlogocode <- list(modelcode=modelcode,
                    guicode=guicode,
                    infotabcode=infotabcode,
                    nlversion=nlversion,
                    bscode=bscode)
  
  return(nlogocode)
}
##################################end info tab