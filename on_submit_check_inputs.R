
observeEvent(input$check_validity, {
  req(input$data)               # loaded data file by user
  
  shinyjs::useShinyjs()
  shinyjs::hide("submit")       # hide submit button when the loaded file changes
  inp$data <- NULL              # reset inputs relying on inp$data
  user$results <- NULL
  
  values$path <- input$data$datapath
  inp$minC <- input$minC
  
  output$check_input <- renderText({
    if (tools::file_ext(values$path) %in% c("tsv","txt")) {
      df <- readData(values$path)
      validate(
        # All heights must be numeric: 
        need(all(varhandle::check.numeric(colnames(df)[-1][-1])), errMsg(1)), 
        
        # The second column should have a non-numeric locus name:
        need(!varhandle::check.numeric(colnames(df)[2]), errMsg(2)), 
        
        # The second column data should be binary
        # need(length(unique(pull(df,2)))==2, errMsg(4)), 
        
        # The first column should be a list of genomes, with a non-numeric heading
        need(!varhandle::check.numeric(colnames(df)[1]), errMsg(3)), 
        
        # There are one or more empty cells in the dataset.
        need(!any(is.na(df)), errMsg(6)), 
        
        # Minimum cluster size not specified.
        need(is.numeric(inp$minC), errMsg(7)))
      
      # Input data formatted correctly
      errMsg(0)
    }else {
      # Invalid filetype (onlyaccepts tsv/txt)
      errMsg(5)
    }
  })
  user$bin <- readData(values$path) %>% pull(2) %>% unique()
})

output$posValUI <- renderUI({
  req(user$bin)
  tagList(
    selectizeInput("posV", "Variable of interest: ", choices = user$bin, multiple = TRUE), 
    textOutput("negV"), tags$br(), 
    shinyjs::show("submit")
  )
})

output$negV <- renderText({
  negSelect <- setdiff(user$bin, input$posV)
  if (length(negSelect) == 0 | length(input$posV) == 0) {
    shinyjs::hide("submit")
    paste0("\nYou need at least one element of each category to continue.")
  }else {
    shinyjs::show("submit")
    paste0("\nEverything else: ", paste0(negSelect, collapse = ","))
  }
})

observe({
  req(user$bin, input$posV)
  user$pvals <- input$posV
  user$nvals <- setdiff(user$bin, input$posV)
  inp$data <- NULL
})

# On submit-click, user input data is check for validity: valid column names, file extension, etc. 
# (ID | Binary Variable | Thresholds...)
observeEvent(input$submit, {
  df <- readData(values$path)
  values$locus <- colnames(df)[2]
  user$pos <- toString(user$pvals)
  user$neg <- toString(user$nvals)
  df[,values$locus] <- pull(df, 2) %>% replace(. %in% user$pvals, user$pos)
  df[,values$locus] <- pull(df, 2) %>% replace(. %in% user$nvals, user$neg)
  inp$data <- df
})

# Basic metrics of what the data looks like: the proportions of binary data, number of thresholds, ...
output$base_metrics <- renderText({
  req(inp$data, user$pos, user$neg)
  a1 <- binaryStats(inp$data[,2], user$pos, user$neg)
  ap <- a1 %>% filter(Type == "Positive")
  an <- a1 %>% filter(Type == "Negative")
  
  lim <- which.min(a1$Freq)
  inp$limiting <- a1$Bin[lim]
  
  user$lim <- a1$Bin[lim] %>% filterPerfect(inp$data, ., inp$minC, c(0,1), values$locus)
  user$nonlim <- inp$data %>% pull(2) %>% setdiff(., a1$Bin[lim]) %>%
    filterPerfect(inp$data, ., inp$minC, c(0,1), values$locus)
  
  basic$data <- paste0(
    "Dataset size: ", nrow(inp$data), " samples\n", 
    "Number of heights: ", ncol(inp$data)-2, "\n", 
    "Proportions of binary variable: \n    ", 
    ap$Bin, ": ", ap$Freq, "/", ap$Tot, " = ", ap$percent, "\n    ", 
    an$Bin, ": ", an$Freq, "/", an$Tot, " = ", an$percent, "\n", 
    "Limiting factor: \"", inp$limiting, "\"\n", 
    "We will be maximizing the proportion found \nin binary clusters for the limiting factor.")
})

