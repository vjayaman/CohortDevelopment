
observeEvent(input$check_validity, {
  req(input$data)               # loaded data file by user
  
  shinyjs::useShinyjs()
  shinyjs::hide("submit")       # hide submit button when the loaded file changes
  inp$data <- NULL              # reset inputs relying on inp$data
  
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
        need(length(unique(pull(df,2)))==2, errMsg(4)), 
        
        # The first column should be a list of genomes, with a non-numeric heading
        need(!varhandle::check.numeric(colnames(df)[1]), errMsg(3)), 
        
        # There are one or more empty cells in the dataset.
        need(!any(is.na(df)), errMsg(6)), 
        
        # Minimum cluster size not specified.
        need(is.numeric(inp$minC), errMsg(7)))
      
      errMsg(0)            # Input data formatted correctly
    }else {
      errMsg(5)            # Invalid filetype (onlyaccepts tsv/txt)
    }
  })
  user$bin <- readData(values$path) %>% pull(2) %>% unique()
  user$pos <- user$bin[1]
  user$neg <- user$bin[2]
})

output$posValUI <- renderUI({
  req(user$bin, length(user$bin)==2)
  selectizeInput("posV", "Positive value: ", choices = user$bin, selected = setdiff(user$bin, user$neg))
})

output$negValUI <- renderUI({
  req(user$bin, length(user$bin)==2)
  tagList(
    selectizeInput("negV", "Negative value", choices = user$bin, selected = setdiff(user$bin, user$pos)), 
    # When data has been loaded and validated, enables the "Submit" button
    shinyjs::show("submit"))
})

observe({
  req(input$posV, input$negV)
  user$pos <- input$posV
  user$neg <- input$negV
  inp$data <- NULL
})

# On submit-click, user input data is check for validity: valid column names, file extension, etc. 
# (ID | Binary Variable | Thresholds...)
observeEvent(input$submit, {
  df <- readData(values$path)
  values$locus <- colnames(df)[2]
  colnames(df)[2] <- "Source"
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
  
  user$lim <- a1$Bin[lim] %>% filterPerfect(inp$data, ., inp$minC, c(0,1))
  user$nonlim <- inp$data %>% pull(2) %>% setdiff(., a1$Bin[lim]) %>%
    filterPerfect(inp$data, ., inp$minC, c(0,1))
  
  paste0("Dataset size: ", nrow(inp$data), " samples\n", "Number of heights: ", ncol(inp$data)-2, "\n", 
         "Proportion of binary variable: \n    ", 
         ap$Type, " (", ap$Bin, "): ", ap$Freq, "/", ap$Tot, " = ", ap$percent, "\n    ", 
         an$Type, " (", an$Bin, "): ", an$Freq, "/", an$Tot, " = ", an$percent, "\n", 
         "Limiting factor: ", a1$Type[lim], " (", inp$limiting, ")\n", 
         "We will be maximizing the proportion found \nin binary clusters for the limiting factor.")
})

