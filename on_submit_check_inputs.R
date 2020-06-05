
cl_data <- reactive({
  # Invalid filetype (only accepts tsv/txt):
  validate(need(tools::file_ext(values$path) %in% c("tsv","txt"), errMsg(5)))
  readData(values$path) %>% as_tibble()         # Threshold data
})

dataObserver <- reactive({
  paste0(input$data$datapath, input$metadata$datapath)
})

observe({
  x <- dataObserver()
  
  shinyjs::useShinyjs()
  shinyjs::hide("submit")       # hide submit button when the loaded file changes
  inp$data <- NULL              # reset inputs relying on inp$data
  user$results <- NULL
  values$prelim <- values$md <- NULL
  values$path <- input$data$datapath
  
})

output$check_input <- renderText({
  req(values$path)
  df <- cl_data()
  df[df==""] <- "Unknown"
  
  validate(
    # There are one or more empty cells in the dataset:
    need(!any(is.na(df)), errMsg(6)),
    # Column names are not unique
    need(length(colnames(df)) == length(unique(colnames(df))), errMsg(9))
  )
  errMsg(0)     # Input data formatted correctly
})

output$check_md <- renderText({
  req(values$path, input$metadata$datapath)     # loaded datafiles by user
  df <- cl_data()
  validate(need(tools::file_ext(input$metadata$datapath) %in% c("tsv","txt"), errMsg(5)))
  
  md <- readData(input$metadata$datapath)
  # Column names are unique:
  validate(need(length(colnames(md)) == length(unique(colnames(md))), errMsg(9)))
  
  inds_in_both <- intersect(pull(md,1), pull(df,1))
  # Contains all the id values in the cluster file (at least them):
  validate(need(length(inds_in_both) >= nrow(df), errMsg(8)))
  
  md <- md[md %>% pull(1) %in% inds_in_both,]
  md[md==NA] <- md[md==""] <- "Unknown"
  values$prelim <- md
  
  errMsg(0)
})

output$metadataColumnUI <- renderUI({
  req(values$prelim)
  selectizeInput("colOfInt", "Column of interest: ", 
              choices = colnames(values$prelim)[-1], 
              options = list(
                placeholder = "Please select a column", 
                onInitialize = I('function() { this.setValue(""); }')
              ))
})

observe({
  req(values$prelim, input$colOfInt)
  values$removed_md <- NULL
  colVar <- dplyr::sym(input$colOfInt)
  if ("Unknown" %in% pull(values$prelim, input$colOfInt) %>% unique()) {
    values$removed_md <- values$prelim %>% filter( !!(colVar) == "Unknown") %>% nrow()
    values$md <- values$prelim %>% filter(!!(colVar) != "Unknown")
  }else {
    values$removed_md <- 0
    values$md <- values$prelim
  }
})

output$posValUI <- renderUI({
  req(values$md, input$colOfInt)
  user$bin <- pull(values$md, input$colOfInt) %>% unique()
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
    if (length(negSelect) <= 15) {
      category2 <- paste0(negSelect, collapse = ",")
    }else {
      category2 <- paste0(length(negSelect), " elements in second group.")
    }
    paste0("\nEverything else: ", category2)
  }
})

observe({
  req(user$bin, input$posV)
  user$pvals <- input$posV
  user$nvals <- setdiff(user$bin, input$posV)
  inp$data <- NULL
})

# On submit-click, user input data is checked for validity: valid column names, file extension, etc. 
# (ID | Binary Variable | Thresholds...)
observeEvent(input$submit, {
  
  withProgress(
    message = "Submitting and processing data: ", 
    value = 0, {
      
      orig_df <- readData(values$path)
      inp$minC <- ifelse(is.na(input$minC), 0, input$minC)
      
      incProgress(1/4)
      
      user$pos <- toString(user$pvals)
      user$neg <- toString(user$nvals)
      
      incProgress(1/4)
      
      values$locus <- input$colOfInt %>% dplyr::sym() %>% as.character()
      df <- values$md %>% select(colnames(values$md)[1], !!(dplyr::sym(input$colOfInt))) %>% 
        left_join(., orig_df)
      
      incProgress(1/4)
      
      df[,values$locus] <- df %>% pull(values$locus) %>% 
        replace(. %in% user$pvals, user$pos)
      df[,values$locus] <- df %>% pull(values$locus) %>% 
        replace(. %in% user$nvals, user$neg)
      
      incProgress(1/4)
      
      basic$metadata <- values$md
      inp$data <- df
    }
  )
})

# Basic metrics of what the data looks like: the proportions of binary data, number of thresholds, ...
output$base_metrics <- renderText({
  req(inp$data, user$pos, user$neg, input$colOfInt)
  
  a1 <- binaryStats(inp$data[,input$colOfInt], user$pos, user$neg)
  ap <- a1 %>% filter(Type == "Positive")
  an <- a1 %>% filter(Type == "Negative")

  lim <- which.min(a1$Freq)
  inp$limiting <- a1$Bin[lim]
  
  user$lim <- a1$Bin[lim] %>% filterPerfect(inp$data, ., inp$minC, c(0,1), values$locus)
  user$nonlim <- inp$data %>% pull(2) %>% setdiff(., a1$Bin[lim]) %>%
    filterPerfect(inp$data, ., inp$minC, c(0,1), values$locus)
  print(2)
  basic$data <- paste0(
    "Dataset size: ", nrow(inp$data), " samples\n", 
    "Number of heights: ", ncol(inp$data)-2, "\n", 
    "Proportions of binary variable: \n    ", 
    ap$Bin, ": ", ap$Freq, "/", ap$Tot, " = ", ap$percent, "\n    ", 
    an$Bin, ": ", an$Freq, "/", an$Tot, " = ", an$percent, "\n", 
    "Limiting factor: \"", inp$limiting, "\"\n", 
    "We will be maximizing the proportion found \nin binary clusters for the limiting factor.")
})

output$removed_rows <- renderText({
  req(inp$data, values$removed_md, values$prelim, input$colOfInt, values$md)
  paste0("Original dataset size: ", nrow(values$prelim), "\n", 
         "There are ", values$removed_md, " metadata rows with empty fields \n", 
         "in column \"", input$colOfInt, "\"\n", 
         "Size of dataset to be used in the analysis: ", nrow(values$md))
})