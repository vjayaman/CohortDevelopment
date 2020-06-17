
# Text output informing the user that clicking within a specified cell range of the table 
# will result in another table, with details about cluster numbers and sizes (those that 
# make up the proportions first selected.)

output$click_cells <- renderUI({
  req(inp$data)
  blurb(type = "ClickCell")
})

# On cell click of the main table, outputs a mini table of clusters and sizes associated 
# with the number selected in the main table
output$cluster_info <- renderDT({
  req(inp$data, user$tbl, length(input$num_clusters_cell_clicked)>0, user$lim)
  clicked <- input$num_clusters_cell_clicked
  h <- user$tbl$Heights[clicked$row]
  
  if (clicked$col %in% 0:1) {
    a <- inp$data %>% pull(h) %>% table() %>% as_tibble() %>% 
      set_colnames(c("Cluster number","Size"))
    a[order(a$Size, decreasing = TRUE),] %>% asDT()
    
  }else if (clicked$col == 2) {
    inp$data %>% pull(h) %>% table() %>% as_tibble() %>% 
      filter(n >= inp$minC) %>% set_colnames(c("Cluster number","Size")) %>% asDT()
    
  }else {
    cnames <- user$lim[[h]] %>% ungroup() %>% colnames()
    user$lim[[h]] %>% ungroup() %>% select(cnames[1],cnames[4],cnames[2]) %>% 
      set_colnames(c("Cluster number", "Size", "100% ___")) %>% asDT()
  }
})

output$all_h_and_cl <- renderDT({
  req(user$results, inp$data, values$locus, inp$limiting, inp$minC)
  
  heights <- user$results$Height %>% unique()
  x <- c(percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
  
  withProgress(message = "Collecting results, for all thresholds and corresponding clusters", 
               value = 0, {
                 user$full_tbl <- lapply(1:length(heights), function(i) {
                   incProgress(1/length(heights), 
                               detail = paste0(round(i/length(heights), digits = 2)*100, "% done"))
                   
                   h <- heights[i]
                   cl.calls <- inp$cl.calls[[h]]
                   mets <- inp$mets[[h]]
                   key.cl <- mets %>% filter(size >= inp$minC)
                   
                   sigClustersOneH(inp$data, values$locus, h, inp$limiting, x, user$results, key.cl)
                 }) %>% bind_rows()
               })
  
  DT::datatable(user$full_tbl, rownames = FALSE, filter = "top", 
                options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                               dom = "ti", pageLength = nrow(user$full_tbl), scrollY = "500px"), 
                selection = list(mode = 'multiple', target = 'row'), caption = "Table 4") %>% 
    formatRound(columns = c(6,11), digits = 4)
})

output$all_h_ui <- renderUI({
  req(user$full_tbl)
  tagList(downloadButton("dnld_all_heights", "Download Table 4"), tags$hr())
})

output$dnld_all_heights <- downloadHandler(
  filename = paste0("Clusters-specifics-all-heights", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$full_tbl, file, sep = "\t", quote = FALSE, row.names = FALSE)
  })

# On row click of the final results table, outputs a table of the clusters and specific info at that height
output$final <- renderDT({
  req(user$results, inp$minC, length(input$results_rows_selected)==1, values$locus)
  withProgress(message = "Generating table of information for selected row: ", value = 0, {
    
    incProgress(1/4, detail = "Collecting input data")
    rowX <- user$results[input$results_rows_selected,]    # the selected row
    lim <- inp$limiting
    h <- rowX$Height %>% as.character()
    df <- inp$data
    x <- c(percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
    
    incProgress(1/4, detail = "Extracting counts")
    cl.calls <- inp$cl.calls[[h]]
    
    incProgress(1/4, detail = "Filtering data")
    mets <- inp$mets[[h]]
    
    tot.size <- sum(mets$size)
    key.cl <- mets %>% filter(size >= inp$minC)
    th <- homogeneityTypes(x[1], x[2], x[3], x[4])
    cl_tbl <- homogeneityTables(th, key.cl)
    b <- th %>% filter(val %in% rowX[,c("Positive threshold", "Negative threshold")]) %>% as_tibble()
    b$val <- b$val %>% as.character()
    
    incProgress(1/4, detail = "Formatting table")
    user$final <- toshow <- cl_tbl[b$val] %>% bind_rows() %>% select(1,3,2,4,5,6) %>% 
      set_colnames(c("Cluster","Limiting factor","Size","Proportion","As decimal","Homogeneity type"))
    
    cnames <- colnames(toshow)
    # factor columns so table filtering will be with selectize, not sliders
    toshow[cnames] <- lapply(toshow[cnames], as.factor)
    
    DT::datatable(toshow, rownames = FALSE, filter = "top", 
                  options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                 dom = "ti", pageLength = nrow(toshow), scrollY = "500px"), 
                  selection = list(mode = 'multiple', target = 'row'), 
                  caption = paste0("Table 2: Select a row in the cluster table below to see the ", 
                                   "metadata associated with each cluster.")) %>% 
      formatRound(columns = 5, digits = 4) %>%
      formatStyle('Homogeneity type', target = 'row',
                  backgroundColor = styleEqual(c('Positive','Negative'), c('lightblue','white')))
  })
})

output$metadata_exp <- renderUI({
  req(!is.null(basic$metadata), user$results)
  tagList(downloadButton("dnld_results", "Download Table 1"), tags$hr())
})

output$selected_clusters <- renderDT({
  req(user$final, length(input$final_rows_selected)>0, !is.null(basic$metadata), 
      user$results, length(input$results_rows_selected)>0)
  
  md <- basic$metadata
  h <- user$results$Height[input$results_rows_selected]    # the selected row
  
  dfx <- inp$data %>% select(1, all_of(h)) %>% 
    filter(., pull(inp$data, h) %in% user$final$Cluster[input$final_rows_selected])
  
  sample_info <- merge(dfx, md, by.x = colnames(dfx)[1], 
                       by.y = colnames(md)[1], all.x = TRUE) %>% as_tibble()
  
  cols <- colnames(sample_info)
  sample_info[cols] <- lapply(sample_info[cols], as.factor)
  
  sample_info %>% DT::datatable(., rownames = FALSE, filter = "top", caption = "Table 3", 
                                options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                               dom = "tif", pageLength = nrow(sample_info), scrollY = "500px"))
})

# Download button: saves datatable info as "Homogeneity-both-<year>-<month>-<day>-<hours>-<minutes>.txt"
output$dnld_final <- downloadHandler(
  filename = paste0("Specific-clusters-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$final, file, sep = "\t", quote = FALSE, row.names = FALSE)
})
