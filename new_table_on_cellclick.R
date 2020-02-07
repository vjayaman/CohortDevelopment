
# Text output informing the user that clicking within a specified cell range of the table 
# will result in another table, with details about cluster numbers and sizes (those that 
# make up the proportions first selected.)

output$click_cells <- renderUI({
  req(inp$data)
  box(width = 12, blurb(type = "ClickCell"))
})

# On cell click of the main table, outputs a mini table of clusters and sizes associated 
# with the number selected in the main table
output$cluster_info <- renderDT({
  req(inp$data, user$tbl, length(input$num_clusters_cell_clicked)>0)
  
  clicked <- input$num_clusters_cell_clicked
  if (clicked$col %in% c(2:6)) {
    cell$col <- clicked$col + 1         # cell$row/col is NULL otherwise
    cell$row <- clicked$row
  }
  h <- user$tbl$Heights[cell$row]

  if (clicked$col %in% c(3,4)) {        # column of limiting factor selected
    user$lim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()
    
  }else if (clicked$col %in% c(5,6)) {  # column of nonlimiting factor selected
    user$nonlim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()
    
  }else if (clicked$col == 2) {         # number of clusters with size >= minC
    inp$data %>% pull(h) %>% table() %>% as.data.frame() %>% 
      filter(Freq >= inp$minC) %>% 
      set_colnames(c("Clusters","Size")) %>% asDT()
  }
})

# mini explanation - click on a row of the results table to get specifics about the indicated clusters
output$click_results <- renderUI({
  req(user$results)
  tagList(
    downloadButton("dnld_results", "Download table"), 
    blurb(type = "ClickRow"))
})

# On row click of the final results table, outputs a table of the clusters and specific info at that height
output$final <- renderDT({
  req(user$results, inp$minC, length(input$results_rows_selected)==1, values$locus)
  
  withProgress(message = "Generating table of information for selected row: ", value = 0, {
    
    incProgress(1/4, detail = "Collecting input data")
    rowX <- user$results[input$results_rows_selected,]    # the selected row
    lim <- inp$limiting
    h <- rowX$Height
    df <- inp$data
    x <- c(percLhs()/100, percRhs()/100, stepLhs(), stepRhs())  
    
    incProgress(1/4, detail = "Extracting counts")
    cl.calls <- df %>% select(colnames(df)[1], values$locus, h) %>% set_colnames(c("id","locus","clusters"))
    
    incProgress(1/4, detail = "Filtering data")
    mets <- cl.calls$clusters %>% unique() %>%
      lapply(., function(cluster) alleleBinCounts(cl.calls, cluster, lim)) %>% bind_rows()
    
    tot.size <- sum(mets$size)
    key.cl <- mets %>% filter(size >= inp$minC)
    th <- homogeneityTypes(x[1], x[2], x[3], x[4])
    cl_tbl <- homogeneityTables(th, key.cl)
    b <- th %>% filter(val %in% rowX[,c("Positive threshold", "Negative threshold")]) %>% as_tibble()
    b$val <- b$val %>% as.character()
    
    incProgress(1/5, detail = "Formatting table")
    user$final <- toshow <- cl_tbl[b$val] %>% bind_rows() %>% select(1,3,2,4,5,6) %>% 
      set_colnames(c("Cluster","Limiting factor","Size","Proportion","As decimal","Homogeneity type"))
    cnames <- colnames(toshow)
    # factor columns so table filtering will be with selectize, not sliders
    toshow[cnames] <- lapply(toshow[cnames], as.factor)
    
    asDT(toshow, filter_opt = "top") %>%
      formatRound(columns = 5, digits = 4) %>%
      formatStyle('Homogeneity type', target = 'row',
                  backgroundColor = styleEqual(c('Positive','Negative'), c('lightblue','white')))  
  })
})

# Download button: saves datatable info as "Homogeneity-both-<year>-<month>-<day>-<hours>-<minutes>.txt"
output$dnld_final <- downloadHandler(
  filename = paste0("Specific-clusters-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$final, file, sep = "\t", quote = FALSE, row.names = FALSE)
})
