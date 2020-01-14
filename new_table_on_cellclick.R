
# Text output informing the user that clicking within a specified cell range of the table 
# will result in another table, with details about cluster numbers and sizes (those that 
# make up the proportions first selected.)

output$click_cells <- renderUI({
  validate(need(!is.null(inp$data), ""))
  box(width = 12, blurb(type = "ClickCell"))
})

# On cell click of the main table, outputs a mini table of clusters and sizes associated 
# with the number selected in the main table
output$cluster_info <- renderDT({
  validate(need(!is.null(user$tbl), ""), need(length(input$num_clusters_cell_clicked)>0, ""))
  
  clicked <- input$num_clusters_cell_clicked
  if (clicked$col %in% c(2:6)) {
    cell$col <- clicked$col + 1         # cell$row/col is NULL otherwise
    cell$row <- clicked$row
  }
  h <- user$tbl$Heights[cell$row]

  if (clicked$col %in% c(3,4)) {
    # column of limiting factor selected
    user$lim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()
    
  }else if (clicked$col %in% c(5,6)) {
    # column of nonlimiting factor selected
    user$nonlim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()
    
  }else if (clicked$col == 2) {
    # number of clusters with size >= minC
    inp$data %>% pull(h) %>% table() %>% as.data.frame() %>% 
      filter(Freq >= inp$minC) %>% 
      set_colnames(c("Clusters","Size")) %>% asDT()
  }
})

# mini explanation - click on a row of the results table to get specifics about the indicated clusters
output$click_results <- renderUI({
  validate(need(!is.null(user$results), ""))
  # fluidRow(column(width = 10, offset = 1, box(width = 12, uiOutput("click_results"))))
  tagList(
    downloadButton("dnld_results", "Download table"), 
    blurb(type = "ClickRow"))
})

# On row click of the final results table, outputs a table of the clusters and specific info at that height
output$final <- renderDT({
  validate(need(!is.null(user$results), ""), need(length(input$results_rows_selected)==1, ""), 
           need(!is.null(inp$minC), ""))
  
  rowX <- user$results[input$results_rows_selected,]    # the selected row
  lim <- values$lim
  
  # | Clusters | Source | Size | Fraction of cluster with source | Decimal | Type (pos/neg)
  df <- inp$data %>% perfClusters(., rowX$Height) %>% ungroup()
  df$charFrac <- paste0(df$Freq, "/", df$Size)
  df <- df %>% select(Clusters, Source, Size, charFrac, Fraction) %>% filter(Size >= inp$minC)
  df$Type <- NA
  
  # When cluster does not have any of the limiting factor, the row only appears for the nonlimiting 
  # factor, as a 100% case. In addition, would like to see 0% for the limiting factor
  tmp <- df[(df$Source != lim) & (df$Fraction == 1),]
  tmp$Source <- lim
  tmp$Fraction <- 0
  tmp$charFrac <- paste0(tmp$Fraction, "/", tmp$Size)
  
  # | Clusters | Limiting factor | Cluster size | Prop. of cluster with limiting factor | Decimal | Type
  df <- rbind(df, tmp) %>% filter(Source == lim)
  
  df$Type[df$Fraction <= rowX$`Negative threshold`] <- "Negative"
  df$Type[df$Fraction >= rowX$`Positive threshold`] <- "Positive"
  
  user$final <- toshow <- df %>% filter(!is.na(Type)) %>% 
    set_colnames(c("Cluster","Limiting factor","Cluster Size","Proportion","As decimal","Homogeneity type"))
  cnames <- colnames(toshow)
  
  # factor columns so table filtering will be with selectize, not sliders
  toshow[cnames] <- lapply(toshow[cnames], as.factor)

  asDT(toshow, filter_opt = "top") %>% 
    formatRound(columns = 5, digits = 4) %>% 
    formatStyle('Homogeneity type', target = 'row', 
                backgroundColor = styleEqual(c('Positive','Negative'), c('lightblue','white'))
    )
})

# Download button: saves datatable info as "Homogeneity-both-<year>-<month>-<day>-<hours>-<minutes>.txt"
output$dnld_final <- downloadHandler(
  filename = paste0("Specific-clusters-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$final, file, sep = "\t", quote = FALSE, row.names = FALSE)
})
