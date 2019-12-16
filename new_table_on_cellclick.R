
# Text output informing the user that clicking within a specified cell range of the table 
# will result in another table, with details about cluster numbers and sizes (those that 
# make up the proportions first selected.)
output$click_cells <- renderText({
  validate(need(!is.null(inp$data), ""))
  blurb(type = "ClickCell")
})

# On cell click of the main table, outputs a mini table of clusters and sizes associated 
# with the number selected in the main table
output$cluster_info <- renderDT({
  validate(need(!is.null(user$tbl), ""), need(length(input$num_clusters_cell_clicked)>0, ""))
  
  clicked <- input$num_clusters_cell_clicked
  if (clicked$col %in% c(2:6)) {
    cell$col <- clicked$col + 1
    cell$row <- clicked$row
  }
  h <- user$tbl$Heights[cell$row]
  
  if (clicked$col %in% c(3,4)) {
    user$lim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()  
  }else if (clicked$col %in% c(5,6)) {
    user$nonlim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()
  }else if (clicked$col == 2) {
    inp$data %>% pull(h) %>% table() %>% as.data.frame() %>% 
      filter(Freq >= inp$minC) %>% set_colnames(c("Clusters","Size")) %>% asDT()
  }
})