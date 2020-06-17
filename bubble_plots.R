output$select_height <- renderUI({
  req(inp$data, user$initial, user$ptype, best$heights)
  tagList(
    box(width = 12, paste0("The best heights, by the 100%, 0% homogeneity (respectively), are ", 
                           toString(best$heights), ". Select a height in the input menu below ", 
                           "to see the composition of the clusters that make up the above ", 
                           "proportions and cluster numbers.")), 
    selectInput("height", "Select height", choices = user$initial$h %>% unique(), 
                selected = as.character(best$heights[2])), 
    actionButton("specific_h", "Submit"), tags$br(), tags$br()
  )
})

observeEvent(input$specific_h, {
  req(inp$data, user$results, input$height, values$locus)
  
  h <- input$height
  plots$bubble_title <- paste0("Clusters used to calculate the selected proportion of ",
                               "limiting factor in each cluster, at height ", h)
  
  neg_h <- seq(0, percLhs()/100, by = stepLhs()) %>% rev()
  pos_h <- seq(percRhs()/100, 1, by = stepRhs()) %>% rev()
  
  df <- inp$data %>% select(all_of(h), all_of(values$locus)) %>% group_by_all() %>% count() %>% 
    set_colnames(c("Clusters", values$locus, "Count")) %>% ungroup()
  
  homogeneous <- table(df$Clusters) %>% as.data.frame() %>% filter(Freq == 1) %>% pull(Var1)
  tmp <- df %>% filter(Clusters %in% homogeneous)
  tmp[,values$locus] <- tmp %>% pull(values$locus) %>% 
    lapply(., function(x) setdiff(inp$data %>% pull(values$locus) %>% unique(), x)) %>% unlist()
  tmp$Count <- 0
  df <- bind_rows(df, tmp)
  df$Clusters <- as.character(df$Clusters)
  
  csizes <- inp$data %>% select(all_of(h)) %>% table() %>% as.data.frame() %>% as_tibble() %>% 
    set_colnames(c("Clusters","Size"))
  csizes$Clusters <- as.character(csizes$Clusters)
  
  df <- left_join(df, csizes, by = "Clusters")
  
  df$Prop <- df$Count/df$Size
  df <- df %>% filter(Size >= inp$minC)
  df$NegFraction <- df$PosFraction <- 0
  
  for (i in neg_h) {df$NegFraction[which(df$Prop <= i)] <- i}
  for (i in rev(pos_h)) {df$PosFraction[which(df$Prop >= i)] <- i}
  df <- df %>% filter(., pull(df, values$locus) %in% inp$limiting)
  
  df$PosPercent <- df$PosFraction %>% percent()
  df$NegPercent <- df$NegFraction %>% percent()
  df$PosPercent <- factor(df$PosPercent, levels = df$PosPercent[order(df$PosFraction, decreasing = TRUE)] %>% unique())
  df$NegPercent <- factor(df$NegPercent, levels = df$NegPercent[order(df$NegFraction, decreasing = FALSE)] %>% unique())
  
  df$Percent <- as.character(df$PosPercent)
  df$Percent[df$NegPercent != 0] <- as.character(df$NegPercent[!is.na(df$NegPercent)])
  # df$Percent[!is.na(df$NegPercent)] <- as.character(df$NegPercent[!is.na(df$NegPercent)])
  df <- df %>% filter(Percent != "0%")
  df$Percent <- factor(df$Percent, levels = df$Percent[order(df$Prop, decreasing = TRUE)] %>% unique())
  
  plots$bubble_data <- df
})

output$bubble_plot <- renderPlot({
  req(inp$data, user$results, plots$bubble_data, plots$bubble_title, inp$limiting)
  # sequence going from 0 to left hand side boundary
  neg_h <- seq(0, percLhs()/100, by = stepLhs()) %>% rev()
  pos_h <- seq(percRhs()/100, 1, by = stepRhs()) %>% rev()
  
  plots$neg_color <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
                       "#4292c6", "#2171b5", "#08519c", "#08306b") %>% rev() %>% colorRampPalette(.)
  plots$pos_color <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a",
                       "#ef3b2c", "#cb181d", "#a50f15", "#67000d") %>% rev() %>% colorRampPalette(.)
  
  df <- plots$bubble_data
  df_pos <- df[!is.na(df$PosFraction),]
  df_neg <- df[!is.na(df$NegFraction),]
  
  n1 <- df_neg %>% pull(NegPercent) %>% unique() %>% length()
  p1 <- df_pos %>% pull(PosPercent) %>% unique() %>% length()
  
  ptitle <- paste0("Positive and negative cluster homogeneity", "\n(red = >= y %, blue = <= y %) ", 
                   "of the cluster has ", tolower(inp$limiting))
  
  suppressWarnings(
    ggplot(mapping = aes(x = Clusters, y = Prop, size = Size)) + 
      xlab("Cluster names") + ylab("Percent of cluster") + ggtitle(ptitle) + 
      (geom_point(data = df_pos, aes(c1 = PosPercent)) %>% 
         rename_geom_aes(new_aes = c("colour" = "c1"))) + 
      scale_manual("c1", plots$pos_color(p1), "Positive") + 
      (geom_point(data = df_neg, aes(c2 = NegPercent)) %>% 
         rename_geom_aes(new_aes = c("colour" = "c2"))) + 
      scale_manual("c2", plots$neg_color(n1), "Negative") + 
      scale_y_continuous(limits = c(0,1)) + 
      scale_size_continuous(range = c(2,7)) + 
      theme(strip.text.y = element_text(margin = margin(0,2,0,2)), 
            strip.text = element_text(size = 14), axis.text.y = element_text(size = 13), 
            axis.text.x = element_blank(), title = element_text(size = 14), 
            axis.ticks.x = element_blank(), legend.text = element_text(size = 12))
  )
})

observe({
  req(inp$data, user$results, plots$bubble_data)
  df_pos <- plots$bubble_data %>% filter(!is.na(PosFraction))
  df_neg <- plots$bubble_data %>% filter(!is.na(NegFraction))
  
  plots$pos_data <- plots$neg_data <- ""
  
  if (nrow(df_pos) > 0) {
    shinyjs::show(id = "positive_bubble")
  }else {
    plots$pos_data <- paste0("No clusters are such that >= y % of the cluster has ", inp$limiting)
    shinyjs::hide(id = "positive_bubble")
  }
  
  if (nrow(df_neg) > 0) {
    shinyjs::show(id = "negative_bubble")
  }else {
    plots$neg_data <- paste0("Negative cluster homogeneity <= y % of the cluster has ", inp$limiting)
    shinyjs::hide(id = "negative_bubble")
  }
  
  if ( (nrow(df_neg) > 0) & (nrow(df_pos) > 0) ) {
    shinyjs::hide(id = "no_bubble_data")
  }else {
    shinyjs::show(id = "no_bubble_data")
  }
})

output$no_bubble_data <- renderText({
  req(plots$pos_data, plots$neg_data)
  paste0(plots$pos_data, "\n", plots$neg_data)
})

output$positive_bubble <- renderPlotly({
  req(inp$data, user$results, plots$bubble_data, plots$pos_color)
  df_pos <- plots$bubble_data %>% filter(!is.na(PosFraction)) %>% select(Clusters,Size,Prop,PosPercent)
  
  colnames(df_pos) <- c("Cluster names","Cluster size","Percent of cluster","Percent interval")
  p1 <- df_pos %>% pull(`Percent interval`) %>% unique() %>% length()
  
  {ggplot(df_pos, aes(x = `Cluster names`, y = `Percent of cluster`, color = `Percent interval`)) + 
      geom_point() + geom_point(aes(size = `Cluster size`), show.legend = FALSE) + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
      ggtitle(paste0("Positive cluster homogeneity (>= y % of the cluster has ",inp$limiting, ")")) + 
      scale_color_manual(values = plots$pos_color(p1), name = "Percent\ninterval")} %>% 
    ggplotly()
})


output$negative_bubble <- renderPlotly({
  req(inp$data, user$results, plots$bubble_data, plots$neg_color)
  
  df_neg <- plots$bubble_data %>% filter(!is.na(NegFraction)) %>% select(Clusters,Size,Prop,NegPercent) %>% 
    set_colnames(c("Cluster names","Cluster size","Percent of cluster","Percent interval"))
  n1 <- df_neg %>% pull(`Percent interval`) %>% unique() %>% length()
  
  {ggplot(df_neg, aes(x = `Cluster names`, y = `Percent of cluster`, color = `Percent interval`)) + 
      geom_point() + geom_point(aes(size = `Cluster size`), show.legend = FALSE) + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
      ggtitle(paste0("Negative cluster homogeneity (<= y % of the cluster has ", inp$limiting, ")")) +
      scale_color_manual(values = plots$neg_color(n1), name = "Percent\ninterval")} %>% 
    ggplotly()
})

