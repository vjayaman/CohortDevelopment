
output$limiting_factor <- renderPlot({
  req(inp$data, user$initial, user$ptype)
  dfx <- user$initial
  
  df_heights <- data.frame(
    h = dfx$h %>% unique(), 
    numeric_height = 1:length(unique(dfx$h)), stringsAsFactors = FALSE)
  df <- left_join(dfx, df_heights)
  df$h <- factor(df$h, levels = unique(df$h))
  
  df$perc.th <- factor(df$perc.th, levels = df$perc.th %>% unique() %>% sort(decreasing = TRUE))
  df$th.type[df$th.type == "pos"] <- "Positive"
  df$th.type[df$th.type == "neg"] <- "Negative"

  # user input on showing "Number of clusters" or "Fraction of population" along the y-axis, where
  # the clusters are those >= minC, with homogeneity at a level specified by the point color
  yval <- if (user$ptype == "num") "num.cl" else "prop.of.data"

  df$new <- NA
  df$new[df$th.type=="Positive"] <- paste0(">= ", df$perc.th[df$th.type=="Positive"])
  df$new[df$th.type=="Negative"] <- paste0("<= ", df$perc.th[df$th.type=="Negative"])
  df$new <- factor(df$new, levels = df$new %>% unique() %>% sort(decreasing = TRUE))

  colnames(df) <- gsub("new", "Percent", colnames(df))

  p1 <- unique(df$Percent[df$th.type=="Positive"]) %>% length()
  n1 <- unique(df$Percent[df$th.type=="Negative"]) %>% length()

  df$pretty_perc <- df$perc.th %>% as.character() %>% as.numeric() %>% percent()
  df$pretty_perc <- factor(df$pretty_perc, levels = df$pretty_perc[order(df$perc.th)] %>% unique())
  df_pos <- filter(df, th.type == "Positive")
  df_neg <- filter(df, th.type == "Negative")

  limplot$df <- df
  limplot$df_pos <- df_pos
  limplot$df_neg <- df_neg
  
  # identify best heights by yval
  types <- df$pretty_perc %>% unique() %>% sort()
  b <- lapply(types, function(x) {
    ind <- df %>% filter(df$pretty_perc == x) %>% pull(yval) %>% which.max()
    tibble(x, unique(df$h)[ind])
  }) %>% bind_rows() %>% set_colnames(c("Perc","Height"))
  y <- b$Height[c(1,nrow(b))] %>% set_names(c("pos","neg"))
  
  best$heights <- y
  
  g <- ggplot(mapping = aes_string(x = "h", y = yval, group = "Percent")) +
    
    (geom_point(data = df_pos, aes(c1 = pretty_perc), shape = 3) %>%
       relayer::rename_geom_aes(new_aes = c("colour" = "c1"))) +
    (geom_line(data = df_pos, aes(c2 = pretty_perc), size = 1.5) %>%
       relayer::rename_geom_aes(new_aes = c("colour" = "c2"))) +
    scale_manual("c1", pos_color_scheme(p1), "Positive") + 
    scale_manual("c2", pos_color_scheme(p1), "Positive") +
    
    (geom_point(data = df_neg, aes(c3 = pretty_perc), shape = 20) %>%
       relayer::rename_geom_aes(new_aes = c("colour" = "c3"))) +
    (geom_line(data = df_neg, aes(c4 = pretty_perc), size = 1.5) %>%
       relayer::rename_geom_aes(new_aes = c("colour" = "c4"))) +
    scale_manual("c3", neg_color_scheme(n1), "Negative") + 
    scale_manual("c4", neg_color_scheme(n1), "Negative") +
    
    theme_bw() + theme(legend.position = "right") + 
    xlab("\nHeight") + guides(color = guide_legend(ncol = 2))

  if (user$ptype == "prop") {
    g <- g + ylab("Fraction of population\n") +
      scale_y_continuous(labels = percent, limits = c(0,1), breaks = pretty(0:1, n = 20)) +
      ggtitle(paste0("Proportion of population in homogeneous clusters of size >= ",
                     inp$minC, ". \n(Limiting factor homogeneity)"))
  }else {
    g <- g + ylab("Number of clusters\n") +
      ggtitle(paste0("Number of homogeneous clusters of size >= ", inp$minC,
                     ". \n(Limiting factor homogeneity)")) +
      scale_y_continuous(breaks = pretty(1:max(df$num.cl), n = max(round(max(df$num.cl)/10), 10)))
  }
  x <- 16
  g + theme(strip.text.y = element_text(margin = margin(0,2,0,2)),
            strip.text = element_text(size = x), axis.text.y = element_text(size = x),
            axis.text.x = element_text(size = x), title = element_text(size = x),
            legend.text = element_text(size = x))
})

output$positive_lines <- renderPlotly({
  req(inp$data, limplot$df, limplot$df_pos)
  yval <- if (user$ptype == "num") "num.cl" else "prop.of.data"

  df <- limplot$df
  p1 <- unique(df$Percent[df$th.type=="Positive"]) %>% length()
  df_pos <- limplot$df_pos

  text1 <- paste0("Height: ", pull(df_pos,1), "\nNumber of clusters: ", 
                  pull(df_pos,5), "\nProportion of data: ", 
                  round(pull(df_pos,6), 4), 
                  "\nProportion of each cluster >=", pull(df_pos,8))
  
  pp <- ggplot(limplot$df_pos, aes_string(x = "h", y = yval, 
                                          group = "Percent", text = "text1")) + 
    geom_point(aes(colour = pretty_perc)) + 
    geom_line(aes(colour = pretty_perc)) + 
    scale_color_manual(values = pos_color_scheme(p1), "Positive") + 
    theme_bw() + theme(legend.position = "right") + xlab("\nHeight")
  
  if (user$ptype == "prop") {
    pp <- pp + ylab("Fraction of population\n") +
      scale_y_continuous(labels = percent, limits = c(0,1), 
                         breaks = pretty(0:1, n = 20)) +
      ggtitle(paste0("Proportion of population in homogeneous clusters of ", 
                     "size >= ", inp$minC, ". \n(Limiting factor homogeneity)\n"))
  }else {
    pp <- pp + ylab("Number of clusters\n") +
      ggtitle(paste0("Number of homogeneous clusters of size >= ", inp$minC,
                     ". (Limiting factor homogeneity)\n")) +
      scale_y_continuous(breaks = pretty(1:max(df$num.cl), n = max(round(max(df$num.cl)/10), 10)))
  }
  pp %>% ggplotly(., tooltip = "text1")
})

output$negative_lines <- renderPlotly({
  req(inp$data, limplot$df, limplot$df_neg)
  
  yval <- if (user$ptype == "num") "num.cl" else "prop.of.data"
  
  df <- limplot$df
  n1 <- unique(df$Percent[df$th.type=="Negative"]) %>% length()
  df_neg <- limplot$df_neg
  
  text1 <- paste0("Height: ", pull(df_neg,1), "\nNumber of clusters: ", 
                  pull(df_neg,5), "\nProportion of data: ", 
                  round(pull(df_neg,6), 4), 
                  "\nProportion of each cluster >=", pull(df_neg,8))
  
  pn <- ggplot(limplot$df_neg, aes_string(x = "h", y = yval, group = "Percent", text = "text1")) + 
    geom_point(aes(colour = pretty_perc)) + geom_line(aes(colour = pretty_perc)) + 
    scale_color_manual(values = neg_color_scheme(n1), "Negative") + 
    theme_bw() + theme(legend.position = "right") + xlab("\nHeight")
  
  if (user$ptype == "prop") {
    pn <- pn + ylab("Fraction of population\n") +
      scale_y_continuous(labels = percent, limits = c(0,1), breaks = pretty(0:1, n = 20)) +
      ggtitle(paste0("Proportion of population in homogeneous clusters of size >= ",
                     inp$minC, ". \n(Limiting factor homogeneity)\n"))
  }else {
    pn <- pn + ylab("Number of clusters\n") +
      ggtitle(paste0("Number of homogeneous clusters of size >= ", inp$minC,
                     ". (Limiting factor homogeneity)\n")) +
      scale_y_continuous(breaks = pretty(1:max(df$num.cl), n = max(round(max(df$num.cl)/10), 10)))
  }
  pn %>% ggplotly(., tooltip = "text1")
})

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
  
  df <- inp$data %>% select(h, all_of(values$locus)) %>% group_by_all() %>% count() %>% 
    set_colnames(c("Clusters", values$locus, "Count")) %>% ungroup()
  
  homogeneous <- table(df$Clusters) %>% as.data.frame() %>% filter(Freq == 1) %>% pull(Var1)
  tmp <- df %>% filter(Clusters %in% homogeneous)
  tmp[,values$locus] <- tmp %>% pull(values$locus) %>% 
    lapply(., function(x) setdiff(inp$data %>% pull(values$locus) %>% unique(), x)) %>% unlist()
  tmp$Count <- 0
  df <- bind_rows(df, tmp)
  df$Clusters <- as.character(df$Clusters)
  
  csizes <- inp$data %>% select(h) %>% table() %>% as.data.frame() %>% as_tibble() %>% 
    set_colnames(c("Clusters","Size"))
  csizes$Clusters <- as.character(csizes$Clusters)
  
  df <- left_join(df, csizes, by = "Clusters")
  
  df$Prop <- df$Count/df$Size
  df <- df %>% filter(Size >= inp$minC)
  df$NegFraction <- df$PosFraction <- NA
  
  for (i in neg_h) {df$NegFraction[which(df$Prop <= i)] <- i}
  for (i in rev(pos_h)) {df$PosFraction[which(df$Prop >= i)] <- i}
  df <- df %>% filter(., pull(df, values$locus) %in% inp$limiting)
  
  df$PosPercent <- df$PosFraction %>% percent()
  df$NegPercent <- df$NegFraction %>% percent()
  df$PosPercent <- factor(df$PosPercent, levels = df$PosPercent[order(df$PosFraction, decreasing = TRUE)] %>% unique())
  df$NegPercent <- factor(df$NegPercent, levels = df$NegPercent[order(df$NegFraction)] %>% unique())
  
  df$Percent <- as.character(df$PosPercent)
  df$Percent[!is.na(df$NegPercent)] <- as.character(df$NegPercent[!is.na(df$NegPercent)])
  df <- df %>% filter(!is.na(Percent))
  df$Percent <- factor(df$Percent, levels = df$Percent[order(df$Prop, decreasing = TRUE)] %>% unique())
  plots$bubble_data <- df
})

output$bubble_plot <- renderPlot({
  req(inp$data, user$results, plots$bubble_data, plots$bubble_title, inp$limiting)
  
  # sequence going from 0 to left hand side boundary
  neg_h <- seq(0, percLhs()/100, by = stepLhs()) %>% rev()
  pos_h <- seq(percRhs()/100, 1, by = stepRhs()) %>% rev()
  
  neg <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
           "#4292c6", "#2171b5", "#08519c", "#08306b") %>% rev() %>% colorRampPalette(.)
  pos <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a",
           "#ef3b2c", "#cb181d", "#a50f15", "#67000d") %>% rev() %>% colorRampPalette(.)
  
  df <- plots$bubble_data
  df_pos <- df[!is.na(df$PosFraction),]
  df_neg <- df[!is.na(df$NegFraction),]
  
  n1 <- df_neg %>% pull(NegPercent) %>% unique() %>% length()
  p1 <- df_pos %>% pull(PosPercent) %>% unique() %>% length()
  
  ptitle <- paste0("Positive and negative cluster homogeneity", 
                   "\n(red = >= y %, blue = <= y %) of the cluster ", 
                   "has ", tolower(inp$limiting))
  
  ggplot(mapping = aes(x = Clusters, y = Prop, size = Size)) + 
      xlab("Cluster names") + ylab("Percent of cluster") + ggtitle(ptitle) + 
    (geom_point(data = df_pos, aes(c1 = PosPercent)) %>% 
       rename_geom_aes(new_aes = c("colour" = "c1"))) + 
    scale_manual("c1", pos(p1), "Positive") + 
    (geom_point(data = df_neg, aes(c2 = NegPercent)) %>% 
       rename_geom_aes(new_aes = c("colour" = "c2"))) + 
    scale_manual("c2", neg(n1), "Negative") + 
    scale_y_continuous(limits = c(0,1)) + 
    scale_size_continuous(range = c(2,7)) + 
    theme(strip.text.y = element_text(margin = margin(0,2,0,2)), 
          strip.text = element_text(size = 14), axis.text.y = element_text(size = 13), 
          axis.text.x = element_blank(), title = element_text(size = 14), 
          axis.ticks.x = element_blank(), legend.text = element_text(size = 12))
})


output$pos_bubbles <- renderUI({
  req(inp$data, user$results, plots$bubble_data)
  df_pos <- plots$bubble_data %>% filter(!is.na(PosFraction))
  
  if (nrow(df_pos) > 0) {
    plots$pos_data <- ""
    plotlyOutput("positive_bubble", width = "100%", height = "650px")
  }else {
    plots$pos_data <- paste0("No clusters are such that >= y % of the cluster has ", inp$limiting)
    verbatimTextOutput("no_pos_data")
  }
})

output$no_pos_data <- renderText({
  req(plots$pos_data)
  plots$pos_data
})

output$positive_bubble <- renderPlotly({
  req(inp$data, user$results, plots$bubble_data)
  plots$pos_data <- ""
  df_pos <- plots$bubble_data %>% filter(!is.na(PosFraction)) %>% select(Clusters,Size,Prop,PosPercent)
  
  colnames(df_pos) <- c("Cluster names","Cluster size","Percent of cluster","Percent interval")
  pos <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a",
           "#ef3b2c", "#cb181d", "#a50f15", "#67000d") %>% rev() %>% colorRampPalette(.)
  p1 <- df_pos %>% pull(`Percent interval`) %>% unique() %>% length()
  
  {ggplot(df_pos, aes(x = `Cluster names`, y = `Percent of cluster`, color = `Percent interval`)) + 
      geom_point() + geom_point(aes(size = `Cluster size`), show.legend = FALSE) + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
      ggtitle(paste0("Positive cluster homogeneity (>= y % of the cluster has ",inp$limiting, ")")) + 
      scale_color_manual(values = pos(p1), name = "Percent\ninterval")} %>% 
    ggplotly()
})

output$neg_bubbles <- renderUI({
  req(inp$data, user$results, plots$bubble_data)
  df_neg <- plots$bubble_data %>% filter(!is.na(NegFraction))
  
  if (nrow(df_neg) > 0) {
    plots$neg_data <- ""
    plotlyOutput("negative_bubble", width = "100%", height = "650px")
  }else {
    plots$neg_data <- paste0("Negative cluster homogeneity (<= y % of the cluster has ", inp$limiting, ")")
    verbatimTextOutput("no_neg_data")
  }
})

output$no_neg_data <- renderText({
  req(plots$neg_data)
  plots$neg_data
})

output$negative_bubble <- renderPlotly({
  req(inp$data, user$results, plots$bubble_data)
  
  plots$neg_data <- ""
  df_neg <- plots$bubble_data %>% filter(!is.na(NegFraction)) %>% select(Clusters,Size,Prop,NegPercent) %>% 
    set_colnames(c("Cluster names","Cluster size","Percent of cluster","Percent interval"))
  
  neg <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
           "#4292c6", "#2171b5", "#08519c", "#08306b") %>% rev() %>% colorRampPalette(.)
  n1 <- df_neg %>% pull(`Percent interval`) %>% unique() %>% length()
  
  {ggplot(df_neg, aes(x = `Cluster names`, y = `Percent of cluster`, color = `Percent interval`)) + 
      geom_point() + geom_point(aes(size = `Cluster size`), show.legend = FALSE) + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
      ggtitle(paste0("Negative cluster homogeneity (<= y % of the cluster has ", inp$limiting, ")")) +
      scale_color_manual(values = neg(n1), name = "Percent\ninterval")} %>% 
    ggplotly()
})

output$indiv_bub_plot <- renderText({
  paste0(plots$pos_data, "\n", plots$neg_data)
})