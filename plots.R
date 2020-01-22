# Brief explanation of the faceted plot on the Parameters tab, describing how the facet 
# variable is used and how the plot can be interpreted.
output$plot_exp <- renderUI({
  req(user$plot, input$facet_by)
  df <- user$plot
  cn <- colnames(df)
  # cn[c(4,3)] == Prop of pop in +'ve homogeneity clusters, +'ve threshold
  # cn[c(6,5)] == Prop of pop in -'ve homogeneity clusters, -'ve threshold
  facet_type <- switch(input$facet_by, 
    "Positive" = list(" or more ", cn[c(4,3)], cn[c(6,5)], " or less "), 
    "Negative" = list(" or less ", cn[c(6,5)], cn[c(4,3)], " or more ")) %>% unlist()
  
  b1 <- df[1,] %>% pull(facet_type[3]) %>% as.numeric() %>% scales::percent()
  cx <- df %>% pull(facet_type[5]) %>% unique() %>% as.numeric() %>% scales::percent() %>% toString()
  nottype <- c("Positive", "Negative") %>% setdiff(., input$facet_by)
  
  c(input$facet_by, nottype) %>% tolower() %>% 
    c(., b1, facet_type[1], cx, facet_type[6]) %>% 
    blurb(., "FacetedPlot")
})

output$limiting_factor <- renderPlot({
  req(user$initial, user$ptype)
  df <- user$initial
  
  df$h <- as.double(df$h)
  df$perc.th <- factor(df$perc.th, levels = df$perc.th %>% unique() %>% sort(decreasing = TRUE))
  df$th.type[df$th.type == "pos"] <- "Positive"
  df$th.type[df$th.type == "neg"] <- "Negative"
  
  text1 <- paste0("Height: ", pull(df,1), "\nNumber of clusters: ", pull(df,5), 
                  "\nFraction of population: ", pull(df,6) %>% round(digits = 3), 
                  "\nRange: ", pull(df,3))
  
  # # user input on showing "Number of clusters" or "Fraction of population" along the y-axis, where 
  # # the clusters are those >= minC, with homogeneity at a level specified by the point color
  yval <- if (user$ptype == "num") "num.cl" else "prop.of.data"
  
  g <- ggplot(df, aes_string(x = "h", y = yval, color = "perc.th", shape = "th.type")) + 
    geom_point(aes(text = text1)) + geom_line() + xlab("\nHeight") + theme_bw() + 
    scale_shape_manual(values = c(20,3), name = "Positive or negative homogeneity") + 
    scale_color_grey(start = 0, end = 0.9, name = "Percent threshold") + 
    theme(plot.margin = unit(c(1.5,1,2,2), "cm"), 
          axis.title.x.top = element_text(margin = margin(t = 20)), 
          axis.title.y.right = element_text(margin = margin(r = 20)))
  
  if (user$ptype == "prop") {
    g <- g + ylab("Fraction of population\n") + 
      scale_y_continuous(labels = percent, limits = c(0,1)) + 
      ggtitle(paste0("Proportion of population in homogeneous clusters of size >= ", 
                     inp$minC, ". \n(Limiting factor homogeneity)"))
  }else {
    g <- g + ylab("Number of clusters\n") + 
      ggtitle(paste0("Number of homogeneous clusters of size >= ", inp$minC, 
                     ". \n(Limiting factor homogeneity)"))
  }
  x <- 16
  g + theme(strip.text.y = element_text(margin = margin(0,2,0,2)), 
            strip.text = element_text(size = x), axis.text.y = element_text(size = x), 
            axis.text.x = element_text(size = x), title = element_text(size = x), 
            legend.text = element_text(size = x))
})


output$all_percents <- renderPlotly({
  req(user$results); req(input$facet_by); req(user$ptype)

  plot_title <- paste0("Data found in homogeneous clusters of size ", inp$minC, " or larger\n")
  expandedPal <- colorRampPalette(brewer.pal(8, "Set1"))

  pt <- switch (user$ptype,
                "num" = list("num", "Number of clusters\n"),
                "prop" = list("wp", "Fraction of population")) %>% unlist()
  ft <- switch(input$facet_by,
               "Positive" = list(tolower(input$facet_by), "negative", 3, 1),
               "Negative" = list(tolower(input$facet_by), "positive", 1, 3)) %>% unlist()
  
  df_ur <- user$results %>% set_colnames(c("h","prop.cl.size","pos.th","pos.num",
                                       "pos.th.wp","neg.th","neg.num","neg.th.wp"))
  cnames <- c("h","prop.cl.size","type","th","num","wp")
  a1 <- df_ur[,1:5] %>% add_column(type = "positive", .before = 3) %>% set_colnames(cnames)
  a2 <- df_ur[,c(1:2,6:8)] %>% add_column(type = "negative", .before = 3) %>% set_colnames(cnames)
  
  df <- bind_rows(a1, a2)
  df$h <- as.integer(df$h)
  df$th <- factor(df$th, levels = df$th %>% unique() %>% sort(decreasing = TRUE))
  
  df1 <- df[df$type == ft[1],]
  text1 <- pointText(df1, c(1,6,4), input$facet_by)
  
  g <- ggplot() + 
    geom_point(data = df1, aes_string(x = "h", y = pt[1], group = "th", text = "text1"), 
               shape = ft[3], size = 1.5) + 
    geom_line(data = df1, aes_string(x = "h", y = pt[1], group = "th")) + 
    facet_wrap( ~ df1$th)
  
  df2 <- df[df$type == ft[2],]
  text2 <- pointText(df2, c(1,6,4), input$facet_by)
  
  g <- g + geom_point(data = df2, aes_string(x = "h", y = pt[1], col = "th", text = "text2"), 
                      shape = ft[4], size = 1.5) + 
    geom_line(data = df2, aes_string(x = "h", y = pt[1], col = "th")) + 
    scale_color_manual(name = "Legend", values = expandedPal(length(unique(df2$th))))
  
  g <- g + theme_bw() + theme(plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm")) + 
    xlab("\n\nHeight") + ylab(pt[2]) + ggtitle(plot_title)
  
  if (user$ptype == "prop") {
    g <- g + scale_y_continuous(labels = scales::percent, limits = c(0,1))
  }
  g %>% textSize(., incl.legend = TRUE) %>% ggplotly(., tooltip = c("text1", "text2"))
})

output$select_height <- renderUI({
  req(user$initial, user$ptype)
  tagList(
    selectInput("height", "Select height", choices = user$initial$h %>% unique()), 
    actionButton("specific_h", "Submit")
  )
})

observeEvent(input$specific_h, {
  req(user$results, inp$data, input$height)
  
  h <- input$height
  plots$bubble_title <- paste0("Clusters used to calculate the selected proportion of ",
                               "limiting factor in each cluster, at height ", h)
  # group data by clusters at selected h and source, then count freq. of the binary variable
  b <- inp$data %>% select(h,"Source") %>%
    group_by_all() %>% count() %>%
    set_colnames(c("Clusters", "Source", "Count"))

  # add columns of the (1) cluster sizes and (2) fraction of cluster with 0 or 1
  b2 <- aggregate(b$Count, by = list(cl = b$Clusters), FUN = sum) %>% as_tibble() %>%
    set_colnames(c("Clusters", "Size")) %>%
    left_join(b, ., by = "Clusters") %>% ungroup()
  b2$Fraction <- b2$Count/b2$Size

  tmp <- b2 %>% filter(Fraction == 1)
  tmp$Source <- lapply(tmp$Source, function(x) setdiff(user$bin, x)) %>% unlist()
  tmp$Count <- 0
  tmp$Fraction <- tmp$Count/tmp$Size
  b2 <- rbind(b2, tmp)
  b2$interval <- NA
  plots$bubble_data <- b2 %>% filter(Size >= inp$minC)
})

output$negative_bubble <- renderPlotly({
  req(user$results, plots$bubble_data, plots$bubble_title)
  
  # sequence going from 0 to left hand side boundary
  pos_h <- seq(0, percLhs()/100, by = stepLhs()) %>% rev()
  
  toplot <- plots$bubble_data %>% filter(Source == inp$limiting)
  
  for (th in pos_h) 
    toplot$interval[toplot$Fraction <= th] <- paste0("<= ", scales::percent(th))
  
  toplot <- toplot %>% filter(!is.na(interval))
  toplot$interval %<>% factor(., levels = toplot$interval %>% unique())
  
  ptitle <- paste0(plots$bubble_title, ", negative homogeneity")  
  num_colors <- toplot$interval %>% unique() %>% length()
  color_set <- colorRampPalette(brewer.pal(8,"Set3"))(num_colors)
  
  {ggplot(toplot, aes(x = Clusters, y = Fraction, color = interval)) +
      geom_point() + 
      geom_point(aes(size = Size), show.legend = FALSE) + 
      scale_y_continuous(limits = c(0,1)) + ggtitle(ptitle) + 
      scale_color_manual(name = "Legend", values = color_set)} %>% ggplotly()
})

output$positive_bubble <- renderPlotly({
  req(user$results, plots$bubble_data, plots$bubble_title)
  
  # sequence going from right hand side boundary to 1
  pos_h <- seq(percRhs()/100, 1, by = stepRhs()) %>% rev()
  
  toplot <- plots$bubble_data %>% filter(Source == inp$limiting)
  for (th in pos_h) 
    toplot$interval[toplot$Fraction >= th] <- paste0(">= ", scales::percent(th))
  
  toplot <- toplot %>% filter(!is.na(interval))
  toplot$interval %<>% factor(., levels = toplot$interval %>% unique())
  
  ptitle <- paste0(plots$bubble_title, ", positive homogeneity")
  num_colors <- toplot$interval %>% unique() %>% length()
  color_set <- colorRampPalette(brewer.pal(8,"Set3"))(num_colors)
  
  {ggplot(toplot, aes(x = Clusters, y = Fraction, color = interval)) +
      geom_point() + 
      geom_point(aes(size = Size), show.legend = FALSE) + 
      scale_y_continuous(limits = c(0,1)) + ggtitle(ptitle) + 
      scale_color_manual(name = "Legend", values = color_set)} %>% ggplotly()
})
