output$limiting_factor <- renderPlotly({
  req(user$results)
  validate(need(!is.null(user$ptype), ""))
  
  # user input on showing "Number of clusters" or "Fraction of population" along the y-axis, where 
  # the clusters are those >= minC, with homogeneity at a level specified by the point color
  df <- (if (user$ptype == "num") c(1:4,6,7) else c(1:3,5,6,8)) %>% 
    selectColsName(user$results, .) %>% 
    set_colnames(c("h","prop.cl","perc.th.p","y","perc.th.n","n"))
  
  text1 <- paste0("Height: ", pull(df,1), 
    "\nNumber of clusters: ", pull(df,2) %>% strsplit(.,"/") %>% map(.,extract2,1) %>% unlist(), 
    "\nFraction: ", pull(df,4) %>% scales::percent(), "\nRange: ", pull(df,3))
  
  g <- ggplot(df, aes(x = h, y = y, col = perc.th.p)) + 
    geom_point(aes(text = text1), shape = 3) + geom_line() + xlab("\nHeight") + 
    scale_color_grey(start = 0.9, end = 0) + theme_bw() + labs(color = "Percent threshold") + 
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
    g %>% textSize() %>% 
      ggplotly(., tooltip = c("text1"), source = "limitplot") %>% 
      event_register(., "plotly_click")
})

observeEvent(suppressWarnings(event_data("plotly_click", source = "limitplot")), {
  suppressWarnings(
    s <- event_data("plotly_click", source = "limitplot")  
  )
  req(inp$data, length(s))
  h <- s$x %>% as.character()
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
  tmp$Source <- abs(tmp$Source - 1)
  tmp$Count <- 0
  tmp$Fraction <- tmp$Count/tmp$Size
  b2 <- rbind(b2, tmp)
  b2$interval <- NA
  plots$bubble_data <- b2 %>% filter(Size >= inp$minC)
})

output$negative_bubble <- renderPlotly({
  req(plots$bubble_data, plots$bubble_title)
  ptitle <- paste0(plots$bubble_title, ", negative homogeneity")
  
  # sequence going from 0 to left hand side boundary
  pos_h <- seq(0, percLhs()/100, by = stepLhs()) %>% rev()
  
  toplot <- plots$bubble_data %>% filter(Source == values$lim)
  for (th in pos_h) 
    toplot$interval[toplot$Fraction <= th] <- th
  
  toplot <- toplot %>% filter(!is.na(interval))
  toplot$interval %<>% factor(., levels = pos_h)
    
  num_colors <- toplot$interval %>% unique() %>% length()
  color_set <- colorRampPalette(brewer.pal(8,"Set3"))(num_colors)
  ggplot(toplot, aes(x = Clusters, y = Fraction, size = Size, color = interval)) + 
    geom_point() + scale_y_continuous(limits = c(0,1)) + 
    scale_color_manual(values = color_set) + ggtitle(ptitle)
})

output$positive_bubble <- renderPlotly({
  req(plots$bubble_data, plots$bubble_title)
  ptitle <- paste0(plots$bubble_title, ", positive homogeneity")
  
  # sequence going from right hand side boundary to 1
  pos_h <- seq(percRhs()/100, 1, by = stepRhs()) %>% rev()
  
  toplot <- plots$bubble_data %>% filter(Source == values$lim)
  for (th in pos_h) 
    toplot$interval[toplot$Fraction >= th] <- th
  
  toplot <- toplot %>% filter(!is.na(interval))
  toplot$interval %<>% factor(., levels = pos_h)
    
  num_colors <- toplot$interval %>% unique() %>% length()
  color_set <- colorRampPalette(brewer.pal(8,"Set3"))(num_colors)
  ggplot(toplot, aes(x = Clusters, y = Fraction, size = Size, color = interval)) + 
    geom_point() + scale_y_continuous(limits = c(0,1)) + 
    scale_color_manual(values = color_set) + ggtitle(ptitle)
})

output$all_percents <- renderPlotly({
  req(user$results); req(input$facet_by); validate(need(!is.null(user$ptype), ""))
  
  plot_title <- paste0("Data found in homogeneous clusters of size ", inp$minC, " or larger\n")
  
  df <- (if (user$ptype == "num") c(1:4,6,7) else c(1:3,5,6,8)) %>% 
    selectColsName(user$results, .) %>% 
    set_colnames(c("h","prop.cl","perc.th.p","p1","perc.th.n","n1"))
  cn <- colnames(df)
  
  ftype <- switch(input$facet_by, 
    "Positive" = list(cn[c(4,3)], cn[c(6,5)], "Negative", 3, 20), 
    "Negative" = list(cn[c(6,5)], cn[c(4,3)], "Positive", 20, 3)) %>% unlist()
  
  text1 <- paste0("Height: ", pull(df,1), "\nFraction: ", pull(df, ftype[1]) %>% scales::percent(), 
                  "\nRange: ", pull(df, ftype[2]), "\nType: ", input$facet_by)
  text2 <- paste0("Height: ", pull(df,1), "\nFraction: ", pull(df, ftype[3]) %>% scales::percent(), 
                  "\nRange: ", pull(df, ftype[4]), "\nType: ", ftype[5])
  
  g <- ggplot(df, aes_string(x = "h", y = ftype[1], group = ftype[2])) + 
    geom_point(aes(text = text1), shape = ftype[6], size = 1.5, show.legend = TRUE) + geom_line() +
    facet_wrap(as.formula(paste0(ftype[2], "~", " ."))) + 
    geom_point(aes_string(x = "h", y = ftype[3], col = ftype[4], text = "text2"), shape = ftype[7], size = 1.5) +
    geom_line(aes_string(x = "h", y = ftype[3], col = ftype[4])) + 
    theme_bw() + theme(plot.margin = unit(c(1.5,1,1.5,1.5), "cm")) +
    xlab("\n\nHeight") + ggtitle(plot_title)
  
  if (user$ptype == "prop") {
    g <- g + scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
      ylab("Fraction of population\n")
  }else {
    g <- g + ylab("Number of clusters\n")
  }
  g %>% textSize() %>% ggplotly(., tooltip = c("text1","text2"))
})

# output$nonlimiting_bubble <- renderPlotly({
#   req(inp$data)
#   s <- event_data("plotly_click", source = "limitplot")
#   if (length(s)) {
#     h <- s$x %>% as.character()
#     plot_title <- paste0("Clusters not included in the above plot, and the proportion of ",
#                          "non-limiting factor in each cluster, at height ", h)
# 
#     pos_h <- seq(percRhs()/100, 1, by = stepRhs()) # sequence going from right hand side boundary to 1
#     b <- inp$data[,c(h,"Source")] %>% set_colnames(c("cl","src")) %>% group_by_all() %>% count()
#     b2 <- aggregate(b$n, by = list(cl = b$cl), FUN = sum) %>% as_tibble() %>%
#       left_join(b, ., by = "cl") %>% ungroup()
#     b2$frac <- b2$n/b2$x
#     impCl <- b2 %>% filter(x >= inp$minC) %>% filter(frac <= pos_h[s$curveNumber + 1])
# 
#     # Initially, we have the set of all clusters, b2. A subset of these is what we consider, impCl.
#     # We want to see what the other clusters look like, with respect to the non-limiting factor.
#     tmp <- b2[!(b2$cl %in% impCl$cl),] %>% select(cl, x, src, frac) %>%
#       pivot_wider(names_from = "src", values_from = "frac")
#     tmp[is.na(tmp)] <- 0
# 
#     inds <- colnames(tmp)==values$lim
#     colnames(tmp)[inds] <- "Limiting"
#     colnames(tmp)[!inds] <- c("Clusters","Size","Non-limiting")
# 
#     tmp$interval <- cut(tmp$`Non-limiting`, breaks = 5)
#     # tmp <- tmp %>% filter(Size >= inp$minC)
#     text1 <- paste0("Cluster: ", tmp$Clusters, "\nProportion of cluster: ",
#                     round(tmp$`Non-limiting`, digits = 3), "\nCluster size: ", tmp$Size)
# 
#     {ggplot(tmp, aes(x = Clusters, y = `Non-limiting`, color = interval,
#                      size = Size, text = text1)) +
#         geom_point() + ylab("Proportion of cluster with non-limiting factor") +
#         scale_color_brewer(palette = "Set3") +
#         ggtitle(plot_title)} %>%
#       ggplotly(tooltip = "text1")
#   }
# })