
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

