options(shiny.maxRequestSize=30*1024^2)
source("modules.R")

server <- function(input, output, session) {
  source("initialize_reactive_values.R", local = TRUE)

  # TAB 1 - INPUT DATA ----------------------------------------------------------------------------------------
  # Validate inputs
  source("description.R", local = TRUE) # errMsg(err_code) - validates inputs
  source("on_submit_check_inputs.R", local = TRUE)
  # output$base_metrics from description.R
  
  # Datatable of heights and number of clusters for each
  output$num_clusters <- renderDT({
    validate(need(!is.null(inp$data), "")); validate(need(!is.null(user$lim), ""))
    
    df <- inp$data
    nonlimiting <- setdiff(0:1,values$lim)
    heights <- colnames(df)[-1][-1]
    
    numC <- apply(df[,3:ncol(df)], 2, FUN = n_distinct)
    numC_above <- apply(df[,3:ncol(df)], 2, FUN = function(x) {
      table(x) %>% as.data.frame() %>% filter(Freq >= inp$minC) %>% nrow()})
    
    lim <- lapply(heights, function(h) {
      tibble("perc" = user$lim[[h]]$Size %>% sum() %>% "/"(nrow(df)) %>% round(digits = 3), 
             "cl" = user$lim[[h]] %>% nrow())}) %>% bind_rows()
    
    lim_not <- lapply(heights, function(h) {
      tibble("perc" = user$nonlim[[h]]$Size %>% sum() %>% "/"(nrow(df)) %>% round(digits = 3), 
             "cl" = user$nonlim[[h]] %>% nrow())}) %>% bind_rows()
    
    t1 <- "Number of clusters"
    t2 <- "Percent of popoulation in clusters >= "
    t3 <- " with 100% or 0% being "
    t4 <- " with size >= "
    user$tbl <- tibble(heights, numC, numC_above) %>% 
      add_column(lim$cl, lim$perc, lim_not$cl, lim_not$perc) %>% 
      set_colnames(c("Heights", t1, paste0(t1, t4, inp$minC), paste0(t1, t4, inp$minC, t3, values$lim),
                     paste0(t2, inp$minC, ",", t3, values$lim), paste0(t1, t4, inp$minC, t3, nonlimiting), 
                     paste0(t2, inp$minC, ",", t3, nonlimiting)))
    introDT <- user$tbl %>% 
      DT::datatable(options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                   dom = "ti", pageLength = nrow(df), scrollY = "500px"), 
                    rownames = FALSE, filter = 'none', selection = list(target = "cell", mode = "single")) %>% 
      formatStyle(3:7, border = '1px solid #ddd')
  })
  
  # Mini table of cluster sizes
  source("new_table_on_cellclick.R", local = TRUE)
  
  # TAB 2 - PARAMETERS ----------------------------------------------------------------------------------------
  # When metrics of the input data are shown to the user, generate the UI components for "Parameters" tab
  source("generate_and_manage_tab2_ui.R", local = TRUE)
  
  # On "update" button-click, uses user inputs to generate the proportions and relevant data
  observeEvent(input$update, {
    req(inp$data, values$locus)
    a1 <- inp$data[,2] %>% binaryStats()
    h <- colnames(inp$data)[3:ncol(inp$data)]
    c1 <- c("h", "prop.cl")
    withProgress(message = "Collecting metric data: ", value = 0, {
      user$initial <-
        lapply(1:length(h), function(i) {
          incProgress(
            1/length(h), 
            detail = paste0("Going through thresholds, ", round(i/length(h), digits=2)*100, "% done"))
          globalMetrics(values$locus, h[i], inp$data, inp$minC, a1$Bin[which.min(a1$Freq)], 
                        percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
        }) %>% bind_rows() %>% set_colnames(., c(c1, "perc.th", "th.type", "num.cl", "prop.of.data"))
    })
    
    df2 <- tableNames(user$initial, "neg", inp$minC)
    user$results <- tableNames(user$initial, "pos", inp$minC) %>% 
      merge(., df2, by = colnames(df2)[1:2]) %>% as_tibble()
    
    df <- user$results %>% select(1,2,3,5,6,8) %>% 
      set_colnames(c(c1,"perc.th.p","prop.of.data.p","perc.th.n","prop.of.data.n"))
    
    df$h <- as.numeric(df$h)
    df$perc.th.p <- as.character(df$perc.th.p)
    df$perc.th.n <- as.character(df$perc.th.n)
    user$plot <- df
    
    output$click_limplot <- renderText({blurb(type = "ClickPoint")})
    shinyjs::show(id = "facet_ui")
  })
  
  # When the data has been generated, use dataset size to define plot height
  # Then set up the UI components for the plot, datatable, and download button

  output$limiting_factor <- renderPlotly({
    req(user$plot)
    df <- user$plot
    text1 <- paste0("Height: ", df$h, "\nFraction: ", pull(df, prop.of.data.p) %>% scales::percent(),
                    "\nRange: ", pull(df, perc.th.p), "\nType: Positive")
    plot_title <- paste0("Proportion of data found in homogeneous clusters of size ", inp$minC, " or larger,\n", 
                         "looking at proportions >= color of the limiting factor")
    
    {ggplot(df, aes(x = h, y = prop.of.data.p, col = perc.th.p)) + 
        geom_point(aes(text = text1)) + geom_line() + 
        xlab("\nHeight") + ylab("Fraction of population\n") + ggtitle(plot_title) + 
        scale_y_continuous(labels = percent, limits = c(0,1)) + theme_bw() + scale_color_grey() + 
        theme(plot.margin = unit(c(1.5,1,2,2), "cm"), 
              axis.title.x.top = element_text(margin = margin(t = 20)), 
              axis.title.y.right = element_text(margin = margin(r = 20)))} %>% 
      textSize() %>% ggplotly(., tooltip = c("text1"), source = "limitplot") %>% 
      event_register(., "plotly_click")
  })
  
  output$nonlimiting <- renderPlotly({
    req(inp$data)
    s <- event_data("plotly_click", source = "limitplot")
    if (length(s)) {
      h <- s$x %>% as.character()
      plot_title <- paste0("Clusters not included in the above plot, and the proportion of ", 
                           "non-limiting factor in each cluster, at height ", h)
      
      pos_h <- seq(percRhs()/100, 1, by = stepRhs()) # sequence going from right hand side boundary to 1
      b <- inp$data[,c(h,"Source")] %>% set_colnames(c("cl","src")) %>% group_by_all() %>% count()
      b2 <- aggregate(b$n, by = list(cl = b$cl), FUN = sum) %>% as_tibble() %>% 
        left_join(b, ., by = "cl") %>% ungroup()
      b2$frac <- b2$n/b2$x
      impCl <- b2 %>% filter(x >= inp$minC) %>% filter(frac <= pos_h[s$curveNumber + 1])
      
      # Initially, we have the set of all clusters, b2. A subset of these is what we consider, impCl.
      # We want to see what the other clusters look like, with respect to the non-limiting factor.
      tmp <- b2[!(b2$cl %in% impCl$cl),] %>% select(cl, x, src, frac) %>% 
        pivot_wider(names_from = "src", values_from = "frac")
      tmp[is.na(tmp)] <- 0
      
      inds <- colnames(tmp)==values$lim
      colnames(tmp)[inds] <- "Limiting"
      colnames(tmp)[!inds] <- c("Clusters","Size","Non-limiting")
      text1 <- paste0("Cluster: ", tmp$Clusters, "\nProportion of cluster: ", 
                      round(tmp$`Non-limiting`, digits = 3), "\nCluster size: ", tmp$Size)
      
      {ggplot(tmp, aes(x = Clusters, y = `Non-limiting`, color = `Non-limiting`, size = Size, text = text1)) + 
          ylab("Proportion of cluster with non-limiting factor") + geom_point() + 
          scale_color_gradient(low = "deepskyblue", high = "darkblue") + ggtitle(plot_title)} %>% 
        ggplotly(tooltip = "text1")
    }
  })
  
  # output$plot_exp from description.R
  
  output$all_percents <- renderPlotly({
    req(user$plot); req(user$initial); req(input$facet_by)
    df <- user$plot
    plot_title <- paste0("Proportion of data found in homogeneous clusters of size ", inp$minC, " or larger\n")
    
    ftype <- switch(input$facet_by, 
                    "Positive" = list(colnames(df)[c(4,3)], colnames(df)[c(6,5)], "negative"), 
                    "Negative" = list(colnames(df)[c(6,5)], colnames(df)[c(4,3)], "positive")) %>% unlist()

    text1 <- paste0("Height: ", df$h, "\nFraction: ", pull(df, ftype[1]) %>% scales::percent(), 
                    "\nRange: ", pull(df, ftype[2]), "\nType: ", input$facet_by)
    text2 <- paste0("Height: ", df$h, "\nFraction: ", pull(df, ftype[3]) %>% scales::percent(), 
                    "\nRange: ", pull(df, ftype[4]), "\nType: ", ftype[5])
    
    {ggplot(df, aes_string(x = "h", y = ftype[1], group = ftype[2])) + 
        geom_point(aes(text = text1)) + geom_line() + 
        scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
        xlab("\n\nHeight") + ylab("Fraction of population\n") + ggtitle(plot_title) +
        facet_wrap(as.formula(paste0(ftype[2], "~", " ."))) + 
        geom_point(aes_string(x = "h", y = ftype[3], col = ftype[4], text = "text2")) + 
        geom_line(aes_string(x = "h", y = ftype[3], col = ftype[4])) + theme_bw() + 
        theme(plot.margin = unit(c(1.5,1,1.5,1.5), "cm"))} %>% 
      textSize() %>% ggplotly(., tooltip = c("text1","text2"))
  })
  
  # datatable
  output$results <- DT::renderDT({
    req(user$results)
    user$results %>% 
      DT::datatable(options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                       dom = "ti", pageLength = nrow(df), scrollY = "500px"), 
                    rownames = FALSE, filter = "top", selection = "single") %>% 
      formatRound(columns = c(5,8), digits = 4)
  })
  
  output$click_final <- renderText({
    validate(need(!is.null(user$results), ""))
    blurb(type = "ClickRow")
  })
  
  output$final <- renderDT({
    validate(need(!is.null(user$results), ""), need(length(input$results_rows_selected)>0, ""))
    
    rowX <- user$results[input$results_rows_selected,]
    df <- readData("FNC_MBS_Example.tsv") %>% perfClusters(., rowX$Height) %>% ungroup()
    df$charFrac <- paste0(df$Freq, "/", df$Size)
    
    df <- df %>% select(Clusters, Source, Size, charFrac, Fraction)
    df_a <- df %>% filter(Source == 0) %>% set_colnames(c("Clusters","Non-human","Size","Non-human proportion of cluster", "NValue"))
    df_b <- df %>% filter(Source != 0) %>% set_colnames(c("Clusters","Human","Size","Human proportion of cluster", "HValue"))
    
    toshow <- full_join(df_a, df_b)
    toshow[is.na(toshow)] <- 0
    asDT(toshow)
  })
}
