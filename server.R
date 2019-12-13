options(shiny.maxRequestSize=30*1024^2)
source("modules.R")
server <- function(input, output, session) {
  cell <- reactiveValues(row = NULL, col = NULL)
  values <- reactiveValues(locus = NULL, h = NULL, point = NULL, path = NULL, lim = NULL)
  user <- reactiveValues(input = NULL, results = NULL, initial = NULL, plot = NULL, 
                         minC = NULL, tbl = NULL)
  perfect <- reactiveValues(lim = NULL, nonlim = NULL)
  
  # calling modules
  percRhs <- callModule(percSlider, "rhs_perc")
  stepRhs <- callModule(stepSlider, "step_size_p")
  
  percLhs <- callModule(percSlider, "lhs_perc")
  stepLhs <- callModule(stepSlider, "step_size_n")
  
  # load data for processing
  observeEvent(input$data, {
    shinyjs::useShinyjs() 
    enable("submit")
  })
  
  # checking user's data for valid column names, file extension, etc. (ID | Binary Variable | Thresholds...)
  observeEvent(input$submit, {
    req(input$data)
    values$path <- input$data$datapath
    user$minC <- input$minC
    
    output$check_input <- renderText({
      if (tools::file_ext(values$path) %in% c("tsv","txt")) {
        df <- values$path %>% 
          read.table(header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
        validate(
          need(all(varhandle::check.numeric(colnames(df)[-1][-1])), errMsg(1)),
          need(!varhandle::check.numeric(colnames(df)[2]), errMsg(2)),
          need(!varhandle::check.numeric(colnames(df)[1]), errMsg(3)),
          need(all(unique(df[,2]) %in% c(1,0)), errMsg(4)))
        user$input <- df
        values$locus <- colnames(user$input)[2]
        errMsg(0)
      }else {
        errMsg(5)
      }
    })
  })
  
  # generating text output with basic overview of loaded data
  output$base_metrics <- renderText({
    validate(need(!is.null(user$input), ""))
    a1 <- binaryStats(user$input[,2])
    p <- a1$Bin==1
    n <- a1$Bin==0
    values$lim <- a1$Bin[which.min(a1$Freq)]

    perfect$lim <- filterPerfect(user$input, values$lim, user$minC, c(0,1))
    perfect$nonlim <- filterPerfect(user$input, setdiff(0:1,values$lim), user$minC, c(0,1))
    
    paste0("Dataset size: ", nrow(user$input), " samples\n", 
           "Number of heights: ", ncol(user$input)-2, "\n", 
           "Proportion of binary variable: \n    ", a1$Type[p], " (", a1$Bin[p], "): ", a1$Freq[p], "/", 
                a1$Tot[1], " = ", a1$percent[p], "\n", 
           "    ", a1$Type[n], " (", a1$Bin[n], "): ", a1$Freq[n], "/", a1$Tot[1], " = ", a1$percent[n], "\n", 
           "Limiting factor: ", a1$Type[which.min(a1$Freq)], " (", values$lim, ")\n", 
           "We will be maximizing the proportion found ", "\nin binary clusters for the limiting factor."
    )
  })
  
  output$click_cells <- renderText({
    validate(need(!is.null(user$input), ""))
    paste0("Click on one of the outlined cells to see the clusters and sizes that result in that field")
  })
  
  # datatable of heights and number of clusters for each
  output$num_clusters <- renderDT({
    validate(need(!is.null(user$input), "")); validate(need(!is.null(perfect$lim), ""))
    
    df <- user$input
    nonlim <- setdiff(0:1,values$lim)
    heights <- colnames(df)[-1][-1]
    
    numC <- apply(df[,3:ncol(df)], 2, FUN = n_distinct)
    numC_above <- apply(df[,3:ncol(df)], 2, FUN = function(x) {
      table(x) %>% as.data.frame() %>% filter(Freq >= user$minC) %>% nrow()})
    
    lim_per <- lapply(heights, function(h) {
      tibble("perc" = perfect$lim[[h]]$Size %>% sum() %>% "/"(nrow(df)) %>% round(digits = 3), 
             "cl" = perfect$lim[[h]] %>% nrow())}) %>% bind_rows()
    
    nonlim_per <- lapply(heights, function(h) {
      tibble("perc" = perfect$nonlim[[h]]$Size %>% sum() %>% "/"(nrow(df)) %>% round(digits = 3), 
             "cl" = perfect$nonlim[[h]] %>% nrow())}) %>% bind_rows()
    
    user$tbl <- tibble(heights, numC, numC_above) %>% 
      add_column(lim_per$cl, lim_per$perc, nonlim_per$cl, nonlim_per$perc) %>% 
      set_colnames(c("Heights", "Number of clusters", paste0("Number of clusters with size >= ", user$minC), 
                     paste0("Number of clusters with size >= ", user$minC, " with 100% or 0% being ", values$lim),
                     paste0("Percent of population in clusters >= ", user$minC, ", with 100% or 0% being ", values$lim), 
                     paste0("Number of clusters with size >= ", user$minC, " with 100% or 0% being ", nonlim), 
                     paste0("Percent of population in clusters >= ", user$minC, ", with 100% or 0% being ", nonlim)))
    introDT <- user$tbl %>% 
      DT::datatable(options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                   dom = "ti", pageLength = nrow(df), scrollY = "500px"), 
                    rownames = FALSE, filter = 'none', selection = list(target = "cell", mode = "single")) %>% 
      formatStyle(3:7, border = '1px solid #ddd')
  })
  
  output$cluster_info <- renderDT({
    validate(need(!is.null(user$tbl), ""), need(length(input$num_clusters_cell_clicked)>0, ""))
    
    clicked <- input$num_clusters_cell_clicked
    if (clicked$col %in% c(2:6)) {
      cell$col <- clicked$col + 1
      cell$row <- clicked$row
    }
    h <- user$tbl$Heights[cell$row]

    if (clicked$col %in% c(3,4)) {
      perfect$lim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()  
    }else if (clicked$col %in% c(5,6)) {
      perfect$nonlim[[h]] %>% ungroup() %>% select(Clusters,Size) %>% asDT()
    }else if (clicked$col == 2) {
      user$input %>% pull(h) %>% table() %>% as.data.frame() %>% 
        filter(Freq >= user$minC) %>% set_colnames(c("Clusters","Size")) %>% asDT()
    }
  })

  # as soon as data has been loaded, generate the UI components for the next tab, "Parameters"
  observe({
    validate(need(!is.null(user$input), ""))
    shinyjs::show(id = "next_tab1")
    output$param_ui <- renderUI({
      tagList(
        fluidRow(
          column(width = 4, offset = 1, percSliderInput("rhs_perc", 
              lbl = "Positive homogeneity (e.g. Cluster must have >= 80% be 1 to be counted):", val = 80), 
            uiOutput("posStepSizeUI")), 
          column(width = 4, offset = 1, percSliderInput("lhs_perc", 
              lbl = "Negative homogeneity (e.g. Cluster must have <= 10% be 1 to be counted):", val = 10),
            uiOutput("negStepSizeUI"), 
            actionButton("update", "Update inputs"), 
            shinyjs::useShinyjs(),
            disabled(
              downloadButton("dnld_tbl", "Download table")
              )
            )
        )
      )
    })
    output$posStepSizeUI <- renderUI({stepSliderInput("step_size_p", 100-percRhs())})
    output$negStepSizeUI <- renderUI({stepSliderInput("step_size_n", percLhs())})
  })
  # user can click on "next" button to switch to next tab
  observeEvent(input$next_tab1, {
    updateTabItems(session, inputId = "tabs", selected = "tab-params")
  })
  
  # on "update" button-click, uses user inputs to generate the proportions and relevant data
  observeEvent(input$update, {
    req(user$input, values$locus)
    a1 <- user$input[,2] %>% binaryStats()
    h <- colnames(user$input)[3:ncol(user$input)]
    withProgress(message = "Collecting metric data: ", value = 0, {
      user$initial <-
        lapply(1:length(h), function(i) {
          incProgress(1/length(h),
                      detail = paste0("Going through thresholds, ", round(i/length(h), digits=2)*100, "% done"))
          globalMetrics(values$locus, h[i], user$input, user$minC, a1$Bin[which.min(a1$Freq)], 
                        percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
        }
        ) %>% bind_rows() %>% 
        set_colnames(., c("h", "prop.cl", "perc.th", "th.type", "num.cl", "prop.of.data"))
    })
    
    df1 <- tableNames(user$initial, "pos", user$minC)
    df2 <- tableNames(user$initial, "neg", user$minC)
    user$results <- merge(df1, df2, by = colnames(df2)[1:2]) %>% as_tibble()
    
    df <- user$results %>% select(1,2,3,5,6,8) %>% 
      set_colnames(c("h","prop.cl","perc.th.p","prop.of.data.p","perc.th.n","prop.of.data.n"))
    
    df$h <- as.numeric(df$h)
    df$perc.th.p <- as.character(df$perc.th.p)
    df$perc.th.n <- as.character(df$perc.th.n)
    
    user$plot <- df
    
    output$click_limplot <- renderText({
      paste0("Click on a point in the plot immediately below to see the ", 
             "proportions in the rest of the dataset.")
    })
    shinyjs::show(id = "facet_ui")
  })
  
  # when the data has been generated, use dataset size to define plot height, then set up the UI components 
  # for the plot, datatable, and download button
  output$facet_ui <- renderUI({
    radioButtons("facet_by", "Facet by: ", choices = c("Positive", "Negative"), 
                 selected = ifelse(values$lim == 0, "Negative", "Positive"))
  })
  
  output$limiting_factor <- renderPlotly({
    req(user$plot)
    df <- user$plot
    text1 <- paste0("Height: ", df$h, "\nFraction: ", pull(df, prop.of.data.p) %>% scales::percent(),
                    "\nRange: ", pull(df, perc.th.p), "\nType: Positive")
    plot_title <- paste0("Proportion of data found in homogeneous clusters of size ", user$minC, " or larger,\n", 
                         "looking at proportions >= color of the limiting factor")
    
    {ggplot(df, aes(x = h, y = prop.of.data.p, col = perc.th.p)) + 
        geom_point(aes(text = text1)) + geom_line() + 
        scale_y_continuous(labels = percent, limits = c(0,1)) + 
        xlab("\nHeight") + ylab("Fraction of population\n") + ggtitle(plot_title) + 
        theme_bw() + scale_color_grey() + 
        theme(plot.margin = unit(c(1.5,1,2,2), "cm"), 
              axis.title.x.top = element_text(margin = margin(t = 20)), 
              axis.title.y.right = element_text(margin = margin(r = 20)))} %>% 
      textSize() %>% ggplotly(., tooltip = c("text1"), source = "limitplot") %>% 
      event_register(., "plotly_click")
  })
  
  output$nonlimiting <- renderPlotly({
    req(user$input)
    s <- event_data("plotly_click", source = "limitplot")
    if (length(s)) {
      h <- s$x %>% as.character()
      df <- user$input
      pos_h <- seq(percRhs()/100, 1, by = stepRhs()) # sequence going from right hand side boundary to 1

      b <- df[,c(h,"Source")] %>% group_by_all() %>% count() %>%
        set_colnames(c("Clusters","source","num"))
      d <- aggregate(b$num, by = list(Clusters = b$Clusters), FUN = sum) %>% as_tibble()
      b2 <- left_join(b, d, by = "Clusters") %>%
        set_colnames(c("Clusters","Source","Freq","Size")) %>% ungroup()
      b2$Fraction <- b2$Freq / b2$Size
      b3 <- b2 %>% filter(Size >= user$minC) %>% filter(Fraction >= pos_h[s$curveNumber + 1])
      # b3 is the set of clusters whose proportions we consider.
      # We want to see what the other clusters look like, with respect to the 
      # non-limiting factor.
      
      b4 <- b2[!(b2$Clusters %in% b3$Clusters),]
      
      tmp <- b4 %>% select(Clusters, Size, Source, Fraction) %>% 
        spread(Source, Fraction) %>% 
        set_colnames(c("Clusters", "Size", "Limiting","Non-limiting"))
      tmp[is.na(tmp)] <- 0
      
      text1 <- paste0("Cluster: ", tmp$Clusters, "\nProportion of cluster: ", 
                      round(tmp$`Non-limiting`, digits = 3), "\nCluster size: ", tmp$Size)
      
      {ggplot(tmp, aes(x = Clusters, y = `Non-limiting`, color = `Non-limiting`, 
                       size = Size, text = text1)) + 
          ylab("Proportion of non-limiting factor") + 
          geom_point() + scale_color_gradient(low = "deepskyblue", high = "darkblue") + 
          ggtitle(paste0("Clusters not included in the above plot, and the proportion of ", 
                         "non-limiting factor in each cluster, at height ", h))} %>% 
        ggplotly(tooltip = "text1")
    }
  })
  
  observe({
    req(user$results)
    shinyjs::useShinyjs()
    enable('dnld_tbl')
  })
  
  output$plot_exp <- renderText({
    req(user$plot); req(input$facet_by)
    df <- user$plot
    
    facet_type <- switch(input$facet_by, 
      "Positive" = list(" or more ", colnames(df)[c(4,3)], colnames(df)[c(6,5)], " or less "), 
      "Negative" = list(" or less ", colnames(df)[c(6,5)], colnames(df)[c(4,3)], " or more ")) %>% 
      unlist()
    
    choices <- c("Positive", "Negative")
    nottype <- setdiff(choices, input$facet_by) %>% tolower()
    b1 <- df[1,] %>% pull(facet_type[3]) %>% as.numeric() %>% scales::percent()
    cx <- df %>% pull(facet_type[5]) %>% unique() %>% as.numeric() %>% scales::percent() %>% toString()
    
    paste0("The selected variable to facet by, ", tolower(input$facet_by), ", is indicated by the ", 
           "black line in the facets \nof the plot below. The colorful lines represent ", nottype, 
           " homogeneity. For example, in the \nfirst of the facets, the black line shows the ", 
           "fraction of the data found in clusters where \n", b1, facet_type[1], "of the cluster ", 
           "has the limiting factor. In the same facet, the lines in color show, \nrespectively, ", 
           "the fraction of the data found in clusters where ", cx, facet_type[6], "\nof the cluster", 
           " has the limiting factor.")
  })
  
  output$all_percents <- renderPlotly({
    req(user$plot); req(user$initial); req(input$facet_by)
    df <- user$plot
    plot_title <- paste0("Population of dataset found in homogeneous clusters of size ", user$minC, " or larger\n")
    # saveRDS(df, "temp.Rds")
    ftype <- switch(input$facet_by, 
      "Positive" = list(colnames(df)[c(4,3)], colnames(df)[c(6,5)], "negative"), 
      "Negative" = list(colnames(df)[c(6,5)], colnames(df)[c(4,3)], "positive")) %>% 
      unlist()
    
    text1 <- paste0("Height: ", df$h, "\nFraction: ", pull(df, ftype[1]) %>% scales::percent(), 
                    "\nRange: ", pull(df, ftype[2]), "\nType: ", input$facet_by)
    text2 <- paste0("Height: ", df$h, "\nFraction: ", pull(df, ftype[3]) %>% scales::percent(), 
                    "\nRange: ", pull(df, ftype[4]), "\nType: ", ftype[5])
    
    h_factor <- df %>% pull(ftype[2]) %>% n_distinct()
    
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
    user$results %>% asDT(filter = "top") %>% formatRound(columns = c(5,8), digits = 4)
  })
  
  output$dnld_tbl <- downloadHandler(
    filename = paste0("Homogeneity-both-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
    content = function(file) {
      write.table(user$results, file, sep = "\t", quote = FALSE, row.names = FALSE)
    })
  
}

