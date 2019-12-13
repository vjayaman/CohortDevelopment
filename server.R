options(shiny.maxRequestSize=30*1024^2)
source("modules.R")

server <- function(input, output, session) {
  
  # Reactive values
  cell <- reactiveValues(row = NULL, col = NULL)
  values <- reactiveValues(locus = NULL, h = NULL, point = NULL, path = NULL, lim = NULL)
  user <- reactiveValues(input = NULL, results = NULL, initial = NULL, plot = NULL, 
                         tbl = NULL, lim = NULL, nonlim = NULL)
  inp <- reactiveValues(minC = NULL, data = NULL)
  
  # TAB 1 - INPUT DATA  # source('tab1_input_data.R', local = TRUE)
  # load data for processing
  observeEvent(input$data, {
    shinyjs::useShinyjs() 
    enable("submit")
  })
  
  # checking user's data for valid column names, file extension, etc. (ID | Binary Variable | Thresholds...)
  observeEvent(input$submit, {
    req(input$data)
    values$path <- input$data$datapath
    inp$minC <- input$minC
    
    output$check_input <- renderText({
      if (tools::file_ext(values$path) %in% c("tsv","txt")) {
        inp$data <- readData(values$path)
        validate(need(all(varhandle::check.numeric(colnames(inp$data)[-1][-1])), errMsg(1)),
                 need(!varhandle::check.numeric(colnames(inp$data)[2]), errMsg(2)),
                 need(!varhandle::check.numeric(colnames(inp$data)[1]), errMsg(3)),
                 need(all(unique(inp$data[,2]) %in% c(1,0)), errMsg(4)))
        values$locus <- colnames(inp$data)[2]
        errMsg(0)
      }else {errMsg(5)}
    })
  })
  
  # generating text output with basic overview of loaded data
  output$base_metrics <- renderText({
    validate(need(!is.null(inp$data), ""))
    a1 <- binaryStats(inp$data[,2])
    p <- a1$Bin==1
    n <- a1$Bin==0
    values$lim <- a1$Bin[which.min(a1$Freq)]
    user$lim <- filterPerfect(inp$data, values$lim, inp$minC, c(0,1))
    user$nonlim <- filterPerfect(inp$data, setdiff(0:1,values$lim), inp$minC, c(0,1))
    
    paste0("Dataset size: ", nrow(inp$data), " samples\nNumber of heights: ", ncol(inp$data)-2, 
           "\n", "Proportion of binary variable: \n    ", a1$Type[p], " (", a1$Bin[p], "): ", 
           a1$Freq[p], "/", a1$Tot[1], " = ", a1$percent[p], "\n    ", a1$Type[n], " (", a1$Bin[n], 
           "): ", a1$Freq[n], "/", a1$Tot[1], " = ", a1$percent[n], "\nLimiting factor: ", 
           a1$Type[which.min(a1$Freq)], " (", values$lim, ")\n", 
           "We will be maximizing the proportion found \nin binary clusters for the limiting factor."
    )
  })
  
  # datatable of heights and number of clusters for each
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
  
  output$click_cells <- renderText({
    validate(need(!is.null(inp$data), ""))
    blurb(type = "ClickCell")
  })
  
  # mini-table of clusters and sizes that result in the data of a single cell from the main table
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
  
  # TAB 2 - PARAMETERS
  # as soon as data has been loaded, generate the UI components for the next tab, "Parameters"
  observe({
    req(inp$data)
    shinyjs::show(id = "next_tab1")
    output$param_ui <- renderUI({
      tagList(
        fluidRow(
          column(width = 4, offset = 1, 
                 percSliderInput("rhs_perc", lbl = "Positive homogeneity (e.g. Cluster must have >= 80% be 1 to be counted):", val = 80), 
                 uiOutput("posStepSizeUI")), 
          column(width = 4, offset = 1, 
                 percSliderInput("lhs_perc", lbl = "Negative homogeneity (e.g. Cluster must have <= 10% be 1 to be counted):", val = 10),
                 uiOutput("negStepSizeUI"), 
                 actionButton("update", "Update inputs"), 
                 shinyjs::useShinyjs(),
                 disabled(downloadButton("dnld_tbl", "Download table")))))
    })
    output$posStepSizeUI <- renderUI({stepSliderInput("step_size_p", 100-percRhs())})
    output$negStepSizeUI <- renderUI({stepSliderInput("step_size_n", percLhs())})
  })
  
  # user can click on "next" button to switch to next tab
  observeEvent(input$next_tab1, {
    updateTabItems(session, inputId = "tabs", selected = "tab-params")
  })
  
  #   Modules
  percRhs <- callModule(percSlider, "rhs_perc")
  stepRhs <- callModule(stepSlider, "step_size_p")
  
  percLhs <- callModule(percSlider, "lhs_perc")
  stepLhs <- callModule(stepSlider, "step_size_n")
  
  # on "update" button-click, uses user inputs to generate the proportions and relevant data
  observeEvent(input$update, {
    req(inp$data, values$locus)
    a1 <- inp$data[,2] %>% binaryStats()
    h <- colnames(inp$data)[3:ncol(inp$data)]
    withProgress(message = "Collecting metric data: ", value = 0, {
      user$initial <-
        lapply(1:length(h), function(i) {
          incProgress(1/length(h),
                      detail = paste0("Going through thresholds, ", round(i/length(h), digits=2)*100, "% done"))
          globalMetrics(values$locus, h[i], inp$data, inp$minC, a1$Bin[which.min(a1$Freq)], 
                        percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
        }) %>% bind_rows() %>% set_colnames(., c("h", "prop.cl", "perc.th", "th.type", "num.cl", "prop.of.data"))
    })
    
    df1 <- tableNames(user$initial, "pos", inp$minC)
    df2 <- tableNames(user$initial, "neg", inp$minC)
    user$results <- merge(df1, df2, by = colnames(df2)[1:2]) %>% as_tibble()
    
    df <- user$results %>% select(1,2,3,5,6,8) %>% 
      set_colnames(c("h","prop.cl","perc.th.p","prop.of.data.p","perc.th.n","prop.of.data.n"))
    
    df$h <- as.numeric(df$h)
    df$perc.th.p <- as.character(df$perc.th.p)
    df$perc.th.n <- as.character(df$perc.th.n)
    user$plot <- df
    
    output$click_limplot <- renderText({blurb(type = "ClickPoint")})
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
    plot_title <- paste0("Proportion of data found in homogeneous clusters of size ", inp$minC, " or larger,\n", 
                         "looking at proportions >= color of the limiting factor")
    
    {ggplot(df, aes(x = h, y = prop.of.data.p, col = perc.th.p)) + geom_point(aes(text = text1)) + 
        geom_line() + scale_y_continuous(labels = percent, limits = c(0,1)) + 
        xlab("\nHeight") + ylab("Fraction of population\n") + ggtitle(plot_title) + theme_bw() + 
        scale_color_grey() + theme(plot.margin = unit(c(1.5,1,2,2), "cm"), 
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
          ylab("Proportion of non-limiting factor") + geom_point() + 
          scale_color_gradient(low = "deepskyblue", high = "darkblue") + 
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
    
    c(tolower(input$facet_by), nottype, b1, facet_type[1], cx, facet_type[6]) %>% blurb(., "FacetedPlot")
  })
  
  output$all_percents <- renderPlotly({
    req(user$plot); req(user$initial); req(input$facet_by)
    df <- user$plot
    plot_title <- paste0("Population of dataset found in homogeneous clusters of size ", inp$minC, " or larger\n")

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

