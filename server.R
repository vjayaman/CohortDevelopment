options(shiny.maxRequestSize=30*1024^2)
source("modules.R")

server <- function(input, output, session) {
  source("initialize_reactive_values.R", local = TRUE)

  # TAB 1 - INPUT DATA ----------------------------------------------------------------------------------------
  # Validate inputs
  source("explanations.R", local = TRUE) # errMsg(err_code) - validates inputs
  source("on_submit_check_inputs.R", local = TRUE)

  ## Datatable of heights and number of clusters for each
  output$num_clusters <- renderDT({
    req(inp$data, user$lim, inp$limiting, user$bin)
    # user$lim <- perfect clusters, with 100% being the lim
    # inp_limiting <- a1$Bin[which.min(a1$Freq)]
    
    df <- inp$data
    heights <- colnames(df)[-1][-1]         # list of heights in the input set
    numC <- apply(df[,3:ncol(df)], 2, FUN = n_distinct)           # (# of clusters at each height)
    
    # (# clusters with size >= minC, each height)
    numC_above <- apply(df[,3:ncol(df)], 2, FUN = function(x) {
      table(x) %>% as.data.frame() %>% filter(Freq >= inp$minC) %>% nrow()})  
    
    # % of pop in perfect clusters >= minC, and the # of such clusters, for lim
    # userlim <- inp_limiting %>% filterPerfect(inpdata, ., minC, c(0,1), "Var")
    lim <- lapply(heights, function(h) {
      df_x <- user$lim[[h]]
      inds_100 <- df_x$Size[which(df_x$Fraction == 1)]
      isolates <- sum(inds_100)
      tibble("heights" = h, "cl_100" = length(inds_100), 
        "props_100" = paste0(isolates, "/", nrow(df)), 
        "perc_100" = (isolates / nrow(df)) %>% percent(accuracy = 0.01) )
    }) %>% bind_rows()
    
    # summary table
    a <- c(paste0("Number of clusters with size >= ", inp$minC), 
           paste0("Proportion of population in clusters >= ", inp$minC, " with"), 
           paste0("Percent of population in clusters >= ", inp$minC, " with"), 
           tolower(inp$limiting))
    b1 <- tibble(heights, numC, numC_above)
    user$tbl <- b2 <- left_join(b1, lim, by = "heights") %>% 
      set_colnames(c("Heights", "Number of clusters", a[1], paste0(a[1], " with 100 % ", a[4]),
                     paste0(a[2], " 100 % ", a[4]), paste0(a[3], " 100 % ", a[4])))
    
    sketch = htmltools::withTags(table(
      class = 'display', 
      thead(
        tr(th(colspan = 3, "General"), th(colspan = 3, "Clusters with size >= 10")), 
        tr(lapply(c("Height", "Number of clusters", 
            paste0("Number of clusters with size >= ", inp$minC), 
            paste0("Number of clusters with 100 % ", tolower(inp$limiting)), 
            paste0("Proportion of population in clusters with 100 % ", tolower(inp$limiting)), 
            paste0("Percent of population in clusters with 100 % ", tolower(inp$limiting))), th))
    )))
    
    DT::datatable(b2, options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                     dom = "ti", pageLength = nrow(b2), scrollY = "500px"), 
                  rownames = FALSE, filter = "none", container = sketch, class = 'cell-border stripe', 
                  selection = list(target = "cell", mode = "single")) %>% 
      formatStyle(3:5, border = '1px solid #ddd')
  })

  # Mini table of cluster sizes
  source("new_table_on_cellclick.R", local = TRUE)
  
  # TAB 2 - PARAMETERS ----------------------------------------------------------------------------------------
  # When metrics of the input data are shown to the user, generate the UI components for "Parameters" tab
  source("generate_and_manage_tab2_ui.R", local = TRUE)
  
  # On "update" button-click, uses user inputs to generate the proportions and relevant data
  observeEvent(input$update, {
    req(inp$data, values$locus, input$num_or_prop)
    # stats of | Source (0/1) | Frequency in pop. | % of pop. | Type (Neg/Pos) | Size of pop.
    a1 <- inp$data[,2] %>% binaryStats(., user$pos, user$neg)
    h <- colnames(inp$data)[3:ncol(inp$data)] # all heights in the dataset

    withProgress(message = "Collecting metric data, going through thresholds: ", value = 0, {
      
      user$initial <-lapply(1:length(h), function(i) {
        incProgress(1/length(h), 
                    detail = paste0(round(i/length(h), digits=2)*100, "% done"))
        # | height | prop.clusters | homogeneity values | type (neg/pos) | num.of.clusters | prop.of.data
        globalMetrics(
          values$locus, h[i], inp$data, inp$minC, a1$Bin[which.min(a1$Freq)], 
          percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
      }) %>% bind_rows() %>% 
        set_colnames(c("h", "prop.cl", "perc.th", "th.type", "num.cl", "prop.of.data"))
    })
    
    # selects columns for neg. homogeneity, then renames
    df2 <- tableNames(user$initial, "neg", inp$minC)
    # selects columns for pos. homogeneity, renames, then merges with neg. table
    ur_tbl <- tableNames(user$initial, "pos", inp$minC) %>% 
      merge(., df2, by = colnames(df2)[1:2]) %>% as_tibble()
    user$results <- ur_tbl[ur_tbl %>% pull(1) %>% as.character() %>% as.numeric() %>% order(),]
    
    # selects columns, then converts as.num(Height), as.char(Pos/Neg Threshold)
    user$plot <- selectColsName(user$results, c(1:3,5,6,8))

    # type of plots (y = number of cluster or y = fraction of population)
    user$ptype <- input$num_or_prop

    # text blurb indicating user can click on points in plot
    # output$click_limplot <- renderText({blurb(type = "ClickPoint")})
    shinyjs::show(id = "facet_ui")
  })

  # When the data has been generated, use dataset size to define plot height
  # Then set up the UI components for the plot, datatable, and download button
  source("plots.R", local = TRUE)
  source("bubble_plots.R", local = TRUE)
  
  # output$plot_exp from explanations.R
  
  # Results datatable, in tab 2, with the heights and (perc,step) combinations, 
  # with relevant proportions and number of clusters
  output$results <- DT::renderDT({
    req(user$results)
    tableX <- user$results
    tableX$`Number of negative homogeneity clusters` %<>% paste0("<b>", ., "</b>")
    tableX$`Number of positive homogeneity clusters` %<>% paste0("<b>", ., "</b>")
    cols <- colnames(tableX)
    tableX[cols] <- lapply(tableX[cols], as.factor)
    #https://stackoverflow.com/questions/33180058/coerce-multiple-columns-to-factors-at-once
    tableX %>% 
      DT::datatable(options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                       dom = "ti", pageLength = nrow(tableX), scrollY = "500px"), 
                    rownames = FALSE, filter = "top", escape = FALSE, selection = "single") %>% 
      formatRound(columns = c(5,8), digits = 4)
  })
}
