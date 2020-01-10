options(shiny.maxRequestSize=30*1024^2)
source("modules.R")

server <- function(input, output, session) {
  source("initialize_reactive_values.R", local = TRUE)

  # TAB 1 - INPUT DATA ----------------------------------------------------------------------------------------
  # Validate inputs
  source("explanations.R", local = TRUE) # errMsg(err_code) - validates inputs
  source("on_submit_check_inputs.R", local = TRUE)
  # output$base_metrics from explanations.R
  
  ## Datatable of heights and number of clusters for each
  output$num_clusters <- renderDT({
    validate(need(!is.null(inp$data), "")); validate(need(!is.null(user$lim), ""))
    
    df <- inp$data
    nonlimiting <- setdiff(0:1,values$lim)  # the nonlim, either 0 or 1
    heights <- colnames(df)[-1][-1]         # list of heights in the input set
    
    # (# of clusters at each height)
    numC <- apply(df[,3:ncol(df)], 2, FUN = n_distinct)
    
    # (# clusters with size >= minC, each height)
    numC_above <- apply(df[,3:ncol(df)], 2, FUN = function(x) {
      table(x) %>% as.data.frame() %>% filter(Freq >= inp$minC) %>% nrow()})  
    
    # user$lim <- perfect clusters, with 100% being the lim
    # % of pop in perfect clusters >= minC, and the # of such clusters, for lim
    lim <- lapply(heights, function(h) {
      tibble("perc" = user$lim[[h]]$Size %>% sum() %>% "/"(nrow(df)) %>% round(digits = 3), 
             "cl" = user$lim[[h]] %>% nrow())}) %>% bind_rows()
    
    # user$nonlim <- perfect clusters, with 100% being the nonlimiting factor
    # % of pop in perfect clusters >= minC, and the # of such clusters, for nonlim
    lim_not <- lapply(heights, function(h) {
      tibble("perc" = user$nonlim[[h]]$Size %>% sum() %>% "/"(nrow(df)) %>% round(digits = 3), 
             "cl" = user$nonlim[[h]] %>% nrow())}) %>% bind_rows()
    
    # summary table, with Heights | # of clusters | # of clusters with size >= minC |
    # | # of clusters >= minC with 100% / 0% lim | % of pop in clusters >= minC, with 100% / 0% lim |
    # | # of clusters >= minC with 100% / 0% non-lim | % of pop in clusters >= minC, with 100% / 0% non-lim |
    a <- c("Number of clusters","Percent of population in clusters >= "," with 100% or 0% being "," with size >= ")
    user$tbl <- tibble(heights, numC, numC_above) %>% 
      add_column(lim$cl, lim$perc, lim_not$cl, lim_not$perc) %>% 
      set_colnames(c("Heights", a[1], paste0(a[1], a[4], inp$minC), paste0(a[1], a[4], inp$minC, a[3], values$lim),
                     paste0(a[2], inp$minC, ",", a[3], values$lim), paste0(a[1], a[4], inp$minC, a[3], nonlimiting), 
                     paste0(a[2], inp$minC, ",", a[3], nonlimiting)))
    
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
    req(inp$data, values$locus, input$num_or_prop)
    
    # stats of | Source (0/1) | Frequency in pop. | % of pop. | Type (Neg/Pos) | Size of pop.
    a1 <- inp$data[,2] %>% binaryStats()
    h <- colnames(inp$data)[3:ncol(inp$data)] # all heights in the dataset
    
    withProgress(message = "Collecting metric data, going through thresholds: ", value = 0, {
      user$initial <-lapply(1:length(h), function(i) {
        incProgress(1/length(h), 
                    detail = paste0(round(i/length(h), digits=2)*100, "% done"))
        # | height | prop.clusters | homogeneity values | type (neg/pos) | num.of.clusters | prop.of.data
        globalMetrics(values$locus, h[i], inp$data, inp$minC, a1$Bin[which.min(a1$Freq)], 
                      percLhs()/100, percRhs()/100, stepLhs(), stepRhs())
      }) %>% bind_rows() %>% 
        set_colnames(c("h", "prop.cl", "perc.th", "th.type", "num.cl", "prop.of.data"))
    })
    
    # selects columns for neg. homogeneity, then renames
    df2 <- tableNames(user$initial, "neg", inp$minC)
    # selects columns for pos. homogeneity, renames, then merges with neg. table
    user$results <- tableNames(user$initial, "pos", inp$minC) %>% 
      merge(., df2, by = colnames(df2)[1:2]) %>% as_tibble()
    
    # selects columns, then converts as.num(Height), as.char(Pos/Neg Threshold)
    user$plot <- selectColsName(user$results, c(1:3,5,6,8))

    # type of plots (y = number of cluster or y = fraction of population)
    user$ptype <- input$num_or_prop
    
    # text blurb indicating user can click on points in plot
    output$click_limplot <- renderText({blurb(type = "ClickPoint")})
    shinyjs::show(id = "facet_ui")
  })
  
  # When the data has been generated, use dataset size to define plot height
  # Then set up the UI components for the plot, datatable, and download button

  source("plots.R", local = TRUE)
  
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
                                       dom = "ti", pageLength = nrow(df), scrollY = "500px"), 
                    rownames = FALSE, filter = "top", escape = FALSE, selection = "single") %>% 
      formatRound(columns = c(5,8), digits = 4)
  })
  
  # mini explanation - click on a row of the results table to get specifics about the indicated clusters
  output$click_results <- renderUI({
    validate(need(!is.null(user$results), ""))
    tagList(
      box(column(12, 
          fluidRow(blurb(type = "ClickRow")), br(), 
          fluidRow(downloadButton("dnld_results", "Download table")))))
  })
}
