
# As soon as data has been loaded, generate the UI components for the next tab, "Parameters"
output$params <- renderMenu({
  req(inp$data)
  menuItem("Parameters", tabName = "tab-params", badgeLabel = "new", badgeColor = "green")
})

# Slider inputs and update and download buttons
output$param_ui <- renderUI({
  req(inp$data)
  pos_lbl <- paste0("Variable of interest >= ___ % (positive homogeneity)")
  neg_lbl <- paste0("Variable of interest <= ___ % (negative homogeneity)")
  tagList(
    fluidRow(
      box(width = 4, 
          percSliderInput("rhs_perc", lbl = pos_lbl, val = 70), uiOutput("posStepSizeUI")), 
      box(width = 4, 
          percSliderInput("lhs_perc", lbl = neg_lbl, val = 35), uiOutput("negStepSizeUI")), 
      verbatimTextOutput("data_summary", placeholder = TRUE)), 
    
    fluidRow(
      box(width = 6, title = paste0("Positive homogeneity"), blurb(type = "PosHExp"), 
          collapsible = TRUE, collapsed = TRUE), 
      box(width = 6, title = paste0("Negative homogeneity"), blurb(type = "NegHExp"), 
          collapsible = TRUE, collapsed = TRUE)), 
    
    fluidRow(
      box(width = 12, 
          fluidRow(
            column(width = 3, radioButtons("num_or_prop", "Type of plot: ", 
                                           choices = c("Number of clusters" = "num", "Fraction of population" = "prop"))), 
            column(width = 9, blurb(type = "ParamsExp"))))), 
    
    fluidRow(column(width = 2, offset = 10, actionBttn("update", "Update inputs"))), br()
  )
})

output$data_summary <- renderText({
  req(basic$data)
  paste0(basic$data)
})
output$posStepSizeUI <- renderUI({
  req(inp$data)
  stepSliderInput("step_size_p", 100-percRhs(), 10)
})

output$negStepSizeUI <- renderUI({
  req(inp$data)
  stepSliderInput("step_size_n", percLhs(), 10)
})

output$close_up_bubbles <- renderUI({
  tagList(
    tags$br(), tags$br(), tags$br(), 
    box(width = 10, collapsible = TRUE, 
        checkboxGroupInput("int_plots", selected = NULL, 
                           "Interactive plots of positive and negative homogeneity clusters, respectively", 
                           choices = c("Positive" = "pos_plot", "Negative" = "neg_plot"))), 
    tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), 
    plotlyOutput("positive_bubble", width = "100%", height = "650px"), 
    tags$br(), tags$br(), 
    plotlyOutput("negative_bubble", width = "100%", height = "650px")
  )
})

# Modules - homogeneity percent thresholds and step sizes
percRhs <- callModule(percSlider, "rhs_perc")
stepRhs <- callModule(stepSlider, "step_size_p")

percLhs <- callModule(percSlider, "lhs_perc")
stepLhs <- callModule(stepSlider, "step_size_n")

# Download button: saves datatable info as "Homogeneity-both-<year>-<month>-<day>-<hours>-<minutes>.txt"
output$dnld_results <- downloadHandler(
  filename = paste0("Homogeneity-both-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$results, file, sep = "\t", quote = FALSE, row.names = FALSE)
})

output$dnldB_final <- renderUI({
  req(user$final)
  downloadButton("dnld_final", "Download table")
})