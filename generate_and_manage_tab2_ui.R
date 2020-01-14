
# As soon as data has been loaded, generate the UI components for the next tab, "Parameters"
observe({
  req(inp$data)
  output$params <- renderMenu({
    menuItem("Parameters", tabName = "tab-params", badgeLabel = "new", badgeColor = "green")
  })

  # Slider inputs and update and download buttons
  output$param_ui <- renderUI({
    pos_lbl <- "Positive homogeneity (e.g. Cluster must have >= 80% be 1 to be counted):"
    neg_lbl <- "Negative homogeneity (e.g. Cluster must have <= 10% be 1 to be counted):"
    tagList(
      fluidRow(
        box(width = 6, 
            percSliderInput("rhs_perc", lbl = pos_lbl, val = 70), uiOutput("posStepSizeUI")), 
        box(width = 6, 
            percSliderInput("lhs_perc", lbl = neg_lbl, val = 35), uiOutput("negStepSizeUI"))), 
      
      fluidRow(
        box(width = 6, title = "Positive homogeneity", blurb(type = "PosHExp"), collapsible = TRUE, collapsed = TRUE), 
        box(width = 6, title = "Negative homogeneity", blurb(type = "NegHExp"), collapsible = TRUE, collapsed = TRUE)), 
      
      fluidRow(
        box(width = 12, 
            fluidRow(
              column(width = 3, radioButtons("num_or_prop", "Type of plot: ", 
                                             choices = c("Number of clusters" = "num", "Fraction of population" = "prop"))), 
              column(width = 9, blurb(type = "ParamsExp"))))), 
      
      fluidRow(column(width = 2, offset = 10, actionBttn("update", "Update inputs"))), br()
    )
  })
  output$posStepSizeUI <- renderUI({stepSliderInput("step_size_p", 100-percRhs(), 2)})
  output$negStepSizeUI <- renderUI({stepSliderInput("step_size_n", percLhs(), 2)})
})

# Modules - homogeneity percent thresholds and step sizes
percRhs <- callModule(percSlider, "rhs_perc")
stepRhs <- callModule(stepSlider, "step_size_p")

percLhs <- callModule(percSlider, "lhs_perc")
stepLhs <- callModule(stepSlider, "step_size_n")

# Pick facet variable for plot
output$facet_ui <- renderUI({
  radioButtons("facet_by", "Facet by: ", choices = c("Positive", "Negative"),
               selected = ifelse(values$lim == 0, "Negative", "Positive"))})

# Download button: saves datatable info as "Homogeneity-both-<year>-<month>-<day>-<hours>-<minutes>.txt"
output$dnld_results <- downloadHandler(
  filename = paste0("Homogeneity-both-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$results, file, sep = "\t", quote = FALSE, row.names = FALSE)
})

output$dnldB_final <- renderUI({
  validate(need(!is.null(user$final), ""))
  # box(width = 2, )
  downloadButton("dnld_final", "Download table")
})