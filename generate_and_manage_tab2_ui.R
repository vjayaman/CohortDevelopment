
# As soon as data has been loaded, generate the UI components for the next tab, "Parameters"
observe({
  req(inp$data)
  shinyjs::show(id = "next_tab1")
  
  # Slider inputs and update and download buttons
  output$param_ui <- renderUI({
    pos_lbl <- "Positive homogeneity (e.g. Cluster must have >= 80% be 1 to be counted):"
    neg_lbl <- "Negative homogeneity (e.g. Cluster must have <= 10% be 1 to be counted):"
    tagList(
      fluidRow(
        column(width = 4, offset = 1, 
          percSliderInput("rhs_perc", lbl = pos_lbl, val = 80), 
          uiOutput("posStepSizeUI")), 
        column(width = 4, offset = 1, 
          percSliderInput("lhs_perc", lbl = neg_lbl, val = 10),
          uiOutput("negStepSizeUI"), 
          actionButton("update", "Update inputs"), 
          shinyjs::useShinyjs(),
          disabled(downloadButton("dnld_tbl", "Download table")))
        )
      )
  })
  output$posStepSizeUI <- renderUI({stepSliderInput("step_size_p", 100-percRhs())})
  output$negStepSizeUI <- renderUI({stepSliderInput("step_size_n", percLhs())})
})

# User can click on "next" button to switch to next tab from initial ("inputs" tab)
observeEvent(input$next_tab1, {
  updateTabItems(session, inputId = "tabs", selected = "tab-params")})

# Modules - homogeneity percent thresholds and step sizes
percRhs <- callModule(percSlider, "rhs_perc")
stepRhs <- callModule(stepSlider, "step_size_p")

percLhs <- callModule(percSlider, "lhs_perc")
stepLhs <- callModule(stepSlider, "step_size_n")

# Pick facet variable for plot
output$facet_ui <- renderUI({
  radioButtons("facet_by", "Facet by: ", choices = c("Positive", "Negative"), 
               selected = ifelse(values$lim == 0, "Negative", "Positive"))})

# If the results for the datatable have been generated, enable the download button
observe({
  req(user$results)
  shinyjs::useShinyjs()
  enable('dnld_tbl')
})

# Download button: saves datatable info as "Homogeneity-both-<year>-<month>-<day>-<hours>-<minutes>.txt"
output$dnld_tbl <- downloadHandler(
  filename = paste0("Homogeneity-both-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$results, file, sep = "\t", quote = FALSE, row.names = FALSE)
})
