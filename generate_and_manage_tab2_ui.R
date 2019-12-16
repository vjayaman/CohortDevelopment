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
  updateTabItems(session, inputId = "tabs", selected = "tab-params")})

# Modules
percRhs <- callModule(percSlider, "rhs_perc")
stepRhs <- callModule(stepSlider, "step_size_p")

percLhs <- callModule(percSlider, "lhs_perc")
stepLhs <- callModule(stepSlider, "step_size_n")

output$facet_ui <- renderUI({
  radioButtons("facet_by", "Facet by: ", choices = c("Positive", "Negative"), 
               selected = ifelse(values$lim == 0, "Negative", "Positive"))})

observe({
  req(user$results)
  shinyjs::useShinyjs()
  enable('dnld_tbl')
})

output$dnld_tbl <- downloadHandler(
  filename = paste0("Homogeneity-both-", format(Sys.time(), format = "%Y-%m-%d-%H-%M"),".txt"),
  content = function(file) {
    write.table(user$results, file, sep = "\t", quote = FALSE, row.names = FALSE)
  })