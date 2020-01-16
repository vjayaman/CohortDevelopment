
# As soon as data has been loaded, generate the UI components for the next tab, "Parameters"
output$params <- renderMenu({
  req(inp$data)
  menuItem("Parameters", tabName = "tab-params", badgeLabel = "new", badgeColor = "green")
})

# Slider inputs and update and download buttons
output$param_ui <- renderUI({
  req(inp$data)
  pos_lbl <- paste0("Positive homogeneity (e.g. Consider clusters where >= ", 70, "% is ", tolower(user$pos), "):")
  neg_lbl <- paste0("Negative homogeneity (e.g. Consider clusters where <= ", 35, "% is ", tolower(user$pos), "):")
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

output$posStepSizeUI <- renderUI({
  req(inp$data)
  stepSliderInput("step_size_p", 100-percRhs(), 2)
})

output$negStepSizeUI <- renderUI({
  req(inp$data)
  stepSliderInput("step_size_n", percLhs(), 2)
})

# Modules - homogeneity percent thresholds and step sizes
percRhs <- callModule(percSlider, "rhs_perc")
stepRhs <- callModule(stepSlider, "step_size_p")

percLhs <- callModule(percSlider, "lhs_perc")
stepLhs <- callModule(stepSlider, "step_size_n")

# Pick facet variable for plot
output$facet_ui <- renderUI({
  radioButtons("facet_by", "Facet by: ", choices = c("Positive", "Negative"))})

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