# Modules for the binary variable app

percSlider <- function(input, output, session) {
  reactive({input$perc_inp})
}

stepSlider <- function(input, output, session) {
  reactive({input$step_size/100})
}
