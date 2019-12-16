
# When data has been loaded into the app, enables the "Submit" button
observeEvent(input$data, {
  shinyjs::useShinyjs() 
  enable("submit")
})

# On submit-click, user input data is check for validity: valid column names, file extension, etc. 
# (ID | Binary Variable | Thresholds...)
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
    }else {
      errMsg(5)
    }
  })
})
