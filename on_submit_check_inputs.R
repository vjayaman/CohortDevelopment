
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
      validate(
        need(
          all(varhandle::check.numeric(colnames(inp$data)[-1][-1])), 
          errMsg(1)        # All heights must be numeric
        ),
        need(
          !varhandle::check.numeric(colnames(inp$data)[2]), 
          errMsg(2)        # The second column should be have a non-numeric locus name
        ),
        need(
          !varhandle::check.numeric(colnames(inp$data)[1]), 
          errMsg(3)        # The first column should be a list of genomes, with a non-numeric heading
        ),
        need(
          all(unique(inp$data[,2]) %in% c(1,0)), 
          errMsg(4)        # The locus data must be binary
        )
      )
      values$locus <- colnames(inp$data)[2]
      errMsg(0)            # Invalid filetype (onlyaccepts tsv/txt)
    }else {
      errMsg(5)            # Input data formatted correctly
    }
  })
})

# When data has been loaded into the app, enables the "Submit" button
# observeEvent(input$data, {
#   shinyjs::useShinyjs()
#   enable("check")
#   # enable("submit")
# })
# 
# observeEvent(input$check, {
#   req(input$data)
#   values$path <- input$data$datapath
#   inp$minC <- input$minC
# 
#   if (!(input$posV %in% "")) {values$posV <- input$posV}
#   if (!(input$negV %in% "")) {values$negV <- input$negV}
#   print("step1")
#   
#   output$check_input <- renderText({
#     print("step2")
#     validate(
#       need(
#         (values$posV == as.character(1) & values$negV == as.character(0)) | 
#           (all(c(values$posV, values$negV) %in% "")), 
#         "No further changes necessary."  
#       )
#     )
#     "Positive and negative values must be selected."
#   })
# })



