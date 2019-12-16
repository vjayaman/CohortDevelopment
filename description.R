blurb <- function(a = NULL, type) {
  if (type == "FacetedPlot") {
    paste0("The selected variable to facet by is ", a[1], ". It is indicated by the \n", 
           "black line in the facets of the plot below. The colorful lines represent \n", 
           a[2], " homogeneity. For example, in the first of the facets, the black \n", 
           "line shows the fraction of the data found in clusters where \n", 
           a[3], a[4], "of the cluster has the limiting factor. \n", 
           "In the same facet, the lines in color show, respectively, the \n", 
           "fraction of the data found in clusters where \n", 
           a[5], a[6], "of the cluster has the limiting factor.") %>% return()  
  }else if (type == "ClickCell") {
    "Click on one of the outlined cells to see the clusters and sizes that result in that field" %>% return()
  }else if (type == "ClickPoint") {
    paste0("Click on a point in the plot immediately below to see the ", 
           "proportions in the rest of the dataset.") %>% return()
  }
}

# Generating text output with basic overview of loaded data
output$base_metrics <- renderText({
  validate(need(!is.null(inp$data), ""))
  a1 <- binaryStats(inp$data[,2])
  p <- a1$Bin==1
  n <- a1$Bin==0
  values$lim <- a1$Bin[which.min(a1$Freq)]
  user$lim <- filterPerfect(inp$data, values$lim, inp$minC, c(0,1))
  user$nonlim <- filterPerfect(inp$data, setdiff(0:1,values$lim), inp$minC, c(0,1))
  
  paste0("Dataset size: ", nrow(inp$data), " samples\nNumber of heights: ", ncol(inp$data)-2, 
         "\n", "Proportion of binary variable: \n    ", a1$Type[p], " (", a1$Bin[p], "): ", 
         a1$Freq[p], "/", a1$Tot[1], " = ", a1$percent[p], "\n    ", a1$Type[n], " (", a1$Bin[n], 
         "): ", a1$Freq[n], "/", a1$Tot[1], " = ", a1$percent[n], "\nLimiting factor: ", 
         a1$Type[which.min(a1$Freq)], " (", values$lim, ")\n", 
         "We will be maximizing the proportion found \nin binary clusters for the limiting factor."
  )
})

output$plot_exp <- renderText({
  req(user$plot); req(input$facet_by)
  df <- user$plot
  
  facet_type <- switch(input$facet_by, 
    "Positive" = list(" or more ", colnames(df)[c(4,3)], colnames(df)[c(6,5)], " or less "), 
    "Negative" = list(" or less ", colnames(df)[c(6,5)], colnames(df)[c(4,3)], " or more ")) %>% unlist()
  
  choices <- c("Positive", "Negative")
  nottype <- setdiff(choices, input$facet_by) %>% tolower()
  b1 <- df[1,] %>% pull(facet_type[3]) %>% as.numeric() %>% scales::percent()
  cx <- df %>% pull(facet_type[5]) %>% unique() %>% as.numeric() %>% scales::percent() %>% toString()
  
  c(tolower(input$facet_by), nottype, b1, facet_type[1], cx, facet_type[6]) %>% blurb(., "FacetedPlot")
})