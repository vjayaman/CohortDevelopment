
# When input data is first submitted, we check that the format is correct, and report with error messages
# (ID | Binary Variable | Thresholds...)
errMsg <- function(err_code) {
  if (err_code == 1) {
    "All heights must be numeric." %>% return()
  }else if (err_code == 2) {
    "The second column should \nbe have a non-numeric locus name." %>% return()
  }else if (err_code == 3) {
    "The first column should \nbe a list of genomes, with a \nnon-numeric heading." %>% return()
  }else if (err_code == 4) {
    "The locus data must \nbe binary." %>% return()
  }else if (err_code == 5) {
    "Invalid filetype \n(onlyaccepts tsv/txt)." %>% return()
  }else if (err_code == 6) {
    "There are one or \nmore empty cells in \nthe dataset." %>% return()
  }else if (err_code == 7) {
    "Minimum cluster size \nnot specified." %>% return()
  }else if (err_code == 0) {
    "Input data formatted \ncorrectly." %>% return()
  }
}

# This is the foundational text for each of the descriptions/explanations so far
blurb <- function(a = NULL, type) {
  
  if (type == "FacetedPlot") {
    paste0("The selected variable to facet by is ", a[1], ". It is indicated by the \n", 
           "black line in the facets of the plot below. The colorful lines represent \n", 
           a[2], " homogeneity. For example, in the first of the facets, the black \n", 
           "line shows the fraction of the data found in clusters where \n", 
           a[3], a[4], "of the cluster has the limiting factor. \n", 
           "In the same facet, the lines in color show, respectively, the \n", 
           "fraction of the data found in clusters where \n", 
           a[5], a[6], "of the cluster has the limiting factor.") %>% p() %>% return()
    
  }else if (type == "ClickCell") {
    paste0("Each of the outlined cells in the table below indicate a number of clusters. ", 
           "Selecting one of these cells results in a table of the corresponding ", 
           "clusters and their respective sizes.") %>% p() %>% return()
    
  }else if (type == "ClickPoint") {
    paste0("Click on a point in the plot immediately below to see the ", 
           "proportions in the rest of the dataset.") %>% return()
    
  }else if (type == "ClickRow") {
    paste0("(Click on a row in the table below to see the specifics of the clusters ", 
           "indicated in bold.)") %>% return()
    
  }else if (type == "PosHExp") {
    paste0("We have two pairs of slider inputs. In the first slider input, we select a percentage ", 
           "between 0 and 100, call this x. Directly below this input, we select a step size, s1. ", 
           "This will tell us the increment size, so we can see at which values we run the calculations. ", 
           "For example, if we pick x = 80 and s1 = 5 for the slider inputs for positive homogeneity, ", 
           "then we look for clusters at least as large as ", inp$minC, ", where at least 80% of the ", 
           "cluster has the limiting factor, then we repeat for 85%, 90%, 95%, and 100%. These are the ", 
           "size and homogeneity conditions. ") %>% p() %>% return()
    
  }else if (type == "NegHExp") {
    paste0("We have two pairs of slider inputs. In the first slider input, we select a percentage between ", 
           "0 and 100, call this y, and a step size s2. This will tell us the increment size. Suppose we ", 
           "say y = 30 and s2 = 10. Then we look for clusters at least as large as ", inp$minC, ", where ", 
           "no more than 30% of the cluster has the limiting factor, and then repeat for 20% and 10%. ") %>% 
      p() %>% return()    
    
  }else if (type == "ParamsExp") {
    paste0("Then, we select whether we want to look at the number of such clusters or the proportion of ", 
           "the population found in such clusters. In the former case, the plot below will show, at each ", 
           "height, the number of clusters that satisfy the described conditions. In the latter scenario, ", 
           "we can see the proportion of the entire population found in clusters satisfying the size and ", 
           "homogeneity conditions.") %>% p() %>% return()
  }
}

# Basic metrics of what the data looks like: the proportions of binary data, number of thresholds, ...
output$base_metrics <- renderText({
  validate(need(!is.null(inp$data), ""))
  a1 <- binaryStats(inp$data[,2])
  p <- a1$Bin==1
  n <- a1$Bin==0
  values$lim <- a1$Bin[which.min(a1$Freq)]
  
  user$lim <- values$lim %>% 
    filterPerfect(inp$data, ., inp$minC, c(0,1))
  
  user$nonlim <- setdiff(0:1,values$lim) %>% 
    filterPerfect(inp$data, ., inp$minC, c(0,1))
  
  paste0("Dataset size: ", nrow(inp$data), " samples\n", 
         "Number of heights: ", ncol(inp$data)-2, "\n", 
         "Proportion of binary variable: \n    ", 
         a1$Type[p], " (", values$posV, " == ", a1$Bin[p], "): ", a1$Freq[p], "/", a1$Tot[1], " = ", a1$percent[p], "\n    ", 
         a1$Type[n], " (", values$negV, " == ", a1$Bin[n], "): ", a1$Freq[n], "/", a1$Tot[1], " = ", a1$percent[n], "\n", 
         "Limiting factor: ", a1$Type[which.min(a1$Freq)], " (", values$lim, ")\n", 
         "We will be maximizing the proportion found \nin binary clusters for the limiting factor."
  )
})

# Brief explanation of the faceted plot on the Parameters tab, describing how the facet 
# variable is used and how the plot can be interpreted.
output$plot_exp <- renderUI({
  req(user$plot); req(input$facet_by)
  df <- user$plot
  cn <- colnames(df)
  # cn[c(4,3)] == Prop of pop in +'ve homogeneity clusters, +'ve threshold
  # cn[c(6,5)] == Prop of pop in -'ve homogeneity clusters, -'ve threshold
  facet_type <- switch(input$facet_by, 
    "Positive" = list(" or more ", cn[c(4,3)], cn[c(6,5)], " or less "), 
    "Negative" = list(" or less ", cn[c(6,5)], cn[c(4,3)], " or more ")) %>% unlist()
  
  nottype <- c("Positive", "Negative") %>% setdiff(., input$facet_by)
  
  b1 <- df[1,] %>% pull(facet_type[3]) %>% as.numeric() %>% scales::percent()
  cx <- df %>% pull(facet_type[5]) %>% unique() %>% as.numeric() %>% scales::percent() %>% toString()
  
  c(input$facet_by, nottype) %>% tolower() %>% 
    c(., b1, facet_type[1], cx, facet_type[6]) %>% 
    blurb(., "FacetedPlot")
})