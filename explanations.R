
# When input data is first submitted, we check that the format is correct, and report with error messages
# (ID | Binary Variable | Thresholds...)
errMsg <- function(err_code) {
  if (err_code == 1) {
    "All heights must be numeric." %>% return()
  }else if (err_code == 2) {
    "The second column should \n have a non-numeric locus name." %>% return()
  }else if (err_code == 3) {
    "The first column should \nbe a list of genomes, with a \nnon-numeric heading." %>% return()
  }else if (err_code == 4) {
    "The data should \nbe binary." %>% return()
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
    paste0("The selected variable to facet by is ", a[1], ". It is indicated by the \nblack line in ", 
           "the facets of the plot below. The colorful lines represent \n", a[2], " homogeneity. For ", 
           "example, in the first of the facets, the black \nline shows the fraction of the data found ", 
           "in clusters where \n", a[3], a[4], "of the cluster has the limiting factor. \nIn the same ", 
           "facet, the lines in color show, respectively, the \nfraction of the data found in clusters ", 
           "where \n", a[5], a[6], "of the cluster has the limiting factor.") %>% p() %>% return()
    
  }else if (type == "ClickCell") {
    paste0("Each of the outlined cells in the table below indicate a number of clusters. Selecting one ", 
           "of these cells results in a table of the corresponding clusters and their respective sizes.") %>% 
      p() %>% return()
    
  }else if (type == "ClickPoint") {
    paste0("Click on a point in the plot immediately below to see the proportions in the ", 
           "rest of the dataset.") %>% return()
    
  }else if (type == "ClickRow") {
    paste0("(Click on a row in the table below to see the specifics of the clusters ", 
           "indicated in bold.)") %>% return()
    
  }else if (type == "PosHExp") {
    paste0("In the first slider input, we select a percentage between 0 and 100, call this x. Directly ", 
           "below this input, we select a step size, s1. This will tell us the increment size, so we can ", 
           "see at which values we run the calculations. For example, if we pick x = 80 and s1 = 5 for the ", 
           "slider inputs for positive homogeneity, then we look for clusters at least as large as ", inp$minC, 
           ", where at least 80% of the cluster has the limiting factor, then we repeat for 85%, 90%, 95%, ", 
           "and 100%. These are the size and homogeneity conditions. ") %>% p() %>% return()
    
  }else if (type == "NegHExp") {
    paste0("We have two pairs of slider inputs. In the first slider input, we select a percentage between ", 
           "0 and 100, call this y, and a step size s2. This will tell us the increment size. Suppose we ", 
           "say y = 30 and s2 = 10. Then we look for clusters at least as large as ", inp$minC, ", where ", 
           "no more than 30% of the cluster has the limiting factor, and then repeat for 20% and 10%. ") %>% 
      p() %>% return()    
    
  }else if (type == "ParamsExp") {
    paste0("We select whether we want to look at the number of such clusters or the proportion of ", 
           "the population found in such clusters. In the former case, the plot below will show, at each ", 
           "height, the number of clusters that satisfy the described conditions. In the latter scenario, ", 
           "we can see the proportion of the entire population found in clusters satisfying the size and ", 
           "homogeneity conditions.") %>% p() %>% return()
  }
}
