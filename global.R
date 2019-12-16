x <- c('shiny','magrittr', 'tibble','dplyr','ggplot2','reshape2', 
       'data.table','DT','RColorBrewer','tidyr','plotly','purrr', 
       'shinyWidgets', 'shinyjs','varhandle', 'shinydashboard','scales')
lapply(x, require, character.only = TRUE)

# Modular inputs -----------------------------------------------------------------------------
percSliderInput <- function(id, lbl, val) {
  ns <- NS(id)
  sliderInput(ns("perc_inp"), label = lbl, min = 1, max = 100, step = 1, value = val)
}

stepSliderInput <- function(id, maxval) {
  ns <- NS(id)
  sliderInput(ns("step_size"), "Step size: ", min = 1, max = maxval, value = maxval, step = 1)
}

thresholdDTInput <- function(id) {
  ns <- NS(id)
  DTOutput(ns("pos_neg"))
}

# Base functions -----------------------------------------------------------------------------
binaryStats <- function(df.col) {
  a1 <- df.col %>% table() %>% as.data.frame() %>% set_colnames(c("Bin","Freq"))
  tot <- sum(a1$Freq)
  a1$percent <- (a1$Freq/tot) %>% scales::percent()
  pos <- a1$Bin==1
  neg <- a1$Bin==0
  a1$Type[pos] <- "Positive"
  a1$Type[neg] <- "Negative"
  a1$Tot <- sum(a1$Freq)
  return(a1)
}

alleleBinCounts <- function(cl.calls, c.i, cohort.lim) {
  calls.of.cluster <- cl.calls[cl.calls$clusters == c.i,]
  nr <- nrow(calls.of.cluster)
  freqs <- table(calls.of.cluster[,"locus"])
  numerator <- 0
  if (!is.na(freqs[cohort.lim])) {numerator <- freqs[cohort.lim]}
  # Returns: cluster number, cluster size, cohort.lim 1, WP
  tibble(cluster = c.i, size = nr, cohort.lim, 
         within.prop = paste(numerator, nr, sep = "/"), WP = numerator / nr) %>% return()
}

homogeneityTypes <- function(lhs, rhs, stepLhs, stepRhs) {
  pos_h <- seq(rhs, 1, by = stepRhs) # sequence going from right hand side boundary to 1
  neg_h <- seq(0, lhs, by = stepLhs) # sequence going from 0 to left hand side boundary
  
  # "type" indicates positive/negative homogeneity, for the given cohort indicator (1 or 0)
  data.frame(val = c(pos_h, neg_h), 
             type = c(rep("pos", length(pos_h)), rep("neg", length(neg_h))), 
             stringsAsFactors = FALSE) %>% return()
}

# Given a table of homogeneity types and values, and one of clusters and WPs, return a list of tables,
# one for each WP threshold value
homogeneityTables <- function(th, key.cl) {
  lapply(1:nrow(th), function(r) {
    if (th$type[r] == "pos") {
      key.cl %>% filter(WP >= th$val[r])
    }else {
      key.cl %>% filter(WP <= th$val[r])
    }
  }) %>% set_names(th$val) %>% return()
}

# Full dataset -------------------------------------------------------------------------------
globalMetrics <- function(gene, h, id.gene.clusters, minC, cohort.lim, lhs, rhs, stepLhs, stepRhs) {
  # df of | id | locus (with binary data just for one cohort - the limiting one) | clusters for a single height
  cl.calls <- id.gene.clusters[,c(colnames(id.gene.clusters)[1],gene,h)] %>% 
    set_colnames(., c("id", "locus", "clusters"))
  
  # all clusters, table of | Cluster | Size | Cohort.lim = 1 or 0 (only the limiting) | WP (fraction, then dec)
  mets <- cl.calls$clusters %>% unique() %>% 
    lapply(., function(cluster) alleleBinCounts(cl.calls, cluster, cohort.lim)) %>% bind_rows()
  
  # clusters of size >= user-set minimum
  key.cl <- mets[mets$size>=minC,]
  prop.clusters <- paste0(nrow(key.cl), "/", nrow(mets))
  
  th <- homogeneityTypes(lhs, rhs, stepLhs, stepRhs)
  
  # if the "type" is pos, calculate the fraction of the population in clusters with WP >= the given 
  # positive homogeneity value (e.g. >= 95 % of the cluster has x), else: WP <= the given negative 
  # homogeneity value (e.g. <= 25 % of the cluster has x)
  # do for all clusters with size >= minC, and note that x is the limiting factor (1 or 0)
  cl_tbl <- homogeneityTables(th, key.cl)
  
  # for each homogeneity threshold (positive or negative), how many clusters of size >= minC have a WP 
  # within the specified range?
  num_cl <- lapply(as.character(th$val), function(r) cl_tbl[[r]] %>% nrow()) %>% unlist() %>% 
    data.frame("val" = th$val, "num.of.clusters" = ., stringsAsFactors = FALSE)
  
  # size of population
  tot.size <- sum(mets$size)
  # for each homogeneity threshold (positive or negative), what is the fraction of the population
  # within the specified range and within clusters of size >= minC?
  prop_cl <- lapply(as.character(th$val), function(r) {
    df <- cl_tbl[[r]]$size
    ifelse(length(df)==0, 0, sum(df)/tot.size) %>% return()
  }) %>% unlist() %>% data.frame("val" = th$val, "prop.of.data" = ., stringsAsFactors = FALSE)

  # table of | height | prop.clusters | homogeneity values | type (neg/pos) | num.of.clusters | prop.of.data
  th %>% merge(num_cl) %>% merge(prop_cl) %>% 
    data.frame(h, prop.clusters, ., stringsAsFactors = FALSE) %>% return()
}

# Plot details -------------------------------------------------------------------------------
textSize <- function(p, incl.legend) {
  p + theme(
    strip.text.y = element_text(margin = margin(0,2,0,2)), 
    strip.text = element_text(size = 14), 
    axis.text.y = element_text(size = 13), 
    axis.text.x = element_text(size = 13), 
    title = element_text(size = 14), 
    legend.position = "bottom", legend.title = element_blank(), 
    legend.text = element_text(size = 12))
}

# Datatable setup ----------------------------------------------------------------------------
tableNames <- function(df, type, minC) {
  if (type == "pos") {fulltype <- "Positive"}else {fulltype <- "Negative"}
  df[df$th.type==type,] %>% select(-th.type) %>%
    set_colnames(., 
      c("Height", paste0("Proportion of clusters with size >= ", minC), 
        paste0(fulltype, " threshold"), 
        paste0("Number of ", tolower(fulltype), " homogeneity clusters"), 
        paste0("Proportion of population in ", tolower(fulltype), " homogeneity clusters"))) %>% return()
}

asDT <- function(df, filter_opt = "none") {
  df %>% 
    DT::datatable(options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                                 dom = "ti", pageLength = nrow(df), scrollY = "500px"), 
                  rownames = FALSE, filter = filter_opt)
}

# Error messages for user inputs -------------------------------------------------------------
errMsg <- function(err_code) {
  if (err_code == 1) {
    "All heights must be numeric." %>% return()
  }else if (err_code == 2) {
    "The second column should \nbe have a non-numeric locus name." %>% return()
  }else if (err_code == 3) {
    "The first column should \nbe a list of genomes, with a \nnon-numeric heading." %>% return()
  }else if (err_code == 4) {
    "The locus data must be \nbinary, 1 for the positive \ncohort, 0 otherwise." %>% return()
  }else if (err_code == 5) {
    "Invalid filetype \n(onlyaccepts tsv/txt)." %>% return()
  }else if (err_code == 0) {
    "Input data formatted \ncorrectly." %>% return()
  }
}

perfClusters <- function(df, minC, h, lim) {
  b <- df[,c(h,"Source")] %>% group_by_all() %>% count() %>% set_colnames(c("Clusters","source","num"))
  d <- aggregate(b$num, by = list(Clusters = b$Clusters), FUN = sum) %>% as_tibble()
  b2 <- left_join(b, d, by = "Clusters") %>% set_colnames(c("Clusters","Source","Freq","Size"))
  b2$Fraction <- b2$Freq/b2$Size
  return(b2)
}

filterPerfect <- function(df, type, minC, perc) {
  heights <- colnames(df)[-1][-1]
  lapply(heights, function(h) {
    perfClusters(df,minC,h,type) %>% 
      filter(Source==type & Size>=minC & (Fraction %in% perc))
  }) %>% set_names(heights) %>% return()
}

readData <- function(datapath) {
  read.table(datapath, header = TRUE, sep = "\t", 
             check.names = FALSE, stringsAsFactors = FALSE) %>% return()
}

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

