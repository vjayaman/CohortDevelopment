# library(packrat)
# packrat::on()
x <- c('shiny','magrittr', 'tibble','dplyr','ggplot2','reshape2', 
       'data.table','DT','RColorBrewer','tidyr','plotly','purrr', 
       'shinyWidgets', 'shinyjs','varhandle', 'shinydashboard','scales', 
       'Hmisc')
lapply(x, require, character.only = TRUE)

# Global variables (keep these at a minimum) -------------------------------------------------

pos_color_scheme <- c("#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", 
                      "#a50f15", "#67000d") %>% rev() %>% colorRampPalette(.)
neg_color_scheme <- c("#9ecae1", "#6baed6", "#4292c6", "#2171b5", 
                      "#08519c", "#08306b") %>% colorRampPalette(.)

# Modular inputs -----------------------------------------------------------------------------
percSliderInput <- function(id, lbl, val) {
  ns <- NS(id)
  sliderInput(ns("perc_inp"), label = lbl, min = 1, max = 100, step = 1, value = val)
}

stepSliderInput <- function(id, maxval, selected = NULL) {
  ns <- NS(id)
  sliderInput(ns("step_size"), "Step size: ", min = 1, max = maxval, 
              value = ifelse(is.null(selected), maxval, selected), step = 1)
}

# Base functions -----------------------------------------------------------------------------
# # Table of: | Bin (0, 1) | Freq (frequency of each in the population) 
# #           | percent (percent of the population, so Freq/Total) 
# #           | Type (Negative, Positive) | Total (size of population) | 
# binaryStats <- function(df.col, pos, neg) {
#   a1 <- df.col %>% table() %>% as.data.frame() %>% set_colnames(c("Bin","Freq"))
#   tot <- sum(a1$Freq)
#   a1$percent <- (a1$Freq/tot) %>% scales::percent()
#   a1$Type[a1$Bin %in% pos] <- "Positive"
#   a1$Type[a1$Bin %in% neg] <- "Negative"
#   a1$Tot <- sum(a1$Freq)
#   return(a1)
# }
# Table of: | Bin (0, 1) | Freq (frequency of each in the population) 
#           | percent (percent of the population, so Freq/Total) 
#           | Type (Negative, Positive) | Total (size of population) | 
binaryStats <- function(df.col, pos, neg) {
  df.x <- unlist(df.col)
  inds <- df.x %in% pos
  a1 <- tibble("Bin" = c(toString(pos), toString(neg)), 
               "Freq" = c(df.x[inds] %>% length(), df.x[!inds] %>% length()))
  a1 %>% add_column("percent" = (a1$Freq/sum(a1$Freq)) %>% scales::percent()) %>% 
    add_column("Type" = c("Positive", "Negative")) %>% 
    add_column(Tot = sum(a1$Freq)) %>% return()
}

# Table of: | cluster number | cluster size | limiting factor (1 or 0) 
# | WP (within-cluster proportion, as a character and a number)
alleleBinCounts <- function(cl.calls, c.i, cohort.lim) {
  calls.of.cluster <- cl.calls %>% filter(clusters == c.i)    # select genomes and source vals for given cluster
  nr <- nrow(calls.of.cluster)                                # nr is the size of the cluster
  freqs <- calls.of.cluster %>% select(locus) %>% table()     # frequencies of the binary variable in cluster
  numerator <- 0
  if (!is.na(freqs[cohort.lim])) {numerator <- freqs[cohort.lim]}
  tibble(cluster = c.i, size = nr, cohort.lim, 
         within.prop = paste(numerator, nr, sep = "/"), 
         WP = numerator / nr) %>% return()
}

# Table of | val (e.g. 0.8, 1.0, 0.0, 0.1) | type (e.g. pos, pos, neg, neg)
# Each of the percent homogeneity thresholds we need to evaluate for
homogeneityTypes <- function(lhs, rhs, stepLhs, stepRhs) {
  pos_h <- seq(rhs, 1, by = stepRhs) # sequence going from right hand side boundary to 1
  neg_h <- seq(0, lhs, by = stepLhs) # sequence going from 0 to left hand side boundary
  
  # "type" indicates positive/negative homogeneity, for the given cohort indicator (1 or 0)
  data.frame(val = c(pos_h, neg_h), 
             type = c(rep("pos", length(pos_h)), rep("neg", length(neg_h))), 
             stringsAsFactors = FALSE) %>% return()
}

# Given a table of homogeneity types and values, and one of clusters and WPs, 
# return a list of tables, one for each WP threshold value
homogeneityTables <- function(th, key.cl) {
  lapply(1:nrow(th), function(r) {
    if (th$type[r] == "pos") {
      key.cl %>% filter(WP >= th$val[r]) %>% add_column(type = "Positive")
    }else {
      key.cl %>% filter(WP <= th$val[r]) %>% add_column(type = "Negative")
    }
  }) %>% set_names(th$val) %>% return()
}

# gene <- "Source"; h <- '0'; df <- readData("data/FNC_MBS_Example.tsv"); minC <- 10;
# cohort.lim <- 1; lhs <- 0.35; rhs <- 0.70; stepLhs <- 0.05; stepRhs <- 0.02
# Full dataset -------------------------------------------------------------------------------
globalMetrics <- function(gene, h, df, minC, cohort.lim, lhs, rhs, stepLhs, stepRhs) {
  # df of | id | locus (with binary data just for one cohort - the limiting one) | clusters for a single height
  cl.calls <- df %>% select(colnames(df)[1], gene, h) %>% 
    set_colnames(c("id","locus","clusters"))
  
  # All clusters, table of | Cluster | Size | Cohort.lim = 1 or 0 (only the limiting) | WP (fraction, then dec)
  mets <- cl.calls$clusters %>% unique() %>% 
    lapply(., function(cluster) alleleBinCounts(cl.calls, cluster, cohort.lim)) %>% bind_rows()
  tot.size <- sum(mets$size)            # Size of population
  
  # Clusters of size >= user-set minimum
  key.cl <- mets %>% filter(size >= minC)
  prop.clusters <- nrow(key.cl) %>% paste0(., "/", nrow(mets))
  
  th <- homogeneityTypes(lhs, rhs, stepLhs, stepRhs) # homogeneity types with corresponding values (e.g. 0.30 | neg)

  # If the "type" is pos, calculate the fraction of the population in clusters with WP >= the given 
  # positive homogeneity value (e.g. >= 95 % of the cluster has x), else: WP <= the given negative 
  # homogeneity value (e.g. <= 25 % of the cluster has x)
  # do for all clusters with size >= minC, and note that x is the limiting factor (1 or 0), return list
  cl_tbl <- homogeneityTables(th, key.cl)
  
  # how many clusters >= minC have a WP w/n the specified range?
  th$num.of.clusters <- as.character(th$val) %>% 
    lapply(., function(r) cl_tbl[[r]] %>% nrow()) %>% unlist()
  
  # what is the fraction of the pop. w/n the specified range and w/n clusters >= minC?
  th$prop.of.data <- as.character(th$val) %>% 
    lapply(., function(r) {
      dfx <- cl_tbl[[r]] %>% pull(size)
      ifelse(length(dfx) == 0, 0, sum(dfx)/tot.size) %>% return()
    }) %>% unlist()
  
  # Table of | height | prop.clusters | homogeneity values | type (neg/pos) | num.of.clusters | prop.of.data
  data.frame(h, prop.clusters, th, stringsAsFactors = FALSE) %>% return()
}

# Plot details, increasing text size and maneuvering legend to below the plot 
textSize <- function(p, incl.legend = FALSE) {
  if (isTRUE(incl.legend)) {
    p + theme(
      strip.text.y = element_text(margin = margin(0,2,0,2)), strip.text = element_text(size = 14), 
      axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 13), 
      title = element_text(size = 14), legend.position = "bottom", legend.text = element_text(size = 12))
  }else {
    p + theme(
      strip.text.y = element_text(margin = margin(0,2,0,2)), strip.text = element_text(size = 14), 
      axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 13), 
      title = element_text(size = 14), legend.position = "bottom", legend.title = element_blank(), 
      legend.text = element_text(size = 12))
  }
}

# Add refined column names to associated plots, e.g. user$results table
tableNames <- function(df, type, minC) {
  fulltype <- ifelse(type == "pos", "Positive", "Negative")
  df %>% filter(th.type == type) %>% select(-th.type) %>% 
    set_colnames(c("Height", paste0("Proportion of clusters with size >= ", minC), 
        paste0(fulltype, " threshold"), 
        paste0("Number of ", tolower(fulltype), " homogeneity clusters"), 
        paste0("Proportion of population in ", tolower(fulltype), " homogeneity clusters"))) %>% 
    return()
}

# Basic as.data.table setup, given a dataframe, center all column elements, 
# remove filtering unless specified, remove pagination and rownames, and allow y-scrolling
asDT <- function(df, filter_opt = "none") {
  DT::datatable(df, rownames = FALSE, filter = filter_opt, 
                options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), 
                               dom = "ti", pageLength = nrow(df), scrollY = "500px"))
}

perfClusters <- function(df, h, source_col) {
  b <- df[,c(h,source_col)] %>% group_by_all() %>% count() %>% 
    set_colnames(c("Clusters","source","num"))
  d <- aggregate(b$num, by = list(Clusters = b$Clusters), FUN = sum) %>% as_tibble()
  b2 <- left_join(b, d, by = "Clusters") %>% set_colnames(c("Clusters",source_col,"Freq","Size"))
  b2$Fraction <- b2$Freq/b2$Size
  return(b2)
}

# List of tables of | Clusters | Source | Freq | Size | Fraction, one table per height
filterPerfect <- function(df, type, minC, perc, source_col) {
  heights <- colnames(df)[-1][-1]
  lapply(heights, function(h) {
    perfClusters(df,h,source_col) %>% 
      filter((get(source_col) %in% type) & Size>=minC & (Fraction %in% perc))
  }) %>% set_names(heights) %>% return()
}

readData <- function(datapath) {
  read.table(datapath, header=TRUE, sep="\t", 
             check.names=FALSE, stringsAsFactors=FALSE, fill = TRUE) %>% return()
}

selectColsName <- function(df, cols) {
  dfx <- df %>% select(cols)
  dfx$Height <- as.character(dfx$Height)
  dfx$`Positive threshold` <- as.character(dfx$`Positive threshold`)
  dfx$`Negative threshold` <- as.character(dfx$`Negative threshold`)
  return(dfx)
}

pointText <- function(df, cols, val) {
  paste0("Height: ", pull(df,cols[1]), "\nFraction: ", pull(df, cols[2]) %>% scales::percent(), 
         "\nRange: ", pull(df, cols[3]), "\nType: ", val) %>% return()
}

scale_manual <- function(aesthetics, values, name, ...) {
  scale_color_manual(
    aesthetics = aesthetics, values = values, name = name, ...)
}
