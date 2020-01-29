
# The variables that are updated and used throughout the app

# cell$row <- selected height in the first height table on the first ("input") tab
# cell$col <- selected column " " (associated with positive/negative homogeneity)
cell <- reactiveValues(row = NULL, col = NULL)

# values$locus <- the name of the second column in the user's input data (e.g. "Source", or "Locus")
# values$path <- the datapath to where the user's input data can be accessed
# values$lim <- 0 or 1, whichever is the limiting factor
values <- reactiveValues(locus = NULL, path = NULL)

# user$results <- table with columns:
#   Height, Proportion of clusters with size >= minC (a user-set minimum cluster size), 
#   Positive threshold (e.g. 0.8, 0.85, ...), Number of positive homogeneity clusters, 
#   Proportion of population in positive homogeneity clusters, 
#   Negative threshold (e.g. 0.10, 0.15, ...), Number of negative homogeneity clusters, 
#   Proportion of population in negative homogeneity clusters

# user$initial <- table with columns:
#   height, prop. of clusters of size >= user-set minimum, homogeneity values, type (neg/pos), 
#   num.of.clusters, proportion of population

# user$plot <- table with columns: 
#   h, prop.cl, perc.th.p, prop.of.data.p, perc.th.n, prop.of.data.n
#   Basically, user$initial split into separate columns by positivity or negativity

# user$tbl <- table with columns:
#   Heights, Number of clusters, Number of clusters with size >= minC, 
#   Number of clusters with size >= minC with 100% or 0% being lim (the limiting factor), 
#   Percent of population in clusters >= minC, with 100% or 0% being lim, 
#   Number of clusters with size >= minC with 100% or 0% being nonlim (the non-limiting factor), 
#   Percent of population in clusters >= minC, with 100% or 0% being nonlim

# user$lim <- list of tables, one for each height, with columns
#   Clusters | Source | Freq | Size | Fraction

# user$nonlim <- same as user$lim, but for the nonlimiting factor instead
user <- reactiveValues(input = NULL, results = NULL, initial = NULL, plot = NULL, 
                       tbl = NULL, lim = NULL, nonlim = NULL, ptype = NULL, final = NULL, 
                       pos = NULL, neg = NULL, bin = NULL)

# inp$minC <- user-set minimum cluster size
# inp$data <- user's input data
inp <- reactiveValues(minC = NULL, data = NULL, type1 = NULL, type2 = NULL, limiting = NULL)

plots <- reactiveValues(bubble_data = NULL, bubble_title = NULL)
