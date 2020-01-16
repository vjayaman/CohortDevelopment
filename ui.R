require(plotly)
require(shinyWidgets)
require(shinydashboard)
require(shinyjs)
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", 
    menuItem("Input data", tabName = "tab-inputs"), 
    menuItemOutput("params")
  )
)

body <-   dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tabItems(
    tabItem(
      tabName = "tab-inputs",
      fluidRow(
        column(width = 12,
               box(width = 12,
                   column(width = 3, 
                          fileInput("data", "Data (in form of ID | Locus | Cluster assignments): ", 
                                    multiple = FALSE, accept = c("text/tsv", ".tsv", ".txt"), 
                                    buttonLabel = "Browse"), 
                          numericInput("minC", label = "Minimum cluster size: ", value = 10)),
                   column(width = 2,
                          verbatimTextOutput("check_input", placeholder = TRUE),
                          actionButton("check_validity", "Check input validity", style = "font-size: 16px;")),
                   column(width = 3, 
                          shinyjs::useShinyjs(),
                          uiOutput("posValUI"), uiOutput("negValUI"), 
                          hidden(actionButton("submit", "Submit", style = "font-size: 16px;"))),
                   column(width = 4,
                          verbatimTextOutput("base_metrics", placeholder = TRUE),
                          tags$head(tags$style(HTML("#base_metrics {font-size: 16px;}"))))
                   )
               )
      ),
      fluidRow(uiOutput("click_cells")),
      fluidRow(column(width = 8, DTOutput("num_clusters")), 
               column(width = 4, DTOutput("cluster_info")))
    ),
    tabItem(
      tabName = "tab-params", 
      fluidRow(column(width = 10, offset = 1, uiOutput("param_ui"))),
            
      fluidRow(column(width = 10, offset = 1,  
                      verbatimTextOutput("click_limplot"), 
                      plotlyOutput("limiting_factor", width = "100%", height = "700px"), 
                      plotlyOutput("negative_bubble", width = "100%", height = "650px"), 
                      plotlyOutput("positive_bubble", width = "100%", height = "650px"))), tags$br(), 
      
      fluidRow(column(width = 10, offset = 1, 
                      box(width = 2, shinyjs::hidden(uiOutput("facet_ui"))), 
                      box(width = 10, uiOutput("plot_exp"), collapsible = TRUE))), tags$br(), 

      fluidRow(column(width = 10, offset = 1, 
                      plotlyOutput("all_percents", width = "100%", height = "800px"))), 
      tags$br(), tags$br(), 
            
      fluidRow(column(width = 10, offset = 1, box(width = 12, uiOutput("click_results")))), 
            
      fluidRow(column(width = 10, offset = 1, DTOutput("results"))), tags$br(), tags$br(), 
            
      fluidRow(column(width = 10, offset = 1, uiOutput("dnldB_final"), DTOutput("final")))
      )
    )
)

dashboardPage(dashboardHeader(), sidebar, body)
