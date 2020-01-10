require(plotly)
require(shinyWidgets)
require(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", 
    menuItem("Input data", tabName = "tab-inputs"), 
    menuItemOutput("params")
  )
)

body <-   dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tabItems(
    tabItem(tabName = "tab-inputs", 
            fluidRow(
              box(column(width = 4, 
                         fileInput("data", "Data (in form of ID | Locus | Cluster assignments): ", 
                                   multiple = FALSE, accept = c("text/tsv", ".tsv", ".txt"), 
                                   buttonLabel = "Browse")),
                  column(width = 4, 
                         numericInput("minC", label = "Minimum cluster size: ", value = 10), 
                         shinyjs::useShinyjs(), 
                         disabled(actionButton("submit", "Submit", style = "font-size: 16px;"))), 
                  column(width = 4, 
                         verbatimTextOutput("check_input"))), 
              column(width = 5, 
                     verbatimTextOutput("base_metrics"), 
                     tags$head(tags$style(HTML("#base_metrics {font-size: 16px;}"))))), 
            
            fluidRow(uiOutput("click_cells")),
            
            fluidRow(column(width = 8, 
                            DTOutput("num_clusters")), 
                     column(width = 3, 
                            DTOutput("cluster_info")))
          ),
    
    tabItem(tabName = "tab-params", 
            fluidRow(column(width = 10, offset = 1, uiOutput("param_ui"))),
            
            fluidRow(column(width = 10, offset = 1,  
                            verbatimTextOutput("click_limplot"), 
                            plotlyOutput("limiting_factor", width = "100%", height = "700px"), 
                            plotlyOutput("negative_bubble", width = "100%", height = "650px"), 
                            plotlyOutput("positive_bubble", width = "100%", height = "650px"))), tags$br(), 
            
            fluidRow(column(width = 2, offset = 1, 
                            shinyjs::hidden(uiOutput("facet_ui"))), 
                     column(width = 6, 
                            verbatimTextOutput("plot_exp"))), tags$br(), 
            
            fluidRow(column(width = 10, offset = 1, 
                            plotlyOutput("all_percents", width = "100%", height = "800px"))), tags$br(), tags$br(), 
            
            fluidRow(column(width = 10, offset = 1, 
                            uiOutput("click_results"), DTOutput("results"))), tags$br(), tags$br(), 
            
            fluidRow(column(width = 10, offset = 1, 
                            uiOutput("click_final"), 
                            DTOutput("final")))
          )
  )
)

dashboardPage(dashboardHeader(), sidebar, body)
# plotlyOutput("nonlimiting_bubble", width = "100%", height = "650px")