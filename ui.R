require(plotly); require(shinyWidgets); require(shinydashboard)
require(shinyjs);require(relayer)

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
                   column(width = 5, 
                          fluidRow(
                            column(7, 
                                   fileInput("data", "Data (ID | Binary variable | Cluster assignments): ", 
                                             multiple = FALSE, accept = c("text/tsv", ".tsv", ".txt"), 
                                             buttonLabel = "Browse")),
                            column(5, style = "margin-top: 26px;",
                                   verbatimTextOutput("check_input", placeholder = TRUE))
                          ), 
                          fluidRow(
                            column(7, 
                                   fileInput("metadata", "Metadata (ID | Metadata fields ...): ", 
                                             multiple = FALSE, accept = c("text/tsv", ".tsv", ".txt"), 
                                             buttonLabel = "Browse")), 
                            column(5, style = "margin-top: 26px;", 
                                   verbatimTextOutput("check_md", placeholder = TRUE))
                          )), 
                   column(width = 3, 
                          numericInput("minC", label = "Minimum cluster size: ", value = 10), 
                          shinyjs::useShinyjs(),
                          uiOutput("metadataColumnUI"), 
                          uiOutput("posValUI"), 
                          hidden(actionButton("submit", "Submit", style = "font-size: 16px;"))), 
                   column(width = 4,
                          verbatimTextOutput("base_metrics", placeholder = TRUE),
                          tags$head(tags$style(HTML("#base_metrics {font-size: 16px;}"))), 
                          verbatimTextOutput("removed_rows", placeholder = TRUE),
                          tags$head(tags$style(HTML("#removed_rows {font-size: 16px;}"))))
               )
        )
      ),
      fluidRow(uiOutput("click_cells")),
      fluidRow(column(width = 8, DTOutput("num_clusters"), DTOutput("metadata_table")), 
               column(width = 4, DTOutput("cluster_info")))
    ),
    tabItem(
      tabName = "tab-params", 
      fluidRow(column(width = 10, offset = 1, uiOutput("param_ui"))),
            
      fluidRow(column(width = 10, offset = 1,  
                      plotOutput("limiting_factor", width = "100%", height = "800px"), 
                      uiOutput("close_up_lines"), tags$br(), tags$br())), tags$br(), 
      
      fluidRow(column(width = 10, offset = 1,  
                      uiOutput("select_height"), 
                      plotOutput("bubble_plot", width = "100%", height = "650px"),
                      uiOutput("close_up_bubbles")
                      )), tags$br(), tags$br(), tags$br(), 
            
      fluidRow(column(width = 10, offset = 1, box(width = 12, uiOutput("click_results")))), 
            
      fluidRow(column(width = 10, offset = 1, DTOutput("results"))), tags$br(), tags$br(), 
            
      fluidRow(column(width = 10, offset = 1, 
                      box(width = 12, uiOutput("dnldB_final"), uiOutput("metadata_exp")), 
                      DTOutput("final"), DTOutput("selected_clusters")))
      )
    )
)

dashboardPage(dashboardHeader(), sidebar, body)
