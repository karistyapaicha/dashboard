# StatInsight Pro - Simplified Application
# Dashboard Analisis Statistik Terpadu
# Author: AI Assistant
# Version: 2.0 (Simplified)

# Load only the successfully installed libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(readr)
library(readxl)
library(broom)
library(mice)

# Create a simplified UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = "StatInsight Pro - Dashboard Analisis Statistik Terpadu",
    titleWidth = 450
  ),
  
  # Sidebar Menu
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      
      # Beranda
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      
      # Data Management
      menuItem("📊 Manajemen Data", tabName = "data_management", icon = icon("database"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
        fluidRow(
          box(
            title = "Selamat Datang di StatInsight Pro",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3("Dashboard Analisis Statistik Terpadu"),
            p("Aplikasi ini menyediakan berbagai fitur analisis statistik yang komprehensif.")
          )
        )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data_management",
        fluidRow(
          box(
            title = "Upload Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fileInput("file", "Pilih File Data",
                     accept = c(".csv", ".xlsx", ".xls")),
            
            conditionalPanel(
              condition = "input.file != null",
              DTOutput("data_table")
            )
          )
        )
      )
    )
  )
)

# Create a simplified server
server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    data = NULL
  )
  
  # File upload and data loading
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv") {
      values$data <- read.csv(input$file$datapath, header = TRUE)
    } else if(ext %in% c("xlsx", "xls")) {
      values$data <- readxl::read_excel(input$file$datapath)
    }
  })
  
  # Data table output
  output$data_table <- renderDT({
    req(values$data)
    
    datatable(values$data, 
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                dom = 'Bfrtip'
              ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)