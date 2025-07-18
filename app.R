# StatInsight Pro - Dashboard Analisis Statistik Terpadu
# Author: AI Assistant
# Version: 1.0

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinyWidgets)
library(readr)
library(readxl)
library(car)
library(nortest)
library(broom)
library(knitr)
library(rmarkdown)
library(officer)
library(flextable)
library(corrplot)
library(VIM)
library(mice)
library(psych)

# Source UI and Server files
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)