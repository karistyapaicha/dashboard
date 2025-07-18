# StatInsight Pro - Combined Application
# Dashboard Analisis Statistik Terpadu
# Author: AI Assistant
# Version: 2.0

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
library(reshape2)

# Source combined UI and Server files
source("ui_combined.R")
source("server_combined.R")

# Run the application
shinyApp(ui = ui, server = server)