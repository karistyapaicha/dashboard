# StatInsight Pro - Dashboard Launcher
# Quick launcher script for the dashboard

cat("🚀 Starting StatInsight Pro Dashboard...\n")
cat("=====================================\n\n")

# Check if required packages are installed
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", "dplyr")
missing_packages <- c()

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("❌ Missing required packages:\n")
  for (pkg in missing_packages) {
    cat(paste("  -", pkg, "\n"))
  }
  cat("\n🔧 Please run: source('install_packages.R') first\n")
  cat("Then try running this script again.\n")
  stop("Missing required packages")
}

# Load required libraries
cat("📦 Loading required packages...\n")
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
})

cat("✅ All packages loaded successfully!\n\n")

# Check if UI and Server files exist
if (!file.exists("ui.R")) {
  stop("❌ ui.R file not found! Please ensure all files are in the correct directory.")
}

if (!file.exists("server.R")) {
  stop("❌ server.R file not found! Please ensure all files are in the correct directory.")
}

cat("📋 Dashboard Information:\n")
cat("========================\n")
cat("🎯 Name: StatInsight Pro\n")
cat("📊 Features: Comprehensive Statistical Analysis\n")
cat("🔍 Includes: Data Management, Visualization, Statistical Tests, Regression\n")
cat("💾 Export: PDF, Word, CSV, Images, and Package Downloads\n\n")

cat("🌐 Dashboard will open in your default web browser\n")
cat("🔗 URL will be displayed below\n")
cat("⏹️  To stop the dashboard, press Ctrl+C (or Cmd+C on Mac)\n\n")

cat("🎉 Launching StatInsight Pro...\n")
cat("===============================\n\n")

# Run the Shiny app
tryCatch({
  runApp(
    appDir = ".",
    port = getOption("shiny.port", 3838),
    host = getOption("shiny.host", "127.0.0.1"),
    launch.browser = TRUE
  )
}, error = function(e) {
  cat("❌ Error launching dashboard:\n")
  cat(paste("Error message:", e$message, "\n"))
  cat("\n🔧 Troubleshooting tips:\n")
  cat("1. Check if all required files are present\n")
  cat("2. Ensure all packages are installed\n")
  cat("3. Try running: source('install_packages.R')\n")
  cat("4. Check your R version (>= 4.0.0 recommended)\n")
})

cat("\n👋 Thank you for using StatInsight Pro!\n")
cat("📧 For support, please check the README.md file\n")