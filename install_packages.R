# StatInsight Pro - Package Installation Script
# This script installs all required packages for the dashboard

cat("🚀 StatInsight Pro - Package Installation\n")
cat("==========================================\n\n")

# List of required packages
required_packages <- c(
  # Core Shiny packages
  "shiny",
  "shinydashboard", 
  "shinycssloaders",
  "shinyWidgets",
  
  # Data manipulation and visualization
  "DT",
  "plotly",
  "ggplot2",
  "dplyr",
  "leaflet",
  
  # Data import/export
  "readr",
  "readxl",
  
  # Statistical analysis
  "car",
  "nortest",
  "broom",
  "psych",
  "lmtest",
  
  # Report generation
  "knitr",
  "rmarkdown",
  "officer",
  "flextable",
  
  # Additional utilities
  "corrplot",
  "VIM",
  "mice"
)

# Function to install packages if not already installed
install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    cat(paste("📦 Installing", package_name, "...\n"))
    tryCatch({
      install.packages(package_name, dependencies = TRUE)
      cat(paste("✅", package_name, "installed successfully!\n"))
    }, error = function(e) {
      cat(paste("❌ Error installing", package_name, ":", e$message, "\n"))
    })
  } else {
    cat(paste("✅", package_name, "already installed\n"))
  }
}

# Install all required packages
cat("Installing required packages...\n\n")
for (package in required_packages) {
  install_if_missing(package)
}

cat("\n==========================================\n")
cat("🎉 Installation Complete!\n\n")

# Check if all packages are available
cat("Checking package availability...\n")
missing_packages <- c()

for (package in required_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, package)
  }
}

if (length(missing_packages) == 0) {
  cat("✅ All packages are successfully installed and loaded!\n")
  cat("\n📋 Next steps:\n")
  cat("1. Run: shiny::runApp() to start the dashboard\n")
  cat("2. Open your web browser and navigate to the displayed URL\n")
  cat("3. Upload your data and start analyzing!\n\n")
  cat("🎯 StatInsight Pro is ready to use!\n")
} else {
  cat("❌ The following packages failed to install:\n")
  for (package in missing_packages) {
    cat(paste("  -", package, "\n"))
  }
  cat("\n🔧 Please try installing these packages manually:\n")
  cat("install.packages(c(", paste(paste0('"', missing_packages, '"'), collapse = ", "), "))\n")
}

# Additional setup for specific packages
cat("\n📝 Additional Notes:\n")
cat("- For PDF report generation, you may need to install LaTeX (e.g., TinyTeX)\n")
cat("- If you encounter issues with 'car' package, try: install.packages('car', type='source')\n")
cat("- For optimal performance, ensure R version >= 4.0.0\n")

cat("\n🆘 If you encounter any issues:\n")
cat("1. Update R to the latest version\n")
cat("2. Update RStudio to the latest version\n")
cat("3. Clear package cache: remove.packages() and reinstall\n")
cat("4. Check your internet connection\n")

cat("\n🎨 StatInsight Pro Dashboard Features:\n")
cat("✨ Data Management & Transformation\n")
cat("📊 Descriptive Statistics & Visualization\n")
cat("🔍 Statistical Assumption Testing\n")
cat("📈 Inferential Statistics (t-tests, ANOVA, etc.)\n")
cat("📊 Multiple Linear Regression\n")
cat("💾 Comprehensive Download Options\n")
cat("🎯 Automated Interpretation\n")

cat("\n==========================================\n")
cat("Happy analyzing! 📊✨\n")