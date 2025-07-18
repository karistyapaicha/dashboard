# StatInsight Pro - Combined Server File
# Server logic for Dashboard Analisis Statistik Terpadu

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

# Server function
server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    data = NULL,
    original_data = NULL,
    metadata = NULL,
    results = list()
  )
  
  # File upload and data loading
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv") {
      values$data <- read.csv(input$file$datapath, 
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
    } else if(ext %in% c("xlsx", "xls")) {
      values$data <- read_excel(input$file$datapath)
    }
    
    values$original_data <- values$data
    
    # Update variable choices
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    categorical_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    all_vars <- names(values$data)
    
    updateSelectInput(session, "transform_var", choices = all_vars)
    updateSelectInput(session, "desc_vars", choices = numeric_vars)
    updateSelectInput(session, "group_var", choices = c("None" = "", categorical_vars))
    updateSelectInput(session, "x_var", choices = all_vars)
    updateSelectInput(session, "y_var", choices = numeric_vars)
    updateSelectInput(session, "color_var", choices = c("None" = "", categorical_vars))
    updateSelectInput(session, "assumption_var", choices = numeric_vars)
    updateSelectInput(session, "assumption_group", choices = c("None" = "", categorical_vars))
    updateSelectInput(session, "mean_test_var", choices = numeric_vars)
    updateSelectInput(session, "group_var_mean", choices = categorical_vars)
    updateSelectInput(session, "paired_var2", choices = numeric_vars)
    updateSelectInput(session, "prop_var", choices = categorical_vars)
    updateSelectInput(session, "prop_group_var", choices = categorical_vars)
    updateSelectInput(session, "var_test_var", choices = numeric_vars)
    updateSelectInput(session, "var_group_var", choices = categorical_vars)
    updateSelectInput(session, "anova_dependent", choices = numeric_vars)
    updateSelectInput(session, "anova_factor1", choices = categorical_vars)
    updateSelectInput(session, "anova_factor2", choices = categorical_vars)
    updateSelectInput(session, "reg_dependent", choices = numeric_vars)
    updateSelectInput(session, "reg_independent", choices = numeric_vars)
    
    # Update proportion category choices
    if(length(categorical_vars) > 0) {
      observe({
        req(input$prop_var)
        if(input$prop_var %in% names(values$data)) {
          categories <- unique(values$data[[input$prop_var]])
          updateSelectInput(session, "prop_category", choices = categories)
        }
      })
    }
    
    # Check for geographic data
    lat_vars <- names(values$data)[grepl("lat|latitude", names(values$data), ignore.case = TRUE)]
    lon_vars <- names(values$data)[grepl("lon|longitude|lng", names(values$data), ignore.case = TRUE)]
    
    if(length(lat_vars) > 0 && length(lon_vars) > 0) {
      updateSelectInput(session, "lat_var", choices = lat_vars)
      updateSelectInput(session, "lon_var", choices = lon_vars)
      updateSelectInput(session, "marker_var", choices = all_vars)
    }
  })
  
  # Reactive outputs for conditional panels
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  
  output$dataLoaded <- reactive({
    return(!is.null(values$data))
  })
  
  output$hasGeoData <- reactive({
    if(is.null(values$data)) return(FALSE)
    lat_vars <- names(values$data)[grepl("lat|latitude", names(values$data), ignore.case = TRUE)]
    lon_vars <- names(values$data)[grepl("lon|longitude|lng", names(values$data), ignore.case = TRUE)]
    return(length(lat_vars) > 0 && length(lon_vars) > 0)
  })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  outputOptions(output, "hasGeoData", suspendWhenHidden = FALSE)
  
  # Value boxes for dashboard
  output$total_observations <- renderValueBox({
    if(is.null(values$data)) {
      valueBox(
        value = 0,
        subtitle = "Total Observasi",
        icon = icon("table"),
        color = "blue"
      )
    } else {
      valueBox(
        value = nrow(values$data),
        subtitle = "Total Observasi",
        icon = icon("table"),
        color = "blue"
      )
    }
  })
  
  output$total_variables <- renderValueBox({
    if(is.null(values$data)) {
      valueBox(
        value = 0,
        subtitle = "Total Variabel",
        icon = icon("columns"),
        color = "green"
      )
    } else {
      valueBox(
        value = ncol(values$data),
        subtitle = "Total Variabel",
        icon = icon("columns"),
        color = "green"
      )
    }
  })
  
  output$numeric_vars <- renderValueBox({
    if(is.null(values$data)) {
      valueBox(
        value = 0,
        subtitle = "Variabel Numerik",
        icon = icon("calculator"),
        color = "yellow"
      )
    } else {
      valueBox(
        value = sum(sapply(values$data, is.numeric)),
        subtitle = "Variabel Numerik",
        icon = icon("calculator"),
        color = "yellow"
      )
    }
  })
  
  output$categorical_vars <- renderValueBox({
    if(is.null(values$data)) {
      valueBox(
        value = 0,
        subtitle = "Variabel Kategorik",
        icon = icon("tags"),
        color = "red"
      )
    } else {
      valueBox(
        value = sum(sapply(values$data, function(x) is.character(x) || is.factor(x))),
        subtitle = "Variabel Kategorik",
        icon = icon("tags"),
        color = "red"
      )
    }
  })
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    if(is.null(values$data)) return(NULL)
    
    metadata <- data.frame(
      Variabel = names(values$data),
      Tipe = sapply(values$data, class),
      Missing = sapply(values$data, function(x) sum(is.na(x))),
      Unique = sapply(values$data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(metadata, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    if(is.null(values$data)) return("No data loaded")
    
    numeric_data <- values$data[sapply(values$data, is.numeric)]
    if(ncol(numeric_data) > 0) {
      summary(numeric_data)
    } else {
      "No numeric variables found"
    }
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    if(is.null(values$data)) return(NULL)
    
    DT::datatable(values$data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Data transformation
  observeEvent(input$apply_transform, {
    req(values$data, input$transform_var, input$transform_type)
    
    var_data <- values$data[[input$transform_var]]
    
    if(input$transform_type == "categorize" && is.numeric(var_data)) {
      if(input$cat_method == "interval") {
        breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), 
                     length.out = input$n_categories + 1)
      } else if(input$cat_method == "quantile") {
        breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
      }
      
      values$data[[paste0(input$transform_var, "_cat")]] <- cut(var_data, breaks = breaks, include.lowest = TRUE)
      
    } else if(input$transform_type == "log" && is.numeric(var_data)) {
      values$data[[paste0(input$transform_var, "_log")]] <- log(var_data + 1)
      
    } else if(input$transform_type == "sqrt" && is.numeric(var_data)) {
      values$data[[paste0(input$transform_var, "_sqrt")]] <- sqrt(var_data)
      
    } else if(input$transform_type == "scale" && is.numeric(var_data)) {
      values$data[[paste0(input$transform_var, "_scaled")]] <- scale(var_data)[,1]
      
    } else if(input$transform_type == "normalize" && is.numeric(var_data)) {
      values$data[[paste0(input$transform_var, "_norm")]] <- (var_data - min(var_data, na.rm = TRUE)) / 
        (max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE))
    }
    
    # Update variable choices
    all_vars <- names(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    categorical_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    
    updateSelectInput(session, "transform_var", choices = all_vars)
    updateSelectInput(session, "desc_vars", choices = numeric_vars)
    updateSelectInput(session, "x_var", choices = all_vars)
    updateSelectInput(session, "y_var", choices = numeric_vars)
  })
  
  # Transform interpretation
  output$transform_interpretation <- renderUI({
    if(is.null(values$data)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi Transformasi Data</h4>",
      "<p><strong>Transformasi yang diterapkan:</strong> Berhasil menambahkan variabel baru hasil transformasi.</p>",
      "<p><strong>Manfaat:</strong> Transformasi data dapat membantu memenuhi asumsi statistik dan meningkatkan kualitas analisis.</p>",
      "<p><strong>Saran:</strong> Periksa distribusi data setelah transformasi untuk memastikan hasil yang diinginkan.</p>"
    ))
  })
  
  # Descriptive statistics
  observeEvent(input$run_descriptive, {
    req(values$data, input$desc_vars)
    
    if(input$group_var == "" || is.null(input$group_var)) {
      # No grouping
      desc_data <- values$data[input$desc_vars]
      values$results$descriptive <- describe(desc_data)
    } else {
      # With grouping
      desc_data <- values$data[c(input$desc_vars, input$group_var)]
      values$results$descriptive <- describeBy(desc_data[input$desc_vars], 
                                              group = desc_data[[input$group_var]], 
                                              mat = TRUE)
    }
  })
  
  output$descriptive_table <- DT::renderDataTable({
    if(is.null(values$results$descriptive)) return(NULL)
    
    DT::datatable(values$results$descriptive, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$descriptive_interpretation <- renderUI({
    if(is.null(values$results$descriptive)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi Statistik Deskriptif</h4>",
      "<p><strong>Ringkasan:</strong> Tabel menampilkan statistik deskriptif lengkap untuk variabel yang dipilih.</p>",
      "<p><strong>Interpretasi:</strong>",
      "<ul>",
      "<li><strong>Mean:</strong> Rata-rata nilai data</li>",
      "<li><strong>SD:</strong> Standar deviasi (ukuran sebaran data)</li>",
      "<li><strong>Min/Max:</strong> Nilai minimum dan maksimum</li>",
      "<li><strong>Skew:</strong> Ukuran kemiringan distribusi</li>",
      "<li><strong>Kurtosis:</strong> Ukuran ketajaman distribusi</li>",
      "</ul></p>",
      "<p><strong>Saran:</strong> Perhatikan nilai skewness dan kurtosis untuk menilai normalitas data.</p>"
    ))
  })
  
  # Visualization
  observeEvent(input$create_plot, {
    req(values$data, input$plot_type, input$x_var)
    
    p <- NULL
    
    if(input$plot_type == "histogram") {
      p <- ggplot(values$data, aes_string(x = input$x_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", input$x_var))
        
    } else if(input$plot_type == "boxplot") {
      if(input$color_var != "" && !is.null(input$color_var)) {
        p <- ggplot(values$data, aes_string(x = input$color_var, y = input$x_var)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$x_var, "by", input$color_var))
      } else {
        p <- ggplot(values$data, aes_string(y = input$x_var)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$x_var))
      }
      
    } else if(input$plot_type == "scatter" && !is.null(input$y_var)) {
      p <- ggplot(values$data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE) +
        theme_minimal() +
        labs(title = paste("Scatter plot:", input$x_var, "vs", input$y_var))
        
    } else if(input$plot_type == "bar") {
      p <- ggplot(values$data, aes_string(x = input$x_var)) +
        geom_bar(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Bar chart of", input$x_var)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    } else if(input$plot_type == "density") {
      p <- ggplot(values$data, aes_string(x = input$x_var)) +
        geom_density(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Density plot of", input$x_var))
        
    } else if(input$plot_type == "correlation") {
      numeric_data <- values$data[sapply(values$data, is.numeric)]
      if(ncol(numeric_data) > 1) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        p <- ggplot(data = reshape2::melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1,1), space = "Lab", 
                              name="Correlation") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Correlation Matrix")
      }
    }
    
    if(input$color_var != "" && !is.null(input$color_var) && input$plot_type != "boxplot" && input$plot_type != "correlation") {
      p <- p + aes_string(color = input$color_var)
    }
    
    values$results$main_plot <- p
  })
  
  output$main_plot <- renderPlotly({
    if(is.null(values$results$main_plot)) return(NULL)
    
    ggplotly(values$results$main_plot)
  })
  
  output$plot_interpretation <- renderUI({
    if(is.null(values$results$main_plot)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi Visualisasi</h4>",
      "<p><strong>Jenis Plot:</strong>", input$plot_type, "</p>",
      "<p><strong>Variabel:</strong>", input$x_var, 
      if(!is.null(input$y_var) && input$y_var != "") paste("dan", input$y_var) else "", "</p>",
      "<p><strong>Saran:</strong> Gunakan plot ini untuk memahami distribusi, hubungan, atau pola dalam data.</p>"
    ))
  })
  
  # Interactive map
  observeEvent(input$create_map, {
    req(values$data, input$lat_var, input$lon_var)
    
    map_data <- values$data[complete.cases(values$data[c(input$lat_var, input$lon_var)]), ]
    
    values$results$map <- leaflet(map_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~get(input$lon_var),
        lat = ~get(input$lat_var),
        popup = if(!is.null(input$marker_var) && input$marker_var != "") {
          ~paste(input$marker_var, ":", get(input$marker_var))
        } else {
          ~paste("Lat:", get(input$lat_var), "Lon:", get(input$lon_var))
        }
      )
  })
  
  output$interactive_map <- renderLeaflet({
    if(is.null(values$results$map)) {
      leaflet() %>% addTiles()
    } else {
      values$results$map
    }
  })
  
  # Interactive table
  output$interactive_table <- DT::renderDataTable({
    if(is.null(values$data)) return(NULL)
    
    DT::datatable(values$data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  filter = "top")
  })
  
  output$map_table_interpretation <- renderUI({
    if(is.null(values$data)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi Peta & Tabel</h4>",
      "<p><strong>Peta Interaktif:</strong> Menampilkan distribusi geografis data jika tersedia koordinat.</p>",
      "<p><strong>Tabel Interaktif:</strong> Memungkinkan filtering, sorting, dan pencarian data.</p>",
      "<p><strong>Manfaat:</strong> Eksplorasi data secara visual dan interaktif untuk menemukan pola spasial.</p>"
    ))
  })
  
  # Assumption tests
  observeEvent(input$run_assumptions, {
    req(values$data, input$assumption_var)
    
    var_data <- values$data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    results <- list()
    
    # Normality tests
    if(input$shapiro_test && length(var_data) <= 5000) {
      results$shapiro <- shapiro.test(var_data)
    }
    
    if(input$anderson_test) {
      results$anderson <- ad.test(var_data)
    }
    
    if(input$lillie_test) {
      results$lillie <- lillie.test(var_data)
    }
    
    # Homogeneity tests (if grouping variable is provided)
    if(input$assumption_group != "" && !is.null(input$assumption_group)) {
      group_data <- values$data[[input$assumption_group]]
      
      if(input$levene_test) {
        results$levene <- leveneTest(var_data ~ group_data)
      }
      
      if(input$bartlett_test) {
        results$bartlett <- bartlett.test(var_data ~ group_data)
      }
    }
    
    values$results$assumptions <- results
  })
  
  output$normality_results <- renderPrint({
    if(is.null(values$results$assumptions)) return("No tests performed")
    
    results <- values$results$assumptions
    
    cat("=== UJI NORMALITAS ===\n\n")
    
    if(!is.null(results$shapiro)) {
      cat("Shapiro-Wilk Test:\n")
      print(results$shapiro)
      cat("\n")
    }
    
    if(!is.null(results$anderson)) {
      cat("Anderson-Darling Test:\n")
      print(results$anderson)
      cat("\n")
    }
    
    if(!is.null(results$lillie)) {
      cat("Lilliefors Test:\n")
      print(results$lillie)
      cat("\n")
    }
  })
  
  output$homogeneity_results <- renderPrint({
    if(is.null(values$results$assumptions)) return("No tests performed")
    
    results <- values$results$assumptions
    
    cat("=== UJI HOMOGENITAS ===\n\n")
    
    if(!is.null(results$levene)) {
      cat("Levene Test:\n")
      print(results$levene)
      cat("\n")
    }
    
    if(!is.null(results$bartlett)) {
      cat("Bartlett Test:\n")
      print(results$bartlett)
      cat("\n")
    }
  })
  
  output$qq_plot <- renderPlot({
    if(is.null(values$data) || is.null(input$assumption_var)) return(NULL)
    
    var_data <- values$data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    qqnorm(var_data, main = paste("Q-Q Plot:", input$assumption_var))
    qqline(var_data, col = "red")
  })
  
  output$assumption_interpretation <- renderUI({
    if(is.null(values$results$assumptions)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi Uji Asumsi</h4>",
      "<p><strong>Uji Normalitas:</strong> Menguji apakah data berdistribusi normal.</p>",
      "<ul>",
      "<li><strong>H0:</strong> Data berdistribusi normal</li>",
      "<li><strong>H1:</strong> Data tidak berdistribusi normal</li>",
      "<li><strong>Keputusan:</strong> Jika p-value < 0.05, tolak H0 (data tidak normal)</li>",
      "</ul>",
      "<p><strong>Uji Homogenitas:</strong> Menguji kesamaan varians antar kelompok.</p>",
      "<ul>",
      "<li><strong>H0:</strong> Varians antar kelompok sama</li>",
      "<li><strong>H1:</strong> Varians antar kelompok berbeda</li>",
      "<li><strong>Keputusan:</strong> Jika p-value < 0.05, tolak H0 (varians tidak sama)</li>",
      "</ul>"
    ))
  })
  
  # Mean tests
  observeEvent(input$run_mean_test, {
    req(values$data, input$mean_test_var, input$mean_test_type)
    
    var_data <- values$data[[input$mean_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(input$mean_test_type == "one_sample") {
      values$results$mean_test <- t.test(var_data, mu = input$test_value, 
                                        conf.level = 1 - input$alpha_mean)
      
    } else if(input$mean_test_type == "two_sample") {
      req(input$group_var_mean)
      group_data <- values$data[[input$group_var_mean]]
      
      values$results$mean_test <- t.test(var_data ~ group_data, 
                                        var.equal = input$equal_var,
                                        conf.level = 1 - input$alpha_mean)
      
    } else if(input$mean_test_type == "paired") {
      req(input$paired_var2)
      var2_data <- values$data[[input$paired_var2]]
      
      values$results$mean_test <- t.test(var_data, var2_data, 
                                        paired = TRUE,
                                        conf.level = 1 - input$alpha_mean)
    }
  })
  
  output$mean_test_results <- renderPrint({
    if(is.null(values$results$mean_test)) return("No test performed")
    
    print(values$results$mean_test)
  })
  
  output$mean_test_plot <- renderPlot({
    if(is.null(values$data) || is.null(input$mean_test_var)) return(NULL)
    
    var_data <- values$data[[input$mean_test_var]]
    
    if(input$mean_test_type == "two_sample" && !is.null(input$group_var_mean)) {
      group_data <- values$data[[input$group_var_mean]]
      boxplot(var_data ~ group_data, 
              main = paste("Boxplot:", input$mean_test_var, "by", input$group_var_mean),
              xlab = input$group_var_mean, ylab = input$mean_test_var)
    } else {
      hist(var_data, main = paste("Histogram:", input$mean_test_var),
           xlab = input$mean_test_var, col = "lightblue")
      abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2)
    }
  })
  
  output$mean_test_interpretation <- renderUI({
    if(is.null(values$results$mean_test)) return(NULL)
    
    result <- values$results$mean_test
    p_value <- result$p.value
    
    interpretation <- if(p_value < input$alpha_mean) {
      "Tolak H0 - Ada perbedaan signifikan"
    } else {
      "Terima H0 - Tidak ada perbedaan signifikan"
    }
    
    HTML(paste(
      "<h4>💡 Interpretasi Uji Beda Rata-rata</h4>",
      "<p><strong>Hasil:</strong>", interpretation, "</p>",
      "<p><strong>P-value:</strong>", round(p_value, 4), "</p>",
      "<p><strong>Confidence Interval:</strong>", 
      round(result$conf.int[1], 4), "to", round(result$conf.int[2], 4), "</p>",
      "<p><strong>Kesimpulan:</strong> Berdasarkan tingkat signifikansi α =", input$alpha_mean, 
      ", hasil menunjukkan", if(p_value < input$alpha_mean) "adanya" else "tidak adanya", 
      "perbedaan yang signifikan secara statistik.</p>"
    ))
  })
  
  # Proportion tests
  observeEvent(input$run_prop_test, {
    req(values$data, input$prop_var, input$prop_category)
    
    prop_data <- values$data[[input$prop_var]]
    successes <- sum(prop_data == input$prop_category, na.rm = TRUE)
    total <- length(prop_data[!is.na(prop_data)])
    
    if(input$prop_test_type == "one_prop") {
      values$results$prop_test <- prop.test(successes, total, 
                                           p = input$prop_test_value,
                                           conf.level = 1 - input$alpha_prop)
    } else if(input$prop_test_type == "two_prop") {
      req(input$prop_group_var)
      # Implementation for two-sample proportion test
      group_data <- values$data[[input$prop_group_var]]
      groups <- unique(group_data[!is.na(group_data)])
      
      if(length(groups) == 2) {
        success1 <- sum(prop_data[group_data == groups[1]] == input$prop_category, na.rm = TRUE)
        total1 <- sum(!is.na(prop_data[group_data == groups[1]]))
        success2 <- sum(prop_data[group_data == groups[2]] == input$prop_category, na.rm = TRUE)
        total2 <- sum(!is.na(prop_data[group_data == groups[2]]))
        
        values$results$prop_test <- prop.test(c(success1, success2), c(total1, total2),
                                             conf.level = 1 - input$alpha_prop)
      }
    }
  })
  
  output$prop_test_results <- renderPrint({
    if(is.null(values$results$prop_test)) return("No test performed")
    
    print(values$results$prop_test)
  })
  
  # Variance tests
  observeEvent(input$run_var_test, {
    req(values$data, input$var_test_var)
    
    var_data <- values$data[[input$var_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(input$var_test_type == "one_var") {
      # Chi-square test for variance
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var_test_value
      p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
      
      values$results$var_test <- list(
        statistic = chi_stat,
        p.value = p_value,
        parameter = n - 1,
        method = "Chi-square test for variance"
      )
      
    } else if(input$var_test_type == "two_var") {
      req(input$var_group_var)
      group_data <- values$data[[input$var_group_var]]
      
      values$results$var_test <- var.test(var_data ~ group_data,
                                         conf.level = 1 - input$alpha_var)
    }
  })
  
  output$var_test_results <- renderPrint({
    if(is.null(values$results$var_test)) return("No test performed")
    
    print(values$results$var_test)
  })
  
  output$prop_var_plot <- renderPlot({
    if(is.null(values$data)) return(NULL)
    
    if(!is.null(input$prop_var) && input$prop_var != "") {
      # Proportion plot
      prop_data <- values$data[[input$prop_var]]
      barplot(table(prop_data), main = paste("Distribution of", input$prop_var),
              col = "lightblue")
    } else if(!is.null(input$var_test_var) && input$var_test_var != "") {
      # Variance plot
      var_data <- values$data[[input$var_test_var]]
      hist(var_data, main = paste("Distribution of", input$var_test_var),
           xlab = input$var_test_var, col = "lightgreen")
    }
  })
  
  output$prop_var_interpretation <- renderUI({
    HTML(paste(
      "<h4>💡 Interpretasi Uji Proporsi & Variance</h4>",
      "<p><strong>Uji Proporsi:</strong> Menguji apakah proporsi populasi sama dengan nilai tertentu.</p>",
      "<p><strong>Uji Variance:</strong> Menguji apakah variance populasi sama dengan nilai tertentu.</p>",
      "<p><strong>Keputusan:</strong> Jika p-value < α, tolak H0.</p>"
    ))
  })
  
  # ANOVA tests
  observeEvent(input$run_anova, {
    req(values$data, input$anova_dependent, input$anova_factor1)
    
    if(input$anova_type == "one_way") {
      formula_str <- paste(input$anova_dependent, "~", input$anova_factor1)
      values$results$anova <- aov(as.formula(formula_str), data = values$data)
      
      if(input$post_hoc) {
        values$results$posthoc <- TukeyHSD(values$results$anova)
      }
      
    } else if(input$anova_type == "two_way") {
      req(input$anova_factor2)
      interaction_term <- if(input$interaction) "*" else "+"
      formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, interaction_term, input$anova_factor2)
      values$results$anova <- aov(as.formula(formula_str), data = values$data)
      
      if(input$post_hoc) {
        values$results$posthoc <- TukeyHSD(values$results$anova)
      }
    }
  })
  
  output$anova_results <- renderPrint({
    if(is.null(values$results$anova)) return("No ANOVA performed")
    
    summary(values$results$anova)
  })
  
  output$posthoc_results <- renderPrint({
    if(is.null(values$results$posthoc)) return("No post-hoc test performed")
    
    print(values$results$posthoc)
  })
  
  output$anova_plot <- renderPlot({
    if(is.null(values$data) || is.null(input$anova_dependent) || is.null(input$anova_factor1)) return(NULL)
    
    if(input$anova_type == "one_way") {
      boxplot(values$data[[input$anova_dependent]] ~ values$data[[input$anova_factor1]],
              main = paste("ANOVA:", input$anova_dependent, "by", input$anova_factor1),
              xlab = input$anova_factor1, ylab = input$anova_dependent)
    } else if(input$anova_type == "two_way" && !is.null(input$anova_factor2)) {
      interaction.plot(values$data[[input$anova_factor1]], values$data[[input$anova_factor2]], 
                      values$data[[input$anova_dependent]],
                      main = paste("Interaction Plot:", input$anova_dependent))
    }
  })
  
  output$anova_interpretation <- renderUI({
    if(is.null(values$results$anova)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi ANOVA</h4>",
      "<p><strong>ANOVA:</strong> Menguji apakah ada perbedaan rata-rata antar kelompok.</p>",
      "<p><strong>H0:</strong> Semua rata-rata kelompok sama</p>",
      "<p><strong>H1:</strong> Minimal ada satu rata-rata kelompok yang berbeda</p>",
      "<p><strong>Post-hoc:</strong> Jika ANOVA signifikan, uji Tukey menunjukkan kelompok mana yang berbeda.</p>"
    ))
  })
  
  # Regression analysis
  observeEvent(input$run_regression, {
    req(values$data, input$reg_dependent, input$reg_independent)
    
    # Build regression formula
    if(input$include_intercept) {
      formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    } else {
      formula_str <- paste(input$reg_dependent, "~ -1 +", paste(input$reg_independent, collapse = " + "))
    }
    
    values$results$regression <- lm(as.formula(formula_str), data = values$data)
    
    # Regression assumptions
    if(input$check_multicollinearity && length(input$reg_independent) > 1) {
      values$results$vif <- vif(values$results$regression)
    }
    
    # Other assumption tests would be implemented here
  })
  
  output$regression_results <- renderPrint({
    if(is.null(values$results$regression)) return("No regression performed")
    
    summary(values$results$regression)
  })
  
  output$regression_diagnostics <- renderPlot({
    if(is.null(values$results$regression)) return(NULL)
    
    par(mfrow = c(2, 2))
    plot(values$results$regression)
  })
  
  output$regression_assumptions <- renderPrint({
    if(is.null(values$results$regression)) return("No regression performed")
    
    cat("=== UJI ASUMSI REGRESI ===\n\n")
    
    if(!is.null(values$results$vif)) {
      cat("Variance Inflation Factor (VIF):\n")
      print(values$results$vif)
      cat("\n")
    }
    
    # Additional assumption tests would be displayed here
  })
  
  output$regression_interpretation <- renderUI({
    if(is.null(values$results$regression)) return(NULL)
    
    HTML(paste(
      "<h4>💡 Interpretasi Regresi</h4>",
      "<p><strong>R-squared:</strong> Menunjukkan proporsi variabilitas yang dijelaskan model.</p>",
      "<p><strong>Koefisien:</strong> Menunjukkan perubahan Y untuk setiap unit perubahan X.</p>",
      "<p><strong>P-value:</strong> Menguji signifikansi koefisien (H0: koefisien = 0).</p>",
      "<p><strong>VIF:</strong> Nilai > 10 menunjukkan multikolinearitas tinggi.</p>"
    ))
  })
  
  # Download handlers
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Save plots (implementation would depend on stored plots)
      writeLines("Plot download functionality", file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values$data)) {
        write.csv(values$data, file, row.names = FALSE)
      }
    }
  )
  
  # Additional download handlers for other buttons
  output$download_current_plot <- downloadHandler(
    filename = function() { paste0("current_plot_", Sys.Date(), ".png") },
    content = function(file) { writeLines("Current plot download", file) }
  )
  
  output$download_report_pdf <- downloadHandler(
    filename = function() { paste0("report_", Sys.Date(), ".pdf") },
    content = function(file) { writeLines("PDF report download", file) }
  )
  
  output$download_results_word <- downloadHandler(
    filename = function() { paste0("results_", Sys.Date(), ".docx") },
    content = function(file) { writeLines("Word results download", file) }
  )
  
  output$download_tables <- downloadHandler(
    filename = function() { paste0("tables_", Sys.Date(), ".xlsx") },
    content = function(file) { writeLines("Tables download", file) }
  )
  
  output$download_descriptive_package <- downloadHandler(
    filename = function() { paste0("descriptive_package_", Sys.Date(), ".zip") },
    content = function(file) { writeLines("Descriptive package download", file) }
  )
  
  output$download_assumptions_package <- downloadHandler(
    filename = function() { paste0("assumptions_package_", Sys.Date(), ".zip") },
    content = function(file) { writeLines("Assumptions package download", file) }
  )
  
  output$download_inference_package <- downloadHandler(
    filename = function() { paste0("inference_package_", Sys.Date(), ".zip") },
    content = function(file) { writeLines("Inference package download", file) }
  )
  
  output$download_regression_package <- downloadHandler(
    filename = function() { paste0("regression_package_", Sys.Date(), ".zip") },
    content = function(file) { writeLines("Regression package download", file) }
  )
  
  output$download_complete_package <- downloadHandler(
    filename = function() { paste0("complete_analysis_", Sys.Date(), ".zip") },
    content = function(file) { writeLines("Complete analysis package download", file) }
  )
  
}