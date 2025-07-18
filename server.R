# Server for StatInsight Pro Dashboard

server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    current_plot = NULL,
    analysis_results = list(),
    interpretations = list()
  )
  
  # Helper function to get numeric and categorical variables
  get_variable_types <- function(data) {
    if (is.null(data)) return(list(numeric = NULL, categorical = NULL))
    
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    
    list(numeric = numeric_vars, categorical = categorical_vars)
  }
  
  # File upload and data loading
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$datapath)
      
      if (ext == "csv") {
        values$raw_data <- read.csv(input$file$datapath, 
                                   header = input$header, 
                                   sep = input$sep, 
                                   quote = input$quote)
      } else if (ext %in% c("xlsx", "xls")) {
        values$raw_data <- read_excel(input$file$datapath)
      }
      
      values$processed_data <- values$raw_data
      
      # Update variable choices
      var_types <- get_variable_types(values$processed_data)
      all_vars <- names(values$processed_data)
      
      updateSelectInput(session, "transform_var", choices = all_vars)
      updateSelectInput(session, "desc_vars", choices = all_vars)
      updateSelectInput(session, "group_var", choices = c("None" = "", var_types$categorical))
      updateSelectInput(session, "x_var", choices = all_vars)
      updateSelectInput(session, "y_var", choices = var_types$numeric)
      updateSelectInput(session, "color_var", choices = c("None" = "", var_types$categorical))
      updateSelectInput(session, "assumption_var", choices = var_types$numeric)
      updateSelectInput(session, "assumption_group", choices = c("None" = "", var_types$categorical))
      updateSelectInput(session, "mean_test_var", choices = var_types$numeric)
      updateSelectInput(session, "group_var_mean", choices = var_types$categorical)
      updateSelectInput(session, "paired_var2", choices = var_types$numeric)
      updateSelectInput(session, "prop_var", choices = var_types$categorical)
      updateSelectInput(session, "prop_group_var", choices = var_types$categorical)
      
      # Update prop_category choices when prop_var changes
      if (length(var_types$categorical) > 0) {
        first_cat_var <- var_types$categorical[1]
        if (first_cat_var %in% names(values$processed_data)) {
          unique_cats <- unique(values$processed_data[[first_cat_var]])
          updateSelectInput(session, "prop_category", choices = unique_cats)
        }
      }
      updateSelectInput(session, "var_test_var", choices = var_types$numeric)
      updateSelectInput(session, "var_group_var", choices = var_types$categorical)
      updateSelectInput(session, "anova_dependent", choices = var_types$numeric)
      updateSelectInput(session, "anova_factor1", choices = var_types$categorical)
      updateSelectInput(session, "anova_factor2", choices = var_types$categorical)
      updateSelectInput(session, "reg_dependent", choices = var_types$numeric)
      updateSelectInput(session, "reg_independent", choices = var_types$numeric)
      
      # Update geo variables if available
      geo_vars <- names(values$processed_data)[grepl("lat|lng|lon", names(values$processed_data), ignore.case = TRUE)]
      if (length(geo_vars) > 0) {
        updateSelectInput(session, "lat_var", choices = geo_vars)
        updateSelectInput(session, "lon_var", choices = geo_vars)
        updateSelectInput(session, "marker_var", choices = all_vars)
      }
      
      showNotification("Data berhasil dimuat!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Output conditionals
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  
  output$dataLoaded <- reactive({
    return(!is.null(values$processed_data))
  })
  
  output$hasGeoData <- reactive({
    if (is.null(values$processed_data)) return(FALSE)
    geo_vars <- names(values$processed_data)[grepl("lat|lng|lon", names(values$processed_data), ignore.case = TRUE)]
    return(length(geo_vars) >= 2)
  })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  outputOptions(output, "hasGeoData", suspendWhenHidden = FALSE)
  
  # Update prop_category choices when prop_var changes
  observeEvent(input$prop_var, {
    req(values$processed_data, input$prop_var)
    if (input$prop_var %in% names(values$processed_data)) {
      unique_cats <- unique(values$processed_data[[input$prop_var]])
      updateSelectInput(session, "prop_category", choices = unique_cats)
    }
  })
  
  # Home page outputs
  output$total_observations <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else nrow(values$processed_data),
      subtitle = "Total Observasi",
      icon = icon("table"),
      color = "blue"
    )
  })
  
  output$total_variables <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else ncol(values$processed_data),
      subtitle = "Total Variabel",
      icon = icon("columns"),
      color = "green"
    )
  })
  
  output$numeric_vars <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else length(get_variable_types(values$processed_data)$numeric),
      subtitle = "Variabel Numerik",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$categorical_vars <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else length(get_variable_types(values$processed_data)$categorical),
      subtitle = "Variabel Kategorik",
      icon = icon("tags"),
      color = "red"
    )
  })
  
  output$metadata_table <- DT::renderDataTable({
    req(values$processed_data)
    
    metadata <- data.frame(
      Variable = names(values$processed_data),
      Type = sapply(values$processed_data, class),
      Missing = sapply(values$processed_data, function(x) sum(is.na(x))),
      `Missing %` = round(sapply(values$processed_data, function(x) sum(is.na(x))/length(x) * 100), 2),
      Unique = sapply(values$processed_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(metadata, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$summary_stats <- renderPrint({
    req(values$processed_data)
    summary(values$processed_data)
  })
  
  # Data Management
  output$data_preview <- DT::renderDataTable({
    req(values$processed_data)
    DT::datatable(values$processed_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Data transformation
  observeEvent(input$apply_transform, {
    req(values$processed_data, input$transform_var, input$transform_type)
    
    tryCatch({
      var_data <- values$processed_data[[input$transform_var]]
      
      if (input$transform_type == "categorize") {
        if (input$cat_method == "interval") {
          breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = input$n_categories + 1)
        } else if (input$cat_method == "quantile") {
          breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
        }
        
        new_var <- cut(var_data, breaks = breaks, include.lowest = TRUE, 
                      labels = paste0("Cat_", 1:input$n_categories))
        values$processed_data[[paste0(input$transform_var, "_cat")]] <- new_var
        
        values$interpretations$transform <- paste0(
          "Variabel '", input$transform_var, "' telah dikategorikan menjadi ", input$n_categories, 
          " kategori menggunakan metode ", input$cat_method, ". Variabel baru bernama '", 
          input$transform_var, "_cat' telah ditambahkan ke dataset."
        )
        
      } else if (input$transform_type == "log") {
        values$processed_data[[paste0(input$transform_var, "_log")]] <- log(var_data + 1)
        values$interpretations$transform <- paste0(
          "Transformasi logaritma natural telah diterapkan pada variabel '", input$transform_var, 
          "'. Variabel baru bernama '", input$transform_var, "_log' telah ditambahkan."
        )
        
      } else if (input$transform_type == "sqrt") {
        values$processed_data[[paste0(input$transform_var, "_sqrt")]] <- sqrt(var_data)
        values$interpretations$transform <- paste0(
          "Transformasi akar kuadrat telah diterapkan pada variabel '", input$transform_var, 
          "'. Variabel baru bernama '", input$transform_var, "_sqrt' telah ditambahkan."
        )
        
      } else if (input$transform_type == "scale") {
        values$processed_data[[paste0(input$transform_var, "_scaled")]] <- scale(var_data)[,1]
        values$interpretations$transform <- paste0(
          "Standardisasi (z-score) telah diterapkan pada variabel '", input$transform_var, 
          "'. Variabel baru bernama '", input$transform_var, "_scaled' telah ditambahkan."
        )
        
      } else if (input$transform_type == "normalize") {
        values$processed_data[[paste0(input$transform_var, "_norm")]] <- (var_data - min(var_data, na.rm = TRUE)) / 
          (max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE))
        values$interpretations$transform <- paste0(
          "Normalisasi min-max telah diterapkan pada variabel '", input$transform_var, 
          "'. Variabel baru bernama '", input$transform_var, "_norm' telah ditambahkan."
        )
      }
      
      showNotification("Transformasi berhasil diterapkan!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error dalam transformasi:", e$message), type = "error")
    })
  })
  
  output$transform_interpretation <- renderUI({
    if (!is.null(values$interpretations$transform)) {
      HTML(paste0("<h4>📊 Interpretasi Transformasi Data</h4>",
                  "<p>", values$interpretations$transform, "</p>"))
    }
  })
  
  # Descriptive Statistics
  observeEvent(input$run_descriptive, {
    req(values$processed_data, input$desc_vars)
    
    tryCatch({
      selected_data <- values$processed_data[, input$desc_vars, drop = FALSE]
      
      if (input$group_var != "") {
        group_data <- values$processed_data[[input$group_var]]
        desc_stats <- selected_data %>%
          mutate(Group = group_data) %>%
          group_by(Group) %>%
          summarise_all(list(
            Mean = ~ mean(., na.rm = TRUE),
            Median = ~ median(., na.rm = TRUE),
            SD = ~ sd(., na.rm = TRUE),
            Min = ~ min(., na.rm = TRUE),
            Max = ~ max(., na.rm = TRUE),
            Q1 = ~ quantile(., 0.25, na.rm = TRUE),
            Q3 = ~ quantile(., 0.75, na.rm = TRUE)
          ), .names = "{.col}_{.fn}")
      } else {
        desc_stats <- selected_data %>%
          summarise_all(list(
            Mean = ~ mean(., na.rm = TRUE),
            Median = ~ median(., na.rm = TRUE),
            SD = ~ sd(., na.rm = TRUE),
            Min = ~ min(., na.rm = TRUE),
            Max = ~ max(., na.rm = TRUE),
            Q1 = ~ quantile(., 0.25, na.rm = TRUE),
            Q3 = ~ quantile(., 0.75, na.rm = TRUE)
          ), .names = "{.col}_{.fn}")
      }
      
      values$analysis_results$descriptive <- desc_stats
      
      # Generate interpretation
      n_vars <- length(input$desc_vars)
      n_obs <- nrow(selected_data)
      
      values$interpretations$descriptive <- paste0(
        "Analisis statistik deskriptif telah dilakukan pada ", n_vars, " variabel dengan ", n_obs, " observasi. ",
        if (input$group_var != "") paste0("Data dikelompokkan berdasarkan variabel '", input$group_var, "'. ") else "",
        "Statistik yang dihitung meliputi rata-rata, median, standar deviasi, nilai minimum dan maksimum, ",
        "serta kuartil pertama dan ketiga. Hasil ini memberikan gambaran distribusi data pada setiap variabel."
      )
      
    }, error = function(e) {
      showNotification(paste("Error dalam analisis deskriptif:", e$message), type = "error")
    })
  })
  
  output$descriptive_table <- DT::renderDataTable({
    req(values$analysis_results$descriptive)
    DT::datatable(values$analysis_results$descriptive, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$descriptive_interpretation <- renderUI({
    if (!is.null(values$interpretations$descriptive)) {
      HTML(paste0("<h4>📊 Interpretasi Statistik Deskriptif</h4>",
                  "<p>", values$interpretations$descriptive, "</p>"))
    }
  })
  
  # Visualization
  observeEvent(input$create_plot, {
    req(values$processed_data, input$plot_type, input$x_var)
    
    tryCatch({
      data_to_plot <- values$processed_data
      
      if (input$plot_type == "histogram") {
        p <- ggplot(data_to_plot, aes_string(x = input$x_var)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$x_var), x = input$x_var, y = "Frequency")
        
        values$interpretations$plot <- paste0(
          "Histogram menunjukkan distribusi frekuensi variabel '", input$x_var, "'. ",
          "Bentuk distribusi dapat mengindikasikan apakah data berdistribusi normal, skewed, atau memiliki pola tertentu."
        )
        
      } else if (input$plot_type == "boxplot") {
        if (input$color_var != "") {
          p <- ggplot(data_to_plot, aes_string(x = input$color_var, y = input$x_var, fill = input$color_var)) +
            geom_boxplot() +
            theme_minimal() +
            labs(title = paste("Boxplot of", input$x_var, "by", input$color_var))
        } else {
          p <- ggplot(data_to_plot, aes_string(y = input$x_var)) +
            geom_boxplot(fill = "steelblue") +
            theme_minimal() +
            labs(title = paste("Boxplot of", input$x_var))
        }
        
        values$interpretations$plot <- paste0(
          "Boxplot menunjukkan distribusi kuartil dari variabel '", input$x_var, "'. ",
          "Plot ini membantu mengidentifikasi outlier, median, dan rentang interkuartil data."
        )
        
      } else if (input$plot_type == "scatter") {
        req(input$y_var)
        aes_mapping <- aes_string(x = input$x_var, y = input$y_var)
        if (input$color_var != "") {
          aes_mapping <- aes_string(x = input$x_var, y = input$y_var, color = input$color_var)
        }
        
        p <- ggplot(data_to_plot, aes_mapping) +
          geom_point(alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE) +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var))
        
        values$interpretations$plot <- paste0(
          "Scatter plot menunjukkan hubungan antara variabel '", input$x_var, "' dan '", input$y_var, "'. ",
          "Garis regresi linear ditambahkan untuk menunjukkan tren hubungan kedua variabel."
        )
        
      } else if (input$plot_type == "bar") {
        p <- ggplot(data_to_plot, aes_string(x = input$x_var)) +
          geom_bar(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Bar Chart of", input$x_var), x = input$x_var, y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        values$interpretations$plot <- paste0(
          "Bar chart menunjukkan frekuensi setiap kategori dalam variabel '", input$x_var, "'. ",
          "Tinggi setiap bar menunjukkan jumlah observasi dalam kategori tersebut."
        )
        
      } else if (input$plot_type == "correlation") {
        numeric_vars <- get_variable_types(values$processed_data)$numeric
        if (length(numeric_vars) > 1) {
          cor_matrix <- cor(values$processed_data[numeric_vars], use = "complete.obs")
          p <- ggplot(data = expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix)), 
                     aes(x = Var1, y = Var2, fill = as.vector(cor_matrix))) +
            geom_tile() +
            scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
            theme_minimal() +
            labs(title = "Correlation Matrix", fill = "Correlation") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
        values$interpretations$plot <- paste0(
          "Matriks korelasi menunjukkan kekuatan hubungan linear antara variabel numerik. ",
          "Nilai mendekati 1 menunjukkan korelasi positif kuat, -1 korelasi negatif kuat, dan 0 tidak ada korelasi."
        )
        
      } else if (input$plot_type == "density") {
        p <- ggplot(data_to_plot, aes_string(x = input$x_var)) +
          geom_density(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$x_var), x = input$x_var, y = "Density")
        
        values$interpretations$plot <- paste0(
          "Density plot menunjukkan estimasi distribusi probabilitas variabel '", input$x_var, "'. ",
          "Kurva yang halus memberikan gambaran bentuk distribusi data."
        )
      }
      
      values$current_plot <- p
      
    }, error = function(e) {
      showNotification(paste("Error dalam membuat plot:", e$message), type = "error")
    })
  })
  
  output$main_plot <- renderPlotly({
    req(values$current_plot)
    ggplotly(values$current_plot)
  })
  
  output$plot_interpretation <- renderUI({
    if (!is.null(values$interpretations$plot)) {
      HTML(paste0("<h4>📊 Interpretasi Visualisasi</h4>",
                  "<p>", values$interpretations$plot, "</p>"))
    }
  })
  
  # Interactive Map
  observeEvent(input$create_map, {
    req(values$processed_data, input$lat_var, input$lon_var)
    
    tryCatch({
      map_data <- values$processed_data[!is.na(values$processed_data[[input$lat_var]]) & 
                                       !is.na(values$processed_data[[input$lon_var]]), ]
      
      values$analysis_results$map <- leaflet(map_data) %>%
        addTiles() %>%
        addMarkers(lng = ~get(input$lon_var), lat = ~get(input$lat_var),
                  popup = if (input$marker_var != "") ~paste(input$marker_var, ":", get(input$marker_var)) else NULL)
      
      values$interpretations$map <- paste0(
        "Peta interaktif menampilkan ", nrow(map_data), " titik data berdasarkan koordinat latitude ('", 
        input$lat_var, "') dan longitude ('", input$lon_var, "'). ",
        if (input$marker_var != "") paste0("Informasi tambahan dari variabel '", input$marker_var, "' ditampilkan dalam popup.") else ""
      )
      
    }, error = function(e) {
      showNotification(paste("Error dalam membuat peta:", e$message), type = "error")
    })
  })
  
  output$interactive_map <- renderLeaflet({
    if (!is.null(values$analysis_results$map)) {
      values$analysis_results$map
    }
  })
  
  output$interactive_table <- DT::renderDataTable({
    req(values$processed_data)
    DT::datatable(values$processed_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$map_table_interpretation <- renderUI({
    if (!is.null(values$interpretations$map)) {
      HTML(paste0("<h4>🗺️ Interpretasi Peta & Tabel</h4>",
                  "<p>", values$interpretations$map, "</p>",
                  "<p>Tabel interaktif menampilkan seluruh dataset dengan fitur pencarian, pengurutan, dan paginasi untuk memudahkan eksplorasi data.</p>"))
    }
  })
  
  # Assumption Tests
  observeEvent(input$run_assumptions, {
    req(values$processed_data, input$assumption_var)
    
    tryCatch({
      var_data <- values$processed_data[[input$assumption_var]]
      var_data <- var_data[!is.na(var_data)]
      
      normality_results <- list()
      homogeneity_results <- list()
      
      # Normality tests
      if (input$shapiro_test && length(var_data) <= 5000) {
        normality_results$shapiro <- shapiro.test(var_data)
      }
      
      if (input$anderson_test) {
        if (requireNamespace("nortest", quietly = TRUE)) {
          normality_results$anderson <- nortest::ad.test(var_data)
        } else {
          normality_results$anderson <- "nortest package not available"
        }
      }
      
      if (input$lillie_test) {
        if (requireNamespace("nortest", quietly = TRUE)) {
          normality_results$lillie <- nortest::lillie.test(var_data)
        } else {
          normality_results$lillie <- "nortest package not available"
        }
      }
      
      # Homogeneity tests
      if (input$assumption_group != "" && (input$levene_test || input$bartlett_test)) {
        group_data <- values$processed_data[[input$assumption_group]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_data_complete <- var_data[complete_cases]
        group_data_complete <- group_data[complete_cases]
        
        if (input$levene_test) {
          if (requireNamespace("car", quietly = TRUE)) {
            homogeneity_results$levene <- car::leveneTest(var_data_complete ~ as.factor(group_data_complete))
          } else {
            homogeneity_results$levene <- "car package not available"
          }
        }
        
        if (input$bartlett_test) {
          homogeneity_results$bartlett <- bartlett.test(var_data_complete ~ as.factor(group_data_complete))
        }
      }
      
      values$analysis_results$normality <- normality_results
      values$analysis_results$homogeneity <- homogeneity_results
      
      # Generate interpretation
      norm_interpretation <- ""
      if (length(normality_results) > 0) {
        norm_pvals <- sapply(normality_results, function(x) x$p.value)
        norm_interpretation <- paste0(
          "Uji normalitas dilakukan dengan ", length(normality_results), " metode. ",
          "Jika p-value > 0.05, data berdistribusi normal. ",
          "Hasil menunjukkan ", 
          if (any(norm_pvals > 0.05)) "beberapa uji mendukung asumsi normalitas" else "data tidak berdistribusi normal",
          "."
        )
      }
      
      homo_interpretation <- ""
      if (length(homogeneity_results) > 0) {
        homo_pvals <- sapply(homogeneity_results, function(x) x$p.value)
        homo_interpretation <- paste0(
          "Uji homogenitas varians dilakukan dengan ", length(homogeneity_results), " metode. ",
          "Jika p-value > 0.05, varians antar kelompok homogen. ",
          "Hasil menunjukkan ", 
          if (any(homo_pvals > 0.05)) "varians relatif homogen" else "varians tidak homogen",
          "."
        )
      }
      
      values$interpretations$assumptions <- paste(norm_interpretation, homo_interpretation)
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji asumsi:", e$message), type = "error")
    })
  })
  
  output$normality_results <- renderPrint({
    req(values$analysis_results$normality)
    values$analysis_results$normality
  })
  
  output$homogeneity_results <- renderPrint({
    req(values$analysis_results$homogeneity)
    values$analysis_results$homogeneity
  })
  
  output$qq_plot <- renderPlot({
    req(values$processed_data, input$assumption_var)
    var_data <- values$processed_data[[input$assumption_var]]
    qqnorm(var_data, main = paste("Q-Q Plot for", input$assumption_var))
    qqline(var_data, col = "red")
  })
  
  output$assumption_interpretation <- renderUI({
    if (!is.null(values$interpretations$assumptions)) {
      HTML(paste0("<h4>✅ Interpretasi Uji Asumsi</h4>",
                  "<p>", values$interpretations$assumptions, "</p>",
                  "<p><strong>Q-Q Plot:</strong> Jika titik-titik mengikuti garis merah dengan baik, data berdistribusi normal.</p>"))
    }
  })
  
  # Mean difference tests
  observeEvent(input$run_mean_test, {
    req(values$processed_data, input$mean_test_var, input$mean_test_type)
    
    tryCatch({
      var_data <- values$processed_data[[input$mean_test_var]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$mean_test_type == "one_sample") {
        test_result <- t.test(var_data, mu = input$test_value, conf.level = 1 - input$alpha_mean)
        
        values$interpretations$mean_test <- paste0(
          "One-sample t-test membandingkan rata-rata sampel dengan nilai hipotesis ", input$test_value, ". ",
          "Dengan α = ", input$alpha_mean, ", ",
          if (test_result$p.value < input$alpha_mean) {
            "H0 ditolak. Ada perbedaan signifikan antara rata-rata sampel dan nilai hipotesis."
          } else {
            "H0 tidak ditolak. Tidak ada perbedaan signifikan antara rata-rata sampel dan nilai hipotesis."
          }
        )
        
      } else if (input$mean_test_type == "two_sample") {
        req(input$group_var_mean)
        group_data <- values$processed_data[[input$group_var_mean]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_data_complete <- var_data[complete_cases]
        group_data_complete <- group_data[complete_cases]
        
        test_result <- t.test(var_data_complete ~ group_data_complete, 
                             var.equal = input$equal_var, 
                             conf.level = 1 - input$alpha_mean)
        
        values$interpretations$mean_test <- paste0(
          "Two-sample t-test membandingkan rata-rata dua kelompok. ",
          "Dengan α = ", input$alpha_mean, ", ",
          if (test_result$p.value < input$alpha_mean) {
            "H0 ditolak. Ada perbedaan signifikan antara rata-rata kedua kelompok."
          } else {
            "H0 tidak ditolak. Tidak ada perbedaan signifikan antara rata-rata kedua kelompok."
          }
        )
        
      } else if (input$mean_test_type == "paired") {
        req(input$paired_var2)
        var2_data <- values$processed_data[[input$paired_var2]]
        complete_cases <- !is.na(var_data) & !is.na(var2_data)
        var_data_complete <- var_data[complete_cases]
        var2_data_complete <- var2_data[complete_cases]
        
        test_result <- t.test(var_data_complete, var2_data_complete, 
                             paired = TRUE, conf.level = 1 - input$alpha_mean)
        
        values$interpretations$mean_test <- paste0(
          "Paired t-test membandingkan rata-rata dua pengukuran berpasangan. ",
          "Dengan α = ", input$alpha_mean, ", ",
          if (test_result$p.value < input$alpha_mean) {
            "H0 ditolak. Ada perbedaan signifikan antara kedua pengukuran."
          } else {
            "H0 tidak ditolak. Tidak ada perbedaan signifikan antara kedua pengukuran."
          }
        )
      }
      
      values$analysis_results$mean_test <- test_result
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji beda rata-rata:", e$message), type = "error")
    })
  })
  
  output$mean_test_results <- renderPrint({
    req(values$analysis_results$mean_test)
    values$analysis_results$mean_test
  })
  
  output$mean_test_plot <- renderPlot({
    req(values$analysis_results$mean_test, values$processed_data, input$mean_test_var)
    
    if (input$mean_test_type == "two_sample" && !is.null(input$group_var_mean)) {
      ggplot(values$processed_data, aes_string(x = input$group_var_mean, y = input$mean_test_var)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Comparison of Groups", x = input$group_var_mean, y = input$mean_test_var)
    } else {
      hist(values$processed_data[[input$mean_test_var]], 
           main = paste("Distribution of", input$mean_test_var),
           xlab = input$mean_test_var, col = "steelblue", alpha = 0.7)
      if (input$mean_test_type == "one_sample") {
        abline(v = input$test_value, col = "red", lwd = 2, lty = 2)
      }
    }
  })
  
  output$mean_test_interpretation <- renderUI({
    if (!is.null(values$interpretations$mean_test)) {
      HTML(paste0("<h4>📊 Interpretasi Uji Beda Rata-rata</h4>",
                  "<p>", values$interpretations$mean_test, "</p>"))
    }
  })
  
  # Proportion and Variance tests
  observeEvent(input$run_prop_test, {
    req(values$processed_data, input$prop_var, input$prop_category)
    
    tryCatch({
      prop_data <- values$processed_data[[input$prop_var]]
      prop_data <- prop_data[!is.na(prop_data)]
      
      if (input$prop_test_type == "one_prop") {
        successes <- sum(prop_data == input$prop_category)
        total <- length(prop_data)
        
        test_result <- prop.test(successes, total, p = input$prop_test_value, 
                                conf.level = 1 - input$alpha_prop)
        
        values$interpretations$prop_test <- paste0(
          "One-sample proportion test membandingkan proporsi sampel dengan nilai hipotesis ", 
          input$prop_test_value, ". ",
          "Dengan α = ", input$alpha_prop, ", ",
          if (test_result$p.value < input$alpha_prop) {
            "H0 ditolak. Ada perbedaan signifikan antara proporsi sampel dan nilai hipotesis."
          } else {
            "H0 tidak ditolak. Tidak ada perbedaan signifikan antara proporsi sampel dan nilai hipotesis."
          }
        )
        
      } else if (input$prop_test_type == "two_prop") {
        req(input$prop_group_var)
        group_data <- values$processed_data[[input$prop_group_var]]
        complete_cases <- !is.na(prop_data) & !is.na(group_data)
        prop_data_complete <- prop_data[complete_cases]
        group_data_complete <- group_data[complete_cases]
        
        groups <- unique(group_data_complete)
        if (length(groups) == 2) {
          successes <- c(sum(prop_data_complete[group_data_complete == groups[1]] == input$prop_category),
                        sum(prop_data_complete[group_data_complete == groups[2]] == input$prop_category))
          totals <- c(sum(group_data_complete == groups[1]),
                     sum(group_data_complete == groups[2]))
          
          test_result <- prop.test(successes, totals, conf.level = 1 - input$alpha_prop)
          
          values$interpretations$prop_test <- paste0(
            "Two-sample proportion test membandingkan proporsi dua kelompok. ",
            "Dengan α = ", input$alpha_prop, ", ",
            if (test_result$p.value < input$alpha_prop) {
              "H0 ditolak. Ada perbedaan signifikan antara proporsi kedua kelompok."
            } else {
              "H0 tidak ditolak. Tidak ada perbedaan signifikan antara proporsi kedua kelompok."
            }
          )
        }
      }
      
      values$analysis_results$prop_test <- test_result
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji proporsi:", e$message), type = "error")
    })
  })
  
  observeEvent(input$run_var_test, {
    req(values$processed_data, input$var_test_var, input$var_test_type)
    
    tryCatch({
      var_data <- values$processed_data[[input$var_test_var]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$var_test_type == "one_var") {
        # Chi-square test for variance
        n <- length(var_data)
        sample_var <- var(var_data)
        chi_stat <- (n - 1) * sample_var / input$var_test_value
        p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
        
        test_result <- list(
          statistic = chi_stat,
          p.value = p_value,
          parameter = n - 1,
          method = "One-sample test for variance"
        )
        
        values$interpretations$var_test <- paste0(
          "One-sample variance test membandingkan varians sampel dengan nilai hipotesis ", 
          input$var_test_value, ". ",
          "Dengan α = ", input$alpha_var, ", ",
          if (p_value < input$alpha_var) {
            "H0 ditolak. Ada perbedaan signifikan antara varians sampel dan nilai hipotesis."
          } else {
            "H0 tidak ditolak. Tidak ada perbedaan signifikan antara varians sampel dan nilai hipotesis."
          }
        )
        
      } else if (input$var_test_type == "two_var") {
        req(input$var_group_var)
        group_data <- values$processed_data[[input$var_group_var]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_data_complete <- var_data[complete_cases]
        group_data_complete <- group_data[complete_cases]
        
        groups <- unique(group_data_complete)
        if (length(groups) == 2) {
          group1_data <- var_data_complete[group_data_complete == groups[1]]
          group2_data <- var_data_complete[group_data_complete == groups[2]]
          
          test_result <- var.test(group1_data, group2_data, conf.level = 1 - input$alpha_var)
          
          values$interpretations$var_test <- paste0(
            "F-test membandingkan varians dua kelompok. ",
            "Dengan α = ", input$alpha_var, ", ",
            if (test_result$p.value < input$alpha_var) {
              "H0 ditolak. Ada perbedaan signifikan antara varians kedua kelompok."
            } else {
              "H0 tidak ditolak. Tidak ada perbedaan signifikan antara varians kedua kelompok."
            }
          )
        }
      }
      
      values$analysis_results$var_test <- test_result
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji varians:", e$message), type = "error")
    })
  })
  
  output$prop_test_results <- renderPrint({
    req(values$analysis_results$prop_test)
    values$analysis_results$prop_test
  })
  
  output$var_test_results <- renderPrint({
    req(values$analysis_results$var_test)
    values$analysis_results$var_test
  })
  
  output$prop_var_plot <- renderPlot({
    if (!is.null(values$analysis_results$prop_test) && input$prop_test_type == "two_prop") {
      # Create proportion comparison plot
      req(input$prop_group_var)
      prop_data <- values$processed_data[[input$prop_var]]
      group_data <- values$processed_data[[input$prop_group_var]]
      
      prop_summary <- table(group_data, prop_data)
      prop_df <- as.data.frame(prop_summary)
      
      ggplot(prop_df, aes(x = group_data, y = Freq, fill = prop_data)) +
        geom_bar(stat = "identity", position = "fill") +
        theme_minimal() +
        labs(title = "Proportion Comparison", x = input$prop_group_var, y = "Proportion")
        
    } else if (!is.null(values$analysis_results$var_test) && input$var_test_type == "two_var") {
      # Create variance comparison plot
      req(input$var_group_var)
      var_data <- values$processed_data[[input$var_test_var]]
      group_data <- values$processed_data[[input$var_group_var]]
      
      ggplot(values$processed_data, aes_string(x = input$var_group_var, y = input$var_test_var)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Variance Comparison", x = input$var_group_var, y = input$var_test_var)
    }
  })
  
  output$prop_var_interpretation <- renderUI({
    interpretation <- ""
    if (!is.null(values$interpretations$prop_test)) {
      interpretation <- paste0("<h4>📊 Interpretasi Uji Proporsi</h4>",
                              "<p>", values$interpretations$prop_test, "</p>")
    }
    if (!is.null(values$interpretations$var_test)) {
      interpretation <- paste0(interpretation, "<h4>📊 Interpretasi Uji Varians</h4>",
                              "<p>", values$interpretations$var_test, "</p>")
    }
    if (interpretation != "") {
      HTML(interpretation)
    }
  })
  
  # ANOVA
  observeEvent(input$run_anova, {
    req(values$processed_data, input$anova_dependent, input$anova_factor1)
    
    tryCatch({
      if (input$anova_type == "one_way") {
        formula_str <- paste(input$anova_dependent, "~", input$anova_factor1)
        anova_result <- aov(as.formula(formula_str), data = values$processed_data)
        
        values$interpretations$anova <- paste0(
          "One-way ANOVA menguji perbedaan rata-rata ", input$anova_dependent, 
          " antar kelompok ", input$anova_factor1, ". ",
          "Dengan α = ", input$alpha_anova, ", ",
          if (summary(anova_result)[[1]][["Pr(>F)"]][1] < input$alpha_anova) {
            "H0 ditolak. Ada perbedaan signifikan antar kelompok."
          } else {
            "H0 tidak ditolak. Tidak ada perbedaan signifikan antar kelompok."
          }
        )
        
      } else if (input$anova_type == "two_way") {
        req(input$anova_factor2)
        if (input$interaction) {
          formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, "*", input$anova_factor2)
        } else {
          formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, "+", input$anova_factor2)
        }
        
        anova_result <- aov(as.formula(formula_str), data = values$processed_data)
        
        values$interpretations$anova <- paste0(
          "Two-way ANOVA menguji pengaruh ", input$anova_factor1, " dan ", input$anova_factor2, 
          " terhadap ", input$anova_dependent, ". ",
          if (input$interaction) "Interaksi antar faktor juga diuji. " else "",
          "Dengan α = ", input$alpha_anova, ", interpretasi berdasarkan p-value masing-masing faktor."
        )
      }
      
      values$analysis_results$anova <- anova_result
      
      # Post-hoc test
      if (input$post_hoc) {
        posthoc_result <- TukeyHSD(anova_result)
        values$analysis_results$posthoc <- posthoc_result
      }
      
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA:", e$message), type = "error")
    })
  })
  
  output$anova_results <- renderPrint({
    req(values$analysis_results$anova)
    summary(values$analysis_results$anova)
  })
  
  output$posthoc_results <- renderPrint({
    req(values$analysis_results$posthoc)
    values$analysis_results$posthoc
  })
  
  output$anova_plot <- renderPlot({
    req(values$analysis_results$anova, input$anova_dependent, input$anova_factor1)
    
    if (input$anova_type == "one_way") {
      ggplot(values$processed_data, aes_string(x = input$anova_factor1, y = input$anova_dependent)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = "One-way ANOVA", x = input$anova_factor1, y = input$anova_dependent)
    } else if (input$anova_type == "two_way") {
      ggplot(values$processed_data, aes_string(x = input$anova_factor1, y = input$anova_dependent, 
                                              fill = input$anova_factor2)) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        labs(title = "Two-way ANOVA", x = input$anova_factor1, y = input$anova_dependent, 
             fill = input$anova_factor2)
    }
  })
  
  output$anova_interpretation <- renderUI({
    if (!is.null(values$interpretations$anova)) {
      HTML(paste0("<h4>📊 Interpretasi ANOVA</h4>",
                  "<p>", values$interpretations$anova, "</p>",
                  if (!is.null(values$analysis_results$posthoc)) {
                    "<p><strong>Post-hoc Test:</strong> Tukey HSD test menunjukkan perbedaan spesifik antar pasangan kelompok.</p>"
                  } else ""
      ))
    }
  })
  
  # Multiple Linear Regression
  observeEvent(input$run_regression, {
    req(values$processed_data, input$reg_dependent, input$reg_independent)
    
    tryCatch({
      # Prepare formula
      if (input$include_intercept) {
        formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
      } else {
        formula_str <- paste(input$reg_dependent, "~ -1 +", paste(input$reg_independent, collapse = " + "))
      }
      
      # Fit regression model
      reg_model <- lm(as.formula(formula_str), data = values$processed_data)
      values$analysis_results$regression <- reg_model
      
      # Regression assumptions
      assumptions_results <- list()
      
      if (input$check_normality_res) {
        residuals <- residuals(reg_model)
        assumptions_results$normality <- shapiro.test(residuals)
      }
      
      if (input$check_homoscedasticity) {
        if (requireNamespace("lmtest", quietly = TRUE)) {
          assumptions_results$homoscedasticity <- lmtest::bptest(reg_model)
        } else {
          assumptions_results$homoscedasticity <- "lmtest package not available"
        }
      }
      
      if (input$check_independence) {
        if (requireNamespace("car", quietly = TRUE)) {
          assumptions_results$independence <- car::durbinWatsonTest(reg_model)
        } else {
          assumptions_results$independence <- "car package not available"
        }
      }
      
      if (input$check_multicollinearity && length(input$reg_independent) > 1) {
        if (requireNamespace("car", quietly = TRUE)) {
          assumptions_results$multicollinearity <- car::vif(reg_model)
        } else {
          assumptions_results$multicollinearity <- "car package not available"
        }
      }
      
      values$analysis_results$regression_assumptions <- assumptions_results
      
      # Generate interpretation
      r_squared <- summary(reg_model)$r.squared
      adj_r_squared <- summary(reg_model)$adj.r.squared
      f_stat <- summary(reg_model)$fstatistic
      f_pvalue <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      
      values$interpretations$regression <- paste0(
        "Model regresi linear berganda menjelaskan ", round(r_squared * 100, 2), 
        "% variasi dalam ", input$reg_dependent, " (R² = ", round(r_squared, 4), ", ",
        "Adjusted R² = ", round(adj_r_squared, 4), "). ",
        "Dengan α = ", input$alpha_reg, ", ",
        if (f_pvalue < input$alpha_reg) {
          "model secara keseluruhan signifikan (F-test p-value < α)."
        } else {
          "model secara keseluruhan tidak signifikan (F-test p-value ≥ α)."
        },
        " Koefisien regresi menunjukkan pengaruh setiap variabel independen terhadap variabel dependen."
      )
      
    }, error = function(e) {
      showNotification(paste("Error dalam regresi:", e$message), type = "error")
    })
  })
  
  output$regression_results <- renderPrint({
    req(values$analysis_results$regression)
    summary(values$analysis_results$regression)
  })
  
  output$regression_diagnostics <- renderPlot({
    req(values$analysis_results$regression)
    par(mfrow = c(2, 2))
    plot(values$analysis_results$regression)
  })
  
  output$regression_assumptions <- renderPrint({
    req(values$analysis_results$regression_assumptions)
    values$analysis_results$regression_assumptions
  })
  
  output$regression_interpretation <- renderUI({
    if (!is.null(values$interpretations$regression)) {
      HTML(paste0("<h4>📊 Interpretasi Regresi Linear Berganda</h4>",
                  "<p>", values$interpretations$regression, "</p>",
                  "<p><strong>Diagnostic Plots:</strong></p>",
                  "<ul>",
                  "<li><strong>Residuals vs Fitted:</strong> Menguji linearitas dan homoskedastisitas</li>",
                  "<li><strong>Normal Q-Q:</strong> Menguji normalitas residual</li>",
                  "<li><strong>Scale-Location:</strong> Menguji homoskedastisitas</li>",
                  "<li><strong>Residuals vs Leverage:</strong> Mengidentifikasi outlier dan influential points</li>",
                  "</ul>"
      ))
    }
  })
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("StatInsight_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$processed_data)
      write.csv(values$processed_data, file, row.names = FALSE)
    }
  )
  
  output$download_current_plot <- downloadHandler(
    filename = function() {
      paste0("StatInsight_Plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$current_plot)
      ggsave(file, values$current_plot, width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      paste0("StatInsight_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Create a comprehensive report
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Copy the report file to a temporary directory
      report_content <- '
---
title: "StatInsight Pro - Laporan Analisis Statistik"
date: "`r Sys.Date()`"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Ringkasan Analisis

Laporan ini berisi hasil analisis statistik yang dilakukan menggunakan StatInsight Pro Dashboard.

## Data Overview

Dataset yang dianalisis memiliki karakteristik sebagai berikut:
- Total observasi: `r if(!is.null(values$processed_data)) nrow(values$processed_data) else "N/A"`
- Total variabel: `r if(!is.null(values$processed_data)) ncol(values$processed_data) else "N/A"`

## Interpretasi Hasil

```{r results, results="asis"}
if (!is.null(values$interpretations$descriptive)) {
  cat("### Statistik Deskriptif\\n")
  cat(values$interpretations$descriptive, "\\n\\n")
}

if (!is.null(values$interpretations$plot)) {
  cat("### Visualisasi Data\\n")
  cat(values$interpretations$plot, "\\n\\n")
}

if (!is.null(values$interpretations$assumptions)) {
  cat("### Uji Asumsi\\n")
  cat(values$interpretations$assumptions, "\\n\\n")
}

if (!is.null(values$interpretations$mean_test)) {
  cat("### Uji Beda Rata-rata\\n")
  cat(values$interpretations$mean_test, "\\n\\n")
}

if (!is.null(values$interpretations$anova)) {
  cat("### ANOVA\\n")
  cat(values$interpretations$anova, "\\n\\n")
}

if (!is.null(values$interpretations$regression)) {
  cat("### Regresi Linear Berganda\\n")
  cat(values$interpretations$regression, "\\n\\n")
}
```

---
Generated by StatInsight Pro Dashboard
'
      
      writeLines(report_content, tempReport)
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file, 
                       params = list(data = values$processed_data,
                                   interpretations = values$interpretations),
                       envir = new.env(parent = globalenv()))
    }
  )
  
  # Package downloads
  output$download_complete_package <- downloadHandler(
    filename = function() {
      paste0("StatInsight_Complete_Analysis_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Save data
      if (!is.null(values$processed_data)) {
        write.csv(values$processed_data, file.path(temp_dir, "data.csv"), row.names = FALSE)
      }
      
      # Save plots
      if (!is.null(values$current_plot)) {
        ggsave(file.path(temp_dir, "visualization.png"), values$current_plot, 
               width = 10, height = 8, dpi = 300)
      }
      
      # Save interpretations
      interpretations_text <- paste(
        "STATINSIGHT PRO - INTERPRETASI HASIL ANALISIS",
        "=" %R% 50,
        "",
        if (!is.null(values$interpretations$descriptive)) {
          paste("STATISTIK DESKRIPTIF:", values$interpretations$descriptive, "", sep = "\n")
        } else "",
        if (!is.null(values$interpretations$plot)) {
          paste("VISUALISASI DATA:", values$interpretations$plot, "", sep = "\n")
        } else "",
        if (!is.null(values$interpretations$assumptions)) {
          paste("UJI ASUMSI:", values$interpretations$assumptions, "", sep = "\n")
        } else "",
        if (!is.null(values$interpretations$mean_test)) {
          paste("UJI BEDA RATA-RATA:", values$interpretations$mean_test, "", sep = "\n")
        } else "",
        if (!is.null(values$interpretations$anova)) {
          paste("ANOVA:", values$interpretations$anova, "", sep = "\n")
        } else "",
        if (!is.null(values$interpretations$regression)) {
          paste("REGRESI LINEAR BERGANDA:", values$interpretations$regression, "", sep = "\n")
        } else "",
        "",
        paste("Generated on:", Sys.time()),
        sep = "\n"
      )
      
      writeLines(interpretations_text, file.path(temp_dir, "interpretations.txt"))
      
      # Create zip file
      zip_files <- list.files(temp_dir, full.names = TRUE)
      zip_files <- zip_files[basename(zip_files) %in% c("data.csv", "visualization.png", "interpretations.txt")]
      
      zip(file, zip_files, flags = "-j")
    }
  )
}

# Helper function for string repetition
`%R%` <- function(x, n) {
  paste(rep(x, n), collapse = "")
}