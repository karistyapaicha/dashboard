# UI for StatInsight Pro Dashboard

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
      menuItem("📊 Manajemen Data", tabName = "data_management", icon = icon("database")),
      
      # Data Exploration
      menuItem("🔍 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line"),
               menuSubItem("Statistik Deskriptif", tabName = "stat_desk"),
               menuSubItem("Visualisasi Data", tabName = "visualisasi"),
               menuSubItem("Peta & Tabel", tabName = "peta_tabel")
      ),
      
      # Data Assumptions
      menuItem("✅ Uji Asumsi Data", tabName = "uji_asumsi", icon = icon("check-circle")),
      
      # Inferential Statistics
      menuItem("📈 Statistik Inferensia", icon = icon("calculator"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_beda_rata"),
               menuSubItem("Uji Proporsi & Variance", tabName = "uji_proporsi_var"),
               menuSubItem("ANOVA (>2 Kelompok)", tabName = "anova")
      ),
      
      # Multiple Linear Regression
      menuItem("📊 Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart")),
      
      # Download Center
      menuItem("💾 Pusat Download", tabName = "download", icon = icon("download"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        .interpretation-box {
          background-color: #e8f4f8;
          border: 1px solid #bee5eb;
          border-radius: 5px;
          padding: 15px;
          margin-top: 10px;
        }
        .metadata-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 15px;
          padding: 20px;
          margin-bottom: 20px;
        }
      "))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
        fluidRow(
          box(
            title = "Selamat Datang di StatInsight Pro", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "metadata-card",
              h3("🎯 Tentang Dashboard"),
              p("StatInsight Pro adalah dashboard analisis statistik terpadu yang dirancang untuk membantu peneliti, 
                 analis data, dan akademisi dalam melakukan analisis statistik komprehensif dengan mudah dan efisien."),
              
              h4("✨ Fitur Utama:"),
              tags$ul(
                tags$li("📊 Manajemen data dengan transformasi otomatis"),
                tags$li("🔍 Eksplorasi data dengan visualisasi interaktif"),
                tags$li("✅ Uji asumsi statistik (normalitas & homogenitas)"),
                tags$li("📈 Statistik inferensia lengkap"),
                tags$li("📊 Regresi linear berganda dengan diagnostik"),
                tags$li("💾 Export hasil dalam berbagai format")
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_observations", width = 3),
          valueBoxOutput("total_variables", width = 3),
          valueBoxOutput("numeric_vars", width = 3),
          valueBoxOutput("categorical_vars", width = 3)
        ),
        
        fluidRow(
          box(
            title = "📋 Metadata Dataset", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(DT::dataTableOutput("metadata_table"))
          ),
          
          box(
            title = "📊 Ringkasan Statistik", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(verbatimTextOutput("summary_stats"))
          )
        )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data_management",
        fluidRow(
          box(
            title = "📁 Upload Data", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            fileInput("file", "Pilih file CSV atau Excel:",
                     accept = c(".csv", ".xlsx", ".xls")),
            
            conditionalPanel(
              condition = "output.fileUploaded",
              checkboxInput("header", "Header", TRUE),
              radioButtons("sep", "Separator:",
                          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                          selected = ","),
              radioButtons("quote", "Quote:",
                          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                          selected = '"')
            )
          )
        ),
        
        fluidRow(
          box(
            title = "🔧 Transformasi Data", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("transform_var", "Pilih Variabel:", choices = NULL),
              selectInput("transform_type", "Jenis Transformasi:",
                         choices = list(
                           "Kategorisasi Kontinyu" = "categorize",
                           "Log Transformation" = "log",
                           "Square Root" = "sqrt",
                           "Standardization" = "scale",
                           "Normalization" = "normalize"
                         )),
              
              conditionalPanel(
                condition = "input.transform_type == 'categorize'",
                numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                radioButtons("cat_method", "Metode Kategorisasi:",
                            choices = list("Equal Intervals" = "interval",
                                         "Quantiles" = "quantile",
                                         "Custom Breaks" = "custom"))
              ),
              
              actionButton("apply_transform", "Terapkan Transformasi", class = "btn-warning")
            )
          ),
          
          box(
            title = "📊 Preview Data", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(DT::dataTableOutput("data_preview"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi Transformasi", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("transform_interpretation"))
            )
          )
        )
      ),
      
      # Statistik Deskriptif Tab
      tabItem(tabName = "stat_desk",
        fluidRow(
          box(
            title = "⚙️ Pengaturan Analisis", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("desc_vars", "Pilih Variabel:", 
                         choices = NULL, multiple = TRUE),
              selectInput("group_var", "Kelompok Berdasarkan:", 
                         choices = NULL),
              checkboxInput("include_na", "Sertakan Missing Values", FALSE),
              actionButton("run_descriptive", "Jalankan Analisis", class = "btn-primary")
            )
          ),
          
          box(
            title = "📊 Statistik Deskriptif", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            withSpinner(DT::dataTableOutput("descriptive_table"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi Statistik Deskriptif", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("descriptive_interpretation"))
            )
          )
        )
      ),
      
      # Visualisasi Tab
      tabItem(tabName = "visualisasi",
        fluidRow(
          box(
            title = "🎨 Pengaturan Visualisasi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("plot_type", "Jenis Plot:",
                         choices = list(
                           "Histogram" = "histogram",
                           "Boxplot" = "boxplot",
                           "Scatter Plot" = "scatter",
                           "Bar Chart" = "bar",
                           "Correlation Matrix" = "correlation",
                           "Density Plot" = "density"
                         )),
              
              selectInput("x_var", "Variabel X:", choices = NULL),
              conditionalPanel(
                condition = "input.plot_type == 'scatter'",
                selectInput("y_var", "Variabel Y:", choices = NULL)
              ),
              
              selectInput("color_var", "Warna Berdasarkan:", choices = NULL),
              
              actionButton("create_plot", "Buat Visualisasi", class = "btn-primary")
            )
          ),
          
          box(
            title = "📈 Visualisasi Data", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            withSpinner(plotlyOutput("main_plot", height = "500px"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi Visualisasi", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("plot_interpretation"))
            )
          )
        )
      ),
      
      # Peta & Tabel Tab
      tabItem(tabName = "peta_tabel",
        fluidRow(
          box(
            title = "🗺️ Peta Interaktif", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            conditionalPanel(
              condition = "output.hasGeoData",
              selectInput("lat_var", "Variabel Latitude:", choices = NULL),
              selectInput("lon_var", "Variabel Longitude:", choices = NULL),
              selectInput("marker_var", "Variabel Marker:", choices = NULL),
              actionButton("create_map", "Buat Peta", class = "btn-primary")
            ),
            withSpinner(leafletOutput("interactive_map", height = "400px"))
          ),
          
          box(
            title = "📋 Tabel Interaktif", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(DT::dataTableOutput("interactive_table"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi Peta & Tabel", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("map_table_interpretation"))
            )
          )
        )
      ),
      
      # Uji Asumsi Tab
      tabItem(tabName = "uji_asumsi",
        fluidRow(
          box(
            title = "⚙️ Pengaturan Uji Asumsi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("assumption_var", "Pilih Variabel:", choices = NULL),
              selectInput("assumption_group", "Kelompok (opsional):", choices = NULL),
              
              h4("Uji Normalitas:"),
              checkboxInput("shapiro_test", "Shapiro-Wilk Test", TRUE),
              checkboxInput("anderson_test", "Anderson-Darling Test", TRUE),
              checkboxInput("lillie_test", "Lilliefors Test", TRUE),
              
              h4("Uji Homogenitas:"),
              checkboxInput("levene_test", "Levene Test", TRUE),
              checkboxInput("bartlett_test", "Bartlett Test", TRUE),
              
              actionButton("run_assumptions", "Jalankan Uji Asumsi", class = "btn-primary")
            )
          ),
          
          box(
            title = "📊 Hasil Uji Normalitas", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            withSpinner(verbatimTextOutput("normality_results"))
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Hasil Uji Homogenitas", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(verbatimTextOutput("homogeneity_results"))
          ),
          
          box(
            title = "📈 Q-Q Plot", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(plotOutput("qq_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi Uji Asumsi", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("assumption_interpretation"))
            )
          )
        )
      ),
      
      # Uji Beda Rata-rata Tab
      tabItem(tabName = "uji_beda_rata",
        fluidRow(
          box(
            title = "⚙️ Pengaturan Uji Beda Rata-rata", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("mean_test_var", "Variabel Numerik:", choices = NULL),
              
              radioButtons("mean_test_type", "Jenis Uji:",
                          choices = list(
                            "One Sample t-test" = "one_sample",
                            "Two Sample t-test" = "two_sample",
                            "Paired t-test" = "paired"
                          )),
              
              conditionalPanel(
                condition = "input.mean_test_type == 'one_sample'",
                numericInput("test_value", "Nilai Uji (μ₀):", value = 0)
              ),
              
              conditionalPanel(
                condition = "input.mean_test_type == 'two_sample'",
                selectInput("group_var_mean", "Variabel Kelompok:", choices = NULL),
                checkboxInput("equal_var", "Asumsi Varians Sama", TRUE)
              ),
              
              conditionalPanel(
                condition = "input.mean_test_type == 'paired'",
                selectInput("paired_var2", "Variabel Kedua:", choices = NULL)
              ),
              
              numericInput("alpha_mean", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              
              actionButton("run_mean_test", "Jalankan Uji", class = "btn-primary")
            )
          ),
          
          box(
            title = "📊 Hasil Uji Beda Rata-rata", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            withSpinner(verbatimTextOutput("mean_test_results"))
          )
        ),
        
        fluidRow(
          box(
            title = "📈 Visualisasi Hasil", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(plotOutput("mean_test_plot"))
          ),
          
          box(
            title = "💡 Interpretasi Uji Beda Rata-rata", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("mean_test_interpretation"))
            )
          )
        )
      ),
      
      # Uji Proporsi & Variance Tab
      tabItem(tabName = "uji_proporsi_var",
        fluidRow(
          box(
            title = "⚙️ Pengaturan Uji", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              
              tabsetPanel(
                tabPanel("Uji Proporsi",
                  br(),
                  selectInput("prop_var", "Variabel Kategorik:", choices = NULL),
                  selectInput("prop_category", "Kategori yang Diuji:", choices = NULL),
                  
                  radioButtons("prop_test_type", "Jenis Uji:",
                              choices = list(
                                "One Sample Proportion" = "one_prop",
                                "Two Sample Proportion" = "two_prop"
                              )),
                  
                  conditionalPanel(
                    condition = "input.prop_test_type == 'one_prop'",
                    numericInput("prop_test_value", "Proporsi Uji (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)
                  ),
                  
                  conditionalPanel(
                    condition = "input.prop_test_type == 'two_prop'",
                    selectInput("prop_group_var", "Variabel Kelompok:", choices = NULL)
                  ),
                  
                  numericInput("alpha_prop", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  actionButton("run_prop_test", "Jalankan Uji Proporsi", class = "btn-primary")
                ),
                
                tabPanel("Uji Variance",
                  br(),
                  selectInput("var_test_var", "Variabel Numerik:", choices = NULL),
                  
                  radioButtons("var_test_type", "Jenis Uji:",
                              choices = list(
                                "One Sample Variance" = "one_var",
                                "Two Sample Variance (F-test)" = "two_var"
                              )),
                  
                  conditionalPanel(
                    condition = "input.var_test_type == 'one_var'",
                    numericInput("var_test_value", "Variance Uji (σ²₀):", value = 1, min = 0.01)
                  ),
                  
                  conditionalPanel(
                    condition = "input.var_test_type == 'two_var'",
                    selectInput("var_group_var", "Variabel Kelompok:", choices = NULL)
                  ),
                  
                  numericInput("alpha_var", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  actionButton("run_var_test", "Jalankan Uji Variance", class = "btn-primary")
                )
              )
            )
          ),
          
          box(
            title = "📊 Hasil Uji", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            tabsetPanel(
              tabPanel("Hasil Uji Proporsi",
                withSpinner(verbatimTextOutput("prop_test_results"))
              ),
              tabPanel("Hasil Uji Variance",
                withSpinner(verbatimTextOutput("var_test_results"))
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📈 Visualisasi Hasil", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(plotOutput("prop_var_plot"))
          ),
          
          box(
            title = "💡 Interpretasi", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("prop_var_interpretation"))
            )
          )
        )
      ),
      
      # ANOVA Tab
      tabItem(tabName = "anova",
        fluidRow(
          box(
            title = "⚙️ Pengaturan ANOVA", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("anova_dependent", "Variabel Dependen:", choices = NULL),
              
              radioButtons("anova_type", "Jenis ANOVA:",
                          choices = list(
                            "One-Way ANOVA" = "one_way",
                            "Two-Way ANOVA" = "two_way"
                          )),
              
              selectInput("anova_factor1", "Faktor 1:", choices = NULL),
              
              conditionalPanel(
                condition = "input.anova_type == 'two_way'",
                selectInput("anova_factor2", "Faktor 2:", choices = NULL),
                checkboxInput("interaction", "Sertakan Interaksi", TRUE)
              ),
              
              numericInput("alpha_anova", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              
              checkboxInput("post_hoc", "Uji Post-Hoc (Tukey HSD)", TRUE),
              
              actionButton("run_anova", "Jalankan ANOVA", class = "btn-primary")
            )
          ),
          
          box(
            title = "📊 Hasil ANOVA", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            withSpinner(verbatimTextOutput("anova_results"))
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Uji Post-Hoc", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(verbatimTextOutput("posthoc_results"))
          ),
          
          box(
            title = "📈 Visualisasi ANOVA", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(plotOutput("anova_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi ANOVA", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("anova_interpretation"))
            )
          )
        )
      ),
      
      # Regresi Tab
      tabItem(tabName = "regresi",
        fluidRow(
          box(
            title = "⚙️ Pengaturan Regresi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            conditionalPanel(
              condition = "output.dataLoaded",
              selectInput("reg_dependent", "Variabel Dependen (Y):", choices = NULL),
              selectInput("reg_independent", "Variabel Independen (X):", choices = NULL, multiple = TRUE),
              
              checkboxInput("include_intercept", "Sertakan Intercept", TRUE),
              numericInput("alpha_reg", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              
              h4("Uji Asumsi:"),
              checkboxInput("check_linearity", "Uji Linearitas", TRUE),
              checkboxInput("check_normality_res", "Uji Normalitas Residual", TRUE),
              checkboxInput("check_homoscedasticity", "Uji Homoskedastisitas", TRUE),
              checkboxInput("check_independence", "Uji Independensi", TRUE),
              checkboxInput("check_multicollinearity", "Uji Multikolinearitas", TRUE),
              
              actionButton("run_regression", "Jalankan Regresi", class = "btn-primary")
            )
          ),
          
          box(
            title = "📊 Hasil Regresi", 
            status = "info", 
            solidHeader = TRUE, 
            width = 8,
            withSpinner(verbatimTextOutput("regression_results"))
          )
        ),
        
        fluidRow(
          box(
            title = "📈 Diagnostic Plots", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(plotOutput("regression_diagnostics"))
          ),
          
          box(
            title = "📊 Uji Asumsi Regresi", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            withSpinner(verbatimTextOutput("regression_assumptions"))
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Interpretasi Regresi", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
                withSpinner(htmlOutput("regression_interpretation"))
            )
          )
        )
      ),
      
      # Download Tab
      tabItem(tabName = "download",
        fluidRow(
          box(
            title = "💾 Pusat Download", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("📊 Download Individual:"),
            fluidRow(
              column(4,
                h5("Gambar (JPG/PNG):"),
                downloadButton("download_plots", "Download Semua Plot", class = "btn-info btn-block"),
                br(), br(),
                downloadButton("download_current_plot", "Download Plot Saat Ini", class = "btn-info btn-block")
              ),
              
              column(4,
                h5("Laporan (PDF):"),
                downloadButton("download_report_pdf", "Download Laporan PDF", class = "btn-success btn-block"),
                br(), br(),
                downloadButton("download_results_word", "Download Hasil Word", class = "btn-success btn-block")
              ),
              
              column(4,
                h5("Data & Tabel:"),
                downloadButton("download_data", "Download Data CSV", class = "btn-warning btn-block"),
                br(), br(),
                downloadButton("download_tables", "Download Semua Tabel", class = "btn-warning btn-block")
              )
            ),
            
            hr(),
            
            h4("📋 Download Gabungan Per Menu:"),
            fluidRow(
              column(3,
                downloadButton("download_descriptive_package", "Paket Eksplorasi Data", class = "btn-primary btn-block")
              ),
              column(3,
                downloadButton("download_assumptions_package", "Paket Uji Asumsi", class = "btn-primary btn-block")
              ),
              column(3,
                downloadButton("download_inference_package", "Paket Statistik Inferensia", class = "btn-primary btn-block")
              ),
              column(3,
                downloadButton("download_regression_package", "Paket Regresi", class = "btn-primary btn-block")
              )
            ),
            
            hr(),
            
            h4("📦 Download Lengkap:"),
            downloadButton("download_complete_package", "Download Analisis Lengkap", class = "btn-danger btn-lg btn-block")
          )
        ),
        
        fluidRow(
          box(
            title = "ℹ️ Informasi Download", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,
            div(class = "interpretation-box",
              h5("Format File yang Tersedia:"),
              tags$ul(
                tags$li("📊 Gambar: JPG, PNG dengan resolusi tinggi"),
                tags$li("📄 Laporan: PDF dengan interpretasi lengkap"),
                tags$li("📝 Dokumen: Word dengan tabel dan hasil"),
                tags$li("📋 Data: CSV dengan data yang telah diproses"),
                tags$li("📦 Paket: ZIP berisi semua file terkait menu")
              ),
              
              h5("Isi Paket Download:"),
              tags$ul(
                tags$li("🔍 Paket Eksplorasi: Statistik deskriptif, visualisasi, tabel"),
                tags$li("✅ Paket Uji Asumsi: Hasil uji normalitas, homogenitas, plot diagnostik"),
                tags$li("📈 Paket Inferensia: Semua hasil uji statistik dengan interpretasi"),
                tags$li("📊 Paket Regresi: Model regresi, diagnostik, asumsi"),
                tags$li("📦 Paket Lengkap: Semua hasil analisis dalam satu paket")
              )
            )
          )
        )
      )
    )
  )
)