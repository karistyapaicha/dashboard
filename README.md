# StatInsight Pro - Dashboard Analisis Statistik Terpadu

## 📊 Tentang Aplikasi

StatInsight Pro adalah dashboard analisis statistik terpadu yang dirancang untuk membantu peneliti, analis data, dan akademisi dalam melakukan analisis statistik komprehensif dengan mudah dan efisien.

## 🗂️ Struktur File

Aplikasi ini telah dikombinasikan menjadi struktur yang lebih sederhana:

### File Utama:
- **`app_combined.R`** - File utama untuk menjalankan aplikasi
- **`ui_combined.R`** - File UI yang berisi semua komponen antarmuka pengguna
- **`server_combined.R`** - File server yang berisi semua logika server dan fungsi

### File Data:
- **`sovi_data.csv`** - Dataset contoh untuk testing
- **`distance.csv`** - Dataset tambahan

### File Dokumentasi:
- **`README.md`** - Dokumentasi aplikasi
- **`1-s2.0-S2352340921010180-main.pdf`** - Dokumentasi penelitian

## ✨ Fitur Utama

### 🏠 Beranda
- Overview aplikasi dan metadata dataset
- Ringkasan statistik dasar
- Value boxes untuk informasi cepat

### 📊 Manajemen Data
- Upload file CSV dan Excel
- Transformasi data (log, sqrt, standardisasi, normalisasi, kategorisasi)
- Preview data interaktif

### 🔍 Eksplorasi Data
- **Statistik Deskriptif**: Analisis deskriptif lengkap dengan grouping
- **Visualisasi Data**: Histogram, boxplot, scatter plot, bar chart, density plot, correlation matrix
- **Peta & Tabel**: Peta interaktif untuk data geografis dan tabel dengan filtering

### ✅ Uji Asumsi Data
- Uji normalitas (Shapiro-Wilk, Anderson-Darling, Lilliefors)
- Uji homogenitas (Levene, Bartlett)
- Q-Q plot untuk diagnostik normalitas

### 📈 Statistik Inferensia
- **Uji Beda Rata-rata**: One-sample, two-sample, dan paired t-test
- **Uji Proporsi & Variance**: One-sample dan two-sample tests
- **ANOVA**: One-way dan two-way ANOVA dengan post-hoc Tukey HSD

### 📊 Regresi Linear Berganda
- Model regresi dengan multiple predictors
- Diagnostic plots
- Uji asumsi regresi (multikolinearitas, normalitas residual, dll.)

### 💾 Pusat Download
- Download individual (plots, reports, data, tables)
- Download paket per menu (eksplorasi, asumsi, inferensia, regresi)
- Download analisis lengkap

## 🚀 Cara Menjalankan

1. **Pastikan R dan RStudio terinstall**

2. **Install packages yang diperlukan:**
```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly", "ggplot2", 
  "dplyr", "leaflet", "shinycssloaders", "shinyWidgets",
  "readr", "readxl", "car", "nortest", "broom", "knitr",
  "rmarkdown", "officer", "flextable", "corrplot", "VIM",
  "mice", "psych", "reshape2"
))
```

3. **Jalankan aplikasi:**
```r
# Metode 1: Langsung dari file utama
source("app_combined.R")

# Metode 2: Menggunakan runApp()
shiny::runApp("app_combined.R")
```

## 📁 Struktur Kode

### UI Components (`ui_combined.R`)
- Dashboard header dan sidebar
- Tab items untuk setiap menu
- Input controls dan output containers
- Custom CSS styling

### Server Logic (`server_combined.R`)
- Reactive values untuk data storage
- Event handlers untuk user interactions
- Statistical analysis functions
- Download handlers
- Output renderers

## 🎯 Penggunaan

1. **Upload Data**: Mulai dengan mengupload file CSV atau Excel di menu "Manajemen Data"
2. **Eksplorasi**: Gunakan menu "Eksplorasi Data" untuk memahami karakteristik data
3. **Uji Asumsi**: Periksa asumsi statistik sebelum melakukan analisis inferensia
4. **Analisis**: Lakukan uji statistik yang sesuai dengan jenis data dan penelitian
5. **Download**: Simpan hasil analisis dalam berbagai format

## 🔧 Kustomisasi

Aplikasi dapat dikustomisasi dengan:
- Menambah jenis plot baru di bagian visualization
- Menambah uji statistik tambahan
- Mengubah tema dan styling CSS
- Menambah format export baru

## 📝 Catatan Pengembangan

- Aplikasi menggunakan reactive programming pattern
- Data disimpan dalam reactive values untuk efisiensi
- Error handling terintegrasi untuk stabilitas
- Interpretasi otomatis untuk membantu pengguna

## 🤝 Kontribusi

Untuk pengembangan lebih lanjut, silakan:
1. Fork repository ini
2. Buat branch untuk fitur baru
3. Submit pull request dengan deskripsi yang jelas

## 📞 Support

Jika mengalami masalah atau memiliki saran, silakan buat issue di repository ini.

---

**StatInsight Pro v2.0** - Dashboard Analisis Statistik Terpadu