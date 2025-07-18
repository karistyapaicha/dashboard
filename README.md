# StatInsight Pro - Dashboard Analisis Statistik Terpadu

![StatInsight Pro](https://img.shields.io/badge/StatInsight-Pro-blue?style=for-the-badge)
![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-9cf?style=for-the-badge)

## 🎯 Tentang Dashboard

**StatInsight Pro** adalah dashboard analisis statistik terpadu yang dirancang khusus untuk membantu peneliti, analis data, dan akademisi dalam melakukan analisis statistik komprehensif dengan mudah dan efisien. Dashboard ini menyediakan antarmuka yang intuitif untuk berbagai jenis analisis statistik, mulai dari eksplorasi data hingga pemodelan regresi berganda.

## ✨ Fitur Utama

### 🏠 Beranda
- **Metadata Dashboard**: Informasi lengkap tentang dataset yang dimuat
- **Ringkasan Statistik**: Overview cepat tentang struktur data
- **Value Boxes**: Indikator visual untuk total observasi, variabel, dan tipe data

### 📊 Manajemen Data
- **Upload Data**: Mendukung format CSV dan Excel (.xlsx, .xls)
- **Transformasi Data**: 
  - Kategorisasi variabel kontinyu (interval, quantile, custom)
  - Transformasi logaritma natural
  - Transformasi akar kuadrat
  - Standardisasi (z-score)
  - Normalisasi min-max
- **Interpretasi Otomatis**: Penjelasan hasil transformasi

### 🔍 Eksplorasi Data
#### Statistik Deskriptif
- Perhitungan mean, median, standar deviasi
- Kuartil dan nilai ekstrim
- Analisis berdasarkan kelompok
- Interpretasi otomatis hasil

#### Visualisasi Data
- **Histogram**: Distribusi frekuensi
- **Boxplot**: Identifikasi outlier dan kuartil
- **Scatter Plot**: Hubungan antar variabel dengan trendline
- **Bar Chart**: Frekuensi kategori
- **Correlation Matrix**: Matriks korelasi dengan heatmap
- **Density Plot**: Estimasi distribusi probabilitas

#### Peta & Tabel
- **Peta Interaktif**: Visualisasi data geografis dengan Leaflet
- **Tabel Interaktif**: Pencarian, pengurutan, dan paginasi

### ✅ Uji Asumsi Data
#### Uji Normalitas
- **Shapiro-Wilk Test**: Untuk sampel ≤ 5000
- **Anderson-Darling Test**: Alternatif untuk sampel besar
- **Lilliefors Test**: Modifikasi Kolmogorov-Smirnov

#### Uji Homogenitas
- **Levene Test**: Robust terhadap non-normalitas
- **Bartlett Test**: Untuk data normal

#### Visualisasi
- **Q-Q Plot**: Pemeriksaan normalitas visual

### 📈 Statistik Inferensia

#### Uji Beda Rata-rata
- **One Sample t-test**: Membandingkan rata-rata dengan nilai hipotesis
- **Two Sample t-test**: Membandingkan dua kelompok independen
- **Paired t-test**: Membandingkan pengukuran berpasangan

#### Uji Proporsi & Variance
- **One/Two Sample Proportion Test**: Uji proporsi
- **One Sample Variance Test**: Uji varians dengan chi-square
- **F-test**: Perbandingan varians dua kelompok

#### ANOVA
- **One-Way ANOVA**: Perbandingan >2 kelompok
- **Two-Way ANOVA**: Dua faktor dengan/tanpa interaksi
- **Post-hoc Test**: Tukey HSD untuk perbandingan spesifik

### 📊 Regresi Linear Berganda
- **Model Fitting**: Regresi dengan multiple predictors
- **Diagnostic Plots**: 4 plot diagnostik standar
- **Uji Asumsi Regresi**:
  - Linearitas
  - Normalitas residual (Shapiro-Wilk)
  - Homoskedastisitas (Breusch-Pagan)
  - Independensi (Durbin-Watson)
  - Multikolinearitas (VIF)

### 💾 Pusat Download
#### Download Individual
- **Gambar**: JPG/PNG resolusi tinggi
- **Laporan**: PDF dengan interpretasi lengkap
- **Data**: CSV terproses
- **Dokumen**: Word dengan hasil analisis

#### Download Paket
- **Paket Eksplorasi**: Statistik deskriptif + visualisasi
- **Paket Uji Asumsi**: Hasil uji + plot diagnostik
- **Paket Inferensia**: Semua uji statistik + interpretasi
- **Paket Regresi**: Model + diagnostik + asumsi
- **Paket Lengkap**: Semua hasil analisis dalam ZIP

## 🚀 Instalasi dan Penggunaan

### Persyaratan Sistem
- R versi 4.0.0 atau lebih baru
- RStudio (opsional, namun direkomendasikan)

### Instalasi Package

1. **Clone atau download repository ini**
```bash
git clone https://github.com/username/statinsight-pro.git
cd statinsight-pro
```

2. **Install package yang diperlukan**
```r
# Jalankan script instalasi
source("install_packages.R")
```

3. **Jalankan dashboard**
```r
# Buka R/RStudio dan jalankan
shiny::runApp()
```

### Package Dependencies

Dashboard ini memerlukan package R berikut:
- `shiny` - Framework web aplikasi
- `shinydashboard` - Layout dashboard
- `DT` - Tabel interaktif
- `plotly` - Visualisasi interaktif
- `ggplot2` - Visualisasi statis
- `dplyr` - Manipulasi data
- `leaflet` - Peta interaktif
- `shinycssloaders` - Loading animations
- `shinyWidgets` - Widget tambahan
- `readr` - Import CSV
- `readxl` - Import Excel
- `car` - Uji statistik lanjutan
- `nortest` - Uji normalitas
- `broom` - Tidy model output
- `knitr` - Report generation
- `rmarkdown` - Document generation
- `officer` - Word documents
- `flextable` - Tabel untuk dokumen
- `corrplot` - Visualisasi korelasi
- `VIM` - Visualisasi missing values
- `mice` - Multiple imputation
- `psych` - Analisis psikometri

## 📖 Panduan Penggunaan

### 1. Upload Data
1. Klik menu "📊 Manajemen Data"
2. Pilih file CSV atau Excel
3. Sesuaikan pengaturan separator dan quote jika diperlukan
4. Data akan dimuat secara otomatis

### 2. Eksplorasi Data
1. Navigasi ke "🔍 Eksplorasi Data"
2. Pilih submenu yang diinginkan:
   - **Statistik Deskriptif**: Analisis numerik
   - **Visualisasi Data**: Grafik dan chart
   - **Peta & Tabel**: Visualisasi geografis

### 3. Uji Asumsi
1. Masuk ke "✅ Uji Asumsi Data"
2. Pilih variabel yang akan diuji
3. Centang uji yang diinginkan
4. Klik "Jalankan Uji Asumsi"

### 4. Analisis Inferensia
1. Pilih menu "📈 Statistik Inferensia"
2. Pilih submenu sesuai jenis uji:
   - **Uji Beda Rata-rata**: t-test
   - **Uji Proporsi & Variance**: Proporsi dan F-test
   - **ANOVA**: Perbandingan multiple groups

### 5. Regresi
1. Masuk ke "📊 Regresi Linear Berganda"
2. Pilih variabel dependen dan independen
3. Atur pengaturan uji asumsi
4. Klik "Jalankan Regresi"

### 6. Download Hasil
1. Kunjungi "💾 Pusat Download"
2. Pilih format download yang diinginkan
3. Gunakan paket download untuk hasil komprehensif

## 🎨 Interpretasi Otomatis

Setiap analisis dilengkapi dengan interpretasi otomatis yang mencakup:
- **Penjelasan Metode**: Apa yang diuji dan bagaimana
- **Interpretasi Hasil**: Kesimpulan berdasarkan p-value dan statistik uji
- **Rekomendasi**: Langkah selanjutnya berdasarkan hasil
- **Visualisasi**: Penjelasan grafik dan plot

## 📊 Contoh Penggunaan

### Analisis Deskriptif
```r
# Data akan otomatis dianalisis setelah upload
# Interpretasi: "Analisis statistik deskriptif telah dilakukan pada 5 variabel 
# dengan 150 observasi. Statistik yang dihitung meliputi rata-rata, median, 
# standar deviasi, nilai minimum dan maksimum, serta kuartil pertama dan ketiga."
```

### Uji Normalitas
```r
# Hasil interpretasi otomatis:
# "Uji normalitas dilakukan dengan 3 metode. Jika p-value > 0.05, data 
# berdistribusi normal. Hasil menunjukkan beberapa uji mendukung asumsi normalitas."
```

### Regresi Linear
```r
# Interpretasi otomatis:
# "Model regresi linear berganda menjelaskan 75.2% variasi dalam variabel_y 
# (R² = 0.752, Adjusted R² = 0.745). Dengan α = 0.05, model secara keseluruhan 
# signifikan (F-test p-value < α)."
```

## 🔧 Kustomisasi

Dashboard dapat dikustomisasi dengan:
- Mengubah tema warna di `ui.R`
- Menambah metode statistik baru di `server.R`
- Menyesuaikan interpretasi otomatis
- Menambah format download baru

## 🐛 Troubleshooting

### Error Package Not Found
```r
# Solusi: Install package yang hilang
install.packages("nama_package")
```

### Error File Upload
- Pastikan file dalam format CSV atau Excel
- Periksa encoding file (UTF-8 direkomendasikan)
- Pastikan tidak ada karakter khusus di nama kolom

### Error Analisis Statistik
- Periksa apakah data memenuhi asumsi uji
- Pastikan variabel yang dipilih sesuai (numerik/kategorik)
- Periksa apakah ada missing values yang perlu ditangani

## 📝 Lisensi

Dashboard ini dikembangkan untuk tujuan edukasi dan penelitian. Silakan gunakan dan modifikasi sesuai kebutuhan dengan tetap mencantumkan sumber.

## 🤝 Kontribusi

Kontribusi sangat diterima! Silakan:
1. Fork repository ini
2. Buat branch fitur baru
3. Commit perubahan Anda
4. Push ke branch
5. Buat Pull Request

## 📞 Kontak

Untuk pertanyaan, saran, atau laporan bug, silakan buat issue di repository ini.

---

**StatInsight Pro** - Mengubah data menjadi insight yang bermakna! 📊✨