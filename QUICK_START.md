# 🚀 StatInsight Pro - Panduan Cepat

## Memulai dalam 3 Langkah

### 1️⃣ Install Dependencies
```r
source("install_packages.R")
```

### 2️⃣ Jalankan Dashboard
```r
source("run_dashboard.R")
```
**ATAU**
```r
shiny::runApp()
```

### 3️⃣ Upload Data & Mulai Analisis!
- Gunakan file `sample_data.csv` untuk testing
- Atau upload file CSV/Excel Anda sendiri

## 📊 Fitur Utama

### 🏠 Beranda
- Overview dataset
- Metadata dan statistik ringkas

### 📊 Manajemen Data
- Upload CSV/Excel
- Transformasi data (kategorisasi, normalisasi, dll)

### 🔍 Eksplorasi Data
- **Statistik Deskriptif**: Mean, median, SD, kuartil
- **Visualisasi**: Histogram, boxplot, scatter plot, correlation matrix
- **Peta & Tabel**: Visualisasi geografis dan tabel interaktif

### ✅ Uji Asumsi
- **Normalitas**: Shapiro-Wilk, Anderson-Darling, Lilliefors
- **Homogenitas**: Levene, Bartlett
- **Q-Q Plot**: Visualisasi normalitas

### 📈 Statistik Inferensia
- **Uji t**: One-sample, two-sample, paired
- **Uji Proporsi**: One/two sample proportion test
- **Uji Varians**: F-test, chi-square test
- **ANOVA**: One-way, two-way dengan post-hoc

### 📊 Regresi Linear Berganda
- Model fitting dengan multiple predictors
- Diagnostic plots (4 plot standar)
- Uji asumsi: linearitas, normalitas, homoskedastisitas, independensi, multikolinearitas

### 💾 Download Center
- **Individual**: Gambar, PDF, Word, CSV
- **Paket**: Eksplorasi, Uji Asumsi, Inferensia, Regresi
- **Lengkap**: Semua hasil dalam satu ZIP

## 🎯 Tips Penggunaan

### Upload Data
- Format: CSV, Excel (.xlsx, .xls)
- Pastikan nama kolom tidak mengandung spasi atau karakter khusus
- Encoding UTF-8 direkomendasikan

### Analisis Statistik
- Pilih variabel yang sesuai (numerik untuk uji t, kategorik untuk proporsi)
- Periksa asumsi sebelum melakukan uji inferensia
- Gunakan interpretasi otomatis sebagai panduan

### Visualisasi
- Histogram/Density: Untuk melihat distribusi
- Boxplot: Identifikasi outlier
- Scatter plot: Hubungan antar variabel
- Correlation matrix: Korelasi multiple variables

### Download
- Gunakan "Download Lengkap" untuk semua hasil
- PDF report berisi interpretasi otomatis
- Gambar dalam resolusi tinggi untuk publikasi

## 🔧 Troubleshooting

### Error Package
```r
# Install ulang packages
source("install_packages.R")
```

### Error Upload Data
- Periksa format file (CSV/Excel)
- Pastikan tidak ada karakter khusus di header
- Coba dengan sample_data.csv terlebih dahulu

### Error Analisis
- Pastikan variabel yang dipilih sesuai tipe data
- Periksa apakah ada missing values
- Untuk uji inferensia, pastikan asumsi terpenuhi

## 📱 Interface

### Sidebar Menu
- 🏠 **Beranda**: Overview dan metadata
- 📊 **Manajemen Data**: Upload dan transformasi
- 🔍 **Eksplorasi Data**: Statistik dan visualisasi
- ✅ **Uji Asumsi**: Normalitas dan homogenitas
- 📈 **Statistik Inferensia**: t-test, ANOVA, proporsi
- 📊 **Regresi**: Multiple linear regression
- 💾 **Download**: Export hasil

### Main Panel
- **Settings Panel**: Pengaturan analisis (kiri)
- **Results Panel**: Hasil analisis (kanan)
- **Interpretation Box**: Interpretasi otomatis (bawah)

## 🎨 Interpretasi Otomatis

Setiap analisis dilengkapi interpretasi yang mencakup:
- **Metode**: Penjelasan uji yang dilakukan
- **Hasil**: Kesimpulan berdasarkan p-value
- **Rekomendasi**: Langkah selanjutnya
- **Visualisasi**: Penjelasan grafik

## 📞 Support

Jika mengalami masalah:
1. Periksa file `README.md` untuk dokumentasi lengkap
2. Pastikan R version >= 4.0.0
3. Update packages ke versi terbaru
4. Restart R session jika perlu

---

**Happy Analyzing! 📊✨**