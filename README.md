# SOVIExplorer: Dashboard Analisis Kerentanan Sosial di Indonesia

Proyek ini merupakan bagian dari Ujian Akhir Semester Mata Kuliah **Komputasi Statistik** di Politeknik Statistika STIS. Dashboard ini dikembangkan menggunakan **R Shiny** untuk menganalisis kerentanan sosial wilayah di Indonesia berdasarkan berbagai indikator sosial dan ekonomi.

---

## Isi Folder
Semua file proyek ini berada dalam satu folder:
- `sovi_data.csv` → Dataset utama
- `distance.csv` → matrik jarak antar wilayah
- `ui.R`, `server.R`, `global.R` → Script utama RShiny
- `laporan_UAS.pdf` → Laporan akhir UAS
- `Fakta_integritas_KOMSTAT.pdf` → Form pernyataan integritas
- `plot_output_*.jpg` → Grafik hasil analisis
- `hasil_analisis.txt` → Interpretasi analisis statistik
- `README.md` → Dokumentasi proyek ini

---

## Fitur Dashboard
- **Manajemen Data**: Transformasi variabel numerik menjadi kategorik
- **Eksplorasi Data**: Statistik deskriptif, visualisasi grafik, peta interaktif
- **Uji Asumsi**: Normalitas dan homogenitas varians
- **Statistik Inferensia**:
  - Uji beda rata-rata (1 dan 2 kelompok)
  - Uji proporsi dan varians
  - ANOVA satu arah dan dua arah
- **Regresi Linear Berganda**: Pemodelan indeks SOVI terhadap indikator sosial ekonomi
- **Fitur Download**: Semua output grafik dan interpretasi bisa diunduh

---

## Variabel Kunci
- **Variabel Tak Bebas (Respon)**: `SOVI` *(Social Vulnerability Index)*  
- **Variabel Bebas (Prediktor)**:  
  `CHILDREN`, `FEMALE`, `ELDERLY`, `FHEAD`, `FAMILYSIZE`, `LOWEDU`, `NOELECTRIC`, `RENTED`, `POVERTY`, `ILLITERATE`, `NOTRAINING`, `NOSEWER`, `TAPWATER`, `DPRONE`, `GROWTH`

---

## Tools & Package R
Dashboard ini dibangun menggunakan:
- `shiny`, `shinydashboard`, `shinythemes`
- `ggplot2`, `dplyr`, `car`, `readr`, `psych`
- `performance`, `lmtest`, `nortest`

---

## Informasi Penulis
- **Nama**: Alifia Deanita  
- **NIM**: 222312960 
- **Kelas**: 2KS3  
- **Tahun**: 2025

---

## Referensi Dataset & Metadata
- Data utama: [`sovi_data.csv`](https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv)
- Metadata: [Artikel di ScienceDirect](https://www.sciencedirect.com/science/article/pii/S2352340921010180)
