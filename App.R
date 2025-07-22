library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(car)
library(e1071)
library(dplyr)
library(sf)
library(leaflet)
library(lmtest)
library(MASS)

sovi_data <- read_delim("sovi_data.csv", 
                        delim = ";", 
                        col_types = cols(
                          DISTRICTCODE = col_character(),
                          PROVINCE_NAME = col_character(),
                          CITY_NAME = col_character()
                        ))

peta_indonesia <- st_read("indonesia.geojson")
matriks_distance <- read.csv("distance.csv", row.names = 1, sep = ";", check.names = FALSE)
matriks_distance <- as.matrix(matriks_distance)
colnames(matriks_distance) <- rownames(matriks_distance)

sovi_data_bantu <- sovi_data
var_numerik <- names(sovi_data)[sapply(sovi_data, is.numeric)]

for (v in var_numerik) {
  var_bantu <- paste0("bantu_", v)
  median_val <- median(sovi_data[[v]], na.rm = TRUE)
  sovi_data_bantu[[var_bantu]] <- ifelse(sovi_data[[v]] > median_val, "Tinggi", "Rendah")
  sovi_data_bantu[[var_bantu]] <- factor(sovi_data_bantu[[var_bantu]])
}

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard SoVI Indonesia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("sliders-h")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("flask")),
      menuItem("Regresi", tabName = "regresi", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                tabBox(
                  width = 12,
                  title = "Beranda Dashboard",
                  tabPanel("Informasi Umum",
                           box(
                             width = 12,
                             title = "Dashboard Kerentanan Sosial (SoVI) di Indonesia",
                             status = "primary",
                             solidHeader = TRUE,
                             HTML("
                         <p><strong>Deskripsi:</strong></p>
                         <p>Dashboard ini menyajikan analisis eksploratif dan inferensia berdasarkan data <em>Social Vulnerability Index (SoVI)</em> Indonesia dalam format CSV. Dataset ini memuat 511 wilayah administratif beserta indikator sosial seperti kemiskinan, pendidikan rendah, akses listrik, air bersih, dan lainnya.</p>

                         <p><strong>Tujuan:</strong></p>
                         <ul>
                           <li>Menggambarkan persebaran dan profil sosial wilayah di Indonesia.</li>
                           <li>Melakukan analisis statistik terhadap indikator-indikator sosial.</li>
                           <li>Menunjukkan hubungan antar variabel dan mengidentifikasi wilayah yang rentan secara sosial.</li>
                         </ul>

                         <p><strong>Sumber Data:</strong></p>
                         <ul>
                           <li><a href='https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv' target='_blank'>Data SoVI (CSV)</a></li>
                           <li><a href='https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv' target='_blank'>Matriks Jarak</a></li>
                           <li><a href='https://www.sciencedirect.com/science/article/pii/S2352340921010180' target='_blank'>Metadata (ScienceDirect)</a></li>
                         </ul>

                         <p><strong>Disusun oleh:</strong><br>
                         Nama: Alifia Deanita<br>
                         NIM: 222312960<br>
                         Kelas: 2KS3<br>
                         </p>
                       ")
                           ),
                           box(
                             width = 12,
                             title = "Panduan Menu Dashboard",
                             status = "warning",
                             solidHeader = TRUE,
                             HTML("
                         <ul>
                           <li><strong>Beranda:</strong> Penjelasan umum, metadata, dan tujuan dashboard</li>
                           <li><strong>Manajemen Data:</strong> Kategorisasi variabel numerik ke dalam kelompok</li>
                           <li><strong>Eksplorasi Data:</strong> Statistik deskriptif, grafik, dan peta interaktif</li>
                           <li><strong>Uji Asumsi:</strong> Uji normalitas dan homogenitas varians</li>
                           <li><strong>Statistik Inferensia:</strong> Uji rata-rata, proporsi, dan ANOVA</li>
                           <li><strong>Regresi:</strong> Model regresi linear berganda dan uji asumsi pendukung</li>
                         </ul>
                       ")
                           )
                  ),
                  tabPanel("Metadata Variabel",
                           box(
                             width = 12,
                             title = "Metadata Dinamis Dataset SoVI",
                             status = "success",
                             solidHeader = TRUE,
                             DTOutput("tabel_metadata_sovi")
                           )
                  )
                )
              )
      ),
      
      tabItem(tabName = "manajemen",
              fluidRow(
                box(width = 4, title = "Pengaturan Kategorisasi",
                    status = "primary", solidHeader = TRUE,
                    selectInput("var_kategori", "Pilih Variabel Kontinu:",
                                choices = setdiff(
                                  names(sovi_data)[sapply(sovi_data, is.numeric)],
                                  c("DISTRICTCODE", "POPULATION")  # disaring
                                )),
                    numericInput("n_kategori", "Jumlah Kategori:", value = 3, min = 2, max = 6),
                    selectInput("metode_kat", "Metode Kategorisasi:",
                                choices = c("Quantile", "Equal Interval")),
                    actionButton("btn_kategorisasi", "Kategorikan", icon = icon("cogs"))
                ),
                
                box(width = 8, title = "Tabel Hasil Kategorisasi", status = "success", solidHeader = TRUE,
                    DTOutput("tabel_kategorisasi")
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Keterangan Kategori Otomatis",
                    status = "info", solidHeader = TRUE,
                    tableOutput("keterangan_kategori")
                ),
                
                box(width = 6, title = "Interpretasi Otomatis",
                    status = "warning", solidHeader = TRUE,
                    textOutput("interpretasi_kategorisasi")
                )
              )
      ),
      
      tabItem(tabName = "eksplorasi",
              h2("Eksplorasi Data"),
              tabsetPanel(
                tabPanel("Statistik Deskriptif",
                         fluidRow(
                           box(width = 4, title = "Pengaturan Variabel",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_eksplorasi", "Pilih Variabel Numerik:",
                                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)],
                                           selected = "POVERTY"),
                                
                               selectInput("jenis_grafik", "Pilih Jenis Grafik:",
                                           choices = c("Histogram", "Boxplot", "Density")),
                               downloadButton("unduh_grafik", "Unduh Grafik"),
                               downloadButton("unduh_data", "Unduh Data")
                           ),
                           box(width = 8, title = "Statistik Deskriptif",
                               status = "success", solidHeader = TRUE,
                               tableOutput("tabel_statistik")
                           )
                         ),
                         fluidRow(
                           box(width = 6, title = "Grafik Visualisasi",
                               status = "info", solidHeader = TRUE,
                               plotOutput("plot_grafik")
                           ),
                           box(width = 6, title = "Interpretasi Otomatis",
                               status = "warning", solidHeader = TRUE,
                               textOutput("interpretasi_eksplorasi")
                           )
                         )
                ),
               
                tabPanel("Peta Interaktif",
                         fluidRow(
                           box(width = 4, title = "Pengaturan Peta",
                               status = "primary", solidHeader = TRUE,
                               selectInput("mode_peta", "Tampilkan Peta Berdasarkan:",
                                           choices = c("Nilai Variabel", "Cluster Spasial")),
                               
                               # Untuk mode 'Nilai Variabel'
                               conditionalPanel(
                                 condition = "input.mode_peta == 'Nilai Variabel'",
                                 selectInput("var_peta", "Pilih Variabel Numerik:",
                                             choices = names(peta_indonesia)[sapply(peta_indonesia, is.numeric)],
                                             selected = "POVERTY")
                               ),
                               
                               # Untuk mode 'Cluster Spasial'
                               conditionalPanel(
                                 condition = "input.mode_peta == 'Cluster Spasial'",
                                 sliderInput("jumlah_cluster", "Jumlah Cluster:", min = 2, max = 10, value = 3)
                               )
                           ),
                           box(width = 8, title = "Peta Sebaran Nilai",
                               status = "info", solidHeader = TRUE,
                               leafletOutput("peta_sovi", height = 550)
                           ),
                           box(width = 12, title = "Interpretasi Peta", status = "warning", solidHeader = TRUE,
                               textOutput("interpretasi_peta"))
                         )
                ),
                
                tabPanel("Perbandingan Provinsi",
                         fluidRow(
                           box(width = 4, title = "Pengaturan",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_banding", "Pilih Variabel:",
                                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)],
                                           selected = "POVERTY"),
                           ),
                           box(width = 8, title = "Boxplot Per Provinsi",
                               status = "success", solidHeader = TRUE,
                               plotOutput("plot_banding_provinsi")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Interpretasi Perbandingan",
                               status = "warning", solidHeader = TRUE,
                               textOutput("interpretasi_banding_provinsi")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "asumsi",
              h2("Uji Asumsi Dasar"),
              tabsetPanel(
                tabPanel("Uji Normalitas",
                         fluidRow(
                           box(width = 4, title = "Pengaturan",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_normal", "Pilih Variabel Numerik:",
                                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)],
                                           selected = "POVERTY")
                               ),
                           box(width = 8, title = "Hasil Uji Shapiro-Wilk",
                               status = "success", solidHeader = TRUE,
                               verbatimTextOutput("hasil_normalisasi"),
                               textOutput("interpretasi_normal")
                               )
                            )
                ),
                tabPanel("Uji Homogenitas",
                         fluidRow(
                           box(width = 4, title = "Pengaturan",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_homo", "Pilih Variabel Numerik:",
                                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)],
                                           selected = "POVERTY"),
                               selectInput("group_homo", "Pilih Variabel Kategorik:",
                                           choices = c("PROVINCE_NAME"))
                           ),
                           box(width = 8, title = "Hasil Uji Levene",
                               status = "success", solidHeader = TRUE,
                               verbatimTextOutput("hasil_homogenitas"),
                               textOutput("interpretasi_homo")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "inferensia",
              h2("Statistik Inferensia"),
              tabsetPanel(
                tabPanel("Uji Rata-rata 1 Kelompok",
                         fluidRow(
                           box(width = 4, title = "Pengaturan Uji",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_mean1", "Pilih Variabel Numerik:",
                                           choices = var_numerik),
                               numericInput("nilai_pembanding", "Nilai Pembanding:", value = 50),
                               actionButton("uji_mean1", "Lakukan Uji")
                           ),
                           box(width = 8, title = "Hasil Uji t (1 Kelompok)",
                               status = "success", solidHeader = TRUE,
                               verbatimTextOutput("hasil_mean1"),
                               textOutput("interpretasi_mean1")
                           )
                         )
                ),
                
                tabPanel("Uji Rata-rata 2 Kelompok",
                         fluidRow(
                           box(width = 4, title = "Pengaturan Uji",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_mean2", "Pilih Variabel Numerik:",
                                           choices = var_numerik),
                               uiOutput("pilih_group_bantu"),
                               actionButton("uji_mean2", "Lakukan Uji")
                           ),
                           box(width = 8, title = "Hasil Uji t (2 Kelompok)",
                               status = "success", solidHeader = TRUE,
                               verbatimTextOutput("hasil_mean2"),
                               textOutput("interpretasi_mean2")
                           )
                         )
                ),
                tabPanel("Uji Proporsi",
                         tabsetPanel(
                           tabPanel("Proporsi 1 Kelompok",
                                    fluidRow(
                                      box(width = 4, title = "Pengaturan",
                                          status = "primary", solidHeader = TRUE,
                                          numericInput("x_proporsi", "Jumlah Kejadian (x):", value = 50, min = 0),
                                          numericInput("n_proporsi", "Total Sampel (n):", value = 100, min = 1),
                                          numericInput("p0_proporsi", "Proporsi Hipotetik (p₀):", value = 0.5, min = 0, max = 1),
                                          actionButton("btn_uji_proporsi", "Lakukan Uji")
                                      ),
                                      box(width = 8, title = "Hasil Uji Proporsi Satu Sampel",
                                          status = "success", solidHeader = TRUE,
                                          verbatimTextOutput("hasil_uji_proporsi"),
                                          textOutput("interpretasi_uji_proporsi")
                                      )
                                    )
                           ),
                           tabPanel("Proporsi 2 Kelompok",
                                    fluidRow(
                                      box(width = 4, title = "Pengaturan",
                                          status = "primary", solidHeader = TRUE,
                                          uiOutput("select_var_kategorik_2prop"),
                                          uiOutput("select_var_biner_2prop")
                                      ),
                                      box(width = 8, title = "Hasil Uji Proporsi Dua Kelompok",
                                          status = "success", solidHeader = TRUE,
                                          verbatimTextOutput("hasil_uji_2prop"),
                                          textOutput("interpretasi_uji_2prop")
                                      )
                                    )
                           )
                         )
                ),
              
                tabPanel("Uji Varians",
                         fluidRow(
                           box(width = 4, title = "Pengaturan Uji Varians",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_variance", "Pilih Variabel Numerik:", 
                                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                               selectInput("group_variance", "Pilih Variabel Kategorik (2 Kelompok):",
                                           choices = names(sovi_data_bantu)[grepl("^bantu_", names(sovi_data_bantu))])
                           ),
                           box(width = 8, title = "Hasil Uji F Varians Dua Sampel",
                               status = "success", solidHeader = TRUE,
                               verbatimTextOutput("hasil_uji_varians"),
                               textOutput("interpretasi_uji_varians")
                           )
                         )
                ),
                tabPanel("Uji ANOVA",
                         fluidRow(
                           box(width = 4, title = "Pengaturan ANOVA",
                               status = "primary", solidHeader = TRUE,
                               selectInput("var_anova", "Pilih Variabel Numerik:", 
                                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                               selectInput("group_anova1", "Faktor 1 (Kategorik):", 
                                           choices = names(sovi_data_bantu)[grepl("^bantu_", names(sovi_data_bantu))]),
                               selectInput("group_anova2", "Faktor 2 (Kategorik):", 
                                           choices = names(sovi_data_bantu)[grepl("^bantu_", names(sovi_data_bantu))])
                           ),
                           box(width = 8, title = "Hasil ANOVA",
                               status = "success", solidHeader = TRUE,
                               verbatimTextOutput("hasil_anova"),
                               textOutput("interpretasi_anova")
                           )
                         )
                )
              )
      ),
          
      tabItem(tabName = "regresi",
              h2("Regresi Linear Berganda"),
              fluidRow(
                box(width = 4, title = "Pengaturan Model",
                    status = "primary", solidHeader = TRUE,
                    selectInput("y_regresi", "Pilih Variabel Y (Respon):", 
                                choices = names(sovi_data)[sapply(sovi_data, is.numeric)], selected = "POVERTY"),
                    selectInput("x_regresi", "Pilih Variabel X (Prediktor):",
                                choices = names(sovi_data)[sapply(sovi_data, is.numeric)], 
                                multiple = TRUE)
                ),
                box(width = 8, title = "Hasil Model Regresi", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("output_model_regresi"))
              ),
              
              fluidRow(
                box(width = 6, title = "Uji Normalitas (Shapiro-Wilk Residual)",
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("uji_normalitas_residual"),
                    textOutput("interpretasi_normalitas_residual")
                ),
                box(width = 6, title = "Uji Homoskedastisitas (Breusch-Pagan)",
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("uji_homoskedastisitas"),
                    textOutput("interpretasi_homoskedastisitas")
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Uji Autokorelasi (Durbin-Watson)",
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("uji_autokorelasi"),
                ),
                box(width = 6, title = "Uji Multikolinieritas (VIF)",
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("uji_vif"),
                    textOutput("interpretasi_vif")
                )
              )
      )
  
    )
  )
)

server <- function(input, output) {
  output$tabel_metadata_sovi <- renderDT({
    df <- sovi_data
   
    metadata_id <- data.frame(
      Label = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC",
                "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED",
                "NOSEWER", "TAPWATER", "POPULATION", "PROVINCE_NAME", "CITY_NAME"),
      Variabel = c("Kode Wilayah", "Anak-anak", "Perempuan", "Lansia", "Kepala Keluarga Perempuan",
                   "Ukuran Keluarga", "Tidak Listrik", "Pendidikan Rendah", "Pertumbuhan Penduduk",
                   "Kemiskinan", "Buta Huruf", "Tidak Pelatihan Bencana", "Rawan Bencana",
                   "Sewa Rumah", "Tanpa Drainase", "Sumber Air Pipa", "Populasi", "Nama Provinsi", "Nama Kota/Kabupaten"),
      Deskripsi = c(
        "Kode dari wilayah/kabupaten",
        "Persentase penduduk usia di bawah lima tahun",
        "Persentase penduduk perempuan",
        "Persentase penduduk usia 65 tahun ke atas",
        "Persentase rumah tangga dengan kepala keluarga perempuan",
        "Rata-rata jumlah anggota rumah tangga per wilayah",
        "Persentase rumah tangga yang tidak menggunakan listrik sebagai penerangan",
        "Persentase penduduk usia ≥15 tahun dengan pendidikan rendah",
        "Persentase perubahan jumlah penduduk",
        "Persentase penduduk miskin",
        "Persentase penduduk yang tidak bisa membaca dan menulis",
        "Persentase rumah tangga yang tidak mengikuti pelatihan bencana",
        "Persentase rumah tangga yang tinggal di daerah rawan bencana",
        "Persentase rumah tangga yang menyewa rumah",
        "Persentase rumah tangga tanpa sistem drainase",
        "Persentase rumah tangga yang menggunakan air perpipaan",
        "Jumlah total penduduk",
        "Nama provinsi tempat wilayah berada",
        "Nama kabupaten/kota tempat wilayah berada"
      )
    )
    
    meta_auto <- data.frame(
      Label = names(df),
      Tipe_Data = sapply(df, function(x) class(x)[1]),
      Jumlah_Nilai_Unik = sapply(df, function(x) length(unique(x))),
      Data_Kosong = sapply(df, function(x) sum(is.na(x))),
      Contoh_Nilai = sapply(df, function(x) paste(head(unique(x), 3), collapse = ", "))
    )
    
    meta_final <- merge(meta_auto, metadata_id, by = "Label", all.x = TRUE)
    meta_final <- meta_final[match(names(df), meta_final$Label), ]
    
    urutan_kolom <- c("Label", "Variabel", "Deskripsi", setdiff(names(meta_final), c("Label", "Variabel", "Deskripsi")))
    meta_final <- meta_final[, urutan_kolom]
    
    datatable(meta_final, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Buat penyimpanan global untuk hasil kategorisasi
  kategorisasi_data <- reactiveValues(
    hasil = NULL,
    label_kategori = NULL
  )
  
  observeEvent(input$btn_kategorisasi, {
    req(input$var_kategori, input$n_kategori, input$metode_kat)
    
    var <- input$var_kategori
    n <- input$n_kategori
    df <- sovi_data
    
    # Tentukan batas berdasarkan metode
    if (input$metode_kat == "Quantile") {
      bins <- quantile(df[[var]], probs = seq(0, 1, length.out = n + 1), na.rm = TRUE, names = FALSE)
    } else {
      bins <- seq(min(df[[var]], na.rm = TRUE), max(df[[var]], na.rm = TRUE), length.out = n + 1)
    }
    
    # Buat label kategori otomatis berdasarkan rentang
    label_kat <- c()
    for (i in 1:n) {
      lower <- round(bins[i], 2)
      upper <- round(bins[i + 1], 2)
      label_kat[i] <- paste0("Kategori ", i, ": ", lower, " – ", upper)
    }
    
    # Terapkan cut
    kategori <- cut(df[[var]], breaks = bins, include.lowest = TRUE, labels = paste("Kategori", 1:n))
    
    # Simpan hasil
    df_kat <- data.frame(Wilayah = df$DISTRICTCODE, Nama_Kab = df$CITY_NAME, Nilai = df[[var]], Kategori = kategori)
    kategorisasi_data$hasil <- df_kat
    kategorisasi_data$label_kategori <- label_kat
    
    # Tampilkan tabel
    output$tabel_kategorisasi <- renderDT({
      datatable(df_kat, options = list(pageLength = 10))
    })
    
    # Interpretasi proporsi
    output$interpretasi_kategorisasi <- renderText({
      proporsi <- prop.table(table(kategori))
      interpretasi <- paste0(
        "Variabel '", var, "' berhasil dikategorikan ke dalam ", n, " kelompok. ",
        "Kategori terbanyak adalah ", names(which.max(proporsi)), " dengan proporsi ",
        round(100 * max(proporsi), 2), "% dari total wilayah."
      )
      interpretasi
    })
    
    # Tampilkan keterangan label kategori
    output$keterangan_kategori <- renderTable({
      data.frame(Kategori = paste("Kategori", 1:n), Rentang = label_kat)
    }, bordered = TRUE, spacing = "s")
  })
  
  output$plot_grafik <- renderPlot({
    req(input$var_eksplorasi)
    var <- sovi_data[[input$var_eksplorasi]]
    
    jenis <- input$jenis_grafik
    if (jenis == "Histogram") {
      hist(var, main = paste("Histogram", input$var_eksplorasi), col = "skyblue", border = "white")
    } else if (jenis == "Boxplot") {
      boxplot(var, horizontal = TRUE, main = paste("Boxplot", input$var_eksplorasi), col = "tomato")
    } else {
      plot(density(var, na.rm = TRUE), main = paste("Density", input$var_eksplorasi), col = "darkgreen", lwd = 2)
    }
  })
  
  output$tabel_statistik <- renderTable({
    req(input$var_eksplorasi)
    var <- input$var_eksplorasi
    data <- sovi_data[[var]]
    
    data.frame(
      Minimum = min(data, na.rm = TRUE),
      Maksimum = max(data, na.rm = TRUE),
      Rata_rata = mean(data, na.rm = TRUE),
      Median = median(data, na.rm = TRUE),
      SD = sd(data, na.rm = TRUE),
      Varians = var(data, na.rm = TRUE),
      Skewness = e1071::skewness(data, na.rm = TRUE),
      Kurtosis = e1071::kurtosis(data, na.rm = TRUE)
    )
  })
  
  clustered_peta <- reactive({
    req(input$mode_peta == "Cluster Spasial")
    
    hc <- hclust(as.dist(matriks_distance), method = "ward.D2")
    cluster_spasial <- cutree(hc, k = input$jumlah_cluster)
    
    temp <- peta_indonesia
    temp$cluster_spasial <- as.factor(cluster_spasial)
    return(temp)
  })
  
  output$peta_sovi <- renderLeaflet({
    req(peta_indonesia)
    
    if (input$mode_peta == "Nilai Variabel") {
      var <- input$var_peta
      pal <- colorNumeric("YlOrRd", domain = peta_indonesia[[var]])
      
      leaflet(peta_indonesia) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(get(var)),
          fillOpacity = 0.8, color = "white", weight = 1,
          label = ~paste0(nmkab, "<br>", var, ": ", round(get(var), 2))
        ) %>%
        addLegend("bottomright", pal = pal, values = peta_indonesia[[var]], title = var)
      
    } else {
      peta_cluster <- clustered_peta()
      pal <- colorFactor("Set2", domain = peta_cluster$cluster_spasial)
      
      leaflet(peta_cluster) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(cluster_spasial),
          fillOpacity = 0.8, color = "white", weight = 1,
          label = ~paste0(nmkab, "<br>Cluster: ", cluster_spasial)
        ) %>%
        addLegend("bottomright", pal = pal, values = peta_cluster$cluster_spasial,
                  title = paste("Cluster (k =", input$jumlah_cluster, ")"))
    }
  })
  output$interpretasi_peta <- renderText({
    req(peta_indonesia, input$mode_peta)
    
    if (input$mode_peta == "Nilai Variabel") {
      var <- input$var_peta
      
      # Cek apakah variabel valid
      req(var %in% names(peta_indonesia))
      
      # Ambil 3 wilayah nilai tertinggi dan terendah
      nilai_rendah <- peta_indonesia %>%
        arrange(.data[[var]]) %>%
        slice(1:3) %>%
        pull(nmkab)
      
      nilai_tinggi <- peta_indonesia %>%
        arrange(desc(.data[[var]])) %>%
        slice(1:3) %>%
        pull(nmkab)
      
      # Ambil range nilai variabel
      min_val <- round(min(peta_indonesia[[var]], na.rm = TRUE), 2)
      max_val <- round(max(peta_indonesia[[var]], na.rm = TRUE), 2)
      
      paste0(
        "Peta menampilkan sebaran nilai variabel *", var, "* di seluruh wilayah. ",
        "Nilai variabel berkisar antara ", min_val, " hingga ", max_val, ". ",
        "Wilayah dengan nilai tertinggi adalah: ", paste(nilai_tinggi, collapse = ", "), 
        "; sedangkan wilayah dengan nilai terendah adalah: ", paste(nilai_rendah, collapse = ", "), "."
      )
      
    } else {
      peta_cluster <- clustered_peta()
      
      summary_cluster <- peta_cluster %>%
        st_drop_geometry() %>%
        count(cluster_spasial) %>%
        arrange(desc(n))
      
      cluster_terbanyak <- summary_cluster$cluster_spasial[1]
      n_terbanyak <- summary_cluster$n[1]
      
      paste0(
        "Peta menampilkan hasil klasterisasi spasial berdasarkan matriks penimbang. ",
        "Sebanyak ", input$jumlah_cluster, " klaster terbentuk. ",
        "Klaster dengan jumlah wilayah terbanyak adalah Klaster ", cluster_terbanyak, 
        " (", n_terbanyak, " wilayah)."
      )
    }
  })
  
  output$unduh_data <- downloadHandler(
    filename = function() {
      paste0("data_eksplorasi_", input$var_eksplorasi, ".csv")
    },
    content = function(file) {
      write.csv(sovi_data[, input$var_eksplorasi, drop = FALSE], file, row.names = FALSE)
    }
  )
  
  output$unduh_grafik <- downloadHandler(
    filename = function() {
      paste0("grafik_", input$jenis_grafik, "_", input$var_eksplorasi, ".png")
    },
    content = function(file) {
      png(file)
      var <- sovi_data[[input$var_eksplorasi]]
      jenis <- input$jenis_grafik
      if (jenis == "Histogram") {
        hist(var, main = paste("Histogram", input$var_eksplorasi), col = "skyblue", border = "white")
      } else if (jenis == "Boxplot") {
        boxplot(var, horizontal = TRUE, main = paste("Boxplot", input$var_eksplorasi), col = "tomato")
      } else {
        plot(density(var, na.rm = TRUE), main = paste("Density", input$var_eksplorasi), col = "darkgreen", lwd = 2)
      }
      dev.off()
    }
  )
  
  output$plot_banding_provinsi <- renderPlot({
    req(input$var_banding)
    var <- input$var_banding
    boxplot(as.formula(paste(var, "~ nmprov")), data = peta_indonesia,
            main = paste("Perbandingan", var, "antar Provinsi"),
            col = "lightblue", las = 2, cex.axis = 0.7, outline = FALSE)
  })
  
  output$interpretasi_banding_provinsi <- renderText({
    req(input$var_banding)
    var <- input$var_banding
    df <- peta_indonesia[, c("nmprov", var)]
    agg <- aggregate(df[[var]], by = list(df$nmprov), FUN = mean, na.rm = TRUE)
    top <- agg[which.max(agg$x), ]
    bottom <- agg[which.min(agg$x), ]
    
    paste0("Provinsi dengan nilai rata-rata tertinggi untuk variabel '", var, "' adalah ", 
           top$Group.1, " dengan nilai ", round(top$x, 2),
           ", sedangkan terendah adalah ", bottom$Group.1, " dengan nilai ", round(bottom$x, 2), ".")
  })
  
  output$interpretasi_eksplorasi <- renderText({
    req(input$var_eksplorasi)
    var <- input$var_eksplorasi
    mean_val <- round(mean(sovi_data[[var]], na.rm = TRUE), 2)
    sd_val <- round(sd(sovi_data[[var]], na.rm = TRUE), 2)
    paste0("Rata-rata nilai ", var, " adalah ", mean_val,
           " dengan standar deviasi ", sd_val, ".")
  })
  
  output$hasil_normalisasi <- renderPrint({
    req(input$var_normal)
    hasil <- shapiro.test(sovi_data[[input$var_normal]])
    print(hasil)
  })
  
  output$interpretasi_normal <- renderText({
    hasil <- shapiro.test(sovi_data[[input$var_normal]])
    if (hasil$p.value > 0.05) {
      "P-value > 0.05. Data berdistribusi normal (tidak cukup bukti untuk menolak H0)."
    } else {
      "P-value <= 0.05. Data tidak berdistribusi normal (tolak H0)."
    }
  })
  
  output$hasil_homogenitas <- renderPrint({
    req(input$var_homo, input$group_homo)
    df <- sovi_data
    df[[input$group_homo]] <- as.factor(df[[input$group_homo]])
    car::leveneTest(as.formula(paste(input$var_homo, "~", input$group_homo)), data = df)
  })
  
  output$interpretasi_homo <- renderText({
    req(input$var_homo, input$group_homo)
    df <- sovi_data
    df[[input$group_homo]] <- as.factor(df[[input$group_homo]])
    hasil <- car::leveneTest(as.formula(paste(input$var_homo, "~", input$group_homo)), data = df)
    pval <- hasil$`Pr(>F)`[1]
    
    if (pval > 0.05) {
      paste0("P-value = ", round(pval, 4), 
             ". Tidak ada cukup bukti untuk menolak H0, sehingga dapat disimpulkan bahwa varians nilai ",
             input$var_homo, " antar provinsi adalah homogen (setara).")
    } else {
      paste0("P-value = ", round(pval, 4),
             ". Terdapat cukup bukti untuk menolak H0, sehingga varians nilai ",
             input$var_homo, " antar provinsi berbeda-beda (tidak homogen).")
    }
  })
  # Uji Proporsi (1 Sample)
  observeEvent(input$btn_uji_proporsi, {
    output$hasil_uji_proporsi <- renderPrint({
      prop.test(x = input$x_proporsi, n = input$n_proporsi, p = input$p0_proporsi)
    })
    
    output$interpretasi_uji_proporsi <- renderText({
      hasil <- prop.test(x = input$x_proporsi, n = input$n_proporsi, p = input$p0_proporsi)
      pval <- hasil$p.value
      if (pval > 0.05) {
        paste0("P-value = ", round(pval, 4), ". Tidak cukup bukti untuk menolak H0. Proporsi kejadian sama dengan p₀.")
      } else {
        paste0("P-value = ", round(pval, 4), ". Terdapat cukup bukti untuk menolak H0. Proporsi kejadian berbeda dari p₀.")
      }
    })
  })
  
  # Uji Varians (2 Kelompok) - F Test
  output$hasil_uji_varians <- renderPrint({
    req(input$var_variance, input$group_variance)
    var1 <- sovi_data_bantu[[input$var_variance]]
    group <- sovi_data_bantu[[input$group_variance]]
    split_vals <- split(var1, group)
    var.test(split_vals[[1]], split_vals[[2]])
  })
  
  output$interpretasi_uji_varians <- renderText({
    req(input$var_variance, input$group_variance)
    var1 <- sovi_data_bantu[[input$var_variance]]
    group <- sovi_data_bantu[[input$group_variance]]
    split_vals <- split(var1, group)
    hasil <- var.test(split_vals[[1]], split_vals[[2]])
    pval <- hasil$p.value
    if (pval > 0.05) {
      paste0("P-value = ", round(pval, 4), ". Tidak ada perbedaan signifikan varians antar dua kelompok.")
    } else {
      paste0("P-value = ", round(pval, 4), ". Terdapat perbedaan signifikan varians antar dua kelompok.")
    }
  })
  
  # ANOVA
  output$hasil_anova <- renderPrint({
    req(input$var_anova, input$group_anova1, input$group_anova2)
    formula_anova <- as.formula(paste(input$var_anova, "~", input$group_anova1, "*", input$group_anova2))
    summary(aov(formula_anova, data = sovi_data_bantu))
  })
  
  output$interpretasi_anova <- renderText({
    req(input$var_anova, input$group_anova1, input$group_anova2)
    model <- aov(as.formula(paste(input$var_anova, "~", input$group_anova1, "*", input$group_anova2)), data = sovi_data_bantu)
    hasil <- summary(model)
    pval <- hasil[[1]]$`Pr(>F)`
    faktor <- c(input$group_anova1, input$group_anova2, paste0(input$group_anova1, " * ", input$group_anova2))
    
    hasil_interpretasi <- paste0(
      "Hasil ANOVA:\n",
      paste0(faktor, ": P-value = ", round(pval, 4), collapse = "\n")
    )
    
    hasil_interpretasi
  })
  
  output$pilih_group_bantu <- renderUI({
    vars_bantu <- names(sovi_data_bantu)[grepl("^bantu_", names(sovi_data_bantu))]
    selectInput("group_bantu", "Pilih Variabel Kategorik (2 Kelompok):", choices = vars_bantu)
  })
  
  observeEvent(input$uji_mean1, {
    req(input$var_mean1, input$nilai_pembanding)
    var <- sovi_data[[input$var_mean1]]
    hasil <- t.test(var, mu = input$nilai_pembanding)
    output$hasil_mean1 <- renderPrint({ hasil })
    output$interpretasi_mean1 <- renderText({
      if (hasil$p.value < 0.05) {
        paste0("P-value = ", round(hasil$p.value, 4),
               ". Terdapat perbedaan signifikan antara rata-rata ", input$var_mean1,
               " dengan nilai pembanding ", input$nilai_pembanding, ".")
      } else {
        paste0("P-value = ", round(hasil$p.value, 4),
               ". Tidak terdapat perbedaan signifikan antara rata-rata ", input$var_mean1,
               " dengan nilai pembanding ", input$nilai_pembanding, ".")
      }
    })
  })
  
  observeEvent(input$uji_mean2, {
    req(input$var_mean2, input$group_bantu)
    df <- sovi_data_bantu
    df[[input$group_bantu]] <- as.factor(df[[input$group_bantu]])
    n_grup <- length(unique(df[[input$group_bantu]]))
    
    if (n_grup != 2) {
      output$hasil_mean2 <- renderPrint({ "Variabel kategorik harus memiliki tepat 2 kelompok." })
      output$interpretasi_mean2 <- renderText({ "" })
    } else {
      hasil <- t.test(as.formula(paste(input$var_mean2, "~", input$group_bantu)), data = df)
      output$hasil_mean2 <- renderPrint({ hasil })
      output$interpretasi_mean2 <- renderText({
        if (hasil$p.value < 0.05) {
          paste0("P-value = ", round(hasil$p.value, 4),
                 ". Terdapat perbedaan rata-rata ", input$var_mean2,
                 " yang signifikan antara dua kelompok pada variabel ", input$group_bantu, ".")
        } else {
          paste0("P-value = ", round(hasil$p.value, 4),
                 ". Tidak terdapat perbedaan rata-rata ", input$var_mean2,
                 " yang signifikan antara dua kelompok pada variabel ", input$group_bantu, ".")
        }
      })
    }
  })
  # --- Uji Proporsi Dua Kelompok ---
  # Pilihan variabel biner (grup) dari variabel bantu kategorik
  output$select_var_biner_2prop <- renderUI({
    var_biner <- names(sovi_data_bantu)[sapply(sovi_data_bantu, function(x) is.factor(x) && length(unique(x)) == 2)]
    
    selectInput("var_biner_2prop", "Pilih Variabel Biner (sebagai Grup):",
                choices = var_biner, selected = var_biner[1])
  })
  
  # Pilihan variabel kategorik (kejadian) dari variabel bantu kategorik juga
  output$select_var_kategorik_2prop <- renderUI({
    var_kat <- names(sovi_data_bantu)[sapply(sovi_data_bantu, function(x) is.factor(x) && length(unique(x)) == 2)]
    
    selectInput("var_kategorik_2prop", "Pilih Variabel Kategorik (sebagai Kejadian):",
                choices = var_kat, selected = var_kat[2])
  })
  
  # Output hasil uji proporsi
  output$hasil_uji_2prop <- renderPrint({
    req(input$var_kategorik_2prop, input$var_biner_2prop)
    df <- sovi_data_bantu
    df[[input$var_kategorik_2prop]] <- as.factor(df[[input$var_kategorik_2prop]])
    df[[input$var_biner_2prop]] <- as.factor(df[[input$var_biner_2prop]])
    
    tab <- table(df[[input$var_biner_2prop]], df[[input$var_kategorik_2prop]])
    
    if (nrow(tab) == 2 && ncol(tab) == 2) {
      prop.test(tab)
    } else {
      cat("❌ Error: Kedua variabel harus memiliki tepat 2 kategori.")
    }
  })
  
  # Interpretasi otomatis
  output$interpretasi_uji_2prop <- renderText({
    req(input$var_kategorik_2prop, input$var_biner_2prop)
    df <- sovi_data_bantu
    df[[input$var_kategorik_2prop]] <- as.factor(df[[input$var_kategorik_2prop]])
    df[[input$var_biner_2prop]] <- as.factor(df[[input$var_biner_2prop]])
    
    tab <- table(df[[input$var_biner_2prop]], df[[input$var_kategorik_2prop]])
    
    if (nrow(tab) == 2 && ncol(tab) == 2) {
      hasil <- prop.test(tab)
      pval <- hasil$p.value
      if (pval > 0.05) {
        paste0("P-value = ", round(pval, 4), 
               ". Tidak terdapat perbedaan signifikan proporsi antara kelompok ",
               input$var_biner_2prop, " terhadap kejadian ", input$var_kategorik_2prop, ".")
      } else {
        paste0("P-value = ", round(pval, 4), 
               ". Terdapat perbedaan signifikan proporsi antara kelompok ",
               input$var_biner_2prop, " terhadap kejadian ", input$var_kategorik_2prop, ".")
      }
    } else {
      "❌ Error: Kedua variabel harus memiliki tepat 2 kategori."
    }
  })
  # Model Regresi
  output$output_model_regresi <- renderPrint({
    req(input$y_regresi, input$x_regresi)
    formula <- as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+")))
    model <- lm(formula, data = sovi_data)
    summary(model)
  })
  
  output$uji_normalitas_residual <- renderPrint({
    req(input$y_regresi)
    if (length(input$x_regresi) == 0) {
      return("Silakan pilih minimal satu variabel X.")
    }
    
    model <- lm(as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+"))), data = sovi_data)
    shapiro.test(resid(model))
  })
  
  output$interpretasi_normalitas_residual <- renderText({
    req(input$y_regresi)
    if (length(input$x_regresi) == 0) {
      return("Silakan pilih minimal satu variabel X.")
    }
    
    model <- lm(as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+"))), data = sovi_data)
    pval <- shapiro.test(resid(model))$p.value
    if (pval > 0.05) {
      "P-value > 0.05 → Residual berdistribusi normal."
    } else {
      "P-value ≤ 0.05 → Residual tidak berdistribusi normal."
    }
  })
  
  output$uji_homoskedastisitas <- renderPrint({
    req(input$y_regresi)
    if (length(input$x_regresi) == 0) return("Silakan pilih minimal satu variabel X terlebih dahulu.")
    
    model <- lm(as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+"))), data = sovi_data)
    bptest(model)
  })
  
  output$interpretasi_homoskedastisitas <- renderText({
    req(input$y_regresi)
    if (length(input$x_regresi) == 0) return("Silakan pilih minimal satu variabel X terlebih dahulu.")
    
    model <- lm(as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+"))), data = sovi_data)
    pval <- bptest(model)$p.value
    if (pval > 0.05) {
      "P-value > 0.05 → Varians residual homogen (homoskedastik)."
    } else {
      "P-value ≤ 0.05 → Varians residual tidak homogen (heteroskedastik)."
    }
  })
  
  # Uji Autokorelasi (Durbin-Watson)
  output$uji_autokorelasi <- renderPrint({
    req(input$y_regresi, input$x_regresi)
    
    tryCatch({
      formula <- as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+")))
      model <- lm(formula, data = sovi_data)
      dwtest(model)
    }, error = function(e) {
      cat("❌ Error Durbin-Watson:", e$message)
    })
  })
  output$uji_vif <- renderPrint({
    req(input$y_regresi)
    if (length(input$x_regresi) < 2) return("Uji VIF hanya dapat dilakukan jika jumlah variabel X ≥ 2.")
    
    model <- lm(as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+"))), data = sovi_data)
    vif(model)
  })
  
  output$interpretasi_vif <- renderText({
    req(input$y_regresi)
    if (length(input$x_regresi) < 2) return("Interpretasi VIF hanya tersedia jika jumlah variabel X ≥ 2.")
    
    model <- lm(as.formula(paste(input$y_regresi, "~", paste(input$x_regresi, collapse = "+"))), data = sovi_data)
    v <- vif(model)
    if (any(v > 10)) {
      "Terdapat indikasi multikolinieritas kuat (VIF > 10) pada beberapa variabel."
    } else if (any(v > 5)) {
      "Terdapat indikasi multikolinieritas sedang (VIF antara 5–10)."
    } else {
      "Tidak terdapat indikasi multikolinieritas (VIF < 5)."
    }
  })
  
  
}

shinyApp(ui, server)