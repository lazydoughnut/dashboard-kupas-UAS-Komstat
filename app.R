library(shiny)
library(shinyjs)
library(bs4Dash)
library(DT)
library(dplyr)
library(rmarkdown)
library(ggplot2)
library(plotly)
library(patchwork) 
library(sf)         
library(leaflet)     
library(spdep)
library(car)
library(lmtest)
library(corrplot) 
library(htmltools)
library(officer)
library(flextable)

data_spasial <- st_read("sovi_data_super_simplified.geojson")
distance_matrix <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")

data_spasial <- data_spasial %>%
  rename(
    KODE_KAB = kddistrict,
    NAMA_WILAYAH = nmkab,
    KEMISKINAN = POVERTY,
    RUMAH_TANGGA_TANPA_LISTRIK = NOELECTRIC,
    RUMAH_TANGGA_TANPA_DRAINASE = NOSEWER,
    RUMAH_TANGGA_AIR_PIPA = TAPWATER,
    PENDIDIKAN_RENDAH = LOWEDU,
    BUTA_HURUF = ILLITERATE
  )


data <- st_drop_geometry(data_spasial)

var_list <- c("KEMISKINAN", "RUMAH_TANGGA_TANPA_LISTRIK", "RUMAH_TANGGA_AIR_PIPA", "RUMAH_TANGGA_TANPA_DRAINASE", "PENDIDIKAN_RENDAH", "BUTA_HURUF")
label_map <- c("KEMISKINAN" = "Kemiskinan", "RUMAH_TANGGA_TANPA_LISTRIK" = "Rumah Tangga Tanpa Listrik", "RUMAH_TANGGA_AIR_PIPA" = "Rumah Tangga Guna Air Pipa", "RUMAH_TANGGA_TANPA_DRAINASE" = "Rumah Tangga Tanpa Drainase", "PENDIDIKAN_RENDAH" = "Pendidikan Rendah", "BUTA_HURUF" = "Buta Huruf")
cat_var_list <- setNames(paste0(names(label_map), "_cat"), paste("Kategori", label_map))

metadata_kamus <- list(KODE_KAB = list("Nama Variabel" = "Kode Kabupaten/Kota", Konsep = "Kode wilayah administratif kabupaten/kota.", Definisi = "Kode unik yang merepresentasikan setiap kabupaten/kota di Indonesia.", "Referensi Waktu" = "2017", Ukuran = "Kode numerik", Satuan = "–", "Tipe Data" = "Integer"), KEMISKINAN = list("Nama Variabel" = "Kemiskinan", Konsep = "Persentase penduduk miskin.", Definisi = "Persentase penduduk dengan pengeluaran di bawah garis kemiskinan.", "Referensi Waktu" = "2017 (Data BPS)", Ukuran = "Persentase", Satuan = "%", "Tipe Data" = "Numerik (float)"), RUMAH_TANGGA_TANPA_LISTRIK = list("Nama Variabel" = "Rumah Tangga Tanpa Listrik", Konsep = "Persentase rumah tangga tanpa listrik.", Definisi = "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan utama.", "Referensi Waktu" = "2017 (SUSENAS)", Ukuran = "Persentase", Satuan = "%", "Tipe Data" = "Numerik (float)"), RUMAH_TANGGA_TANPA_DRAINASE = list("Nama Variabel" = "Rumah Tangga Tanpa Sistem Drainase", Konsep = "Persentase rumah tangga tanpa sistem drainase  atau toilet/MCK.", Definisi = "Persentase rumah tangga yang tidak memiliki fasilitas sanitasi layak (toilet atau saluran pembuangan).", "Referensi Waktu" = "2017 (SUSENAS)", Ukuran = "Persentase", Satuan = "%", "Tipe Data" = "Numerik (float)"), RUMAH_TANGGA_AIR_PIPA = list("Nama Variabel" = "Rumah Tangga Guna Air Pipa", Konsep = "Persentase rumah tangga dengan sumber air dari jaringan pipa.", Definisi = "Persentase rumah tangga yang menggunakan air dari jaringan perpipaan sebagai sumber utama air bersih.", "Referensi Waktu" = "2017 (SUSENAS)", Ukuran = "Persentase", Satuan = "%", "Tipe Data" = "Numerik (float)"), PENDIDIKAN_RENDAH = list("Nama Variabel" = "Pendidikan Rendah", Konsep = "Persentase penduduk dengan pendidikan rendah.", Definisi = "Persentase penduduk usia 15 tahun ke atas yang tidak tamat SD.", "Referensi Waktu" = "2017 (SUSENAS)", Ukuran = "Persentase", Satuan = "%", "Tipe Data" = "Numerik (float)"), BUTA_HURUF = list("Nama Variabel" = "Buta Huruf", Konsep = "Persentase penduduk buta huruf.", Definisi = "Persentase penduduk yang tidak bisa membaca dan menulis kalimat sederhana.", "Referensi Waktu" = "2017 (SUSENAS)", Ukuran = "Persentase", Satuan = "%", "Tipe Data" = "Numerik (float)"))

kategorisasi_semua <- function(df) {
  for (var in var_list) {
    q <- quantile(df[[var]], probs = c(0, 0.25, 0.75, 1), na.rm = TRUE)
    df[[paste0(var, "_cat")]] <- cut(df[[var]], breaks = c(q[1] - 1e-6, q[2], q[3], q[4] + 1e-6), labels = c("Rendah", "Menengah", "Tinggi"), include.lowest = TRUE, right = TRUE)
  }
  df
}

generate_interpretasi_stat_deskriptif <- function(var_name, label_map, vec) {
  m <- mean(vec, na.rm = TRUE)
  med <- median(vec, na.rm = TRUE)
  minv <- min(vec, na.rm = TRUE)
  maxv <- max(vec, na.rm = TRUE)
  label <- label_map[[var_name]]
  interpretasi <- paste0("<p>Data ini merepresentasikan kondisi sosial di seluruh kabupaten/kota di Indonesia terkait variabel <strong>", label, "</strong>. ", "Nilai yang ditampilkan adalah persentase, sehingga angka mencerminkan proporsi rumah tangga atau penduduk yang terdampak oleh kondisi tersebut. ", "Secara nasional, rata-rata nilai variabel ini adalah <strong>", round(m, 2), "%</strong>, yang menunjukkan bahwa secara umum, ", "sekitar ", round(m, 2), "% dari populasi di setiap kabupaten/kota mengalami kondisi ini. ", "Median (nilai tengah) tercatat sebesar <strong>", round(med, 2), "%</strong>, memberikan gambaran tipikal di tingkat kabupaten/kota. ", "Nilai minimum ditemukan sebesar <strong>", round(minv, 2), "%</strong>, dan maksimum mencapai <strong>", round(maxv, 2), "%</strong>.</p>")
  skew_info <- if (m > med) { paste0("<p>Karena rata-rata lebih tinggi dari median, hal ini mengindikasikan adanya beberapa daerah dengan persentase yang sangat tinggi, ", "sehingga menarik rata-rata ke atas. Distribusi seperti ini disebut <em>condong ke kanan</em> atau <em>positif skew</em>.</p>") } else if (m < med) { paste0("<p>Jika rata-rata lebih rendah dari median, berarti ada beberapa daerah dengan persentase sangat rendah yang memengaruhi nilai rata-rata. ", "Distribusinya <em>condong ke kiri</em> atau <em>negatif skew</em>.</p>") } else { paste0("<p>Ketika rata-rata dan median hampir sama, hal ini menunjukkan bahwa data terdistribusi secara <em>simetris</em>, ", "dengan penyebaran nilai yang relatif merata di sekitar nilai tengah.</p>") }
  paste0(interpretasi, skew_info)
}

ui_beranda <- function() { 
  tagList( 
    div(
      style = "background-color: #ffffff; border: 1px solid #ddd; border-radius: 10px; padding: 20px; margin-top:20px; margin-bottom: 25px;
           box-shadow: 2px 2px 10px rgba(0,0,0,0.05);",
      
      h2("📊 KUPAS: Kemiskinan, Pendidikan, dan Akses Infrastruktur", 
         style = "color:#28a745; font-weight: bold; margin-bottom: 20px;"),
      
      p("Dashboard ", strong("KUPAS"), " (Kemiskinan, Pendidikan, dan Akses Infrastruktur) dikembangkan untuk menyajikan ",
        strong("analisis komprehensif"), " mengenai faktor-faktor yang memengaruhi tingkat ",
        strong("kemiskinan di wilayah Kabupaten/Kota di Indonesia.")),
      
      p("Fokus utama dashboard ini mencakup dua aspek penting:"),
      
      tags$ul(
        tags$li(strong("Akses Infrastruktur:"), " meliputi ketersediaan listrik, air bersih melalui pipa, dan sistem sanitasi yang layak."),
        tags$li(strong("Pendidikan:"), " meliputi pendidikan rendah dan buta huruf.")
      ),
      
      p("Melalui kombinasi ", strong("visualisasi interaktif"), " dan ", strong("analisis statistik inferensial serta regresi linear"), 
        ", dashboard ini bertujuan menjadi ", em("alat pendukung keputusan"), " bagi pembuat kebijakan, peneliti, dan pemangku kepentingan lainnya dalam memahami dinamika kemiskinan dan merumuskan solusi berbasis data.")
    ), 
    div(style = "background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 5px 20px 20px 20px; margin-bottom: 20px;", 
        h3("Sumber Data", style = "color:#28a745; margin-bottom: 20px;"), 
        p("Data utama pada dashboard ini berasal dari penelitian:"), 
        p("R. Kurniawan, B.I. Nasution, N. Agustina et al. (2022), ", em("Data in Brief 40 (107743), "), a(href = "https://doi.org/10.1016/j.ijdrr.2020.101801", target = "_blank", "Revisiting social vulnerability analysis in Indonesia: An optimized spatial fuzzy clustering approach"), "."), 
        p("Dataset yang digunakan meliputi:"), 
        tags$ul( 
          tags$li(strong("SOVI Data (511 Kabupaten/Kota): "), a(href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", "Unduh CSV")), 
          tags$li(strong("Matriks Penimbang Jarak: "), a(href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", "Unduh CSV")) 
        ) 
    ), 
    div(style = "background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 5px 20px 20px 20px;", 
        h3("Eksplorasi Metadata Variabel", style = "color:#28a745; margin-bottom: 20px;"), 
        selectInput("pilih_variabel_metadata", "Pilih variabel untuk melihat detail:", choices = setNames(names(label_map), label_map), width = "100%"), 
        hr(), 
        uiOutput("tampil_metadata_variabel"), 
        br(), 
        downloadButton("download_metadata_pdf", "Unduh Metadata (PDF)", class = "btn btn-success") 
    ) 
  ) 
}

ui_manajemen_data <- function(subpage = "kategorisasi") { 
  switch(subpage, 
         "kategorisasi" = div(style = "background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-top: 20px;", 
                              h3("Manajemen Data: Kategorisasi Variabel", style = "color:#28a745; margin-bottom: 20px;"), 
                              p("Gunakan fitur ini untuk mengubah variabel kontinyu menjadi kategori ", strong("Rendah / Menengah / Tinggi"), " berdasarkan kuartil."), 
                              selectInput("var_kategori_manajemen", "Pilih Variabel untuk Ditampilkan:", 
                                          choices = { 
                                            filtered_map <- label_map[names(label_map) != "KEMISKINAN"]
                                            c("Semua Variabel" = "Semua Variabel", setNames(names(filtered_map), filtered_map)) 
                                          }, 
                                          selected = "Semua Variabel", width = "100%"), 
                              downloadButton("download_kategorisasi", "Download Data (CSV)", class = "btn btn-success"), 
                              hr(), 
                              DTOutput("table_kategori") 
         ), 
         "tabel" = div(style = "background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-top: 20px;", 
                       h3("Manajemen Data: Tabel Data", style = "color:#28a745; margin-bottom: 20px;"), 
                       p("Menampilkan dataset SOVI yang telah digabung dengan nama wilayah dan nama kolom yang disesuaikan."), 
                       downloadButton("download_raw", "Download Data (CSV)", class = "btn btn-success"), 
                       hr(), 
                       DTOutput("table_raw") 
         ) 
  ) 
}

ui_eksplorasi_data <- function(subpage = "deskriptif") {
  switch(subpage,
         "deskriptif" = tagList(
           h3("Eksplorasi: Statistik Deskriptif"),
           p("Pilih variabel untuk melihat ringkasan statistik dan interpretasi otomatis."),
           selectInput("var_deskriptif", "Pilih Variabel:", choices = setNames(names(label_map), label_map), selected = names(label_map)[1], width = "50%"),
           h4("Ringkasan Statistik"), verbatimTextOutput("deskriptif_summary"), br(),
           h4("Interpretasi"), uiOutput("deskriptif_interpretasi_ui"),
           br(),
           downloadButton("download_deskriptif", "Unduh Ringkasan PDF", class = "btn-primary")
         
         ),
         
         "visualisasi" = tagList(
           h3("Grafik dan Plot visualisasi Data"),
           p("Pilih jenis grafik / plot untuk melakukan analisis eksploratif dari beberapa tab di bawah ini."),
           
           tabsetPanel(
             type = "tabs",
             tabPanel("Peringkat Wilayah", 
                      div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-top: 20px; margin-bottom: 20px;",
                          fluidRow(
                            column(4, selectInput("var_ranking", "Pilih Variabel:", choices = setNames(names(label_map), label_map), selected = "KEMISKINAN")),
                            column(3, radioButtons("sort_order_ranking", "Tampilkan Peringkat:", choices = c("Tertinggi" = "desc", "Terendah" = "asc"), selected = "desc")),
                            column(3, numericInput("n_ranking", "Jumlah Wilayah:", value = 10, min = 5, max = 30, step = 1)),
                            column(2, style = "text-align: right; margin-top: 25px;", downloadButton("download_ranking", "Unduh PDF", class = "btn btn-success"))
                          )
                      ),
                      plotlyOutput("ranking_plot", height="600px"),
                      hr(), 
                      h4("Interpretasi Chart dan Plot"),
                      uiOutput("ranking_interpretation")
             ),
             
             tabPanel("Distribusi (Histogram)",
                      div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-top: 20px; margin-bottom: 20px;",
                          fluidRow(
                            column(4, selectInput("var_hist", "Pilih Variabel:", choices = setNames(names(label_map), label_map))),
                            column(4, sliderInput("bins_hist", "Jumlah Bins (Resolusi):", min = 10, max = 100, value = 30)),
                            column(4, style = "text-align: right; margin-top: 25px;", downloadButton("download_hist", "Unduh PDF", class = "btn btn-success"))
                          )
                      ),
                      plotlyOutput("histogram_plot", height="600px"),
                      hr(),
                      h4("Interpretasi Statistik"),
                      uiOutput("hist_interpretation")
             ),
             
             tabPanel("Perbandingan (Box Plot)",
                      div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-top: 20px; margin-bottom: 20px;",
                          fluidRow(
                            column(4, selectInput("var_boxplot_y", "Variabel Dependen (Y):", choices = setNames(names(label_map), label_map), selected="BUTA_HURUF")),
                            column(4, selectInput("var_boxplot_x", "Variabel Grup (X):", choices = cat_var_list, selected="KEMISKINAN_cat")),
                            column(4, style = "text-align: right; margin-top: 25px;", downloadButton("download_boxplot", "Unduh PDF", class = "btn btn-success"))
                          )
                      ),
                      plotlyOutput("boxplot_plot", height="600px"),
                      hr(),
                      h4("Interpretasi Statistik"),
                      uiOutput("boxplot_interpretation")
             )
           )
         ),
         
         "spasial" = tagList(
           h3("Eksplorasi Spasial: Peta Tematik Indonesia"),
           p("Pilih variabel untuk memvisualisasikan persebarannya di seluruh wilayah Indonesia. Arahkan kursor ke sebuah wilayah untuk melihat detail."),
           
           # Kontrol Input
           div(
             style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
             fluidRow(
               column(6,
                      selectInput(
                        "var_map", 
                        "Pilih Variabel:", 
                        choices = setNames(names(label_map), label_map), 
                        selected = "KEMISKINAN", 
                        width = "100%"
                      )
               ),
               column(3, style = "margin-top: 25px;",
                      actionButton("run_lisa", "Tampilkan Peta Klaster", class = "btn-primary")
               ),
               column(3, style = "text-align: right; margin-top: 25px;",
                      downloadButton("download_map_jpg", "Unduh Peta (JPG)", class = "btn-success")
               )
             )
           ),
           
           
           leafletOutput("choropleth_map", height = "600px"),
           hr(),
           h4("Interpretasi Peta Persebaran"),
           uiOutput("map_interpretation_ui"),
           
           
           
           uiOutput("lisa_results_ui")
         )
  )
}

ui_analisis_data <- function(subpage = "asumsi") {
  switch(
    subpage,
    "asumsi" = tagList(
      h3("Analisis: Uji Asumsi Klasik"),
      p(
        "Halaman ini digunakan untuk menguji: ",
        strong("Normalitas Sebaran Data"), " dan ", strong("Homogenitas Varian"), " antar kelompok."
      ),
      div(
        style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
        fluidRow(
          column(6, selectInput("var_asumsi_numerik", "Pilih Variabel Dependen (Numerik):", choices = setNames(names(label_map), label_map))),
          column(6, selectInput("var_asumsi_grup", "Pilih Variabel Grup (Kategorik):", choices = cat_var_list))
        )
      ),
      div(style="border: 1px solid #ddd; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
          h4("1. Hasil Uji Normalitas (Shapiro-Wilk)"),
          uiOutput("normality_test_output"),
          plotlyOutput("qq_plot_output", height = "400px"),
          h5("Interpretasi Plot Normalitas"),
          uiOutput("normality_interpretation_output")
      ),
      div(style="border: 1px solid #ddd; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
          h4("2. Hasil Uji Homogenitas Varian (Bartlett)"),
          uiOutput("homogeneity_test_output"),
          plotlyOutput("box_plot_output", height = "400px"),
          h5("Interpretasi Plot Homogenitas"),
          uiOutput("homogeneity_interpretation_output")
      ),
      div(style = "text-align: center; margin-top: 20px;",
          downloadButton("download_asumsi_report", "Unduh Laporan Uji Asumsi (PDF)", class = "btn btn-success")
      )
    ),
    
    "inferensia" = tagList(
      h3("Analisis: Statistik Inferensia"),
      p("Halaman ini menyediakan berbagai metode uji hipotesis statistik. Hasil akan diperbarui secara otomatis saat Anda mengubah pilihan input."),
      
      tabsetPanel(
        id = "infer_tabs",
        type = "pills",
        
        tabPanel("Uji Beda Rata-rata (Uji-t)",
                 br(),
                 div(
                   style = "background-color: #eafaf1; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                   h5("Panduan Pengguna: Uji-t"),
                   p(strong("Tujuan:"), " Digunakan untuk menguji apakah ada perbedaan rata-rata (mean) yang signifikan secara statistik."),
                   p(strong("Cara Melakukan Uji:")),
                   tags$ol(
                     tags$li("Pilih jenis uji yang sesuai (Satu Sampel atau Dua Sampel)."),
                     tags$li("Untuk ", strong("Satu Sampel"), ", pilih variabel yang akan diuji dan masukkan sebuah nilai acuan (μ0) untuk dibandingkan."),
                     tags$li("Untuk ", strong("Dua Sampel"), ", pilih variabel numerik (Y) dan variabel grup (X) yang akan membagi data menjadi dua kelompok (misal: 'Rendah' vs 'Tinggi').")
                   )
                 ),
                 selectInput("t_test_type", "Pilih Jenis Uji-t:",
                             choices = c("Satu Sampel" = "one_sample", "Dua Sampel Independen" = "two_sample")),
                 uiOutput("t_test_inputs_ui"),
                 hr(),
                 h4("Hasil Uji-t"),
                 uiOutput("t_test_results_ui")
        ),
        
        tabPanel("Uji Proporsi",
                 br(),
                 div(
                   style = "background-color: #eafaf1; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                   h5("Panduan Pengguna: Uji Proporsi"),
                   p(strong("Tujuan:"), " Digunakan untuk menguji apakah proporsi 'sukses' dalam data Anda berbeda secara signifikan dari sebuah nilai acuan (hipotesis)."),
                   p(strong("Cara Melakukan Uji:")),
                   tags$ol(
                     tags$li("Pilih variabel numerik yang ingin dianalisis (misal: 'Kemiskinan')."),
                     tags$li("Tentukan ambang batas untuk mendefinisikan 'sukses'. Contoh: jika Anda ingin menguji proporsi wilayah dengan kemiskinan tinggi, Anda bisa mendefinisikan 'sukses' sebagai wilayah dengan persentase kemiskinan > 15."),
                     tags$li("Tentukan nilai hipotesis (antara 0 dan 1) sebagai pembanding. Contoh: untuk menguji klaim bahwa 'setengah dari seluruh wilayah tergolong miskin', masukkan nilai 0.5.")
                   )
                 ),
                 uiOutput("prop_test_inputs_ui"),
                 hr(),
                 h4("Hasil Uji Proporsi Satu Sampel"),
                 uiOutput("prop_test_results_ui")
        ),
        
        tabPanel("Uji Ragam (Varian)",
                 br(),
                 div(
                   style = "background-color: #eafaf1; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                   h5("Panduan Pengguna: Uji F untuk Dua Ragam"),
                   p(strong("Tujuan:"), " Digunakan untuk menguji apakah tingkat sebaran atau variasi (ragam) dari sebuah variabel numerik sama atau berbeda antara dua kelompok."),
                   p(strong("Cara Melakukan Uji:")),
                   tags$ol(
                     tags$li("Pilih variabel numerik yang sebarannya ingin Anda bandingkan."),
                     tags$li("Pilih variabel grup yang akan membagi data menjadi dua kelompok (misal: kategori 'Rendah' dan 'Tinggi').")
                   ),
                   p(strong("Kegunaan:"), " Uji ini penting untuk memeriksa asumsi 'homogenitas varian' sebelum melakukan Uji-t Dua Sampel.")
                 ),
                 uiOutput("var_test_inputs_ui"),
                 hr(),
                 h4("Hasil Uji F untuk Dua Ragam"),
                 uiOutput("var_test_results_ui")
        ),
        
        tabPanel("ANOVA",
                 br(),
                 div(
                   style = "background-color: #eafaf1; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                   h5("Panduan Pengguna: ANOVA (Analysis of Variance)"),
                   p(strong("Tujuan:"), " Digunakan untuk menguji apakah terdapat perbedaan rata-rata yang signifikan di antara tiga atau lebih kelompok secara bersamaan."),
                   p(strong("Cara Melakukan Uji:")),
                   p(strong("ANOVA Satu Arah:"), " Pilih satu variabel dependen (numerik) dan satu variabel grup (faktor) yang memiliki 3 kategori ('Rendah', 'Menengah', 'Tinggi')."),
                   p(strong("ANOVA Dua Arah:"), " Pilih satu variabel dependen (numerik) dan dua variabel grup (faktor) yang berbeda untuk melihat pengaruh masing-masing faktor dan interaksinya.")
                 ),
                 h4("ANOVA Satu Arah"),
                 div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
                     fluidRow(
                       column(6, selectInput("var_anova_y", "Pilih Variabel Dependen (Numerik):", choices = setNames(names(label_map), label_map))),
                       column(6, selectInput("var_anova_x", "Pilih Variabel Grup (Kategorik):", choices = cat_var_list))
                     )
                 ),
                 uiOutput("anova_results_ui"),
                 hr(),
                 h4("ANOVA Dua Arah"),
                 div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
                     fluidRow(
                       column(4, selectInput("var_anova2_y", "Variabel Dependen (Y):", choices = setNames(names(label_map), label_map))),
                       column(4, selectInput("var_anova2_x1", "Faktor 1 (X1):", choices = cat_var_list)),
                       column(4, selectInput("var_anova2_x2", "Faktor 2 (X2):", choices = cat_var_list, selected = cat_var_list[2]))
                     )
                 ),
                 uiOutput("anova2_results_ui")
        )
      ),
      hr(),
      div(style = "text-align: center; margin-top: 20px;",
          downloadButton("download_infer_report_pdf", "Unduh Laporan Uji (PDF)", class = "btn btn-success")
      )
    ),
    "regresi" = tagList(
      h3("Analisis Regresi"),
      p("Halaman ini menyajikan analisis model regresi linear berganda, dilengkapi dengan pengujian asumsi dan perbaikan model, untuk mengevaluasi pengaruh akses terhadap infrastruktur dasar serta kondisi pendidikan terhadap tingkat kemiskinan di wilayah kabupaten/kota di Indonesia."),
      
      div(
        style = "border: 1px solid #007bff; border-left: 5px solid #007bff; background-color: #f8f9fa; padding: 20px; margin-bottom: 20px; border-radius: 6px;",
        h4("Langkah 1: Analisis Model Awal"),
        p("Analisis diawali dengan membangun model regresi linear berganda menggunakan seluruh variabel prediktor yang dihipotesiskan memiliki pengaruh terhadap tingkat kemiskinan."),
        p(HTML("<code>Model Awal: KEMISKINAN ~ RT Tanpa Listrik + RT Tanpa Drainase + RT Guna Air Pipa + Pendidikan Rendah + Buta Huruf</code>")),
        uiOutput("model1_results_ui")
      ),

      div(
        style = "border: 1px solid #ffc107; border-left: 5px solid #ffc107; background-color: #fffcf2; padding: 20px; margin-bottom: 20px; border-radius: 6px;",
        h4("Langkah 2: Model Perbaikan Parsial"),
        p(HTML("Berdasarkan hasil analisis Model Awal, variabel <strong>Pendidikan Rendah (PENDIDIKAN_RENDAH)</strong> menunjukkan nilai p-value yang melebihi batas signifikansi (p > 0.05), sehingga tidak terbukti memiliki pengaruh yang signifikan terhadap tingkat kemiskinan. Jika dibandingkan dengan variabel lain dalam model yang terbukti signifikan, kontribusi variabel ini relatif rendah. Oleh karena itu, untuk memperoleh model yang lebih baik dan interpretatif, variabel tersebut dikeluarkan dari model pada tahap perbaikan.")),
        p(HTML("<code>Model Perbaikan: KEMISKINAN ~ RT Tanpa Listrik + RT Tanpa Drainase + RT Guna Air Pipa + Buta Huruf</code>")),
        uiOutput("model2_results_ui")
      ),

      div(
        style = "border: 1px solid #dc3545; border-left: 5px solid #dc3545; background-color: #fdf5f6; padding: 20px; margin-bottom: 20px; border-radius: 6px;",
        h4("Langkah 3: Uji Asumsi Klasik pada Model Perbaikan"),
        p("Pengujian asumsi klasik dilakukan terhadap Model Perbaikan guna mengevaluasi kesesuaian model dengan prasyarat analisis regresi linear."),
        uiOutput("model2_assumptions_ui"),
        p(HTML("<b>Kesimpulan:</b> Hasil Uji Normalitas menunjukkan p-value < 0.05, yang berarti <strong>nilai residual model tidak berdistribusi normal</strong>. Pelanggaran asumsi ini menandakan bahwa kita perlu melakukan perbaikan lebih lanjut pada model, salah satunya dengan transformasi data."))
      ),
      
      div(
        style = "border: 1px solid #28a745; border-left: 5px solid #28a745; background-color: #f4faf5; padding: 20px; margin-bottom: 20px; border-radius: 6px;",
        h4("Langkah 4: Model Final dengan Transformasi Log (Log-Linear)"),
        p("Transformasi logaritma natural diterapkan pada variabel dependen <strong>KEMISKINAN</strong> untuk mengatasi pelanggaran asumsi klasik, khususnya ketidakterpenuhan normalitas residual dan indikasi heteroskedastisitas. Digunakannya model log-lin (log-linear) menghasilkan distribusi residual menjadi lebih mendekati normal dan variansi model menjadi lebih stabil. Selain itu, model log-lin memungkinkan interpretasi koefisien sebagai perubahan persentase pada variabel dependen akibat perubahan satu unit pada variabel prediktor, yang relevan dalam analisis sosial-ekonomi."),
        p(HTML("<code>Model Final: log(KEMISKINAN) ~ RT Tanpa Listrik + RT Tanpa Drainase + RT Guna Air Pipa + Buta Huruf</code>")),
        
        h5("4.1 Hasil Model Final"),
        uiOutput("model3_results_ui"),
        
        h5("4.2 Uji Asumsi pada Model Final"),
        uiOutput("model3_assumptions_ui")
      ),
      
      div(
        style = "border: 2px solid #17a2b8; background-color: #f2fbfc; padding: 20px; margin-bottom: 20px; border-radius: 8px;",
        uiOutput("final_conclusion_ui")
      ),
      
      div(
        style = "text-align: center; margin-top: 30px; margin-bottom: 20px;",
        downloadButton("download_regression_report", 
                       "Unduh Laporan Analisis Regresi", 
                       class = "btn-success btn-lg")
      )
    )
    )
}


ui <- fluidPage( 
  useShinyjs(), 
  tags$head( 
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"), 
    tags$style(HTML(" * 
                    { margin: 0; 
                    padding: 0; 
                    box-sizing: border-box; } 
                    html, body { 
                    height: 100vh; 
                    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; 
                    background-color: #f8f9fa; } 
                    .container-fluid { 
                    padding: 0 !important; 
                    margin: 0 !important; 
                    width: 100% !important; 
                    max-width: none !important; 
                    height: 100vh !important; 
                    display: flex !important; 
                    flex-direction: column !important; } 
                    
                    #app-header { 
                    background: linear-gradient(135deg, #f8c291, #82ccdd); 
                    color: #2c3e50; 
                    padding: 15px 25px; 
                    display: flex; 
                    justify-content: space-between; 
                    align-items: center; width: 100%; position: fixed; top: 0; left: 0; z-index: 1050; box-shadow: 0 2px 5px rgba(0,0,0,0.2); } #content-wrapper { display: block; overflow-y: visible; } #main-content { width: 95%; max-width: 1400px; padding-top: 80px; margin: 30px auto; padding: 30px; background-color: #f8f9fa; border-radius: 8px; flex: 1; min-height: 0; } #app-footer { background: rgba(230, 230, 230, 0.8); text-align: center; padding: 20px; color: #333; font-size: 14px; width: 100%; position: static; margin-top: 20px; } #app-header .title { font-size: 24px; font-weight: 600; } .main-nav ul { list-style: none; display: flex; gap: 5px; margin: 0; padding: 0; } .main-nav .nav-link, .main-nav .dropdown > a { display: block; color: white; text-decoration: none; padding: 10px 15px; border-radius: 5px; transition: background-color 0.3s ease; font-weight: 500; background: none; border: none; font-family: inherit; font-size: 1em; cursor: pointer; } .main-nav .nav-link:hover, .main-nav .dropdown > a:hover { background-color: rgba(255, 255, 255, 0.2); } .dropdown { position: relative; } .dropdown-content { display: none; position: absolute; background-color: white; min-width: 200px; box-shadow: 0px 8px 16px rgba(0,0,0,0.2); z-index: 1001; border-radius: 5px; padding: 5px 0; top: 100%; left: 0; } .dropdown-content .nav-link { color: #333; padding: 10px 15px; display: block; text-align: left; } .dropdown-content .nav-link:hover { background-color: #f1f1f1; color: #d35400; } .dropdown:hover .dropdown-content { display: block; } #menu-toggle { display: none; background: none; border: none; color: white; font-size: 28px; cursor: pointer; } @media (max-width: 992px) { #menu-toggle { display: block; } .main-nav .nav-links { display: none; flex-direction: column; position: absolute; top: 100%; left: 0; width: 100%; background-color: #34495e; z-index: 1002; } .main-nav .nav-links.active { display: flex; } .dropdown-content { position: static; box-shadow: none; background-color: #465a6f; min-width: auto; } .dropdown-content .nav-link { color: #ecf0f1; padding-left: 30px; } .dropdown.open .dropdown-content { display: block; } } h3, h4, h5 { color: #d35400; border-bottom: 1px solid #eee; padding-bottom: 10px; margin-top: 25px; } p { line-height: 1.6; margin-bottom: 15px; } hr { border: 0; height: 1px; background: linear-gradient(to right, transparent, #d35400, transparent); margin: 25px 0; } ")) 
  ), 
  div(id = "app-header", 
      div(class = "title", "Dashboard KUPAS"), 
      tags$nav(class = "main-nav", 
               tags$button(id = "menu-toggle", "☰"), 
               tags$ul(class = "nav-links", 
                       tags$li(actionLink("nav_beranda", "Beranda", class="nav-link")), 
                       tags$li(class = "dropdown", 
                               a(href = "#", "Manajemen Data", onclick = "return false;"), 
                               tags$div(class = "dropdown-content", 
                                        actionLink("nav_manajemen_kategorisasi", "Kategorisasi Data", class = "nav-link"), 
                                        actionLink("nav_manajemen_tabel", "Tabel Data", class = "nav-link") 
                               ) 
                       ), 
                       tags$li(class="dropdown", 
                               a(href = "#", "Eksplorasi Data", onclick="return false;"), 
                               tags$div(class="dropdown-content", 
                                        actionLink("nav_eksplorasi_deskriptif", "Statistik Deskriptif", class="nav-link"), 
                                        actionLink("nav_eksplorasi_visualisasi", "Visualisasi Data", class="nav-link"), 
                                        actionLink("nav_eksplorasi_spasial", "Pemetaan", class="nav-link") 
                               ) 
                       ), 
                       tags$li(class="dropdown", 
                               a(href = "#", "Analisis Data", onclick="return false;"), 
                               tags$div(class="dropdown-content", 
                                        actionLink("nav_analisis_asumsi", "Uji Asumsi", class="nav-link"), 
                                        actionLink("nav_analisis_inferensia", "Statistik Inferensia", class="nav-link"), 
                                        actionLink("nav_analisis_regresi", "Analisis Regresi", class="nav-link") 
                               ) 
                       ) 
               ) 
      ) 
  ), 
  div(id = "content-wrapper", div(id = "main-content", uiOutput("page_content")) ), 
  div(id = "app-footer", HTML("Qurany Nadhira Tsabita - 30 - 2KS3 - 222313323@stis.ac.id<br> <small>dibuat untuk memenuhi UAS Komputasi Statistik</small>") ), 
  tags$script(HTML(" document.getElementById('menu-toggle').addEventListener('click', function() { document.querySelector('.main-nav .nav-links').classList.toggle('active'); }); document.querySelectorAll('.nav-link').forEach(function(link) { link.addEventListener('click', function() { if (window.innerWidth <= 992) { document.querySelector('.main-nav .nav-links').classList.remove('active'); document.querySelectorAll('.dropdown.open').forEach(d => d.classList.remove('open')); } }); }); ")) 
)


server <- function(input, output, session) {
  
  
  data_kategorisasi <- reactive({
    kategorisasi_semua(data)
  })
  
  currentPage <- reactiveVal(list(page = "beranda", subpage = NULL))
  observeEvent(input$nav_beranda, { currentPage(list(page = "beranda")) })
  observeEvent(input$nav_manajemen_kategorisasi, { currentPage(list(page = "manajemen", subpage = "kategorisasi"))})
  observeEvent(input$nav_manajemen_tabel, { currentPage(list(page = "manajemen", subpage = "tabel")) })
  observeEvent(input$nav_eksplorasi_deskriptif, { currentPage(list(page = "eksplorasi", subpage = "deskriptif")) })
  observeEvent(input$nav_eksplorasi_visualisasi, { currentPage(list(page = "eksplorasi", subpage = "visualisasi")) })
  observeEvent(input$nav_eksplorasi_spasial, { currentPage(list(page = "eksplorasi", subpage = "spasial")) })
  observeEvent(input$nav_analisis_asumsi, { currentPage(list(page = "analisis", subpage = "asumsi")) })
  observeEvent(input$nav_analisis_inferensia, { currentPage(list(page = "analisis", subpage = "inferensia")) })
  observeEvent(input$nav_analisis_regresi, { currentPage(list(page = "analisis", subpage = "regresi")) })
  output$page_content <- renderUI({ req(currentPage()); current <- currentPage(); switch(current$page, "beranda" = ui_beranda(), "manajemen" = ui_manajemen_data(current$subpage), "eksplorasi" = ui_eksplorasi_data(current$subpage), "analisis" = ui_analisis_data(current$subpage)) })
  
  
  output$tampil_metadata_variabel <- renderUI({ req(input$pilih_variabel_metadata); metadata <- metadata_kamus[[input$pilih_variabel_metadata]]; div(style = "background-color: #f8f9fa; padding: 20px;", h4(metadata$`Nama Variabel`, style = "color: #a75f28ff; margin-bottom: 20px; font-weight: bold; margin-top: 10px"), tags$table(style = "width: 100%; border-collapse: collapse;", tags$tbody( lapply(names(metadata), function(key) { tags$tr( tags$td(style = "width: 150px; font-weight: bold; padding: 4px;", key), tags$td(style = "padding: 4px;", metadata[[key]]) ) }) ) ) ) })
  output$download_metadata_pdf <- downloadHandler( filename = function() { paste0("Metadata_SOVI_", Sys.Date(), ".pdf") }, content = function(file) { tempReport <- tempfile(fileext = ".Rmd"); metadata_content <- "---
title: 'Metadata Variabel Dashboard'
output: pdf_document
---

## Daftar Metadata Variabel
"; 
  for (var in names(metadata_kamus)) { metadata <- metadata_kamus[[var]]; metadata_content <- paste0(metadata_content, "### ", metadata$`Nama Variabel`, "\n", "- Konsep: ", metadata$Konsep, "\n", "- Definisi: ", metadata$Definisi, "\n", "- Referensi Waktu: ", metadata$`Referensi Waktu`, "\n", "- Ukuran: ", metadata$Ukuran, "\n", "- Satuan: ", metadata$Satuan, "\n", "- Tipe Data: ", metadata$`Tipe Data`, "\n\n") }; writeLines(metadata_content, tempReport); rmarkdown::render(tempReport, output_file = file, quiet = TRUE) } )
  
  output$table_kategori <- DT::renderDataTable({ df <- data_kategorisasi(); if (input$var_kategori_manajemen == "Semua Variabel") { interleaved_vars <- as.vector(rbind(var_list, paste0(var_list, "_cat"))); all_vars <- c("KODE_KAB", "NAMA_WILAYAH", interleaved_vars); df_display <- df[, all_vars]; label_colnames <- unlist(lapply(var_list, function(var) c(label_map[[var]], paste("Kategori", label_map[[var]])))); colnames(df_display) <- c("Kode", "Nama Wilayah", label_colnames) } else { var <- input$var_kategori_manajemen; df_display <- df[, c("KODE_KAB", "NAMA_WILAYAH", var, paste0(var, "_cat"))]; label_var <- label_map[[var]]; colnames(df_display) <- c("Kode", "Nama Wilayah", label_var, paste("Kategori", label_var)) }; DT::datatable(df_display, options = list(pageLength = 10, scrollX = TRUE)) })
  output$download_kategorisasi <- downloadHandler( filename = function() { paste0("data_kategorisasi_", Sys.Date(), ".csv") }, content = function(file) { write.csv(data_kategorisasi(), file, row.names = FALSE, na = "") } )
  output$table_raw <- DT::renderDataTable({
    df_display <- data %>%
      dplyr::select(
        KODE_KAB, NAMA_WILAYAH, KEMISKINAN,
        RUMAH_TANGGA_TANPA_LISTRIK, RUMAH_TANGGA_TANPA_DRAINASE,
        RUMAH_TANGGA_AIR_PIPA, PENDIDIKAN_RENDAH, BUTA_HURUF
      )
    
    colnames(df_display) <- c(
      "Kode Wilayah", "Nama Wilayah", "Kemiskinan (%)",
      "Rumah Tangga Tanpa Listrik (%)", "Rumah Tangga Tanpa Drainase (%)",
      "Rumah Tangga Guna Air Pipa (%)", "Pendidikan Rendah (%)",
      "Buta Huruf (%)"
    )
    
    DT::datatable(df_display, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_raw <- downloadHandler( filename = function() { "data_lengkap_sovi.csv" }, content = function(file) { write.csv(data, file, row.names = FALSE, na = "") } )
  
  output$deskriptif_summary <- renderPrint({ req(input$var_deskriptif); var <- input$var_deskriptif; summary_stats <- summary(data[[var]]); cat("Ringkasan statistik untuk '", label_map[var], "':\n\n", sep=""); print(summary_stats) })
  output$deskriptif_interpretasi_ui <- renderUI({ req(input$var_deskriptif); HTML(generate_interpretasi_stat_deskriptif(input$var_deskriptif, label_map, data[[input$var_deskriptif]])) })
  
  ranking_plot_obj <- reactive({
    req(input$var_ranking, input$sort_order_ranking, input$n_ranking)
    

    n_label <- input$n_ranking
    var_label <- label_map[input$var_ranking]
    order_label <- ifelse(input$sort_order_ranking == "desc", "Tertinggi", "Terendah")
    plot_title <- paste(n_label, "Wilayah dengan Persentase", var_label, order_label)
    
    plot_data <- data %>%
      dplyr::select(all_of(c("NAMA_WILAYAH", input$var_ranking))) %>%
      filter(!is.na(.data[[input$var_ranking]]))
    
    if (input$sort_order_ranking == "desc") {
      plot_data <- plot_data %>% arrange(desc(.data[[input$var_ranking]]))
    } else {
      plot_data <- plot_data %>% arrange(.data[[input$var_ranking]])
    }
    
    plot_data <- plot_data %>% slice_head(n = input$n_ranking)
    
    # Plot
    ggplot(plot_data, aes(x = reorder(NAMA_WILAYAH, .data[[input$var_ranking]]), 
                          y = .data[[input$var_ranking]])) +
      geom_col(aes(fill = .data[[input$var_ranking]]), show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = "#fdeac7", high = "#d35400") +
      labs(title = plot_title, x = "", y = "Persentase (%)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # --- OUTPUT PLOTLY ---
  output$ranking_plot <- renderPlotly({
    ggplotly(ranking_plot_obj())
  })
  
  output$download_ranking <- downloadHandler(
    filename = function() {
      paste0("Peringkat_", input$var_ranking, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = ranking_plot_obj(), device = "pdf", width = 10, height = 8, units = "in")
    }
  )
  
  histogram_plot_obj <- reactive({ req(input$var_hist, input$bins_hist); var_label <- label_map[input$var_hist]; plot_title <- paste("Distribusi Frekuensi:", var_label, "di Seluruh Wilayah"); ggplot(data, aes(x = .data[[input$var_hist]])) + geom_histogram(bins = input$bins_hist, fill = "#d35400", color = "white", alpha = 0.8) + labs(title = plot_title, x = "Persentase (%)", y = "Frekuensi (Jumlah Wilayah)") + theme_minimal(base_size = 12) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) })
  output$histogram_plot <- renderPlotly({ ggplotly(histogram_plot_obj()) })
  output$download_hist <- downloadHandler( filename = function() { paste0("Histogram_", input$var_hist, "_", Sys.Date(), ".pdf") }, content = function(file) { ggsave(file, plot = histogram_plot_obj(), device = "pdf", width = 10, height = 8, units = "in") } )
  boxplot_plot_obj <- reactive({ req(input$var_boxplot_y, input$var_boxplot_x); y_label <- label_map[input$var_boxplot_y]; x_label <- names(cat_var_list[cat_var_list == input$var_boxplot_x]); plot_title <- paste("Distribusi", y_label, "berdasarkan", x_label); ggplot(data_kategorisasi(), aes(x = .data[[input$var_boxplot_x]], y = .data[[input$var_boxplot_y]])) + geom_boxplot(aes(fill = .data[[input$var_boxplot_x]]), show.legend = FALSE, color = "darkred", alpha = 0.7) + labs(title = plot_title, x = x_label, y = paste("Persentase", y_label, "(%)")) + theme_minimal(base_size = 12) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) })
  output$boxplot_plot <- renderPlotly({ ggplotly(boxplot_plot_obj()) })
  output$download_boxplot <- downloadHandler( filename = function() { paste0("Boxplot_", input$var_boxplot_y, "_vs_", input$var_boxplot_x, "_", Sys.Date(), ".pdf") }, content = function(file) { ggsave(file, plot = boxplot_plot_obj(), device = "pdf", width = 10, height = 8, units = "in") } )
  output$ranking_interpretation <- renderUI({ req(input$var_ranking, input$n_ranking, input$sort_order_ranking); var_label <- label_map[input$var_ranking]; n <- input$n_ranking; order_text <- ifelse(input$sort_order_ranking == "desc", "tertinggi", "terendah"); HTML(paste0("<p>Halaman ini menyajikan diagram batang yang telah diurutkan (<em>ranked bar chart</em>) untuk menunjukkan <strong>", n, " wilayah</strong> dengan persentase <strong>", var_label, " ", order_text, "</strong>. ", "Visualisasi ini bertujuan membantu pembaca mengenali wilayah-wilayah dengan nilai yang sangat tinggi atau sangat rendah dibandingkan wilayah lain, yang disebut sebagai pencilan (<em>outlier</em>).</p>")) })
  output$hist_interpretation <- renderUI({ req(input$var_hist); var_label <- label_map[input$var_hist]; vec <- data[[input$var_hist]]; mean_val <- round(mean(vec, na.rm = TRUE), 2); median_val <- round(median(vec, na.rm = TRUE), 2); skew_desc <- if (mean_val > median_val * 1.05) { paste0("Distribusi ini menunjukkan kecondongan ke kanan (<em>positive skew</em>), di mana nilai rata-rata (", mean_val, "%) lebih besar dari median (", median_val, "%). ", "Hal ini mengindikasikan bahwa sebagian besar wilayah memiliki nilai relatif rendah hingga menengah untuk ", var_label, ", namun terdapat beberapa wilayah dengan nilai ekstrem tinggi yang secara signifikan 'menarik' nilai rata-rata ke atas.") } else if (median_val > mean_val * 1.05) { paste0("Distribusi ini menunjukkan kecondongan ke kiri (<em>negative skew</em>), di mana nilai median (", median_val, "%) lebih besar dari rata-rata (", mean_val, "%). ", "Ini menyiratkan bahwa mayoritas wilayah terkonsentrasi pada nilai-nilai tinggi, dengan beberapa kasus ekstrem rendah.") } else { paste0("Distribusi ini tampak relatif simetris, di mana nilai rata-rata (", mean_val, "%) dan median (", median_val, "%) hampir sama. ", "Ini menunjukkan bahwa sebaran data untuk ", var_label, " cukup merata di sekitar pusatnya tanpa kecondongan yang signifikan.") }; HTML(paste0("<p>Histogram ini menggambarkan distribusi frekuensi dari variabel <strong>", var_label, "</strong> di 511 wilayah. Sumbu-X merepresentasikan rentang nilai (dikelompokkan ke dalam ", input$bins_hist, " <em>bins</em>), sedangkan sumbu-Y menunjukkan jumlah wilayah (frekuensi) yang nilainya jatuh dalam setiap rentang tersebut.</p>", "<p>", skew_desc, "</p>")) })
  output$boxplot_interpretation <- renderUI({ req(input$var_boxplot_y, input$var_boxplot_x); y_label <- label_map[input$var_boxplot_y]; x_label <- names(cat_var_list[cat_var_list == input$var_boxplot_x]); HTML(paste0("<p>Diagram box plot (diagram kotak-garis) ini digunakan untuk membandingkan sebaran nilai dari variabel numerik (<strong>'", y_label, "'</strong>) di setiap kategori dalam variabel grup (<strong>'", x_label, "'</strong>). ", "Visualisasi ini membantu untuk memahami perbedaan nilai tengah, variasi data, serta kemungkinan adanya data pencilan antar kelompok.</p>", "<ul>", "<li><strong>Kotak (Box)</strong>: Menunjukkan rentang antara kuartil pertama (Q1, persentil ke-25) dan kuartil ketiga (Q3, persentil ke-75), disebut juga sebagai <em>Interquartile Range</em> (IQR). Bagian ini mencakup 50% data di tengah.</li>", "<li><strong>Garis Tengah</strong>: Menandakan nilai <strong>median</strong> (Q2), yaitu nilai tengah dari data. Posisi garis ini antar kotak bisa dibandingkan untuk melihat perbedaan nilai tengah antar kelompok.</li>", "<li><strong>Kumis (Whiskers)</strong>: Garis yang memanjang dari kotak hingga batas tertentu (umumnya 1,5 kali IQR). Titik data yang berada di luar kisaran ini dianggap sebagai <strong>pencilan (outlier)</strong> dan biasanya ditandai sebagai titik terpisah.</li>", "</ul>", "<p>Dengan melihat posisi median dan panjang kotak, kita bisa menilai apakah ada perbedaan atau pola tertentu antar kelompok yang dianalisis.</p>")) })
  
  
  output$choropleth_map <- renderLeaflet({
    leaflet(data_spasial) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2, zoom = 5)
  })
  
  observe({
    req(input$var_map)
    selected_var <- input$var_map
    values <- data_spasial[[selected_var]]
    pal <- colorNumeric(palette = "YlOrRd", domain = values, na.color = "#d9d9d9")
    popup_text <- paste0("<strong>", data_spasial$NAMA_WILAYAH, "</strong><br/>", label_map[selected_var], ": ", round(values, 2), "%") %>% lapply(htmltools::HTML)
    
    leafletProxy("choropleth_map", data = data_spasial) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(values), weight = 1, opacity = 1, color = "white",
        dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        label = popup_text,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = values, opacity = 0.7, title = as.character(label_map[selected_var]), position = "bottomright")
  })
  
  output$map_interpretation_ui <- renderUI({
    req(input$var_map)
    var_label <- label_map[input$var_map]
    HTML(paste0(
      "<p>Peta tematik (<em>choropleth map</em>) ini menunjukkan distribusi geografis dari variabel <strong>", var_label, "</strong>. ",
      "Wilayah diwarnai berdasarkan nilai persentasenya: warna <strong>kuning muda</strong> untuk nilai rendah dan semakin pekat menuju <strong>merah tua</strong> untuk nilai yang lebih tinggi. ",
      "Peta ini berguna untuk mengidentifikasi pola spasial secara umum.</p>"
    ))
  })
  
  
  
  output$lisa_results_ui <- renderUI({
    req(input$run_lisa) 
    tagList(
      hr(),
      h3("Analisis Klaster Spasial (Local Moran's I)"),
      
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("download_lisa_jpg", "Unduh Peta Klaster (JPG)", class = "btn-success")
      ),
      
      leafletOutput("lisa_map_output", height = "600px"),
      hr(),
      h4("Interpretasi Peta Klaster"),
      uiOutput("lisa_interpretation_ui")
    )
  })
  
  
  lisa_data <- eventReactive(input$run_lisa, {
    showNotification("Menjalankan analisis Local Moran's I...", type = "message", duration = 3)
    
    
    dist_mat <- as.matrix(distance_matrix[, -1])
    dist_mat_inv <- 1/dist_mat
    diag(dist_mat_inv) <- 0
    spatial_weights <- mat2listw(dist_mat_inv, style = "W")
    
    # Kalkulasi LISA
    selected_var <- input$var_map
    local_moran <- localmoran(data_spasial[[selected_var]], spatial_weights)
    scaled_var <- scale(data_spasial[[selected_var]])
    
    data_spasial$lisa_cluster <- "Tidak Signifikan"
    data_spasial$lisa_cluster[local_moran[, 5] < 0.05 & scaled_var > 0 & local_moran[, 4] > 0] <- "High-High"
    data_spasial$lisa_cluster[local_moran[, 5] < 0.05 & scaled_var < 0 & local_moran[, 4] > 0] <- "Low-Low"
    data_spasial$lisa_cluster[local_moran[, 5] < 0.05 & scaled_var > 0 & local_moran[, 4] < 0] <- "High-Low"
    data_spasial$lisa_cluster[local_moran[, 5] < 0.05 & scaled_var < 0 & local_moran[, 4] < 0] <- "Low-High"
    
    return(data_spasial)
  })
  
  output$lisa_map_output <- renderLeaflet({
    map_data <- lisa_data()
    
    lisa_colors <- c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6", "High-Low" = "#fdae61", "Low-High" = "#abd9e9", "Tidak Signifikan" = "#d9d9d9")
    pal_lisa <- colorFactor(palette = lisa_colors, domain = map_data$lisa_cluster)
    
    popup_text_lisa <- paste0("<strong>", map_data$NAMA_WILAYAH, "</strong><br/>", "Tipe Klaster: ", map_data$lisa_cluster, "<br/>", label_map[input$var_map], ": ", round(map_data[[input$var_map]], 2), "%") %>% lapply(htmltools::HTML)
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal_lisa(lisa_cluster), weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        label = popup_text_lisa,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = pal_lisa, values = ~lisa_cluster, opacity = 0.8, title = "Tipe Klaster Spasial", position = "bottomright")
  })
  

  output$lisa_interpretation_ui <- renderUI({
    req(lisa_data()) 
    var_label <- label_map[input$var_map]
    
    HTML(paste0(
      "<p>Peta Klaster LISA (<em>Local Indicators of Spatial Association</em>) ini mengidentifikasi wilayah dengan pola pengelompokan statistik yang signifikan untuk variabel <strong>", var_label, "</strong>.</p>",
      "<ul>",
      "<li><span style='color:#d7191c;'><strong>High-High (Hot Spot):</strong></span> Wilayah bernilai tinggi yang dikelilingi oleh wilayah lain yang juga bernilai tinggi.</li>",
      "<li><span style='color:#2c7bb6;'><strong>Low-Low (Cold Spot):</strong></span> Wilayah bernilai rendah yang dikelilingi oleh wilayah lain yang juga bernilai rendah.</li>",
      "<li><span style='color:#fdae61;'><strong>High-Low:</strong></span> Wilayah bernilai tinggi yang dikelilingi oleh wilayah bernilai rendah (anomali).</li>",
      "<li><span style='color:#abd9e9;'><strong>Low-High:</strong></span> Wilayah bernilai rendah yang dikelilingi oleh wilayah bernilai tinggi (anomali).</li>",
      "<li><span style='color:#808080;'><strong>Tidak Signifikan:</strong></span> Wilayah tanpa pola pengelompokan yang signifikan secara statistik.</li>",
      "</ul>"
    ))
  })
  
  static_lisa_map_to_save <- reactive({
    map_data <- lisa_data()
    
    ggplot(map_data) +
      geom_sf(aes(fill = lisa_cluster), color = "white", size = 0.2) +
      scale_fill_manual(
        values = c(
          "High-High" = "#d7191c",
          "Low-Low" = "#2c7bb6",
          "High-Low" = "#fdae61",
          "Low-High" = "#abd9e9",
          "Tidak Signifikan" = "#d9d9d9"
        ),
        name = "Tipe Klaster"
      ) +
      labs(title = paste0("Peta Klaster LISA - ", label_map[input$var_map])) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$download_lisa_jpg <- downloadHandler(
    filename = function() {
      paste0("Peta_LISA_", input$var_map, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      ggsave(file, plot = static_lisa_map_to_save(), device = "jpeg", 
             width = 8, height = 6, units = "in", dpi = 300)
    }
  )
  
  static_map_to_save <- reactive({
    req(input$var_map) 
    
    selected_var <- input$var_map
    var_label <- label_map[selected_var]
    
    ggplot(data = data_spasial) +
      geom_sf(aes(fill = .data[[selected_var]]), color = "white", size = 0.1) +
      scale_fill_gradient(
        low = "#fdeac7", 
        high = "#d35400", 
        name = paste(var_label, "(%)"),
        na.value = "#d9d9d9"
      ) +
      labs(
        title = paste("Peta Persebaran:", var_label),
        caption = "Sumber Data: SOVI Data"
      ) +
      theme_void() + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm")
      )
  })
  
  output$download_map_jpg <- downloadHandler(
    filename = function() {
      paste0("Peta_", input$var_map, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      ggsave(file, plot = static_map_to_save(), device = "jpeg", 
             width = 8, height = 6, units = "in", dpi = 300)
    }
  )
  
  assumption_analysis <- reactive({
    req(input$var_asumsi_numerik, input$var_asumsi_grup)
    
    var_num_name <- input$var_asumsi_numerik
    var_grup_name <- input$var_asumsi_grup
    
    df_kategori <- data_kategorisasi()
    
    shapiro_test <- shapiro.test(df_kategori[[var_num_name]])
    
    bartlett_test <- bartlett.test(df_kategori[[var_num_name]] ~ df_kategori[[var_grup_name]])
    
    qq_plot <- ggplot(data.frame(val = df_kategori[[var_num_name]]), aes(sample = val)) +
      stat_qq(color = "#2c3e50") + stat_qq_line(color = "#d35400", linewidth = 1) +
      labs(title = paste("Q-Q Plot untuk", label_map[var_num_name]), x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    
    box_plot <- ggplot(df_kategori, aes(x = .data[[var_grup_name]], y = .data[[var_num_name]])) +
      geom_boxplot(aes(fill = .data[[var_grup_name]]), show.legend = FALSE, alpha = 0.7) +
      labs(title = paste("Box Plot", label_map[var_num_name], "berdasarkan", names(cat_var_list[cat_var_list == var_grup_name])), 
           x = names(cat_var_list[cat_var_list == var_grup_name]), y = label_map[var_num_name]) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    
    list(
      shapiro = shapiro_test, bartlett = bartlett_test,
      qq_plot = qq_plot, box_plot = box_plot,
      var_num_label = label_map[var_num_name],
      var_grup_label = names(cat_var_list[cat_var_list == var_grup_name])
    )
  })
  
  output$normality_test_output <- renderUI({
    res <- assumption_analysis()
    test <- res$shapiro
    alpha <- 0.05
    keputusan <- ifelse(test$p.value < alpha, "Tolak H0", "Gagal Tolak H0")
    
    tagList(
      div(
        style = "margin-bottom: 10px;",
        HTML("<strong>Hipotesis:</strong><br>
           H0: Data berdistribusi normal<br>
           H1: Data tidak berdistribusi normal")
      ),
      div(
        style = "margin-bottom: 10px;",
        HTML(
          paste0(
            "<strong>Hasil Uji:</strong><br>",
            "Statistik uji Shapiro-Wilk (W) = ", round(test$statistic, 4), "<br>",
            "p-value = ", format.pval(test$p.value, digits = 4), "<br><br>",
            "Dengan tingkat signifikansi alfa = 0.05, maka keputusan yang diambil adalah: ",
            "<strong>", keputusan, "</strong>."
          )
        )
      )
    )
  })
  
  output$qq_plot_output <- renderPlotly({
    req(assumption_analysis()$qq_plot)
    ggplotly(assumption_analysis()$qq_plot)
  })
  
  output$normality_interpretation_output <- renderUI({
    res <- assumption_analysis()
    test <- res$shapiro
    
    kesimpulan <- ifelse(
      test$p.value < 0.05,
      "titik-titik data terlihat menyebar menjauhi garis diagonal. Pola ini mengindikasikan adanya penyimpangan dari distribusi normal, sehingga asumsi normalitas tidak terpenuhi. Artinya, jika kamu berencana melakukan analisis statistik yang mensyaratkan distribusi normal (seperti uji parametrik), hasilnya bisa kurang valid.",
      "sebagian besar titik data berbaris rapi di sepanjang garis diagonal. Pola ini menunjukkan bahwa distribusi data cukup mendekati distribusi normal, sehingga asumsi normalitas terpenuhi dan kamu bisa lanjut pakai metode statistik parametrik dengan lebih percaya diri."
    )
    
    p(
      paste(
        "Plot Q-Q (Quantile-Quantile) di atas digunakan untuk membandingkan distribusi data sampel dengan distribusi normal teoritis.",
        "Jika titik-titik mengikuti garis diagonal, maka data dianggap normal.",
        "Dalam kasus ini,", kesimpulan
      )
    )
  })
  
  
  output$homogeneity_test_output <- renderUI({
    res <- assumption_analysis()
    test <- res$bartlett
    alpha <- 0.05
    keputusan <- ifelse(test$p.value < alpha, "Tolak H0", "Gagal Tolak H0")
    
    tagList(
      HTML("<strong>Hipotesis:</strong> 
           <br> H0: Varian data antar kelompok adalah homogen (sama) 
           <br> H1: Setidaknya ada satu kelompok dengan varian yang berbeda."),
      br(), br(),
      HTML(paste0(
        "<strong>Hasil:</strong> Berdasarkan uji Bartlett, diperoleh statistik uji <em>K-squared</em> = ", round(test$statistic, 4), 
        " dengan p-value = ", format.pval(test$p.value, digits = 4), ".<br>",
        "Dengan tingkat signifikansi alfa = 0.05, maka keputusan yang diambil adalah <strong>", keputusan, "</strong>."
      ))
    )
  })
  
  
  output$box_plot_output <- renderPlotly({
    req(assumption_analysis()$box_plot)
    ggplotly(assumption_analysis()$box_plot)
  })
  
  output$homogeneity_interpretation_output <- renderUI({
    res <- assumption_analysis()
    test <- res$bartlett
    
    kesimpulan <- ifelse(
      test$p.value < 0.05,
      "box plot menunjukkan perbedaan yang mencolok dalam tinggi kotak (yang merepresentasikan rentang antar kuartil atau IQR) di antara kelompok. Ini mengindikasikan bahwa variansi data antar kelompok tidak seragam, sehingga asumsi homogenitas varian tidak terpenuhi. Hal ini dapat mempengaruhi keabsahan hasil analisis yang mengandalkan asumsi ini, seperti ANOVA.",
      "box plot memperlihatkan bahwa tinggi kotak (IQR) dari masing-masing kelompok terlihat relatif mirip, yang menunjukkan bahwa variansi antar kelompok cukup seragam. Dengan demikian, asumsi homogenitas varian terpenuhi, dan analisis statistik yang memerlukan asumsi ini dapat dilanjutkan dengan lebih yakin."
    )
    
    p(
      paste(
        "Box plot di atas menampilkan sebaran dan penyebaran data (variansi) dari masing-masing kelompok.",
        "Homogenitas varian berarti bahwa variansi data antar kelompok harus seragam.",
        "Dalam konteks ini,", kesimpulan
      )
    )
  })

  output$download_asumsi_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_Asumsi_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_asumsi.Rmd")
      
      rmd_content <- "---
title: 'Laporan Uji Asumsi Klasik'
output: pdf_document
params:
  norm_test: NA
  homog_test: NA
  qq_plot: NA
  box_plot: NA
  var_num_label: ''
  var_grup_label: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
```

## Hasil Analisis Uji Asumsi

Laporan ini merangkum hasil uji asumsi normalitas dan homogenitas varian yang dilakukan pada data.

### 1. Uji Normalitas (Shapiro-Wilk)
Variabel yang Diuji: `r params$var_num_label`

Hipotesis:
- H0: Data berdistribusi normal  
- H1: Data tidak berdistribusi normal

Statistik Uji (W): `r round(params$norm_test$statistic, 4)`  
p-value: `r format.pval(params$norm_test$p.value, digits = 4)`  
Keputusan (alfa = 0.05): `r ifelse(params$norm_test$p.value < 0.05, 'Tolak H0', 'Gagal Tolak H0')`  
Kesimpulan: `r ifelse(params$norm_test$p.value < 0.05, paste('Data untuk variabel', params$var_num_label, 'tidak terdistribusi secara normal.'), paste('Data untuk variabel', params$var_num_label, 'terdistribusi secara normal.'))`

Visualisasi (Q-Q Plot):

```{r}
params$qq_plot
```

### 2. Uji Homogenitas Varian (Bartlett)
Variabel Dependen: `r params$var_num_label`  
Variabel Grup: `r params$var_grup_label`

Hipotesis:
- H0: Varian data homogen di semua kelompok  
- H1: Setidaknya ada satu kelompok dengan varian berbeda

Statistik Uji (K-squared): `r round(params$homog_test$statistic, 4)`  
p-value: `r format.pval(params$homog_test$p.value, digits = 4)`  
Keputusan (alfa = 0.05): `r ifelse(params$homog_test$p.value < 0.05, 'Tolak H0', 'Gagal Tolak H0')`  
Kesimpulan: `r ifelse(params$homog_test$p.value < 0.05, paste('Varian dari', params$var_num_label, 'tidak homogen antar kelompok.'), paste('Varian dari', params$var_num_label, 'adalah homogen antar kelompok.'))`

Visualisasi (Box Plot):

```{r}
params$box_plot
```
"
      
      writeLines(rmd_content, tempReport)
      
      res <- assumption_analysis()
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = list(
          norm_test = res$shapiro,
          homog_test = res$bartlett,
          qq_plot = res$qq_plot,
          box_plot = res$box_plot,
          var_num_label = res$var_num_label,
          var_grup_label = res$var_grup_label
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
 
  output$t_test_inputs_ui <- renderUI({
    div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
        if (input$t_test_type == "one_sample") {
          fluidRow(
            column(6, selectInput("var_t_one", "Pilih Variabel:", choices = setNames(names(label_map), label_map))),
            column(6, numericInput("mu_t_one", "Nilai Hipotesis Rata-rata (μ0):", value = round(mean(data$KEMISKINAN), 2)))
          )
        } else { # two_sample
          fluidRow(
            column(6, selectInput("var_t_two_y", "Variabel Numerik (Y):", choices = setNames(names(label_map), label_map))),
            column(6, selectInput("var_t_two_x", "Variabel Grup (X):", choices = cat_var_list))
          )
        }
    )
  })
  
  t_test_results <- reactive({
    if (input$t_test_type == "one_sample") {
      req(input$var_t_one, input$mu_t_one)
      test <- t.test(data[[input$var_t_one]], mu = input$mu_t_one)
      list(test = test, type = "one_sample")
    } else {
      req(input$var_t_two_y, input$var_t_two_x)
      df_kategori <- data_kategorisasi()
      df_filtered <- df_kategori %>% filter(.data[[input$var_t_two_x]] %in% c("Rendah", "Tinggi"))
      
      validate(need(n_distinct(df_filtered[[input$var_t_two_x]]) == 2, "Variabel grup harus memiliki dua level ('Rendah' dan 'Tinggi') untuk perbandingan."))
      
      formula <- as.formula(paste(input$var_t_two_y, "~", input$var_t_two_x))
      test <- t.test(formula, data = df_filtered)
      list(test = test, type = "two_sample")
    }
  })
  
  output$t_test_results_ui <- renderUI({
    res <- t_test_results()
    req(res)
    test <- res$test
    alpha <- 0.05
    confidence <- (1 - alpha) * 100
    
    h0 <- ifelse(res$type == "one_sample", paste("H0: μ =", test$null.value), "H0: μ1 - μ2 = 0")
    h1 <- ifelse(res$type == "one_sample", paste("H1: μ ≠", test$null.value), "H1: μ1 - μ2 ≠ 0")
    
    keputusan <- ifelse(test$p.value < alpha, "<strong style='color:red;'>Tolak H<sub>0</sub></strong>", "<strong style='color:green;'>Gagal Tolak H<sub>0</sub></strong>")
    kesimpulan <- ifelse(test$p.value < alpha,
                         "Terdapat perbedaan rata-rata yang signifikan secara statistik.",
                         "Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan.")
    
    interpretasi <- if (test$p.value < alpha) {
      if (res$type == "one_sample") {
        paste0("Berdasarkan hasil uji t dengan tingkat signifikansi ", alpha,
               " (", confidence, "% tingkat kepercayaan), terdapat cukup bukti untuk menyatakan bahwa rata-rata populasi <strong>berbeda</strong> dari nilai hipotesis (", test$null.value, ").")
      } else {
        paste0("Berdasarkan hasil uji t dengan tingkat signifikansi ", alpha,
               " (", confidence, "% tingkat kepercayaan), terdapat cukup bukti untuk menyatakan bahwa rata-rata dari kedua kelompok <strong>berbeda</strong> secara signifikan.")
      }
    } else {
      if (res$type == "one_sample") {
        paste0("Berdasarkan hasil uji t dengan tingkat signifikansi ", alpha,
               " (", confidence, "% tingkat kepercayaan), <strong>tidak terdapat cukup bukti</strong> untuk menyatakan bahwa rata-rata populasi berbeda dari nilai hipotesis (", test$null.value, ").")
      } else {
        paste0("Berdasarkan hasil uji t dengan tingkat signifikansi ", alpha,
               " (", confidence, "% tingkat kepercayaan), <strong>tidak terdapat cukup bukti</strong> untuk menyatakan bahwa rata-rata dari kedua kelompok berbeda secara signifikan.")
      }
    }
    
    div(
      style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px;",
      p(HTML("<strong>Hipotesis:</strong>")),
      tags$ul(tags$li(h0), tags$li(h1)),
      
      p(HTML(paste0("<strong>Statistik Uji (t):</strong> ", round(test$statistic, 4)))),
      p(HTML(paste0("<strong>p-value:</strong> ", format.pval(test$p.value, digits = 4)))),
      p(HTML(paste0("<strong>Keputusan (α = ", alpha, "):</strong> ", keputusan))),
      hr(),
      p(HTML("<strong>Kesimpulan Statistik:</strong><br>", kesimpulan)),
      p(HTML("<strong>Interpretasi:</strong><br>", interpretasi))
    )
  })
  
  output$prop_test_inputs_ui <- renderUI({
    div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
        fluidRow(
          column(4, selectInput("var_prop", "Pilih Variabel:", choices = setNames(names(label_map), label_map))),
          column(4, numericInput("prop_threshold", "Definisikan 'Sukses' jika nilai >", value = round(mean(data$KEMISKINAN),0))),
          column(4, numericInput("prop_h0", "Nilai Hipotesis Proporsi (p0):", value = 0.5, min = 0, max = 1, step = 0.01))
        )
    )
  })
  
  prop_test_results <- reactive({
    req(input$var_prop, !is.na(input$prop_threshold), !is.na(input$prop_h0))
    
    success_count <- sum(data[[input$var_prop]] > input$prop_threshold, na.rm = TRUE)
    total_count <- sum(!is.na(data[[input$var_prop]]))
    
    test <- prop.test(x = success_count, n = total_count, p = input$prop_h0)
    list(test = test, success = success_count, total = total_count)
  })
  
  output$prop_test_results_ui <- renderUI({
    res <- prop_test_results()
    req(res)
    test <- res$test
    alpha <- 0.05
    
    keputusan <- ifelse(
      test$p.value < alpha,
      "<strong style='color:red;'>Tolak H<sub>0</sub></strong>",
      "<strong style='color:green;'>Gagal Tolak H<sub>0</sub></strong>"
    )
    
    kesimpulan <- ifelse(
      test$p.value < alpha,
      "Proporsi sampel berbeda secara signifikan dari nilai hipotesis.",
      "Tidak terdapat cukup bukti untuk menyatakan proporsi sampel berbeda dari nilai hipotesis."
    )
    
    confidence <- (1 - alpha) * 100
    interpretasi_awam <- if (test$p.value < alpha) {
      paste0("Berdasarkan hasil uji proporsi dengan tingkat signifikansi ", alpha,
             " (", confidence, "% tingkat kepercayaan), terdapat cukup bukti untuk menyatakan bahwa proporsi populasi <strong>berbeda</strong> dari nilai hipotesis (", test$null.value, ").")
    } else {
      paste0("Berdasarkan hasil uji proporsi dengan tingkat signifikansi ", alpha,
             " (", confidence, "% tingkat kepercayaan), <strong>tidak terdapat cukup bukti</strong> untuk menyatakan bahwa proporsi populasi berbeda dari nilai hipotesis (", test$null.value, ").")
    }
    
    
    div(
      style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px;",
      p(HTML(paste0("<strong>Data:</strong> Terdapat ", res$success, " wilayah 'sukses' dari total ", res$total, " wilayah."))),
      
      p(HTML(paste0("<strong>Hipotesis:</strong><br>",
                    "H<sub>0</sub>: p = ", test$null.value, "<br>",
                    "H<sub>1</sub>: p ≠ ", test$null.value))),
      
      p(HTML(paste0("<strong>Statistik Uji (X-squared):</strong> ", round(test$statistic, 4)))),
      p(HTML(paste0("<strong>p-value:</strong> ", format.pval(test$p.value, digits = 4)))),
      p(HTML(paste0("<strong>Keputusan (α = ", alpha, "):</strong> ", keputusan))),
      hr(),
      p(HTML(paste0("<strong>Kesimpulan:</strong><br>", kesimpulan))),
      p(HTML(paste0("<strong>Interpretasi:</strong><br>", interpretasi_awam)))
    )
  })
  
  output$var_test_inputs_ui <- renderUI({
    div(style="background-color: #ffffff; border: 1px solid #ddd; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
        fluidRow(
          column(6, selectInput("var_var_y", "Variabel Numerik (Y):", choices = setNames(names(label_map), label_map))),
          column(6, selectInput("var_var_x", "Variabel Grup (X):", choices = cat_var_list))
        )
    )
  })
  
  var_test_results <- reactive({
    req(input$var_var_y, input$var_var_x)
    df_kategori <- data_kategorisasi()
    df_filtered <- df_kategori %>% filter(.data[[input$var_var_x]] %in% c("Rendah", "Tinggi"))
    
    validate(need(n_distinct(df_filtered[[input$var_var_x]]) == 2, "Variabel grup harus memiliki dua level ('Rendah' dan 'Tinggi') untuk perbandingan."))
    
    formula <- as.formula(paste(input$var_var_y, "~", input$var_var_x))
    test <- var.test(formula, data = df_filtered)
    list(test = test)
  })
  
  output$var_test_results_ui <- renderUI({
    res <- var_test_results()
    req(res)
    test <- res$test
    alpha <- 0.05
    
    keputusan <- ifelse(
      test$p.value < alpha,
      "<strong style='color:red;'>Tolak H<sub>0</sub></strong>",
      "<strong style='color:green;'>Gagal Tolak H<sub>0</sub></strong>"
    )
    
    kesimpulan <- ifelse(
      test$p.value < alpha,
      "Terdapat perbedaan ragam (varians) yang signifikan secara statistik antar kelompok.",
      "Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan ragam (varians) antar kelompok."
    )
    
    confidence <- (1 - alpha) * 100
    interpretasi_awam <- if (test$p.value < alpha) {
      paste0("Berdasarkan hasil uji F dengan tingkat signifikansi ", alpha,
             " (", confidence, "% tingkat kepercayaan), terdapat cukup bukti untuk menyatakan bahwa varians antar kelompok <strong>berbeda secara signifikan</strong>. ",
             "Artinya, penyebaran data antar kelompok tidak seragam — terdapat kelompok yang lebih tersebar atau lebih konsisten dari yang lain. ",
             "Hal ini penting diketahui sebelum melakukan analisis lanjutan seperti uji t atau ANOVA karena asumsi kesamaan varians mungkin tidak terpenuhi.")
    } else {
      paste0("Berdasarkan hasil uji F dengan tingkat signifikansi ", alpha,
             " (", confidence, "% tingkat kepercayaan), <strong>tidak terdapat cukup bukti</strong> untuk menyatakan bahwa varians antar kelompok berbeda secara signifikan. ",
             "Artinya, penyebaran data antar kelompok dapat dianggap kurang lebih sama. Asumsi kesamaan varians masih wajar digunakan untuk analisis lanjutan seperti uji t atau ANOVA.")
    }
    
    div(
      style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px;",
      
      p(HTML("<strong>Hipotesis:</strong><br>
            H<sub>0</sub>: Rasio ragam = 1 (varians kedua kelompok sama)<br>
            H<sub>1</sub>: Rasio ragam ≠ 1 (varians kedua kelompok berbeda)")),
      
      p(HTML(paste0("<strong>Statistik Uji (F):</strong> ", round(test$statistic, 4)))),
      p(HTML(paste0("<strong>p-value:</strong> ", format.pval(test$p.value, digits = 4)))),
      p(HTML(paste0("<strong>Keputusan (α = ", alpha, "):</strong> ", keputusan))),
      hr(),
      p(HTML(paste0("<strong>Kesimpulan :</strong><br>", kesimpulan))),
      p(HTML(paste0("<strong>Interpretasi :</strong><br>", interpretasi_awam)))
    )
  })
  
  
  anova_results <- reactive({
    req(input$var_anova_y, input$var_anova_x)
    formula <- as.formula(paste(input$var_anova_y, "~", input$var_anova_x))
    model <- aov(formula, data = data_kategorisasi())
    summary(model)
  })
  
  output$anova_results_ui <- renderUI({
    res_summary <- anova_results()
    req(res_summary)
    
    f_value <- res_summary[[1]]$`F value`[1]
    p_value <- res_summary[[1]]$`Pr(>F)`[1]
    alpha <- 0.05
    keputusan <- ifelse(
      p_value < alpha,
      "<strong style='color:red;'>Tolak H0</strong>",
      "<strong style='color:green;'>Gagal Tolak H0</strong>"
    )
    
    
    confidence <- (1 - alpha) * 100
    
    kesimpulan <- if (p_value < alpha) {
      paste0("Berdasarkan hasil uji ANOVA dengan tingkat signifikansi ", alpha,
             " (", confidence, "% tingkat kepercayaan), terdapat cukup bukti untuk menyatakan bahwa <strong>setidaknya ada satu kelompok</strong> yang memiliki rata-rata berbeda secara signifikan. ",
             "Artinya, perlakuan atau kondisi antar kelompok <strong>berpengaruh terhadap nilai rata-rata</strong> yang diamati.")
    } else {
      paste0("Berdasarkan hasil uji ANOVA dengan tingkat signifikansi ", alpha,
             " (", confidence, "% tingkat kepercayaan), <strong>tidak ditemukan cukup bukti</strong> untuk menyatakan adanya perbedaan rata-rata antar kelompok. ",
             "Artinya, seluruh kelompok cenderung memiliki rata-rata yang serupa, dan perlakuan yang diberikan <strong>tidak berpengaruh signifikan</strong>.")
    }
    
    div(
      style="border: 1px solid #ddd; border-radius: 6px; padding: 15px;",
      
      h5(strong("Hipotesis Uji ANOVA")),
      tags$ul(
        tags$li(HTML("<strong>H<sub>0</sub></strong>: Rata-rata semua kelompok adalah sama")),
        tags$li(HTML("<strong>H<sub>1</sub></strong>: Setidaknya ada satu rata-rata kelompok yang berbeda"))
      ),
      
      h5("Tabel Ringkasan ANOVA"),
      verbatimTextOutput("anova_summary_table"),
      hr(),
      
      p(HTML(paste0("<strong>Statistik Uji (F):</strong> ", round(f_value, 4)))),
      p(HTML(paste0("<strong>p-value:</strong> ", format.pval(p_value, digits = 4)))),
      p(HTML(paste0("<strong>Keputusan (α = ", alpha, "):</strong> ", keputusan))),
      hr(),
      p(HTML(paste0("<strong>Interpretasi:</strong><br>", kesimpulan)))
    )
  })
  
  
  output$anova_summary_table <- renderPrint({
    anova_results()
  })
  
  
  anova2_results <- reactive({
    req(input$var_anova2_y, input$var_anova2_x1, input$var_anova2_x2)
    validate(need(input$var_anova2_x1 != input$var_anova2_x2, "Silakan pilih dua faktor grup yang berbeda."))
    
    formula <- as.formula(paste(input$var_anova2_y, "~", input$var_anova2_x1, "*", input$var_anova2_x2))
    model <- aov(formula, data = data_kategorisasi())
    summary(model)
  })
  
  output$anova2_results_ui <- renderUI({
    res_summary <- anova2_results()
    req(res_summary)
    
    p_values <- res_summary[[1]]$`Pr(>F)`
    f_values <- res_summary[[1]]$`F value`
    alpha <- 0.05
    
    faktor1 <- names(cat_var_list[cat_var_list == input$var_anova2_x1])
    faktor2 <- names(cat_var_list[cat_var_list == input$var_anova2_x2])
    
    keputusan <- function(pval) {
      if (is.na(pval)) return("Data tidak lengkap")
      if (pval < alpha) return("<span style='color:red'><strong>Tolak H<sub>0</sub></strong></span>")
      return("<span style='color:green'><strong>Gagal Tolak H<sub>0</sub></strong></span>")
    }
    
    interpretasi <- function(pval, label, efek = "pengaruh") {
      if (is.na(pval)) return(paste0("Efek ", label, " tidak dapat dievaluasi karena data tidak lengkap."))
      
      if (pval < alpha) {
        return(paste0(
          "Berdasarkan hasil uji ANOVA dua arah dengan tingkat signifikansi 5%, ",
          label, " memiliki ", efek, " yang signifikan terhadap variabel respon."
        ))
      }
      
      return(paste0(
        "Berdasarkan hasil uji ANOVA dua arah dengan tingkat signifikansi 5%, ",
        label, " tidak memiliki ", efek, " yang signifikan terhadap variabel respon."
      ))
    }
    
    div(
      style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px;",
      
      h5(strong("Hipotesis Uji ANOVA Dua Arah")),
      tags$ul(
        tags$li(HTML(paste0("<strong>Faktor 1 (", faktor1, "):</strong><br>H<sub>0</sub>: Tidak ada perbedaan rata-rata antar level ", faktor1, "<br>H<sub>1</sub>: Setidaknya ada satu rata-rata yang berbeda"))),
        tags$li(HTML(paste0("<strong>Faktor 2 (", faktor2, "):</strong><br>H<sub>0</sub>: Tidak ada perbedaan rata-rata antar level ", faktor2, "<br>H<sub>1</sub>: Setidaknya ada satu rata-rata yang berbeda"))),
        tags$li(HTML(paste0("<strong>Interaksi:</strong><br>H<sub>0</sub>: Tidak ada interaksi antara ", faktor1, " dan ", faktor2, "<br>H<sub>1</sub>: Terdapat interaksi antara ", faktor1, " dan ", faktor2)))
      ),
      
      h5("Tabel Ringkasan ANOVA Dua Arah"),
      verbatimTextOutput("anova2_summary_table"),
      hr(),
      
      h5("Statistik Uji, p-value, dan Keputusan"),
      tags$table(
        class = "table table-bordered",
        style = "width:100%;",
        tags$thead(
          tags$tr(
            tags$th("Komponen"),
            tags$th("Statistik Uji (F)"),
            tags$th("p-value"),
            tags$th(paste0("Keputusan (α = ", alpha, ")"))
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(faktor1),
            tags$td(round(f_values[1], 4)),
            tags$td(format.pval(p_values[1], digits = 4)),
            tags$td(HTML(keputusan(p_values[1])))
          ),
          tags$tr(
            tags$td(faktor2),
            tags$td(round(f_values[2], 4)),
            tags$td(format.pval(p_values[2], digits = 4)),
            tags$td(HTML(keputusan(p_values[2])))
          ),
          tags$tr(
            tags$td(paste("Interaksi", faktor1, "*", faktor2)),
            tags$td(round(f_values[3], 4)),
            tags$td(format.pval(p_values[3], digits = 4)),
            tags$td(HTML(keputusan(p_values[3])))
          )
        )
      ),
      
      hr(),
      div(
        HTML("<strong>Interpretasi:</strong>"),
        tags$ul(
          tags$li(interpretasi(p_values[1], paste0("Faktor ", faktor1))),
          tags$li(interpretasi(p_values[2], paste0("Faktor ", faktor2))),
          tags$li(interpretasi(p_values[3], paste0("Interaksi antara ", faktor1, " dan ", faktor2), "interaksi"))
        )
      )
    )
  })
  
  output$anova2_summary_table <- renderPrint({
    anova2_results()
  })
  
  output$download_infer_report_pdf <- downloadHandler(
    filename = function() {
      safe_name <- gsub("[^a-zA-Z0-9]", "_", input$infer_tabs)
      paste0("Laporan_", safe_name, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(input$infer_tabs)
      
      tempReport <- file.path(tempdir(), "report_inferensia.Rmd")
      params <- list()
      rmd_content <- ""
      
        if (input$infer_tabs == "Uji Beda Rata-rata (T-Test)") {
          res <- t_test_results()
          req(res, cancelOutput = TRUE)
          params <- list(result = res$test, type = res$type)
          rmd_content <- "
---
title: 'Laporan Uji Beda Rata-rata (Uji-t)'
output: pdf_document
params:
  result: NA
  type: ''
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r}
test <- params$result
h0 <- ifelse(params$type == 'one_sample', paste('H0: μ =', test$null.value), 'H0: μ1 - μ2 = 0')
h1 <- ifelse(params$type == 'one_sample', paste('H1: μ \neq', test$null.value), 'H1: μ1 - μ2 \neq 0')
kesimpulan <- ifelse(test$p.value < 0.05, 'Terdapat perbedaan rata-rata yang signifikan secara statistik.', 'Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan.')
cat(paste('Hipotesis:', h0, 'vs', h1, '\n\n'))
cat('Hasil Uji:\n')
cat(paste('- Statistik Uji (t):', round(test$statistic, 4), '\n'))
cat(paste('- p-value:', format.pval(test$p.value, digits = 4), '\n'))
cat(paste('- Keputusan (alfa = 0.05):', ifelse(test$p.value < 0.05, 'Tolak H0', 'Gagal Tolak H0'), '\n\n'))
cat(paste('Kesimpulan:', kesimpulan))
```
"
          
        } else if (input$infer_tabs == "Uji Proporsi") {
          res <- prop_test_results()
          req(res, cancelOutput = TRUE)
          params <- list(result = res$test, counts = res)
          rmd_content <- "
---
title: 'Laporan Uji Proporsi'
output: pdf_document
params:
  result: NA
  counts: NA
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r}
test <- params$result
kesimpulan <- ifelse(test$p.value < 0.05, 'Proporsi sampel berbeda signifikan dari nilai hipotesis.', 'Proporsi sampel tidak berbeda signifikan dari nilai hipotesis.')
cat(paste0('Data: ', params$counts$success, ' sukses dari ', params$counts$total, ' total.\n\n'))
cat(paste('Hipotesis: H0: p =', test$null.value, ' vs H1: p \neq', test$null.value, '\n\n'))
cat('Hasil Uji:\n')
cat(paste('- Statistik Uji (X-squared):', round(test$statistic, 4), '\n'))
cat(paste('- p-value:', format.pval(test$p.value, digits = 4), '\n'))
cat(paste('- Keputusan (alfa = 0.05):', ifelse(test$p.value < 0.05, 'Tolak H0', 'Gagal Tolak H0'), '\n\n'))
cat(paste('Kesimpulan:', kesimpulan))
```
"
          
        } else if (input$infer_tabs == "Uji Ragam (Varian)") {
          res <- var_test_results()
          req(res, cancelOutput = TRUE)
          params <- list(result = res$test)
          rmd_content <- "
---
title: 'Laporan Uji Ragam (Uji F)'
output: pdf_document
params:
  result: NA
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r}
test <- params$result
kesimpulan <- ifelse(test$p.value < 0.05, 'Terdapat perbedaan ragam yang signifikan.', 'Tidak ada perbedaan ragam yang signifikan (ragam homogen).')
cat('Hipotesis: H0: Rasio ragam = 1 vs H1: Rasio ragam \neq 1\n\n')
cat('Hasil Uji:\n')
cat(paste('- Statistik Uji (F):', round(test$statistic, 4), '\n'))
cat(paste('- p-value:', format.pval(test$p.value, digits = 4), '\n'))
cat(paste('- Keputusan (alfa = 0.05):', ifelse(test$p.value < 0.05, 'Tolak H0', 'Gagal Tolak H0'), '\n\n'))
cat(paste('Kesimpulan:', kesimpulan))
```
"
          
        } else if (input$infer_tabs == "ANOVA") {
          res1 <- anova_results()
          res2 <- anova2_results()
          req(res1, res2, cancelOutput = TRUE)
          params <- list(result1 = res1, result2 = res2)
          rmd_content <- "
---
title: 'Laporan Analisis Ragam (ANOVA)'
output: pdf_document
params:
  result1: NA
  result2: NA
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Hasil ANOVA Satu Arah
```{r}
print(params$result1)
```

Kesimpulan:
```{r}
cat(paste('Dengan p-value', format.pval(params$result1[[1]]$`Pr(>F)`[1], digits=4),
          ifelse(params$result1[[1]]$`Pr(>F)`[1] < 0.05,
                 'terdapat perbedaan rata-rata yang signifikan antar kelompok.',
                 'tidak terdapat perbedaan rata-rata yang signifikan antar kelompok.')))
```

\newpage

## Hasil ANOVA Dua Arah
```{r}
print(params$result2)
```

Kesimpulan:
```{r}
cat('Berdasarkan tabel di atas, dapat disimpulkan pengaruh dari masing-masing faktor dan interaksinya.')
```
"
        }
      
      writeLines(rmd_content, tempReport)
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  model1_results <- reactive({
    showNotification("Menganalisis Model Awal...", type = "message", duration = 2)
    req(data, "KEMISKINAN", "RUMAH_TANGGA_TANPA_LISTRIK", "RUMAH_TANGGA_TANPA_DRAINASE", 
        "RUMAH_TANGGA_AIR_PIPA", "PENDIDIKAN_RENDAH", "BUTA_HURUF")
    
    model <- lm(KEMISKINAN ~ RUMAH_TANGGA_TANPA_LISTRIK + RUMAH_TANGGA_TANPA_DRAINASE + RUMAH_TANGGA_AIR_PIPA + PENDIDIKAN_RENDAH + BUTA_HURUF, data = data)
    list(summary = summary(model))
  })
  
  output$model1_results_ui <- renderUI({
    res <- model1_results()
    tagList(
      h6("Tabel Hasil Uji F dan Uji t:"),
      tags$pre(paste(capture.output(print(res$summary)), collapse = "\n")),
      p(HTML(
        "<strong>Uji F:</strong> Menguji signifikansi semua variabel dalam model secara simultan.<br>
      H<sub>0</sub>: Semua β = 0 (tidak ada pengaruh signifikan).<br>
      H<sub>1</sub>: Minimal satu β ≠ 0.<br>
      Keputusan: Lihat p-value pada bagian 'F-statistic'. Jika < 0.05, tolak H<sub>0</sub> → model signifikan secara keseluruhan.<br><br>
      <strong>Uji t:</strong> Menguji signifikansi masing-masing variabel.<br>
      H<sub>0</sub>: β = 0 (tidak ada pengaruh signifikan).<br>
      H<sub>1</sub>: β ≠ 0.<br>
      Keputusan: Lihat p-value masing-masing koefisien. Jika < 0.05 → variabel signifikan mempengaruhi Y."
      ))
    )
  })
    
  model2_results <- reactive({
    showNotification("Menganalisis Model Perbaikan...", type = "message", duration = 2)
    model <- lm(KEMISKINAN ~ RUMAH_TANGGA_TANPA_LISTRIK + RUMAH_TANGGA_TANPA_DRAINASE + RUMAH_TANGGA_AIR_PIPA + BUTA_HURUF, data = data)
    list(
      summary = summary(model),
      vif = vif(model),
      shapiro = shapiro.test(residuals(model)),
      bptest = bptest(model)
    )
  })
  
  output$model2_results_ui <- renderUI({
    res <- model2_results()
    tagList(
      h5("Hasil Uji F dan Uji t pada Model Perbaikan"),
      tags$pre(paste(capture.output(print(res$summary)), collapse = "\n")),
      p(HTML("
      <strong>Uji F (Signifikansi Model Secara Keseluruhan)</strong><br>
      - <em>Hipotesis:</em> H<sub>0</sub>: Semua koefisien regresi = 0; H<sub>1</sub>: Minimal satu ≠ 0.<br>
      - <em>Keputusan:</em> Lihat p-value pada F-statistic. Jika p-value < 0.05, maka tolak H<sub>0</sub>.<br>
      - <em>Kesimpulan:</em> Model secara keseluruhan signifikan jika p-value < 0.05.<br><br>
      
      <strong>Uji t (Signifikansi Masing-masing Variabel)</strong><br>
      - <em>Hipotesis:</em> H<sub>0</sub>: Koefisien β = 0 (tidak berpengaruh); H<sub>1</sub>: β ≠ 0.<br>
      - <em>Keputusan:</em> Lihat p-value pada masing-masing variabel. Jika p-value < 0.05, maka variabel signifikan.<br>
      - <em>Kesimpulan:</em> Variabel yang signifikan secara statistik memiliki pengaruh terhadap tingkat kemiskinan.")
      )
    )
  })
  
  output$model2_assumptions_ui <- renderUI({
    res <- model2_results()
    tagList(
      h5("Uji Multikolinearitas (VIF)"),
      tags$pre(paste(capture.output(print(res$vif)), collapse = "\n")),
      p("→ Jika semua nilai VIF < 10, maka tidak ada indikasi multikolinearitas yang serius."),
      
      h5("Uji Normalitas (Shapiro-Wilk)"),
      tags$pre(paste(capture.output(print(res$shapiro)), collapse = "\n")),
      p(HTML(paste0(
        "→ H<sub>0</sub>: error berdistribusi normal. H<sub>1</sub>: error tidak berdistribusi normal.<br>",
        "p-value = ", round(res$shapiro$p.value, 4), ". ",
        if (res$shapiro$p.value > 0.05) {
          "Karena p-value > 0.05, gagal tolak H₀ → error berdistribusi normal."
        } else {
          "Karena p-value < 0.05, tolak H₀ → error tidak berdistribusi normal."
        }
      ))),
      
      h5("Uji Homoskedastisitas (Breusch-Pagan)"),
      tags$pre(paste(capture.output(print(res$bptest)), collapse = "\n")),
      p(HTML(paste0(
        "→ H<sub>0</sub>: Residual homoskedastik (varian konstan). H<sub>1</sub>: Residual heteroskedastik.<br>",
        "p-value = ", round(res$bptest$p.value, 4), ". ",
        if (res$bptest$p.value > 0.05) {
          "Karena p-value > 0.05, gagal tolak H₀ → tidak ada indikasi heteroskedastisitas."
        } else {
          "Karena p-value < 0.05, tolak H₀ → ada indikasi heteroskedastisitas."
        }
      )))
    )
  })
  
  model3_results <- reactive({
    showNotification("Menganalisis Model Final (Log-Linear)...", type = "message", duration = 2)
    
    data_transformed <- data
    data_transformed$log_KEMISKINAN <- log(data_transformed$KEMISKINAN)
  
    model <- lm(log_KEMISKINAN ~ RUMAH_TANGGA_TANPA_LISTRIK + RUMAH_TANGGA_TANPA_DRAINASE + RUMAH_TANGGA_AIR_PIPA + BUTA_HURUF, data = data_transformed)
    
    list(
      model = model,
      summary = summary(model),
      vif = vif(model),
      shapiro = shapiro.test(residuals(model)),
      bptest = bptest(model)
    )
  })
  
  output$model3_results_ui <- renderUI({
    res <- model3_results()
    coef_matrix <- res$summary$coefficients
    coef_vals <- coef_matrix[, "Estimate"]
    coef_names <- names(coef_vals)
    
    label_map <- c(
      "KEMISKINAN" = "Kemiskinan",
      "RUMAH_TANGGA_TANPA_LISTRIK" = "Rumah Tangga Tanpa Listrik",
      "RUMAH_TANGGA_AIR_PIPA" = "Rumah Tangga Guna Air Pipa",
      "RUMAH_TANGGA_TANPA_DRAINASE" = "Rumah Tangga Tanpa Drainase",
      "PENDIDIKAN_RENDAH" = "Pendidikan Rendah",
      "BUTA_HURUF" = "Buta Huruf"
    )
    
    interpretasi_list <- lapply(seq_along(coef_names), function(i) {
      var <- coef_names[i]
      beta <- coef_vals[i]
      label <- ifelse(var %in% names(label_map), label_map[[var]], var)
      
      if (i == 1) {
        paste0("b0 = ", round(beta, 4),
               ", artinya saat seluruh variabel prediktor bernilai nol, maka logaritma tingkat kemiskinan diperkirakan sebesar ",
               round(beta, 4), ". Ini adalah baseline model.")
      } else {
        arah <- if (beta > 0) "meningkatkan" else "menurunkan"
        paste0("b", i - 1, " = ", round(beta, 4),
               ", artinya setiap kenaikan 1% pada proporsi ", label,
               " akan ", arah, " tingkat kemiskinan sekitar ",
               round(abs(beta * 100), 2), "%, dengan asumsi variabel lain tetap konstan.")
      }
    })
    
    interpretasi_final <- paste(interpretasi_list, collapse = "\n")
    
    tagList(
      h6("Tabel Koefisien Model Log-Linear:"),
      p("Interpretasi koefisien (β) pada model ini adalah dalam bentuk semi-elastisitas: jika variabel X naik 1 unit, maka Y akan berubah sebesar (β × 100)%."),
      
      tags$pre(paste(capture.output(print(coef_matrix)), collapse = "\n")),
      h6("Interpretasi Koefisien:"),
      tags$pre(interpretasi_final)
    )
  })
  
  
  output$model3_assumptions_ui <- renderUI({
    res <- model3_results()
    tagList(
      h6("Uji Multikolinearitas (VIF)"),
      tags$pre(paste(capture.output(print(res$vif)), collapse = "\n")),
      p("→ VIF < 10 berarti tidak ada multikolinearitas serius."),
      
      h6("Uji Normalitas(Shapiro-Wilk)"),
      tags$pre(paste(capture.output(print(res$shapiro)), collapse = "\n")),
      p(HTML(paste0(
        "→ H<sub>0</sub>: error berdistribusi normal.<br>",
        "p-value = ", round(res$shapiro$p.value, 4), ". ",
        if (res$shapiro$p.value > 0.05) {
          "Gagal tolak H₀ → error berdistribusi normal."
        } else {
          "Tolak H₀ → error tidak berdistribusi normal."
        }
      ))),
      
      h6("Uji Homoskedastisitas (Breusch-Pagan)"),
      tags$pre(paste(capture.output(print(res$bptest)), collapse = "\n")),
      p(HTML(paste0(
        "→ H<sub>0</sub>: Varian residual konstan (homoskedastik).<br>",
        "p-value = ", round(res$bptest$p.value, 4), ". ",
        if (res$bptest$p.value > 0.05) {
          "Tidak ada indikasi heteroskedastisitas."
        } else {
          "Ada indikasi heteroskedastisitas."
        }
      )))
    )
  })
  
  output$final_conclusion_ui <- renderUI({
    res3 <- model3_results()
    
    tagList(
      h4("Model Terbaik dan Justifikasi Pemilihan"),
      p("Berdasarkan hasil analisis regresi yang dilakukan, model log-linear (dengan variabel logaritma Y) tanpa variabel pendidikan rendah dipilih sebagai model terbaik. Alasan pemilihan ini didasarkan pada kriteria statistik dan diagnostik model berikut:"),
      tags$ol(
        tags$li("Seluruh variabel prediktor dalam model final signifikan secara statistik berdasarkan uji t (p-value < 0.05), menunjukkan bahwa variabel-variabel tersebut memiliki kontribusi yang nyata terhadap tingkat kemiskinan."),
        tags$li("Uji F menunjukkan bahwa model signifikan secara keseluruhan, artinya model secara umum menjelaskan variasi dalam data dengan baik."),
        tags$li("Tidak terdapat indikasi multikolinearitas yang serius karena seluruh nilai Variance Inflation Factor (VIF) berada di bawah ambang batas 10."),
        tags$li("Transformasi logaritma pada variabel dependen berhasil memperbaiki indikasi heteroskedastisitas, seperti ditunjukkan oleh p-value dari uji Breusch-Pagan yang meningkat."),
        tags$li(HTML(paste0("Meskipun asumsi normalitas belum sepenuhnya terpenuhi (p-value Shapiro-Wilk = ", round(res3$shapiro$p.value, 4), "), ukuran sampel yang besar memungkinkan penerapan <strong>Teorema Limit Pusat</strong>. Oleh karena itu, inferensi berdasarkan model ini tetap dapat dipertanggungjawabkan secara statistik.")))
      ),
      h4("Kesimpulan"),
      p("Dashboard ini merupakan alat analisis komprehensif yang dirancang untuk mengevaluasi pengaruh akses terhadap infrastruktur dasar (listrik, air bersih melalui pipa, dan sistem drainase) serta kondisi pendidikan (tingkat pendidikan rendah dan buta huruf) terhadap tingkat kemiskinan di wilayah kabupaten/kota di Indonesia."),
      p("Melalui pendekatan regresi linear berganda yang disempurnakan dengan transformasi logaritma (log-linear), analisis ini menemukan bahwa faktor-faktor seperti keterbatasan akses listrik, drainase, dan air bersih, serta tingginya angka buta huruf, memiliki hubungan yang signifikan terhadap tingkat kemiskinan."),
      p("Model log-linear yang digunakan tidak hanya memenuhi sebagian besar asumsi klasik regresi, tetapi juga menunjukkan kapasitas yang kuat dalam menjelaskan variasi kemiskinan antar wilayah. Temuan ini menegaskan pentingnya intervensi kebijakan yang berfokus pada peningkatan akses infrastruktur dan perbaikan kualitas pendidikan sebagai strategi pengurangan kemiskinan di wilayah Kab/Kota di Indonesia.")
    )
  })
  
  
  output$download_regression_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Regresi_Kemiskinan_", Sys.Date(), ".docx")
    },
    content = function(file) {
      res <- model3_results()
      coef_matrix <- res$summary$coefficients
      coef_vals <- coef_matrix[, "Estimate"]
      coef_names <- names(coef_vals)
      
      label_map <- c(
        "KEMISKINAN" = "Kemiskinan",
        "RUMAH_TANGGA_TANPA_LISTRIK" = "Rumah Tangga Tanpa Listrik",
        "RUMAH_TANGGA_AIR_PIPA" = "Rumah Tangga Guna Air Pipa",
        "RUMAH_TANGGA_TANPA_DRAINASE" = "Rumah Tangga Tanpa Drainase",
        "PENDIDIKAN_RENDAH" = "Pendidikan Rendah",
        "BUTA_HURUF" = "Buta Huruf"
      )
      
      interpretasi_list <- lapply(seq_along(coef_names), function(i) {
        var <- coef_names[i]
        beta <- coef_vals[i]
        label <- ifelse(var %in% names(label_map), label_map[[var]], var)
        
        if (i == 1) {
          paste0("b0 = ", round(beta, 4), ", artinya saat seluruh variabel prediktor bernilai nol, maka logaritma tingkat kemiskinan diperkirakan sebesar ", round(beta, 4), ". Ini adalah baseline model.")
        } else {
          arah <- if (beta > 0) "meningkatkan" else "menurunkan"
          paste0("b", i - 1, " = ", round(beta, 4), ", artinya setiap kenaikan 1% pada proporsi ", label, " akan ", arah, " tingkat kemiskinan sekitar ", round(abs(beta * 100), 2), "%, dengan asumsi variabel lain tetap konstan.")
        }
      })
      
      interpretasi_final <- paste(interpretasi_list, collapse = "\n")
      
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Laporan Hasil Analisis Regresi (Model Log-Linear)", style = "heading 1")
      
      doc <- officer::body_add_par(doc, "Langkah Analisis:", style = "heading 2")
      doc <- officer::body_add_par(doc, paste(
        "1. Eksplorasi awal data dan transformasi variabel untuk mengatasi distribusi yang tidak normal dan potensi heteroskedastisitas.",
        "2. Pemodelan awal dengan regresi linier berganda tanpa transformasi.",
        "3. Evaluasi multikolinearitas dengan VIF dan penghapusan variabel yang memiliki korelasi tinggi antar prediktor.",
        "4. Transformasi logaritma pada variabel dependen untuk membentuk model log-linear.",
        "5. Evaluasi asumsi klasik: normalitas (Shapiro-Wilk), homoskedastisitas (Breusch-Pagan), dan multikolinearitas (VIF).",
        "6. Pemilihan model terbaik berdasarkan uji signifikansi dan diagnostik asumsi.",
        sep = "\n")
      )
      
      doc <- officer::body_add_par(doc, "Tabel Koefisien:", style = "heading 2")
      doc <- officer::body_add_table(doc, value = as.data.frame(coef_matrix), style = "table_template")
      
      doc <- officer::body_add_par(doc, "\nInterpretasi Koefisien:", style = "heading 2")
      doc <- officer::body_add_par(doc, interpretasi_final)
      
      doc <- officer::body_add_par(doc, "\nUji Asumsi Klasik:", style = "heading 2")
      
      doc <- officer::body_add_par(doc, "Multikolinearitas (VIF):", style = "heading 3")
      doc <- officer::body_add_table(doc, as.data.frame(res$vif), style = "table_template")
      
      doc <- officer::body_add_par(doc, "Normalitas (Shapiro-Wilk):", style = "heading 3")
      doc <- officer::body_add_par(doc, paste(capture.output(print(res$shapiro)), collapse = "\n"))
      
      doc <- officer::body_add_par(doc, "Homoskedastisitas (Breusch-Pagan):", style = "heading 3")
      doc <- officer::body_add_par(doc, paste(capture.output(print(res$bptest)), collapse = "\n"))
      
      doc <- officer::body_add_par(doc, "\nKesimpulan dan Justifikasi Model:", style = "heading 2")
      doc <- officer::body_add_par(doc, paste(
        "Model log-linear (dengan variabel terlogaritma) tanpa variabel PENDIDIKAN_RENDAH dipilih sebagai model terbaik.",
        "Alasan pemilihan ini didasarkan pada kriteria statistik dan diagnostik model berikut:",
        "- Seluruh variabel prediktor dalam model final signifikan secara statistik berdasarkan uji t (p-value < 0.05).",
        "- Uji F menunjukkan bahwa model signifikan secara keseluruhan.",
        "- Tidak terdapat indikasi multikolinearitas serius karena VIF < 10.",
        "- Transformasi logaritma pada variabel dependen berhasil memperbaiki indikasi heteroskedastisitas.",
        "- Meskipun normalitas belum sempurna (p-value Shapiro-Wilk = 0.0245), ukuran sampel besar memungkinkan penerapan Teorema Limit Pusat.",
        "\nModel ini menjelaskan variasi kemiskinan antar wilayah secara signifikan, dan mendukung perumusan kebijakan berbasis data terhadap akses listrik, air bersih, drainase, dan pendidikan.",
        sep = "\n")
      )
      
      print(doc, target = file)
    }
  )
  
  output$download_deskriptif <- downloadHandler(
    filename = function() {
      paste0("Ringkasan_Deskriptif_", input$var_deskriptif, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_deskriptif.Rmd")
      
      writeLines(c(
        "---",
        'title: "Ringkasan Statistik Deskriptif"',
        "output: pdf_document",
        "params:",
        "  var_name: NA",
        "  var_label: NA",
        "  summary_data: NA",
        "  interpretasi: NA",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE)",
        "```",
        "",
        "# Ringkasan Statistik Deskriptif",
        "",
        "**Variabel**: `r params$var_label` (`r params$var_name`)",
        "",
        "## Ringkasan Statistik",
        "",
        "```{r summary}",
        "params$summary_data",
        "```",
        "",
        "## Interpretasi",
        "",
        "`r params$interpretasi`"
      ), tempReport)
      
      var_name <- input$var_deskriptif
      var_label <- label_map[[var_name]]
      summary_data <- summary(data[[var_name]])
      interpretasi_text <- renderText({ 
        paste0("Berdasarkan nilai ringkasan, dapat dilihat bahwa nilai median dari ", var_label, 
               " adalah sekitar ", round(summary_data["Median"], 2), 
               ", dengan nilai maksimum sebesar ", round(summary_data["Max."], 2),
               " dan minimum sebesar ", round(summary_data["Min."], 2), 
               ". Ini memberi gambaran awal tentang persebaran data.")
      })
      
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = list(
          var_name = var_name,
          var_label = var_label,
          summary_data = summary_data,
          interpretasi = interpretasi_text
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)