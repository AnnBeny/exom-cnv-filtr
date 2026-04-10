library(shiny)
library(bslib)
library(magrittr)
library(DT)
options(shiny.maxRequestSize = 30 * 1024^2) # max 30 MB

# Load helper functions
source("helpers.R")

# UI
ui <- page_sidebar(
  title = "Exom Analýza",

  bg = "#fafafac7",

  tags$head(
    tags$script(HTML("document.title = 'Exom Analýza';")),
    tags$style(HTML("
      .navbar.navbar-static-top {
        background: #007BC2;
        background: linear-gradient(90deg, rgba(0, 123, 194, 1) 0%, rgba(255, 255, 255, 1) 25%); # nolint
      }
      .navbar-brand {
        color: #ffffff !important;
        font-weight: 700 !important;
        font-size: 26px !importnat;
      }
      .navbar.navbar-static-top {
        background: #007BC2;
        background: linear-gradient(90deg,rgba(0, 123, 194, 1) 0%, rgba(255, 255, 255, 1) 25%); # nolint
      }
      .navbar-brand {
        color: #ffffff !important;
        font-weight: 700 !important;
      }
      .btn-file {
        font-size: 16px;
        width: 100%;
      }
      .btn-file:hover {
        font-size: 16px;
      }
      .input-group,
      .input-group-prepend {
        width: 100% !important;
        padding-top: 0px !important;
        margin-top: 0px;
      }
      .gender-row {
        display: flex;
        align-items: center;
        gap: 0 !important; 
        margin-bottom: 5px;
        padding: 0;
      }
      .gender-label {
        width: 120px;
      }
      .gender-select .form-group {
        margin: 0;
        width: 80px;
      }
      .card-body.bslib-gap-spacing {
        gap: 0 !important;
      }
      hr {
        margin-top: 5px;
        margin-bottom: 5px;
        border: 1px solid #ccc;
      }
    "))
  ),

  sidebar = sidebar(
    tags$h5(textOutput("text"), style = "color: #007BC2; font-weight: bold; font-size: 20px; margin-top: 10px"), # nolint
    fileInput("file", NULL, multiple = TRUE, accept = ".txt", buttonLabel = "Vybrat soubory", # nolint
              placeholder = "Nevybrán žádný soubor", width = "100%"),
    tags$style("
      .btn-file { font-size: 16px; }
      .btn-file:hover { font-size: 16px; }
      .btn-file { width: 100%; }
      .input-group { width: 100% !important; margin-top: 0px; padding-top: 0px !important; } # nolint
      .input-group-prepend { width: 100% !important; padding-top: 0px !important; } # nolint
    "),
    downloadButton("downloadCoveragemean", "Cov Mean ALL", class = "btn-lg btn-primary"), # nolint
    downloadButton("downloadCNVMmean", "CNV M Mean", class = "btn-lg btn-primary"), # nolint
    downloadButton("downloadCNVZmean", "CNV Z Mean", class = "btn-lg btn-primary"), # nolint
    tags$hr(),

    #downloadButton("downloadCoverageproc", "Cov Procenta ALL", class = "btn-lg btn-primary"), # nolint
    #downloadButton("downloadCNVMproc", "CNV M Procenta", class = "btn-lg btn-primary"), # nolint
    #downloadButton("downloadCNVZproc", "CNV Z Procenta", class = "btn-lg btn-primary"), # nolint
    tags$hr(),
    tags$a(
      href = "https://www.omim.org", target = "_blank",
      style = "font-weight: bold; font-size: 16px; display: block; margin-top: 10px;", # nolint
      icon("database"), "OMIM databáze"
    )
  ),

  card(
    uiOutput("gender_input"),
    uiOutput("action_button"),
  ),

  card(
    navset_card_tab(
      nav_panel("Coverage  Mean ALL", DT::dataTableOutput("coverage_table")), # nolint
      nav_panel("CNV Muži Mean", DT::dataTableOutput("cnv_m")), # nolint
      nav_panel("CNV Ženy Mean", DT::dataTableOutput("cnv_z")), # nolint
      # nav_panel()
      #nav_panel("Coverage Procenta ALL", DT::dataTableOutput("coverage_table_proc")), # nolint
      #nav_panel("CNV Muži Procenta", DT::dataTableOutput("cnv_m_proc")), # nolint
      #nav_panel("CNV Ženy Procenta", DT::dataTableOutput("cnv_z_proc")) # nolint
    )
  )
)

######################################################################################################################## # nolint

# Server logic
server <- function(input, output, session) {
  output$text <- renderText({
    if (is.null(input$file)) return("Kód várky: ")
    base_names <- sub(".coveragefin\\.txt$", "", input$file$name)
    codes <- substr(base_names, nchar(base_names) - 1, nchar(base_names))
    paste("Kód várky: ", unique(codes), collapse = ", \n")
  })

  sample_id <- reactive({
    req(input$file)
    gsub(".coveragefin\\.txt$", "", input$file$name)
  })

  output$gender_input <- renderUI({
    ids <- sample_id()
    lapply(ids, function(id) {
      div(class = "gender-row",
        div(class = "gender-label", strong(id)),
        div(class = "gender-select",
          selectInput(
            inputId = paste0("pohlavi", id),
            label = NULL,
            choices = c("Muž" = "M", "Žena" = "Z"),
            width = "80px",
            selectize = TRUE
          )
        )
      )
    })
  })

  final_data <- reactiveVal()
  final_data_proc <- reactiveVal()
  pohlavi_data <- reactiveVal()
  cnv_m_data <- reactiveVal()
  cnv_z_data <- reactiveVal()
  cnv_m_data_proc <- reactiveVal()
  cnv_z_data_proc <- reactiveVal()
  submit_status <- reactiveVal("ready")

  output$action_button <- renderUI({
    req(input$file)
    actionButton(
      "submit",
      label = if (submit_status() == "processing") "Zpracovávám..." else "Zpracovat", # nolint
      icon = if (submit_status() != "processing") icon("check") else NULL,
      class = if (submit_status() == "processing") "btn btn-primary" else "btn btn-success", # nolint
      style = "margin-top: 5px; width: 200px; font-size: 20px; padding: 10px;",
      disabled = submit_status() == "processing"
    )
  })

  observeEvent(input$submit, {
    req(input$file)
    submit_status("processing")

    withProgress(message = "Zpracování CNV...", value = 0, {
      submit_status("processing")
      incProgress(0.1, detail = "Načítání souborů...")

      file_list <- input$file$datapath
      filenames <- input$file$name
      ids <- sample_id()
      pohlavi <- sapply(ids, function(id) input[[paste0("pohlavi", id)]])
      pohlavi_df <- data.frame(ID = ids, Gender = pohlavi)
      pohlavi_data(pohlavi_df)
      if (!dir.exists("../data_output")) dir.create("../data_output")
      write.csv(pohlavi_df, "../data_output/pohlavi.csv", row.names = FALSE) # nolint

      #cat("file_list:", file_list, "\n")
      #cat("filenames:", filenames, "\n")
      #cat("ids:", ids, "\n")
      #cat("pohlavi:", pohlavi, "\n")

      #showNotification("Soubory coverage a CNV se generují.", type = "message") # nolint

      incProgress(0.3, detail = "Generování coverage dat...")

      # MEAN
      selected_cols_list <- lapply(seq_along(file_list), function(i) {
        tryCatch({
          df <- read.delim(file_list[i], check.names = FALSE)
          #if (nrow(df) < 1 || ncol(df) < 15) stop() # nolint
          selected <- df[, 5, drop = FALSE]
          base_name <- tools::file_path_sans_ext(gsub(".coveragefin\\.txt$", "", filenames[i])) # nolint
          gender <- input[[paste0("pohlavi", ids[i])]]
          colnames(selected) <- paste0(gender, "_", base_name)
          return(selected)
          print(gender)
        }, error = function(e) {
          showNotification(paste("Chyba u souboru:", filenames[i]), type = "error") # nolint
          return(NULL)
        })
      })
      write.csv(selected_cols_list, "../data_output/selected_cols_list.csv", row.names = FALSE) # nolint

      #cat("selected_cols_list \n")
      #print(head(selected_cols_list, 5))

      # PERCENTAGE
      selected_cols_list_proc <- lapply(seq_along(file_list), function(i) {
        tryCatch({
          df <- read.delim(file_list[i], check.names = FALSE)
          #if (nrow(df) < 1 || ncol(df) < 15) stop()
          selected <- df[, 6, drop = FALSE]
          base_name <- tools::file_path_sans_ext(gsub(".coveragefin\\.txt$", "", filenames[i])) # nolint
          gender <- input[[paste0("pohlavi", ids[i])]]
          colnames(selected) <- paste0(gender, "_", base_name)
          return(selected)
        }, error = function(e) {
          showNotification(paste("Chyba u souboru:", filenames[i]), type = "error") # nolint
          return(NULL)
        })
      })
      write.csv(selected_cols_list_proc, "../data_output/selected_col_list_proc.csv", row.names = FALSE) # nolint

      #cat("selected_cols_list_proc \n")
      #print(head(selected_cols_list_proc, 5))

      result <- do.call(cbind, selected_cols_list)
      result_proc <- do.call(cbind, selected_cols_list_proc)

      prvni_trisloupce <- read.delim(file_list[1], check.names = FALSE)[, 1:4]

      combined <- cbind(prvni_trisloupce, result)
      combined_proc <- cbind(prvni_trisloupce, result_proc)
      write.csv(combined, "../data_output/combined.csv", row.names = FALSE) # nolint

      colnames(combined) <- trimws(gsub(".COV-mean", "", colnames(combined), fixed = TRUE)) # nolint
      colnames(combined_proc) <- trimws(gsub(".COV-procento", "", colnames(combined_proc), fixed = TRUE)) # nolint
      final_data(combined)
      final_data_proc(combined_proc)

      incProgress(0.6, detail = "Normalizace CNV M...")

      # CNV logic
      coverage <- final_data()
      coverage_proc <- final_data_proc()
      pohlavi <- pohlavi_data()
      #row_id <- seq.int(nrow(coverage)) # nolint
      m <- colnames(coverage)[grepl("^M_", colnames(coverage))]
      z <- colnames(coverage)[grepl("^Z_", colnames(coverage))]
      m_p <- colnames(coverage_proc)[grepl("^M_", colnames(coverage_proc))]
      z_p <- colnames(coverage_proc)[grepl("^Z_", colnames(coverage_proc))]
      omimgeny <- load_omim_file()
      write.csv(m, "../data_output/m.csv", row.names = FALSE) # nolint
      write.csv(omimgeny, "../data_output/omimgeny.csv", row.names = FALSE) # nolint

      # MEN
      if (length(m) > 0) {

        # MEAN
        normalized_m <- normalize_coverage(coverage[, m, drop = FALSE])
        #coverage$row_id <- seq.int(nrow(coverage)) # nolint
        coverage$Row_id <- seq.int(nrow(coverage))
        coverage_m_final <- cbind(
          coverage[, c("chr", "start", "stop", "name", "Row_id")],
          normalized_m
        )
        #coverage_m_final <- cbind(coverage[, c(1:4)], normalized_m) # nolint
        #coverage_m_final <- cbind(coverage[, c(1:3)], Row_id = seq.int(nrow(coverage)), normalized_m) # nolint
        coverage_cols <- coverage_m_final[, -c(1:5), drop = FALSE]
        m_values <- abs(coverage_cols) > 0.25
        greater_m <- coverage_m_final[rowSums(m_values, na.rm = TRUE) > 0, ]
        greater_m <- annotate_with_omim(greater_m, omimgeny)
        cnv_m_data(greater_m)
        write.csv(greater_m, "../data_output/greater_m.csv", row.names = FALSE) # nolint)

        cat("greater_m \n")
        print(head(greater_m, 5))
        cat("coverage_m_final \n")
        print(head(coverage_m_final, 5))

        # PERCENTAGE
        coverage_proc$Row_id <- seq_len(nrow(coverage_proc))
        #cnv_m_data_proc(cbind(coverage_proc[, c(1:4)], coverage_proc[, m_p, drop = FALSE])) # nolint
        cnv_m_data_proc(
          cbind(
            coverage_proc[, c("chr", "start", "stop", "name", "Row_id")],
            coverage_proc[, m_p, drop = FALSE]
          )
        )
        #write.table(cnv_m_data_proc, "../data_output/cnv_m_data_proc.csv", row.names = FALSE) # nolint)

        cat("coverage_proc \n")
        print(head(coverage_proc, 5))
      }

      incProgress(0.8, detail = "Normalizace CNV Z...")

      # WOMEN
      if (length(z) > 0) {

        # MEAN
        normalized_z <- normalize_coverage(coverage[, z, drop = FALSE])
        coverage$row_id <- seq.int(nrow(coverage))
        coverage_z_final <- cbind(
          coverage[, c("chr", "start", "stop", "name", "Row_id")],
          normalized_z
        )
        #coverage_z_final <- cbind(coverage[, c(1:4)], normalized_z) # nolint
        coverage_cols <- coverage_z_final[, -c(1:5), drop = FALSE]
        z_values <- abs(coverage_cols) > 0.25
        greater_z <- coverage_z_final[rowSums(z_values, na.rm = TRUE) > 0, ]
        greater_z <- annotate_with_omim(greater_z, omimgeny)
        cnv_z_data(greater_z)

        # PERCENTAGE
        cnv_z_data_proc(cbind(coverage_proc[, c(1:5)], coverage_proc[, z_p, drop = FALSE])) # nolint
        cnv_z_data_proc(
          cbind(
            coverage_proc[, c("chr", "start", "stop", "name", "Row_id")],
            coverage_proc[, z_p, drop = FALSE]
          )
        )
      }
      incProgress(1, detail = "Hotovo")
    })

    submit_status("ready")
  })

  # Tables
  # MEAN
  output$coverage_table <- DT::renderDataTable({
    req(final_data())
    df <- final_data()
    validate(need(nrow(df) > 0, "Žádná data pro pokrytí"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  output$cnv_m <- DT::renderDataTable({
    req(cnv_m_data())
    df <- cnv_m_data()
    validate(need(nrow(df) > 0, "Žádná data pro CNV M"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  output$cnv_z <- DT::renderDataTable({
    req(cnv_z_data())
    df <- cnv_z_data()
    validate(need(nrow(df) > 0, "Žádná data pro CNV Z"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  # PERCENTAGE
  output$coverage_table_proc <- DT::renderDataTable({
    req(final_data_proc())
    df <- final_data_proc()
    validate(need(nrow(df) > 0, "Žádná data pro procenta pokrytí"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  output$cnv_m_proc <- DT::renderDataTable({
    req(cnv_m_data_proc())
    df <- cnv_m_data_proc()
    validate(need(nrow(df) > 0, "Žádná data pro procenta CNV M"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  output$cnv_z_proc <- DT::renderDataTable({
    req(cnv_z_data_proc())
    df <- cnv_z_data_proc()
    validate(need(nrow(df) > 0, "Žádná data pro procenta CNV Z"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  # Downloads
  output$downloadCoveragemean <- downloadHandler(
    filename = function() { "coveragemeanALL.csv" },
    content = function(file) {
      write.csv2(final_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )
  output$downloadCNVMmean <- downloadHandler(
    filename = function() { "CNV_M_mean.csv" },
    content = function(file) {
      write.csv2(cnv_m_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )
  output$downloadCNVZmean <- downloadHandler(
    filename = function() { "CNV_Z_mean.csv" },
    content = function(file) {
      write.csv2(cnv_z_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )
  #output$downloadCoverageproc <- downloadHandler(
  #  filename = function() { "coverageprocentoALL.csv" },
  #  content = function(file) {
  #    write.csv2(final_data_proc(), file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8") # nolint
  #  }
  #)
  #output$downloadCNVMproc <- downloadHandler(
  #  filename = function() { "CNV_M_procento.csv" },
  #  content = function(file) {
  #    write.csv2(cnv_m_data_proc(), file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8") # nolint
  #  }
  #)
  #output$downloadCNVZproc <- downloadHandler(
  #  filename = function() { "CNV_Z_procento.csv" },
  #  content = function(file) {
  #    write.csv2(cnv_z_data_proc(), file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8") # nolint
  #  }
  #)
}

# Run the application
shinyApp(ui = ui, server = server)
