library(shiny)
library(bslib)
library(magrittr)
library(stringr)
library(DT)
options(shiny.maxRequestSize = 30 * 1024^2) # max 30 MB

# Load helper functions
source("helpers.R")

# Filter
filter_regions <- c()

# UI
ui <- page_sidebar(
  title = "Exom Analýza a filtr genů",

  bg = "#fafafac7",

  tags$head(

    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.title = 'Exom Analýza a filtr genů';
        const handle = document.querySelector('.resize-handle');
        const box = document.querySelector('.resizable-card');

        if (!handle || !box) return;

        let startY, startHeight;

        handle.addEventListener('mousedown', function(e) {
          startY = e.clientY;
          startHeight = box.offsetHeight;

          document.addEventListener('mousemove', onMove);
          document.addEventListener('mouseup', stopMove);
        });

        function onMove(e) {
          const newHeight = startHeight + (e.clientY - startY);
          box.style.height = newHeight + 'px';
        }

        function stopMove() {
          document.removeEventListener('mousemove', onMove);
          document.removeEventListener('mouseup', stopMove);
        }


      });
      $(document).on('paste', '#regions + .selectize-control input', function(e) {
        var pasted = (e.originalEvent || e).clipboardData.getData('text');
        if (!pasted) return;

        e.preventDefault();

        var el = $('#regions')[0];
        if (!el || !el.selectize) return;

        var selectize = el.selectize;

        var parts = pasted
          .split(/[\\n,;\\t ]+/)
          .map(function(x) { return x.trim(); })
          .filter(function(x) { return x.length > 0; });

        parts.forEach(function(val) {
          if (selectize.options.hasOwnProperty(val)) {
            selectize.addItem(val);
          }
        });

        selectize.refreshItems();
        selectize.refreshOptions(false);
        selectize.updateOriginalInput();
        $(el).trigger('change');
      });
    ")),

    tags$style(HTML("
      .navbar.navbar-static-top {
        background: #007BC2;
        background: linear-gradient(90deg, rgba(0, 123, 194, 1) 20%, rgba(234, 203, 215, 1) 90%);
      }
      .navbar-brand {
        color: #ffffff !important;
        font-weight: 700 !important;
        font-size: 24px !important;
      }
      .sidebar {
        background-color: #f8f9fa;
        padding: 0px;
        border-radius: 0px;
        box-shadow: 0 0.085rem 0.20rem rgba(0, 0, 0, 0.150);
        --bslib-spacer: 0.2rem;
      }
      .card-container {
        position: relative;
        width: 100%;
      }
      .resizable-card {
        min-height: 300px;
        overflow: auto;
      }
      .resizable-card .card-body {
        height: 100%;
        overflow: auto;
      }
      .resize-handle {
        position: relative;
        bottom: 0;
        left: 50%;
        transform: translateX(-50%);
        cursor: ns-resize;
        width: 40px;
        height: 6px;
        background: #ccc;
        border-radius: 3px;
      }
      .resize-vertical-card{
        resize: vertical; 
        overflow: auto; 
        height: 300px; 
        box-shadow: 0 0.085rem 0.20rem rgba(0, 0, 0, 0.150);
        border-radius: 0.5rem;
        background-color: white;
        margin-bottom: 0px;
      }
      .resize-vertical-card .card {
        box-shadow: none !important;
        border: none !important;
      }
      .btn-file {
        font-size: 16px;
        font-weight: bold;
        width: 100%;
        background-color: #B9547B;
        color: white;
        border: none;
        border-top-left-radius: 8px;
        border-top-right-radius: 8px;
      }
      .btn-file:hover {
        font-size: 16px;
      }
      .form-control {
        font-size: 16px;
        padding: 10px;
        border-radius: 8px;
        border: 1px solid #ced4da;
      }
      .btn-primary {
        font-size: 16px;
        font-weight: bold;
        width: 100%;
        background-color: #007BC2;
        color: white;
        border: none;
        border-radius: 8px;
      }
      .input-group {
        margin-bottom: 0px;
        padding: 0px;
      }
      .input-group-prepend {
        width: 100% !important;
        padding-top: 0px !important;
        margin-top: 0px;
        margin-bottom: 0px;
        border-top-left-radius: 8px;
        border-top-right-radius: 8px;
      }
      .input-group .form-control {
        border-radius: 0px 0px 8px 8px !important;
        margin-left: 0px !important;
      }
      .input-group .btn {
        border-radius: 8px 8px 0px 0px !important;
      }
      .gender-row {
        display: flex;
        align-items: center;
        margin-bottom: 5px;
        gap: 10px;
      }
      .gender-label {
        width: 300px;
        text-align: right;
        padding-bottom: 0px;
      }
      .gender-select .form-group {
        margin-bottom: 0px;
      }
      .gender-select .form-group .selectize-input {
        border: 1px solid #ced4da;
        border-radius: 8px !important;
        font-size: 16px;
      }
      .action-button-container {
        display: flex;
        justify-content: center;
      }
      .info-gender-card {
        margin-bottom: 0px;
        overflow: auto;
        box-shadow: 0 0.085rem 0.20rem rgba(0, 0, 0, 0.150);
        border-radius: 0.5rem;
        padding: 1rem;
        background-color: white;
      }
      .submit-task-button {
        text-align: left; 
        margin-top: 10px;
        margin-left: 190px;
        margin-bottom: 20px;
      }
      .submit-task-button .btn {
        width: 200px; 
        font-size: 16px; 
        padding: 10px; 
        background-color: #008871; 
        border: none; 
        border-radius: 8px;
      }
      .form-check .form-check-input {
        float: right;
        align-items: center;
        cursor: none;
      }
      .form-check {
        align-items: center;
        margin-bottom: 0px;
        cursor: none;
      }
      .form-check .form-check-label {
        font-size: 16px;
        padding-right: 35px;
        align-items: center;
        cursor: default;
      }
      h5 {
        margin-top: 0px;
        margin-bottom: 10px;
        color: #007BC2; 
        font-weight: bold; 
        font-size: 18px; 
      }
      hr {
        margin-top: 5px;
        margin-bottom: 5px;
        border: 1px solid #ccc;
      }
      .gene-filter-block {
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .gene-filter-block .form-group {
        margin-bottom: 0px;
      }
      .gene-filter-block .selectize-input {
        border: 1px solid #ced4da;
        border-radius: 0px 0px 8px 8px !important;
        max-height: 200px;
        overflow-y: auto;
      }
      .gene-filter-block p {
        margin-top: 0px;
        margin-bottom: 5px;
      }
      .gene-filter-block .btn {
        margin-bottom: 0px; 
        width: 100%; 
        font-size: 16px; 
        padding: 10px; 
        background-color: #B9547B; 
        border: none; 
        border-radius: 8px 8px 8px 8px;
      }
      .gene-filter-block .btn-secondary {
        background-color: #A03E5B;
      }
      .download-buttons-group {
        margin-top: 15px;
        margin-bottom: 15px;
      }
      .download-buttons {
        margin-bottom: 0px;
        gap: 0px;
      }
      .download-buttons .form-group {
        margin-bottom: 0px;
        gap: 0px;
      }
    "))
  ),

  sidebar = sidebar(
    # tags$h5(textOutput("text"), style = "color: #007BC2; font-weight: bold; font-size: 20px; margin-top: 10px"), # nolint
    fileInput(
      "file",
      NULL,
      multiple = TRUE,
      accept = ".txt",
      buttonLabel = "Vybrat soubory",
      placeholder = "Nevybrán žádný soubor",
      width = "100%"
    ),

    tags$hr(),

    # h4("Filtr oblastí hg38", style = "margin-top: 30px; font-weight: bold;"), # nolint
    uiOutput("regions_selector"),
    uiOutput("warn_text"),

    tags$hr(),
    tags$br(),
    downloadButton("downloadCoveragemean", "Cov Mean vše", class = "btn-lg btn-primary"), # nolint
    downloadButton("downloadCNVMmean", "CNV Mean muži", class = "btn-lg btn-primary"), # nolint
    downloadButton("downloadCNVZmean", "CNV Mean ženy", class = "btn-lg btn-primary"), # nolint
    tags$br(),
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

  # div(
  #   class = "card-container",
  #   card(
  #     class = "resizable-card",
  #     uiOutput("info_panel"),
  #     uiOutput("panel_karta"),
  #   )
  # ),
  # div(class = "resize-handle"),

  div(
    # class = "card-container",
  #   card(
      class = "resize-vertical-card",
      uiOutput("info_panel"),
      uiOutput("panel_karta")
    # )
  ),
  # div(class = "resize-handle"),

  # div(
  #   class = "card-container",
  #   card(
  #     class = "resizable-card",
      navset_card_tab(
        nav_panel("Coverage  Mean ALL", DT::dataTableOutput("coverage_table")), # nolint
        nav_panel("CNV Muži Mean", DT::dataTableOutput("cnv_m")), # nolint
        nav_panel("CNV Ženy Mean", DT::dataTableOutput("cnv_z")), # nolint
        nav_spacer(),
        nav_item(align = c("right"), input_switch("switch_on", label = "Filtr", value = FALSE, width = NULL)) # nolint
        # nav_panel()
        #nav_panel("Coverage Procenta ALL", DT::dataTableOutput("coverage_table_proc")), # nolint
        #nav_panel("CNV Muži Procenta", DT::dataTableOutput("cnv_m_proc")), # nolint
        #nav_panel("CNV Ženy Procenta", DT::dataTableOutput("cnv_z_proc")) # nolint
      )
  #   )
  # ),
  # div(class = "resize-handle")
)

######################################################################################################################## # nolint

# Server logic
server <- function(input, output, session) {
  output$info_panel <- renderUI({
    if (is.null(input$file)) {
      card(
        tags$div(
          class = "text-left",
          style = "margin-bottom: 50px;",
          tags$p("Nejprve nahrajte soubory s příponou 'coveragefin.txt'."), # nolint
          tags$p("Po té zvolte pohlaví a potvrďte tlačítkem Zpracovat. "), # nolint
          tags$p("Po zpracování se zobrazí výsledky v jednotlivých záložkách."), # nolint
          tags$p("V případě problémů s nahráváním souborů zkontrolujte, zda jsou ve správném formátu a nepřesahují velikost 30 MB."), # nolint
          tags$p("Pro vyfiltrování požadovaných genů vyplňte pole pro filtr na bočním panelu a klikněte na 'Použít filtr'. Tlačítkem Obnovit vše filtr smažete a vrátíte tabullky do původního stavu. V poli pro filtr se automaticky nabízejí geny ze sloupce 'name' z nahraných souborů. ") # nolint
        )
      )
    } else {
      NULL
    }
  })

  sample_id <- reactive({
    req(input$file)
    gsub(".coveragefin\\.txt$", "", input$file$name)
  })

  output$panel_karta <- renderUI({
    req(input$file)
    div(class = "info-gender-card",
      tags$h5("Zadejte pohlaví pro každý vzorek:"), # nolint
      uiOutput("gender_input"),
      uiOutput("action_button")
    )
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

  # Reactive values to store data and processing status
  final_data <- reactiveVal()
  final_data_proc <- reactiveVal()
  final_data_proc_original <- reactiveVal()
  final_data_original <- reactiveVal()
  pohlavi_data <- reactiveVal()
  cnv_m_data <- reactiveVal()
  cnv_z_data <- reactiveVal()
  cnv_m_data_proc <- reactiveVal()
  cnv_z_data_proc <- reactiveVal()
  cnv_m_data_original <- reactiveVal()
  cnv_z_data_original <- reactiveVal()
  submit_status <- reactiveVal("ready")
  regions_data <- reactiveVal(NULL)

  regions <- reactive({
    selected_regions <- Filter(function(x) "x" != "", input$regions)
    if (length(selected_regions) == 0) {
      return(NULL)
    } else {
      return(selected_regions)
    }
  })

  # show a green submit button 'Zpracovat' after file upload
  output$action_button <- renderUI({
    req(input$file)
    div(class = "submit-task-button",
      input_task_button(
        "submit",
        label = "Zpracovat",
        submit_status()
      )
      # actionButton(
      #   inputId = "submit",
      #   label = if (submit_status() == "processing") "Zpracovávám..." else "Zpracovat", # nolint
      #   icon = if (submit_status() != "processing") icon("check") else NULL,
      #   class = if (submit_status() == "processing") "btn btn-primary" else "btn btn-success", # nolint
      #   style = "width: 200px; font-size: 20px; padding: 10px;",
      #   disabled = submit_status() == "processing"
      # )
    )
    # "submit",
    # label = if (submit_status() == "processing") "Zpracovávám..." else "Zpracovat", # nolint
    # icon = if (submit_status() != "processing") icon("check") else NULL,
    # class = if (submit_status() == "processing") "btn btn-primary" else "btn btn-success", # nolint
    # style = "margin-top: 5px; width: 200px; font-size: 20px; padding: 10px;",
    # disabled = submit_status() == "processing"
  })

  # filter genes - buttons, choices
  output$regions_selector <- renderUI({
    div(class = "gene-filter-block",
      #if (is.null(input$file) || is.null(regions_data())) {
      if (is.null(input$file)) {
        tagList(
          p("Zadejte geny pro filtraci:"),
          input_task_button(
            "submit_filtr",
            label = "Použít filtr",
            disabled = TRUE,
            style = "pointer-events: none; opacity: 0.5; border-radius: 8px 8px 0px 0px;"
          ),
          selectizeInput(
            inputId = "regions",
            label = NULL,
            choices = character(0),
            selected = filter_regions,
            width = "100%",
            multiple = TRUE,
            options = list(
              create = TRUE,
              delimiter = " ",
              persist = FALSE
            )
          ),
          input_task_button(
            "reset_filtr",
            label = "Obnovit vše",
            style = "pointer-events: none; opacity: 0.5; border-radius: 8px 8px 8px 8px;"
          ),
          helpText("Uvedené geny budou vybrány do analýzy. Pokud výběr necháte prázdný, budou zahrnuty všechny oblasti.") # nolint
        )
      } else {
        # after files are loaded
        all_genes <- unique(c(filter_regions, regions_data()))
        tagList(
          p("Zadejte geny pro filtraci:"),
          input_task_button(
            "submit_filtr",
            label = "Použít filtr",
            style = "border-radius: 8px 8px 0px 0px;"
          ),
          selectizeInput(
            inputId = "regions",
            label = NULL,
            choices = all_genes,
            selected = filter_regions,
            width = "100%",
            multiple = TRUE
          ),
          input_task_button(
            "reset_filtr",
            label = "Obnovit vše",
            style = "border-radius: 8px 8px 8px 8px;"
          ),
          helpText("Uvedené geny budou vybrány do analýzy. Pokud výběr necháte prázdný, budou zahrnuty všechny oblasti.") # nolint
        )
      }
    )
  })

  # after upload - extract unique gene names from the 4th column of each file
  observeEvent(input$file, {
    withProgress(message = "Načítám seznam oblastí...", value = 0, {
      incProgress(0.2, detail = "Čtení souborů...")

      dfs <- lapply(input$file$datapath, function(path) {
        read.delim(path, check.names = FALSE)
      })
      gene_names <- unique(unlist(lapply(dfs, function(df) {
        df[[4]]
      })))
      cat("---gene_names---\n")
      print(head(gene_names, 5))

      incProgress(0.9, detail = "Dokončuji...")
      regions_data(gene_names)
    })
  })

  # when the submit button is clicked, process the data
  observeEvent(input$submit, {
    req(input$file)
    submit_status("processing")

    withProgress(message = "Zpracování CNV...", value = 0, {
      submit_status("processing")

      # Step 1: Load coverage data from all files
      incProgress(0.1, detail = "Načítání souborů...")

      file_list <- input$file$datapath
      filenames <- input$file$name
      ids <- sample_id()

      # get gender information for each sample
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

      # Step 2: Extract MEAN column from each input file
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

      # filter out regions selected in the UI
      selected_names <- regions()
      if (!is.null(regions()) && length(regions()) > 0) {
        pattern <- paste0("\\b(", paste(selected_names, collapse = "|"), ")[A-Za-z0-9_]*\\b") # nolint
        combined <- combined[!grepl(pattern, combined$name), ]
      }

      final_data(combined)
      final_data_original(combined)
      final_data_proc(combined_proc)
      final_data_proc_original(combined_proc)

      # Step 3: CNV detection
      incProgress(0.6, detail = "Normalizace CNV M...")

      # CNV logic
      coverage <- final_data_original()
      coverage_proc <- final_data_proc_original()
      pohlavi <- pohlavi_data()
      #row_id <- seq.int(nrow(coverage)) # nolint
      m <- colnames(coverage)[grepl("^M_", colnames(coverage))]
      z <- colnames(coverage)[grepl("^Z_", colnames(coverage))]
      m_p <- colnames(coverage_proc)[grepl("^M_", colnames(coverage_proc))]
      z_p <- colnames(coverage_proc)[grepl("^Z_", colnames(coverage_proc))]
      omimgeny <- load_omim_file()
      write.csv(m, "../data_output/m.csv", row.names = FALSE) # nolint
      write.csv(omimgeny, "../data_output/omimgeny.csv", row.names = FALSE) # nolint

      # CNV for males
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
        # write.csv(greater_m, "../data_output/greater_m.csv", row.names = FALSE) # nolint)
        cnv_m_data_original(greater_m)

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

      # CNV for females
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
        cnv_z_data_original(greater_z)
        # write.csv(greater_z, "../data_output/greater_z.csv", row.names = FALSE) # nolint

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

    # mark processing as done
    submit_status("ready")
  })

  # filter

  # when filter button is clicked, update the tables based on selected regions
  observeEvent(input$submit_filtr, {
    req(final_data_original())

    selected_names <- regions()
    selected_names <- trimws(as.character(selected_names))
    selected_names <- selected_names[selected_names != ""]

    if (length(selected_names) == 0) {
      final_data(final_data_original())

      if (!is.null(cnv_m_data_original())) {
        cnv_m_data(cnv_m_data_original())
      }
      if (!is.null(cnv_z_data_original())) {
        cnv_z_data(cnv_z_data_original())
      }

      showNotification("Nejsou vybrány žádné geny", type = "warning")

      bslib::update_switch("switch_on", value = FALSE, session = session)

      cat("No genes selected\n")

      return(NULL)
    }

    pattern <- paste0("\\b(", paste(selected_names, collapse = "|"), ")\\b")

    filtered_data <- final_data_original() %>%
      dplyr::filter(stringr::str_detect(name, pattern))

    final_data(filtered_data)

    if (!is.null(cnv_m_data_original())) {
      filtered_cnv_m <- cnv_m_data_original() %>%
        dplyr::filter(stringr::str_detect(name, pattern))
      cnv_m_data(filtered_cnv_m)
    }

    if (!is.null(cnv_z_data_original())) {
      filtered_cnv_z <- cnv_z_data_original() %>%
        dplyr::filter(stringr::str_detect(name, pattern))
      cnv_z_data(filtered_cnv_z)
    }

    bslib::update_switch("switch_on", value = TRUE, session = session)

    cat("---genes for filter--- \n")
    print(head(filtered_data, 10))
  })

  # } else {
  #   final_data(final_data_original())
  #   if (!is.null(cnv_m_data_original())) {
  #     cnv_m_data(cnv_m_data_original())
  #   }

  #   if (!is.null(cnv_z_data_original())) {
  #     cnv_z_data(cnv_z_data_original())
  #   }
  #   bslib::update_switch("switch_on", value = FALSE, session = session)

  #   cat("---genes for filter--- \n")
  #   print(filtered_data)
  # })

  # when reset button is clicked, reset the tables to original data
  observeEvent(input$reset_filtr, {
    req(final_data_original())

    final_data(final_data_original())
    if (!is.null(cnv_m_data_original())) {
      cnv_m_data(cnv_m_data_original())
    }
    if (!is.null(cnv_z_data_original())) {
      cnv_z_data(cnv_z_data_original())
    }

    updateSelectizeInput(session, "selected_names", selected = character(0))
    bslib::update_switch("switch_on", value = FALSE, session = session)

    cat("---genes for filter--- \n")
    print("no genes for filter \n")
  })

  # compare with genes from hg19 and hg38
  # df_unique_genes <- lapply(input$file$datapath, function(path) {
  #   read.delim(path, check.names = FALSE)
  # })

  output$warn_text <- renderUI({
    selected_genes <- regions()
    req(selected_genes)

    df_unique_genes <- read.delim(
      "../unique_hg19_hg38.txt",
      header = FALSE,
      stringsAsFactors = FALSE
    )

    col1 <- trimws(as.character(df_unique_genes[[1]]))
    col2 <- trimws(as.character(selected_genes))

    cat("---col1--- \n")
    print(head(col1, 5))
    cat("---col2--- \n")
    print(head(col2, 5))

    notInHG <- setdiff(col2, col1)

    cat("---notInHG--- \n")
    print(head(notInHG, 5))


    if (length(col2) == 0) {
      return(NULL)
    }

    if (length(notInHG) > 0) {
      div(
        style = "color: #A03E5B;",
        paste(
          "Tyto geny nejsou v referenčním seznamu hg37 a hg38:",
          paste(sort(notInHG), collapse = ", ")
        )
      )
    } else {
      div(
        style = "color: #008871;",
        "Všechny zadané geny jsou v referenčním seznamu hg37 a hg38."
      )
    }
  })

  # Tables
  # the main coverage table - Renders the combined mean coverage data in a scrollable DataTable  # nolint
  # with pagination and horizontal/vertical scrolling.
  output$coverage_table <- DT::renderDataTable({
    req(final_data())
    df <- final_data()
    validate(need(nrow(df) > 0, "Žádná data pro pokrytí"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )
    )
  })

  # CNV results for males
  # Shows normalized coverage deviations for male samples, with annotation from OMIM. # nolint
  output$cnv_m <- DT::renderDataTable({
    req(cnv_m_data())
    df <- cnv_m_data()
    validate(need(nrow(df) > 0, "Žádná data pro CNV M"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        columnDefs = list(
          list(
            targets = which(colnames(df) == "OMIM"),
            width = "800px"
          )
        )
      ),
      escape = FALSE
    ) %>%
      DT::formatStyle(
        "OMIM",
        `white-space` = "normal"
      )
  })

  # CNV results for females
  # Shows normalized coverage deviations for female samples, with annotation from OMIM. # nolint
  output$cnv_z <- DT::renderDataTable({
    req(cnv_z_data())
    df <- cnv_z_data()
    validate(need(nrow(df) > 0, "Žádná data pro CNV Z"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = which(colnames(df) == "OMIM"),
            width = "800px"
          )
        )
      ),
      escape = FALSE
    ) %>%
      DT::formatStyle(
        "OMIM",
        `white-space` = "normal"
      )
  })

  # coverage data in percentages
  # This table displays percent coverage values before normalization.
  output$coverage_table_proc <- DT::renderDataTable({
    req(final_data_proc_original())
    df <- final_data_proc_original()
    validate(need(nrow(df) > 0, "Žádná data pro procenta pokrytí"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  # raw percentage values for CNV M
  # Displays coverage percentage for male samples without normalization.
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

  # raw percentage values for CNV Z
  # Displays coverage percentage for female samples without normalization.
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
    #filename = function() { "coveragemeanALL.csv" },
    filename = function() {
      paste0("coveragecoveragemeanALL_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv2(final_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )
  output$downloadCNVMmean <- downloadHandler(
    #filename = function() { "CNV_M_mean.csv" },
    filename = function() { 
      paste0("CNV_M_mean_", format(Sys.time(), "%Y%m%d"), ".csv") 
    },
    content = function(file) {
      write.csv2(cnv_m_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )
  output$downloadCNVZmean <- downloadHandler(
    #filename = function() { "CNV_Z_mean.csv" },
    filename = function() { 
      paste0("CNV_Z_mean_", format(Sys.time(), "%Y%m%d"), ".csv") 
    },
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
