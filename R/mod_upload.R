# =============================================================================
# FacultyIQ - Upload Module
# =============================================================================
# Handles roster file upload, validation, and preview

#' Upload Module UI
#'
#' @param id Module namespace ID
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Upload Roster File"),
          shiny::fileInput(
            ns("roster_file"),
            "Choose CSV or Excel file",
            accept = c(
              ".csv",
              ".xlsx",
              ".xls",
              "text/csv",
              "text/comma-separated-values",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              "application/vnd.ms-excel"
            )
          ),
          shiny::helpText(
            "Accepted formats: CSV, Excel (.xlsx, .xls)",
            shiny::br(),
            "Expected columns: Name, Email, Academic Rank, Scopus ID, Scholar ID",
            shiny::br(),
            "See 'Expected Format' panel for details."
          ),
          shiny::hr(),
          shiny::h5("Or upload bibliographic file (optional)"),
          shiny::fileInput(
            ns("biblio_file"),
            "Choose bibliographic export",
            accept = c(
              ".bib",
              ".ris",
              ".txt",
              ".csv"
            )
          ),
          shiny::helpText("Scopus, Web of Science, or PubMed exports")
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Expected Format"),
          shiny::p("Your roster file should contain some or all of these columns:"),
          DT::dataTableOutput(ns("expected_columns")),
          shiny::br(),
          shiny::downloadButton(ns("download_template"), "Download Template CSV")
        )
      )
    ),

    shiny::hr(),

    # Validation results
    shiny::uiOutput(ns("validation_ui")),

    # Data preview
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] != null", ns("data_ready")),
      ns = ns,
      shiny::wellPanel(
        shiny::h4("Data Preview"),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            DT::dataTableOutput(ns("data_preview"))
          )
        )
      ),
      shiny::wellPanel(
        shiny::h4("Data Completeness"),
        DT::dataTableOutput(ns("completeness_table")),
        shiny::br(),
        shiny::actionButton(
          ns("proceed_btn"),
          "Proceed to Identity Resolution",
          class = "btn-primary btn-lg"
        )
      )
    )
  )
}

#' Upload Module Server
#'
#' @param id Module namespace ID
#' @return Reactive values with roster data
mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store data
    rv <- shiny::reactiveValues(
      roster = NULL,
      validation = NULL,
      biblio_data = NULL,
      ready = FALSE
    )

    # Expected columns table
    output$expected_columns <- DT::renderDataTable({
      expected <- data.frame(
        Column = c(
          "Name",
          "Email",
          "Academic Rank",
          "Last Promotion Date",
          "REAIMS Publications",
          "Scopus ID",
          "Google Scholar ID",
          "Associations"
        ),
        Required = c(
          "Yes",
          "Recommended",
          "Recommended",
          "Optional",
          "Optional",
          "Recommended",
          "Recommended",
          "Optional"
        ),
        Description = c(
          "Full name of faculty member",
          "Email address (kept private)",
          "Current rank (e.g., Assistant Professor)",
          "Year or date of last promotion",
          "Self-reported publication count",
          "Scopus Author ID (numeric)",
          "Google Scholar profile ID",
          "Professional associations and roles"
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        expected,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE
      )
    })

    # Download template
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        "facultyiq_roster_template.csv"
      },
      content = function(file) {
        template <- data.frame(
          Name = c("Jane Smith", "John Doe"),
          Email = c("jsmith@university.edu", "jdoe@university.edu"),
          `Academic Rank` = c("Associate Professor", "Assistant Professor"),
          `Last Promotion Date` = c("2020", "2022"),
          `REAIMS Publications` = c(45, 12),
          `Scopus ID` = c("12345678901", "98765432101"),
          `Google Scholar ID` = c("ABC123def45", "XYZ789ghi01"),
          Associations = c("IDSA member", "ACOEM board"),
          check.names = FALSE
        )
        utils::write.csv(template, file, row.names = FALSE)
      }
    )

    # Handle roster file upload
    shiny::observeEvent(input$roster_file, {
      req(input$roster_file)

      tryCatch({
        file_path <- input$roster_file$datapath
        file_ext <- tools::file_ext(input$roster_file$name)

        # Read file based on extension
        if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else if (file_ext == "csv") {
          data <- readr::read_csv(file_path, show_col_types = FALSE)
        } else {
          shiny::showNotification(
            "Unsupported file format. Please use CSV or Excel.",
            type = "error"
          )
          return()
        }

        # Convert to data frame
        data <- as.data.frame(data)

        # Validate
        validation <- validate_roster(data)
        rv$validation <- validation

        if (validation$valid) {
          # Clean the data
          rv$roster <- clean_roster(data)
          rv$ready <- TRUE

          shiny::showNotification(
            sprintf("Loaded %d rows successfully!", nrow(rv$roster)),
            type = "message"
          )
        } else {
          shiny::showNotification(
            paste("Validation errors:", paste(validation$errors, collapse = "; ")),
            type = "error",
            duration = 10
          )
        }

      }, error = function(e) {
        shiny::showNotification(
          paste("Error reading file:", e$message),
          type = "error"
        )
        rv$roster <- NULL
        rv$validation <- NULL
        rv$ready <- FALSE
      })
    })

    # Handle bibliographic file upload
    shiny::observeEvent(input$biblio_file, {
      req(input$biblio_file)

      tryCatch({
        file_path <- input$biblio_file$datapath

        # Use bibliometrix to parse
        biblio_data <- parse_bibliographic_file(file_path)

        if (!is.null(biblio_data) && nrow(biblio_data) > 0) {
          rv$biblio_data <- biblio_data
          shiny::showNotification(
            sprintf("Loaded %d bibliographic records", nrow(biblio_data)),
            type = "message"
          )
        } else {
          shiny::showNotification(
            "Could not parse bibliographic file. Try a different format.",
            type = "warning"
          )
        }

      }, error = function(e) {
        shiny::showNotification(
          paste("Error parsing bibliographic file:", e$message),
          type = "warning"
        )
      })
    })

    # Validation UI
    output$validation_ui <- shiny::renderUI({
      req(rv$validation)
      val <- rv$validation

      shiny::wellPanel(
        shiny::h4("Validation Results"),

        # Status badge
        if (val$valid) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            sprintf(" Valid file: %d rows, %d columns", val$n_rows, val$n_cols)
          )
        } else {
          shiny::div(
            class = "alert alert-danger",
            shiny::icon("exclamation-triangle"),
            " Validation failed"
          )
        },

        # Errors
        if (length(val$errors) > 0) {
          shiny::div(
            class = "alert alert-danger",
            shiny::h5("Errors:"),
            shiny::tags$ul(
              lapply(val$errors, function(e) shiny::tags$li(e))
            )
          )
        },

        # Warnings
        if (length(val$warnings) > 0) {
          shiny::div(
            class = "alert alert-warning",
            shiny::h5("Warnings:"),
            shiny::tags$ul(
              lapply(val$warnings, function(w) shiny::tags$li(w))
            )
          )
        },

        # Info
        if (length(val$info) > 0) {
          shiny::div(
            class = "alert alert-info",
            shiny::h5("Information:"),
            shiny::tags$ul(
              lapply(val$info, function(i) shiny::tags$li(i))
            )
          )
        },

        # Column mapping info
        if (!is.null(val$original_names)) {
          shiny::div(
            shiny::h5("Column Mapping:"),
            shiny::helpText("Original column names were normalized to standard format"),
            shiny::verbatimTextOutput(ns("column_mapping"))
          )
        }
      )
    })

    # Column mapping display
    output$column_mapping <- shiny::renderPrint({
      req(rv$validation)
      val <- rv$validation

      mapping_df <- data.frame(
        Original = val$original_names,
        Normalized = val$normalized_names,
        stringsAsFactors = FALSE
      )

      # Only show rows where names changed
      changed <- mapping_df$Original != mapping_df$Normalized
      if (any(changed)) {
        mapping_df[changed, ]
      } else {
        "All column names matched expected format"
      }
    })

    # Data preview
    output$data_preview <- DT::renderDataTable({
      req(rv$roster)

      # Select key columns for preview
      preview_cols <- c("id", "name", "email", "academic_rank",
                        "scopus_id", "scholar_id", "reaims_pubs")
      preview_cols <- intersect(preview_cols, names(rv$roster))

      DT::datatable(
        rv$roster[, preview_cols],
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # Completeness table
    output$completeness_table <- DT::renderDataTable({
      req(rv$roster)

      completeness <- calculate_completeness_summary(rv$roster)

      DT::datatable(
        completeness,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Percent Complete",
          background = DT::styleColorBar(c(0, 100), "steelblue"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    # Output for conditional panel
    output$data_ready <- shiny::reactive({
      if (rv$ready) "ready" else NULL
    })
    shiny::outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

    # Proceed button
    shiny::observeEvent(input$proceed_btn, {
      # Signal to parent that we're ready to proceed
      rv$proceed <- TRUE
    })

    # Return reactive values
    return(rv)
  })
}
