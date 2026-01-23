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
    # Choose input method
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("database"), " Data Input Method"),
          shiny::radioButtons(
            ns("input_method"),
            "How would you like to add faculty?",
            choices = c(
              "Upload a file (CSV/Excel)" = "file",
              "Search and add manually" = "manual",
              "Import previously exported roster" = "reimport"
            ),
            selected = "file",
            inline = TRUE
          )
        )
      )
    ),

    # File Upload Section
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'file'", ns("input_method")),
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
      )
    ),

    # Manual Entry Section
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'manual'", ns("input_method")),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h4(shiny::icon("search"), " Search for Faculty"),
            shiny::textInput(
              ns("manual_search_name"),
              "Faculty Name",
              placeholder = "Enter name to search (e.g., John Smith)"
            ),
            shiny::textInput(
              ns("manual_search_institution"),
              "Institution (optional)",
              placeholder = "Filter by institution"
            ),
            shiny::actionButton(
              ns("manual_search_btn"),
              "Search OpenAlex",
              icon = shiny::icon("search"),
              class = "btn-primary"
            ),
            shiny::hr(),
            shiny::h5("Search Results"),
            DT::dataTableOutput(ns("manual_search_results")),
            shiny::br(),
            shiny::uiOutput(ns("manual_add_controls"))
          )
        ),
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h4(shiny::icon("user-plus"), " Add Faculty Details"),
            shiny::uiOutput(ns("manual_entry_form")),
            shiny::hr(),
            shiny::h5("Current Roster"),
            shiny::p(
              shiny::textOutput(ns("manual_roster_count"), inline = TRUE),
              " faculty member(s) added"
            ),
            DT::dataTableOutput(ns("manual_roster_preview")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::actionButton(
                  ns("manual_clear_roster"),
                  "Clear All",
                  icon = shiny::icon("trash"),
                  class = "btn-warning"
                )
              ),
              shiny::column(
                width = 6,
                shiny::actionButton(
                  ns("manual_proceed_btn"),
                  "Proceed to Resolution",
                  icon = shiny::icon("arrow-right"),
                  class = "btn-success"
                )
              )
            )
          )
        )
      )
    ),

    # Reimport Section
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'reimport'", ns("input_method")),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h4(shiny::icon("upload"), " Import Previous Roster"),
            shiny::fileInput(
              ns("reimport_file"),
              "Choose exported roster CSV",
              accept = c(".csv", "text/csv")
            ),
            shiny::helpText(
              "Import a roster that was previously exported from FacultyIQ.",
              shiny::br(),
              "This preserves all resolved IDs and settings."
            )
          )
        ),
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h4(shiny::icon("info-circle"), " About Reimport"),
            shiny::p(
              "Reimporting a roster allows you to:",
              shiny::tags$ul(
                shiny::tags$li("Resume work on a previous analysis"),
                shiny::tags$li("Share roster data between sessions"),
                shiny::tags$li("Skip re-resolving faculty IDs")
              )
            ),
            shiny::p(
              shiny::strong("Tip: "),
              "Export your roster from the 'Data Export' tab after resolving IDs."
            )
          )
        )
      )
    ),

    shiny::hr(),

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
      ready = FALSE,
      manual_roster = NULL,  # For manual entry mode
      search_results = NULL  # Search results for manual entry
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

    # =========================================================================
    # Manual Entry Handlers
    # =========================================================================

    # Handle manual search
    shiny::observeEvent(input$manual_search_btn, {
      req(input$manual_search_name)

      name <- trimws(input$manual_search_name)
      if (nchar(name) < 2) {
        shiny::showNotification("Please enter at least 2 characters", type = "warning")
        return()
      }

      shiny::withProgress(message = "Searching OpenAlex...", {
        institution <- if (!is.null(input$manual_search_institution) &&
                          nchar(trimws(input$manual_search_institution)) > 0) {
          trimws(input$manual_search_institution)
        } else {
          NULL
        }

        results <- search_openalex_authors(name, institution = institution, limit = 15)

        if (is.null(results) || nrow(results) == 0) {
          shiny::showNotification("No results found. Try a different name.", type = "warning")
          rv$search_results <- NULL
        } else {
          rv$search_results <- results
          shiny::showNotification(
            sprintf("Found %d matching author(s)", nrow(results)),
            type = "message"
          )
        }
      })
    })

    # Display search results
    output$manual_search_results <- DT::renderDataTable({
      req(rv$search_results)

      display_df <- rv$search_results %>%
        dplyr::mutate(
          works_count = format(works_count, big.mark = ","),
          cited_by_count = format(cited_by_count, big.mark = ","),
          affiliation = ifelse(is.na(affiliation), "-", stringr::str_trunc(affiliation, 40))
        ) %>%
        dplyr::select(display_name, affiliation, works_count, cited_by_count)

      DT::datatable(
        display_df,
        selection = "single",
        options = list(
          pageLength = 5,
          dom = "tp",
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Name", "Institution", "Works", "Citations")
      )
    })

    # Show add controls when a result is selected
    output$manual_add_controls <- shiny::renderUI({
      selected <- input$manual_search_results_rows_selected

      if (is.null(selected) || length(selected) == 0) {
        return(shiny::helpText("Click a row above to select an author to add"))
      }

      selected_author <- rv$search_results[selected, ]

      shiny::tagList(
        shiny::div(
          class = "alert alert-info",
          shiny::strong("Selected: "), selected_author$display_name,
          shiny::br(),
          shiny::em(selected_author$affiliation)
        ),
        shiny::actionButton(
          ns("manual_add_selected"),
          "Add to Roster",
          icon = shiny::icon("plus"),
          class = "btn-success"
        )
      )
    })

    # Manual entry form (for adding details to selected author)
    output$manual_entry_form <- shiny::renderUI({
      selected <- input$manual_search_results_rows_selected

      if (is.null(selected) || length(selected) == 0) {
        return(shiny::tagList(
          shiny::helpText("Select an author from search results, or add manually:"),
          shiny::hr(),
          shiny::textInput(ns("manual_name"), "Name *", placeholder = "Full name"),
          shiny::textInput(ns("manual_email"), "Email", placeholder = "email@university.edu"),
          shiny::selectInput(
            ns("manual_rank"),
            "Academic Rank",
            choices = c(
              "" = "",
              "Instructor" = "Instructor",
              "Assistant Professor" = "Assistant Professor",
              "Associate Professor" = "Associate Professor",
              "Full Professor" = "Full Professor",
              "Research Faculty" = "Research Faculty",
              "Clinical Faculty" = "Clinical Faculty",
              "Adjunct" = "Adjunct",
              "Emeritus" = "Emeritus"
            )
          ),
          shiny::textInput(ns("manual_scopus_id"), "Scopus ID", placeholder = "e.g., 7004567890123"),
          shiny::textInput(ns("manual_scholar_id"), "Google Scholar ID", placeholder = "e.g., ABCD1234efgh"),
          shiny::actionButton(
            ns("manual_add_custom"),
            "Add to Roster",
            icon = shiny::icon("user-plus"),
            class = "btn-primary"
          )
        ))
      }

      selected_author <- rv$search_results[selected, ]

      shiny::tagList(
        shiny::helpText("Add details for selected author:"),
        shiny::textInput(
          ns("manual_email_selected"),
          "Email",
          placeholder = "email@university.edu"
        ),
        shiny::selectInput(
          ns("manual_rank_selected"),
          "Academic Rank",
          choices = c(
            "" = "",
            "Instructor" = "Instructor",
            "Assistant Professor" = "Assistant Professor",
            "Associate Professor" = "Associate Professor",
            "Full Professor" = "Full Professor",
            "Research Faculty" = "Research Faculty",
            "Clinical Faculty" = "Clinical Faculty",
            "Adjunct" = "Adjunct",
            "Emeritus" = "Emeritus"
          )
        )
      )
    })

    # Add selected author from search results
    shiny::observeEvent(input$manual_add_selected, {
      selected <- input$manual_search_results_rows_selected
      req(selected)

      selected_author <- rv$search_results[selected, ]

      # Create new roster row
      new_row <- data.frame(
        id = if (is.null(rv$manual_roster)) 1L else max(rv$manual_roster$id) + 1L,
        name = selected_author$display_name,
        email = input$manual_email_selected %||% NA_character_,
        academic_rank = if (!is.null(input$manual_rank_selected) &&
                            input$manual_rank_selected != "") {
          input$manual_rank_selected
        } else {
          NA_character_
        },
        last_promotion = NA_character_,
        reaims_pubs = NA_integer_,
        scopus_id = NA_character_,
        scholar_id = NA_character_,
        associations = NA_character_,
        openalex_id = selected_author$openalex_id,
        resolution_status = "resolved",
        resolution_method = "manual_search",
        stringsAsFactors = FALSE
      )

      # Add to manual roster
      if (is.null(rv$manual_roster)) {
        rv$manual_roster <- new_row
      } else {
        rv$manual_roster <- dplyr::bind_rows(rv$manual_roster, new_row)
      }

      shiny::showNotification(
        sprintf("Added %s to roster", selected_author$display_name),
        type = "message"
      )

      # Clear selection
      rv$search_results <- NULL
    })

    # Add custom entry (without search)
    shiny::observeEvent(input$manual_add_custom, {
      name <- trimws(input$manual_name %||% "")

      if (nchar(name) < 2) {
        shiny::showNotification("Please enter a name", type = "error")
        return()
      }

      new_row <- data.frame(
        id = if (is.null(rv$manual_roster)) 1L else max(rv$manual_roster$id) + 1L,
        name = name,
        email = input$manual_email %||% NA_character_,
        academic_rank = if (!is.null(input$manual_rank) && input$manual_rank != "") {
          input$manual_rank
        } else {
          NA_character_
        },
        last_promotion = NA_character_,
        reaims_pubs = NA_integer_,
        scopus_id = input$manual_scopus_id %||% NA_character_,
        scholar_id = input$manual_scholar_id %||% NA_character_,
        associations = NA_character_,
        openalex_id = NA_character_,
        resolution_status = "pending",
        resolution_method = "manual_entry",
        stringsAsFactors = FALSE
      )

      if (is.null(rv$manual_roster)) {
        rv$manual_roster <- new_row
      } else {
        rv$manual_roster <- dplyr::bind_rows(rv$manual_roster, new_row)
      }

      shiny::showNotification(sprintf("Added %s to roster", name), type = "message")

      # Clear form
      shiny::updateTextInput(session, "manual_name", value = "")
      shiny::updateTextInput(session, "manual_email", value = "")
      shiny::updateSelectInput(session, "manual_rank", selected = "")
      shiny::updateTextInput(session, "manual_scopus_id", value = "")
      shiny::updateTextInput(session, "manual_scholar_id", value = "")
    })

    # Manual roster count
    output$manual_roster_count <- shiny::renderText({
      if (is.null(rv$manual_roster)) 0 else nrow(rv$manual_roster)
    })

    # Manual roster preview
    output$manual_roster_preview <- DT::renderDataTable({
      req(rv$manual_roster)

      display_df <- rv$manual_roster %>%
        dplyr::select(id, name, academic_rank, resolution_status)

      DT::datatable(
        display_df,
        selection = "single",
        options = list(
          pageLength = 5,
          dom = "tp"
        ),
        rownames = FALSE,
        colnames = c("ID", "Name", "Rank", "Status")
      )
    })

    # Clear manual roster
    shiny::observeEvent(input$manual_clear_roster, {
      rv$manual_roster <- NULL
      shiny::showNotification("Roster cleared", type = "message")
    })

    # Proceed from manual entry
    shiny::observeEvent(input$manual_proceed_btn, {
      req(rv$manual_roster)

      if (nrow(rv$manual_roster) == 0) {
        shiny::showNotification("Please add at least one faculty member", type = "warning")
        return()
      }

      rv$roster <- rv$manual_roster
      rv$ready <- TRUE
      rv$proceed <- TRUE

      shiny::showNotification(
        sprintf("Proceeding with %d faculty member(s)", nrow(rv$roster)),
        type = "message"
      )
    })

    # =========================================================================
    # Reimport Handlers
    # =========================================================================

    # Handle reimport file upload
    shiny::observeEvent(input$reimport_file, {
      req(input$reimport_file)

      tryCatch({
        file_path <- input$reimport_file$datapath

        data <- readr::read_csv(file_path, show_col_types = FALSE)
        data <- as.data.frame(data)

        # Check for required columns from export format
        required_cols <- c("id", "name")
        missing <- setdiff(required_cols, names(data))

        if (length(missing) > 0) {
          shiny::showNotification(
            paste("Missing required columns:", paste(missing, collapse = ", ")),
            type = "error"
          )
          return()
        }

        # Ensure standard columns exist
        if (!"resolution_status" %in% names(data)) {
          data$resolution_status <- ifelse(
            !is.na(data$openalex_id) & data$openalex_id != "",
            "resolved", "pending"
          )
        }
        if (!"resolution_method" %in% names(data)) {
          data$resolution_method <- "reimport"
        }

        # Standardize column names
        col_mapping <- c(
          "id" = "id",
          "name" = "name",
          "email" = "email",
          "academic_rank" = "academic_rank",
          "last_promotion" = "last_promotion",
          "reaims_pubs" = "reaims_pubs",
          "scopus_id" = "scopus_id",
          "scholar_id" = "scholar_id",
          "openalex_id" = "openalex_id",
          "associations" = "associations",
          "resolution_status" = "resolution_status",
          "resolution_method" = "resolution_method"
        )

        # Ensure all expected columns exist
        for (col in names(col_mapping)) {
          if (!col %in% names(data)) {
            data[[col]] <- NA
          }
        }

        rv$roster <- data[, names(col_mapping)]
        rv$ready <- TRUE

        shiny::showNotification(
          sprintf("Imported %d faculty members", nrow(rv$roster)),
          type = "message"
        )

      }, error = function(e) {
        shiny::showNotification(
          paste("Error importing file:", e$message),
          type = "error"
        )
      })
    })

    # Return reactive values
    return(rv)
  })
}

# Helper for NULL coalescing
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || x == "") y else x
