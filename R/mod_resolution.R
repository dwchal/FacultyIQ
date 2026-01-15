# =============================================================================
# FacultyIQ - Identity Resolution Module
# =============================================================================
# Handles author identity resolution using OpenAlex and other sources
# Supports automatic resolution via Scopus ID and interactive search

#' Resolution Module UI
#'
#' @param id Module namespace ID
mod_resolution_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4("Identity Resolution Status"),
          shiny::p(
            "This step maps roster entries to unique author identifiers in OpenAlex.",
            "Records with Scopus IDs can be resolved automatically.",
            "Others require manual selection from search results."
          ),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::valueBoxOutput(ns("vb_total"), width = 12)
            ),
            shiny::column(
              width = 3,
              shiny::valueBoxOutput(ns("vb_resolved"), width = 12)
            ),
            shiny::column(
              width = 3,
              shiny::valueBoxOutput(ns("vb_pending"), width = 12)
            ),
            shiny::column(
              width = 3,
              shiny::valueBoxOutput(ns("vb_failed"), width = 12)
            )
          )
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4("Auto-Resolution"),
          shiny::p(
            "Automatically resolve records with Scopus IDs to OpenAlex identities."
          ),
          shiny::actionButton(
            ns("auto_resolve_btn"),
            "Auto-Resolve via Scopus IDs",
            class = "btn-primary"
          ),
          shiny::span(
            shiny::textOutput(ns("auto_resolve_status")),
            style = "margin-left: 20px;"
          )
        )
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Resolution Table"),
          shiny::helpText(
            "Click a row to manually resolve or override.",
            shiny::br(),
            "Status: ",
            shiny::span("pending", style = "color: orange;"), " | ",
            shiny::span("resolved", style = "color: green;"), " | ",
            shiny::span("manual", style = "color: blue;"), " | ",
            shiny::span("failed", style = "color: red;"), " | ",
            shiny::span("skipped", style = "color: gray;")
          ),
          DT::dataTableOutput(ns("resolution_table"))
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Manual Resolution"),
          shiny::uiOutput(ns("manual_resolution_ui"))
        )
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::actionButton(
                ns("save_mappings_btn"),
                "Save Mappings",
                class = "btn-info"
              ),
              shiny::downloadButton(
                ns("export_mappings_btn"),
                "Export Mappings CSV"
              )
            ),
            shiny::column(
              width = 6,
              shiny::actionButton(
                ns("fetch_data_btn"),
                "Fetch Data for Resolved Authors",
                class = "btn-success btn-lg"
              ),
              shiny::helpText("This will query APIs for all resolved authors")
            )
          )
        )
      )
    ),

    # Progress indicator
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("fetching")),
      shiny::wellPanel(
        shiny::h4("Fetching Data..."),
        shiny::progressBar(
          id = ns("fetch_progress"),
          value = 0,
          total = 100,
          display_pct = TRUE
        ),
        shiny::verbatimTextOutput(ns("fetch_log"))
      )
    )
  )
}

#' Resolution Module Server
#'
#' @param id Module namespace ID
#' @param roster_rv Reactive values from upload module
#' @return Reactive values with resolved data
mod_resolution_server <- function(id, roster_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      resolution_df = NULL,
      selected_row = NULL,
      search_results = NULL,
      person_data = NULL,
      fetching = FALSE,
      fetch_log = character()
    )

    # Initialize resolution data frame when roster is available
    shiny::observe({
      req(roster_rv$roster)

      # Create resolution tracking data frame
      roster <- roster_rv$roster
      rv$resolution_df <- data.frame(
        id = roster$id,
        name = roster$name,
        email = roster$email,
        scopus_id = roster$scopus_id,
        scholar_id = roster$scholar_id,
        openalex_id = roster$openalex_id,
        resolution_status = roster$resolution_status,
        resolution_method = roster$resolution_method,
        openalex_name = NA_character_,
        openalex_works = NA_integer_,
        openalex_citations = NA_integer_,
        stringsAsFactors = FALSE
      )

      # Load any saved mappings
      saved_mappings <- load_identity_mappings()
      if (!is.null(saved_mappings)) {
        # Merge saved mappings
        for (i in seq_len(nrow(rv$resolution_df))) {
          saved_row <- saved_mappings[saved_mappings$name == rv$resolution_df$name[i], ]
          if (nrow(saved_row) > 0) {
            if (!is.na(saved_row$openalex_id[1]) && saved_row$openalex_id[1] != "") {
              rv$resolution_df$openalex_id[i] <- saved_row$openalex_id[1]
              rv$resolution_df$resolution_status[i] <- "resolved"
              rv$resolution_df$resolution_method[i] <- "saved"
            }
          }
        }
      }
    })

    # Value boxes
    output$vb_total <- shinydashboard::renderValueBox({
      n <- if (!is.null(rv$resolution_df)) nrow(rv$resolution_df) else 0
      shinydashboard::valueBox(
        n, "Total Records",
        icon = shiny::icon("users"),
        color = "blue"
      )
    })

    output$vb_resolved <- shinydashboard::renderValueBox({
      n <- if (!is.null(rv$resolution_df)) {
        sum(rv$resolution_df$resolution_status %in% c("resolved", "manual"), na.rm = TRUE)
      } else 0
      shinydashboard::valueBox(
        n, "Resolved",
        icon = shiny::icon("check-circle"),
        color = "green"
      )
    })

    output$vb_pending <- shinydashboard::renderValueBox({
      n <- if (!is.null(rv$resolution_df)) {
        sum(rv$resolution_df$resolution_status == "pending", na.rm = TRUE)
      } else 0
      shinydashboard::valueBox(
        n, "Pending",
        icon = shiny::icon("clock"),
        color = "yellow"
      )
    })

    output$vb_failed <- shinydashboard::renderValueBox({
      n <- if (!is.null(rv$resolution_df)) {
        sum(rv$resolution_df$resolution_status %in% c("failed", "skipped"), na.rm = TRUE)
      } else 0
      shinydashboard::valueBox(
        n, "Failed/Skipped",
        icon = shiny::icon("exclamation-triangle"),
        color = "red"
      )
    })

    # Resolution table
    output$resolution_table <- DT::renderDataTable({
      req(rv$resolution_df)

      display_df <- rv$resolution_df %>%
        dplyr::select(id, name, scopus_id, scholar_id, openalex_id,
                      resolution_status, openalex_name, openalex_works)

      DT::datatable(
        display_df,
        selection = "single",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = c(0))  # Hide id column
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "resolution_status",
          backgroundColor = DT::styleEqual(
            c("pending", "resolved", "manual", "failed", "skipped"),
            c("#fff3cd", "#d4edda", "#cce5ff", "#f8d7da", "#e2e3e5")
          )
        )
    })

    # Auto-resolve using Scopus IDs
    shiny::observeEvent(input$auto_resolve_btn, {
      req(rv$resolution_df)

      # Find rows with Scopus ID but no OpenAlex ID
      to_resolve <- which(
        !is.na(rv$resolution_df$scopus_id) &
          rv$resolution_df$scopus_id != "" &
          (is.na(rv$resolution_df$openalex_id) | rv$resolution_df$openalex_id == "") &
          rv$resolution_df$resolution_status == "pending"
      )

      if (length(to_resolve) == 0) {
        shiny::showNotification(
          "No records with Scopus IDs pending resolution",
          type = "message"
        )
        return()
      }

      shiny::withProgress(message = "Resolving via Scopus IDs...", {
        resolved_count <- 0
        failed_count <- 0

        for (i in seq_along(to_resolve)) {
          idx <- to_resolve[i]
          shiny::incProgress(1 / length(to_resolve))

          tryCatch({
            result <- get_openalex_by_scopus(rv$resolution_df$scopus_id[idx])

            if (!is.null(result)) {
              rv$resolution_df$openalex_id[idx] <- result$openalex_id
              rv$resolution_df$openalex_name[idx] <- result$display_name
              rv$resolution_df$openalex_works[idx] <- result$works_count
              rv$resolution_df$openalex_citations[idx] <- result$cited_by_count
              rv$resolution_df$resolution_status[idx] <- "resolved"
              rv$resolution_df$resolution_method[idx] <- "scopus_auto"
              resolved_count <- resolved_count + 1
            } else {
              rv$resolution_df$resolution_status[idx] <- "failed"
              rv$resolution_df$resolution_method[idx] <- "scopus_not_found"
              failed_count <- failed_count + 1
            }

            Sys.sleep(0.5)  # Rate limiting

          }, error = function(e) {
            rv$resolution_df$resolution_status[idx] <- "failed"
            failed_count <- failed_count + 1
          })
        }
      })

      shiny::showNotification(
        sprintf("Resolved: %d, Failed: %d", resolved_count, failed_count),
        type = "message"
      )
    })

    output$auto_resolve_status <- shiny::renderText({
      req(rv$resolution_df)
      n_scopus <- sum(!is.na(rv$resolution_df$scopus_id) &
                        rv$resolution_df$scopus_id != "" &
                        rv$resolution_df$resolution_status == "pending")
      sprintf("%d records with Scopus IDs pending", n_scopus)
    })

    # Track selected row
    shiny::observeEvent(input$resolution_table_rows_selected, {
      rv$selected_row <- input$resolution_table_rows_selected
      rv$search_results <- NULL  # Clear previous search
    })

    # Manual resolution UI
    output$manual_resolution_ui <- shiny::renderUI({
      if (is.null(rv$selected_row) || is.null(rv$resolution_df)) {
        return(shiny::helpText("Select a row from the table to manually resolve"))
      }

      row <- rv$resolution_df[rv$selected_row, ]

      shiny::tagList(
        shiny::h5(sprintf("Resolving: %s", row$name)),

        # Current status
        shiny::p(
          "Status: ", shiny::strong(row$resolution_status),
          if (!is.na(row$openalex_id) && row$openalex_id != "") {
            sprintf(" (OpenAlex: %s)", row$openalex_id)
          }
        ),

        shiny::hr(),

        # Search interface
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::textInput(
              ns("search_name"),
              "Search by name",
              value = row$name
            )
          ),
          shiny::column(
            width = 4,
            shiny::textInput(
              ns("search_institution"),
              "Institution (optional)",
              value = ""
            )
          )
        ),

        shiny::actionButton(ns("search_btn"), "Search OpenAlex", class = "btn-info"),

        shiny::hr(),

        # Search results
        if (!is.null(rv$search_results) && nrow(rv$search_results) > 0) {
          shiny::tagList(
            shiny::h5("Search Results"),
            shiny::helpText("Click a row to select this author"),
            DT::dataTableOutput(ns("search_results_table")),
            shiny::br(),
            shiny::actionButton(ns("select_author_btn"), "Select Author", class = "btn-success"),
            shiny::actionButton(ns("skip_btn"), "Skip (No Match)", class = "btn-warning")
          )
        } else if (!is.null(rv$search_results)) {
          shiny::div(
            class = "alert alert-warning",
            "No results found. Try a different search term or skip this record."
          )
        },

        shiny::hr(),

        # Manual entry
        shiny::h5("Or enter OpenAlex ID directly"),
        shiny::textInput(ns("manual_openalex_id"), "OpenAlex ID"),
        shiny::actionButton(ns("manual_set_btn"), "Set ID", class = "btn-primary"),

        shiny::hr(),

        # Clear resolution
        shiny::actionButton(ns("clear_resolution_btn"), "Clear Resolution", class = "btn-danger btn-sm")
      )
    })

    # Search OpenAlex
    shiny::observeEvent(input$search_btn, {
      req(input$search_name)

      shiny::withProgress(message = "Searching OpenAlex...", {
        results <- search_openalex_authors(
          name = input$search_name,
          institution = input$search_institution,
          limit = 10
        )

        rv$search_results <- results
      })

      if (is.null(rv$search_results) || nrow(rv$search_results) == 0) {
        shiny::showNotification("No results found", type = "warning")
      }
    })

    # Search results table
    output$search_results_table <- DT::renderDataTable({
      req(rv$search_results)

      DT::datatable(
        rv$search_results,
        selection = "single",
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "t"
        ),
        rownames = FALSE
      )
    })

    # Select author from search results
    shiny::observeEvent(input$select_author_btn, {
      req(rv$selected_row, rv$search_results)
      req(input$search_results_table_rows_selected)

      selected_result <- rv$search_results[input$search_results_table_rows_selected, ]
      idx <- rv$selected_row

      rv$resolution_df$openalex_id[idx] <- selected_result$openalex_id
      rv$resolution_df$openalex_name[idx] <- selected_result$display_name
      rv$resolution_df$openalex_works[idx] <- selected_result$works_count
      rv$resolution_df$openalex_citations[idx] <- selected_result$cited_by_count
      rv$resolution_df$resolution_status[idx] <- "manual"
      rv$resolution_df$resolution_method[idx] <- "search_select"

      shiny::showNotification(
        sprintf("Resolved %s to %s", rv$resolution_df$name[idx], selected_result$display_name),
        type = "message"
      )

      rv$search_results <- NULL
    })

    # Skip record
    shiny::observeEvent(input$skip_btn, {
      req(rv$selected_row)
      idx <- rv$selected_row

      rv$resolution_df$resolution_status[idx] <- "skipped"
      rv$resolution_df$resolution_method[idx] <- "user_skip"

      shiny::showNotification("Record skipped", type = "message")
      rv$search_results <- NULL
    })

    # Manual ID entry
    shiny::observeEvent(input$manual_set_btn, {
      req(rv$selected_row, input$manual_openalex_id)
      idx <- rv$selected_row

      # Validate OpenAlex ID format
      openalex_id <- trimws(input$manual_openalex_id)

      if (!grepl("^https://openalex.org/A[0-9]+$", openalex_id) &&
          !grepl("^A[0-9]+$", openalex_id)) {
        shiny::showNotification(
          "Invalid OpenAlex ID format. Expected: A1234567890 or https://openalex.org/A1234567890",
          type = "error"
        )
        return()
      }

      # Normalize to full URL
      if (!grepl("^https://", openalex_id)) {
        openalex_id <- paste0("https://openalex.org/", openalex_id)
      }

      # Verify by fetching author data
      shiny::withProgress(message = "Verifying OpenAlex ID...", {
        author_data <- get_openalex_author(openalex_id)

        if (!is.null(author_data)) {
          rv$resolution_df$openalex_id[idx] <- openalex_id
          rv$resolution_df$openalex_name[idx] <- author_data$display_name
          rv$resolution_df$openalex_works[idx] <- author_data$works_count
          rv$resolution_df$openalex_citations[idx] <- author_data$cited_by_count
          rv$resolution_df$resolution_status[idx] <- "manual"
          rv$resolution_df$resolution_method[idx] <- "manual_entry"

          shiny::showNotification(
            sprintf("Verified: %s", author_data$display_name),
            type = "message"
          )
        } else {
          shiny::showNotification(
            "Could not verify OpenAlex ID. Author not found.",
            type = "error"
          )
        }
      })
    })

    # Clear resolution
    shiny::observeEvent(input$clear_resolution_btn, {
      req(rv$selected_row)
      idx <- rv$selected_row

      rv$resolution_df$openalex_id[idx] <- NA_character_
      rv$resolution_df$openalex_name[idx] <- NA_character_
      rv$resolution_df$openalex_works[idx] <- NA_integer_
      rv$resolution_df$openalex_citations[idx] <- NA_integer_
      rv$resolution_df$resolution_status[idx] <- "pending"
      rv$resolution_df$resolution_method[idx] <- NA_character_

      shiny::showNotification("Resolution cleared", type = "message")
    })

    # Save mappings
    shiny::observeEvent(input$save_mappings_btn, {
      req(rv$resolution_df)

      success <- save_identity_mappings(rv$resolution_df)

      if (success) {
        shiny::showNotification("Mappings saved successfully", type = "message")
      } else {
        shiny::showNotification("Failed to save mappings", type = "error")
      }
    })

    # Export mappings
    output$export_mappings_btn <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_mappings_%s.csv", Sys.Date())
      },
      content = function(file) {
        export_df <- rv$resolution_df %>%
          dplyr::select(id, name, scopus_id, scholar_id, openalex_id,
                        resolution_status, resolution_method, openalex_name)
        utils::write.csv(export_df, file, row.names = FALSE)
      }
    )

    # Fetch data for resolved authors
    shiny::observeEvent(input$fetch_data_btn, {
      req(rv$resolution_df)

      # Get resolved records
      resolved <- rv$resolution_df %>%
        dplyr::filter(resolution_status %in% c("resolved", "manual"))

      if (nrow(resolved) == 0) {
        shiny::showNotification(
          "No resolved authors to fetch data for",
          type = "warning"
        )
        return()
      }

      rv$fetching <- TRUE
      rv$fetch_log <- character()

      # Prepare roster for fetching
      roster_for_fetch <- roster_rv$roster %>%
        dplyr::left_join(
          rv$resolution_df %>%
            dplyr::select(id, openalex_id),
          by = "id"
        )

      shiny::withProgress(message = "Fetching author data...", value = 0, {
        person_data <- list()

        for (i in seq_len(nrow(roster_for_fetch))) {
          row <- roster_for_fetch[i, ]

          rv$fetch_log <- c(rv$fetch_log,
                            sprintf("[%d/%d] Fetching %s...",
                                    i, nrow(roster_for_fetch), row$name))

          shiny::incProgress(1 / nrow(roster_for_fetch),
                             detail = row$name)

          person_data[[i]] <- fetch_person_data(
            as.list(row),
            cache_dir = "cache"
          )
          person_data[[i]]$roster_id <- row$id

          Sys.sleep(0.3)  # Rate limiting
        }

        rv$person_data <- person_data
      })

      rv$fetching <- FALSE

      n_success <- sum(sapply(rv$person_data, function(x) length(x$data_sources) > 0))
      shiny::showNotification(
        sprintf("Fetched data for %d of %d authors", n_success, nrow(roster_for_fetch)),
        type = "message"
      )
    })

    # Fetch log output
    output$fetch_log <- shiny::renderText({
      paste(tail(rv$fetch_log, 10), collapse = "\n")
    })

    # Fetching indicator for conditional panel
    output$fetching <- shiny::reactive({
      rv$fetching
    })
    shiny::outputOptions(output, "fetching", suspendWhenHidden = FALSE)

    # Return reactive values
    return(rv)
  })
}
