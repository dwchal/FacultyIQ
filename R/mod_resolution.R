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
    # =========================================================================
    # Build Roster Section (shown when no roster loaded)
    # =========================================================================
    shiny::uiOutput(ns("build_roster_ui")),

    # =========================================================================
    # Resolution Section (shown when roster exists)
    # =========================================================================
    shiny::conditionalPanel(
      condition = sprintf("output['%s'] != null", ns("has_roster")),
      ns = ns,

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
                shinydashboard::valueBoxOutput(ns("vb_total"), width = 12)
              ),
              shiny::column(
                width = 3,
                shinydashboard::valueBoxOutput(ns("vb_resolved"), width = 12)
              ),
              shiny::column(
                width = 3,
                shinydashboard::valueBoxOutput(ns("vb_pending"), width = 12)
              ),
              shiny::column(
                width = 3,
                shinydashboard::valueBoxOutput(ns("vb_failed"), width = 12)
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
          shiny::div(
            class = "progress",
            shiny::div(
              class = "progress-bar progress-bar-striped active",
              role = "progressbar",
              style = "width: 100%"
            )
          ),
          shiny::verbatimTextOutput(ns("fetch_log"))
        )
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
      fetch_log = character(),
      auto_fetch_done = FALSE,
      # For roster building
      build_search_results = NULL,
      manual_roster = NULL
    )

    # =========================================================================
    # Roster Building Section
    # =========================================================================

    # Output to control conditional panel visibility
    output$has_roster <- shiny::reactive({
      if (!is.null(roster_rv$roster)) {
        "ready"
      } else {
        NULL
      }
    })
    shiny::outputOptions(output, "has_roster", suspendWhenHidden = FALSE)

    # Build Roster UI - shown when no roster is loaded
    output$build_roster_ui <- shiny::renderUI({
      # Only show when no roster is loaded
      if (!is.null(roster_rv$roster)) {
        return(NULL)
      }

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "alert alert-info",
              shiny::icon("info-circle"),
              shiny::strong(" Getting Started: "),
              "Build your faculty roster by searching OpenAlex, entering details manually, or importing a CSV file."
            )
          )
        ),

        shiny::fluidRow(
          # Left column: Search and Add
          shiny::column(
            width = 6,
            shiny::wellPanel(
              shiny::h4(shiny::icon("search"), " Search OpenAlex"),
              shiny::p("Search for faculty members by name and add them to your roster."),
              shiny::fluidRow(
                shiny::column(
                  width = 8,
                  shiny::textInput(
                    ns("build_search_name"),
                    "Faculty Name",
                    placeholder = "e.g., John Smith"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    ns("build_search_inst"),
                    "Institution",
                    placeholder = "(optional)"
                  )
                )
              ),
              shiny::actionButton(
                ns("build_search_btn"),
                "Search",
                icon = shiny::icon("search"),
                class = "btn-primary"
              ),
              shiny::hr(),
              shiny::uiOutput(ns("build_search_results_ui"))
            ),

            shiny::wellPanel(
              shiny::h4(shiny::icon("user-plus"), " Add Manually"),
              shiny::p("Add a faculty member without searching."),
              shiny::textInput(ns("manual_name"), "Name *", placeholder = "Full name"),
              shiny::textInput(ns("manual_email"), "Email", placeholder = "email@university.edu"),
              shiny::selectInput(
                ns("manual_rank"),
                "Academic Rank",
                choices = c(
                  "",
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
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::textInput(ns("manual_scopus"), "Scopus ID", placeholder = "e.g., 7004567890")
                ),
                shiny::column(
                  width = 6,
                  shiny::textInput(ns("manual_scholar"), "Scholar ID", placeholder = "e.g., ABC123def")
                )
              ),
              shiny::actionButton(
                ns("manual_add_btn"),
                "Add to Roster",
                icon = shiny::icon("plus"),
                class = "btn-success"
              )
            )
          ),

          # Right column: Current Roster and Import
          shiny::column(
            width = 6,
            shiny::wellPanel(
              shiny::h4(shiny::icon("users"), " Current Roster"),
              shiny::p(
                shiny::textOutput(ns("roster_count"), inline = TRUE),
                " faculty member(s) added"
              ),
              DT::dataTableOutput(ns("roster_preview")),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::actionButton(
                    ns("roster_remove_btn"),
                    "Remove Selected",
                    icon = shiny::icon("trash"),
                    class = "btn-warning btn-sm"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::actionButton(
                    ns("roster_clear_btn"),
                    "Clear All",
                    icon = shiny::icon("times"),
                    class = "btn-danger btn-sm"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::actionButton(
                    ns("roster_done_btn"),
                    "Done Building",
                    icon = shiny::icon("check"),
                    class = "btn-success"
                  )
                )
              )
            ),

            shiny::wellPanel(
              shiny::h4(shiny::icon("file-import"), " Import from File"),
              shiny::p("Import a roster from CSV file."),
              shiny::fileInput(
                ns("import_file"),
                "Choose CSV file",
                accept = c(".csv", "text/csv")
              ),
              shiny::helpText(
                "CSV should have columns: name (required), email, academic_rank, scopus_id, scholar_id"
              )
            )
          )
        )
      )
    })

    # Search OpenAlex for authors
    shiny::observeEvent(input$build_search_btn, {
      name <- trimws(null_coalesce(input$build_search_name, ""))
      if (nchar(name) < 2) {
        shiny::showNotification("Please enter at least 2 characters", type = "warning")
        return()
      }

      shiny::withProgress(message = "Searching OpenAlex...", {
        inst <- if (!is.null(input$build_search_inst) && nchar(trimws(input$build_search_inst)) > 0) {
          trimws(input$build_search_inst)
        } else {
          NULL
        }

        results <- search_openalex_authors(name, institution = inst, limit = 10)
        rv$build_search_results <- results
      })

      if (is.null(rv$build_search_results) || nrow(rv$build_search_results) == 0) {
        shiny::showNotification("No results found", type = "warning")
      }
    })

    # Display search results
    output$build_search_results_ui <- shiny::renderUI({
      if (is.null(rv$build_search_results) || nrow(rv$build_search_results) == 0) {
        return(shiny::helpText("Search results will appear here"))
      }

      shiny::tagList(
        shiny::h5("Search Results"),
        DT::dataTableOutput(ns("build_search_table")),
        shiny::br(),
        shiny::actionButton(
          ns("build_add_selected_btn"),
          "Add Selected to Roster",
          icon = shiny::icon("plus"),
          class = "btn-success"
        )
      )
    })

    output$build_search_table <- DT::renderDataTable({
      req(rv$build_search_results)

      display_df <- rv$build_search_results
      if (nrow(display_df) > 0) {
        display_df <- display_df %>%
          dplyr::mutate(
            works = format(works_count, big.mark = ","),
            citations = format(cited_by_count, big.mark = ","),
            h_idx = ifelse(is.na(h_index), "-", as.character(h_index)),
            inst = ifelse(is.na(affiliation), "-", stringr::str_trunc(affiliation, 30))
          ) %>%
          dplyr::select(display_name, inst, works, citations, h_idx)
      }

      DT::datatable(
        display_df,
        selection = "single",
        options = list(pageLength = 5, dom = "t", scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Name", "Institution", "Works", "Citations", "h-index")
      )
    })

    # Add selected search result to roster
    shiny::observeEvent(input$build_add_selected_btn, {
      selected_idx <- input$build_search_table_rows_selected
      if (is.null(selected_idx) || length(selected_idx) == 0) {
        shiny::showNotification("Please select an author from the search results", type = "warning")
        return()
      }

      selected <- rv$build_search_results[selected_idx, ]

      # Normalize OpenAlex ID to consistent format
      openalex_id <- selected$openalex_id
      if (!is.null(openalex_id) && !is.na(openalex_id) && openalex_id != "") {
        # Extract just the ID part if URL format
        if (grepl("^https://", openalex_id, ignore.case = TRUE)) {
          openalex_id <- sub("^https://openalex.org/", "", openalex_id, ignore.case = TRUE)
        }
        # Uppercase the A prefix and rebuild URL
        openalex_id <- paste0("A", sub("^[Aa]", "", openalex_id))
        openalex_id <- paste0("https://openalex.org/", openalex_id)
      }

      # Compute next ID
      next_id <- 1L
      if (!is.null(rv$manual_roster)) {
        next_id <- max(rv$manual_roster$id) + 1L
      }

      new_row <- data.frame(
        id = next_id,
        name = selected$display_name,
        email = NA_character_,
        academic_rank = NA_character_,
        last_promotion = NA_character_,
        reaims_pubs = NA_integer_,
        scopus_id = NA_character_,
        scholar_id = NA_character_,
        associations = NA_character_,
        openalex_id = openalex_id,
        resolution_status = "resolved",
        resolution_method = "search_add",
        stringsAsFactors = FALSE
      )

      if (is.null(rv$manual_roster)) {
        rv$manual_roster <- new_row
      } else {
        rv$manual_roster <- dplyr::bind_rows(rv$manual_roster, new_row)
      }

      shiny::showNotification(sprintf("Added %s", selected$display_name), type = "message")
      rv$build_search_results <- NULL
    })

    # Add manual entry to roster
    shiny::observeEvent(input$manual_add_btn, {
      name <- trimws(null_coalesce(input$manual_name, ""))
      if (nchar(name) < 2) {
        shiny::showNotification("Please enter a name", type = "error")
        return()
      }

      # Compute next ID
      next_id <- 1L
      if (!is.null(rv$manual_roster)) {
        next_id <- max(rv$manual_roster$id) + 1L
      }

      # Get academic rank
      rank_val <- NA_character_
      if (!is.null(input$manual_rank) && input$manual_rank != "") {
        rank_val <- input$manual_rank
      }

      new_row <- data.frame(
        id = next_id,
        name = name,
        email = null_coalesce(trimws(input$manual_email), NA_character_),
        academic_rank = rank_val,
        last_promotion = NA_character_,
        reaims_pubs = NA_integer_,
        scopus_id = null_coalesce(trimws(input$manual_scopus), NA_character_),
        scholar_id = null_coalesce(trimws(input$manual_scholar), NA_character_),
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

      shiny::showNotification(sprintf("Added %s", name), type = "message")

      # Clear form
      shiny::updateTextInput(session, "manual_name", value = "")
      shiny::updateTextInput(session, "manual_email", value = "")
      shiny::updateSelectInput(session, "manual_rank", selected = "")
      shiny::updateTextInput(session, "manual_scopus", value = "")
      shiny::updateTextInput(session, "manual_scholar", value = "")
    })

    # Roster count
    output$roster_count <- shiny::renderText({
      if (is.null(rv$manual_roster)) 0 else nrow(rv$manual_roster)
    })

    # Roster preview table
    output$roster_preview <- DT::renderDataTable({
      if (is.null(rv$manual_roster) || nrow(rv$manual_roster) == 0) {
        return(DT::datatable(
          data.frame(Message = "No faculty added yet"),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      display_df <- rv$manual_roster %>%
        dplyr::select(id, name, academic_rank, resolution_status)

      DT::datatable(
        display_df,
        selection = "single",
        options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
        rownames = FALSE,
        colnames = c("ID", "Name", "Rank", "Status")
      )
    })

    # Remove selected from roster
    shiny::observeEvent(input$roster_remove_btn, {
      selected_idx <- input$roster_preview_rows_selected
      if (is.null(selected_idx) || is.null(rv$manual_roster)) {
        shiny::showNotification("Please select a row to remove", type = "warning")
        return()
      }

      removed_name <- rv$manual_roster$name[selected_idx]
      rv$manual_roster <- rv$manual_roster[-selected_idx, ]
      if (nrow(rv$manual_roster) == 0) {
        rv$manual_roster <- NULL
      }

      shiny::showNotification(sprintf("Removed %s", removed_name), type = "message")
    })

    # Clear roster
    shiny::observeEvent(input$roster_clear_btn, {
      rv$manual_roster <- NULL
      shiny::showNotification("Roster cleared", type = "message")
    })

    # Done building roster - transfer to main roster
    shiny::observeEvent(input$roster_done_btn, {
      if (is.null(rv$manual_roster) || nrow(rv$manual_roster) == 0) {
        shiny::showNotification("Please add at least one faculty member", type = "warning")
        return()
      }

      roster_rv$roster <- rv$manual_roster
      roster_rv$ready <- TRUE

      shiny::showNotification(
        sprintf("Roster created with %d faculty member(s)", nrow(rv$manual_roster)),
        type = "message"
      )
    })

    # Import from CSV
    shiny::observeEvent(input$import_file, {
      req(input$import_file)

      tryCatch({
        data <- readr::read_csv(input$import_file$datapath, show_col_types = FALSE)
        data <- as.data.frame(data)

        # Check for required name column
        name_col <- intersect(c("name", "Name", "NAME", "full_name", "faculty_name"), names(data))
        if (length(name_col) == 0) {
          shiny::showNotification("CSV must have a 'name' column", type = "error")
          return()
        }

        # Standardize column names
        names(data) <- tolower(names(data))
        n_rows <- nrow(data)
        col_names <- names(data)

        # Extract columns with defaults
        email_col <- if ("email" %in% col_names) data$email else rep(NA_character_, n_rows)
        rank_col <- if ("academic_rank" %in% col_names) data$academic_rank else rep(NA_character_, n_rows)
        promo_col <- if ("last_promotion" %in% col_names) as.character(data$last_promotion) else rep(NA_character_, n_rows)
        reaims_col <- if ("reaims_pubs" %in% col_names) as.integer(data$reaims_pubs) else rep(NA_integer_, n_rows)
        scopus_col <- if ("scopus_id" %in% col_names) as.character(data$scopus_id) else rep(NA_character_, n_rows)
        scholar_col <- if ("scholar_id" %in% col_names) as.character(data$scholar_id) else rep(NA_character_, n_rows)
        assoc_col <- if ("associations" %in% col_names) data$associations else rep(NA_character_, n_rows)
        oalex_col <- if ("openalex_id" %in% col_names) data$openalex_id else rep(NA_character_, n_rows)

        # Normalize OpenAlex IDs to consistent format
        oalex_col <- sapply(oalex_col, function(openalex_id) {
          if (is.null(openalex_id) || is.na(openalex_id) || openalex_id == "") {
            return(NA_character_)
          }
          openalex_id <- as.character(openalex_id)
          if (grepl("^https://", openalex_id, ignore.case = TRUE)) {
            openalex_id <- sub("^https://openalex.org/", "", openalex_id, ignore.case = TRUE)
          }
          openalex_id <- paste0("A", sub("^[Aa]", "", openalex_id))
          paste0("https://openalex.org/", openalex_id)
        }, USE.NAMES = FALSE)

        # Determine resolution status
        status_col <- rep("pending", n_rows)
        if ("openalex_id" %in% col_names && any(!is.na(data$openalex_id))) {
          status_col <- ifelse(!is.na(oalex_col) & oalex_col != "", "resolved", "pending")
        }

        # Build roster from imported data
        imported_roster <- data.frame(
          id = seq_len(n_rows),
          name = data$name,
          email = email_col,
          academic_rank = rank_col,
          last_promotion = promo_col,
          reaims_pubs = reaims_col,
          scopus_id = scopus_col,
          scholar_id = scholar_col,
          associations = assoc_col,
          openalex_id = oalex_col,
          resolution_status = status_col,
          resolution_method = "import",
          stringsAsFactors = FALSE
        )

        # Add to existing manual roster or replace
        if (is.null(rv$manual_roster)) {
          rv$manual_roster <- imported_roster
        } else {
          # Adjust IDs to continue from existing
          imported_roster$id <- imported_roster$id + max(rv$manual_roster$id)
          rv$manual_roster <- dplyr::bind_rows(rv$manual_roster, imported_roster)
        }

        shiny::showNotification(
          sprintf("Imported %d faculty members from CSV", nrow(imported_roster)),
          type = "message"
        )

      }, error = function(e) {
        shiny::showNotification(
          paste("Error importing CSV:", e$message),
          type = "error"
        )
      })
    })

    # =========================================================================
    # Resolution Section (existing functionality)
    # =========================================================================

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
              # Normalize OpenAlex ID to consistent format
              openalex_id <- result$openalex_id
              if (!is.null(openalex_id) && !is.na(openalex_id) && openalex_id != "") {
                if (grepl("^https://", openalex_id, ignore.case = TRUE)) {
                  openalex_id <- sub("^https://openalex.org/", "", openalex_id, ignore.case = TRUE)
                }
                openalex_id <- paste0("A", sub("^[Aa]", "", openalex_id))
                openalex_id <- paste0("https://openalex.org/", openalex_id)
              }
              rv$resolution_df$openalex_id[idx] <- openalex_id
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
          } else {
            NULL
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
        } else {
          NULL
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

      display_df <- rv$search_results
      if (nrow(display_df) > 0) {
        display_df <- display_df %>%
          dplyr::mutate(
            works = format(works_count, big.mark = ","),
            citations = format(cited_by_count, big.mark = ","),
            h_idx = ifelse(is.na(h_index), "-", as.character(h_index)),
            inst = ifelse(is.na(affiliation), "-", stringr::str_trunc(affiliation, 25))
          ) %>%
          dplyr::select(display_name, inst, works, citations, h_idx)
      }

      DT::datatable(
        display_df,
        selection = "single",
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "t"
        ),
        rownames = FALSE,
        colnames = c("Name", "Institution", "Works", "Citations", "h-index")
      )
    })

    # Select author from search results
    shiny::observeEvent(input$select_author_btn, {
      req(rv$selected_row, rv$search_results)
      req(input$search_results_table_rows_selected)

      selected_result <- rv$search_results[input$search_results_table_rows_selected, ]
      idx <- rv$selected_row

      # Normalize OpenAlex ID to consistent format
      openalex_id <- selected_result$openalex_id
      if (!is.null(openalex_id) && !is.na(openalex_id) && openalex_id != "") {
        if (grepl("^https://", openalex_id, ignore.case = TRUE)) {
          openalex_id <- sub("^https://openalex.org/", "", openalex_id, ignore.case = TRUE)
        }
        openalex_id <- paste0("A", sub("^[Aa]", "", openalex_id))
        openalex_id <- paste0("https://openalex.org/", openalex_id)
      }

      rv$resolution_df$openalex_id[idx] <- openalex_id
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

      # Validate and normalize OpenAlex ID format (case-insensitive)
      openalex_id <- trimws(input$manual_openalex_id)

      # Accept both lowercase and uppercase, with or without URL prefix
      if (!grepl("^https://openalex.org/[Aa][0-9]+$", openalex_id, ignore.case = TRUE) &&
          !grepl("^[Aa][0-9]+$", openalex_id)) {
        shiny::showNotification(
          "Invalid OpenAlex ID format. Expected: A1234567890 or https://openalex.org/A1234567890",
          type = "error"
        )
        return()
      }

      # Normalize: extract just the ID part and uppercase it
      if (grepl("^https://", openalex_id, ignore.case = TRUE)) {
        # Extract ID from URL
        openalex_id <- sub("^https://openalex.org/", "", openalex_id, ignore.case = TRUE)
      }
      # Uppercase the A prefix
      openalex_id <- paste0("A", sub("^[Aa]", "", openalex_id))
      # Convert to full URL
      openalex_id <- paste0("https://openalex.org/", openalex_id)

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

    fetch_resolved_data <- function(show_empty_warning = TRUE) {
      req(rv$resolution_df)

      resolved <- rv$resolution_df %>%
        dplyr::filter(resolution_status %in% c("resolved", "manual"))

      if (nrow(resolved) == 0) {
        if (show_empty_warning) {
          shiny::showNotification(
            "No resolved authors to fetch data for",
            type = "warning"
          )
        }
        return(invisible(FALSE))
      }

      rv$fetching <- TRUE
      rv$fetch_log <- character()

      roster_base <- roster_rv$roster
      if ("openalex_id" %in% names(roster_base)) {
        roster_base <- roster_base %>% dplyr::select(-openalex_id)
      }
      roster_for_fetch <- roster_base %>%
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
      rv$auto_fetch_done <- TRUE

      n_success <- sum(sapply(rv$person_data, function(x) length(x$data_sources) > 0))
      shiny::showNotification(
        sprintf("Fetched data for %d of %d authors", n_success, nrow(roster_for_fetch)),
        type = "message"
      )

      invisible(TRUE)
    }

    # Fetch data for resolved authors
    shiny::observeEvent(input$fetch_data_btn, {
      fetch_resolved_data()
    })

    # Auto-fetch when a resolved manual roster is completed
    shiny::observeEvent(roster_rv$roster, {
      req(rv$resolution_df)
      if (isTRUE(rv$auto_fetch_done)) {
        return()
      }

      resolved <- rv$resolution_df %>%
        dplyr::filter(resolution_status %in% c("resolved", "manual"))

      if (nrow(resolved) == 0) {
        return()
      }

      fetch_resolved_data(show_empty_warning = FALSE)
    }, ignoreInit = TRUE)

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
