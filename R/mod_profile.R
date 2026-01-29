# =============================================================================
# FacultyIQ - Individual Profile Module
# =============================================================================
# Individual faculty member profiles and detailed metrics

#' Profile Module UI
#'
#' @param id Module namespace ID
mod_profile_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::h4("Select Faculty Member"),
          shiny::selectInput(
            ns("person_select"),
            "Choose person",
            choices = c("Select a person" = "")
          ),
          shiny::hr(),
          shiny::uiOutput(ns("person_info"))
        )
      ),
      shiny::column(
        width = 8,
        shiny::uiOutput(ns("profile_content"))
      )
    )
  )
}

#' Profile Module Server
#'
#' @param id Module namespace ID
#' @param resolution_rv Reactive values from resolution module
#' @param roster_rv Reactive values from upload module
mod_profile_server <- function(id, resolution_rv, roster_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update person selector when data is available
    shiny::observe({
      req(resolution_rv$person_data, roster_rv$roster)

      persons <- setNames(
        roster_rv$roster$id,
        roster_rv$roster$name
      )

      shiny::updateSelectInput(
        session, "person_select",
        choices = c("Select a person" = "", persons)
      )
    })

    # Get selected person's data
    selected_person <- shiny::reactive({
      req(input$person_select)
      req(resolution_rv$person_data)

      person_id <- as.integer(input$person_select)

      # Find in person_data
      for (pd in resolution_rv$person_data) {
        if (pd$roster_id == person_id) {
          return(pd)
        }
      }
      return(NULL)
    })

    # Get selected person's roster row
    selected_roster <- shiny::reactive({
      req(input$person_select)
      req(roster_rv$roster)

      person_id <- as.integer(input$person_select)
      roster_rv$roster[roster_rv$roster$id == person_id, ]
    })

    # Compute metrics for selected person
    selected_metrics <- shiny::reactive({
      pd <- selected_person()
      roster_row <- selected_roster()

      if (is.null(pd)) return(NULL)

      compute_person_metrics(pd, roster_row)
    })

    # Person info sidebar
    output$person_info <- shiny::renderUI({
      roster_row <- selected_roster()
      metrics <- selected_metrics()

      if (is.null(roster_row) || nrow(roster_row) == 0) {
        return(shiny::helpText("Select a person to view their profile"))
      }

      shiny::tagList(
        shiny::h5(roster_row$name),

        # Basic info
        if (!is.na(roster_row$academic_rank)) {
          shiny::p(shiny::strong("Rank: "), roster_row$academic_rank)
        },

        if (!is.na(roster_row$email)) {
          shiny::p(shiny::strong("Email: "), roster_row$email)
        },

        shiny::hr(),

        # IDs
        shiny::h6("External IDs"),
        if (!is.na(roster_row$scopus_id) && roster_row$scopus_id != "") {
          shiny::p(
            shiny::strong("Scopus: "),
            shiny::a(
              roster_row$scopus_id,
              href = sprintf("https://www.scopus.com/authid/detail.uri?authorId=%s",
                             roster_row$scopus_id),
              target = "_blank"
            )
          )
        },

        if (!is.na(roster_row$scholar_id) && roster_row$scholar_id != "") {
          shiny::p(
            shiny::strong("Scholar: "),
            shiny::a(
              roster_row$scholar_id,
              href = sprintf("https://scholar.google.com/citations?user=%s",
                             roster_row$scholar_id),
              target = "_blank"
            )
          )
        },

        if (!is.null(metrics) && !is.na(resolution_rv$resolution_df$openalex_id[
          resolution_rv$resolution_df$id == roster_row$id])) {
          oa_id <- resolution_rv$resolution_df$openalex_id[
            resolution_rv$resolution_df$id == roster_row$id]
          shiny::p(
            shiny::strong("OpenAlex: "),
            shiny::a(
              gsub("https://openalex.org/", "", oa_id),
              href = oa_id,
              target = "_blank"
            )
          )
        },

        shiny::hr(),

        # Data sources
        if (!is.null(metrics)) {
          shiny::div(
            shiny::h6("Data Sources"),
            if (length(metrics$data_sources) > 0) {
              shiny::tags$ul(
                lapply(metrics$data_sources, function(s) shiny::tags$li(s))
              )
            } else {
              shiny::span("No data sources available", class = "text-warning")
            },

            if (metrics$data_quality == "unavailable") {
              shiny::div(
                class = "alert alert-warning",
                shiny::icon("exclamation-triangle"),
                " Data unavailable: ",
                paste(metrics$unavailable_reason, collapse = "; ")
              )
            }
          )
        },

        shiny::hr(),

        # REAIMS comparison
        if (!is.na(roster_row$reaims_pubs)) {
          shiny::div(
            shiny::h6("REAIMS Comparison"),
            shiny::p(
              shiny::strong("Self-reported: "), roster_row$reaims_pubs, " publications"
            ),
            if (!is.null(metrics) && !is.na(metrics$works_count)) {
              shiny::p(
                shiny::strong("OpenAlex indexed: "), metrics$works_count, " works"
              )
            }
          )
        },

        shiny::hr(),

        # Export button
        shiny::downloadButton(ns("export_profile"), "Export Profile Report")
      )
    })

    # Main profile content
    output$profile_content <- shiny::renderUI({
      pd <- selected_person()
      metrics <- selected_metrics()

      if (is.null(pd) || is.null(metrics)) {
        return(shiny::wellPanel(
          shiny::h4("No Profile Selected"),
          shiny::p("Select a faculty member from the dropdown to view their profile.")
        ))
      }

      shiny::tagList(
        # KPI row
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::h3(format(null_coalesce(metrics$works_count, 0), big.mark = ",")),
              shiny::p("Total Works")
            )
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::h3(format(null_coalesce(metrics$citations_count, 0), big.mark = ",")),
              shiny::p("Citations")
            )
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::h3(null_coalesce(metrics$h_index, "N/A")),
              shiny::p("h-index")
            )
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::h3(null_coalesce(metrics$i10_index, "N/A")),
              shiny::p("i10-index")
            )
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::h3(paste0(null_coalesce(metrics$oa_percentage, "N/A"), "%")),
              shiny::p("Open Access")
            )
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::h3(sprintf("%d-%d",
                                null_coalesce(metrics$first_pub_year, NA),
                                null_coalesce(metrics$last_pub_year, NA))),
              shiny::p("Career Span")
            )
          )
        ),

        shiny::hr(),

        # Charts row
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::wellPanel(
              shiny::h4("Publications Over Time"),
              plotly::plotlyOutput(ns("plot_works_trend"), height = "300px")
            )
          ),
          shiny::column(
            width = 6,
            shiny::wellPanel(
              shiny::h4("Citations Over Time"),
              plotly::plotlyOutput(ns("plot_citations_trend"), height = "300px")
            )
          )
        ),

        # Works table
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::wellPanel(
              shiny::h4("Publications"),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::selectInput(
                    ns("works_sort"),
                    "Sort by",
                    choices = c(
                      "Most Cited" = "citations",
                      "Most Recent" = "year"
                    )
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::sliderInput(
                    ns("works_year_filter"),
                    "Year range",
                    min = 1990,
                    max = as.integer(format(Sys.Date(), "%Y")),
                    value = c(2000, as.integer(format(Sys.Date(), "%Y"))),
                    sep = ""
                  )
                )
              ),
              DT::dataTableOutput(ns("table_works"))
            )
          )
        ),

        # Topics/Concepts (if available)
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::wellPanel(
              shiny::h4("Research Topics"),
              shiny::uiOutput(ns("topics_display"))
            )
          )
        )
      )
    })

    # Publications trend plot
    output$plot_works_trend <- plotly::renderPlotly({
      metrics <- selected_metrics()

      if (is.null(metrics) || is.null(metrics$yearly_works)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      yearly <- metrics$yearly_works

      # Add cumulative
      yearly <- yearly %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(cumulative = cumsum(works))

      p <- ggplot2::ggplot(yearly, ggplot2::aes(x = year)) +
        ggplot2::geom_col(ggplot2::aes(y = works), fill = "steelblue", alpha = 0.7) +
        ggplot2::geom_line(ggplot2::aes(y = cumulative / max(cumulative, na.rm = TRUE) * max(works, na.rm = TRUE)),
                           color = "darkblue", size = 1, linetype = "dashed") +
        ggplot2::labs(x = "Year", y = "Works per Year") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Citations trend plot
    output$plot_citations_trend <- plotly::renderPlotly({
      metrics <- selected_metrics()

      if (is.null(metrics) || is.null(metrics$yearly_citations)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      yearly <- metrics$yearly_citations

      p <- ggplot2::ggplot(yearly, ggplot2::aes(x = year, y = citations)) +
        ggplot2::geom_col(fill = "forestgreen", alpha = 0.7) +
        ggplot2::labs(x = "Year", y = "Citations") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Works table
    output$table_works <- DT::renderDataTable({
      pd <- selected_person()

      if (is.null(pd) || is.null(pd$works)) {
        return(DT::datatable(
          data.frame(Message = "No publication data available"),
          options = list(dom = "t")
        ))
      }

      works <- pd$works

      # Filter by year
      works <- works %>%
        dplyr::filter(
          is.na(publication_year) |
            (publication_year >= input$works_year_filter[1] &
               publication_year <= input$works_year_filter[2])
        )

      # Sort
      if (input$works_sort == "citations") {
        works <- works %>%
          dplyr::arrange(dplyr::desc(cited_by_count))
      } else {
        works <- works %>%
          dplyr::arrange(dplyr::desc(publication_year), dplyr::desc(cited_by_count))
      }

      # Prepare display
      display_df <- works %>%
        dplyr::select(title, publication_year, cited_by_count, journal, is_oa, doi) %>%
        dplyr::mutate(
          title = sapply(seq_len(dplyr::n()), function(i) {
            if (!is.na(doi[i])) {
              sprintf('<a href="%s" target="_blank">%s</a>',
                      doi[i], stringr::str_trunc(title[i], 80))
            } else {
              stringr::str_trunc(title[i], 80)
            }
          }),
          journal = stringr::str_trunc(journal, 40),
          is_oa = ifelse(is_oa == TRUE, "Yes", "No")
        ) %>%
        dplyr::select(-doi)

      DT::datatable(
        display_df,
        escape = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Title", "Year", "Citations", "Journal", "OA")
      )
    })

    # Topics display
    output$topics_display <- shiny::renderUI({
      pd <- selected_person()

      if (is.null(pd) || is.null(pd$works) || !"concepts" %in% names(pd$works)) {
        return(shiny::helpText("Topic data not available"))
      }

      # Extract and count concepts
      all_concepts <- unlist(strsplit(pd$works$concepts, "; "))
      all_concepts <- all_concepts[!is.na(all_concepts)]

      if (length(all_concepts) == 0) {
        return(shiny::helpText("No topic data available"))
      }

      concept_counts <- sort(table(all_concepts), decreasing = TRUE)
      top_concepts <- head(concept_counts, 15)

      # Create tag cloud style display
      shiny::div(
        lapply(names(top_concepts), function(concept) {
          count <- top_concepts[[concept]]
          size <- min(24, max(12, 10 + count))
          shiny::span(
            concept,
            class = "label label-info",
            style = sprintf("font-size: %dpx; margin: 2px; display: inline-block;", size)
          )
        })
      )
    })

    # Export profile report
    output$export_profile <- shiny::downloadHandler(
      filename = function() {
        roster_row <- selected_roster()
        name_clean <- gsub("[^a-zA-Z0-9]", "_", roster_row$name)
        sprintf("FacultyIQ_Profile_%s_%s.html", name_clean, Sys.Date())
      },
      content = function(file) {
        pd <- selected_person()
        metrics <- selected_metrics()
        roster_row <- selected_roster()

        # Generate simple HTML report
        html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <title>FacultyIQ Profile: %s</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; }
    h1 { color: #2c3e50; }
    .metric-box { display: inline-block; padding: 15px; margin: 5px;
                  background: #ecf0f1; border-radius: 5px; text-align: center; }
    .metric-value { font-size: 24px; font-weight: bold; color: #3498db; }
    .metric-label { font-size: 12px; color: #7f8c8d; }
    table { border-collapse: collapse; width: 100%%; margin-top: 20px; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #3498db; color: white; }
  </style>
</head>
<body>
  <h1>%s</h1>
  <p><strong>Academic Rank:</strong> %s</p>
  <p><strong>Report Generated:</strong> %s</p>

  <h2>Summary Metrics</h2>
  <div class="metric-box">
    <div class="metric-value">%s</div>
    <div class="metric-label">Total Works</div>
  </div>
  <div class="metric-box">
    <div class="metric-value">%s</div>
    <div class="metric-label">Total Citations</div>
  </div>
  <div class="metric-box">
    <div class="metric-value">%s</div>
    <div class="metric-label">h-index</div>
  </div>
  <div class="metric-box">
    <div class="metric-value">%s</div>
    <div class="metric-label">i10-index</div>
  </div>
  <div class="metric-box">
    <div class="metric-value">%s%%%%</div>
    <div class="metric-label">Open Access</div>
  </div>

  <h2>Career Statistics</h2>
  <ul>
    <li><strong>First Publication Year:</strong> %s</li>
    <li><strong>Last Publication Year:</strong> %s</li>
    <li><strong>Career Span:</strong> %s years</li>
    <li><strong>Works per Year:</strong> %s</li>
    <li><strong>Citations per Work:</strong> %s</li>
  </ul>

  <h2>Data Sources</h2>
  <p>%s</p>

  <hr>
  <p><em>Generated by FacultyIQ</em></p>
</body>
</html>',
                                 roster_row$name,
                                 roster_row$name,
                                 null_coalesce(roster_row$academic_rank, "Not specified"),
                                 Sys.time(),
                                 format(null_coalesce(metrics$works_count, 0), big.mark = ","),
                                 format(null_coalesce(metrics$citations_count, 0), big.mark = ","),
                                 null_coalesce(metrics$h_index, "N/A"),
                                 null_coalesce(metrics$i10_index, "N/A"),
                                 null_coalesce(metrics$oa_percentage, "N/A"),
                                 null_coalesce(metrics$first_pub_year, "N/A"),
                                 null_coalesce(metrics$last_pub_year, "N/A"),
                                 null_coalesce(metrics$career_years, "N/A"),
                                 null_coalesce(metrics$works_per_year, "N/A"),
                                 null_coalesce(metrics$citations_per_work, "N/A"),
                                 paste(metrics$data_sources, collapse = ", ")
        )

        writeLines(html_content, file)
      }
    )

    return(NULL)
  })
}
