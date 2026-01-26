# =============================================================================
# FacultyIQ - Export Module
# =============================================================================
# Data export functionality

# Helper for NULL coalescing
null_coalesce <- function(x, y) if (is.null(x) || is.na(x)) y else x

#' Export Module UI
#'
#' @param id Module namespace ID
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Roster Export for Reimport (Prominent)
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          style = "background-color: #e8f4f8; border-left: 4px solid #3498db;",
          shiny::h4(shiny::icon("save"), " Save Roster for Later"),
          shiny::p(
            "Export your current roster with all resolved IDs. ",
            "You can reimport this file later to skip the identity resolution step."
          ),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::downloadButton(
                ns("export_roster_reimport"),
                "Export Roster for Reimport",
                class = "btn-primary btn-lg"
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "alert alert-info",
                style = "margin-bottom: 0;",
                shiny::icon("info-circle"),
                " To reimport: Go to 'Upload Data' > select 'Import previously exported roster' > upload this file."
              )
            )
          )
        )
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4(shiny::icon("table"), " Export Data Tables"),

          shiny::h5("Roster with Resolved IDs"),
          shiny::p("Export the original roster data with resolved OpenAlex IDs"),
          shiny::downloadButton(ns("export_roster"), "Download Roster CSV"),

          shiny::hr(),

          shiny::h5("Computed Metrics (Wide Format)"),
          shiny::p("One row per person with all computed metrics"),
          shiny::downloadButton(ns("export_metrics_wide"), "Download Metrics CSV"),

          shiny::hr(),

          shiny::h5("Yearly Metrics (Long Format)"),
          shiny::p("Year-by-year metrics for all people (works, citations)"),
          shiny::downloadButton(ns("export_metrics_long"), "Download Yearly CSV"),

          shiny::hr(),

          shiny::h5("Publication List"),
          shiny::p("All indexed publications for all faculty"),
          shiny::downloadButton(ns("export_publications"), "Download Publications CSV")
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4(shiny::icon("chart-bar"), " Export Visualizations"),

          shiny::h5("Division Summary Plot"),
          shiny::p("Works and citations over time"),
          shiny::downloadButton(ns("export_plot_summary"), "Download PNG"),

          shiny::hr(),

          shiny::h5("Rank Comparison Plot"),
          shiny::p("Metrics by academic rank"),
          shiny::downloadButton(ns("export_plot_rank"), "Download PNG"),

          shiny::hr(),

          shiny::h5("Open Access Trend Plot"),
          shiny::p("OA percentage over time"),
          shiny::downloadButton(ns("export_plot_oa"), "Download PNG")
        )
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("file-alt"), " Generate Division Report"),
          shiny::p("Generate a comprehensive HTML report of division metrics"),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::textInput(ns("report_title"), "Report Title", "Division Research Analytics Report")
            ),
            shiny::column(
              width = 4,
              shiny::textInput(ns("report_division"), "Division Name", "")
            ),
            shiny::column(
              width = 4,
              shiny::dateInput(ns("report_date"), "Report Date", Sys.Date())
            )
          ),
          shiny::downloadButton(ns("export_report"), "Generate Report", class = "btn-primary")
        )
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("cog"), " Cache Management"),
          shiny::p("Manage cached API responses"),
          shiny::verbatimTextOutput(ns("cache_stats")),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::actionButton(ns("clear_expired"), "Clear Expired Cache", class = "btn-warning")
            ),
            shiny::column(
              width = 4,
              shiny::actionButton(ns("clear_all"), "Clear All Cache", class = "btn-danger")
            )
          )
        )
      )
    )
  )
}

#' Export Module Server
#'
#' @param id Module namespace ID
#' @param resolution_rv Reactive values from resolution module
#' @param roster_rv Reactive values from upload module
mod_export_server <- function(id, resolution_rv, roster_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Computed metrics (reactive)
    metrics_df <- shiny::reactive({
      req(resolution_rv$person_data, roster_rv$roster)
      compute_all_metrics(resolution_rv$person_data, roster_rv$roster)
    })

    # Export roster specifically formatted for reimport
    output$export_roster_reimport <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_roster_export_%s.csv", Sys.Date())
      },
      content = function(file) {
        req(roster_rv$roster)

        # Start with the roster
        export_df <- roster_rv$roster

        # Add resolved IDs if available
        if (!is.null(resolution_rv$resolution_df)) {
          export_df <- export_df %>%
            dplyr::left_join(
              resolution_rv$resolution_df %>%
                dplyr::select(id, openalex_id, resolution_status, resolution_method),
              by = "id",
              suffix = c("", ".resolved")
            )

          # Prefer resolved values
          if ("openalex_id.resolved" %in% names(export_df)) {
            export_df$openalex_id <- dplyr::coalesce(
              export_df$openalex_id.resolved,
              export_df$openalex_id
            )
            export_df$openalex_id.resolved <- NULL
          }
          if ("resolution_status.resolved" %in% names(export_df)) {
            export_df$resolution_status <- dplyr::coalesce(
              export_df$resolution_status.resolved,
              export_df$resolution_status
            )
            export_df$resolution_status.resolved <- NULL
          }
          if ("resolution_method.resolved" %in% names(export_df)) {
            export_df$resolution_method <- dplyr::coalesce(
              export_df$resolution_method.resolved,
              export_df$resolution_method
            )
            export_df$resolution_method.resolved <- NULL
          }
        }

        # Ensure all standard columns are present
        standard_cols <- c(
          "id", "name", "email", "academic_rank", "last_promotion",
          "reaims_pubs", "scopus_id", "scholar_id", "openalex_id",
          "associations", "resolution_status", "resolution_method"
        )

        for (col in standard_cols) {
          if (!col %in% names(export_df)) {
            export_df[[col]] <- NA
          }
        }

        # Select only standard columns in order
        export_df <- export_df[, standard_cols]

        utils::write.csv(export_df, file, row.names = FALSE, na = "")

        shiny::showNotification(
          "Roster exported! You can reimport this file later.",
          type = "message"
        )
      }
    )

    # Export roster with resolved IDs
    output$export_roster <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_roster_%s.csv", Sys.Date())
      },
      content = function(file) {
        req(roster_rv$roster, resolution_rv$resolution_df)

        export_df <- roster_rv$roster %>%
          dplyr::left_join(
            resolution_rv$resolution_df %>%
              dplyr::select(id, openalex_id, resolution_status, resolution_method),
            by = "id"
          )

        # Remove internal tracking columns
        export_df <- export_df %>%
          dplyr::select(-dplyr::any_of(c("openalex_id.x", "resolution_status.x",
                                         "resolution_method.x", "last_updated")))

        utils::write.csv(export_df, file, row.names = FALSE)
      }
    )

    # Export metrics wide format
    output$export_metrics_wide <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_metrics_%s.csv", Sys.Date())
      },
      content = function(file) {
        req(metrics_df())
        utils::write.csv(metrics_df(), file, row.names = FALSE)
      }
    )

    # Export yearly metrics long format
    output$export_metrics_long <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_yearly_%s.csv", Sys.Date())
      },
      content = function(file) {
        req(resolution_rv$person_data)

        # Get works yearly
        works_yearly <- create_yearly_timeseries(
          resolution_rv$person_data,
          metric = "works",
          from_year = 1990
        )
        works_yearly$metric <- "works"

        # Get citations yearly
        citations_yearly <- create_yearly_timeseries(
          resolution_rv$person_data,
          metric = "citations",
          from_year = 1990
        )
        citations_yearly$metric <- "citations"

        # Combine
        combined <- dplyr::bind_rows(works_yearly, citations_yearly) %>%
          dplyr::select(roster_id, name, year, metric, value) %>%
          dplyr::arrange(name, metric, year)

        utils::write.csv(combined, file, row.names = FALSE)
      }
    )

    # Export publications
    output$export_publications <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_publications_%s.csv", Sys.Date())
      },
      content = function(file) {
        req(resolution_rv$person_data)

        all_pubs <- list()
        for (pd in resolution_rv$person_data) {
          if (!is.null(pd$works) && nrow(pd$works) > 0) {
            pubs <- pd$works
            pubs$author_name <- pd$name
            pubs$roster_id <- pd$roster_id
            all_pubs[[length(all_pubs) + 1]] <- pubs
          }
        }

        if (length(all_pubs) == 0) {
          utils::write.csv(data.frame(Message = "No publication data"), file, row.names = FALSE)
          return()
        }

        combined <- dplyr::bind_rows(all_pubs) %>%
          dplyr::select(
            roster_id, author_name, title, publication_year,
            cited_by_count, journal, doi, is_oa, oa_status,
            dplyr::any_of(c("concepts", "type"))
          ) %>%
          dplyr::arrange(author_name, dplyr::desc(publication_year))

        utils::write.csv(combined, file, row.names = FALSE)
      }
    )

    # Export summary plot
    output$export_plot_summary <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_summary_plot_%s.png", Sys.Date())
      },
      content = function(file) {
        req(resolution_rv$person_data)

        works_yearly <- create_yearly_timeseries(
          resolution_rv$person_data,
          metric = "works",
          from_year = 2000
        )

        agg <- aggregate_division_yearly(works_yearly)

        if (nrow(agg) == 0) {
          # Create empty plot
          p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            ggplot2::theme_void()
        } else {
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = year)) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75),
                                 fill = "steelblue", alpha = 0.3) +
            ggplot2::geom_line(ggplot2::aes(y = total), color = "steelblue", size = 1.2) +
            ggplot2::geom_line(ggplot2::aes(y = median), color = "darkblue",
                               linetype = "dashed", size = 0.8) +
            ggplot2::labs(
              title = "Division Research Output Over Time",
              subtitle = "Solid: Total | Dashed: Median | Shaded: IQR",
              x = "Year",
              y = "Works Published"
            ) +
            ggplot2::theme_minimal(base_size = 14)
        }

        ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)
      }
    )

    # Export rank comparison plot
    output$export_plot_rank <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_rank_plot_%s.png", Sys.Date())
      },
      content = function(file) {
        m <- metrics_df()

        if (!any(!is.na(m$academic_rank))) {
          p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No rank data available") +
            ggplot2::theme_void()
        } else {
          rank_summary <- m %>%
            dplyr::filter(!is.na(academic_rank)) %>%
            dplyr::group_by(academic_rank) %>%
            dplyr::summarise(
              median_works = median(works_count, na.rm = TRUE),
              n = dplyr::n(),
              .groups = "drop"
            )

          p <- ggplot2::ggplot(rank_summary,
                               ggplot2::aes(x = reorder(academic_rank, median_works),
                                            y = median_works)) +
            ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
            ggplot2::geom_text(ggplot2::aes(label = sprintf("n=%d", n)),
                               hjust = -0.2, size = 3.5) +
            ggplot2::coord_flip() +
            ggplot2::labs(
              title = "Research Output by Academic Rank",
              x = "Academic Rank",
              y = "Median Works"
            ) +
            ggplot2::theme_minimal(base_size = 14)
        }

        ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)
      }
    )

    # Export OA plot
    output$export_plot_oa <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_oa_plot_%s.png", Sys.Date())
      },
      content = function(file) {
        req(resolution_rv$person_data)

        oa_yearly <- compute_oa_by_year(resolution_rv$person_data, from_year = 2010)

        if (is.null(oa_yearly) || nrow(oa_yearly) == 0) {
          p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No OA data available") +
            ggplot2::theme_void()
        } else {
          p <- ggplot2::ggplot(oa_yearly, ggplot2::aes(x = year, y = pct_oa)) +
            ggplot2::geom_col(fill = "coral", alpha = 0.7) +
            ggplot2::geom_line(color = "darkred", size = 1) +
            ggplot2::geom_point(color = "darkred", size = 2) +
            ggplot2::labs(
              title = "Open Access Publishing Trend",
              x = "Year",
              y = "Open Access %"
            ) +
            ggplot2::ylim(0, 100) +
            ggplot2::theme_minimal(base_size = 14)
        }

        ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)
      }
    )

    # Generate comprehensive report
    output$export_report <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_report_%s.html", Sys.Date())
      },
      content = function(file) {
        req(metrics_df(), resolution_rv$person_data)

        m <- metrics_df()
        summary_stats <- compute_division_summary(m)

        # Generate HTML report
        html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <title>%s</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; max-width: 1200px; }
    h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
    h2 { color: #34495e; margin-top: 30px; }
    .summary-box { display: inline-block; padding: 20px; margin: 10px;
                   background: #ecf0f1; border-radius: 8px; text-align: center;
                   min-width: 150px; }
    .summary-value { font-size: 32px; font-weight: bold; color: #3498db; }
    .summary-label { font-size: 14px; color: #7f8c8d; margin-top: 5px; }
    table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 10px; text-align: left; }
    th { background-color: #3498db; color: white; }
    tr:nth-child(even) { background-color: #f9f9f9; }
    .footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd;
              color: #7f8c8d; font-size: 12px; }
  </style>
</head>
<body>
  <h1>%s</h1>
  <p><strong>Division:</strong> %s</p>
  <p><strong>Report Date:</strong> %s</p>
  <p><strong>Data Coverage:</strong> %d of %d faculty members with indexed data</p>

  <h2>Summary Metrics</h2>
  <div class="summary-box">
    <div class="summary-value">%d</div>
    <div class="summary-label">Faculty Members</div>
  </div>
  <div class="summary-box">
    <div class="summary-value">%s</div>
    <div class="summary-label">Total Works</div>
  </div>
  <div class="summary-box">
    <div class="summary-value">%s</div>
    <div class="summary-label">Total Citations</div>
  </div>
  <div class="summary-box">
    <div class="summary-value">%s</div>
    <div class="summary-label">Median Works/Person</div>
  </div>
  <div class="summary-box">
    <div class="summary-value">%s</div>
    <div class="summary-label">Median h-index</div>
  </div>

  <h2>Top Faculty by Total Works</h2>
  <table>
    <tr><th>Name</th><th>Rank</th><th>Works</th><th>Citations</th><th>h-index</th></tr>
    %s
  </table>

  <h2>Most Cited Works</h2>
  <table>
    <tr><th>Title</th><th>Author</th><th>Year</th><th>Citations</th></tr>
    %s
  </table>

  <div class="footer">
    <p>Generated by FacultyIQ - Academic Research Analytics</p>
    <p>Data sources: OpenAlex, Google Scholar, Scopus (where configured)</p>
    <p>Note: Metrics are based on indexed publications and may not include all scholarly output.</p>
  </div>
</body>
</html>',
                                 input$report_title,
                                 input$report_title,
                                 input$report_division,
                                 format(input$report_date, "%B %d, %Y"),
                                 summary_stats$n_with_data,
                                 summary_stats$n_people,
                                 summary_stats$n_people,
                                 format(summary_stats$total_works, big.mark = ","),
                                 format(summary_stats$total_citations, big.mark = ","),
                                 summary_stats$median_works,
                                 summary_stats$median_h_index,
                                 # Top people rows
                                 paste(sapply(seq_len(min(10, nrow(m))), function(i) {
                                   row <- m %>%
                                     dplyr::arrange(dplyr::desc(works_count)) %>%
                                     dplyr::slice(i)
                                   sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                                           row$name,
                                           null_coalesce(row$academic_rank, "N/A"),
                                           format(null_coalesce(row$works_count, 0), big.mark = ","),
                                           format(null_coalesce(row$citations_count, 0), big.mark = ","),
                                           null_coalesce(row$h_index, "N/A"))
                                 }), collapse = "\n"),
                                 # Top works rows
                                 {
                                   top_works <- get_top_works(resolution_rv$person_data, n = 10)
                                   if (nrow(top_works) > 0) {
                                     paste(sapply(seq_len(nrow(top_works)), function(i) {
                                       row <- top_works[i, ]
                                       sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                                               stringr::str_trunc(row$title, 60),
                                               row$author_name,
                                               null_coalesce(row$publication_year, "N/A"),
                                               format(null_coalesce(row$cited_by_count, 0), big.mark = ","))
                                     }), collapse = "\n")
                                   } else {
                                     "<tr><td colspan='4'>No publication data available</td></tr>"
                                   }
                                 }
        )

        writeLines(html_content, file)
      }
    )

    # Cache statistics
    output$cache_stats <- shiny::renderText({
      stats <- cache_stats()
      sprintf(
        "Cache Statistics:\n- Files: %d\n- Total Size: %.2f MB\n- Oldest: %s\n- Newest: %s",
        stats$n_files,
        stats$total_size_mb,
        if (is.na(stats$oldest_file)) "N/A" else format(stats$oldest_file, "%Y-%m-%d %H:%M"),
        if (is.na(stats$newest_file)) "N/A" else format(stats$newest_file, "%Y-%m-%d %H:%M")
      )
    })

    # Clear expired cache
    shiny::observeEvent(input$clear_expired, {
      n_deleted <- cache_clear_expired(expiry_days = 7)
      shiny::showNotification(
        sprintf("Cleared %d expired cache files", n_deleted),
        type = "message"
      )
    })

    # Clear all cache
    shiny::observeEvent(input$clear_all, {
      shiny::showModal(shiny::modalDialog(
        title = "Confirm Cache Clear",
        "Are you sure you want to clear all cached data? This will require re-fetching from APIs.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_clear"), "Clear All", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_clear, {
      n_deleted <- cache_clear_all()
      shiny::removeModal()
      shiny::showNotification(
        sprintf("Cleared %d cache files", n_deleted),
        type = "message"
      )
    })

    return(NULL)
  })
}
