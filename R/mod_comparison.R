# =============================================================================
# FacultyIQ - Faculty Comparison Module
# =============================================================================
# Side-by-side comparison, trajectory analysis, and research alerts

#' Comparison Module UI
#'
#' @param id Module namespace ID
mod_comparison_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    # Comparison Tool Section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("balance-scale"), " Faculty Comparison Tool"),
          shiny::p("Select up to 4 faculty members to compare side-by-side."),
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::selectizeInput(
                ns("compare_faculty"),
                "Select Faculty to Compare",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 4, placeholder = "Select 2-4 faculty members...")
              )
            ),
            shiny::column(
              width = 4,
              shiny::br(),
              shiny::actionButton(
                ns("compare_btn"),
                "Compare",
                icon = shiny::icon("chart-bar"),
                class = "btn-primary"
              )
            )
          )
        )
      )
    ),

    # Comparison Results
    shiny::conditionalPanel(
      condition = sprintf("output['%s']", ns("comparison_ready")),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::wellPanel(
            shiny::h4("Comparison Results"),
            shiny::uiOutput(ns("comparison_summary")),
            shiny::hr(),
            DT::dataTableOutput(ns("comparison_table"))
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h5("Metrics Comparison"),
            plotly::plotlyOutput(ns("comparison_radar"), height = "400px")
          )
        ),
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h5("Career Trajectories"),
            shiny::radioButtons(
              ns("trajectory_metric"),
              "Metric",
              choices = c("Works" = "works", "Citations" = "citations"),
              selected = "works",
              inline = TRUE
            ),
            plotly::plotlyOutput(ns("trajectory_plot"), height = "350px")
          )
        )
      )
    ),

    shiny::hr(),

    # Individual Career Analysis Section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("chart-line"), " Individual Career Analysis"),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                ns("career_faculty"),
                "Select Faculty Member",
                choices = NULL
              )
            ),
            shiny::column(
              width = 8,
              shiny::uiOutput(ns("career_summary"))
            )
          ),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h5("Productivity Over Time"),
              plotly::plotlyOutput(ns("career_productivity"), height = "300px")
            ),
            shiny::column(
              width = 6,
              shiny::h5("Cumulative Impact"),
              plotly::plotlyOutput(ns("career_cumulative"), height = "300px")
            )
          ),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h5("Publication Analysis"),
              shiny::uiOutput(ns("publication_analysis"))
            ),
            shiny::column(
              width = 6,
              shiny::h5("Top Journals"),
              DT::dataTableOutput(ns("top_journals"))
            )
          )
        )
      )
    ),

    shiny::hr(),

    # Research Alerts Section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("bell"), " Research Alerts"),
          shiny::p("Faculty members who may need attention based on productivity metrics."),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::uiOutput(ns("alerts_summary"))
            )
          ),
          shiny::hr(),
          DT::dataTableOutput(ns("alerts_table"))
        )
      )
    ),

    shiny::hr(),

    # Percentile Rankings Section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("trophy"), " Division Rankings"),
          shiny::p("Faculty percentile rankings within the division."),
          shiny::selectInput(
            ns("ranking_metric"),
            "Rank By",
            choices = c(
              "Total Works" = "works_count",
              "Total Citations" = "citations_count",
              "h-index" = "h_index",
              "Works per Year" = "works_per_year",
              "Citations per Work" = "citations_per_work"
            )
          ),
          DT::dataTableOutput(ns("rankings_table"))
        )
      )
    )
  )
}

#' Comparison Module Server
#'
#' @param id Module namespace ID
#' @param resolution_rv Reactive values from resolution module
#' @param roster_rv Reactive values from upload module
mod_comparison_server <- function(id, resolution_rv, roster_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Computed metrics
    metrics_df <- shiny::reactive({
      req(resolution_rv$person_data)
      req(roster_rv$roster)
      compute_all_metrics(resolution_rv$person_data, roster_rv$roster)
    })

    # Division summary
    division_summary <- shiny::reactive({
      req(metrics_df())
      compute_division_summary(metrics_df())
    })

    # Comparison data (reactive)
    comparison_data <- shiny::reactiveVal(NULL)

    # Update faculty selectors
    shiny::observe({
      req(metrics_df())
      m <- metrics_df()

      choices <- setNames(m$roster_id, m$name)

      shiny::updateSelectizeInput(
        session, "compare_faculty",
        choices = choices,
        server = TRUE
      )

      shiny::updateSelectInput(
        session, "career_faculty",
        choices = c("Select..." = "", choices)
      )
    })

    # Handle comparison
    shiny::observeEvent(input$compare_btn, {
      req(input$compare_faculty)

      if (length(input$compare_faculty) < 2) {
        shiny::showNotification("Please select at least 2 faculty members",
                                type = "warning")
        return()
      }

      ids <- as.integer(input$compare_faculty)
      comparison <- compare_faculty(metrics_df(), ids)
      comparison_data(comparison)
    })

    # Comparison ready flag
    output$comparison_ready <- shiny::reactive({
      !is.null(comparison_data()) && nrow(comparison_data()) > 0
    })
    shiny::outputOptions(output, "comparison_ready", suspendWhenHidden = FALSE)

    # Comparison summary
    output$comparison_summary <- shiny::renderUI({
      req(comparison_data())

      summary_stats <- create_comparison_summary(comparison_data())

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "alert alert-info",
              shiny::strong("Most Works: "),
              summary_stats$leaders$most_works
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = "alert alert-success",
              shiny::strong("Most Citations: "),
              summary_stats$leaders$most_citations
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = "alert alert-warning",
              shiny::strong("Highest h-index: "),
              summary_stats$leaders$highest_h_index
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = "alert alert-primary",
              shiny::strong("Most Productive: "),
              summary_stats$leaders$most_productive
            )
          )
        )
      )
    })

    # Comparison table
    output$comparison_table <- DT::renderDataTable({
      req(comparison_data())

      display_df <- comparison_data() %>%
        dplyr::select(
          name, academic_rank, works_count, citations_count,
          h_index, i10_index, works_per_year, citations_per_work,
          oa_percentage, career_years
        ) %>%
        dplyr::mutate(
          works_per_year = round(works_per_year, 1),
          citations_per_work = round(citations_per_work, 1),
          oa_percentage = round(oa_percentage, 0)
        )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Name", "Rank", "Works", "Citations", "h-index",
                     "i10-index", "Works/Yr", "Cites/Work", "OA%", "Career Yrs")
      )
    })

    # Radar/bar chart for comparison
    output$comparison_radar <- plotly::renderPlotly({
      req(comparison_data())

      comp <- comparison_data()

      # Normalize metrics for comparison
      normalize <- function(x) {
        if (all(is.na(x)) || max(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
      }

      comp_norm <- comp %>%
        dplyr::mutate(
          works_norm = normalize(works_count),
          citations_norm = normalize(citations_count),
          h_index_norm = normalize(h_index),
          productivity_norm = normalize(works_per_year),
          impact_norm = normalize(citations_per_work)
        )

      # Create grouped bar chart
      plot_data <- comp_norm %>%
        dplyr::select(name, works_norm, citations_norm, h_index_norm,
                      productivity_norm, impact_norm) %>%
        tidyr::pivot_longer(
          cols = -name,
          names_to = "metric",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          metric = dplyr::case_when(
            metric == "works_norm" ~ "Works",
            metric == "citations_norm" ~ "Citations",
            metric == "h_index_norm" ~ "h-index",
            metric == "productivity_norm" ~ "Productivity",
            metric == "impact_norm" ~ "Impact"
          )
        )

      p <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = metric, y = value, fill = name)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.8) +
        ggplot2::labs(x = "", y = "Normalized Score (0-100)", fill = "Faculty") +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    })

    # Trajectory comparison plot
    output$trajectory_plot <- plotly::renderPlotly({
      req(comparison_data())
      req(resolution_rv$person_data)

      ids <- comparison_data()$roster_id
      trajectories <- compare_trajectories(resolution_rv$person_data, ids)

      if (is.null(trajectories) || nrow(trajectories) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No trajectory data available"))
      }

      metric_col <- if (input$trajectory_metric == "works") "works_count" else "cited_by_count"
      metric_label <- if (input$trajectory_metric == "works") "Works" else "Citations"

      # Filter to recent years
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      trajectories <- trajectories %>%
        dplyr::filter(year >= current_year - 20)

      p <- ggplot2::ggplot(trajectories,
                           ggplot2::aes_string(x = "year", y = metric_col,
                                               color = "name", group = "name")) +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(x = "Year", y = metric_label, color = "Faculty") +
        ggplot2::scale_color_brewer(palette = "Set2") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Individual career analysis
    selected_person_data <- shiny::reactive({
      req(input$career_faculty)
      req(resolution_rv$person_data)

      person_id <- as.integer(input$career_faculty)

      for (pd in resolution_rv$person_data) {
        if (pd$roster_id == person_id) {
          return(pd)
        }
      }
      return(NULL)
    })

    # Career trajectory
    career_trajectory <- shiny::reactive({
      req(selected_person_data())
      extract_career_trajectory(selected_person_data())
    })

    # Career summary
    output$career_summary <- shiny::renderUI({
      req(career_trajectory())

      phases <- analyze_career_phases(career_trajectory())

      if (is.null(phases)) {
        return(shiny::helpText("Insufficient data for career analysis"))
      }

      trend_color <- switch(phases$productivity_trend,
                            "increasing" = "success",
                            "decreasing" = "warning",
                            "stable" = "info",
                            "secondary")

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "text-center",
              shiny::h3(phases$total_works),
              shiny::p("Total Works")
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = "text-center",
              shiny::h3(format(phases$total_citations, big.mark = ",")),
              shiny::p("Total Citations")
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = "text-center",
              shiny::h3(sprintf("%d years", phases$career_span)),
              shiny::p("Career Span")
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = paste0("alert alert-", trend_color),
              shiny::strong("Trend: "),
              stringr::str_to_title(phases$productivity_trend),
              shiny::br(),
              sprintf("%.1f → %.1f works/yr", phases$early_avg_works, phases$recent_avg_works)
            )
          )
        )
      )
    })

    # Career productivity plot
    output$career_productivity <- plotly::renderPlotly({
      req(career_trajectory())

      traj <- career_trajectory()
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      traj <- traj %>% dplyr::filter(year >= current_year - 25)

      p <- ggplot2::ggplot(traj, ggplot2::aes(x = year)) +
        ggplot2::geom_col(ggplot2::aes(y = works_count),
                          fill = "steelblue", alpha = 0.7) +
        ggplot2::geom_line(ggplot2::aes(y = works_3yr_avg),
                           color = "darkblue", size = 1, na.rm = TRUE) +
        ggplot2::labs(x = "Year", y = "Works",
                      caption = "Bars: Annual | Line: 3-year average") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Cumulative impact plot
    output$career_cumulative <- plotly::renderPlotly({
      req(career_trajectory())

      traj <- career_trajectory()
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      traj <- traj %>% dplyr::filter(year >= current_year - 25)

      p <- ggplot2::ggplot(traj, ggplot2::aes(x = year)) +
        ggplot2::geom_area(ggplot2::aes(y = cumulative_works),
                           fill = "steelblue", alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(y = cumulative_works),
                           color = "steelblue", size = 1) +
        ggplot2::geom_area(ggplot2::aes(y = cumulative_citations / 10),
                           fill = "coral", alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(y = cumulative_citations / 10),
                           color = "coral", size = 1) +
        ggplot2::scale_y_continuous(
          name = "Cumulative Works",
          sec.axis = ggplot2::sec_axis(~.*10, name = "Cumulative Citations")
        ) +
        ggplot2::labs(x = "Year") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Publication analysis
    output$publication_analysis <- shiny::renderUI({
      req(selected_person_data())

      works <- selected_person_data()$works
      if (is.null(works) || nrow(works) == 0) {
        return(shiny::helpText("No publication data available"))
      }

      analysis <- analyze_publications(works)

      shiny::tagList(
        shiny::div(
          class = "well",
          shiny::fluidRow(
            shiny::column(6, shiny::strong("Total Publications: "), analysis$n_works),
            shiny::column(6, shiny::strong("Total Citations: "),
                          format(analysis$citation_stats$total, big.mark = ","))
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::strong("Mean Citations: "),
                          round(analysis$citation_stats$mean, 1)),
            shiny::column(6, shiny::strong("Median Citations: "),
                          analysis$citation_stats$median)
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::strong("Highly Cited (50+): "),
                          analysis$citation_stats$highly_cited),
            shiny::column(6, shiny::strong("Uncited: "),
                          analysis$citation_stats$uncited)
          )
        )
      )
    })

    # Top journals table
    output$top_journals <- DT::renderDataTable({
      req(selected_person_data())

      works <- selected_person_data()$works
      if (is.null(works) || nrow(works) == 0) {
        return(DT::datatable(data.frame(Message = "No data")))
      }

      analysis <- analyze_publications(works)

      DT::datatable(
        analysis$top_journals,
        options = list(dom = "t", pageLength = 10),
        rownames = FALSE,
        colnames = c("Journal", "Count")
      )
    })

    # Research alerts
    output$alerts_summary <- shiny::renderUI({
      req(metrics_df())

      alerts <- identify_research_alerts(metrics_df(), resolution_rv$person_data)

      if (nrow(alerts) == 0) {
        return(shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          " No alerts - all faculty members have healthy research metrics!"
        ))
      }

      high_count <- sum(alerts$level == "high")
      medium_count <- sum(alerts$level == "medium")
      low_count <- sum(alerts$level == "low")

      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::div(
            class = "alert alert-danger",
            shiny::icon("exclamation-circle"),
            sprintf(" High Priority: %d", high_count)
          )
        ),
        shiny::column(
          width = 4,
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            sprintf(" Medium Priority: %d", medium_count)
          )
        ),
        shiny::column(
          width = 4,
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            sprintf(" Low Priority: %d", low_count)
          )
        )
      )
    })

    # Alerts table
    output$alerts_table <- DT::renderDataTable({
      req(metrics_df())

      alerts <- identify_research_alerts(metrics_df(), resolution_rv$person_data)

      if (nrow(alerts) == 0) {
        return(DT::datatable(
          data.frame(Message = "No alerts at this time"),
          options = list(dom = "t")
        ))
      }

      DT::datatable(
        alerts %>% dplyr::select(name, academic_rank, alert, level),
        options = list(
          pageLength = 15,
          order = list(list(3, "asc"))
        ),
        rownames = FALSE,
        colnames = c("Name", "Rank", "Alert", "Priority")
      ) %>%
        DT::formatStyle(
          "level",
          backgroundColor = DT::styleEqual(
            c("high", "medium", "low"),
            c("#f8d7da", "#fff3cd", "#d1ecf1")
          )
        )
    })

    # Rankings table
    output$rankings_table <- DT::renderDataTable({
      req(metrics_df())

      ranked <- calculate_percentile_rankings(metrics_df())
      metric <- input$ranking_metric

      if (!metric %in% names(ranked)) {
        return(DT::datatable(data.frame(Message = "Metric not available")))
      }

      display_df <- ranked %>%
        dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
        dplyr::mutate(
          rank = dplyr::row_number(),
          percentile = round(dplyr::percent_rank(.data[[metric]]) * 100, 0)
        ) %>%
        dplyr::select(rank, name, academic_rank, !!metric, percentile)

      DT::datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Rank", "Name", "Academic Rank", "Value", "Percentile")
      ) %>%
        DT::formatStyle(
          "percentile",
          background = DT::styleColorBar(c(0, 100), "lightblue"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    return(NULL)
  })
}
