# =============================================================================
# FacultyIQ - Division Dashboard Module
# =============================================================================
# Division-level analytics and visualizations

#' Dashboard Module UI
#'
#' @param id Module namespace ID
mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Filters row
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::sliderInput(
                ns("year_range"),
                "Year Range",
                min = 1990,
                max = as.integer(format(Sys.Date(), "%Y")),
                value = c(2010, as.integer(format(Sys.Date(), "%Y"))),
                sep = "",
                step = 1
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                ns("rank_filter"),
                "Academic Rank",
                choices = c("All Ranks" = "all"),
                multiple = TRUE,
                selected = "all"
              )
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(
                ns("person_filter"),
                "Include/Exclude People",
                choices = c("All" = "all"),
                multiple = TRUE,
                selected = "all"
              )
            ),
            shiny::column(
              width = 2,
              shiny::radioButtons(
                ns("filter_mode"),
                "Filter Mode",
                choices = c("Include" = "include", "Exclude" = "exclude"),
                selected = "include",
                inline = TRUE
              )
            )
          )
        )
      )
    ),

    # KPI Cards
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("kpi_total_works"), width = 2),
      shinydashboard::valueBoxOutput(ns("kpi_total_citations"), width = 2),
      shinydashboard::valueBoxOutput(ns("kpi_median_works"), width = 2),
      shinydashboard::valueBoxOutput(ns("kpi_median_hindex"), width = 2),
      shinydashboard::valueBoxOutput(ns("kpi_median_oa"), width = 2),
      shinydashboard::valueBoxOutput(ns("kpi_data_coverage"), width = 2)
    ),

    shiny::hr(),

    # Time series plots
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Works Published per Year"),
          plotly::plotlyOutput(ns("plot_works_year"), height = "350px")
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Citations Received per Year"),
          plotly::plotlyOutput(ns("plot_citations_year"), height = "350px")
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Open Access Share Over Time"),
          plotly::plotlyOutput(ns("plot_oa_year"), height = "300px")
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Productivity by Academic Rank"),
          plotly::plotlyOutput(ns("plot_by_rank"), height = "300px")
        )
      )
    ),

    shiny::hr(),

    # Tables
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Top Faculty by Selected Metric"),
          shiny::selectInput(
            ns("top_people_metric"),
            "Sort by",
            choices = c(
              "Total Works" = "works_count",
              "Total Citations" = "citations_count",
              "h-index" = "h_index",
              "Recent Works (5 yr)" = "recent_works"
            ),
            selected = "works_count"
          ),
          DT::dataTableOutput(ns("table_top_people"))
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Most Cited Works"),
          DT::dataTableOutput(ns("table_top_works"))
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4("Individual Trajectories (Sparklines)"),
          shiny::helpText("Works per year for each faculty member"),
          DT::dataTableOutput(ns("table_sparklines"))
        )
      )
    ),

    # Metric definitions
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("info-circle"), " Metric Definitions"),
          shiny::actionLink(ns("show_definitions"), "Click to expand"),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] %% 2 == 1", ns("show_definitions")),
            shiny::hr(),
            shiny::uiOutput(ns("metric_definitions"))
          )
        )
      )
    )
  )
}

#' Dashboard Module Server
#'
#' @param id Module namespace ID
#' @param resolution_rv Reactive values from resolution module
#' @param roster_rv Reactive values from upload module
mod_dashboard_server <- function(id, resolution_rv, roster_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Computed metrics
    metrics_df <- shiny::reactive({
      req(resolution_rv$person_data)
      req(roster_rv$roster)

      compute_all_metrics(resolution_rv$person_data, roster_rv$roster)
    })

    # Recent metrics (5 year window)
    recent_metrics <- shiny::reactive({
      req(resolution_rv$person_data)
      compute_recent_metrics(resolution_rv$person_data, years = 5)
    })

    # Division summary
    division_summary <- shiny::reactive({
      req(metrics_df())
      compute_division_summary(metrics_df())
    })

    # Update filter choices when data is available
    shiny::observe({
      req(metrics_df())
      m <- metrics_df()

      # Update rank filter
      ranks <- unique(m$academic_rank[!is.na(m$academic_rank)])
      shiny::updateSelectInput(
        session, "rank_filter",
        choices = c("All Ranks" = "all", setNames(ranks, ranks)),
        selected = "all"
      )

      # Update person filter
      persons <- setNames(m$roster_id, m$name)
      shiny::updateSelectInput(
        session, "person_filter",
        choices = c("All" = "all", persons),
        selected = "all"
      )
    })

    # Filtered metrics
    filtered_metrics <- shiny::reactive({
      req(metrics_df())
      m <- metrics_df()

      # Apply rank filter
      if (!"all" %in% input$rank_filter && length(input$rank_filter) > 0) {
        m <- m %>% dplyr::filter(academic_rank %in% input$rank_filter)
      }

      # Apply person filter
      if (!"all" %in% input$person_filter && length(input$person_filter) > 0) {
        ids <- as.integer(input$person_filter)
        if (input$filter_mode == "include") {
          m <- m %>% dplyr::filter(roster_id %in% ids)
        } else {
          m <- m %>% dplyr::filter(!roster_id %in% ids)
        }
      }

      m
    })

    # KPI boxes
    output$kpi_total_works <- shinydashboard::renderValueBox({
      m <- filtered_metrics()
      val <- if (nrow(m) > 0) sum(m$works_count, na.rm = TRUE) else 0
      shinydashboard::valueBox(
        format(val, big.mark = ","),
        "Total Works",
        icon = shiny::icon("file-alt"),
        color = "blue"
      )
    })

    output$kpi_total_citations <- shinydashboard::renderValueBox({
      m <- filtered_metrics()
      val <- if (nrow(m) > 0) sum(m$citations_count, na.rm = TRUE) else 0
      shinydashboard::valueBox(
        format(val, big.mark = ","),
        "Total Citations",
        icon = shiny::icon("quote-right"),
        color = "green"
      )
    })

    output$kpi_median_works <- shinydashboard::renderValueBox({
      m <- filtered_metrics()
      val <- if (nrow(m) > 0) round(median(m$works_count, na.rm = TRUE), 0) else 0
      shinydashboard::valueBox(
        val,
        "Median Works/Person",
        icon = shiny::icon("chart-bar"),
        color = "purple"
      )
    })

    output$kpi_median_hindex <- shinydashboard::renderValueBox({
      m <- filtered_metrics()
      val <- if (nrow(m) > 0) round(median(m$h_index, na.rm = TRUE), 0) else 0
      shinydashboard::valueBox(
        val,
        "Median h-index",
        icon = shiny::icon("chart-line"),
        color = "orange"
      )
    })

    output$kpi_median_oa <- shinydashboard::renderValueBox({
      m <- filtered_metrics()
      val <- if (nrow(m) > 0) round(median(m$oa_percentage, na.rm = TRUE), 0) else 0
      shinydashboard::valueBox(
        paste0(val, "%"),
        "Median OA %",
        icon = shiny::icon("unlock"),
        color = "teal"
      )
    })

    output$kpi_data_coverage <- shinydashboard::renderValueBox({
      m <- filtered_metrics()
      if (nrow(m) > 0) {
        n_with_data <- sum(m$data_quality != "unavailable", na.rm = TRUE)
        pct <- round(100 * n_with_data / nrow(m), 0)
      } else {
        pct <- 0
      }
      shinydashboard::valueBox(
        paste0(pct, "%"),
        "Data Coverage",
        icon = shiny::icon("database"),
        color = if (pct >= 80) "green" else if (pct >= 50) "yellow" else "red"
      )
    })

    # Works per year plot
    output$plot_works_year <- plotly::renderPlotly({
      req(resolution_rv$person_data)

      # Get filtered person IDs
      m <- filtered_metrics()
      filtered_ids <- m$roster_id

      # Create time series for filtered people
      yearly <- create_yearly_timeseries(
        resolution_rv$person_data,
        metric = "works",
        from_year = input$year_range[1],
        to_year = input$year_range[2]
      )

      if (nrow(yearly) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      yearly <- yearly %>%
        dplyr::filter(roster_id %in% filtered_ids)

      # Aggregate
      agg <- aggregate_division_yearly(yearly)

      if (nrow(agg) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      p <- ggplot2::ggplot(agg, ggplot2::aes(x = year)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75),
                             fill = "steelblue", alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(y = total), color = "steelblue", size = 1) +
        ggplot2::geom_line(ggplot2::aes(y = median), color = "darkblue",
                           linetype = "dashed", size = 0.8) +
        ggplot2::labs(x = "Year", y = "Works Count",
                      caption = "Solid: Total | Dashed: Median | Shaded: IQR") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Citations per year plot
    output$plot_citations_year <- plotly::renderPlotly({
      req(resolution_rv$person_data)

      m <- filtered_metrics()
      filtered_ids <- m$roster_id

      yearly <- create_yearly_timeseries(
        resolution_rv$person_data,
        metric = "citations",
        from_year = input$year_range[1],
        to_year = input$year_range[2]
      )

      if (nrow(yearly) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      yearly <- yearly %>%
        dplyr::filter(roster_id %in% filtered_ids)

      agg <- aggregate_division_yearly(yearly)

      if (nrow(agg) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      p <- ggplot2::ggplot(agg, ggplot2::aes(x = year)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75),
                             fill = "forestgreen", alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(y = total), color = "forestgreen", size = 1) +
        ggplot2::geom_line(ggplot2::aes(y = median), color = "darkgreen",
                           linetype = "dashed", size = 0.8) +
        ggplot2::labs(x = "Year", y = "Citations",
                      caption = "Solid: Total | Dashed: Median | Shaded: IQR") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # OA percentage over time
    output$plot_oa_year <- plotly::renderPlotly({
      req(resolution_rv$person_data)

      oa_yearly <- compute_oa_by_year(
        resolution_rv$person_data,
        from_year = input$year_range[1]
      )

      if (is.null(oa_yearly) || nrow(oa_yearly) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No OA data available"))
      }

      oa_yearly <- oa_yearly %>%
        dplyr::filter(year <= input$year_range[2])

      p <- ggplot2::ggplot(oa_yearly, ggplot2::aes(x = year, y = pct_oa)) +
        ggplot2::geom_col(fill = "coral", alpha = 0.7) +
        ggplot2::geom_line(color = "darkred", size = 1) +
        ggplot2::labs(x = "Year", y = "Open Access %") +
        ggplot2::ylim(0, 100) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Productivity by rank
    output$plot_by_rank <- plotly::renderPlotly({
      m <- filtered_metrics()

      if (!any(!is.na(m$academic_rank))) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No rank data available"))
      }

      rank_summary <- m %>%
        dplyr::filter(!is.na(academic_rank)) %>%
        dplyr::group_by(academic_rank) %>%
        dplyr::summarise(
          median_works = median(works_count, na.rm = TRUE),
          median_citations = median(citations_count, na.rm = TRUE),
          n = dplyr::n(),
          .groups = "drop"
        )

      # Order ranks
      rank_order <- c("Instructor", "Assistant Professor", "Associate Professor",
                      "Full Professor", "Emeritus", "Research Faculty",
                      "Clinical Faculty", "Adjunct")
      rank_summary$academic_rank <- factor(
        rank_summary$academic_rank,
        levels = intersect(rank_order, rank_summary$academic_rank)
      )

      p <- ggplot2::ggplot(rank_summary, ggplot2::aes(x = academic_rank, y = median_works)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("n=%d", n)),
                           vjust = -0.5, size = 3) +
        ggplot2::labs(x = "Academic Rank", y = "Median Works") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    })

    # Top people table
    output$table_top_people <- DT::renderDataTable({
      m <- filtered_metrics()

      if (nrow(m) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      # Add recent metrics if available
      recent <- recent_metrics()
      if (!is.null(recent) && nrow(recent) > 0) {
        m <- m %>%
          dplyr::left_join(
            recent %>% dplyr::select(roster_id, recent_works, recent_citations),
            by = "roster_id"
          )
      }

      metric_col <- input$top_people_metric

      if (!metric_col %in% names(m)) {
        return(DT::datatable(data.frame(Message = "Metric not available")))
      }

      top_df <- m %>%
        dplyr::filter(!is.na(.data[[metric_col]])) %>%
        dplyr::arrange(dplyr::desc(.data[[metric_col]])) %>%
        head(15) %>%
        dplyr::select(name, academic_rank, works_count, citations_count,
                      h_index, dplyr::any_of(c("recent_works")))

      DT::datatable(
        top_df,
        options = list(
          pageLength = 10,
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # Top works table
    output$table_top_works <- DT::renderDataTable({
      req(resolution_rv$person_data)

      m <- filtered_metrics()
      filtered_ids <- m$roster_id

      # Filter person_data to only include filtered people
      filtered_person_data <- resolution_rv$person_data[
        sapply(resolution_rv$person_data, function(x) x$roster_id %in% filtered_ids)
      ]

      top_works <- get_top_works(filtered_person_data, n = 15)

      if (nrow(top_works) == 0) {
        return(DT::datatable(data.frame(Message = "No works data available")))
      }

      display_df <- top_works %>%
        dplyr::select(title, author_name, publication_year,
                      cited_by_count, journal, is_oa) %>%
        dplyr::mutate(
          title = stringr::str_trunc(title, 60),
          journal = stringr::str_trunc(journal, 30)
        )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          dom = "tp",
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Title", "Author", "Year", "Citations", "Journal", "OA")
      )
    })

    # Sparklines table
    output$table_sparklines <- DT::renderDataTable({
      req(resolution_rv$person_data)

      m <- filtered_metrics()
      filtered_ids <- m$roster_id

      yearly <- create_yearly_timeseries(
        resolution_rv$person_data,
        metric = "works",
        from_year = max(1990, input$year_range[1]),
        to_year = input$year_range[2]
      )

      if (nrow(yearly) == 0) {
        return(DT::datatable(data.frame(Message = "No time series data")))
      }

      yearly <- yearly %>%
        dplyr::filter(roster_id %in% filtered_ids)

      # Create sparkline data for each person
      sparkline_data <- yearly %>%
        dplyr::group_by(roster_id, name) %>%
        dplyr::arrange(year) %>%
        dplyr::summarise(
          sparkline = paste(value, collapse = ","),
          total = sum(value, na.rm = TRUE),
          .groups = "drop"
        )

      # Join with metrics
      sparkline_df <- sparkline_data %>%
        dplyr::left_join(
          m %>% dplyr::select(roster_id, academic_rank, h_index),
          by = "roster_id"
        ) %>%
        dplyr::arrange(dplyr::desc(total)) %>%
        head(30)

      # Create sparkline column using sparkline package
      # Note: This requires sparkline to be loaded
      sparkline_df$Trend <- sapply(sparkline_df$sparkline, function(s) {
        vals <- as.numeric(strsplit(s, ",")[[1]])
        as.character(
          htmltools::as.tags(
            sparkline::sparkline(vals, type = "line", width = 100, height = 20)
          )
        )
      })

      display_df <- sparkline_df %>%
        dplyr::select(name, academic_rank, total, h_index, Trend)

      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th("Name"),
            th("Rank"),
            th("Total Works"),
            th("h-index"),
            th("Trend")
          )
        )
      ))

      DT::datatable(
        display_df,
        container = sketch,
        escape = FALSE,
        options = list(
          pageLength = 15,
          dom = "tp",
          fnDrawCallback = htmlwidgets::JS(
            "function(){ HTMLWidgets.staticRender(); }"
          )
        ),
        rownames = FALSE
      ) %>%
        sparkline::spk_add_deps()
    })

    # Metric definitions
    output$metric_definitions <- shiny::renderUI({
      shiny::tagList(
        lapply(names(METRIC_DEFINITIONS), function(key) {
          def <- METRIC_DEFINITIONS[[key]]
          shiny::div(
            class = "well well-sm",
            shiny::strong(def$name), " (", def$short_name, ")",
            shiny::br(),
            def$description,
            shiny::br(),
            shiny::em("Source: ", def$source),
            if (!is.null(def$caveat)) {
              shiny::span(
                shiny::br(),
                shiny::icon("exclamation-triangle"),
                " ", def$caveat,
                style = "color: #856404;"
              )
            }
          )
        })
      )
    })

    return(NULL)
  })
}
