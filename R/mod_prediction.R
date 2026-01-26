# =============================================================================
# FacultyIQ - Faculty Rank Prediction Module
# =============================================================================
# Predict faculty rank based on metrics and compare to similar faculty

# Helper for NULL coalescing
null_coalesce <- function(x, y) if (is.null(x) || is.na(x)) y else x

#' Prediction Module UI
#'
#' @param id Module namespace ID
mod_prediction_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Overview section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("chart-bar"), " Rank Prediction Analytics"),
          shiny::p(
            "This module predicts appropriate faculty ranks based on research metrics ",
            "and identifies faculty who may be ready for promotion by comparing their ",
            "metrics to benchmarks from similar faculty."
          ),
          shiny::p(
            shiny::em("Note: Predictions are based solely on research metrics and should be ",
                      "considered as one input among many factors in promotion decisions.")
          )
        )
      )
    ),

    # Rank benchmarks section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("ruler"), " Rank Benchmarks"),
          shiny::helpText(
            "Median metrics by academic rank based on current faculty data. ",
            "These benchmarks are used for rank prediction."
          ),
          DT::dataTableOutput(ns("benchmarks_table")),
          shiny::hr(),
          plotly::plotlyOutput(ns("benchmarks_plot"), height = "350px")
        )
      )
    ),

    shiny::hr(),

    # Individual prediction section
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::h4(shiny::icon("user-graduate"), " Individual Prediction"),
          shiny::selectInput(
            ns("person_select"),
            "Select Faculty Member",
            choices = c("Select a person" = "")
          ),
          shiny::hr(),
          shiny::uiOutput(ns("prediction_result"))
        )
      ),
      shiny::column(
        width = 8,
        shiny::wellPanel(
          shiny::h4("Comparison to Rank Benchmarks"),
          shiny::uiOutput(ns("comparison_content"))
        )
      )
    ),

    shiny::hr(),

    # Promotion candidates section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("award"), " Promotion Candidates"),
          shiny::helpText(
            "Faculty members whose metrics meet or exceed the median benchmarks ",
            "for the next rank level in at least 2 of 3 key metrics (works, citations, h-index)."
          ),
          DT::dataTableOutput(ns("promotion_candidates_table"))
        )
      )
    ),

    shiny::hr(),

    # Similar faculty (k-NN) section
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("users"), " Similar Faculty"),
          shiny::helpText(
            "Faculty members with the most similar research profiles based on ",
            "publication count, citations, h-index, and career length."
          ),
          DT::dataTableOutput(ns("similar_faculty_table"))
        )
      )
    ),

    shiny::hr(),

    # All predictions table
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::h4(shiny::icon("list"), " All Predictions"),
          shiny::helpText(
            "Predicted ranks for all faculty members based on research metrics."
          ),
          DT::dataTableOutput(ns("all_predictions_table")),
          shiny::hr(),
          shiny::downloadButton(ns("download_predictions"), "Download Predictions CSV")
        )
      )
    )
  )
}

#' Prediction Module Server
#'
#' @param id Module namespace ID
#' @param resolution_rv Reactive values from resolution module
#' @param roster_rv Reactive values from upload module
mod_prediction_server <- function(id, resolution_rv, roster_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Computed metrics
    metrics_df <- shiny::reactive({
      req(resolution_rv$person_data)
      req(roster_rv$roster)

      compute_all_metrics(resolution_rv$person_data, roster_rv$roster)
    })

    # Rank benchmarks
    benchmarks <- shiny::reactive({
      req(metrics_df())
      compute_rank_benchmarks(metrics_df())
    })

    # All predictions
    all_predictions <- shiny::reactive({
      req(metrics_df())
      predict_all_ranks(metrics_df())
    })

    # Promotion candidates
    promotion_candidates <- shiny::reactive({
      req(metrics_df())
      req(benchmarks())
      identify_promotion_candidates(metrics_df(), benchmarks())
    })

    # Update person selector
    shiny::observe({
      req(metrics_df())
      m <- metrics_df()

      persons <- setNames(m$roster_id, m$name)
      shiny::updateSelectInput(
        session, "person_select",
        choices = c("Select a person" = "", persons)
      )
    })

    # Selected person's metrics
    selected_person_metrics <- shiny::reactive({
      req(input$person_select)
      req(metrics_df())

      person_id <- as.integer(input$person_select)
      metrics_df() %>%
        dplyr::filter(roster_id == person_id)
    })

    # Individual prediction
    individual_prediction <- shiny::reactive({
      req(selected_person_metrics())
      req(benchmarks())

      person <- selected_person_metrics()
      if (nrow(person) == 0) return(NULL)

      predict_faculty_rank(person, benchmarks(), metrics_df())
    })

    # Benchmarks table
    output$benchmarks_table <- DT::renderDataTable({
      req(benchmarks())

      display_df <- benchmarks() %>%
        dplyr::select(
          academic_rank, n,
          works_median, works_q25, works_q75,
          citations_median, h_index_median, i10_index_median,
          career_years_median
        )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Rank", "N", "Works (Median)", "Works (25th)",
                     "Works (75th)", "Citations (Median)", "h-index (Median)",
                     "i10-index (Median)", "Career Years (Median)")
      ) %>%
        DT::formatRound(columns = c("works_median", "citations_median",
                                    "h_index_median", "i10_index_median",
                                    "career_years_median"), digits = 0)
    })

    # Benchmarks plot
    output$benchmarks_plot <- plotly::renderPlotly({
      req(benchmarks())

      bench_long <- benchmarks() %>%
        dplyr::select(academic_rank, works_median, citations_median, h_index_median) %>%
        tidyr::pivot_longer(
          cols = c(works_median, citations_median, h_index_median),
          names_to = "metric",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          metric = dplyr::case_when(
            metric == "works_median" ~ "Works",
            metric == "citations_median" ~ "Citations",
            metric == "h_index_median" ~ "h-index"
          ),
          academic_rank = factor(academic_rank, levels = RANK_ORDER)
        )

      # Normalize for better visualization (different scales)
      bench_norm <- bench_long %>%
        dplyr::group_by(metric) %>%
        dplyr::mutate(
          value_norm = value / max(value, na.rm = TRUE) * 100
        ) %>%
        dplyr::ungroup()

      p <- ggplot2::ggplot(bench_norm,
                           ggplot2::aes(x = academic_rank, y = value_norm,
                                        fill = metric, group = metric)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.8) +
        ggplot2::labs(
          x = "Academic Rank",
          y = "Normalized Value (% of max)",
          fill = "Metric"
        ) +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    })

    # Prediction result display
    output$prediction_result <- shiny::renderUI({
      if (is.null(input$person_select) || input$person_select == "") {
        return(shiny::helpText("Select a faculty member to see their predicted rank"))
      }

      pred <- individual_prediction()
      if (is.null(pred)) {
        return(shiny::div(
          class = "alert alert-warning",
          "Unable to generate prediction. Ensure the person has research metrics."
        ))
      }

      # Determine color based on prediction vs current
      current_level <- get_rank_level(pred$current_rank)
      predicted_level <- get_rank_level(pred$predicted_rank)

      if (is.na(current_level) || is.na(predicted_level)) {
        status_color <- "info"
        status_icon <- "question-circle"
      } else if (predicted_level > current_level) {
        status_color <- "success"
        status_icon <- "arrow-up"
      } else if (predicted_level < current_level) {
        status_color <- "warning"
        status_icon <- "arrow-down"
      } else {
        status_color <- "primary"
        status_icon <- "check"
      }

      shiny::tagList(
        shiny::div(
          class = paste0("alert alert-", status_color),
          shiny::h5(shiny::icon(status_icon), " Prediction Result"),
          shiny::p(
            shiny::strong("Current Rank: "),
            null_coalesce(pred$current_rank, "Unknown")
          ),
          shiny::p(
            shiny::strong("Predicted Rank: "),
            null_coalesce(pred$predicted_rank, "Unknown")
          ),
          shiny::p(
            shiny::strong("Confidence: "),
            if (!is.na(pred$confidence)) {
              sprintf("%.0f%%", pred$confidence * 100)
            } else {
              "N/A"
            }
          )
        ),
        shiny::hr(),
        shiny::p(
          shiny::icon("lightbulb"),
          " ",
          shiny::em(pred$recommendation)
        )
      )
    })

    # Comparison content
    output$comparison_content <- shiny::renderUI({
      if (is.null(input$person_select) || input$person_select == "") {
        return(shiny::helpText("Select a faculty member to see comparisons"))
      }

      person <- selected_person_metrics()
      pred <- individual_prediction()

      if (is.null(pred) || is.null(pred$rank_distances)) {
        return(shiny::helpText("Comparison data not available"))
      }

      shiny::tagList(
        # Distance to each rank
        shiny::h5("Distance to Each Rank"),
        shiny::helpText("Lower distance indicates better fit to that rank"),
        DT::dataTableOutput(ns("distance_table")),

        shiny::hr(),

        # Similar faculty
        if (!is.null(pred$nearest_neighbors) && nrow(pred$nearest_neighbors) > 0) {
          shiny::tagList(
            shiny::h5("Most Similar Faculty (k-NN)"),
            DT::dataTableOutput(ns("neighbors_table"))
          )
        },

        shiny::hr(),

        # Visual comparison
        shiny::h5("Metrics Comparison"),
        plotly::plotlyOutput(ns("comparison_plot"), height = "300px")
      )
    })

    # Distance table
    output$distance_table <- DT::renderDataTable({
      pred <- individual_prediction()
      req(pred)
      req(pred$rank_distances)

      display_df <- pred$rank_distances %>%
        dplyr::select(academic_rank, n, distance) %>%
        dplyr::mutate(
          distance = round(distance, 3),
          match = dplyr::case_when(
            dplyr::row_number() == 1 ~ "Best Match",
            dplyr::row_number() == 2 ~ "Second",
            TRUE ~ ""
          )
        )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 5,
          dom = "t"
        ),
        rownames = FALSE,
        colnames = c("Rank", "N Faculty", "Distance", "Match")
      ) %>%
        DT::formatStyle(
          "match",
          backgroundColor = DT::styleEqual(
            c("Best Match", "Second"),
            c("#d4edda", "#fff3cd")
          )
        )
    })

    # Neighbors table
    output$neighbors_table <- DT::renderDataTable({
      pred <- individual_prediction()
      req(pred)
      req(pred$nearest_neighbors)

      display_df <- pred$nearest_neighbors %>%
        dplyr::select(name, academic_rank, works_count, citations_count, h_index, distance) %>%
        dplyr::mutate(distance = round(distance, 3))

      DT::datatable(
        display_df,
        options = list(
          pageLength = 5,
          dom = "t"
        ),
        rownames = FALSE,
        colnames = c("Name", "Rank", "Works", "Citations", "h-index", "Distance")
      )
    })

    # Comparison plot
    output$comparison_plot <- plotly::renderPlotly({
      person <- selected_person_metrics()
      bench <- benchmarks()

      req(person)
      req(bench)

      if (nrow(person) == 0 || nrow(bench) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      # Get person's values
      p_works <- person$works_count[1]
      p_citations <- person$citations_count[1]
      p_h_index <- person$h_index[1]

      # Create comparison data
      comp_data <- bench %>%
        dplyr::select(academic_rank, works_median, citations_median, h_index_median) %>%
        dplyr::mutate(type = "Benchmark") %>%
        dplyr::rename(works = works_median, citations = citations_median, h_index = h_index_median)

      person_row <- data.frame(
        academic_rank = "This Person",
        works = p_works,
        citations = p_citations,
        h_index = p_h_index,
        type = "Person",
        stringsAsFactors = FALSE
      )

      comp_data <- dplyr::bind_rows(comp_data, person_row)

      # Normalize values for radar-style visualization
      comp_long <- comp_data %>%
        tidyr::pivot_longer(
          cols = c(works, citations, h_index),
          names_to = "metric",
          values_to = "value"
        )

      # Simple bar comparison
      p <- ggplot2::ggplot(comp_long,
                           ggplot2::aes(x = academic_rank, y = value, fill = metric)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.8) +
        ggplot2::labs(x = "", y = "Value", fill = "Metric") +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    })

    # Promotion candidates table
    output$promotion_candidates_table <- DT::renderDataTable({
      candidates <- promotion_candidates()

      if (is.null(candidates) || nrow(candidates) == 0) {
        return(DT::datatable(
          data.frame(Message = "No promotion candidates identified based on current criteria"),
          options = list(dom = "t")
        ))
      }

      display_df <- candidates %>%
        dplyr::select(name, current_rank, potential_rank, criteria_met,
                      works_status, citations_status, h_index_status)

      DT::datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Name", "Current Rank", "Potential Rank", "Criteria Met",
                     "Works", "Citations", "h-index")
      ) %>%
        DT::formatStyle(
          "criteria_met",
          backgroundColor = DT::styleInterval(
            c(2, 3),
            c("#fff3cd", "#d4edda", "#d4edda")
          )
        ) %>%
        DT::formatStyle(
          c("works_status", "citations_status", "h_index_status"),
          backgroundColor = DT::styleEqual(
            c("Meets", "Below"),
            c("#d4edda", "#f8d7da")
          )
        )
    })

    # Similar faculty table (for selected person)
    output$similar_faculty_table <- DT::renderDataTable({
      if (is.null(input$person_select) || input$person_select == "") {
        return(DT::datatable(
          data.frame(Message = "Select a faculty member to see similar colleagues"),
          options = list(dom = "t")
        ))
      }

      person <- selected_person_metrics()
      req(person)

      if (nrow(person) == 0 || is.na(person$works_count)) {
        return(DT::datatable(
          data.frame(Message = "Selected person has insufficient data"),
          options = list(dom = "t")
        ))
      }

      neighbors <- find_k_nearest_neighbors(person, metrics_df(), k = 10)

      if (is.null(neighbors) || nrow(neighbors) == 0) {
        return(DT::datatable(
          data.frame(Message = "No similar faculty found"),
          options = list(dom = "t")
        ))
      }

      display_df <- neighbors %>%
        dplyr::select(name, academic_rank, works_count, citations_count,
                      h_index, career_years, distance) %>%
        dplyr::mutate(distance = round(distance, 3))

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Name", "Rank", "Works", "Citations", "h-index",
                     "Career Years", "Distance")
      )
    })

    # All predictions table
    output$all_predictions_table <- DT::renderDataTable({
      preds <- all_predictions()

      if (is.null(preds) || nrow(preds) == 0) {
        return(DT::datatable(
          data.frame(Message = "No predictions available"),
          options = list(dom = "t")
        ))
      }

      display_df <- preds %>%
        dplyr::mutate(
          confidence = sprintf("%.0f%%", confidence * 100),
          match = dplyr::case_when(
            is.na(current_rank) ~ "Unknown",
            current_rank == predicted_rank ~ "Match",
            get_rank_level(predicted_rank) > get_rank_level(current_rank) ~ "Above",
            get_rank_level(predicted_rank) < get_rank_level(current_rank) ~ "Below",
            TRUE ~ "N/A"
          )
        )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("ID", "Name", "Current Rank", "Predicted Rank",
                     "Confidence", "Recommendation", "Status")
      ) %>%
        DT::formatStyle(
          "match",
          backgroundColor = DT::styleEqual(
            c("Match", "Above", "Below", "Unknown"),
            c("#d4edda", "#cce5ff", "#fff3cd", "#e2e3e5")
          )
        )
    })

    # Download predictions
    output$download_predictions <- shiny::downloadHandler(
      filename = function() {
        sprintf("facultyiq_predictions_%s.csv", Sys.Date())
      },
      content = function(file) {
        preds <- all_predictions()
        if (!is.null(preds)) {
          utils::write.csv(preds, file, row.names = FALSE)
        }
      }
    )

    return(NULL)
  })
}
