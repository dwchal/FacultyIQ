# =============================================================================
# FacultyIQ - Metrics Computation Utilities
# =============================================================================
# Functions for computing research productivity and impact metrics
# All metrics include clear definitions and handle missing data gracefully

# =============================================================================
# Metric Definitions
# =============================================================================
# These definitions are shown in the UI to explain each metric

METRIC_DEFINITIONS <- list(
  works_count = list(
    name = "Total Works",
    short_name = "Works",
    description = "Total number of scholarly works (articles, books, etc.) indexed in OpenAlex",
    source = "OpenAlex",
    caveat = "May not include all publications; depends on indexing coverage"
  ),
  citations_count = list(
    name = "Total Citations",
    short_name = "Citations",
    description = "Total number of times the author's works have been cited by other works",
    source = "OpenAlex/Scholar",
    caveat = "Citation counts vary between databases; not directly comparable across sources"
  ),
  h_index = list(
    name = "h-index",
    short_name = "h-index",
    description = "h papers have been cited at least h times each. Measures both productivity and impact.",
    source = "OpenAlex/Scholar",
    caveat = "Field-dependent; earlier career stages naturally have lower values"
  ),
  i10_index = list(
    name = "i10-index",
    short_name = "i10",
    description = "Number of publications with at least 10 citations",
    source = "OpenAlex/Scholar",
    caveat = "Google Scholar specific metric; may not be available from all sources"
  ),
  citations_per_work = list(
    name = "Citations per Work",
    short_name = "Cites/Work",
    description = "Average number of citations per published work",
    source = "Computed",
    caveat = "Heavily influenced by highly-cited papers; recent papers have fewer citations"
  ),
  works_per_year = list(
    name = "Works per Year",
    short_name = "Works/Year",
    description = "Average number of works published per year over career span",
    source = "Computed",
    caveat = "Calculated from first to last indexed publication year"
  ),
  oa_percentage = list(
    name = "Open Access %",
    short_name = "OA %",
    description = "Percentage of works that are open access (gold, green, hybrid, or bronze)",
    source = "OpenAlex",
    caveat = "Based on OpenAlex OA detection; may undercount some OA versions"
  ),
  reaims_pubs = list(
    name = "REAIMS Publications",
    short_name = "REAIMS",
    description = "Self-reported peer-reviewed publication count from internal REAIMS system",
    source = "Self-reported",
    caveat = "Self-reported data; may differ from indexed counts"
  )
)

# =============================================================================
# Individual Metrics
# =============================================================================

#' Compute metrics for a single person from their fetched data
#'
#' @param person_data List with person's data from fetch_person_data()
#' @param roster_row Original roster row for this person
#' @return List with computed metrics
compute_person_metrics <- function(person_data, roster_row = NULL) {
  metrics <- list(
    # Identity info
    name = person_data$name,
    roster_id = person_data$roster_id,

    # Raw counts (prefer OpenAlex, fallback to Scholar)
    works_count = NA_integer_,
    citations_count = NA_integer_,
    h_index = NA_integer_,
    i10_index = NA_integer_,

    # Computed metrics
    citations_per_work = NA_real_,
    works_per_year = NA_real_,
    oa_percentage = NA_real_,
    first_pub_year = NA_integer_,
    last_pub_year = NA_integer_,
    career_years = NA_integer_,

    # Comparison fields from roster
    reaims_pubs = NA_integer_,
    academic_rank = NA_character_,

    # Time series data
    yearly_works = NULL,
    yearly_citations = NULL,

    # Data quality
    data_sources = person_data$data_sources,
    data_quality = "unknown",
    unavailable_reason = character()
  )

  # Extract from roster if provided
  if (!is.null(roster_row)) {
    metrics$reaims_pubs <- as.integer(roster_row$reaims_pubs)
    metrics$academic_rank <- roster_row$academic_rank
  }

  # Get OpenAlex data if available
  if (!is.null(person_data$openalex)) {
    oa <- person_data$openalex

    metrics$works_count <- as.integer(oa$works_count)
    metrics$citations_count <- as.integer(oa$cited_by_count)
    metrics$h_index <- as.integer(oa$h_index)
    metrics$i10_index <- as.integer(oa$i10_index)

    # Extract yearly counts from OpenAlex counts_by_year
    if (!is.null(oa$counts_by_year) && is.data.frame(oa$counts_by_year)) {
      metrics$yearly_works <- oa$counts_by_year[, c("year", "works_count")]
      metrics$yearly_citations <- oa$counts_by_year[, c("year", "cited_by_count")]
      names(metrics$yearly_citations)[2] <- "citations"
      names(metrics$yearly_works)[2] <- "works"

      # Calculate career span
      if (nrow(oa$counts_by_year) > 0) {
        years_with_works <- oa$counts_by_year$year[oa$counts_by_year$works_count > 0]
        if (length(years_with_works) > 0) {
          metrics$first_pub_year <- min(years_with_works, na.rm = TRUE)
          metrics$last_pub_year <- max(years_with_works, na.rm = TRUE)
          metrics$career_years <- metrics$last_pub_year - metrics$first_pub_year + 1
        }
      }
    }

    metrics$data_quality <- "good"
  }

  # Supplement with Scholar data if OpenAlex incomplete
  if (!is.null(person_data$scholar)) {
    sch <- person_data$scholar

    if (is.na(metrics$citations_count) || metrics$citations_count == 0) {
      metrics$citations_count <- as.integer(sch$total_cites)
    }
    if (is.na(metrics$h_index) || metrics$h_index == 0) {
      metrics$h_index <- as.integer(sch$h_index)
    }
    if (is.na(metrics$i10_index) || metrics$i10_index == 0) {
      metrics$i10_index <- as.integer(sch$i10_index)
    }

    if (metrics$data_quality == "unknown") {
      metrics$data_quality <- "partial"
    }
  }

  # Calculate computed metrics
  if (!is.na(metrics$works_count) && metrics$works_count > 0) {
    if (!is.na(metrics$citations_count)) {
      metrics$citations_per_work <- round(metrics$citations_count / metrics$works_count, 2)
    }

    if (!is.na(metrics$career_years) && metrics$career_years > 0) {
      metrics$works_per_year <- round(metrics$works_count / metrics$career_years, 2)
    }
  }

  # Calculate OA percentage from works data
  if (!is.null(person_data$works) && nrow(person_data$works) > 0) {
    works <- person_data$works
    n_oa <- sum(works$is_oa == TRUE, na.rm = TRUE)
    metrics$oa_percentage <- round(100 * n_oa / nrow(works), 1)
  }

  # Determine unavailable reasons
  if (is.na(metrics$works_count)) {
    if (length(person_data$data_sources) == 0) {
      metrics$unavailable_reason <- c(metrics$unavailable_reason, "No IDs resolved")
    } else {
      metrics$unavailable_reason <- c(metrics$unavailable_reason, "API returned no data")
    }
    metrics$data_quality <- "unavailable"
  }

  return(metrics)
}

#' Compute metrics for all people in roster
#'
#' @param person_data_list List of person data from fetch_all_person_data()
#' @param roster Original roster data frame
#' @return Data frame with all computed metrics
compute_all_metrics <- function(person_data_list, roster) {
  metrics_list <- lapply(seq_along(person_data_list), function(i) {
    person_data <- person_data_list[[i]]
    roster_row <- roster[roster$id == person_data$roster_id, ]
    if (nrow(roster_row) == 0) roster_row <- NULL

    m <- compute_person_metrics(person_data, roster_row)

    # Convert to flat data frame row
    data.frame(
      roster_id = m$roster_id,
      name = m$name,
      academic_rank = m$academic_rank,
      works_count = m$works_count,
      citations_count = m$citations_count,
      h_index = m$h_index,
      i10_index = m$i10_index,
      citations_per_work = m$citations_per_work,
      works_per_year = m$works_per_year,
      oa_percentage = m$oa_percentage,
      first_pub_year = m$first_pub_year,
      last_pub_year = m$last_pub_year,
      career_years = m$career_years,
      reaims_pubs = m$reaims_pubs,
      data_quality = m$data_quality,
      data_sources = paste(m$data_sources, collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(metrics_list)
}

# =============================================================================
# Time Series Metrics
# =============================================================================

#' Create yearly time series for all people
#'
#' @param person_data_list List of person data
#' @param metric Type of metric ("works" or "citations")
#' @param from_year Start year
#' @param to_year End year
#' @return Data frame in long format
create_yearly_timeseries <- function(person_data_list, metric = "works",
                                      from_year = 2000, to_year = NULL) {
  if (is.null(to_year)) {
    to_year <- as.integer(format(Sys.Date(), "%Y"))
  }

  all_years <- seq(from_year, to_year)
  results <- list()

  for (i in seq_along(person_data_list)) {
    pd <- person_data_list[[i]]

    if (is.null(pd$openalex) || is.null(pd$openalex$counts_by_year)) {
      next
    }

    cby <- pd$openalex$counts_by_year

    if (metric == "works") {
      col_name <- "works_count"
    } else {
      col_name <- "cited_by_count"
    }

    if (!col_name %in% names(cby)) next

    # Create complete year range and merge
    year_df <- data.frame(year = all_years)
    cby_subset <- cby[, c("year", col_name)]
    names(cby_subset)[2] <- "value"

    merged <- dplyr::left_join(year_df, cby_subset, by = "year")
    merged$value[is.na(merged$value)] <- 0
    merged$name <- pd$name
    merged$roster_id <- pd$roster_id

    results[[i]] <- merged
  }

  if (length(results) == 0) {
    return(data.frame(year = integer(), value = numeric(),
                      name = character(), roster_id = integer()))
  }

  dplyr::bind_rows(results)
}

#' Aggregate division-level yearly metrics
#'
#' @param yearly_data Long format yearly data from create_yearly_timeseries
#' @return Data frame with division totals by year
aggregate_division_yearly <- function(yearly_data) {
  if (is.null(yearly_data) || nrow(yearly_data) == 0) {
    return(data.frame())
  }

  yearly_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      total = sum(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      q25 = stats::quantile(value, 0.25, na.rm = TRUE),
      q75 = stats::quantile(value, 0.75, na.rm = TRUE),
      n_people = dplyr::n_distinct(roster_id),
      .groups = "drop"
    )
}

# =============================================================================
# Division Summary Metrics
# =============================================================================

#' Compute division-level summary statistics
#'
#' @param metrics_df Data frame from compute_all_metrics
#' @return List with division summary
compute_division_summary <- function(metrics_df) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return(list(
      n_people = 0,
      total_works = NA,
      total_citations = NA,
      median_works = NA,
      median_citations = NA,
      median_h_index = NA
    ))
  }

  list(
    n_people = nrow(metrics_df),
    n_with_data = sum(metrics_df$data_quality != "unavailable", na.rm = TRUE),

    # Totals
    total_works = sum(metrics_df$works_count, na.rm = TRUE),
    total_citations = sum(metrics_df$citations_count, na.rm = TRUE),

    # Central tendency
    mean_works = round(mean(metrics_df$works_count, na.rm = TRUE), 1),
    median_works = stats::median(metrics_df$works_count, na.rm = TRUE),
    mean_citations = round(mean(metrics_df$citations_count, na.rm = TRUE), 1),
    median_citations = stats::median(metrics_df$citations_count, na.rm = TRUE),
    median_h_index = stats::median(metrics_df$h_index, na.rm = TRUE),

    # Dispersion
    iqr_works = stats::IQR(metrics_df$works_count, na.rm = TRUE),
    iqr_citations = stats::IQR(metrics_df$citations_count, na.rm = TRUE),

    # By rank (if available)
    by_rank = if ("academic_rank" %in% names(metrics_df) &&
                   any(!is.na(metrics_df$academic_rank))) {
      metrics_df %>%
        dplyr::filter(!is.na(academic_rank)) %>%
        dplyr::group_by(academic_rank) %>%
        dplyr::summarise(
          n = dplyr::n(),
          median_works = stats::median(works_count, na.rm = TRUE),
          median_citations = stats::median(citations_count, na.rm = TRUE),
          median_h_index = stats::median(h_index, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      NULL
    }
  )
}

# =============================================================================
# Top Works/People
# =============================================================================

#' Get top works across all people
#'
#' @param person_data_list List of person data
#' @param n Number of top works to return
#' @param sort_by Sort column ("cited_by_count" or "publication_year")
#' @return Data frame of top works
get_top_works <- function(person_data_list, n = 20, sort_by = "cited_by_count") {
  all_works <- list()

  for (i in seq_along(person_data_list)) {
    pd <- person_data_list[[i]]

    if (is.null(pd$works) || nrow(pd$works) == 0) next

    works <- pd$works
    works$author_name <- pd$name
    works$roster_id <- pd$roster_id
    all_works[[i]] <- works
  }

  if (length(all_works) == 0) {
    return(data.frame())
  }

  combined <- dplyr::bind_rows(all_works)

  # Sort and limit
  if (sort_by == "cited_by_count") {
    combined <- combined %>%
      dplyr::arrange(dplyr::desc(cited_by_count))
  } else {
    combined <- combined %>%
      dplyr::arrange(dplyr::desc(publication_year), dplyr::desc(cited_by_count))
  }

  head(combined, n)
}

#' Get top people by metric
#'
#' @param metrics_df Data frame from compute_all_metrics
#' @param metric Column name to sort by
#' @param n Number of top people
#' @return Data frame of top people
get_top_people <- function(metrics_df, metric = "works_count", n = 10) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return(data.frame())
  }

  if (!metric %in% names(metrics_df)) {
    return(data.frame())
  }

  metrics_df %>%
    dplyr::filter(!is.na(.data[[metric]])) %>%
    dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
    head(n)
}

# =============================================================================
# Recent Window Metrics
# =============================================================================

#' Compute metrics for a recent time window
#'
#' @param person_data_list List of person data
#' @param years Number of years to include
#' @return Data frame with recent metrics
compute_recent_metrics <- function(person_data_list, years = 5) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  from_year <- current_year - years + 1

  results <- list()

  for (i in seq_along(person_data_list)) {
    pd <- person_data_list[[i]]

    if (is.null(pd$openalex) || is.null(pd$openalex$counts_by_year)) {
      results[[i]] <- data.frame(
        name = pd$name,
        roster_id = pd$roster_id,
        recent_works = NA_integer_,
        recent_citations = NA_integer_,
        stringsAsFactors = FALSE
      )
      next
    }

    cby <- pd$openalex$counts_by_year
    recent <- cby[cby$year >= from_year, ]

    results[[i]] <- data.frame(
      name = pd$name,
      roster_id = pd$roster_id,
      recent_works = sum(recent$works_count, na.rm = TRUE),
      recent_citations = sum(recent$cited_by_count, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(results)
}

# =============================================================================
# Open Access Metrics
# =============================================================================

#' Compute OA metrics from works data
#'
#' @param person_data_list List of person data
#' @return Data frame with OA statistics
compute_oa_metrics <- function(person_data_list) {
  results <- list()

  for (i in seq_along(person_data_list)) {
    pd <- person_data_list[[i]]

    if (is.null(pd$works) || nrow(pd$works) == 0) {
      results[[i]] <- data.frame(
        name = pd$name,
        roster_id = pd$roster_id,
        n_works = NA_integer_,
        n_oa = NA_integer_,
        pct_oa = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    works <- pd$works
    n_works <- nrow(works)
    n_oa <- sum(works$is_oa == TRUE, na.rm = TRUE)

    results[[i]] <- data.frame(
      name = pd$name,
      roster_id = pd$roster_id,
      n_works = n_works,
      n_oa = n_oa,
      pct_oa = round(100 * n_oa / n_works, 1),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(results)
}

#' Get OA status breakdown by year
#'
#' @param person_data_list List of person data
#' @param from_year Start year
#' @return Data frame with OA breakdown
compute_oa_by_year <- function(person_data_list, from_year = 2010) {
  all_works <- list()

  for (i in seq_along(person_data_list)) {
    pd <- person_data_list[[i]]
    if (!is.null(pd$works) && nrow(pd$works) > 0) {
      all_works[[i]] <- pd$works
    }
  }

  if (length(all_works) == 0) {
    return(data.frame())
  }

  combined <- dplyr::bind_rows(all_works)

  combined %>%
    dplyr::filter(!is.na(publication_year), publication_year >= from_year) %>%
    dplyr::group_by(publication_year) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_oa = sum(is_oa == TRUE, na.rm = TRUE),
      pct_oa = round(100 * n_oa / n_total, 1),
      .groups = "drop"
    ) %>%
    dplyr::rename(year = publication_year)
}

# =============================================================================
# Collaboration Metrics
# =============================================================================

#' Compute basic collaboration metrics from works
#'
#' @param person_data_list List of person data
#' @return Data frame with collaboration stats
compute_collaboration_metrics <- function(person_data_list) {
  # Note: This is a simplified version. Full coauthor network
  # would require additional API calls.

  results <- list()

  for (i in seq_along(person_data_list)) {
    pd <- person_data_list[[i]]

    # OpenAlex works don't always include full author lists
    # This is a placeholder for more detailed implementation
    results[[i]] <- data.frame(
      name = pd$name,
      roster_id = pd$roster_id,
      avg_coauthors = NA_real_,  # Would need author counts per work
      unique_coauthors = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(results)
}

# =============================================================================
# Export Functions
# =============================================================================

#' Prepare metrics for export in wide format
#'
#' @param metrics_df Metrics data frame
#' @param roster Original roster
#' @return Data frame ready for CSV export
prepare_export_wide <- function(metrics_df, roster) {
  # Merge with original roster data
  export_df <- roster %>%
    dplyr::select(id, name, email, academic_rank, scopus_id, scholar_id) %>%
    dplyr::left_join(
      metrics_df %>% dplyr::select(-name, -academic_rank),
      by = c("id" = "roster_id")
    )

  return(export_df)
}

#' Prepare yearly metrics for export in long format
#'
#' @param yearly_data Yearly time series data
#' @return Data frame ready for CSV export
prepare_export_long <- function(yearly_data) {
  yearly_data %>%
    dplyr::arrange(name, year) %>%
    dplyr::select(name, roster_id, year, value)
}
