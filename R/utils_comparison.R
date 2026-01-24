# =============================================================================
# FacultyIQ - Faculty Comparison Utilities
# =============================================================================
# Functions for comparing faculty members side-by-side and analyzing trajectories

# =============================================================================
# Side-by-Side Comparison
# =============================================================================

#' Compare multiple faculty members
#'
#' @param metrics_df Full metrics data frame
#' @param roster_ids Vector of roster IDs to compare
#' @return Data frame with comparison data
compare_faculty <- function(metrics_df, roster_ids) {
  if (is.null(metrics_df) || length(roster_ids) == 0) {
    return(NULL)
  }

  comparison <- metrics_df %>%
    dplyr::filter(roster_id %in% roster_ids) %>%
    dplyr::select(
      roster_id, name, academic_rank,
      works_count, citations_count, h_index, i10_index,
      citations_per_work, works_per_year, oa_percentage,
      first_pub_year, last_pub_year, career_years,
      data_quality
    )

  return(comparison)
}

#' Create comparison summary statistics
#'
#' @param comparison Data frame from compare_faculty()
#' @return List with comparison insights
create_comparison_summary <- function(comparison) {
  if (is.null(comparison) || nrow(comparison) == 0) {
    return(NULL)
  }

  # Find leader in each metric
  leaders <- list(
    most_works = comparison$name[which.max(comparison$works_count)],
    most_citations = comparison$name[which.max(comparison$citations_count)],
    highest_h_index = comparison$name[which.max(comparison$h_index)],
    most_productive = comparison$name[which.max(comparison$works_per_year)],
    highest_impact = comparison$name[which.max(comparison$citations_per_work)],
    most_oa = comparison$name[which.max(comparison$oa_percentage)]
  )

  # Calculate ranges
  ranges <- list(
    works_range = range(comparison$works_count, na.rm = TRUE),
    citations_range = range(comparison$citations_count, na.rm = TRUE),
    h_index_range = range(comparison$h_index, na.rm = TRUE),
    career_range = range(comparison$career_years, na.rm = TRUE)
  )

  list(
    n_compared = nrow(comparison),
    leaders = leaders,
    ranges = ranges
  )
}

# =============================================================================
# Career Trajectory Analysis
# =============================================================================

#' Extract career trajectory for a person
#'
#' @param person_data Single person's data from fetch_person_data()
#' @return Data frame with yearly trajectory
extract_career_trajectory <- function(person_data) {
  if (is.null(person_data$openalex) ||
      is.null(person_data$openalex$counts_by_year)) {
    return(NULL)
  }

  cby <- person_data$openalex$counts_by_year

  if (!is.data.frame(cby) || nrow(cby) == 0) {
    return(NULL)
  }

  trajectory <- cby %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      cumulative_works = cumsum(works_count),
      cumulative_citations = cumsum(cited_by_count),
      # Rolling averages (3-year window)
      works_3yr_avg = zoo::rollmean(works_count, k = 3, fill = NA, align = "right"),
      citations_3yr_avg = zoo::rollmean(cited_by_count, k = 3, fill = NA, align = "right")
    )

  trajectory$name <- person_data$name
  trajectory$roster_id <- person_data$roster_id

  return(trajectory)
}

#' Analyze career phases
#'
#' @param trajectory Data frame from extract_career_trajectory()
#' @return List with phase analysis
analyze_career_phases <- function(trajectory) {
  if (is.null(trajectory) || nrow(trajectory) < 3) {
    return(NULL)
  }

  # Calculate productivity trend
  recent_5yr <- tail(trajectory, 5)
  early_5yr <- head(trajectory, 5)

  recent_avg <- mean(recent_5yr$works_count, na.rm = TRUE)
  early_avg <- mean(early_5yr$works_count, na.rm = TRUE)

  # Determine trend
  if (is.na(recent_avg) || is.na(early_avg)) {
    trend <- "unknown"
  } else if (recent_avg > early_avg * 1.2) {
    trend <- "increasing"
  } else if (recent_avg < early_avg * 0.8) {
    trend <- "decreasing"
  } else {
    trend <- "stable"
  }

  # Find peak year
  peak_year <- trajectory$year[which.max(trajectory$works_count)]
  peak_works <- max(trajectory$works_count, na.rm = TRUE)

  # Calculate career span
  active_years <- trajectory$year[trajectory$works_count > 0]
  career_start <- min(active_years, na.rm = TRUE)
  career_latest <- max(active_years, na.rm = TRUE)

  list(
    career_start = career_start,
    career_latest = career_latest,
    career_span = career_latest - career_start + 1,
    peak_year = peak_year,
    peak_works = peak_works,
    recent_avg_works = round(recent_avg, 1),
    early_avg_works = round(early_avg, 1),
    productivity_trend = trend,
    total_works = sum(trajectory$works_count, na.rm = TRUE),
    total_citations = sum(trajectory$cited_by_count, na.rm = TRUE)
  )
}

#' Compare trajectories of multiple faculty
#'
#' @param person_data_list List of person data
#' @param roster_ids Vector of roster IDs to compare
#' @param align_by How to align careers ("calendar" or "career_start")
#' @return Data frame with aligned trajectories
compare_trajectories <- function(person_data_list, roster_ids, align_by = "calendar") {
  trajectories <- list()

  for (pd in person_data_list) {
    if (pd$roster_id %in% roster_ids) {
      traj <- extract_career_trajectory(pd)
      if (!is.null(traj)) {
        trajectories[[length(trajectories) + 1]] <- traj
      }
    }
  }

  if (length(trajectories) == 0) {
    return(NULL)
  }

  combined <- dplyr::bind_rows(trajectories)

  if (align_by == "career_start") {
    # Align by years since first publication
    combined <- combined %>%
      dplyr::group_by(roster_id) %>%
      dplyr::mutate(
        career_year = year - min(year) + 1
      ) %>%
      dplyr::ungroup()
  }

  return(combined)
}

# =============================================================================
# Research Alerts
# =============================================================================

#' Identify faculty needing attention
#'
#' @param metrics_df Full metrics data frame
#' @param person_data_list List of person data for trajectory analysis
#' @param thresholds List of threshold values
#' @return Data frame with alerts
identify_research_alerts <- function(metrics_df, person_data_list = NULL,
                                     thresholds = NULL) {
  if (is.null(thresholds)) {
    thresholds <- list(
      min_recent_works = 1,          # At least 1 work in last 5 years
      min_works_per_year = 0.5,      # At least 0.5 works/year
      low_oa_pct = 25,               # Below 25% OA
      stagnant_h_index = 5,          # h-index below 5 after 10+ years
      productivity_drop_pct = 50     # 50% drop in recent productivity
    )
  }

  alerts <- list()

  for (i in seq_len(nrow(metrics_df))) {
    person <- metrics_df[i, ]
    person_alerts <- character()
    alert_levels <- character()

    # Check for low recent productivity
    if (!is.null(person_data_list)) {
      pd <- person_data_list[[which(sapply(person_data_list,
                                           function(x) x$roster_id) == person$roster_id)]]
      if (length(pd) > 0) {
        traj <- extract_career_trajectory(pd[[1]])
        if (!is.null(traj)) {
          current_year <- as.integer(format(Sys.Date(), "%Y"))
          recent <- traj %>% dplyr::filter(year >= current_year - 4)
          recent_works <- sum(recent$works_count, na.rm = TRUE)

          if (recent_works < thresholds$min_recent_works) {
            person_alerts <- c(person_alerts,
                               sprintf("No publications in last 5 years (found %d)", recent_works))
            alert_levels <- c(alert_levels, "high")
          }

          # Check for productivity decline
          phases <- analyze_career_phases(traj)
          if (!is.null(phases) && phases$productivity_trend == "decreasing") {
            if (phases$recent_avg_works < phases$early_avg_works * 0.5) {
              person_alerts <- c(person_alerts,
                                 sprintf("Significant productivity decline (%.1f to %.1f works/yr)",
                                         phases$early_avg_works, phases$recent_avg_works))
              alert_levels <- c(alert_levels, "medium")
            }
          }
        }
      }
    }

    # Check for low productivity rate
    if (!is.na(person$works_per_year) &&
        person$works_per_year < thresholds$min_works_per_year) {
      person_alerts <- c(person_alerts,
                         sprintf("Low productivity rate (%.1f works/year)", person$works_per_year))
      alert_levels <- c(alert_levels, "medium")
    }

    # Check for low OA percentage
    if (!is.na(person$oa_percentage) &&
        person$oa_percentage < thresholds$low_oa_pct) {
      person_alerts <- c(person_alerts,
                         sprintf("Low open access rate (%.0f%%)", person$oa_percentage))
      alert_levels <- c(alert_levels, "low")
    }

    # Check for stagnant h-index relative to career length
    if (!is.na(person$h_index) && !is.na(person$career_years)) {
      if (person$career_years >= 10 && person$h_index < thresholds$stagnant_h_index) {
        person_alerts <- c(person_alerts,
                           sprintf("h-index (%d) may be low for career length (%d years)",
                                   person$h_index, person$career_years))
        alert_levels <- c(alert_levels, "medium")
      }
    }

    # Check for data quality issues
    if (!is.na(person$data_quality) && person$data_quality == "unavailable") {
      person_alerts <- c(person_alerts, "No indexed publications found")
      alert_levels <- c(alert_levels, "high")
    }

    if (length(person_alerts) > 0) {
      alerts[[length(alerts) + 1]] <- data.frame(
        roster_id = person$roster_id,
        name = person$name,
        academic_rank = person$academic_rank,
        alert = person_alerts,
        level = alert_levels,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(alerts) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(alerts) %>%
    dplyr::mutate(
      level = factor(level, levels = c("high", "medium", "low"))
    ) %>%
    dplyr::arrange(level, name)
}

# =============================================================================
# Percentile Rankings
# =============================================================================

#' Calculate percentile rankings within the dataset
#'
#' @param metrics_df Full metrics data frame
#' @return Data frame with percentile columns added
calculate_percentile_rankings <- function(metrics_df) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return(metrics_df)
  }

  metrics_df %>%
    dplyr::mutate(
      works_percentile = dplyr::percent_rank(works_count) * 100,
      citations_percentile = dplyr::percent_rank(citations_count) * 100,
      h_index_percentile = dplyr::percent_rank(h_index) * 100,
      productivity_percentile = dplyr::percent_rank(works_per_year) * 100,
      impact_percentile = dplyr::percent_rank(citations_per_work) * 100
    )
}

#' Create individual performance card
#'
#' @param person_metrics Single row of metrics
#' @param division_summary Division-level summary for context
#' @return List with performance summary
create_performance_card <- function(person_metrics, division_summary) {
  if (is.null(person_metrics) || nrow(person_metrics) == 0) {
    return(NULL)
  }

  # Compare to division medians
  comparisons <- list(
    works = list(
      value = person_metrics$works_count,
      median = division_summary$median_works,
      status = if (!is.na(person_metrics$works_count) && !is.na(division_summary$median_works)) {
        if (person_metrics$works_count >= division_summary$median_works) "above" else "below"
      } else "unknown"
    ),
    citations = list(
      value = person_metrics$citations_count,
      median = division_summary$median_citations,
      status = if (!is.na(person_metrics$citations_count) && !is.na(division_summary$median_citations)) {
        if (person_metrics$citations_count >= division_summary$median_citations) "above" else "below"
      } else "unknown"
    ),
    h_index = list(
      value = person_metrics$h_index,
      median = division_summary$median_h_index,
      status = if (!is.na(person_metrics$h_index) && !is.na(division_summary$median_h_index)) {
        if (person_metrics$h_index >= division_summary$median_h_index) "above" else "below"
      } else "unknown"
    )
  )

  # Overall assessment
  above_count <- sum(sapply(comparisons, function(x) x$status == "above"))

  overall <- if (above_count >= 2) {
    "Above division average"
  } else if (above_count == 1) {
    "Near division average"
  } else {
    "Below division average"
  }

  list(
    name = person_metrics$name,
    rank = person_metrics$academic_rank,
    comparisons = comparisons,
    overall = overall,
    above_count = above_count
  )
}

# =============================================================================
# Publication Analysis
# =============================================================================

#' Analyze publication patterns
#'
#' @param works Data frame of publications
#' @return List with publication analysis
analyze_publications <- function(works) {
  if (is.null(works) || nrow(works) == 0) {
    return(NULL)
  }

  # Journal distribution
  journal_counts <- works %>%
    dplyr::filter(!is.na(journal)) %>%
    dplyr::count(journal, sort = TRUE) %>%
    head(10)

  # Year distribution
  year_counts <- works %>%
    dplyr::filter(!is.na(publication_year)) %>%
    dplyr::count(publication_year) %>%
    dplyr::arrange(publication_year)

  # OA status distribution
  oa_counts <- works %>%
    dplyr::count(is_oa) %>%
    dplyr::mutate(
      status = ifelse(is_oa, "Open Access", "Closed"),
      pct = round(n / sum(n) * 100, 1)
    )

  # Citation distribution
  citation_stats <- list(
    total = sum(works$cited_by_count, na.rm = TRUE),
    mean = mean(works$cited_by_count, na.rm = TRUE),
    median = median(works$cited_by_count, na.rm = TRUE),
    max = max(works$cited_by_count, na.rm = TRUE),
    uncited = sum(works$cited_by_count == 0, na.rm = TRUE),
    highly_cited = sum(works$cited_by_count >= 50, na.rm = TRUE)
  )

  list(
    n_works = nrow(works),
    top_journals = journal_counts,
    by_year = year_counts,
    oa_distribution = oa_counts,
    citation_stats = citation_stats
  )
}

# =============================================================================
# Research Strength Areas
# =============================================================================

#' Identify research strengths based on publications
#'
#' @param works Data frame of publications with concepts
#' @return Data frame with research areas and strengths
identify_research_strengths <- function(works) {
  if (is.null(works) || nrow(works) == 0 || !"concepts" %in% names(works)) {
    return(NULL)
  }

  # Parse concepts (stored as semicolon-separated string)
  all_concepts <- works %>%
    dplyr::filter(!is.na(concepts)) %>%
    dplyr::pull(concepts) %>%
    strsplit("; ") %>%
    unlist()

  if (length(all_concepts) == 0) {
    return(NULL)
  }

  # Count concept occurrences
  concept_counts <- as.data.frame(table(all_concepts), stringsAsFactors = FALSE)
  names(concept_counts) <- c("concept", "count")

  concept_counts %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    head(15) %>%
    dplyr::mutate(
      pct = round(count / nrow(works) * 100, 1)
    )
}
