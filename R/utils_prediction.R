# =============================================================================
# FacultyIQ - Faculty Rank Prediction Utilities
# =============================================================================
# Functions for predicting faculty rank based on metrics from similar faculty
# Uses k-nearest neighbors and statistical comparison approaches

# =============================================================================
# Rank Definitions and Order
# =============================================================================

# Standard academic rank order (lowest to highest)
RANK_ORDER <- c(
  "Instructor",
  "Assistant Professor",
  "Associate Professor",
  "Full Professor",
  "Emeritus"
)

# Ranks that don't fit the standard tenure track
NON_TENURE_RANKS <- c(
  "Research Faculty",
  "Clinical Faculty",
  "Adjunct"
)

#' Get numeric rank level for comparison
#'
#' @param rank Academic rank string
#' @return Numeric level (1-5 for tenure track, NA for others)
get_rank_level <- function(rank) {
  if (is.na(rank) || rank == "") return(NA_integer_)

  level <- match(rank, RANK_ORDER)
  if (is.na(level)) return(NA_integer_)
  return(level)
}

# =============================================================================
# Feature Extraction for Prediction
# =============================================================================

#' Extract prediction features from metrics
#'
#' @param metrics_df Data frame from compute_all_metrics()
#' @return Data frame with normalized features for prediction
extract_prediction_features <- function(metrics_df) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return(NULL)
  }

  # Select relevant features for prediction
  features <- metrics_df %>%
    dplyr::select(
      roster_id,
      name,
      academic_rank,
      works_count,
      citations_count,
      h_index,
      i10_index,
      citations_per_work,
      works_per_year,
      oa_percentage,
      career_years
    ) %>%
    dplyr::mutate(
      # Add rank level for comparison
      rank_level = sapply(academic_rank, get_rank_level)
    )

  return(features)
}

#' Normalize features to 0-1 scale using min-max scaling
#'
#' @param features Data frame of features
#' @param feature_cols Columns to normalize
#' @return Data frame with normalized features
normalize_features <- function(features, feature_cols = NULL) {
  if (is.null(feature_cols)) {
    feature_cols <- c("works_count", "citations_count", "h_index", "i10_index",
                      "citations_per_work", "works_per_year", "career_years")
  }

  normalized <- features

  for (col in feature_cols) {
    if (col %in% names(features)) {
      vals <- features[[col]]
      min_val <- min(vals, na.rm = TRUE)
      max_val <- max(vals, na.rm = TRUE)

      if (max_val > min_val) {
        normalized[[paste0(col, "_norm")]] <- (vals - min_val) / (max_val - min_val)
      } else {
        normalized[[paste0(col, "_norm")]] <- 0.5
      }
    }
  }

  return(normalized)
}

# =============================================================================
# Rank Benchmarks Computation
# =============================================================================

#' Compute benchmark statistics by academic rank
#'
#' @param metrics_df Data frame from compute_all_metrics()
#' @return Data frame with benchmark statistics per rank
compute_rank_benchmarks <- function(metrics_df) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return(NULL)
  }

  # Filter to faculty with known ranks in the standard track
  ranked_df <- metrics_df %>%
    dplyr::filter(!is.na(academic_rank)) %>%
    dplyr::filter(academic_rank %in% RANK_ORDER) %>%
    dplyr::filter(!is.na(works_count))  # Need at least some data

  if (nrow(ranked_df) == 0) {
    return(NULL)
  }

  benchmarks <- ranked_df %>%
    dplyr::group_by(academic_rank) %>%
    dplyr::summarise(
      n = dplyr::n(),

      # Works statistics
      works_mean = mean(works_count, na.rm = TRUE),
      works_median = median(works_count, na.rm = TRUE),
      works_q25 = quantile(works_count, 0.25, na.rm = TRUE),
      works_q75 = quantile(works_count, 0.75, na.rm = TRUE),
      works_min = min(works_count, na.rm = TRUE),
      works_max = max(works_count, na.rm = TRUE),

      # Citations statistics
      citations_mean = mean(citations_count, na.rm = TRUE),
      citations_median = median(citations_count, na.rm = TRUE),
      citations_q25 = quantile(citations_count, 0.25, na.rm = TRUE),
      citations_q75 = quantile(citations_count, 0.75, na.rm = TRUE),

      # h-index statistics
      h_index_mean = mean(h_index, na.rm = TRUE),
      h_index_median = median(h_index, na.rm = TRUE),
      h_index_q25 = quantile(h_index, 0.25, na.rm = TRUE),
      h_index_q75 = quantile(h_index, 0.75, na.rm = TRUE),

      # i10-index statistics
      i10_index_mean = mean(i10_index, na.rm = TRUE),
      i10_index_median = median(i10_index, na.rm = TRUE),

      # Career statistics
      career_years_mean = mean(career_years, na.rm = TRUE),
      career_years_median = median(career_years, na.rm = TRUE),

      # Productivity statistics
      works_per_year_mean = mean(works_per_year, na.rm = TRUE),
      citations_per_work_mean = mean(citations_per_work, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      rank_level = sapply(academic_rank, get_rank_level)
    ) %>%
    dplyr::arrange(rank_level)

  return(benchmarks)
}

# =============================================================================
# Similarity and Distance Functions
# =============================================================================

#' Compute distance between a person and rank benchmarks
#'
#' @param person_metrics Single row of metrics for one person
#' @param benchmarks Data frame of rank benchmarks
#' @param weights Named vector of feature weights
#' @return Data frame with distance to each rank
compute_rank_distances <- function(person_metrics, benchmarks, weights = NULL) {
  if (is.null(weights)) {
    # Default weights emphasizing productivity metrics
    weights <- c(
      works_count = 1.0,
      citations_count = 1.0,
      h_index = 1.5,  # h-index is particularly important for rank
      i10_index = 0.8,
      career_years = 0.5,
      works_per_year = 0.7
    )
  }

  # Get person's values
  p_works <- person_metrics$works_count
  p_citations <- person_metrics$citations_count
  p_h_index <- person_metrics$h_index
  p_i10_index <- person_metrics$i10_index
  p_career <- person_metrics$career_years
  p_wpy <- person_metrics$works_per_year

  # Compute global scaling factors from benchmarks
  max_works <- max(benchmarks$works_median, na.rm = TRUE)
  max_citations <- max(benchmarks$citations_median, na.rm = TRUE)
  max_h_index <- max(benchmarks$h_index_median, na.rm = TRUE)
  max_i10 <- max(benchmarks$i10_index_median, na.rm = TRUE)
  max_career <- max(benchmarks$career_years_median, na.rm = TRUE)
  max_wpy <- max(benchmarks$works_per_year_mean, na.rm = TRUE)

  # Compute distance to each rank
  distances <- benchmarks %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Normalized differences (scaled to 0-1 range)
      diff_works = if (!is.na(p_works) && max_works > 0)
        abs((p_works - works_median) / max_works) else NA_real_,
      diff_citations = if (!is.na(p_citations) && max_citations > 0)
        abs((p_citations - citations_median) / max_citations) else NA_real_,
      diff_h_index = if (!is.na(p_h_index) && max_h_index > 0)
        abs((p_h_index - h_index_median) / max_h_index) else NA_real_,
      diff_i10 = if (!is.na(p_i10_index) && max_i10 > 0)
        abs((p_i10_index - i10_index_median) / max_i10) else NA_real_,
      diff_career = if (!is.na(p_career) && max_career > 0)
        abs((p_career - career_years_median) / max_career) else NA_real_,
      diff_wpy = if (!is.na(p_wpy) && max_wpy > 0)
        abs((p_wpy - works_per_year_mean) / max_wpy) else NA_real_,

      # Weighted distance (Euclidean)
      distance = sqrt(
        sum(c(
          (diff_works * weights["works_count"])^2,
          (diff_citations * weights["citations_count"])^2,
          (diff_h_index * weights["h_index"])^2,
          (diff_i10 * weights["i10_index"])^2,
          (diff_career * weights["career_years"])^2,
          (diff_wpy * weights["works_per_year"])^2
        ), na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(academic_rank, rank_level, n, distance) %>%
    dplyr::arrange(distance)

  return(distances)
}

#' Find k nearest neighbors from a reference set
#'
#' @param target_metrics Metrics for the target person
#' @param reference_metrics Metrics data frame for reference set
#' @param k Number of neighbors
#' @param feature_cols Columns to use for distance
#' @return Data frame of k nearest neighbors
find_k_nearest_neighbors <- function(target_metrics, reference_metrics, k = 5, feature_cols = NULL) {
  if (is.null(feature_cols)) {
    feature_cols <- c("works_count", "citations_count", "h_index", "i10_index",
                      "career_years", "works_per_year")
  }

  # Filter out target person and those without data
  ref_df <- reference_metrics %>%
    dplyr::filter(roster_id != target_metrics$roster_id) %>%
    dplyr::filter(!is.na(works_count))

  if (nrow(ref_df) == 0) {
    return(NULL)
  }

  # Get target values
  target_vals <- as.numeric(target_metrics[feature_cols])

  # Compute scaling factors
  scales <- sapply(feature_cols, function(col) {
    vals <- ref_df[[col]]
    max_val <- max(vals, na.rm = TRUE)
    if (is.na(max_val) || max_val == 0) 1 else max_val
  })

  # Compute distance to each reference person
  ref_df$distance <- sapply(seq_len(nrow(ref_df)), function(i) {
    ref_vals <- as.numeric(ref_df[i, feature_cols])

    # Normalized Euclidean distance
    diffs <- (target_vals - ref_vals) / scales
    diffs[is.na(diffs)] <- 0
    sqrt(sum(diffs^2))
  })

  # Get k nearest
  neighbors <- ref_df %>%
    dplyr::arrange(distance) %>%
    head(k) %>%
    dplyr::select(roster_id, name, academic_rank, distance,
                  works_count, citations_count, h_index, career_years)

  return(neighbors)
}

# =============================================================================
# Rank Prediction Functions
# =============================================================================

#' Predict appropriate rank based on metrics
#'
#' @param person_metrics Single row of metrics for one person
#' @param benchmarks Rank benchmarks from compute_rank_benchmarks()
#' @param reference_metrics Full metrics data frame for k-NN
#' @return List with prediction results
predict_faculty_rank <- function(person_metrics, benchmarks, reference_metrics = NULL) {
  result <- list(
    person_name = person_metrics$name,
    current_rank = person_metrics$academic_rank,
    predicted_rank = NA_character_,
    confidence = NA_real_,
    rank_distances = NULL,
    nearest_neighbors = NULL,
    recommendation = NA_character_
  )

  # Check if we have enough data
  if (is.na(person_metrics$works_count) || is.null(benchmarks) || nrow(benchmarks) == 0) {
    result$recommendation <- "Insufficient data for prediction"
    return(result)
  }

  # Method 1: Distance to rank benchmarks
  rank_distances <- compute_rank_distances(person_metrics, benchmarks)
  result$rank_distances <- rank_distances

  # Predicted rank is the closest
  predicted_rank <- rank_distances$academic_rank[1]
  min_distance <- rank_distances$distance[1]

  # Confidence based on distance ratio (closest vs second closest)
  if (nrow(rank_distances) >= 2) {
    second_distance <- rank_distances$distance[2]
    if (second_distance > 0) {
      # Higher ratio = more confident
      result$confidence <- round(1 - (min_distance / second_distance), 2)
      result$confidence <- max(0, min(1, result$confidence))  # Clamp to 0-1
    }
  }

  result$predicted_rank <- predicted_rank

  # Method 2: k-NN for supporting evidence
  if (!is.null(reference_metrics)) {
    neighbors <- find_k_nearest_neighbors(person_metrics, reference_metrics, k = 5)
    result$nearest_neighbors <- neighbors

    # Check if k-NN agrees with benchmark prediction
    if (!is.null(neighbors) && nrow(neighbors) > 0) {
      neighbor_ranks <- neighbors$academic_rank[!is.na(neighbors$academic_rank)]
      if (length(neighbor_ranks) > 0) {
        most_common_rank <- names(sort(table(neighbor_ranks), decreasing = TRUE))[1]

        # Adjust confidence if k-NN agrees
        if (most_common_rank == predicted_rank) {
          result$confidence <- min(1, result$confidence + 0.1)
        }
      }
    }
  }

  # Generate recommendation
  current_level <- get_rank_level(result$current_rank)
  predicted_level <- get_rank_level(result$predicted_rank)

  if (is.na(current_level)) {
    result$recommendation <- sprintf(
      "Based on metrics, this faculty member aligns most closely with %s rank.",
      result$predicted_rank
    )
  } else if (is.na(predicted_level)) {
    result$recommendation <- "Unable to determine predicted rank level."
  } else if (predicted_level > current_level) {
    result$recommendation <- sprintf(
      "Metrics suggest potential for promotion: currently %s, metrics align with %s.",
      result$current_rank, result$predicted_rank
    )
  } else if (predicted_level < current_level) {
    result$recommendation <- sprintf(
      "Metrics are below typical for %s. Current rank: %s, metrics align with %s.",
      result$current_rank, result$current_rank, result$predicted_rank
    )
  } else {
    result$recommendation <- sprintf(
      "Metrics are consistent with current rank of %s.",
      result$current_rank
    )
  }

  return(result)
}

#' Predict rank for all faculty in the dataset
#'
#' @param metrics_df Full metrics data frame
#' @return Data frame with predictions for each person
predict_all_ranks <- function(metrics_df) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return(NULL)
  }

  # Compute benchmarks from the data
  benchmarks <- compute_rank_benchmarks(metrics_df)

  if (is.null(benchmarks) || nrow(benchmarks) == 0) {
    return(NULL)
  }

  # Predict for each person
  predictions <- lapply(seq_len(nrow(metrics_df)), function(i) {
    person <- metrics_df[i, ]
    pred <- predict_faculty_rank(person, benchmarks, metrics_df)

    data.frame(
      roster_id = person$roster_id,
      name = person$name,
      current_rank = person$academic_rank,
      predicted_rank = pred$predicted_rank,
      confidence = pred$confidence,
      recommendation = pred$recommendation,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(predictions)
}

# =============================================================================
# Comparison and Gap Analysis
# =============================================================================

#' Compare person's metrics to rank benchmarks
#'
#' @param person_metrics Single row of metrics for one person
#' @param benchmarks Rank benchmarks
#' @return Data frame showing how person compares to each rank
compare_to_benchmarks <- function(person_metrics, benchmarks) {
  if (is.null(benchmarks) || nrow(benchmarks) == 0) {
    return(NULL)
  }

  comparison <- benchmarks %>%
    dplyr::mutate(
      # Position relative to median (as percentile)
      works_pct = dplyr::case_when(
        is.na(person_metrics$works_count) ~ NA_character_,
        person_metrics$works_count >= works_q75 ~ "Above 75th",
        person_metrics$works_count >= works_median ~ "50th-75th",
        person_metrics$works_count >= works_q25 ~ "25th-50th",
        TRUE ~ "Below 25th"
      ),
      citations_pct = dplyr::case_when(
        is.na(person_metrics$citations_count) ~ NA_character_,
        person_metrics$citations_count >= citations_q75 ~ "Above 75th",
        person_metrics$citations_count >= citations_median ~ "50th-75th",
        person_metrics$citations_count >= citations_q25 ~ "25th-50th",
        TRUE ~ "Below 25th"
      ),
      h_index_pct = dplyr::case_when(
        is.na(person_metrics$h_index) ~ NA_character_,
        person_metrics$h_index >= h_index_q75 ~ "Above 75th",
        person_metrics$h_index >= h_index_median ~ "50th-75th",
        person_metrics$h_index >= h_index_q25 ~ "25th-50th",
        TRUE ~ "Below 25th"
      ),

      # Gaps to median
      works_gap = person_metrics$works_count - works_median,
      citations_gap = person_metrics$citations_count - citations_median,
      h_index_gap = person_metrics$h_index - h_index_median
    ) %>%
    dplyr::select(
      academic_rank, n,
      works_median, works_gap, works_pct,
      citations_median, citations_gap, citations_pct,
      h_index_median, h_index_gap, h_index_pct
    )

  return(comparison)
}

#' Identify promotion-ready faculty
#'
#' @param metrics_df Full metrics data frame
#' @param benchmarks Rank benchmarks
#' @return Data frame of faculty who might be ready for promotion
identify_promotion_candidates <- function(metrics_df, benchmarks) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0 ||
      is.null(benchmarks) || nrow(benchmarks) == 0) {
    return(NULL)
  }

  candidates <- list()

  for (i in seq_len(nrow(metrics_df))) {
    person <- metrics_df[i, ]

    # Skip if no current rank or metrics
    if (is.na(person$academic_rank) || is.na(person$works_count)) {
      next
    }

    current_level <- get_rank_level(person$academic_rank)
    if (is.na(current_level) || current_level >= length(RANK_ORDER)) {
      next  # Already at top or non-tenure track
    }

    # Get next rank benchmarks
    next_rank <- RANK_ORDER[current_level + 1]
    next_benchmarks <- benchmarks %>%
      dplyr::filter(academic_rank == next_rank)

    if (nrow(next_benchmarks) == 0) next

    # Check if metrics meet or exceed next rank median
    meets_works <- !is.na(person$works_count) &&
      person$works_count >= next_benchmarks$works_median
    meets_citations <- !is.na(person$citations_count) &&
      person$citations_count >= next_benchmarks$citations_median
    meets_h_index <- !is.na(person$h_index) &&
      person$h_index >= next_benchmarks$h_index_median

    # Count how many criteria met
    criteria_met <- sum(c(meets_works, meets_citations, meets_h_index))

    if (criteria_met >= 2) {
      candidates[[length(candidates) + 1]] <- data.frame(
        roster_id = person$roster_id,
        name = person$name,
        current_rank = person$academic_rank,
        potential_rank = next_rank,
        criteria_met = criteria_met,
        works_status = ifelse(meets_works, "Meets", "Below"),
        citations_status = ifelse(meets_citations, "Meets", "Below"),
        h_index_status = ifelse(meets_h_index, "Meets", "Below"),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(candidates) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(candidates) %>%
    dplyr::arrange(dplyr::desc(criteria_met), name)
}
