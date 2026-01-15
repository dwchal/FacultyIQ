# =============================================================================
# FacultyIQ - Data Validation Utilities
# =============================================================================
# Functions for validating and cleaning roster data
# All functions are unit-testable and handle missing data gracefully

#' Expected column names in the roster file (from README specification)
#' These are the canonical names we normalize to internally
ROSTER_COLUMNS <- list(
  id = "id",
  start_time = "start_time",
  completion_time = "completion_time",
  email = "email",
  name = "name",
  academic_rank = "academic_rank",
  last_promotion = "last_promotion",
  reaims_pubs = "reaims_pubs",
  scopus_id = "scopus_id",
  scholar_id = "scholar_id",
  associations = "associations"
)

#' Column name mappings from various input formats to canonical names
COLUMN_MAPPINGS <- list(

  # ID variations
  "id" = "id",
  "Id" = "id",
  "ID" = "id",


  # Time variations
  "start time" = "start_time",
  "Start time" = "start_time",
  "start_time" = "start_time",
  "completion time" = "completion_time",
  "Completion time" = "completion_time",
  "completion_time" = "completion_time",

  # Email variations
  "email" = "email",
  "Email" = "email",
  "EMAIL" = "email",
  "e-mail" = "email",


  # Name variations
  "name" = "name",
  "Name" = "name",
  "NAME" = "name",
  "full name" = "name",
  "Full Name" = "name",

  # Academic rank variations
  "academic_rank" = "academic_rank",
  "academic rank" = "academic_rank",
  "Academic Rank" = "academic_rank",
  "What is your current academic rank?" = "academic_rank",
  "current academic rank" = "academic_rank",
  "rank" = "academic_rank",
  "Rank" = "academic_rank",

  # Promotion date variations
  "last_promotion" = "last_promotion",
  "last promotion" = "last_promotion",
  "What was the date/ year of your last academic promotion?" = "last_promotion",
  "promotion date" = "last_promotion",
  "promotion year" = "last_promotion",
  "date of last promotion" = "last_promotion",

  # REAIMS publications variations
  "reaims_pubs" = "reaims_pubs",
  "reaims publications" = "reaims_pubs",
  "How many peer-reviewed publications do you have in REAIMS?" = "reaims_pubs",
  "peer-reviewed publications" = "reaims_pubs",
  "publications in reaims" = "reaims_pubs",

  # Scopus ID variations
  "scopus_id" = "scopus_id",
  "scopus id" = "scopus_id",
  "Scopus ID" = "scopus_id",
  "What is your Scopus ID?" = "scopus_id",
  "scopusid" = "scopus_id",
  "ScopusID" = "scopus_id",

  # Google Scholar ID variations
  "scholar_id" = "scholar_id",
  "scholar id" = "scholar_id",
  "Google Scholar ID" = "scholar_id",
  "google scholar id" = "scholar_id",
  "What is your Google Scholar username? (highlighted in screenshot below, this includes the letters and numbers between V-J in the search bar)" = "scholar_id",
  "google scholar username" = "scholar_id",
  "scholar username" = "scholar_id",

  # Associations variations
  "associations" = "associations",
  "specialty associations" = "associations",
  "Please make note of any specialty associations (e.g IDSA, ACOEM), including regional offices you hold/ have held, national offices you hold/ have held, editorial positions, institutional services (e.g" = "associations",
  "leadership roles" = "associations",
  "service" = "associations"
)

#' Normalize column names from input file to canonical names
#'
#' @param col_names Character vector of column names from input file
#' @return Character vector of normalized column names
normalize_column_names <- function(col_names) {
  normalized <- sapply(col_names, function(col) {
    # Try exact match first
    if (col %in% names(COLUMN_MAPPINGS)) {
      return(COLUMN_MAPPINGS[[col]])
    }

    # Try case-insensitive partial match
    col_lower <- tolower(trimws(col))
    for (pattern in names(COLUMN_MAPPINGS)) {
      if (tolower(trimws(pattern)) == col_lower) {
        return(COLUMN_MAPPINGS[[pattern]])
      }
    }

    # Try partial matching for long column names
    for (pattern in names(COLUMN_MAPPINGS)) {
      pattern_lower <- tolower(trimws(pattern))
      if (nchar(pattern_lower) > 10 && grepl(pattern_lower, col_lower, fixed = TRUE)) {
        return(COLUMN_MAPPINGS[[pattern]])
      }
      if (nchar(col_lower) > 10 && grepl(col_lower, pattern_lower, fixed = TRUE)) {
        return(COLUMN_MAPPINGS[[pattern]])
      }
    }

    # Return original (cleaned) if no match
    return(janitor::make_clean_names(col))
  }, USE.NAMES = FALSE)

  # Handle duplicates by appending numbers
  normalized <- make.unique(normalized, sep = "_")
  return(normalized)
}

#' Validate roster data and return validation report
#'
#' @param data Data frame of roster data
#' @return List with validation results
validate_roster <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      valid = FALSE,
      errors = "No data provided or empty dataset",
      warnings = character(),
      info = character(),
      n_rows = 0,
      n_cols = 0,
      columns_found = character(),
      columns_missing = character(),
      completeness = data.frame()
    ))
  }

  errors <- character()
  warnings <- character()
  info <- character()

  # Normalize column names
  original_names <- names(data)
  names(data) <- normalize_column_names(original_names)

  # Check for required columns (at minimum, we need name)
  required_cols <- c("name")
  missing_required <- setdiff(required_cols, names(data))

  if (length(missing_required) > 0) {
    errors <- c(errors, paste("Missing required column(s):", paste(missing_required, collapse = ", ")))
  }

  # Check for recommended columns

  recommended_cols <- c("email", "academic_rank", "scopus_id", "scholar_id")
  missing_recommended <- setdiff(recommended_cols, names(data))

  if (length(missing_recommended) > 0) {
    warnings <- c(warnings, paste("Missing recommended column(s):", paste(missing_recommended, collapse = ", ")))
  }

  # Calculate completeness for each column
  # Handle different column types safely (datetime columns can't be compared to strings)
  count_present <- function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt") || inherits(x, "Date")) {
      # For datetime columns, just check for NA
      return(sum(!is.na(x)))
    }
    # For other columns, check for NA and empty strings
    x_char <- as.character(x)
    sum(!is.na(x_char) & x_char != "" & x_char != "NA")
  }

  completeness <- data.frame(
    column = names(data),
    n_total = nrow(data),
    n_present = sapply(data, count_present),
    stringsAsFactors = FALSE
  )
  completeness$n_missing <- completeness$n_total - completeness$n_present
  completeness$pct_complete <- round(100 * completeness$n_present / completeness$n_total, 1)

  # Info about what was found
  id_cols <- c("scopus_id", "scholar_id")
  found_ids <- intersect(id_cols, names(data))
  if (length(found_ids) > 0) {
    for (id_col in found_ids) {
      n_present <- sum(!is.na(data[[id_col]]) & data[[id_col]] != "" & data[[id_col]] != "NA")
      info <- c(info, sprintf("%s: %d of %d present (%.1f%%)",
                              id_col, n_present, nrow(data), 100 * n_present / nrow(data)))
    }
  }

  # Validate specific columns
  if ("name" %in% names(data)) {
    empty_names <- sum(is.na(data$name) | data$name == "" | data$name == "NA")
    if (empty_names > 0) {
      warnings <- c(warnings, sprintf("%d row(s) have missing names", empty_names))
    }
  }

  # Validate Scopus ID format (should be numeric, typically 10-11 digits)
  if ("scopus_id" %in% names(data)) {
    scopus_ids <- data$scopus_id[!is.na(data$scopus_id) & data$scopus_id != ""]
    if (length(scopus_ids) > 0) {
      invalid_scopus <- !grepl("^[0-9]+$", as.character(scopus_ids))
      if (any(invalid_scopus)) {
        warnings <- c(warnings, sprintf("%d Scopus ID(s) have invalid format (expected numeric)",
                                        sum(invalid_scopus)))
      }
    }
  }

  # Validate Scholar ID format (typically alphanumeric, 12 chars)
  if ("scholar_id" %in% names(data)) {
    scholar_ids <- data$scholar_id[!is.na(data$scholar_id) & data$scholar_id != ""]
    if (length(scholar_ids) > 0) {
      # Scholar IDs are alphanumeric, often with underscores
      invalid_scholar <- !grepl("^[A-Za-z0-9_-]+$", as.character(scholar_ids))
      if (any(invalid_scholar)) {
        warnings <- c(warnings, sprintf("%d Scholar ID(s) have potentially invalid format",
                                        sum(invalid_scholar)))
      }
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    info = info,
    n_rows = nrow(data),
    n_cols = ncol(data),
    columns_found = names(data),
    columns_missing = setdiff(unlist(ROSTER_COLUMNS), names(data)),
    completeness = completeness,
    original_names = original_names,
    normalized_names = names(data)
  )
}

#' Clean and standardize roster data
#'
#' @param data Data frame of roster data
#' @return Cleaned data frame with standardized columns
clean_roster <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Normalize column names
  names(data) <- normalize_column_names(names(data))

  # Ensure all expected columns exist (add as NA if missing)
  for (col in unlist(ROSTER_COLUMNS)) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_character_
    }
  }

  # Clean string columns
  string_cols <- c("name", "email", "scopus_id", "scholar_id", "academic_rank", "associations")
  for (col in string_cols) {
    if (col %in% names(data)) {
      data[[col]] <- trimws(as.character(data[[col]]))
      data[[col]] <- ifelse(data[[col]] %in% c("", "NA", "N/A", "n/a", "-"), NA_character_, data[[col]])
    }
  }

  # Parse REAIMS publications as numeric
  if ("reaims_pubs" %in% names(data)) {
    data$reaims_pubs <- suppressWarnings(as.numeric(as.character(data$reaims_pubs)))
  }

  # Parse datetime columns from Excel (may be POSIXct, numeric, or character)
  datetime_cols <- c("start_time", "completion_time")
  for (col in datetime_cols) {
    if (col %in% names(data)) {
      data[[col]] <- parse_excel_datetime(data[[col]])
    }
  }

  # Standardize academic rank
  if ("academic_rank" %in% names(data)) {
    data$academic_rank <- standardize_rank(data$academic_rank)
  }

  # Parse promotion year
  if ("last_promotion" %in% names(data)) {
    data$last_promotion_year <- extract_year(data$last_promotion)
  }

  # Add row ID if not present
  if (!"id" %in% names(data) || all(is.na(data$id))) {
    data$id <- seq_len(nrow(data))
  }

  # Add internal tracking columns
  data$openalex_id <- NA_character_
  data$resolution_status <- "pending"
  data$resolution_method <- NA_character_
  data$last_updated <- NA_character_

  return(data)
}

#' Standardize academic rank to consistent categories
#'
#' @param rank Character vector of rank values
#' @return Standardized rank values
standardize_rank <- function(rank) {
  rank <- tolower(trimws(as.character(rank)))

  dplyr::case_when(
    grepl("instructor|lecturer", rank) ~ "Instructor",
    grepl("assistant", rank) ~ "Assistant Professor",
    grepl("associate", rank) ~ "Associate Professor",
    grepl("full|^professor$", rank) ~ "Full Professor",
    grepl("emerit", rank) ~ "Emeritus",
    grepl("research", rank) ~ "Research Faculty",
    grepl("clinical", rank) ~ "Clinical Faculty",
    grepl("adjunct", rank) ~ "Adjunct",
    is.na(rank) | rank == "" ~ NA_character_,
    TRUE ~ stringr::str_to_title(rank)
  )
}

#' Parse Excel datetime values safely
#'
#' Handles datetime columns that may come from Excel as:
#' - POSIXct/POSIXlt objects (already parsed)
#' - Numeric (Excel serial dates)
#' - Character strings in various formats
#'
#' @param x Vector of datetime values from Excel
#' @return POSIXct vector
parse_excel_datetime <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.POSIXct(NA))
  }

  # If already POSIXct, return as-is
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  # If already POSIXlt, convert to POSIXct
  if (inherits(x, "POSIXlt")) {
    return(as.POSIXct(x))
  }

  # If Date, convert to POSIXct
  if (inherits(x, "Date")) {
    return(as.POSIXct(x))
  }

  # If numeric, treat as Excel serial date
  # Excel serial dates: days since 1899-12-30 (with 1900 leap year bug)
  if (is.numeric(x)) {
    # Excel epoch is 1899-12-30 (accounting for Excel's leap year bug)
    origin <- as.POSIXct("1899-12-30", tz = "UTC")
    return(as.POSIXct(origin + x * 86400, tz = "UTC"))
  }

  # If character, try various formats
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)

    # Replace NA-like strings with actual NA
    x <- ifelse(x %in% c("", "NA", "N/A", "n/a", "-"), NA_character_, x)

    # Try lubridate parsing (handles many formats automatically)
    result <- suppressWarnings(lubridate::parse_date_time(
      x,
      orders = c("mdy HM", "mdy HMS", "ymd HM", "ymd HMS", "dmy HM", "dmy HMS",
                 "mdy", "ymd", "dmy"),
      quiet = TRUE
    ))

    return(as.POSIXct(result))
  }

  # Fallback: try to coerce
  suppressWarnings(as.POSIXct(x))
}

#' Extract year from various date formats
#'
#' @param date_str Character vector of date strings
#' @return Numeric vector of years
extract_year <- function(date_str) {
  # Handle POSIXct/Date objects directly
  if (inherits(date_str, "POSIXct") || inherits(date_str, "POSIXlt") || inherits(date_str, "Date")) {
    years <- as.numeric(format(date_str, "%Y"))
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    years <- ifelse(years >= 1950 & years <= current_year + 1, years, NA_integer_)
    return(years)
  }

  # Handle numeric (Excel serial dates)
  if (is.numeric(date_str)) {
    # Check if values look like years (1950-2100) or Excel serial dates (>30000)
    looks_like_year <- date_str >= 1950 & date_str <= 2100
    if (all(looks_like_year | is.na(date_str))) {
      # Values are already years
      years <- as.integer(date_str)
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      years <- ifelse(years >= 1950 & years <= current_year + 1, years, NA_integer_)
      return(years)
    } else {
      # Treat as Excel serial dates
      parsed <- parse_excel_datetime(date_str)
      years <- as.numeric(format(parsed, "%Y"))
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      years <- ifelse(years >= 1950 & years <= current_year + 1, years, NA_integer_)
      return(years)
    }
  }

  date_str <- as.character(date_str)

  # Try to extract 4-digit year
  years <- stringr::str_extract(date_str, "\\b(19|20)\\d{2}\\b")
  years <- suppressWarnings(as.numeric(years))

  # Validate years are reasonable (1950 to next year)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- ifelse(years >= 1950 & years <= current_year + 1, years, NA_integer_)

  return(years)
}

#' Calculate data completeness summary for display
#'
#' @param data Cleaned roster data frame
#' @return Data frame with completeness statistics
calculate_completeness_summary <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }

  # Key columns for analysis
  key_cols <- c("name", "email", "academic_rank", "scopus_id", "scholar_id",
                "reaims_pubs", "last_promotion")
  key_cols <- intersect(key_cols, names(data))

  summary_df <- data.frame(
    Field = key_cols,
    stringsAsFactors = FALSE
  )

  summary_df$Present <- sapply(key_cols, function(col) {
    sum(!is.na(data[[col]]) & data[[col]] != "")
  })

  summary_df$Missing <- nrow(data) - summary_df$Present
  summary_df$`Percent Complete` <- round(100 * summary_df$Present / nrow(data), 1)

  # Add display-friendly field names
  field_labels <- c(
    name = "Name",
    email = "Email",
    academic_rank = "Academic Rank",
    scopus_id = "Scopus ID",
    scholar_id = "Google Scholar ID",
    reaims_pubs = "REAIMS Publications",
    last_promotion = "Last Promotion Date"
  )
  summary_df$Field <- sapply(summary_df$Field, function(f) {
    if (f %in% names(field_labels)) field_labels[[f]] else f
  })

  return(summary_df)
}
