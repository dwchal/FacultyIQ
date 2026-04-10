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
  initial_hire_date = "initial_hire_date",
  assistant_start_date = "assistant_start_date",
  associate_start_date = "associate_start_date",
  full_start_date = "full_start_date",
  reaims_pubs = "reaims_pubs",
  scopus_id = "scopus_id",
  scholar_id = "scholar_id",
  orcid_id = "orcid_id",
  semantic_scholar_id = "semantic_scholar_id",
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

  # Initial hire date variations
  "initial_hire_date" = "initial_hire_date",
  "initial hire date" = "initial_hire_date",
  "hire date" = "initial_hire_date",
  "date of hire" = "initial_hire_date",
  "start date" = "initial_hire_date",
  "institution start date" = "initial_hire_date",
  "When did you join the institution?" = "initial_hire_date",
  "faculty start date" = "initial_hire_date",

  # Assistant Professor appointment date variations
  "assistant_start_date" = "assistant_start_date",
  "assistant start date" = "assistant_start_date",
  "assistant professor start date" = "assistant_start_date",
  "assistant professor appointment date" = "assistant_start_date",
  "date of assistant professor appointment" = "assistant_start_date",
  "When did you become an Assistant Professor?" = "assistant_start_date",

  # Associate Professor appointment date variations
  "associate_start_date" = "associate_start_date",
  "associate start date" = "associate_start_date",
  "associate professor start date" = "associate_start_date",
  "associate professor appointment date" = "associate_start_date",
  "date of associate professor appointment" = "associate_start_date",
  "When did you become an Associate Professor?" = "associate_start_date",

  # Full Professor appointment date variations
  "full_start_date" = "full_start_date",
  "full start date" = "full_start_date",
  "full professor start date" = "full_start_date",
  "full professor appointment date" = "full_start_date",
  "date of full professor appointment" = "full_start_date",
  "professor appointment date" = "full_start_date",
  "When did you become a Full Professor?" = "full_start_date",

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

  # ORCID ID variations
  "orcid_id" = "orcid_id",
  "orcid id" = "orcid_id",
  "ORCID" = "orcid_id",
  "ORCID ID" = "orcid_id",
  "orcid" = "orcid_id",
  "What is your ORCID iD?" = "orcid_id",
  "orcid identifier" = "orcid_id",

  # Semantic Scholar ID variations
  "semantic_scholar_id" = "semantic_scholar_id",
  "semantic scholar id" = "semantic_scholar_id",
  "Semantic Scholar ID" = "semantic_scholar_id",
  "S2 ID" = "semantic_scholar_id",
  "s2 id" = "semantic_scholar_id",
  "semanticscholar id" = "semantic_scholar_id",

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
  id_cols <- c("scopus_id", "scholar_id", "orcid_id", "semantic_scholar_id")
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

  # Validate ORCID ID format (xxxx-xxxx-xxxx-xxxx, last digit may be X)
  if ("orcid_id" %in% names(data)) {
    orcid_ids <- data$orcid_id[!is.na(data$orcid_id) & data$orcid_id != ""]
    if (length(orcid_ids) > 0) {
      # Strip ORCID URL prefix if present
      orcid_ids <- gsub("^https?://orcid\\.org/", "", as.character(orcid_ids))
      invalid_orcid <- !grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]$", orcid_ids)
      if (any(invalid_orcid)) {
        warnings <- c(warnings, sprintf("%d ORCID ID(s) have invalid format (expected xxxx-xxxx-xxxx-xxxx)",
                                        sum(invalid_orcid)))
      }
    }
  }

  # Validate chronological ordering of appointment dates (if any present)
  appointment_cols <- c("initial_hire_date", "assistant_start_date",
                        "associate_start_date", "full_start_date")
  if (any(appointment_cols %in% names(data))) {
    # Parse dates into a temporary copy for the check (without mutating `data`)
    check_df <- data
    for (col in intersect(appointment_cols, names(data))) {
      check_df[[col]] <- parse_appointment_date(data[[col]])
    }
    chronology_warnings <- check_appointment_chronology(check_df)
    if (length(chronology_warnings) > 0) {
      warnings <- c(warnings, chronology_warnings)
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
  string_cols <- c("name", "email", "scopus_id", "scholar_id", "orcid_id",
                   "semantic_scholar_id", "academic_rank", "associations")
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

  # Parse appointment date columns to Date and extract years
  appointment_cols <- c("initial_hire_date", "assistant_start_date",
                        "associate_start_date", "full_start_date")
  for (col in appointment_cols) {
    if (col %in% names(data)) {
      data[[col]] <- parse_appointment_date(data[[col]])
      year_col <- paste0(col, "_year")
      data[[year_col]] <- suppressWarnings(as.integer(format(data[[col]], "%Y")))
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

  # Compute current rank start date and years in current rank
  data$current_rank_start_date <- compute_current_rank_start(data)
  data$years_in_current_rank <- compute_years_in_current_rank(data$current_rank_start_date)

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

#' Parse academic appointment date values
#'
#' Accepts Date/POSIXct objects, Excel serial numbers, 4-digit years,
#' or date strings in common formats. Year-only values are normalized to
#' July 1 of that year (mid-year default, safer for "years in rank" math).
#' Out-of-range results (before 1950 or after next year) become NA.
#'
#' @param x Vector of appointment date values
#' @return Date vector
parse_appointment_date <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.Date(character(0)))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Already Date
  if (inherits(x, "Date")) {
    return(x)
  }

  # POSIXct/POSIXlt -> Date
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }

  # Numeric: either a year or an Excel serial date
  if (is.numeric(x)) {
    looks_like_year <- !is.na(x) & x >= 1950 & x <= current_year + 1
    result <- as.Date(rep(NA, length(x)))
    if (any(looks_like_year)) {
      result[looks_like_year] <- as.Date(paste0(as.integer(x[looks_like_year]), "-07-01"))
    }
    if (any(!looks_like_year & !is.na(x))) {
      serial_idx <- which(!looks_like_year & !is.na(x))
      parsed <- parse_excel_datetime(x[serial_idx])
      result[serial_idx] <- as.Date(parsed)
    }
    # Clamp to plausible range
    out_of_range <- !is.na(result) &
      (result < as.Date("1950-01-01") | result > as.Date(paste0(current_year + 1, "-12-31")))
    result[out_of_range] <- NA
    return(result)
  }

  # Character / factor
  x <- as.character(x)
  x <- ifelse(x %in% c("", "NA", "N/A", "n/a", "-"), NA_character_, x)

  # Year-only strings → July 1 of that year
  year_only <- !is.na(x) & grepl("^\\s*(19|20)\\d{2}\\s*$", x)
  result <- as.Date(rep(NA, length(x)))
  if (any(year_only)) {
    result[year_only] <- as.Date(paste0(trimws(x[year_only]), "-07-01"))
  }

  # Full date parsing for everything else
  to_parse_idx <- which(!is.na(x) & !year_only)
  if (length(to_parse_idx) > 0) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x[to_parse_idx],
      orders = c("ymd", "mdy", "dmy", "Ym", "mY", "bY", "Yb", "bdY", "dbY"),
      quiet = TRUE
    ))
    result[to_parse_idx] <- as.Date(parsed)
  }

  # Clamp to plausible range
  out_of_range <- !is.na(result) &
    (result < as.Date("1950-01-01") | result > as.Date(paste0(current_year + 1, "-12-31")))
  result[out_of_range] <- NA

  return(result)
}

#' Select the start date for each row's current rank
#'
#' Picks the appointment date that corresponds to the person's current rank.
#' Falls back through more-junior appointment dates if the specific one is
#' missing, and finally to initial_hire_date.
#'
#' @param data Roster data frame (post-normalization, with appointment columns)
#' @return Date vector, one entry per row
compute_current_rank_start <- function(data) {
  n <- nrow(data)
  result <- as.Date(rep(NA, n))

  rank <- if ("academic_rank" %in% names(data)) as.character(data$academic_rank) else rep(NA_character_, n)

  get_col <- function(col) {
    if (col %in% names(data)) data[[col]] else as.Date(rep(NA, n))
  }

  full_d <- get_col("full_start_date")
  assoc_d <- get_col("associate_start_date")
  asst_d <- get_col("assistant_start_date")
  hire_d <- get_col("initial_hire_date")

  for (i in seq_len(n)) {
    r <- rank[i]
    if (is.na(r)) {
      # Unknown rank: use most recent appointment we have
      candidates <- c(full_d[i], assoc_d[i], asst_d[i], hire_d[i])
      candidates <- candidates[!is.na(candidates)]
      if (length(candidates) > 0) result[i] <- max(candidates)
      next
    }

    if (r == "Full Professor") {
      result[i] <- first_non_na(full_d[i], assoc_d[i], asst_d[i], hire_d[i])
    } else if (r == "Associate Professor") {
      result[i] <- first_non_na(assoc_d[i], asst_d[i], hire_d[i])
    } else if (r == "Assistant Professor") {
      result[i] <- first_non_na(asst_d[i], hire_d[i])
    } else {
      # Instructor, Research Faculty, Clinical Faculty, etc.
      result[i] <- hire_d[i]
    }
  }

  result
}

#' Return the first non-NA Date value
#' @keywords internal
first_non_na <- function(...) {
  vals <- c(...)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(as.Date(NA)) else return(vals[1])
}

#' Compute years in current rank given a start date
#'
#' @param start_date Date vector of rank start dates
#' @param as_of Reference date (defaults to today)
#' @return Numeric vector of years (with one decimal), NA if start unknown or in the future
compute_years_in_current_rank <- function(start_date, as_of = Sys.Date()) {
  if (length(start_date) == 0) return(numeric(0))
  days <- as.numeric(as_of - start_date)
  years <- round(days / 365.25, 1)
  years[is.na(years) | years < 0] <- NA_real_
  years
}

#' Validate chronological ordering of appointment dates
#'
#' Returns character vector of warning messages for any rows where dates
#' are out of order (e.g. associate_start_date before assistant_start_date).
#' Pairs where either column is missing from the data frame are skipped.
#'
#' @param data Cleaned roster data frame with Date-type appointment columns
#' @return Character vector of warnings (empty if no problems)
check_appointment_chronology <- function(data) {
  warnings <- character()
  appointment_cols <- c("initial_hire_date", "assistant_start_date",
                        "associate_start_date", "full_start_date")
  present_cols <- intersect(appointment_cols, names(data))
  if (length(present_cols) == 0) return(warnings)

  today <- Sys.Date()
  name_col <- if ("name" %in% names(data)) as.character(data$name) else as.character(seq_len(nrow(data)))

  check_pair <- function(earlier_col, later_col, label) {
    if (!(earlier_col %in% names(data)) || !(later_col %in% names(data))) return()
    earlier <- data[[earlier_col]]
    later <- data[[later_col]]
    bad <- !is.na(earlier) & !is.na(later) & later < earlier
    if (any(bad)) {
      bad_names <- name_col[bad]
      warnings <<- c(warnings, sprintf(
        "%d row(s) have %s: %s",
        sum(bad), label, paste(bad_names, collapse = ", ")
      ))
    }
  }

  check_pair("initial_hire_date", "assistant_start_date",
             "Assistant Professor date before initial hire date")
  check_pair("assistant_start_date", "associate_start_date",
             "Associate Professor date before Assistant Professor date")
  check_pair("associate_start_date", "full_start_date",
             "Full Professor date before Associate Professor date")
  check_pair("initial_hire_date", "full_start_date",
             "Full Professor date before initial hire date")

  # Future dates
  for (col in present_cols) {
    future <- !is.na(data[[col]]) & data[[col]] > today
    if (any(future)) {
      warnings <- c(warnings, sprintf(
        "%d row(s) have %s in the future: %s",
        sum(future), col, paste(name_col[future], collapse = ", ")
      ))
    }
  }

  warnings
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
                "orcid_id", "semantic_scholar_id", "reaims_pubs", "last_promotion",
                "initial_hire_date", "assistant_start_date",
                "associate_start_date", "full_start_date")
  key_cols <- intersect(key_cols, names(data))

  summary_df <- data.frame(
    Field = key_cols,
    stringsAsFactors = FALSE
  )

  summary_df$Present <- sapply(key_cols, function(col) {
    col_data <- data[[col]]
    if (inherits(col_data, c("Date", "POSIXct", "POSIXlt"))) {
      sum(!is.na(col_data))
    } else {
      sum(!is.na(col_data) & as.character(col_data) != "")
    }
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
    orcid_id = "ORCID iD",
    semantic_scholar_id = "Semantic Scholar ID",
    reaims_pubs = "REAIMS Publications",
    last_promotion = "Last Promotion Date",
    initial_hire_date = "Initial Hire Date",
    assistant_start_date = "Assistant Professor Start Date",
    associate_start_date = "Associate Professor Start Date",
    full_start_date = "Full Professor Start Date"
  )
  summary_df$Field <- sapply(summary_df$Field, function(f) {
    if (f %in% names(field_labels)) field_labels[[f]] else f
  })

  return(summary_df)
}
