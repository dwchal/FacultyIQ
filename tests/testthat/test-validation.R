# =============================================================================
# FacultyIQ - Validation Tests
# =============================================================================

library(testthat)

# Load required packages and source files
suppressPackageStartupMessages({
  library(dplyr)
  library(janitor)
  library(stringr)
})

source("../../R/utils_validation.R")

# -----------------------------------------------------------------------------
# Column Name Normalization Tests
# -----------------------------------------------------------------------------

test_that("normalize_column_names handles standard columns", {
  cols <- c("Name", "Email", "Scopus ID")
  result <- normalize_column_names(cols)

  expect_equal(result[1], "name")
  expect_equal(result[2], "email")
  expect_equal(result[3], "scopus_id")
})

test_that("normalize_column_names handles long README column names", {
  cols <- c(
    "What is your current academic rank?",
    "What is your Scopus ID?",
    "What is your Google Scholar username? (highlighted in screenshot below, this includes the letters and numbers between V-J in the search bar)"
  )
  result <- normalize_column_names(cols)

  expect_equal(result[1], "academic_rank")
  expect_equal(result[2], "scopus_id")
  expect_equal(result[3], "scholar_id")
})

test_that("normalize_column_names handles case variations", {
  cols <- c("NAME", "email", "SCOPUS_ID", "Scholar_Id")
  result <- normalize_column_names(cols)

  expect_equal(result[1], "name")
  expect_equal(result[2], "email")
})

test_that("normalize_column_names makes unique names for duplicates", {
  cols <- c("Name", "Name", "name")
  result <- normalize_column_names(cols)

  expect_length(unique(result), 3)
})

# -----------------------------------------------------------------------------
# Roster Validation Tests
# -----------------------------------------------------------------------------

test_that("validate_roster returns valid for minimal data", {
  data <- data.frame(
    Name = c("Alice", "Bob"),
    stringsAsFactors = FALSE
  )

  result <- validate_roster(data)

  expect_true(result$valid)
  expect_equal(result$n_rows, 2)
  expect_length(result$errors, 0)
})

test_that("validate_roster returns invalid for empty data", {
  data <- data.frame()

  result <- validate_roster(data)

  expect_false(result$valid)
  expect_true(length(result$errors) > 0)
})

test_that("validate_roster returns invalid for missing name column", {
  data <- data.frame(
    Email = c("a@test.com", "b@test.com"),
    stringsAsFactors = FALSE
  )

  result <- validate_roster(data)

  expect_false(result$valid)
  expect_true(any(grepl("name", result$errors, ignore.case = TRUE)))
})

test_that("validate_roster generates warnings for missing recommended columns", {
  data <- data.frame(
    Name = c("Alice", "Bob"),
    stringsAsFactors = FALSE
  )

  result <- validate_roster(data)

  expect_true(length(result$warnings) > 0)
  expect_true(any(grepl("recommended", result$warnings, ignore.case = TRUE)))
})

test_that("validate_roster calculates completeness correctly", {
  data <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Email = c("a@test.com", NA, "c@test.com"),
    stringsAsFactors = FALSE
  )

  result <- validate_roster(data)

  # Check completeness for email column
  email_row <- result$completeness[result$completeness$column == "email", ]
  expect_equal(email_row$n_present, 2)
  expect_equal(email_row$n_missing, 1)
})

# -----------------------------------------------------------------------------
# Roster Cleaning Tests
# -----------------------------------------------------------------------------

test_that("clean_roster standardizes column names", {
  data <- data.frame(
    Name = c("Alice"),
    `Scopus ID` = c("12345"),
    check.names = FALSE
  )

  result <- clean_roster(data)

  expect_true("name" %in% names(result))
  expect_true("scopus_id" %in% names(result))
})

test_that("clean_roster adds missing columns as NA", {
  data <- data.frame(
    Name = c("Alice"),
    stringsAsFactors = FALSE
  )

  result <- clean_roster(data)

  expect_true("email" %in% names(result))
  expect_true("scopus_id" %in% names(result))
  expect_true("scholar_id" %in% names(result))
  expect_true(is.na(result$email[1]))
})

test_that("clean_roster cleans empty strings to NA", {
  data <- data.frame(
    Name = c("Alice", "Bob"),
    Email = c("a@test.com", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_roster(data)

  expect_true(is.na(result$email[2]))
})

test_that("clean_roster adds tracking columns", {
  data <- data.frame(
    Name = c("Alice"),
    stringsAsFactors = FALSE
  )

  result <- clean_roster(data)

  expect_true("openalex_id" %in% names(result))
  expect_true("resolution_status" %in% names(result))
  expect_equal(result$resolution_status[1], "pending")
})

# -----------------------------------------------------------------------------
# Academic Rank Standardization Tests
# -----------------------------------------------------------------------------

test_that("standardize_rank handles common variations", {
  ranks <- c("assistant professor", "ASSOCIATE PROFESSOR", "Full Professor",
             "Instructor", "emeritus")

  result <- standardize_rank(ranks)

  expect_equal(result[1], "Assistant Professor")
  expect_equal(result[2], "Associate Professor")
  expect_equal(result[3], "Full Professor")
  expect_equal(result[4], "Instructor")
  expect_equal(result[5], "Emeritus")
})

test_that("standardize_rank handles NA and empty strings", {
  ranks <- c("Professor", NA, "")

  result <- standardize_rank(ranks)

  expect_equal(result[1], "Full Professor")
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

# -----------------------------------------------------------------------------
# Year Extraction Tests
# -----------------------------------------------------------------------------

test_that("extract_year handles various formats", {
  dates <- c("2020", "2021-05-15", "May 2019", "2018-01")

  result <- extract_year(dates)

  expect_equal(result[1], 2020)
  expect_equal(result[2], 2021)
  expect_equal(result[3], 2019)
  expect_equal(result[4], 2018)
})

test_that("extract_year validates year range", {
  dates <- c("1940", "2025", "2030", "1990")

  result <- extract_year(dates)

  expect_true(is.na(result[1]))  # Too old
  expect_false(is.na(result[2]))
  expect_true(is.na(result[3]))  # Too far in future
  expect_false(is.na(result[4]))
})

# -----------------------------------------------------------------------------
# Completeness Summary Tests
# -----------------------------------------------------------------------------

test_that("calculate_completeness_summary works correctly", {
  data <- data.frame(
    name = c("A", "B", "C"),
    email = c("a@t.com", NA, "c@t.com"),
    scopus_id = c("1", "2", NA),
    academic_rank = c("Prof", NA, NA),
    stringsAsFactors = FALSE
  )

  result <- calculate_completeness_summary(data)

  expect_true(nrow(result) > 0)
  expect_true("Percent Complete" %in% names(result))

  name_row <- result[result$Field == "Name", ]
  expect_equal(name_row$Present, 3)

  email_row <- result[result$Field == "Email", ]
  expect_equal(email_row$Present, 2)
})

test_that("calculate_completeness_summary counts Date columns correctly", {
  data <- data.frame(
    name = c("A", "B", "C"),
    initial_hire_date = as.Date(c("2015-07-01", NA, "2020-01-15")),
    assistant_start_date = as.Date(c("2015-07-01", NA, NA)),
    stringsAsFactors = FALSE
  )

  result <- calculate_completeness_summary(data)

  hire_row <- result[result$Field == "Initial Hire Date", ]
  expect_equal(hire_row$Present, 2)
  expect_equal(hire_row$Missing, 1)

  asst_row <- result[result$Field == "Assistant Professor Start Date", ]
  expect_equal(asst_row$Present, 1)
})

# -----------------------------------------------------------------------------
# Appointment Date Parsing Tests
# -----------------------------------------------------------------------------

test_that("parse_appointment_date handles year-only strings", {
  result <- parse_appointment_date(c("2018", "2020", NA))

  expect_equal(result[1], as.Date("2018-07-01"))
  expect_equal(result[2], as.Date("2020-07-01"))
  expect_true(is.na(result[3]))
})

test_that("parse_appointment_date handles full date strings", {
  result <- parse_appointment_date(c("2018-09-01", "2020-01-15", "07/04/2019"))

  expect_equal(result[1], as.Date("2018-09-01"))
  expect_equal(result[2], as.Date("2020-01-15"))
  expect_equal(result[3], as.Date("2019-07-04"))
})

test_that("parse_appointment_date handles Date and POSIXct inputs", {
  d <- as.Date(c("2018-05-01", "2020-06-15"))
  expect_equal(parse_appointment_date(d), d)

  p <- as.POSIXct(c("2018-05-01", "2020-06-15"), tz = "UTC")
  result <- parse_appointment_date(p)
  expect_equal(result[1], as.Date("2018-05-01"))
})

test_that("parse_appointment_date handles numeric years", {
  result <- parse_appointment_date(c(2018, 2020))
  expect_equal(result[1], as.Date("2018-07-01"))
  expect_equal(result[2], as.Date("2020-07-01"))
})

test_that("parse_appointment_date rejects out-of-range values", {
  result <- parse_appointment_date(c("1800", "2200", "garbage", ""))
  expect_true(all(is.na(result)))
})

# -----------------------------------------------------------------------------
# Years in Current Rank Tests
# -----------------------------------------------------------------------------

test_that("compute_years_in_current_rank computes years correctly", {
  start <- as.Date(c("2020-01-01", "2018-07-01", NA))
  result <- compute_years_in_current_rank(start, as_of = as.Date("2023-01-01"))

  expect_equal(result[1], 3.0)
  expect_equal(result[2], 4.5)
  expect_true(is.na(result[3]))
})

test_that("compute_years_in_current_rank returns NA for future start dates", {
  start <- as.Date(c("2030-01-01"))
  result <- compute_years_in_current_rank(start, as_of = as.Date("2023-01-01"))
  expect_true(is.na(result[1]))
})

test_that("compute_current_rank_start picks correct appointment by rank", {
  data <- data.frame(
    academic_rank = c("Full Professor", "Associate Professor",
                      "Assistant Professor", "Instructor"),
    initial_hire_date = as.Date(c("2000-07-01", "2010-07-01",
                                  "2020-07-01", "2023-07-01")),
    assistant_start_date = as.Date(c("2000-07-01", "2010-07-01",
                                     "2020-07-01", NA)),
    associate_start_date = as.Date(c("2006-07-01", "2016-07-01", NA, NA)),
    full_start_date = as.Date(c("2012-07-01", NA, NA, NA)),
    stringsAsFactors = FALSE
  )

  result <- compute_current_rank_start(data)

  expect_equal(result[1], as.Date("2012-07-01"))  # Full Prof -> full_start
  expect_equal(result[2], as.Date("2016-07-01"))  # Assoc -> associate_start
  expect_equal(result[3], as.Date("2020-07-01"))  # Asst -> assistant_start
  expect_equal(result[4], as.Date("2023-07-01"))  # Instructor -> hire
})

test_that("compute_current_rank_start falls back through missing dates", {
  data <- data.frame(
    academic_rank = c("Full Professor", "Associate Professor"),
    initial_hire_date = as.Date(c("2000-07-01", "2010-07-01")),
    assistant_start_date = as.Date(c("2000-07-01", NA)),
    associate_start_date = as.Date(c("2006-07-01", NA)),
    full_start_date = as.Date(c(NA, NA)),
    stringsAsFactors = FALSE
  )

  result <- compute_current_rank_start(data)

  # Full Prof with no full_start -> falls back to associate_start
  expect_equal(result[1], as.Date("2006-07-01"))
  # Associate with no associate_start -> falls back to hire
  expect_equal(result[2], as.Date("2010-07-01"))
})

# -----------------------------------------------------------------------------
# Chronological Validation Tests
# -----------------------------------------------------------------------------

test_that("check_appointment_chronology flags out-of-order dates", {
  data <- data.frame(
    name = c("Bad Order", "Good Order"),
    initial_hire_date = as.Date(c("2015-07-01", "2010-07-01")),
    assistant_start_date = as.Date(c("2015-07-01", "2010-07-01")),
    associate_start_date = as.Date(c("2010-07-01", "2016-07-01")),  # Row 1 out of order
    full_start_date = as.Date(c(NA, NA)),
    stringsAsFactors = FALSE
  )

  warnings <- check_appointment_chronology(data)

  expect_true(length(warnings) > 0)
  expect_true(any(grepl("Bad Order", warnings)))
  expect_false(any(grepl("Good Order", warnings)))
})

test_that("check_appointment_chronology flags future dates", {
  future_year <- as.numeric(format(Sys.Date(), "%Y")) + 5
  data <- data.frame(
    name = c("Time Traveler"),
    initial_hire_date = as.Date(paste0(future_year, "-01-01")),
    assistant_start_date = as.Date(NA),
    associate_start_date = as.Date(NA),
    full_start_date = as.Date(NA),
    stringsAsFactors = FALSE
  )

  warnings <- check_appointment_chronology(data)

  expect_true(length(warnings) > 0)
  expect_true(any(grepl("future", warnings)))
})

test_that("check_appointment_chronology returns empty when all ok", {
  data <- data.frame(
    name = c("Normal Career"),
    initial_hire_date = as.Date("2000-07-01"),
    assistant_start_date = as.Date("2000-07-01"),
    associate_start_date = as.Date("2006-07-01"),
    full_start_date = as.Date("2012-07-01"),
    stringsAsFactors = FALSE
  )

  warnings <- check_appointment_chronology(data)
  expect_length(warnings, 0)
})

# -----------------------------------------------------------------------------
# Column Mapping Tests for Appointment Dates
# -----------------------------------------------------------------------------

test_that("normalize_column_names maps appointment date variations", {
  cols <- c("Initial hire date", "Assistant Professor start date",
            "Associate Professor start date", "Full Professor start date")
  result <- normalize_column_names(cols)

  expect_equal(result[1], "initial_hire_date")
  expect_equal(result[2], "assistant_start_date")
  expect_equal(result[3], "associate_start_date")
  expect_equal(result[4], "full_start_date")
})

# -----------------------------------------------------------------------------
# clean_roster Integration Tests for Appointment Dates
# -----------------------------------------------------------------------------

test_that("clean_roster parses appointment dates and computes years in rank", {
  data <- data.frame(
    Name = c("Test Prof"),
    `What is your current academic rank?` = c("Associate Professor"),
    `Initial hire date` = c("2014-07-01"),
    `Assistant Professor start date` = c("2014-07-01"),
    `Associate Professor start date` = c("2020-07-01"),
    `Full Professor start date` = c(""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_roster(data)

  expect_true(inherits(result$initial_hire_date, "Date"))
  expect_equal(result$initial_hire_date[1], as.Date("2014-07-01"))
  expect_equal(result$associate_start_date[1], as.Date("2020-07-01"))
  expect_true(is.na(result$full_start_date[1]))

  # Derived fields
  expect_true("current_rank_start_date" %in% names(result))
  expect_equal(result$current_rank_start_date[1], as.Date("2020-07-01"))

  expect_true("years_in_current_rank" %in% names(result))
  expect_true(result$years_in_current_rank[1] > 0)
  expect_true(!is.na(result$initial_hire_date_year))
  expect_equal(result$initial_hire_date_year[1], 2014L)
})

test_that("clean_roster handles year-only appointment values", {
  data <- data.frame(
    Name = c("Year Only"),
    academic_rank = c("Full Professor"),
    initial_hire_date = c("2000"),
    assistant_start_date = c("2000"),
    associate_start_date = c("2006"),
    full_start_date = c("2012"),
    stringsAsFactors = FALSE
  )

  result <- clean_roster(data)

  expect_equal(result$initial_hire_date[1], as.Date("2000-07-01"))
  expect_equal(result$full_start_date[1], as.Date("2012-07-01"))
  expect_equal(result$current_rank_start_date[1], as.Date("2012-07-01"))
})

test_that("validate_roster warns on out-of-order appointment dates", {
  data <- data.frame(
    Name = c("Backward Promotion"),
    `Academic Rank` = c("Associate Professor"),
    `Initial hire date` = c("2010-07-01"),
    `Assistant Professor start date` = c("2015-07-01"),
    `Associate Professor start date` = c("2012-07-01"),  # Before asst start
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- validate_roster(data)

  expect_true(length(result$warnings) > 0)
  expect_true(any(grepl("Associate.*before.*Assistant", result$warnings)))
})
