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
