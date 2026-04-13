# =============================================================================
# FacultyIQ - API Utility Tests
# =============================================================================
# Tests for shared helpers and guard conditions in utils_api.R.
# Actual network calls are not made here - tests cover pure utilities and
# input validation (the functions return NULL on empty/invalid input).

library(testthat)

# Minimal stubs so utils_api.R sources without requiring optional packages
# or network access. Guard-condition tests return NULL before any HTTP call.
if (!exists("app_config")) {
  app_config <- list(
    scholar           = list(rate_limit_per_second = 0.5),
    nih_reporter      = list(rate_limit_per_second = 7),
    nsf_awards        = list(rate_limit_per_second = 5),
    semantic_scholar  = list(rate_limit_per_second = 1),
    crossref          = list(rate_limit_per_second = 50)
  )
}
if (!exists("null_coalesce")) {
  null_coalesce <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

source("../../R/utils_api.R")

# =============================================================================
# is_empty_id()
# =============================================================================

test_that("is_empty_id returns TRUE for NULL", {
  expect_true(is_empty_id(NULL))
})

test_that("is_empty_id returns TRUE for NA", {
  expect_true(is_empty_id(NA))
  expect_true(is_empty_id(NA_character_))
  expect_true(is_empty_id(NA_integer_))
})

test_that("is_empty_id returns TRUE for empty string", {
  expect_true(is_empty_id(""))
})

test_that("is_empty_id returns TRUE for zero-length vector", {
  expect_true(is_empty_id(character(0)))
  expect_true(is_empty_id(integer(0)))
})

test_that("is_empty_id returns FALSE for valid IDs", {
  expect_false(is_empty_id("57209168452"))
  expect_false(is_empty_id("A5023888391"))
  expect_false(is_empty_id(123456))
  expect_false(is_empty_id("0000-0002-1825-0097"))
})

# =============================================================================
# extract_summary_stats()
# =============================================================================

test_that("extract_summary_stats returns NAs for NULL input", {
  result <- extract_summary_stats(NULL)
  expect_equal(result$h_index,   NA_integer_)
  expect_equal(result$i10_index, NA_integer_)
})

test_that("extract_summary_stats handles data.frame stats", {
  stats_df <- data.frame(h_index = 15L, i10_index = 22L)
  result <- extract_summary_stats(stats_df)
  expect_equal(result$h_index,   15L)
  expect_equal(result$i10_index, 22L)
})

test_that("extract_summary_stats handles list stats (nested)", {
  stats_list <- list(list(h_index = 10L, i10_index = 8L))
  result <- extract_summary_stats(stats_list)
  expect_equal(result$h_index,   10L)
  expect_equal(result$i10_index, 8L)
})

test_that("extract_summary_stats handles flat list stats", {
  stats_flat <- list(h_index = 7L, i10_index = 3L)
  result <- extract_summary_stats(stats_flat)
  expect_equal(result$h_index,   7L)
  expect_equal(result$i10_index, 3L)
})

test_that("extract_summary_stats returns NAs for empty list", {
  result <- extract_summary_stats(list())
  expect_equal(result$h_index,   NA_integer_)
  expect_equal(result$i10_index, NA_integer_)
})

# =============================================================================
# extract_institution()
# =============================================================================

test_that("extract_institution returns NA for NULL input", {
  expect_equal(extract_institution(NULL), NA_character_)
})

test_that("extract_institution returns NA for empty list", {
  expect_equal(extract_institution(list()), NA_character_)
})

test_that("extract_institution handles data.frame institution", {
  inst_df <- data.frame(display_name = "Stanford University", stringsAsFactors = FALSE)
  expect_equal(extract_institution(inst_df), "Stanford University")
})

test_that("extract_institution handles list institution", {
  inst_list <- list(display_name = "MIT")
  expect_equal(extract_institution(inst_list), "MIT")
})

test_that("extract_institution returns NA when display_name is absent", {
  inst_no_name <- list(id = "I123", ror = "abc")
  expect_equal(extract_institution(inst_no_name), NA_character_)
})

# =============================================================================
# api_retry()
# =============================================================================

test_that("api_retry returns value on first success", {
  result <- api_retry(function() 42L, max_attempts = 3, base_delay = 0)
  expect_equal(result, 42L)
})

test_that("api_retry returns value after transient failures", {
  counter <- 0L
  result <- api_retry(function() {
    counter <<- counter + 1L
    if (counter < 3L) stop("transient error")
    "success"
  }, max_attempts = 3, base_delay = 0)
  expect_equal(result, "success")
  expect_equal(counter, 3L)
})

test_that("api_retry returns NULL after all attempts fail", {
  result <- api_retry(function() stop("always fails"),
                      max_attempts = 3, base_delay = 0)
  expect_null(result)
})

test_that("api_retry with max_attempts = 1 does not retry", {
  calls <- 0L
  result <- api_retry(function() {
    calls <<- calls + 1L
    stop("fail")
  }, max_attempts = 1, base_delay = 0)
  expect_null(result)
  expect_equal(calls, 1L)
})

# =============================================================================
# Guard conditions: functions return NULL on empty input
# (no network calls — tests rely only on input validation)
# =============================================================================

test_that("get_openalex_by_scopus returns NULL for empty scopus_id", {
  expect_null(get_openalex_by_scopus(NULL))
  expect_null(get_openalex_by_scopus(NA))
  expect_null(get_openalex_by_scopus(""))
})

test_that("get_openalex_author returns NULL for empty openalex_id", {
  expect_null(get_openalex_author(NULL))
  expect_null(get_openalex_author(NA))
  expect_null(get_openalex_author(""))
})

test_that("get_openalex_works returns NULL for empty openalex_id", {
  expect_null(get_openalex_works(NULL))
  expect_null(get_openalex_works(NA))
  expect_null(get_openalex_works(""))
})

test_that("get_orcid_profile returns NULL for empty orcid_id", {
  # Only tests guard — no network call
  expect_null(get_orcid_profile(NULL))
  expect_null(get_orcid_profile(NA))
  expect_null(get_orcid_profile(""))
})

test_that("get_semantic_scholar_author returns NULL for empty id", {
  expect_null(get_semantic_scholar_author(NULL))
  expect_null(get_semantic_scholar_author(NA))
  expect_null(get_semantic_scholar_author(""))
})

test_that("get_semantic_scholar_papers returns NULL for empty id", {
  expect_null(get_semantic_scholar_papers(NULL))
  expect_null(get_semantic_scholar_papers(NA))
  expect_null(get_semantic_scholar_papers(""))
})
