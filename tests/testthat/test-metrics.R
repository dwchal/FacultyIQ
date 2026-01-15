# =============================================================================
# FacultyIQ - Metrics Tests
# =============================================================================

library(testthat)

# Load required packages and source files
suppressPackageStartupMessages({
  library(dplyr)
})

source("../../R/utils_metrics.R")

# -----------------------------------------------------------------------------
# Helper: Create mock person data
# -----------------------------------------------------------------------------

create_mock_person_data <- function(
  name = "Test Person",
  roster_id = 1,
  works_count = 50,
  cited_by_count = 1000,
  h_index = 15,
  i10_index = 20,
  include_yearly = TRUE
) {
  yearly_data <- if (include_yearly) {
    data.frame(
      year = 2018:2023,
      works_count = c(5, 8, 10, 12, 8, 7),
      cited_by_count = c(50, 100, 150, 200, 250, 250)
    )
  } else {
    NULL
  }

  list(
    name = name,
    roster_id = roster_id,
    openalex = list(
      works_count = works_count,
      cited_by_count = cited_by_count,
      h_index = h_index,
      i10_index = i10_index,
      counts_by_year = yearly_data
    ),
    scholar = NULL,
    scopus = NULL,
    works = data.frame(
      work_id = paste0("W", 1:10),
      title = paste("Paper", 1:10),
      publication_year = 2020:2023,
      cited_by_count = c(100, 80, 60, 50, 40, 30, 20, 15, 10, 5),
      is_oa = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE),
      stringsAsFactors = FALSE
    ),
    data_sources = c("OpenAlex"),
    errors = character()
  )
}

# -----------------------------------------------------------------------------
# Person Metrics Tests
# -----------------------------------------------------------------------------

test_that("compute_person_metrics extracts basic metrics", {
  person_data <- create_mock_person_data()

  result <- compute_person_metrics(person_data)

  expect_equal(result$name, "Test Person")
  expect_equal(result$works_count, 50)
  expect_equal(result$citations_count, 1000)
  expect_equal(result$h_index, 15)
  expect_equal(result$i10_index, 20)
})

test_that("compute_person_metrics calculates derived metrics", {
  person_data <- create_mock_person_data(
    works_count = 100,
    cited_by_count = 2000
  )

  result <- compute_person_metrics(person_data)

  expect_equal(result$citations_per_work, 20)
  expect_equal(result$data_quality, "good")
})

test_that("compute_person_metrics calculates OA percentage", {
  person_data <- create_mock_person_data()

  result <- compute_person_metrics(person_data)

  # 5 out of 10 works are OA in mock data
  expect_equal(result$oa_percentage, 50)
})

test_that("compute_person_metrics handles missing data gracefully", {
  person_data <- list(
    name = "No Data Person",
    roster_id = 2,
    openalex = NULL,
    scholar = NULL,
    scopus = NULL,
    works = NULL,
    data_sources = character(),
    errors = character()
  )

  result <- compute_person_metrics(person_data)

  expect_equal(result$name, "No Data Person")
  expect_true(is.na(result$works_count))
  expect_equal(result$data_quality, "unavailable")
  expect_true(length(result$unavailable_reason) > 0)
})

test_that("compute_person_metrics includes roster data", {
  person_data <- create_mock_person_data()
  roster_row <- data.frame(
    reaims_pubs = 45,
    academic_rank = "Associate Professor",
    stringsAsFactors = FALSE
  )

  result <- compute_person_metrics(person_data, roster_row)

  expect_equal(result$reaims_pubs, 45)
  expect_equal(result$academic_rank, "Associate Professor")
})

# -----------------------------------------------------------------------------
# Time Series Tests
# -----------------------------------------------------------------------------

test_that("create_yearly_timeseries creates correct structure", {
  person_data_list <- list(
    create_mock_person_data(name = "Person A", roster_id = 1),
    create_mock_person_data(name = "Person B", roster_id = 2)
  )

  result <- create_yearly_timeseries(
    person_data_list,
    metric = "works",
    from_year = 2020,
    to_year = 2023
  )

  expect_true(is.data.frame(result))
  expect_true("year" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("name" %in% names(result))
  expect_true("roster_id" %in% names(result))
})

test_that("create_yearly_timeseries handles empty data", {
  person_data_list <- list(
    list(
      name = "No Data",
      roster_id = 1,
      openalex = NULL,
      data_sources = character()
    )
  )

  result <- create_yearly_timeseries(person_data_list, metric = "works")

  expect_equal(nrow(result), 0)
})

test_that("aggregate_division_yearly computes correct stats", {
  yearly_data <- data.frame(
    year = rep(2020:2022, each = 3),
    value = c(10, 20, 30, 15, 25, 35, 20, 30, 40),
    roster_id = rep(1:3, 3),
    stringsAsFactors = FALSE
  )

  result <- aggregate_division_yearly(yearly_data)

  expect_true(is.data.frame(result))
  expect_true("total" %in% names(result))
  expect_true("median" %in% names(result))
  expect_true("n_people" %in% names(result))

  # Check 2020: total = 10+20+30 = 60, median = 20
  row_2020 <- result[result$year == 2020, ]
  expect_equal(row_2020$total, 60)
  expect_equal(row_2020$median, 20)
  expect_equal(row_2020$n_people, 3)
})

# -----------------------------------------------------------------------------
# Division Summary Tests
# -----------------------------------------------------------------------------

test_that("compute_division_summary handles valid data", {
  metrics_df <- data.frame(
    roster_id = 1:5,
    name = paste("Person", 1:5),
    works_count = c(10, 20, 30, 40, 50),
    citations_count = c(100, 200, 300, 400, 500),
    h_index = c(5, 10, 15, 20, 25),
    academic_rank = c("Assistant", "Associate", "Full", "Associate", "Full"),
    data_quality = rep("good", 5),
    stringsAsFactors = FALSE
  )

  result <- compute_division_summary(metrics_df)

  expect_equal(result$n_people, 5)
  expect_equal(result$total_works, 150)
  expect_equal(result$total_citations, 1500)
  expect_equal(result$median_works, 30)
  expect_equal(result$median_h_index, 15)
})

test_that("compute_division_summary handles empty data", {
  result <- compute_division_summary(data.frame())

  expect_equal(result$n_people, 0)
  expect_true(is.na(result$total_works))
})

test_that("compute_division_summary computes by-rank stats", {
  metrics_df <- data.frame(
    roster_id = 1:4,
    name = paste("Person", 1:4),
    works_count = c(10, 20, 30, 40),
    citations_count = c(100, 200, 300, 400),
    h_index = c(5, 10, 15, 20),
    academic_rank = c("Assistant", "Assistant", "Full", "Full"),
    data_quality = rep("good", 4),
    stringsAsFactors = FALSE
  )

  result <- compute_division_summary(metrics_df)

  expect_true(!is.null(result$by_rank))
  expect_equal(nrow(result$by_rank), 2)

  asst_row <- result$by_rank[result$by_rank$academic_rank == "Assistant", ]
  expect_equal(asst_row$n, 2)
  expect_equal(asst_row$median_works, 15)
})

# -----------------------------------------------------------------------------
# Top Works/People Tests
# -----------------------------------------------------------------------------

test_that("get_top_works returns sorted results", {
  person_data_list <- list(
    create_mock_person_data(name = "Person A", roster_id = 1)
  )

  result <- get_top_works(person_data_list, n = 5, sort_by = "cited_by_count")

  expect_equal(nrow(result), 5)
  expect_true(result$cited_by_count[1] >= result$cited_by_count[2])
})

test_that("get_top_works handles empty data", {
  person_data_list <- list(
    list(name = "No Works", roster_id = 1, works = NULL)
  )

  result <- get_top_works(person_data_list)

  expect_equal(nrow(result), 0)
})

test_that("get_top_people returns sorted results", {
  metrics_df <- data.frame(
    roster_id = 1:5,
    name = paste("Person", 1:5),
    works_count = c(50, 30, 40, 10, 20),
    stringsAsFactors = FALSE
  )

  result <- get_top_people(metrics_df, metric = "works_count", n = 3)

  expect_equal(nrow(result), 3)
  expect_equal(result$works_count[1], 50)
  expect_equal(result$name[1], "Person 1")
})

# -----------------------------------------------------------------------------
# Recent Metrics Tests
# -----------------------------------------------------------------------------

test_that("compute_recent_metrics calculates correctly", {
  person_data_list <- list(
    create_mock_person_data(name = "Person A", roster_id = 1)
  )

  result <- compute_recent_metrics(person_data_list, years = 3)

  expect_true(is.data.frame(result))
  expect_true("recent_works" %in% names(result))
  expect_true("recent_citations" %in% names(result))
})

# -----------------------------------------------------------------------------
# OA Metrics Tests
# -----------------------------------------------------------------------------

test_that("compute_oa_metrics calculates percentages", {
  person_data_list <- list(
    create_mock_person_data()  # Has 5/10 OA works
  )

  result <- compute_oa_metrics(person_data_list)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_works[1], 10)
  expect_equal(result$n_oa[1], 5)
  expect_equal(result$pct_oa[1], 50)
})

test_that("compute_oa_by_year groups correctly", {
  person_data_list <- list(
    create_mock_person_data()
  )

  result <- compute_oa_by_year(person_data_list, from_year = 2020)

  expect_true(is.data.frame(result))
  expect_true("year" %in% names(result))
  expect_true("pct_oa" %in% names(result))
})

# -----------------------------------------------------------------------------
# Export Preparation Tests
# -----------------------------------------------------------------------------

test_that("prepare_export_wide merges data correctly", {
  metrics_df <- data.frame(
    roster_id = 1:2,
    name = c("A", "B"),
    works_count = c(10, 20),
    stringsAsFactors = FALSE
  )

  roster <- data.frame(
    id = 1:2,
    name = c("A", "B"),
    email = c("a@t.com", "b@t.com"),
    academic_rank = c("Prof", "Asst"),
    scopus_id = c("123", "456"),
    scholar_id = c("abc", "def"),
    stringsAsFactors = FALSE
  )

  result <- prepare_export_wide(metrics_df, roster)

  expect_true("email" %in% names(result))
  expect_true("works_count" %in% names(result))
  expect_equal(nrow(result), 2)
})
