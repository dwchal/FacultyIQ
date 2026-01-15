# =============================================================================
# FacultyIQ - Cache Tests
# =============================================================================

library(testthat)

# Load required packages and source files
suppressPackageStartupMessages({
  library(digest)
})

source("../../R/utils_cache.R")

# Use a temp directory for tests
test_cache_dir <- file.path(tempdir(), "facultyiq_test_cache")

# Clean up before tests
if (dir.exists(test_cache_dir)) {
  unlink(test_cache_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Cache Initialization Tests
# -----------------------------------------------------------------------------

test_that("init_cache creates directory", {
  result <- init_cache(test_cache_dir)

  expect_true(result)
  expect_true(dir.exists(test_cache_dir))
})

# -----------------------------------------------------------------------------
# Cache Key Tests
# -----------------------------------------------------------------------------

test_that("make_cache_key generates consistent keys", {
  key1 <- make_cache_key("test", "a", "b", "c")
  key2 <- make_cache_key("test", "a", "b", "c")
  key3 <- make_cache_key("test", "a", "b", "d")

  expect_equal(key1, key2)
  expect_false(key1 == key3)
})

test_that("make_cache_key includes prefix", {
  key <- make_cache_key("myprefix", "value")

  expect_true(grepl("^myprefix_", key))
})

# -----------------------------------------------------------------------------
# Cache Read/Write Tests
# -----------------------------------------------------------------------------

test_that("cache_set and cache_get work correctly", {
  test_data <- list(x = 1, y = "test", z = c(1, 2, 3))
  key <- "test_key_1"

  # Write
  result <- cache_set(key, test_data, test_cache_dir)
  expect_true(result)

  # Read
  retrieved <- cache_get(key, expiry_days = 1, test_cache_dir)
  expect_equal(retrieved, test_data)
})

test_that("cache_get returns NULL for missing key", {
  result <- cache_get("nonexistent_key", expiry_days = 1, test_cache_dir)

  expect_null(result)
})

test_that("cache_exists returns TRUE for existing key", {
  key <- "test_exists_key"
  cache_set(key, "data", test_cache_dir)

  result <- cache_exists(key, expiry_days = 1, test_cache_dir)

  expect_true(result)
})

test_that("cache_exists returns FALSE for expired key", {
  key <- "test_expired_key"
  cache_set(key, "data", test_cache_dir)

  # Set expiry to -1 days (already expired)
  result <- cache_exists(key, expiry_days = -1, test_cache_dir)

  expect_false(result)
})

# -----------------------------------------------------------------------------
# Cache Delete Tests
# -----------------------------------------------------------------------------

test_that("cache_delete removes entry", {
  key <- "test_delete_key"
  cache_set(key, "data", test_cache_dir)

  # Verify exists
  expect_true(cache_exists(key, expiry_days = 1, test_cache_dir))

  # Delete
  result <- cache_delete(key, test_cache_dir)
  expect_true(result)

  # Verify deleted
  expect_false(cache_exists(key, expiry_days = 1, test_cache_dir))
})

test_that("cache_delete succeeds for nonexistent key", {
  result <- cache_delete("never_existed", test_cache_dir)

  expect_true(result)
})

# -----------------------------------------------------------------------------
# Cache Stats Tests
# -----------------------------------------------------------------------------

test_that("cache_stats returns correct structure", {
  # Add some data
  cache_set("stats_test_1", "data1", test_cache_dir)
  cache_set("stats_test_2", "data2", test_cache_dir)

  stats <- cache_stats(test_cache_dir)

  expect_true(is.list(stats))
  expect_true("n_files" %in% names(stats))
  expect_true("total_size_mb" %in% names(stats))
  expect_true(stats$n_files >= 2)
})

test_that("cache_stats handles empty directory", {
  empty_dir <- file.path(tempdir(), "empty_cache_test")
  dir.create(empty_dir, showWarnings = FALSE)

  stats <- cache_stats(empty_dir)

  expect_equal(stats$n_files, 0)
  expect_equal(stats$total_size_mb, 0)

  unlink(empty_dir, recursive = TRUE)
})

# -----------------------------------------------------------------------------
# Cache Clear Tests
# -----------------------------------------------------------------------------

test_that("cache_clear_all removes all entries", {
  clear_test_dir <- file.path(tempdir(), "clear_test_cache")
  init_cache(clear_test_dir)

  cache_set("clear1", "data1", clear_test_dir)
  cache_set("clear2", "data2", clear_test_dir)
  cache_set("clear3", "data3", clear_test_dir)

  n_deleted <- cache_clear_all(clear_test_dir)

  expect_equal(n_deleted, 3)

  stats <- cache_stats(clear_test_dir)
  expect_equal(stats$n_files, 0)

  unlink(clear_test_dir, recursive = TRUE)
})

# -----------------------------------------------------------------------------
# Identity Mapping Tests
# -----------------------------------------------------------------------------

test_that("save_identity_mappings and load_identity_mappings work", {
  mappings <- data.frame(
    name = c("Alice", "Bob"),
    openalex_id = c("A123", "A456"),
    stringsAsFactors = FALSE
  )

  mapping_file <- file.path(test_cache_dir, "test_mappings.rds")

  # Save
  result <- save_identity_mappings(mappings, mapping_file)
  expect_true(result)

  # Load
  loaded <- load_identity_mappings(mapping_file)
  expect_equal(loaded, mappings)
})

test_that("load_identity_mappings returns NULL for missing file", {
  result <- load_identity_mappings(file.path(test_cache_dir, "nonexistent.rds"))

  expect_null(result)
})

# -----------------------------------------------------------------------------
# Memoization Tests
# -----------------------------------------------------------------------------

test_that("memoize_with_cache caches function results", {
  call_count <- 0

  # Function that counts calls
  expensive_fn <- function(x) {
    call_count <<- call_count + 1
    x * 2
  }

  # Memoize it
  memoized_fn <- memoize_with_cache(expensive_fn, "memo_test",
                                     expiry_days = 1, cache_dir = test_cache_dir)

  # First call - should execute function
  result1 <- memoized_fn(5)
  expect_equal(result1, 10)
  expect_equal(call_count, 1)

  # Second call with same arg - should use cache
  result2 <- memoized_fn(5)
  expect_equal(result2, 10)
  expect_equal(call_count, 1)  # Count unchanged

  # Different arg - should execute function
  result3 <- memoized_fn(7)
  expect_equal(result3, 14)
  expect_equal(call_count, 2)
})

# -----------------------------------------------------------------------------
# Cleanup
# -----------------------------------------------------------------------------

# Clean up test cache directory
if (dir.exists(test_cache_dir)) {
  unlink(test_cache_dir, recursive = TRUE)
}
