# =============================================================================
# FacultyIQ - Caching Utilities
# =============================================================================
# Functions for caching API responses and computed data
# Uses local RDS files with configurable expiration

#' Initialize cache directory
#'
#' @param cache_dir Path to cache directory
#' @return TRUE if successful
init_cache <- function(cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(TRUE)
}

#' Generate cache key from parameters
#'
#' @param prefix Cache type prefix (e.g., "openalex", "scholar")
#' @param ... Additional parameters to include in key
#' @return Character string cache key
make_cache_key <- function(prefix, ...) {
  params <- list(...)
  # Create hash of parameters
  param_str <- paste(sapply(params, as.character), collapse = "_")
  key <- paste0(prefix, "_", digest::digest(param_str, algo = "md5"))
  return(key)
}

#' Get cache file path
#'
#' @param key Cache key
#' @param cache_dir Cache directory
#' @return File path
get_cache_path <- function(key, cache_dir = "cache") {
  file.path(cache_dir, paste0(key, ".rds"))
}

#' Check if cache exists and is valid
#'
#' @param key Cache key
#' @param expiry_days Number of days until expiration
#' @param cache_dir Cache directory
#' @return TRUE if valid cache exists
cache_exists <- function(key, expiry_days = 7, cache_dir = "cache") {
  path <- get_cache_path(key, cache_dir)

  if (!file.exists(path)) {
    return(FALSE)
  }

  # Check file age
  file_time <- file.info(path)$mtime
  age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))

  return(age_days <= expiry_days)
}

#' Read from cache
#'
#' @param key Cache key
#' @param expiry_days Number of days until expiration
#' @param cache_dir Cache directory
#' @return Cached data or NULL if not found/expired
cache_get <- function(key, expiry_days = 7, cache_dir = "cache") {
  if (!cache_exists(key, expiry_days, cache_dir)) {
    return(NULL)
  }

  path <- get_cache_path(key, cache_dir)
  tryCatch({
    readRDS(path)
  }, error = function(e) {
    warning(sprintf("Failed to read cache %s: %s", key, e$message))
    NULL
  })
}

#' Write to cache
#'
#' @param key Cache key
#' @param data Data to cache
#' @param cache_dir Cache directory
#' @return TRUE if successful
cache_set <- function(key, data, cache_dir = "cache") {
  init_cache(cache_dir)
  path <- get_cache_path(key, cache_dir)

  tryCatch({
    saveRDS(data, path)
    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to write cache %s: %s", key, e$message))
    FALSE
  })
}

#' Delete specific cache entry
#'
#' @param key Cache key
#' @param cache_dir Cache directory
#' @return TRUE if deleted (or didn't exist)
cache_delete <- function(key, cache_dir = "cache") {
  path <- get_cache_path(key, cache_dir)
  if (file.exists(path)) {
    file.remove(path)
  }
  return(TRUE)
}

#' Clear all cache entries
#'
#' @param cache_dir Cache directory
#' @return Number of files deleted
cache_clear_all <- function(cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    return(0)
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  n_deleted <- sum(file.remove(files))
  return(n_deleted)
}

#' Clear expired cache entries
#'
#' @param expiry_days Number of days until expiration
#' @param cache_dir Cache directory
#' @return Number of files deleted
cache_clear_expired <- function(expiry_days = 7, cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    return(0)
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  n_deleted <- 0

  for (f in files) {
    file_time <- file.info(f)$mtime
    age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))

    if (age_days > expiry_days) {
      if (file.remove(f)) {
        n_deleted <- n_deleted + 1
      }
    }
  }

  return(n_deleted)
}

#' Get cache statistics
#'
#' @param cache_dir Cache directory
#' @return List with cache statistics
cache_stats <- function(cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    return(list(
      n_files = 0,
      total_size_mb = 0,
      oldest_file = NA,
      newest_file = NA
    ))
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    return(list(
      n_files = 0,
      total_size_mb = 0,
      oldest_file = NA,
      newest_file = NA
    ))
  }

  file_info <- file.info(files)
  total_size <- sum(file_info$size, na.rm = TRUE)

  list(
    n_files = length(files),
    total_size_mb = round(total_size / (1024 * 1024), 2),
    oldest_file = min(file_info$mtime, na.rm = TRUE),
    newest_file = max(file_info$mtime, na.rm = TRUE)
  )
}

#' Memoized function wrapper using file cache
#'
#' @param f Function to memoize
#' @param prefix Cache key prefix
#' @param expiry_days Cache expiration in days
#' @param cache_dir Cache directory
#' @return Memoized function
memoize_with_cache <- function(f, prefix, expiry_days = 7, cache_dir = "cache") {
  function(...) {
    key <- make_cache_key(prefix, ...)
    cached <- cache_get(key, expiry_days, cache_dir)

    if (!is.null(cached)) {
      return(cached)
    }

    result <- f(...)
    cache_set(key, result, cache_dir)
    return(result)
  }
}

#' Save identity mappings to persistent storage
#'
#' @param mappings Data frame with identity mappings
#' @param file_path Path to save file
#' @return TRUE if successful
save_identity_mappings <- function(mappings, file_path = "cache/identity_mappings.rds") {
  tryCatch({
    saveRDS(mappings, file_path)
    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to save identity mappings: %s", e$message))
    FALSE
  })
}

#' Load identity mappings from persistent storage
#'
#' @param file_path Path to mappings file
#' @return Data frame with mappings or NULL
load_identity_mappings <- function(file_path = "cache/identity_mappings.rds") {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  tryCatch({
    readRDS(file_path)
  }, error = function(e) {
    warning(sprintf("Failed to load identity mappings: %s", e$message))
    NULL
  })
}
