# =============================================================================
# FacultyIQ - Global Configuration
# =============================================================================
# Load packages, source modules, and set up configuration

# -----------------------------------------------------------------------------
# Package Dependencies
# -----------------------------------------------------------------------------

# Core Shiny packages
library(shiny)
library(shinydashboard)

# Data manipulation (tidyverse-first)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(lubridate)
library(purrr)

# Tables and visualization
library(DT)
library(ggplot2)
library(plotly)
library(sparkline)

# API clients
library(openalexR)
library(httr)
library(jsonlite)

# Optional packages - load if available
# Scopus (rscopus) - optional, requires API key
if (requireNamespace("rscopus", quietly = TRUE)) {
  library(rscopus)
  message("rscopus loaded - Scopus enrichment available")
} else {
  message("rscopus not installed - Scopus enrichment disabled")
}

# Google Scholar (scholar) - optional, fragile scraping
if (requireNamespace("scholar", quietly = TRUE)) {
  library(scholar)
  message("scholar package loaded - Google Scholar data available")
} else {
  message("scholar package not installed - Scholar data disabled")
}

# Bibliometrix (bibliometrix) - optional, for file imports
if (requireNamespace("bibliometrix", quietly = TRUE)) {
  library(bibliometrix)
  message("bibliometrix loaded - Bibliographic file import available")
} else {
  message("bibliometrix not installed - Bibliographic import disabled")
}

# Utility packages
library(janitor)
library(digest)
library(config)
library(htmltools)
library(htmlwidgets)

# -----------------------------------------------------------------------------
# Load Configuration
# -----------------------------------------------------------------------------

# Load config from config.yml
app_config <- tryCatch({
  config::get()
}, error = function(e) {
  message("Could not load config.yml, using defaults")
  list(
    app_title = "FacultyIQ - Academic Research Analytics",
    cache_dir = "cache",
    cache_expiry_days = 7,
    openalex = list(enabled = TRUE, mailto = "", rate_limit_per_second = 10),
    scopus = list(enabled = TRUE, api_key = ""),
    scholar = list(enabled = TRUE, rate_limit_per_second = 0.5),
    data = list(max_roster_size = 500, recent_years = 5, min_year = 1990)
  )
})

# Set up OpenAlex polite pool
openalex_email <- Sys.getenv("OPENALEX_EMAIL", "")
if (openalex_email != "") {
  options(openalexR.mailto = openalex_email)
  message(sprintf("OpenAlex polite pool enabled with email: %s", openalex_email))
} else {
  message("OPENALEX_EMAIL not set - using standard rate limits")
}

# Check Scopus configuration
scopus_configured <- Sys.getenv("SCOPUS_API_KEY", "") != ""
if (scopus_configured) {
  message("Scopus API key found - enrichment enabled")
} else {
  message("SCOPUS_API_KEY not set - Scopus enrichment disabled")
}

# -----------------------------------------------------------------------------
# Source Utility Functions
# -----------------------------------------------------------------------------

source("R/utils_validation.R")
source("R/utils_cache.R")
source("R/utils_api.R")
source("R/utils_metrics.R")
source("R/utils_prediction.R")

# -----------------------------------------------------------------------------
# Source Shiny Modules
# -----------------------------------------------------------------------------

source("R/mod_upload.R")
source("R/mod_resolution.R")
source("R/mod_dashboard.R")
source("R/mod_profile.R")
source("R/mod_export.R")
source("R/mod_prediction.R")

# -----------------------------------------------------------------------------
# Initialize Cache
# -----------------------------------------------------------------------------

init_cache(app_config$cache_dir %||% "cache")

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Safe NA check
is_missing <- function(x) {
  is.null(x) || is.na(x) || x == "" || x == "NA"
}

# Format large numbers
format_number <- function(x) {
  if (is.na(x)) return("N/A")
  format(x, big.mark = ",", scientific = FALSE)
}

# -----------------------------------------------------------------------------
# App-wide Settings
# -----------------------------------------------------------------------------

# Set ggplot2 theme
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))

# DT options
options(DT.options = list(
  pageLength = 10,
  language = list(search = "Filter:"),
  dom = "lfrtip"
))

# Shiny options
options(shiny.maxRequestSize = 50 * 1024^2)  # 50MB max upload

message("FacultyIQ initialized successfully")
