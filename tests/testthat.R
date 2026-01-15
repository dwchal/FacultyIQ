# =============================================================================
# FacultyIQ Test Runner
# =============================================================================
# Run with: testthat::test_dir("tests/testthat")

library(testthat)

# Source the files to test
source("../R/utils_validation.R")
source("../R/utils_cache.R")
source("../R/utils_metrics.R")

# Run tests
test_check("FacultyIQ")
