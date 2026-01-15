# FacultyIQ - Academic Research Analytics Dashboard

A Shiny application for analyzing academic division research productivity and impact over time, at both division and individual levels.

## Overview

FacultyIQ helps academic divisions:
- Track research productivity metrics across faculty members
- Identify candidates for academic promotion
- Benchmark individuals against division and rank-level medians
- Visualize trends in publications, citations, and open access publishing

## Features

### Data Sources (Layered Approach)

The app uses multiple data sources with graceful degradation:

1. **OpenAlex** (Primary, Free) - Author profiles, publications, citations, h-index
2. **Scopus** (Optional, requires API key) - Additional author metrics
3. **Google Scholar** (Optional, fragile) - Citation history, h-index, i10-index
4. **Bibliometric Files** (Fallback) - Import Scopus/WoS/PubMed exports directly

### Metrics Computed

| Metric | Description | Source |
|--------|-------------|--------|
| Total Works | Count of indexed publications | OpenAlex |
| Total Citations | Cumulative citation count | OpenAlex/Scholar |
| h-index | h papers cited at least h times | OpenAlex/Scholar |
| i10-index | Papers with 10+ citations | OpenAlex/Scholar |
| Citations/Work | Average citations per publication | Computed |
| Works/Year | Average annual productivity | Computed |
| Open Access % | Percentage of OA publications | OpenAlex |

### Key Workflows

1. **Upload**: Import faculty roster (CSV/Excel) with names and external IDs
2. **Resolution**: Map roster entries to unique OpenAlex author IDs
3. **Dashboard**: View division-level trends and benchmarks
4. **Profiles**: Drill into individual faculty metrics
5. **Export**: Download data tables, plots, and reports

## Installation

### Prerequisites

- R >= 4.0
- RStudio (recommended)

### Required Packages

```r
install.packages(c(
  # Core Shiny
  "shiny", "shinydashboard",
  # Data manipulation
  "dplyr", "tidyr", "stringr", "readr", "readxl", "lubridate", "purrr",
  # Visualization
  "DT", "ggplot2", "plotly", "sparkline",
  # API clients
  "openalexR", "httr", "jsonlite",
  # Utilities
  "janitor", "digest", "config", "htmltools", "htmlwidgets"
))
```

### Optional Packages

```r
# For Scopus integration (requires API key)
install.packages("rscopus")

# For Google Scholar data
install.packages("scholar")

# For bibliographic file imports
install.packages("bibliometrix")
```

## Configuration

### Environment Variables

Copy `.Renviron.example` to `.Renviron` and configure:

```bash
# OpenAlex polite pool (recommended for faster rate limits)
OPENALEX_EMAIL=your.email@institution.edu

# Scopus API key (optional)
SCOPUS_API_KEY=your_scopus_api_key
```

Get your Scopus API key at: https://dev.elsevier.com/

### Configuration File

Edit `config.yml` to customize:
- Cache expiration settings
- Rate limiting parameters
- Default time windows

## Usage

### Running the App

```r
# From R console
shiny::runApp()

# Or in RStudio: Click "Run App" button
```

### Input File Format

The roster file should be CSV or Excel with these columns (all optional except Name):

| Column | Required | Description |
|--------|----------|-------------|
| Name | Yes | Faculty member's full name |
| Email | Recommended | Email address (kept private, not sent to APIs) |
| Academic Rank | Recommended | Current rank (Assistant/Associate/Full Professor, etc.) |
| Last Promotion Date | Optional | Year or date of last promotion |
| REAIMS Publications | Optional | Self-reported publication count |
| Scopus ID | Recommended | Scopus Author ID (enables auto-resolution) |
| Google Scholar ID | Recommended | Scholar profile ID |
| Associations | Optional | Professional associations and roles |

See `data/sample_roster.csv` for an example.

### Typical Workflow

1. **Upload** your roster CSV/Excel file
2. **Validate** - Review column mapping and completeness
3. **Auto-resolve** - Click to resolve authors with Scopus IDs
4. **Manual resolve** - Search and select for remaining authors
5. **Fetch data** - Pull metrics from OpenAlex
6. **Explore** - View dashboard and individual profiles
7. **Export** - Download reports and data

## File Structure

```
FacultyIQ/
├── app.R                 # Main Shiny application
├── global.R              # Package loading and configuration
├── config.yml            # Application configuration
├── .Renviron.example     # Environment variable template
├── R/
│   ├── mod_upload.R      # Data upload module
│   ├── mod_resolution.R  # Identity resolution module
│   ├── mod_dashboard.R   # Division dashboard module
│   ├── mod_profile.R     # Individual profile module
│   ├── mod_export.R      # Export module
│   ├── utils_api.R       # API client functions
│   ├── utils_cache.R     # Caching utilities
│   ├── utils_metrics.R   # Metric computation
│   └── utils_validation.R # Data validation
├── data/
│   └── sample_roster.csv # Example roster file
├── cache/                # API response cache
├── tests/
│   └── testthat/         # Unit tests
└── www/                  # Static assets
```

## Data Privacy

- **Emails are sensitive**: Never sent to external APIs
- **Only names and IDs** are used for API queries
- **Cache locally**: All API responses cached to minimize external requests
- **Identity mappings**: Stored locally, never transmitted

## Handling Missing Data

The app gracefully handles missing data:

- Missing IDs: Shows "pending" status, allows manual resolution
- API failures: Displays "unavailable" with reason
- Partial data: Uses best available source
- Empty fields: Excluded from aggregations

The "Data Completeness" panel shows missingness by field and person.

## Troubleshooting

### Common Issues

**"OpenAlex rate limited"**
- Set `OPENALEX_EMAIL` for polite pool access
- Wait a few minutes and retry

**"Scopus not configured"**
- This is informational; Scopus is optional
- Set `SCOPUS_API_KEY` to enable

**"Google Scholar blocked"**
- Scholar scraping is fragile; this is expected
- Data will come from OpenAlex instead

**"No results for author search"**
- Try variations of the name
- Add institution keywords
- Enter OpenAlex ID directly if known

### Running Tests

```r
testthat::test_dir("tests/testthat")
```

## License

MIT License

## Acknowledgments

- [OpenAlex](https://openalex.org/) - Free, open bibliographic data
- [openalexR](https://github.com/ropensci/openalexR) - R client for OpenAlex
- [scholar](https://github.com/jkeirstead/scholar) - Google Scholar scraping
- [bibliometrix](https://www.bibliometrix.org/) - Bibliometric analysis

---

*Originally designed for tracking division-wide metrics to support academic promotion decisions.*
