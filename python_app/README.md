# FacultyIQ Python

**Academic Research Analytics Dashboard** - Python/Streamlit Version

FacultyIQ is a comprehensive analytics platform for tracking and analyzing faculty research productivity and impact metrics. This Python version provides the same functionality as the original R/Shiny application with enhanced features.

## Features

- **Identity Resolution**: Match faculty to OpenAlex profiles using Scopus ID, ORCID, or manual search
- **Division Dashboard**: Aggregate analytics with interactive visualizations
- **Faculty Profiles**: Individual career trajectories and publication lists
- **Rank Prediction**: Benchmark faculty against peers and identify promotion candidates
- **Faculty Comparison**: Side-by-side analysis and similarity finding
- **Data Export**: Export metrics, publications, and time series data

## Quick Start

### Prerequisites

- Python 3.9 or higher
- pip package manager

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd FacultyIQ/python_app

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Copy environment template
cp .env.example .env
# Edit .env with your settings (optional)
```

### Running the Application

```bash
# Start the Streamlit server
streamlit run app.py

# The application will open in your browser at http://localhost:8501
```

## Configuration

### Environment Variables

Set these in your `.env` file:

| Variable | Description | Required |
|----------|-------------|----------|
| `OPENALEX_EMAIL` | Email for OpenAlex polite pool | Recommended |
| `SCOPUS_API_KEY` | Scopus API key for enhanced data | Optional |
| `SCHOLAR_RATE_LIMIT` | Seconds between Google Scholar requests | Optional |

### config.yaml

The `config.yaml` file contains application settings:

```yaml
default:
  app:
    title: "FacultyIQ - Academic Research Analytics"
    max_roster_size: 500

  cache:
    enabled: true
    directory: ".cache"
    expiration_days: 7

  openalex:
    per_page: 200
    max_works: 1000
```

## Usage

### 1. Upload Roster

Upload a CSV or Excel file containing faculty information:

**Required columns:**
- `name` - Faculty member name

**Recommended columns:**
- `email` - Email address
- `rank` - Academic rank (e.g., "Assistant Professor")
- `scopus_id` - Scopus Author ID (for auto-resolution)
- `orcid` - ORCID identifier
- `scholar_id` - Google Scholar profile ID

### 2. Identity Resolution

The system will attempt to auto-resolve faculty using:
1. Scopus ID → OpenAlex lookup
2. ORCID → OpenAlex lookup
3. Manual search for remaining entries

### 3. Explore Analytics

Once identities are resolved:
- **Dashboard**: View division-level metrics and trends
- **Profiles**: Explore individual faculty details
- **Prediction**: Analyze rank appropriateness
- **Comparison**: Compare faculty side-by-side
- **Export**: Download data for external use

## Data Sources

FacultyIQ uses a layered data fetching approach:

1. **OpenAlex** (Primary) - Free, comprehensive academic data
2. **Scopus** (Optional) - Enhanced author metrics
3. **Google Scholar** (Optional) - Citation history, may have rate limits

## Project Structure

```
python_app/
├── app.py                 # Main application entry
├── config.yaml            # Configuration settings
├── requirements.txt       # Python dependencies
├── .env.example           # Environment template
├── pages/
│   ├── resolution.py      # Identity resolution
│   ├── dashboard.py       # Division dashboard
│   ├── profiles.py        # Faculty profiles
│   ├── prediction.py      # Rank prediction
│   ├── comparison.py      # Faculty comparison
│   └── export.py          # Data export
├── utils/
│   ├── api.py             # API clients
│   ├── cache.py           # Caching system
│   ├── metrics.py         # Metrics computation
│   ├── validation.py      # Data validation
│   ├── prediction.py      # Rank prediction
│   └── comparison.py      # Comparison utilities
└── data/
    └── sample_roster.csv  # Example data
```

## Development

### Running Tests

```bash
pytest tests/ -v
```

### Code Style

```bash
# Type checking
mypy utils/ pages/

# Format code
black .
```

## Comparison with R/Shiny Version

| Feature | R/Shiny | Python/Streamlit |
|---------|---------|------------------|
| Language | R | Python |
| Framework | Shiny + shinydashboard | Streamlit |
| Visualization | ggplot2 + plotly | plotly |
| Data handling | tidyverse | pandas |
| API clients | openalexR, rscopus | requests, scholarly |
| Caching | RDS files | pickle files |

Both versions provide equivalent functionality with similar workflows.

## License

MIT License - See LICENSE file for details.

## Support

- Report issues at: [GitHub Issues]
- Documentation: [Wiki]
