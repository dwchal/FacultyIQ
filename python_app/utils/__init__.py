"""FacultyIQ Utility Modules"""

from .cache import CacheManager
from .api import OpenAlexAPI, fetch_person_data, fetch_all_person_data
from .validation import validate_roster, normalize_column_names, clean_roster, standardize_rank
from .metrics import (
    compute_person_metrics,
    compute_all_metrics,
    create_yearly_timeseries,
    compute_division_summary,
    get_top_works,
    get_top_people,
)
from .prediction import compute_rank_benchmarks, predict_rank, identify_promotion_candidates
from .comparison import compare_faculty, extract_career_trajectory, analyze_career_phases

__all__ = [
    "CacheManager",
    "OpenAlexAPI",
    "fetch_person_data",
    "fetch_all_person_data",
    "validate_roster",
    "normalize_column_names",
    "clean_roster",
    "standardize_rank",
    "compute_person_metrics",
    "compute_all_metrics",
    "create_yearly_timeseries",
    "compute_division_summary",
    "get_top_works",
    "get_top_people",
    "compute_rank_benchmarks",
    "predict_rank",
    "identify_promotion_candidates",
    "compare_faculty",
    "extract_career_trajectory",
    "analyze_career_phases",
]
