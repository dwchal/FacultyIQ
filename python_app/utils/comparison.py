"""Faculty comparison utilities for FacultyIQ"""

from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime
from collections import defaultdict
import logging

import pandas as pd
import numpy as np
from scipy import stats

from .metrics import compute_person_metrics, create_yearly_timeseries
from .api import Author, Work

logger = logging.getLogger(__name__)


def compare_faculty(
    faculty_data: List[Dict],
    metrics_to_compare: Optional[List[str]] = None,
) -> pd.DataFrame:
    """
    Create side-by-side comparison of multiple faculty members.
    Returns DataFrame with metrics for each person as columns.
    """
    if metrics_to_compare is None:
        metrics_to_compare = [
            "total_works",
            "total_citations",
            "h_index",
            "i10_index",
            "citations_per_work",
            "works_per_year",
            "oa_percentage",
            "first_pub_year",
            "last_pub_year",
            "career_length_years",
        ]

    comparison_data = []

    for person_data in faculty_data:
        person = person_data.get("person", {})
        author = person_data.get("openalex_author")
        works = person_data.get("works", [])

        metrics = compute_person_metrics(author, works)

        row = {
            "name": person.get("name", "Unknown"),
            "rank": person.get("rank"),
        }

        for metric in metrics_to_compare:
            row[metric] = metrics.get(metric, 0)

        comparison_data.append(row)

    return pd.DataFrame(comparison_data)


def extract_career_trajectory(
    person_data: Dict,
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> pd.DataFrame:
    """
    Extract yearly productivity trajectory for a single person.
    Returns DataFrame with year, works, citations, cumulative metrics.
    """
    if from_year is None:
        from_year = 1990
    if to_year is None:
        to_year = datetime.now().year

    person = person_data.get("person", {})
    author = person_data.get("openalex_author")
    works = person_data.get("works", [])

    # Build yearly data from author counts_by_year
    yearly_data = {}

    if author and author.counts_by_year:
        for year_data in author.counts_by_year:
            year = year_data.get("year")
            if year and from_year <= year <= to_year:
                yearly_data[year] = {
                    "works": year_data.get("works_count", 0),
                    "citations": year_data.get("cited_by_count", 0),
                }

    # Supplement with works data if needed
    works_by_year = defaultdict(int)
    for work in works:
        year = work.publication_year
        if year and from_year <= year <= to_year:
            works_by_year[year] += 1

    for year, count in works_by_year.items():
        if year not in yearly_data:
            yearly_data[year] = {"works": count, "citations": 0}

    # Create complete timeline
    rows = []
    cumulative_works = 0
    cumulative_citations = 0

    for year in range(from_year, to_year + 1):
        data = yearly_data.get(year, {"works": 0, "citations": 0})
        cumulative_works += data["works"]
        cumulative_citations += data["citations"]

        rows.append({
            "year": year,
            "name": person.get("name", "Unknown"),
            "works": data["works"],
            "citations": data["citations"],
            "cumulative_works": cumulative_works,
            "cumulative_citations": cumulative_citations,
        })

    return pd.DataFrame(rows)


def compare_trajectories(
    faculty_data: List[Dict],
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> pd.DataFrame:
    """
    Compare career trajectories of multiple faculty members.
    Returns long-format DataFrame suitable for plotting.
    """
    trajectories = []

    for person_data in faculty_data:
        traj = extract_career_trajectory(person_data, from_year, to_year)
        trajectories.append(traj)

    if trajectories:
        return pd.concat(trajectories, ignore_index=True)
    return pd.DataFrame()


def analyze_career_phases(
    person_data: Dict,
    window_years: int = 5,
) -> Dict[str, Any]:
    """
    Analyze career phases (growth, plateau, decline) based on productivity trends.
    Uses rolling window analysis.
    """
    trajectory = extract_career_trajectory(person_data)

    if trajectory.empty or len(trajectory) < window_years:
        return {
            "phases": [],
            "current_phase": "unknown",
            "trend_direction": "unknown",
            "message": "Insufficient data for phase analysis",
        }

    # Calculate rolling statistics
    trajectory["works_ma"] = trajectory["works"].rolling(
        window=window_years, min_periods=1
    ).mean()
    trajectory["works_growth"] = trajectory["works_ma"].pct_change()

    # Identify phases
    phases = []
    current_phase = None
    phase_start_year = None

    for _, row in trajectory.iterrows():
        year = row["year"]
        growth = row["works_growth"]

        if pd.isna(growth):
            phase = "early"
        elif growth > 0.1:
            phase = "growth"
        elif growth < -0.1:
            phase = "decline"
        else:
            phase = "stable"

        if phase != current_phase:
            if current_phase is not None:
                phases.append({
                    "phase": current_phase,
                    "start_year": phase_start_year,
                    "end_year": year - 1,
                })
            current_phase = phase
            phase_start_year = year

    # Add final phase
    if current_phase is not None:
        phases.append({
            "phase": current_phase,
            "start_year": phase_start_year,
            "end_year": trajectory["year"].max(),
        })

    # Determine overall trend from recent years
    recent = trajectory.tail(window_years)
    recent_growth = recent["works_ma"].iloc[-1] - recent["works_ma"].iloc[0]

    if recent_growth > 0:
        trend_direction = "increasing"
    elif recent_growth < 0:
        trend_direction = "decreasing"
    else:
        trend_direction = "stable"

    return {
        "phases": phases,
        "current_phase": current_phase,
        "trend_direction": trend_direction,
        "recent_avg_works": round(recent["works"].mean(), 2),
        "career_avg_works": round(trajectory["works"].mean(), 2),
    }


def create_comparison_summary(
    faculty_data: List[Dict],
) -> Dict[str, Any]:
    """
    Create narrative summary comparing faculty members.
    """
    if not faculty_data:
        return {"summary": "No faculty data provided"}

    comparison = compare_faculty(faculty_data)

    if comparison.empty:
        return {"summary": "Unable to compute comparison"}

    # Find leaders in each category
    leaders = {}
    metrics = ["total_works", "total_citations", "h_index", "works_per_year"]

    for metric in metrics:
        if metric in comparison.columns:
            max_idx = comparison[metric].idxmax()
            leaders[metric] = {
                "name": comparison.loc[max_idx, "name"],
                "value": comparison.loc[max_idx, metric],
            }

    # Generate insights
    insights = []

    if "total_works" in leaders:
        insights.append(
            f"{leaders['total_works']['name']} has the most publications "
            f"({leaders['total_works']['value']} works)"
        )

    if "h_index" in leaders:
        insights.append(
            f"{leaders['h_index']['name']} has the highest h-index "
            f"({leaders['h_index']['value']})"
        )

    if "works_per_year" in leaders:
        insights.append(
            f"{leaders['works_per_year']['name']} is most productive "
            f"({leaders['works_per_year']['value']:.1f} works/year)"
        )

    # Compute variance/spread
    spreads = {}
    for metric in metrics:
        if metric in comparison.columns:
            values = comparison[metric].dropna()
            if len(values) > 1:
                spreads[metric] = {
                    "mean": round(values.mean(), 2),
                    "std": round(values.std(), 2),
                    "cv": round(values.std() / values.mean(), 2) if values.mean() > 0 else 0,
                    "range": round(values.max() - values.min(), 2),
                }

    return {
        "faculty_count": len(faculty_data),
        "leaders": leaders,
        "insights": insights,
        "spreads": spreads,
        "comparison_table": comparison.to_dict(orient="records"),
    }


def rank_faculty(
    data: List[Dict],
    metric: str = "h_index",
    ascending: bool = False,
) -> pd.DataFrame:
    """
    Rank all faculty by a specific metric.
    """
    comparison = compare_faculty(data)

    if comparison.empty or metric not in comparison.columns:
        return pd.DataFrame()

    ranked = comparison.sort_values(metric, ascending=ascending).reset_index(drop=True)
    ranked["rank_position"] = range(1, len(ranked) + 1)

    return ranked[["rank_position", "name", "rank", metric]]


def find_similar_faculty(
    target_data: Dict,
    all_data: List[Dict],
    n: int = 5,
    metrics: Optional[List[str]] = None,
) -> List[Dict]:
    """
    Find faculty most similar to target based on metrics.
    Uses Euclidean distance on normalized metrics.
    """
    if metrics is None:
        metrics = ["total_works", "total_citations", "h_index", "works_per_year"]

    target_person = target_data.get("person", {})
    target_name = target_person.get("name")

    # Get all metrics
    all_metrics = []
    for person_data in all_data:
        person = person_data.get("person", {})
        if person.get("name") == target_name:
            continue

        author = person_data.get("openalex_author")
        works = person_data.get("works", [])
        person_metrics = compute_person_metrics(author, works)

        if not person_metrics.get("has_data"):
            continue

        all_metrics.append({
            "name": person.get("name"),
            "rank": person.get("rank"),
            **{m: person_metrics.get(m, 0) for m in metrics},
        })

    if not all_metrics:
        return []

    # Get target metrics
    target_author = target_data.get("openalex_author")
    target_works = target_data.get("works", [])
    target_metrics = compute_person_metrics(target_author, target_works)

    if not target_metrics.get("has_data"):
        return []

    target_values = np.array([target_metrics.get(m, 0) for m in metrics])

    # Normalize using min-max across all faculty
    all_values = np.array([[p[m] for m in metrics] for p in all_metrics])

    # Handle case where all values are the same
    mins = all_values.min(axis=0)
    maxs = all_values.max(axis=0)
    ranges = maxs - mins
    ranges[ranges == 0] = 1  # Avoid division by zero

    normalized_target = (target_values - mins) / ranges
    normalized_all = (all_values - mins) / ranges

    # Calculate distances
    distances = np.linalg.norm(normalized_all - normalized_target, axis=1)

    # Get top N similar
    similar_indices = np.argsort(distances)[:n]

    results = []
    for idx in similar_indices:
        person = all_metrics[idx]
        results.append({
            "name": person["name"],
            "rank": person["rank"],
            "similarity_score": round(1 / (1 + distances[idx]), 3),
            **{m: person[m] for m in metrics},
        })

    return results


def compute_collaboration_potential(
    faculty_a: Dict,
    faculty_b: Dict,
) -> Dict[str, Any]:
    """
    Analyze potential for collaboration based on complementary strengths.
    """
    author_a = faculty_a.get("openalex_author")
    author_b = faculty_b.get("openalex_author")
    works_a = faculty_a.get("works", [])
    works_b = faculty_b.get("works", [])

    metrics_a = compute_person_metrics(author_a, works_a)
    metrics_b = compute_person_metrics(author_b, works_b)

    person_a = faculty_a.get("person", {})
    person_b = faculty_b.get("person", {})

    # Find complementary areas
    strengths_a = []
    strengths_b = []

    comparison_metrics = [
        ("total_works", "Publication Volume"),
        ("total_citations", "Citation Impact"),
        ("h_index", "Sustained Impact"),
        ("works_per_year", "Productivity"),
        ("oa_percentage", "Open Access"),
    ]

    for metric, label in comparison_metrics:
        val_a = metrics_a.get(metric, 0)
        val_b = metrics_b.get(metric, 0)

        if val_a > val_b * 1.2:
            strengths_a.append(label)
        elif val_b > val_a * 1.2:
            strengths_b.append(label)

    return {
        "faculty_a": person_a.get("name"),
        "faculty_b": person_b.get("name"),
        "strengths_a": strengths_a,
        "strengths_b": strengths_b,
        "complementary": len(strengths_a) > 0 and len(strengths_b) > 0,
        "metrics_a": {k: v for k, v in metrics_a.items() if k != "works_by_year" and k != "citations_by_year"},
        "metrics_b": {k: v for k, v in metrics_b.items() if k != "works_by_year" and k != "citations_by_year"},
    }
