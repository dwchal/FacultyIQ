"""Metrics computation utilities for FacultyIQ"""

from typing import Dict, List, Optional, Any
from datetime import datetime
from collections import defaultdict
import logging

import pandas as pd
import numpy as np

from .api import Author, Work

logger = logging.getLogger(__name__)


def compute_person_metrics(
    author: Optional[Author],
    works: List[Work],
    person_data: Optional[Dict] = None,
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> Dict[str, Any]:
    """
    Compute comprehensive metrics for a single person.
    Combines data from OpenAlex author profile and works.
    """
    metrics = {
        "total_works": 0,
        "total_citations": 0,
        "h_index": 0,
        "i10_index": 0,
        "citations_per_work": 0.0,
        "works_per_year": 0.0,
        "oa_count": 0,
        "oa_percentage": 0.0,
        "first_pub_year": None,
        "last_pub_year": None,
        "career_length_years": 0,
        "works_by_year": {},
        "citations_by_year": {},
        "has_data": False,
    }

    # Filter works by year range
    filtered_works = works
    if from_year or to_year:
        filtered_works = [
            w for w in works
            if w.publication_year and
            (from_year is None or w.publication_year >= from_year) and
            (to_year is None or w.publication_year <= to_year)
        ]

    if author:
        # Use author-level metrics when available
        metrics["total_works"] = author.works_count
        metrics["total_citations"] = author.cited_by_count
        metrics["h_index"] = author.h_index
        metrics["i10_index"] = author.i10_index
        metrics["has_data"] = True

        # Extract yearly counts from author profile
        for year_data in author.counts_by_year:
            year = year_data.get("year")
            if year:
                if from_year and year < from_year:
                    continue
                if to_year and year > to_year:
                    continue
                metrics["works_by_year"][year] = year_data.get("works_count", 0)
                metrics["citations_by_year"][year] = year_data.get("cited_by_count", 0)

    # Compute metrics from works if available
    if filtered_works:
        # Use works-based count if no author profile
        if not author:
            metrics["total_works"] = len(filtered_works)
            metrics["total_citations"] = sum(w.cited_by_count for w in filtered_works)
            metrics["has_data"] = True

        # Compute h-index from works if not from author
        if metrics["h_index"] == 0:
            metrics["h_index"] = compute_h_index(filtered_works)
            metrics["i10_index"] = sum(1 for w in filtered_works if w.cited_by_count >= 10)

        # Open access metrics
        oa_works = [w for w in filtered_works if w.is_oa]
        metrics["oa_count"] = len(oa_works)
        metrics["oa_percentage"] = (
            round(len(oa_works) / len(filtered_works) * 100, 1)
            if filtered_works else 0.0
        )

        # Publication year range
        pub_years = [w.publication_year for w in filtered_works if w.publication_year]
        if pub_years:
            metrics["first_pub_year"] = min(pub_years)
            metrics["last_pub_year"] = max(pub_years)
            metrics["career_length_years"] = metrics["last_pub_year"] - metrics["first_pub_year"] + 1

        # Works and citations by year from works
        for work in filtered_works:
            year = work.publication_year
            if year:
                metrics["works_by_year"][year] = metrics["works_by_year"].get(year, 0) + 1

    # Computed metrics
    if metrics["total_works"] > 0:
        metrics["citations_per_work"] = round(
            metrics["total_citations"] / metrics["total_works"], 2
        )

    if metrics["career_length_years"] > 0:
        metrics["works_per_year"] = round(
            metrics["total_works"] / metrics["career_length_years"], 2
        )

    return metrics


def compute_h_index(works: List[Work]) -> int:
    """Compute h-index from list of works"""
    if not works:
        return 0

    citations = sorted([w.cited_by_count for w in works], reverse=True)
    h = 0
    for i, c in enumerate(citations):
        if c >= i + 1:
            h = i + 1
        else:
            break
    return h


def compute_all_metrics(
    data: List[Dict],
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> pd.DataFrame:
    """
    Compute metrics for all people in the dataset.
    Returns DataFrame with one row per person.
    """
    rows = []

    for person_data in data:
        person = person_data.get("person", {})
        author = person_data.get("openalex_author")
        works = person_data.get("works", [])

        metrics = compute_person_metrics(
            author, works, person_data,
            from_year=from_year, to_year=to_year
        )

        row = {
            "name": person.get("name"),
            "email": person.get("email"),
            "rank": person.get("rank"),
            "department": person.get("department"),
            "openalex_id": author.id if author else person.get("openalex_id"),
            "scopus_id": person.get("scopus_id"),
            "scholar_id": person.get("scholar_id"),
            "resolution_status": person_data.get("resolution_status"),
            **{k: v for k, v in metrics.items() if k not in ["works_by_year", "citations_by_year"]},
        }

        rows.append(row)

    return pd.DataFrame(rows)


def create_yearly_timeseries(
    data: List[Dict],
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> pd.DataFrame:
    """
    Create yearly time series data for all people.
    Returns DataFrame with year, person, works, citations.
    """
    if from_year is None:
        from_year = 1990
    if to_year is None:
        to_year = datetime.now().year

    rows = []

    for person_data in data:
        person = person_data.get("person", {})
        name = person.get("name", "Unknown")
        author = person_data.get("openalex_author")
        works = person_data.get("works", [])

        # Get yearly counts from author profile
        works_by_year = {}
        citations_by_year = {}

        if author and author.counts_by_year:
            for year_data in author.counts_by_year:
                year = year_data.get("year")
                if year and from_year <= year <= to_year:
                    works_by_year[year] = year_data.get("works_count", 0)
                    citations_by_year[year] = year_data.get("cited_by_count", 0)

        # Supplement with works data
        for work in works:
            year = work.publication_year
            if year and from_year <= year <= to_year:
                if year not in works_by_year:
                    works_by_year[year] = works_by_year.get(year, 0) + 1

        # Create rows for each year
        for year in range(from_year, to_year + 1):
            rows.append({
                "name": name,
                "year": year,
                "works": works_by_year.get(year, 0),
                "citations": citations_by_year.get(year, 0),
                "rank": person.get("rank"),
            })

    return pd.DataFrame(rows)


def compute_division_summary(
    data: List[Dict],
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> Dict[str, Any]:
    """
    Compute aggregate metrics for the entire division.
    """
    metrics_df = compute_all_metrics(data, from_year, to_year)
    resolved = metrics_df[metrics_df["has_data"] == True]

    summary = {
        "total_faculty": len(metrics_df),
        "resolved_faculty": len(resolved),
        "resolution_rate": round(len(resolved) / len(metrics_df) * 100, 1) if len(metrics_df) > 0 else 0,

        # Aggregate metrics
        "total_works": resolved["total_works"].sum(),
        "total_citations": resolved["total_citations"].sum(),

        # Averages
        "mean_works": round(resolved["total_works"].mean(), 1) if len(resolved) > 0 else 0,
        "mean_citations": round(resolved["total_citations"].mean(), 1) if len(resolved) > 0 else 0,
        "mean_h_index": round(resolved["h_index"].mean(), 1) if len(resolved) > 0 else 0,
        "mean_i10_index": round(resolved["i10_index"].mean(), 1) if len(resolved) > 0 else 0,

        # Medians
        "median_works": resolved["total_works"].median() if len(resolved) > 0 else 0,
        "median_citations": resolved["total_citations"].median() if len(resolved) > 0 else 0,
        "median_h_index": resolved["h_index"].median() if len(resolved) > 0 else 0,
        "median_i10_index": resolved["i10_index"].median() if len(resolved) > 0 else 0,

        # Open access
        "mean_oa_percentage": round(resolved["oa_percentage"].mean(), 1) if len(resolved) > 0 else 0,

        # By rank breakdown
        "by_rank": {},
    }

    # Compute by-rank summary
    if "rank" in resolved.columns:
        for rank in resolved["rank"].dropna().unique():
            rank_data = resolved[resolved["rank"] == rank]
            summary["by_rank"][rank] = {
                "count": len(rank_data),
                "mean_works": round(rank_data["total_works"].mean(), 1),
                "mean_citations": round(rank_data["total_citations"].mean(), 1),
                "mean_h_index": round(rank_data["h_index"].mean(), 1),
            }

    return summary


def get_top_works(
    data: List[Dict],
    n: int = 10,
    sort_by: str = "cited_by_count",
) -> List[Dict]:
    """
    Get top N works across all faculty by citations or other metric.
    """
    all_works = []

    for person_data in data:
        person = person_data.get("person", {})
        works = person_data.get("works", [])

        for work in works:
            all_works.append({
                "title": work.title,
                "year": work.publication_year,
                "citations": work.cited_by_count,
                "is_oa": work.is_oa,
                "venue": work.venue,
                "doi": work.doi,
                "author_name": person.get("name"),
                "work_id": work.id,
            })

    # Sort and return top N
    all_works.sort(key=lambda x: x.get(sort_by.replace("cited_by_count", "citations"), 0), reverse=True)
    return all_works[:n]


def get_top_people(
    data: List[Dict],
    n: int = 10,
    metric: str = "total_citations",
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> pd.DataFrame:
    """
    Get top N faculty by specified metric.
    """
    metrics_df = compute_all_metrics(data, from_year, to_year)
    resolved = metrics_df[metrics_df["has_data"] == True]

    if metric not in resolved.columns:
        metric = "total_citations"

    return resolved.nlargest(n, metric)[
        ["name", "rank", metric, "total_works", "total_citations", "h_index"]
    ]


def compute_recent_metrics(
    data: List[Dict],
    recent_years: int = 5,
) -> pd.DataFrame:
    """
    Compute metrics for recent years only.
    """
    current_year = datetime.now().year
    from_year = current_year - recent_years + 1

    return compute_all_metrics(data, from_year=from_year, to_year=current_year)


def compute_oa_metrics(data: List[Dict]) -> Dict[str, Any]:
    """
    Compute detailed open access metrics.
    """
    oa_stats = {
        "total_works": 0,
        "oa_works": 0,
        "oa_percentage": 0.0,
        "by_status": defaultdict(int),
        "by_year": defaultdict(lambda: {"total": 0, "oa": 0}),
    }

    for person_data in data:
        works = person_data.get("works", [])

        for work in works:
            oa_stats["total_works"] += 1
            year = work.publication_year

            if year:
                oa_stats["by_year"][year]["total"] += 1

            if work.is_oa:
                oa_stats["oa_works"] += 1
                if year:
                    oa_stats["by_year"][year]["oa"] += 1
                if work.oa_status:
                    oa_stats["by_status"][work.oa_status] += 1

    if oa_stats["total_works"] > 0:
        oa_stats["oa_percentage"] = round(
            oa_stats["oa_works"] / oa_stats["total_works"] * 100, 1
        )

    # Convert defaultdicts to regular dicts
    oa_stats["by_status"] = dict(oa_stats["by_status"])
    oa_stats["by_year"] = dict(oa_stats["by_year"])

    return oa_stats


def compute_productivity_trends(
    data: List[Dict],
    window_years: int = 3,
) -> pd.DataFrame:
    """
    Analyze productivity trends over time with moving averages.
    """
    yearly = create_yearly_timeseries(data)

    if yearly.empty:
        return pd.DataFrame()

    # Aggregate by year
    by_year = yearly.groupby("year").agg({
        "works": "sum",
        "citations": "sum",
    }).reset_index()

    # Add moving averages
    by_year["works_ma"] = by_year["works"].rolling(window=window_years, min_periods=1).mean()
    by_year["citations_ma"] = by_year["citations"].rolling(window=window_years, min_periods=1).mean()

    # Calculate year-over-year growth
    by_year["works_growth"] = by_year["works"].pct_change() * 100
    by_year["citations_growth"] = by_year["citations"].pct_change() * 100

    return by_year
