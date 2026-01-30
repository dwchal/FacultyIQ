"""Faculty rank prediction utilities for FacultyIQ"""

from typing import Dict, List, Optional, Any, Tuple
from collections import defaultdict
import logging

import pandas as pd
import numpy as np
from scipy import stats

from .metrics import compute_all_metrics, compute_person_metrics

logger = logging.getLogger(__name__)

# Default academic rank ordering
RANK_ORDER = [
    "Instructor",
    "Assistant Professor",
    "Associate Professor",
    "Full Professor",
    "Professor Emeritus",
]


def compute_rank_benchmarks(
    data: List[Dict],
    from_year: Optional[int] = None,
    to_year: Optional[int] = None,
) -> pd.DataFrame:
    """
    Compute benchmark statistics for each academic rank.
    Returns DataFrame with percentiles and statistics per rank.
    """
    metrics_df = compute_all_metrics(data, from_year, to_year)
    resolved = metrics_df[metrics_df["has_data"] == True].copy()

    if resolved.empty or "rank" not in resolved.columns:
        return pd.DataFrame()

    # Metrics to benchmark
    benchmark_metrics = [
        "total_works",
        "total_citations",
        "h_index",
        "i10_index",
        "citations_per_work",
        "works_per_year",
        "oa_percentage",
    ]

    benchmarks = []

    for rank in resolved["rank"].dropna().unique():
        rank_data = resolved[resolved["rank"] == rank]

        if len(rank_data) < 2:
            continue

        row = {
            "rank": rank,
            "count": len(rank_data),
        }

        for metric in benchmark_metrics:
            if metric in rank_data.columns:
                values = rank_data[metric].dropna()
                if len(values) > 0:
                    row[f"{metric}_mean"] = round(values.mean(), 2)
                    row[f"{metric}_median"] = round(values.median(), 2)
                    row[f"{metric}_std"] = round(values.std(), 2)
                    row[f"{metric}_min"] = round(values.min(), 2)
                    row[f"{metric}_max"] = round(values.max(), 2)
                    row[f"{metric}_p25"] = round(values.quantile(0.25), 2)
                    row[f"{metric}_p75"] = round(values.quantile(0.75), 2)

        benchmarks.append(row)

    benchmarks_df = pd.DataFrame(benchmarks)

    # Sort by rank order
    if not benchmarks_df.empty:
        rank_order_map = {rank: i for i, rank in enumerate(RANK_ORDER)}
        benchmarks_df["rank_order"] = benchmarks_df["rank"].map(
            lambda x: rank_order_map.get(x, 999)
        )
        benchmarks_df = benchmarks_df.sort_values("rank_order").drop("rank_order", axis=1)

    return benchmarks_df


def extract_prediction_features(
    person_metrics: Dict[str, Any],
    feature_names: Optional[List[str]] = None,
) -> Dict[str, float]:
    """
    Extract normalized features for rank prediction.
    """
    if feature_names is None:
        feature_names = [
            "total_works",
            "total_citations",
            "h_index",
            "i10_index",
            "citations_per_work",
            "works_per_year",
            "career_length_years",
        ]

    features = {}
    for name in feature_names:
        value = person_metrics.get(name, 0)
        features[name] = float(value) if value is not None else 0.0

    return features


def normalize_features(
    features: Dict[str, float],
    benchmarks: pd.DataFrame,
    reference_rank: Optional[str] = None,
) -> Dict[str, float]:
    """
    Normalize features using min-max scaling based on benchmarks.
    """
    if benchmarks.empty:
        return features

    normalized = {}

    for metric, value in features.items():
        min_col = f"{metric}_min"
        max_col = f"{metric}_max"

        if min_col in benchmarks.columns and max_col in benchmarks.columns:
            min_val = benchmarks[min_col].min()
            max_val = benchmarks[max_col].max()

            if max_val > min_val:
                normalized[metric] = (value - min_val) / (max_val - min_val)
            else:
                normalized[metric] = 0.5
        else:
            normalized[metric] = value

    return normalized


def compute_rank_scores(
    features: Dict[str, float],
    benchmarks: pd.DataFrame,
) -> Dict[str, float]:
    """
    Compute similarity scores for each rank based on features.
    Returns dict of rank -> similarity score.
    """
    if benchmarks.empty:
        return {}

    scores = {}

    for _, row in benchmarks.iterrows():
        rank = row["rank"]
        score = 0.0
        count = 0

        for metric, value in features.items():
            mean_col = f"{metric}_mean"
            std_col = f"{metric}_std"

            if mean_col in row and std_col in row:
                mean_val = row[mean_col]
                std_val = row[std_col]

                if pd.notna(mean_val) and pd.notna(std_val) and std_val > 0:
                    # Compute z-score and convert to similarity
                    z = abs(value - mean_val) / std_val
                    similarity = np.exp(-0.5 * z)  # Gaussian similarity
                    score += similarity
                    count += 1

        if count > 0:
            scores[rank] = score / count

    return scores


def predict_rank(
    person_data: Dict,
    benchmarks: pd.DataFrame,
    min_works: int = 3,
) -> Dict[str, Any]:
    """
    Predict appropriate academic rank for a person based on their metrics.
    Uses similarity to rank benchmarks.
    """
    author = person_data.get("openalex_author")
    works = person_data.get("works", [])
    person = person_data.get("person", {})
    current_rank = person.get("rank")

    # Compute person's metrics
    metrics = compute_person_metrics(author, works)

    if metrics["total_works"] < min_works or not metrics["has_data"]:
        return {
            "predicted_rank": None,
            "confidence": 0.0,
            "current_rank": current_rank,
            "rank_scores": {},
            "message": "Insufficient data for prediction",
        }

    # Extract features
    features = extract_prediction_features(metrics)

    # Compute scores for each rank
    rank_scores = compute_rank_scores(features, benchmarks)

    if not rank_scores:
        return {
            "predicted_rank": None,
            "confidence": 0.0,
            "current_rank": current_rank,
            "rank_scores": {},
            "message": "No benchmark data available",
        }

    # Find best matching rank
    predicted_rank = max(rank_scores, key=rank_scores.get)
    max_score = rank_scores[predicted_rank]

    # Calculate confidence
    scores_list = list(rank_scores.values())
    if len(scores_list) > 1:
        scores_list.sort(reverse=True)
        confidence = (scores_list[0] - scores_list[1]) / scores_list[0] if scores_list[0] > 0 else 0
    else:
        confidence = max_score

    # Determine if promotion candidate
    is_promotion_candidate = False
    if current_rank and predicted_rank:
        current_idx = RANK_ORDER.index(current_rank) if current_rank in RANK_ORDER else -1
        predicted_idx = RANK_ORDER.index(predicted_rank) if predicted_rank in RANK_ORDER else -1
        is_promotion_candidate = predicted_idx > current_idx

    return {
        "predicted_rank": predicted_rank,
        "confidence": round(confidence, 3),
        "current_rank": current_rank,
        "rank_scores": {k: round(v, 3) for k, v in rank_scores.items()},
        "metrics": metrics,
        "is_promotion_candidate": is_promotion_candidate,
        "message": None,
    }


def identify_promotion_candidates(
    data: List[Dict],
    benchmarks: pd.DataFrame,
    min_works: int = 3,
    confidence_threshold: float = 0.3,
) -> List[Dict]:
    """
    Identify faculty who may be candidates for promotion.
    Returns list of candidates with prediction details.
    """
    candidates = []

    for person_data in data:
        prediction = predict_rank(person_data, benchmarks, min_works)

        if prediction["is_promotion_candidate"] and prediction["confidence"] >= confidence_threshold:
            person = person_data.get("person", {})
            candidates.append({
                "name": person.get("name"),
                "current_rank": prediction["current_rank"],
                "predicted_rank": prediction["predicted_rank"],
                "confidence": prediction["confidence"],
                "total_works": prediction["metrics"]["total_works"],
                "h_index": prediction["metrics"]["h_index"],
                "total_citations": prediction["metrics"]["total_citations"],
            })

    # Sort by confidence
    candidates.sort(key=lambda x: x["confidence"], reverse=True)

    return candidates


def compare_to_peers(
    person_data: Dict,
    peer_data: List[Dict],
    metrics_to_compare: Optional[List[str]] = None,
) -> Dict[str, Any]:
    """
    Compare a person's metrics to their peers (same rank).
    """
    if metrics_to_compare is None:
        metrics_to_compare = ["total_works", "total_citations", "h_index", "i10_index"]

    person = person_data.get("person", {})
    person_rank = person.get("rank")

    if not person_rank:
        return {"error": "Person has no rank assigned"}

    # Filter to same rank
    peers = [p for p in peer_data if p.get("person", {}).get("rank") == person_rank]

    if not peers:
        return {"error": f"No peers found with rank {person_rank}"}

    # Get person's metrics
    author = person_data.get("openalex_author")
    works = person_data.get("works", [])
    person_metrics = compute_person_metrics(author, works)

    # Get peer metrics
    comparisons = {}

    for metric in metrics_to_compare:
        person_value = person_metrics.get(metric, 0)
        peer_values = []

        for peer in peers:
            peer_author = peer.get("openalex_author")
            peer_works = peer.get("works", [])
            peer_metrics = compute_person_metrics(peer_author, peer_works)
            peer_values.append(peer_metrics.get(metric, 0))

        if peer_values:
            comparisons[metric] = {
                "person_value": person_value,
                "peer_mean": round(np.mean(peer_values), 2),
                "peer_median": round(np.median(peer_values), 2),
                "peer_min": round(min(peer_values), 2),
                "peer_max": round(max(peer_values), 2),
                "percentile": round(stats.percentileofscore(peer_values, person_value), 1),
                "peer_count": len(peer_values),
            }

    return {
        "name": person.get("name"),
        "rank": person_rank,
        "peer_count": len(peers),
        "comparisons": comparisons,
    }
