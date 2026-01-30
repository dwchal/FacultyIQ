"""Data validation and cleaning utilities for FacultyIQ"""

import re
from typing import Dict, List, Optional, Tuple, Any
from datetime import datetime
import logging

import pandas as pd
import numpy as np

logger = logging.getLogger(__name__)

# Column name mappings for flexible input
COLUMN_MAPPINGS = {
    "name": [
        "name", "full_name", "fullname", "faculty_name", "facultyname",
        "author", "author_name", "authorname", "person", "faculty",
        "display_name", "displayname", "researcher",
    ],
    "email": [
        "email", "e-mail", "email_address", "emailaddress", "mail",
        "contact", "contact_email",
    ],
    "rank": [
        "rank", "academic_rank", "academicrank", "title", "position",
        "faculty_rank", "facultyrank", "job_title", "jobtitle",
    ],
    "department": [
        "department", "dept", "division", "unit", "section",
        "academic_unit", "academicunit",
    ],
    "scopus_id": [
        "scopus_id", "scopusid", "scopus", "scopus_author_id",
        "scopusauthorid", "author_id", "authorid",
    ],
    "scholar_id": [
        "scholar_id", "scholarid", "google_scholar", "googlescholar",
        "google_scholar_id", "googlescholarid", "gs_id", "gsid",
    ],
    "orcid": [
        "orcid", "orcid_id", "orcidid",
    ],
    "openalex_id": [
        "openalex_id", "openalexid", "openalex", "oa_id", "oaid",
    ],
    "reaims_publications": [
        "reaims_publications", "reaimspublications", "reaims",
        "self_reported_pubs", "self_reported", "reported_publications",
    ],
    "last_promotion": [
        "last_promotion", "lastpromotion", "promotion_date",
        "promotiondate", "last_promoted", "date_promoted",
    ],
}

# Academic rank standardization
RANK_MAPPINGS = {
    "instructor": [
        "instructor", "lecturer", "teaching instructor",
        "clinical instructor", "visiting instructor",
    ],
    "assistant professor": [
        "assistant professor", "asst prof", "asst. prof",
        "assistant prof", "asst professor", "ast prof",
        "clinical assistant professor", "research assistant professor",
    ],
    "associate professor": [
        "associate professor", "assoc prof", "assoc. prof",
        "associate prof", "asc prof", "asc professor",
        "clinical associate professor", "research associate professor",
    ],
    "full professor": [
        "full professor", "professor", "prof",
        "clinical professor", "research professor",
        "distinguished professor", "university professor",
    ],
    "research faculty": [
        "research faculty", "research scientist", "researcher",
        "research associate", "senior researcher", "postdoc",
        "postdoctoral", "research fellow", "fellow",
    ],
    "professor emeritus": [
        "professor emeritus", "emeritus", "emerita",
        "emeritus professor", "retired professor",
    ],
}


def normalize_column_names(df: pd.DataFrame) -> Tuple[pd.DataFrame, Dict[str, str]]:
    """
    Normalize column names to standard format.
    Returns the modified dataframe and a mapping of original to standard names.
    """
    mapping_used = {}
    new_columns = {}

    for col in df.columns:
        col_lower = col.lower().strip().replace(" ", "_").replace("-", "_")

        # Check against known mappings
        matched = False
        for standard_name, variations in COLUMN_MAPPINGS.items():
            if col_lower in variations or col.lower().strip() in variations:
                new_columns[col] = standard_name
                mapping_used[standard_name] = col
                matched = True
                break

        if not matched:
            # Keep original name but clean it
            new_columns[col] = col_lower

    df_normalized = df.rename(columns=new_columns)
    return df_normalized, mapping_used


def standardize_rank(rank: Any) -> Optional[str]:
    """Standardize academic rank to common format"""
    if pd.isna(rank) or rank is None:
        return None

    rank_lower = str(rank).lower().strip()

    for standard_rank, variations in RANK_MAPPINGS.items():
        if rank_lower in variations:
            return standard_rank.title()
        # Also check for partial matches
        for var in variations:
            if var in rank_lower or rank_lower in var:
                return standard_rank.title()

    # Return cleaned version if no match
    return str(rank).strip().title()


def clean_scopus_id(scopus_id: Any) -> Optional[str]:
    """Clean and validate Scopus ID"""
    if pd.isna(scopus_id) or scopus_id is None:
        return None

    # Convert to string and clean
    scopus_str = str(scopus_id).strip()

    # Remove common prefixes
    scopus_str = re.sub(r"^(scopus:|author:)", "", scopus_str, flags=re.IGNORECASE)

    # Extract numeric ID
    match = re.search(r"(\d{10,})", scopus_str)
    if match:
        return match.group(1)

    # If it's already a pure number
    if scopus_str.isdigit() and len(scopus_str) >= 10:
        return scopus_str

    return None


def clean_scholar_id(scholar_id: Any) -> Optional[str]:
    """Clean and validate Google Scholar ID"""
    if pd.isna(scholar_id) or scholar_id is None:
        return None

    scholar_str = str(scholar_id).strip()

    # Extract from URL if present
    match = re.search(r"user=([A-Za-z0-9_-]+)", scholar_str)
    if match:
        return match.group(1)

    # Validate format (alphanumeric with - and _)
    if re.match(r"^[A-Za-z0-9_-]+$", scholar_str) and len(scholar_str) >= 8:
        return scholar_str

    return None


def clean_orcid(orcid: Any) -> Optional[str]:
    """Clean and validate ORCID"""
    if pd.isna(orcid) or orcid is None:
        return None

    orcid_str = str(orcid).strip()

    # Extract ORCID pattern
    match = re.search(r"(\d{4}-\d{4}-\d{4}-\d{3}[0-9X])", orcid_str)
    if match:
        return match.group(1)

    return None


def clean_openalex_id(openalex_id: Any) -> Optional[str]:
    """Clean and validate OpenAlex author ID"""
    if pd.isna(openalex_id) or openalex_id is None:
        return None

    oa_str = str(openalex_id).strip()

    # Handle full URL
    if "openalex.org" in oa_str:
        match = re.search(r"(A\d+)", oa_str)
        if match:
            return match.group(1)

    # Handle just the ID
    if re.match(r"^A?\d+$", oa_str):
        if not oa_str.startswith("A"):
            return f"A{oa_str}"
        return oa_str

    return None


def clean_name(name: Any) -> Optional[str]:
    """Clean and normalize a person's name"""
    if pd.isna(name) or name is None:
        return None

    name_str = str(name).strip()

    # Remove extra whitespace
    name_str = re.sub(r"\s+", " ", name_str)

    # Remove titles/suffixes
    name_str = re.sub(
        r"\b(Dr\.?|PhD|Ph\.D\.?|MD|M\.D\.?|Jr\.?|Sr\.?|III?|IV)\b",
        "",
        name_str,
        flags=re.IGNORECASE,
    )

    return name_str.strip() if name_str else None


def clean_roster(df: pd.DataFrame) -> pd.DataFrame:
    """Apply all cleaning functions to roster data"""
    df = df.copy()

    # Clean name
    if "name" in df.columns:
        df["name"] = df["name"].apply(clean_name)

    # Standardize rank
    if "rank" in df.columns:
        df["rank"] = df["rank"].apply(standardize_rank)

    # Clean IDs
    if "scopus_id" in df.columns:
        df["scopus_id"] = df["scopus_id"].apply(clean_scopus_id)

    if "scholar_id" in df.columns:
        df["scholar_id"] = df["scholar_id"].apply(clean_scholar_id)

    if "orcid" in df.columns:
        df["orcid"] = df["orcid"].apply(clean_orcid)

    if "openalex_id" in df.columns:
        df["openalex_id"] = df["openalex_id"].apply(clean_openalex_id)

    # Clean email
    if "email" in df.columns:
        df["email"] = df["email"].apply(
            lambda x: str(x).strip().lower() if pd.notna(x) else None
        )

    return df


def validate_roster(df: pd.DataFrame) -> Dict[str, Any]:
    """
    Validate roster data and return validation results.
    Returns dict with validation status and any issues found.
    """
    issues = []
    warnings = []

    # Check for required columns
    if "name" not in df.columns:
        issues.append("Missing required column: name")

    # Check for empty dataframe
    if len(df) == 0:
        issues.append("Roster is empty")

    # Check for duplicate names
    if "name" in df.columns:
        name_counts = df["name"].value_counts()
        duplicates = name_counts[name_counts > 1]
        if len(duplicates) > 0:
            warnings.append(f"Duplicate names found: {list(duplicates.index)}")

    # Check for missing names
    if "name" in df.columns:
        missing_names = df["name"].isna().sum()
        if missing_names > 0:
            issues.append(f"{missing_names} rows have missing names")

    # Calculate completeness
    completeness = calculate_completeness_summary(df)

    return {
        "is_valid": len(issues) == 0,
        "issues": issues,
        "warnings": warnings,
        "completeness": completeness,
        "row_count": len(df),
    }


def calculate_completeness_summary(df: pd.DataFrame) -> Dict[str, Any]:
    """Calculate data completeness for each column"""
    summary = {}

    for col in df.columns:
        non_null = df[col].notna().sum()
        total = len(df)
        summary[col] = {
            "present": non_null,
            "missing": total - non_null,
            "percentage": round(non_null / total * 100, 1) if total > 0 else 0,
        }

    return summary


def parse_date(date_value: Any) -> Optional[datetime]:
    """Parse various date formats"""
    if pd.isna(date_value) or date_value is None:
        return None

    if isinstance(date_value, datetime):
        return date_value

    date_str = str(date_value).strip()

    # Try common formats
    formats = [
        "%Y-%m-%d",
        "%m/%d/%Y",
        "%d/%m/%Y",
        "%Y/%m/%d",
        "%m-%d-%Y",
        "%d-%m-%Y",
        "%Y",
        "%B %Y",
        "%b %Y",
    ]

    for fmt in formats:
        try:
            return datetime.strptime(date_str, fmt)
        except ValueError:
            continue

    # Try pandas parsing as fallback
    try:
        return pd.to_datetime(date_str).to_pydatetime()
    except Exception:
        return None


def extract_year(date_value: Any) -> Optional[int]:
    """Extract year from various date formats"""
    if pd.isna(date_value) or date_value is None:
        return None

    # Try direct year extraction
    if isinstance(date_value, (int, float)):
        year = int(date_value)
        if 1900 <= year <= 2100:
            return year
        return None

    # Parse as date
    parsed = parse_date(date_value)
    if parsed:
        return parsed.year

    # Try regex extraction
    date_str = str(date_value)
    match = re.search(r"(19|20)\d{2}", date_str)
    if match:
        return int(match.group())

    return None


def roster_to_dict_list(df: pd.DataFrame) -> List[Dict]:
    """Convert roster DataFrame to list of dictionaries"""
    records = df.to_dict(orient="records")

    # Clean None values
    for record in records:
        for key, value in record.items():
            if pd.isna(value):
                record[key] = None

    return records


def dict_list_to_roster(records: List[Dict]) -> pd.DataFrame:
    """Convert list of dictionaries back to DataFrame"""
    return pd.DataFrame(records)
