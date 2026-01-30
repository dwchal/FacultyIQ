"""
Export Page

Export roster, metrics, and publication data in various formats.
"""

import streamlit as st
import pandas as pd
import io
from datetime import datetime
from typing import Dict, List, Optional

from utils.metrics import compute_all_metrics, create_yearly_timeseries


def render_export_page():
    """Render the export page"""
    st.title("📁 Export Data")

    if st.session_state.resolved_data is None:
        st.warning("Please resolve faculty identities first.")
        return

    data = st.session_state.resolved_data

    # Filter to resolved
    resolved_data = [d for d in data if d.get("resolution_status") in ["resolved", "auto_resolved"]]

    if not resolved_data:
        st.warning("No resolved faculty data available to export.")
        return

    st.markdown("""
    Export your faculty data, computed metrics, and publication lists for use in
    other applications or for record keeping.
    """)

    st.divider()

    # Export sections
    col1, col2 = st.columns(2)

    with col1:
        render_roster_export(data)
        st.divider()
        render_metrics_export(resolved_data)

    with col2:
        render_publications_export(resolved_data)
        st.divider()
        render_timeseries_export(resolved_data)


def render_roster_export(data: List[Dict]):
    """Render roster export section"""
    st.subheader("📋 Export Roster")

    st.markdown("""
    Export the roster with resolved OpenAlex IDs. This file can be re-imported
    to skip the resolution step in future sessions.
    """)

    # Build roster DataFrame
    roster_data = []
    for d in data:
        person = d.get("person", {})
        author = d.get("openalex_author")

        row = {
            "name": person.get("name"),
            "email": person.get("email"),
            "rank": person.get("rank"),
            "department": person.get("department"),
            "scopus_id": person.get("scopus_id"),
            "scholar_id": person.get("scholar_id"),
            "orcid": person.get("orcid"),
            "openalex_id": author.id if author else person.get("openalex_id"),
            "resolution_status": d.get("resolution_status"),
        }
        roster_data.append(row)

    roster_df = pd.DataFrame(roster_data)

    # Preview
    with st.expander("Preview Roster"):
        st.dataframe(roster_df.head(10), use_container_width=True)

    # Download button
    csv = roster_df.to_csv(index=False)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    st.download_button(
        label="📥 Download Roster (CSV)",
        data=csv,
        file_name=f"faculty_roster_{timestamp}.csv",
        mime="text/csv",
    )


def render_metrics_export(data: List[Dict]):
    """Render metrics export section"""
    st.subheader("📊 Export Metrics")

    st.markdown("""
    Export computed metrics for all faculty members. Choose between wide format
    (one row per person) or long format (one row per person per metric).
    """)

    # Format selection
    export_format = st.radio(
        "Export format",
        options=["Wide (one row per person)", "Long (one row per metric)"],
        key="metrics_format",
        horizontal=True,
    )

    # Compute metrics
    metrics_df = compute_all_metrics(data)

    if export_format.startswith("Wide"):
        export_df = metrics_df
    else:
        # Convert to long format
        id_vars = ["name", "email", "rank", "department", "openalex_id", "scopus_id", "scholar_id"]
        value_vars = [
            "total_works", "total_citations", "h_index", "i10_index",
            "citations_per_work", "works_per_year", "oa_percentage",
            "first_pub_year", "last_pub_year", "career_length_years",
        ]

        # Filter to columns that exist
        id_vars = [c for c in id_vars if c in metrics_df.columns]
        value_vars = [c for c in value_vars if c in metrics_df.columns]

        export_df = metrics_df.melt(
            id_vars=id_vars,
            value_vars=value_vars,
            var_name="metric",
            value_name="value",
        )

    # Preview
    with st.expander("Preview Metrics"):
        st.dataframe(export_df.head(20), use_container_width=True)

    # Download button
    csv = export_df.to_csv(index=False)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    format_suffix = "wide" if export_format.startswith("Wide") else "long"

    st.download_button(
        label="📥 Download Metrics (CSV)",
        data=csv,
        file_name=f"faculty_metrics_{format_suffix}_{timestamp}.csv",
        mime="text/csv",
    )


def render_publications_export(data: List[Dict]):
    """Render publications export section"""
    st.subheader("📚 Export Publications")

    st.markdown("""
    Export the full list of publications for all faculty members.
    """)

    # Collect all publications
    all_works = []
    for d in data:
        person = d.get("person", {})
        name = person.get("name", "Unknown")
        works = d.get("works", [])

        for work in works:
            all_works.append({
                "faculty_name": name,
                "faculty_rank": person.get("rank"),
                "work_id": work.id,
                "title": work.title,
                "publication_year": work.publication_year,
                "publication_date": work.publication_date,
                "doi": work.doi,
                "cited_by_count": work.cited_by_count,
                "is_oa": work.is_oa,
                "oa_status": work.oa_status,
                "type": work.type,
                "venue": work.venue,
            })

    if not all_works:
        st.info("No publications available to export")
        return

    works_df = pd.DataFrame(all_works)

    st.write(f"**{len(works_df):,} publications** from {len(data)} faculty members")

    # Preview
    with st.expander("Preview Publications"):
        st.dataframe(works_df.head(20), use_container_width=True)

    # Download button
    csv = works_df.to_csv(index=False)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    st.download_button(
        label="📥 Download Publications (CSV)",
        data=csv,
        file_name=f"faculty_publications_{timestamp}.csv",
        mime="text/csv",
    )


def render_timeseries_export(data: List[Dict]):
    """Render time series export section"""
    st.subheader("📈 Export Time Series")

    st.markdown("""
    Export yearly productivity data for all faculty members.
    """)

    # Year range
    col1, col2 = st.columns(2)

    with col1:
        from_year = st.number_input(
            "From Year",
            min_value=1990,
            max_value=2025,
            value=2000,
            key="export_from_year",
        )

    with col2:
        to_year = st.number_input(
            "To Year",
            min_value=1990,
            max_value=2030,
            value=2025,
            key="export_to_year",
        )

    # Create time series
    timeseries = create_yearly_timeseries(data, from_year=from_year, to_year=to_year)

    if timeseries.empty:
        st.info("No time series data available")
        return

    st.write(f"**{len(timeseries):,} data points** across {timeseries['name'].nunique()} faculty members")

    # Preview
    with st.expander("Preview Time Series"):
        st.dataframe(timeseries.head(20), use_container_width=True)

    # Download button
    csv = timeseries.to_csv(index=False)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    st.download_button(
        label="📥 Download Time Series (CSV)",
        data=csv,
        file_name=f"faculty_timeseries_{timestamp}.csv",
        mime="text/csv",
    )


def render_excel_export(data: List[Dict]):
    """Render combined Excel export with multiple sheets"""
    st.subheader("📊 Export Excel Workbook")

    st.markdown("""
    Export all data in a single Excel workbook with multiple sheets.
    """)

    # Create Excel buffer
    buffer = io.BytesIO()

    # Compute all data
    metrics_df = compute_all_metrics(data)
    timeseries = create_yearly_timeseries(data)

    # Publications
    all_works = []
    for d in data:
        person = d.get("person", {})
        name = person.get("name", "Unknown")
        works = d.get("works", [])

        for work in works:
            all_works.append({
                "faculty_name": name,
                "title": work.title,
                "publication_year": work.publication_year,
                "doi": work.doi,
                "cited_by_count": work.cited_by_count,
                "is_oa": work.is_oa,
            })

    works_df = pd.DataFrame(all_works) if all_works else pd.DataFrame()

    # Roster
    roster_data = []
    for d in data:
        person = d.get("person", {})
        author = d.get("openalex_author")
        roster_data.append({
            "name": person.get("name"),
            "rank": person.get("rank"),
            "openalex_id": author.id if author else "",
            "resolution_status": d.get("resolution_status"),
        })
    roster_df = pd.DataFrame(roster_data)

    # Write to Excel
    with pd.ExcelWriter(buffer, engine="openpyxl") as writer:
        roster_df.to_excel(writer, sheet_name="Roster", index=False)
        metrics_df.to_excel(writer, sheet_name="Metrics", index=False)
        if not timeseries.empty:
            timeseries.to_excel(writer, sheet_name="Time Series", index=False)
        if not works_df.empty:
            works_df.to_excel(writer, sheet_name="Publications", index=False)

    buffer.seek(0)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    st.download_button(
        label="📥 Download Excel Workbook",
        data=buffer,
        file_name=f"faculty_data_{timestamp}.xlsx",
        mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    )
