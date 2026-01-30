"""
Faculty Profiles Page

Individual faculty member profiles with detailed metrics,
publication lists, and career trajectory visualizations.
"""

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from typing import Dict, List, Optional

from utils.metrics import compute_person_metrics
from utils.comparison import extract_career_trajectory, analyze_career_phases


def render_profiles_page():
    """Render the faculty profiles page"""
    st.title("👤 Faculty Profiles")

    if st.session_state.resolved_data is None:
        st.warning("Please resolve faculty identities first.")
        return

    data = st.session_state.resolved_data

    # Filter to resolved
    resolved_data = [d for d in data if d.get("resolution_status") in ["resolved", "auto_resolved"]]

    if not resolved_data:
        st.warning("No resolved faculty data available.")
        return

    # Faculty selector
    faculty_names = [d.get("person", {}).get("name", "Unknown") for d in resolved_data]
    selected_name = st.selectbox(
        "Select Faculty Member",
        options=faculty_names,
        key="profile_selected_faculty",
    )

    if not selected_name:
        return

    # Find selected data
    selected_data = next(
        (d for d in resolved_data if d.get("person", {}).get("name") == selected_name),
        None,
    )

    if selected_data is None:
        st.error("Could not find selected faculty member")
        return

    st.divider()

    # Render profile
    render_faculty_profile(selected_data)


def render_faculty_profile(person_data: Dict):
    """Render complete profile for a faculty member"""
    person = person_data.get("person", {})
    author = person_data.get("openalex_author")
    works = person_data.get("works", [])

    # Compute metrics
    metrics = compute_person_metrics(author, works)

    # Header with info
    col1, col2 = st.columns([1, 3])

    with col1:
        render_profile_sidebar(person, author, metrics)

    with col2:
        render_profile_main(person, author, works, metrics)


def render_profile_sidebar(person: Dict, author, metrics: Dict):
    """Render profile sidebar with basic info"""
    st.subheader(person.get("name", "Unknown"))

    # Academic info
    if person.get("rank"):
        st.write(f"**Rank:** {person['rank']}")

    if person.get("department"):
        st.write(f"**Department:** {person['department']}")

    if person.get("email"):
        st.write(f"**Email:** {person['email']}")

    st.divider()

    # External IDs
    st.write("**External IDs:**")

    if author:
        st.write(f"[OpenAlex](https://openalex.org/{author.id})")

    if person.get("scopus_id"):
        scopus_url = f"https://www.scopus.com/authid/detail.uri?authorId={person['scopus_id']}"
        st.write(f"[Scopus]({scopus_url})")

    if person.get("scholar_id"):
        scholar_url = f"https://scholar.google.com/citations?user={person['scholar_id']}"
        st.write(f"[Google Scholar]({scholar_url})")

    if person.get("orcid"):
        orcid_url = f"https://orcid.org/{person['orcid']}"
        st.write(f"[ORCID]({orcid_url})")

    st.divider()

    # Institution
    if author and author.last_known_institutions:
        st.write("**Institution:**")
        for inst in author.last_known_institutions[:2]:
            st.write(f"- {inst.get('display_name', 'Unknown')}")


def render_profile_main(person: Dict, author, works: List, metrics: Dict):
    """Render main profile content"""
    # Metrics cards
    render_metrics_cards(metrics)

    st.divider()

    # Tabs for different sections
    tab1, tab2, tab3 = st.tabs(["📊 Career Trajectory", "📚 Publications", "🔬 Analysis"])

    with tab1:
        render_career_trajectory(person, author, works, metrics)

    with tab2:
        render_publication_list(works)

    with tab3:
        render_profile_analysis(person, author, works, metrics)


def render_metrics_cards(metrics: Dict):
    """Render metrics as cards"""
    col1, col2, col3, col4, col5 = st.columns(5)

    with col1:
        st.metric("Total Works", f"{metrics['total_works']:,}")

    with col2:
        st.metric("Total Citations", f"{metrics['total_citations']:,}")

    with col3:
        st.metric("h-index", metrics["h_index"])

    with col4:
        st.metric("i10-index", metrics["i10_index"])

    with col5:
        st.metric("Citations/Work", f"{metrics['citations_per_work']:.1f}")

    # Second row
    col1, col2, col3, col4, col5 = st.columns(5)

    with col1:
        st.metric("Works/Year", f"{metrics['works_per_year']:.1f}")

    with col2:
        st.metric("Open Access", f"{metrics['oa_percentage']:.0f}%")

    with col3:
        st.metric("First Pub", metrics.get("first_pub_year", "N/A"))

    with col4:
        st.metric("Last Pub", metrics.get("last_pub_year", "N/A"))

    with col5:
        st.metric("Career Years", metrics.get("career_length_years", 0))


def render_career_trajectory(person: Dict, author, works: List, metrics: Dict):
    """Render career trajectory visualizations"""
    st.subheader("Career Trajectory")

    # Build person_data dict for trajectory extraction
    person_data = {
        "person": person,
        "openalex_author": author,
        "works": works,
    }

    trajectory = extract_career_trajectory(person_data)

    if trajectory.empty:
        st.info("Insufficient data for trajectory analysis")
        return

    # Filter to years with data
    trajectory_filtered = trajectory[trajectory["works"] > 0]

    if trajectory_filtered.empty:
        trajectory_filtered = trajectory.tail(20)

    col1, col2 = st.columns(2)

    with col1:
        # Works per year
        fig = px.bar(
            trajectory_filtered,
            x="year",
            y="works",
            title="Works Published per Year",
            labels={"year": "Year", "works": "Works"},
        )
        fig.update_layout(showlegend=False)
        st.plotly_chart(fig, use_container_width=True)

    with col2:
        # Cumulative works
        fig = px.line(
            trajectory_filtered,
            x="year",
            y="cumulative_works",
            title="Cumulative Works Over Time",
            labels={"year": "Year", "cumulative_works": "Total Works"},
            markers=True,
        )
        fig.update_layout(showlegend=False)
        st.plotly_chart(fig, use_container_width=True)

    # Citations trajectory
    if "citations" in trajectory.columns:
        citations_filtered = trajectory[trajectory["citations"] > 0]

        if not citations_filtered.empty:
            col1, col2 = st.columns(2)

            with col1:
                fig = px.bar(
                    citations_filtered,
                    x="year",
                    y="citations",
                    title="Citations Received per Year",
                    labels={"year": "Year", "citations": "Citations"},
                    color_discrete_sequence=["#27ae60"],
                )
                fig.update_layout(showlegend=False)
                st.plotly_chart(fig, use_container_width=True)

            with col2:
                fig = px.line(
                    citations_filtered,
                    x="year",
                    y="cumulative_citations",
                    title="Cumulative Citations Over Time",
                    labels={"year": "Year", "cumulative_citations": "Total Citations"},
                    markers=True,
                    color_discrete_sequence=["#27ae60"],
                )
                fig.update_layout(showlegend=False)
                st.plotly_chart(fig, use_container_width=True)


def render_publication_list(works: List):
    """Render list of publications"""
    st.subheader(f"Publications ({len(works)} total)")

    if not works:
        st.info("No publications available")
        return

    # Sort options
    sort_option = st.selectbox(
        "Sort by",
        options=["Year (newest)", "Year (oldest)", "Citations (highest)", "Citations (lowest)"],
        key="pub_sort",
    )

    # Sort works
    sorted_works = works.copy()

    if sort_option == "Year (newest)":
        sorted_works.sort(key=lambda x: x.publication_year or 0, reverse=True)
    elif sort_option == "Year (oldest)":
        sorted_works.sort(key=lambda x: x.publication_year or 9999)
    elif sort_option == "Citations (highest)":
        sorted_works.sort(key=lambda x: x.cited_by_count, reverse=True)
    elif sort_option == "Citations (lowest)":
        sorted_works.sort(key=lambda x: x.cited_by_count)

    # Pagination
    page_size = 20
    total_pages = (len(sorted_works) - 1) // page_size + 1

    page = st.number_input(
        "Page",
        min_value=1,
        max_value=total_pages,
        value=1,
        key="pub_page",
    )

    start_idx = (page - 1) * page_size
    end_idx = start_idx + page_size

    page_works = sorted_works[start_idx:end_idx]

    # Display works
    for i, work in enumerate(page_works, start=start_idx + 1):
        render_work_card(work, i)


def render_work_card(work, index: int):
    """Render a single publication card"""
    with st.container():
        col1, col2 = st.columns([5, 1])

        with col1:
            title = work.title or "Untitled"
            st.write(f"**{index}. {title}**")

            meta_parts = []
            if work.publication_year:
                meta_parts.append(str(work.publication_year))
            if work.venue:
                meta_parts.append(work.venue)
            if work.type:
                meta_parts.append(work.type)
            if work.is_oa:
                meta_parts.append("🔓 Open Access")

            if meta_parts:
                st.caption(" | ".join(meta_parts))

            if work.doi:
                st.write(f"[DOI]({work.doi})")

        with col2:
            st.metric("Citations", work.cited_by_count)

    st.divider()


def render_profile_analysis(person: Dict, author, works: List, metrics: Dict):
    """Render analysis and insights"""
    st.subheader("Career Analysis")

    person_data = {
        "person": person,
        "openalex_author": author,
        "works": works,
    }

    # Career phase analysis
    phases = analyze_career_phases(person_data)

    col1, col2 = st.columns(2)

    with col1:
        st.write("**Current Status:**")
        st.write(f"- Current phase: **{phases.get('current_phase', 'Unknown').title()}**")
        st.write(f"- Trend direction: **{phases.get('trend_direction', 'Unknown').title()}**")
        st.write(f"- Recent avg works/year: **{phases.get('recent_avg_works', 0):.1f}**")
        st.write(f"- Career avg works/year: **{phases.get('career_avg_works', 0):.1f}**")

    with col2:
        st.write("**Productivity Insights:**")

        # Compare to career average
        recent = phases.get("recent_avg_works", 0)
        career = phases.get("career_avg_works", 0)

        if career > 0:
            ratio = recent / career
            if ratio > 1.2:
                st.success(f"Recent productivity is {ratio:.1f}x above career average")
            elif ratio < 0.8:
                st.warning(f"Recent productivity is {ratio:.1f}x below career average")
            else:
                st.info("Recent productivity is consistent with career average")

        # Impact insight
        if metrics["total_works"] > 0:
            impact = metrics["total_citations"] / metrics["total_works"]
            if impact > 20:
                st.success(f"High impact: {impact:.1f} citations per work")
            elif impact > 10:
                st.info(f"Good impact: {impact:.1f} citations per work")

    # Phase timeline
    if phases.get("phases"):
        st.write("**Career Phases:**")

        phase_data = []
        for phase in phases["phases"]:
            phase_data.append({
                "Phase": phase["phase"].title(),
                "Start": phase["start_year"],
                "End": phase["end_year"],
                "Duration": phase["end_year"] - phase["start_year"] + 1,
            })

        phase_df = pd.DataFrame(phase_data)
        st.dataframe(phase_df, use_container_width=True, hide_index=True)

    # Publication type breakdown
    if works:
        st.write("**Publication Types:**")

        type_counts = {}
        for work in works:
            pub_type = work.type or "unknown"
            type_counts[pub_type] = type_counts.get(pub_type, 0) + 1

        type_df = pd.DataFrame([
            {"Type": k.replace("-", " ").title(), "Count": v}
            for k, v in sorted(type_counts.items(), key=lambda x: x[1], reverse=True)
        ])

        fig = px.pie(
            type_df,
            values="Count",
            names="Type",
            title="Publication Type Distribution",
            hole=0.4,
        )
        st.plotly_chart(fig, use_container_width=True)
