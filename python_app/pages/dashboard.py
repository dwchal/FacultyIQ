"""
Division Dashboard Page

Aggregate analytics for the entire faculty division with filtering
and interactive visualizations.
"""

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from datetime import datetime
from typing import Dict, List, Optional

from utils.metrics import (
    compute_all_metrics,
    compute_division_summary,
    create_yearly_timeseries,
    get_top_people,
    get_top_works,
    compute_oa_metrics,
    compute_productivity_trends,
)


def render_dashboard_page():
    """Render the division dashboard page"""
    st.title("📈 Division Dashboard")

    if st.session_state.resolved_data is None:
        st.warning("Please resolve faculty identities first.")
        return

    data = st.session_state.resolved_data

    # Check for resolved data
    resolved_data = [d for d in data if d.get("resolution_status") in ["resolved", "auto_resolved"]]

    if not resolved_data:
        st.warning("No resolved faculty data available. Please complete identity resolution first.")
        return

    # Filters
    render_filters()

    # Apply filters
    filtered_data = apply_filters(resolved_data)

    if not filtered_data:
        st.info("No data matches the current filters.")
        return

    st.divider()

    # KPI Cards
    render_kpi_cards(filtered_data)

    st.divider()

    # Time series charts
    col1, col2 = st.columns(2)

    with col1:
        render_works_timeseries(filtered_data)

    with col2:
        render_citations_timeseries(filtered_data)

    st.divider()

    # Rank analysis
    render_rank_analysis(filtered_data)

    st.divider()

    # Top performers
    col1, col2 = st.columns(2)

    with col1:
        render_top_faculty(filtered_data)

    with col2:
        render_top_publications(filtered_data)

    st.divider()

    # Open access analysis
    render_oa_analysis(filtered_data)


def render_filters():
    """Render filter controls"""
    with st.expander("🎛️ Filters", expanded=True):
        col1, col2, col3 = st.columns(3)

        with col1:
            # Year range
            config = st.session_state.config
            default_range = config.get("analysis", {}).get("default_year_range", [2010, 2025])

            year_range = st.slider(
                "Year Range",
                min_value=1990,
                max_value=datetime.now().year,
                value=tuple(default_range),
                key="filter_years",
            )
            st.session_state["filter_year_range"] = year_range

        with col2:
            # Academic rank filter
            data = st.session_state.resolved_data
            all_ranks = set()
            for d in data:
                rank = d.get("person", {}).get("rank")
                if rank:
                    all_ranks.add(rank)

            ranks = list(sorted(all_ranks))
            selected_ranks = st.multiselect(
                "Academic Ranks",
                options=ranks,
                default=ranks,
                key="filter_ranks",
            )
            st.session_state["filter_selected_ranks"] = selected_ranks

        with col3:
            # Minimum works filter
            min_works = st.number_input(
                "Minimum Works",
                min_value=0,
                value=0,
                step=1,
                key="filter_min_works",
            )
            st.session_state["filter_min_works"] = min_works


def apply_filters(data: List[Dict]) -> List[Dict]:
    """Apply current filters to data"""
    filtered = []

    selected_ranks = st.session_state.get("filter_selected_ranks", [])
    min_works = st.session_state.get("filter_min_works", 0)

    for d in data:
        person = d.get("person", {})
        author = d.get("openalex_author")

        # Rank filter
        rank = person.get("rank")
        if selected_ranks and rank not in selected_ranks:
            continue

        # Works filter
        works_count = author.works_count if author else len(d.get("works", []))
        if works_count < min_works:
            continue

        filtered.append(d)

    return filtered


def render_kpi_cards(data: List[Dict]):
    """Render KPI metric cards"""
    year_range = st.session_state.get("filter_year_range", (2010, 2025))

    summary = compute_division_summary(data, from_year=year_range[0], to_year=year_range[1])

    col1, col2, col3, col4, col5 = st.columns(5)

    with col1:
        st.metric(
            "Faculty",
            f"{summary['resolved_faculty']}/{summary['total_faculty']}",
            help="Resolved / Total faculty",
        )

    with col2:
        st.metric(
            "Total Works",
            f"{summary['total_works']:,}",
            help="Total indexed publications",
        )

    with col3:
        st.metric(
            "Total Citations",
            f"{summary['total_citations']:,}",
            help="Total citation count",
        )

    with col4:
        st.metric(
            "Median h-index",
            f"{summary['median_h_index']:.0f}",
            help="Median h-index across faculty",
        )

    with col5:
        st.metric(
            "Open Access",
            f"{summary['mean_oa_percentage']:.1f}%",
            help="Average open access percentage",
        )


def render_works_timeseries(data: List[Dict]):
    """Render works published per year chart"""
    st.subheader("📚 Works Published per Year")

    year_range = st.session_state.get("filter_year_range", (2010, 2025))

    timeseries = create_yearly_timeseries(data, from_year=year_range[0], to_year=year_range[1])

    if timeseries.empty:
        st.info("No time series data available")
        return

    # Aggregate by year
    by_year = timeseries.groupby("year").agg({"works": "sum"}).reset_index()

    fig = px.bar(
        by_year,
        x="year",
        y="works",
        title="Total Works Published",
        labels={"year": "Year", "works": "Works"},
    )

    fig.update_layout(
        xaxis_title="Year",
        yaxis_title="Number of Works",
        showlegend=False,
    )

    st.plotly_chart(fig, use_container_width=True)


def render_citations_timeseries(data: List[Dict]):
    """Render citations received per year chart"""
    st.subheader("📊 Citations per Year")

    year_range = st.session_state.get("filter_year_range", (2010, 2025))

    timeseries = create_yearly_timeseries(data, from_year=year_range[0], to_year=year_range[1])

    if timeseries.empty:
        st.info("No time series data available")
        return

    # Aggregate by year
    by_year = timeseries.groupby("year").agg({"citations": "sum"}).reset_index()

    fig = px.area(
        by_year,
        x="year",
        y="citations",
        title="Total Citations Received",
        labels={"year": "Year", "citations": "Citations"},
    )

    fig.update_layout(
        xaxis_title="Year",
        yaxis_title="Citations",
        showlegend=False,
    )

    st.plotly_chart(fig, use_container_width=True)


def render_rank_analysis(data: List[Dict]):
    """Render productivity by academic rank"""
    st.subheader("🎓 Productivity by Academic Rank")

    year_range = st.session_state.get("filter_year_range", (2010, 2025))

    metrics_df = compute_all_metrics(data, from_year=year_range[0], to_year=year_range[1])
    resolved = metrics_df[metrics_df["has_data"] == True]

    if resolved.empty or "rank" not in resolved.columns:
        st.info("No rank data available")
        return

    # Group by rank
    rank_stats = resolved.groupby("rank").agg({
        "total_works": ["mean", "median", "sum"],
        "total_citations": ["mean", "median", "sum"],
        "h_index": ["mean", "median"],
        "name": "count",
    }).round(1)

    rank_stats.columns = ["_".join(col).strip() for col in rank_stats.columns.values]
    rank_stats = rank_stats.rename(columns={"name_count": "faculty_count"})
    rank_stats = rank_stats.reset_index()

    col1, col2 = st.columns(2)

    with col1:
        # Bar chart of mean works by rank
        fig = px.bar(
            rank_stats,
            x="rank",
            y="total_works_mean",
            color="rank",
            title="Average Works by Rank",
            labels={"total_works_mean": "Average Works", "rank": "Academic Rank"},
        )
        fig.update_layout(showlegend=False)
        st.plotly_chart(fig, use_container_width=True)

    with col2:
        # Bar chart of mean h-index by rank
        fig = px.bar(
            rank_stats,
            x="rank",
            y="h_index_mean",
            color="rank",
            title="Average h-index by Rank",
            labels={"h_index_mean": "Average h-index", "rank": "Academic Rank"},
        )
        fig.update_layout(showlegend=False)
        st.plotly_chart(fig, use_container_width=True)

    # Summary table
    with st.expander("View Detailed Statistics"):
        st.dataframe(rank_stats, use_container_width=True)


def render_top_faculty(data: List[Dict]):
    """Render top faculty by various metrics"""
    st.subheader("🏆 Top Faculty")

    metric_options = {
        "total_citations": "Citations",
        "total_works": "Works",
        "h_index": "h-index",
        "citations_per_work": "Citations/Work",
    }

    selected_metric = st.selectbox(
        "Rank by",
        options=list(metric_options.keys()),
        format_func=lambda x: metric_options[x],
        key="top_faculty_metric",
    )

    year_range = st.session_state.get("filter_year_range", (2010, 2025))

    top_df = get_top_people(
        data,
        n=10,
        metric=selected_metric,
        from_year=year_range[0],
        to_year=year_range[1],
    )

    if top_df.empty:
        st.info("No data available")
        return

    # Add rank column
    top_df.insert(0, "#", range(1, len(top_df) + 1))

    st.dataframe(
        top_df,
        use_container_width=True,
        hide_index=True,
        column_config={
            "#": st.column_config.NumberColumn(width="small"),
            "total_works": st.column_config.NumberColumn("Works"),
            "total_citations": st.column_config.NumberColumn("Citations"),
            "h_index": st.column_config.NumberColumn("h-index"),
        },
    )


def render_top_publications(data: List[Dict]):
    """Render top publications by citations"""
    st.subheader("📄 Top Publications")

    top_works = get_top_works(data, n=10)

    if not top_works:
        st.info("No publication data available")
        return

    for i, work in enumerate(top_works, 1):
        with st.container():
            col1, col2 = st.columns([5, 1])
            with col1:
                title = work.get("title", "Untitled")
                if len(title) > 100:
                    title = title[:100] + "..."
                st.write(f"**{i}. {title}**")
                st.caption(f"{work.get('author_name')} ({work.get('year', 'N/A')})")
            with col2:
                st.metric("Citations", work.get("citations", 0))


def render_oa_analysis(data: List[Dict]):
    """Render open access analysis"""
    st.subheader("🔓 Open Access Analysis")

    oa_stats = compute_oa_metrics(data)

    col1, col2 = st.columns(2)

    with col1:
        # OA percentage pie chart
        labels = ["Open Access", "Closed"]
        values = [oa_stats["oa_works"], oa_stats["total_works"] - oa_stats["oa_works"]]

        fig = go.Figure(data=[go.Pie(
            labels=labels,
            values=values,
            hole=0.4,
            marker_colors=["#2ecc71", "#95a5a6"],
        )])

        fig.update_layout(
            title=f"Open Access: {oa_stats['oa_percentage']}%",
            showlegend=True,
        )

        st.plotly_chart(fig, use_container_width=True)

    with col2:
        # OA status breakdown
        if oa_stats["by_status"]:
            status_df = pd.DataFrame([
                {"Status": k.replace("_", " ").title(), "Count": v}
                for k, v in oa_stats["by_status"].items()
            ])

            fig = px.bar(
                status_df,
                x="Status",
                y="Count",
                title="OA Status Breakdown",
                color="Status",
            )
            fig.update_layout(showlegend=False)
            st.plotly_chart(fig, use_container_width=True)
        else:
            st.info("No OA status data available")

    # OA trend over time
    if oa_stats["by_year"]:
        year_data = []
        for year, counts in sorted(oa_stats["by_year"].items()):
            if counts["total"] > 0:
                pct = round(counts["oa"] / counts["total"] * 100, 1)
                year_data.append({
                    "Year": year,
                    "OA Percentage": pct,
                    "OA Works": counts["oa"],
                    "Total Works": counts["total"],
                })

        if year_data:
            year_df = pd.DataFrame(year_data)

            fig = px.line(
                year_df,
                x="Year",
                y="OA Percentage",
                title="Open Access Trend Over Time",
                markers=True,
            )

            fig.update_layout(
                yaxis_title="Open Access %",
                yaxis_range=[0, 100],
            )

            st.plotly_chart(fig, use_container_width=True)
