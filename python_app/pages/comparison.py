"""
Comparison Page

Side-by-side comparison of faculty members with career trajectories
and ranking analysis.
"""

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from typing import Dict, List, Optional

from utils.comparison import (
    compare_faculty,
    compare_trajectories,
    create_comparison_summary,
    rank_faculty,
    find_similar_faculty,
    compute_collaboration_potential,
)
from utils.metrics import compute_person_metrics


def render_comparison_page():
    """Render the comparison page"""
    st.title("⚖️ Faculty Comparison")

    if st.session_state.resolved_data is None:
        st.warning("Please resolve faculty identities first.")
        return

    data = st.session_state.resolved_data

    # Filter to resolved
    resolved_data = [d for d in data if d.get("resolution_status") in ["resolved", "auto_resolved"]]

    if not resolved_data:
        st.warning("No resolved faculty data available.")
        return

    st.markdown("""
    Compare faculty members side-by-side, analyze career trajectories,
    and explore division rankings.
    """)

    # Tabs for different comparison views
    tab1, tab2, tab3, tab4 = st.tabs([
        "👥 Side-by-Side",
        "📈 Trajectories",
        "🏆 Rankings",
        "🔍 Find Similar",
    ])

    with tab1:
        render_side_by_side(resolved_data)

    with tab2:
        render_trajectories(resolved_data)

    with tab3:
        render_rankings(resolved_data)

    with tab4:
        render_similar_faculty(resolved_data)


def render_side_by_side(data: List[Dict]):
    """Render side-by-side faculty comparison"""
    st.subheader("Side-by-Side Comparison")

    # Multi-select for faculty
    faculty_names = [d.get("person", {}).get("name", "Unknown") for d in data]

    selected_names = st.multiselect(
        "Select faculty to compare (2-5 recommended)",
        options=faculty_names,
        default=faculty_names[:min(3, len(faculty_names))],
        max_selections=5,
        key="comparison_faculty",
    )

    if len(selected_names) < 2:
        st.info("Please select at least 2 faculty members to compare.")
        return

    # Get selected data
    selected_data = [
        d for d in data
        if d.get("person", {}).get("name") in selected_names
    ]

    # Create comparison
    comparison = compare_faculty(selected_data)

    if comparison.empty:
        st.error("Unable to generate comparison")
        return

    st.divider()

    # Display comparison table
    st.write("**Metrics Comparison:**")

    # Transpose for better display
    comparison_t = comparison.set_index("name").T

    # Style with highlights
    st.dataframe(
        comparison_t.style.highlight_max(axis=1, color="#90EE90"),
        use_container_width=True,
    )

    st.divider()

    # Radar chart comparison
    st.write("**Metrics Radar:**")

    metrics_for_radar = ["total_works", "total_citations", "h_index", "i10_index", "works_per_year"]

    # Normalize metrics for radar
    radar_data = []
    for _, row in comparison.iterrows():
        values = []
        for metric in metrics_for_radar:
            if metric in row:
                # Normalize to 0-100 scale based on max in dataset
                max_val = comparison[metric].max()
                if max_val > 0:
                    values.append(row[metric] / max_val * 100)
                else:
                    values.append(0)
            else:
                values.append(0)
        radar_data.append({
            "name": row["name"],
            "values": values,
        })

    fig = go.Figure()

    for person in radar_data:
        fig.add_trace(go.Scatterpolar(
            r=person["values"],
            theta=[m.replace("_", " ").title() for m in metrics_for_radar],
            name=person["name"],
            fill="toself",
        ))

    fig.update_layout(
        polar=dict(radialaxis=dict(visible=True, range=[0, 100])),
        showlegend=True,
        title="Normalized Metrics Comparison",
    )

    st.plotly_chart(fig, use_container_width=True)

    # Insights
    st.divider()
    st.write("**Comparison Insights:**")

    summary = create_comparison_summary(selected_data)

    for insight in summary.get("insights", []):
        st.write(f"• {insight}")


def render_trajectories(data: List[Dict]):
    """Render career trajectory comparisons"""
    st.subheader("Career Trajectories")

    # Multi-select for faculty
    faculty_names = [d.get("person", {}).get("name", "Unknown") for d in data]

    selected_names = st.multiselect(
        "Select faculty to compare trajectories",
        options=faculty_names,
        default=faculty_names[:min(3, len(faculty_names))],
        max_selections=5,
        key="trajectory_faculty",
    )

    if len(selected_names) < 1:
        st.info("Please select at least 1 faculty member.")
        return

    # Get selected data
    selected_data = [
        d for d in data
        if d.get("person", {}).get("name") in selected_names
    ]

    # Get trajectories
    trajectories = compare_trajectories(selected_data)

    if trajectories.empty:
        st.info("No trajectory data available")
        return

    # Filter by year range
    col1, col2 = st.columns(2)

    with col1:
        min_year = st.number_input(
            "From Year",
            min_value=1990,
            max_value=2025,
            value=2000,
            key="traj_from_year",
        )

    with col2:
        max_year = st.number_input(
            "To Year",
            min_value=1990,
            max_value=2030,
            value=2025,
            key="traj_to_year",
        )

    trajectories = trajectories[
        (trajectories["year"] >= min_year) &
        (trajectories["year"] <= max_year)
    ]

    st.divider()

    # Works trajectory
    col1, col2 = st.columns(2)

    with col1:
        fig = px.line(
            trajectories,
            x="year",
            y="works",
            color="name",
            title="Works Published per Year",
            labels={"year": "Year", "works": "Works", "name": "Faculty"},
            markers=True,
        )
        st.plotly_chart(fig, use_container_width=True)

    with col2:
        fig = px.line(
            trajectories,
            x="year",
            y="cumulative_works",
            color="name",
            title="Cumulative Works Over Time",
            labels={"year": "Year", "cumulative_works": "Total Works", "name": "Faculty"},
            markers=True,
        )
        st.plotly_chart(fig, use_container_width=True)

    # Citations trajectory if available
    if "citations" in trajectories.columns:
        col1, col2 = st.columns(2)

        with col1:
            fig = px.line(
                trajectories,
                x="year",
                y="citations",
                color="name",
                title="Citations per Year",
                labels={"year": "Year", "citations": "Citations", "name": "Faculty"},
                markers=True,
            )
            st.plotly_chart(fig, use_container_width=True)

        with col2:
            if "cumulative_citations" in trajectories.columns:
                fig = px.line(
                    trajectories,
                    x="year",
                    y="cumulative_citations",
                    color="name",
                    title="Cumulative Citations Over Time",
                    labels={"year": "Year", "cumulative_citations": "Total Citations", "name": "Faculty"},
                    markers=True,
                )
                st.plotly_chart(fig, use_container_width=True)


def render_rankings(data: List[Dict]):
    """Render faculty rankings by various metrics"""
    st.subheader("Division Rankings")

    # Metric selector
    metric_options = {
        "h_index": "h-index",
        "total_citations": "Total Citations",
        "total_works": "Total Works",
        "citations_per_work": "Citations per Work",
        "works_per_year": "Works per Year",
        "oa_percentage": "Open Access %",
    }

    selected_metric = st.selectbox(
        "Rank by",
        options=list(metric_options.keys()),
        format_func=lambda x: metric_options[x],
        key="ranking_metric",
    )

    # Get rankings
    rankings = rank_faculty(data, metric=selected_metric, ascending=False)

    if rankings.empty:
        st.info("No ranking data available")
        return

    st.divider()

    # Display rankings
    col1, col2 = st.columns([2, 3])

    with col1:
        st.write("**Rankings:**")
        st.dataframe(
            rankings,
            use_container_width=True,
            hide_index=True,
            column_config={
                "rank_position": st.column_config.NumberColumn("#", width="small"),
                selected_metric: st.column_config.NumberColumn(
                    metric_options[selected_metric],
                    format="%.1f" if "per" in selected_metric or "percentage" in selected_metric else "%d",
                ),
            },
        )

    with col2:
        # Bar chart of rankings
        fig = px.bar(
            rankings.head(15),
            x="name",
            y=selected_metric,
            color="rank",
            title=f"Top 15 by {metric_options[selected_metric]}",
            labels={"name": "Faculty", selected_metric: metric_options[selected_metric]},
        )
        fig.update_xaxes(tickangle=45)
        st.plotly_chart(fig, use_container_width=True)

    # Distribution
    st.divider()
    st.write("**Distribution:**")

    comparison = compare_faculty(data)

    if selected_metric in comparison.columns:
        fig = px.histogram(
            comparison,
            x=selected_metric,
            nbins=20,
            title=f"Distribution of {metric_options[selected_metric]}",
            labels={selected_metric: metric_options[selected_metric]},
        )

        # Add median line
        median_val = comparison[selected_metric].median()
        fig.add_vline(
            x=median_val,
            line_dash="dash",
            line_color="red",
            annotation_text=f"Median: {median_val:.1f}",
        )

        st.plotly_chart(fig, use_container_width=True)


def render_similar_faculty(data: List[Dict]):
    """Render similar faculty finder"""
    st.subheader("Find Similar Faculty")

    st.markdown("""
    Find faculty members with similar research profiles based on productivity metrics.
    """)

    # Faculty selector
    faculty_names = [d.get("person", {}).get("name", "Unknown") for d in data]

    selected_name = st.selectbox(
        "Select a faculty member",
        options=faculty_names,
        key="similar_faculty",
    )

    if not selected_name:
        return

    # Find selected data
    selected_data = next(
        (d for d in data if d.get("person", {}).get("name") == selected_name),
        None,
    )

    if selected_data is None:
        st.error("Could not find selected faculty member")
        return

    # Settings
    n_similar = st.slider(
        "Number of similar faculty to find",
        min_value=3,
        max_value=10,
        value=5,
        key="n_similar",
    )

    st.divider()

    # Find similar
    similar = find_similar_faculty(selected_data, data, n=n_similar)

    if not similar:
        st.info("Could not find similar faculty members")
        return

    st.write(f"**Faculty similar to {selected_name}:**")

    # Display as table
    similar_df = pd.DataFrame(similar)

    st.dataframe(
        similar_df,
        use_container_width=True,
        hide_index=True,
        column_config={
            "similarity_score": st.column_config.ProgressColumn(
                "Similarity",
                min_value=0,
                max_value=1,
                format="%.2f",
            ),
        },
    )

    # Comparison with selected
    st.divider()
    st.write("**Detailed Comparison:**")

    # Get data for similar faculty
    similar_names = [s["name"] for s in similar]
    comparison_data = [selected_data] + [
        d for d in data
        if d.get("person", {}).get("name") in similar_names
    ]

    comparison = compare_faculty(comparison_data)

    if not comparison.empty:
        # Highlight the selected person
        st.dataframe(
            comparison.style.apply(
                lambda x: ["background-color: #E8F4EA" if x.name == 0 else "" for _ in x],
                axis=1,
            ),
            use_container_width=True,
            hide_index=True,
        )

    # Collaboration potential
    if len(similar) > 0:
        st.divider()
        st.write("**Collaboration Potential:**")

        for s in similar[:3]:
            similar_person_data = next(
                (d for d in data if d.get("person", {}).get("name") == s["name"]),
                None,
            )

            if similar_person_data:
                potential = compute_collaboration_potential(selected_data, similar_person_data)

                with st.expander(f"With {s['name']}"):
                    if potential.get("complementary"):
                        st.success("Complementary strengths identified!")
                        st.write(f"**{selected_name} strengths:** {', '.join(potential['strengths_a']) or 'Similar to peer'}")
                        st.write(f"**{s['name']} strengths:** {', '.join(potential['strengths_b']) or 'Similar to peer'}")
                    else:
                        st.info("Similar profiles - may benefit from collaboration on shared interests")
