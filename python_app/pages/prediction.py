"""
Rank Prediction Page

Analyze faculty metrics to predict appropriate academic rank
and identify potential promotion candidates.
"""

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from typing import Dict, List, Optional

from utils.prediction import (
    compute_rank_benchmarks,
    predict_rank,
    identify_promotion_candidates,
    compare_to_peers,
)
from utils.metrics import compute_person_metrics


def render_prediction_page():
    """Render the rank prediction page"""
    st.title("🎯 Rank Prediction & Benchmarking")

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
    Analyze faculty metrics against benchmarks to predict appropriate academic rank
    and identify potential promotion candidates based on research productivity.
    """)

    # Compute benchmarks
    benchmarks = compute_rank_benchmarks(resolved_data)

    if benchmarks.empty:
        st.warning("Insufficient data to compute rank benchmarks. Need at least 2 faculty per rank.")
        return

    st.divider()

    # Tabs for different views
    tab1, tab2, tab3 = st.tabs([
        "📊 Rank Benchmarks",
        "🎯 Individual Predictions",
        "⬆️ Promotion Candidates",
    ])

    with tab1:
        render_rank_benchmarks(benchmarks, resolved_data)

    with tab2:
        render_individual_predictions(resolved_data, benchmarks)

    with tab3:
        render_promotion_candidates(resolved_data, benchmarks)


def render_rank_benchmarks(benchmarks: pd.DataFrame, data: List[Dict]):
    """Render rank benchmark statistics and visualizations"""
    st.subheader("Rank Benchmarks")

    st.markdown("""
    Statistics computed from resolved faculty data, grouped by academic rank.
    Use these benchmarks to understand expectations at each career stage.
    """)

    # Summary metrics by rank
    cols = st.columns(len(benchmarks))

    for i, (_, row) in enumerate(benchmarks.iterrows()):
        with cols[i]:
            st.write(f"**{row['rank']}**")
            st.write(f"n = {row['count']}")
            if "total_works_mean" in row:
                st.metric("Avg Works", f"{row['total_works_mean']:.0f}")
            if "h_index_mean" in row:
                st.metric("Avg h-index", f"{row['h_index_mean']:.0f}")

    st.divider()

    # Visualizations
    col1, col2 = st.columns(2)

    with col1:
        # Works distribution by rank
        metrics_data = []
        for _, row in benchmarks.iterrows():
            if "total_works_mean" in row and "total_works_std" in row:
                metrics_data.append({
                    "Rank": row["rank"],
                    "Mean": row["total_works_mean"],
                    "Std": row["total_works_std"],
                    "Min": row.get("total_works_min", 0),
                    "Max": row.get("total_works_max", 0),
                })

        if metrics_data:
            df = pd.DataFrame(metrics_data)

            fig = go.Figure()

            fig.add_trace(go.Bar(
                name="Mean Works",
                x=df["Rank"],
                y=df["Mean"],
                error_y=dict(type="data", array=df["Std"]),
            ))

            fig.update_layout(
                title="Total Works by Rank",
                xaxis_title="Academic Rank",
                yaxis_title="Works",
            )

            st.plotly_chart(fig, use_container_width=True)

    with col2:
        # h-index distribution by rank
        metrics_data = []
        for _, row in benchmarks.iterrows():
            if "h_index_mean" in row and "h_index_std" in row:
                metrics_data.append({
                    "Rank": row["rank"],
                    "Mean": row["h_index_mean"],
                    "Std": row["h_index_std"],
                })

        if metrics_data:
            df = pd.DataFrame(metrics_data)

            fig = go.Figure()

            fig.add_trace(go.Bar(
                name="Mean h-index",
                x=df["Rank"],
                y=df["Mean"],
                error_y=dict(type="data", array=df["Std"]),
                marker_color="#27ae60",
            ))

            fig.update_layout(
                title="h-index by Rank",
                xaxis_title="Academic Rank",
                yaxis_title="h-index",
            )

            st.plotly_chart(fig, use_container_width=True)

    # Detailed benchmark table
    with st.expander("View Detailed Benchmarks"):
        # Select which metrics to show
        available_metrics = [col.replace("_mean", "") for col in benchmarks.columns if col.endswith("_mean")]

        display_cols = ["rank", "count"]
        for metric in available_metrics:
            for suffix in ["_mean", "_median", "_min", "_max"]:
                col_name = f"{metric}{suffix}"
                if col_name in benchmarks.columns:
                    display_cols.append(col_name)

        st.dataframe(benchmarks[display_cols], use_container_width=True, hide_index=True)


def render_individual_predictions(data: List[Dict], benchmarks: pd.DataFrame):
    """Render individual rank predictions for each faculty member"""
    st.subheader("Individual Rank Predictions")

    # Faculty selector
    faculty_names = [d.get("person", {}).get("name", "Unknown") for d in data]
    selected_name = st.selectbox(
        "Select Faculty Member",
        options=faculty_names,
        key="prediction_selected_faculty",
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

    # Get prediction
    prediction = predict_rank(selected_data, benchmarks)

    st.divider()

    # Display prediction results
    col1, col2 = st.columns(2)

    with col1:
        st.write("**Prediction Results:**")

        if prediction.get("message"):
            st.warning(prediction["message"])
        else:
            current = prediction.get("current_rank", "Unknown")
            predicted = prediction.get("predicted_rank", "Unknown")
            confidence = prediction.get("confidence", 0)

            st.write(f"Current Rank: **{current}**")
            st.write(f"Predicted Rank: **{predicted}**")
            st.progress(confidence, text=f"Confidence: {confidence:.1%}")

            if prediction.get("is_promotion_candidate"):
                st.success("⬆️ This faculty member may be a promotion candidate!")

    with col2:
        st.write("**Rank Scores:**")

        scores = prediction.get("rank_scores", {})
        if scores:
            score_df = pd.DataFrame([
                {"Rank": k, "Score": v}
                for k, v in sorted(scores.items(), key=lambda x: x[1], reverse=True)
            ])

            fig = px.bar(
                score_df,
                x="Rank",
                y="Score",
                color="Score",
                title="Similarity to Each Rank",
                color_continuous_scale="viridis",
            )
            fig.update_layout(showlegend=False)
            st.plotly_chart(fig, use_container_width=True)

    # Peer comparison
    st.divider()
    st.write("**Peer Comparison:**")

    peer_comparison = compare_to_peers(selected_data, data)

    if "error" in peer_comparison:
        st.info(peer_comparison["error"])
    else:
        st.write(f"Comparing to {peer_comparison['peer_count']} peers with rank '{peer_comparison['rank']}'")

        comparison_data = []
        for metric, stats in peer_comparison.get("comparisons", {}).items():
            comparison_data.append({
                "Metric": metric.replace("_", " ").title(),
                "Your Value": stats["person_value"],
                "Peer Mean": stats["peer_mean"],
                "Peer Median": stats["peer_median"],
                "Percentile": f"{stats['percentile']:.0f}%",
            })

        if comparison_data:
            comp_df = pd.DataFrame(comparison_data)
            st.dataframe(comp_df, use_container_width=True, hide_index=True)

            # Percentile visualization
            percentiles = [c["Percentile"].replace("%", "") for c in comparison_data]
            metrics = [c["Metric"] for c in comparison_data]

            fig = go.Figure()

            fig.add_trace(go.Bar(
                x=metrics,
                y=[float(p) for p in percentiles],
                marker_color=["#27ae60" if float(p) >= 50 else "#e74c3c" for p in percentiles],
            ))

            fig.add_hline(y=50, line_dash="dash", line_color="gray")

            fig.update_layout(
                title="Your Percentile Among Peers",
                xaxis_title="Metric",
                yaxis_title="Percentile",
                yaxis_range=[0, 100],
            )

            st.plotly_chart(fig, use_container_width=True)


def render_promotion_candidates(data: List[Dict], benchmarks: pd.DataFrame):
    """Render list of potential promotion candidates"""
    st.subheader("Promotion Candidates")

    st.markdown("""
    Faculty members whose metrics suggest they may be performing at a higher rank
    than their current designation.
    """)

    # Settings
    col1, col2 = st.columns(2)

    with col1:
        min_confidence = st.slider(
            "Minimum Confidence",
            min_value=0.0,
            max_value=1.0,
            value=0.3,
            step=0.1,
            key="promo_confidence",
        )

    with col2:
        min_works = st.number_input(
            "Minimum Works",
            min_value=1,
            value=3,
            step=1,
            key="promo_min_works",
        )

    # Get candidates
    candidates = identify_promotion_candidates(
        data,
        benchmarks,
        min_works=min_works,
        confidence_threshold=min_confidence,
    )

    if not candidates:
        st.info("No promotion candidates identified with current thresholds.")
        return

    st.divider()

    st.write(f"**Found {len(candidates)} potential promotion candidates:**")

    # Display as cards
    for candidate in candidates:
        with st.container():
            col1, col2, col3 = st.columns([3, 2, 2])

            with col1:
                st.write(f"**{candidate['name']}**")
                st.write(f"{candidate['current_rank']} → {candidate['predicted_rank']}")

            with col2:
                st.metric("Confidence", f"{candidate['confidence']:.0%}")

            with col3:
                st.write(f"Works: {candidate['total_works']}")
                st.write(f"h-index: {candidate['h_index']}")
                st.write(f"Citations: {candidate['total_citations']:,}")

            st.divider()

    # Summary table
    with st.expander("View as Table"):
        candidates_df = pd.DataFrame(candidates)
        st.dataframe(candidates_df, use_container_width=True, hide_index=True)

    # Export option
    if st.button("📥 Export Candidates List"):
        candidates_df = pd.DataFrame(candidates)
        csv = candidates_df.to_csv(index=False)
        st.download_button(
            label="Download CSV",
            data=csv,
            file_name="promotion_candidates.csv",
            mime="text/csv",
        )
