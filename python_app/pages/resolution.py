"""
Identity Resolution Page

Match faculty from roster to OpenAlex author profiles.
Supports auto-resolution via Scopus ID/ORCID and manual search.
"""

import streamlit as st
import pandas as pd
from typing import Dict, List, Optional

from utils.api import OpenAlexAPI, fetch_person_data, fetch_all_person_data
from utils.cache import CacheManager, IdentityMappingStore
from utils.validation import roster_to_dict_list


def render_resolution_page():
    """Render the identity resolution page"""
    st.title("🔍 Identity Resolution")

    if st.session_state.roster is None:
        st.warning("Please upload a roster first on the Home page.")

        # Manual roster building option
        st.divider()
        render_manual_roster_builder()
        return

    st.markdown("""
    Match faculty members to their OpenAlex author profiles. This enables
    fetching publication metrics and citation data.

    **Resolution Methods:**
    - **Auto-resolve**: Uses Scopus ID or ORCID to find OpenAlex profiles
    - **Manual search**: Search by name to find and select the correct author
    """)

    # Initialize resolved data if needed
    if st.session_state.resolved_data is None:
        roster = st.session_state.roster
        records = roster_to_dict_list(roster)
        st.session_state.resolved_data = [
            {
                "person": record,
                "openalex_author": None,
                "works": [],
                "resolution_status": "pending",
                "data_sources": [],
            }
            for record in records
        ]

    # Resolution controls
    col1, col2, col3 = st.columns(3)

    with col1:
        if st.button("🔄 Auto-Resolve All", type="primary", use_container_width=True):
            run_auto_resolution()

    with col2:
        if st.button("📥 Fetch All Data", use_container_width=True):
            fetch_all_data()

    with col3:
        if st.button("🗑️ Reset Resolution", use_container_width=True):
            roster = st.session_state.roster
            records = roster_to_dict_list(roster)
            st.session_state.resolved_data = [
                {
                    "person": record,
                    "openalex_author": None,
                    "works": [],
                    "resolution_status": "pending",
                    "data_sources": [],
                }
                for record in records
            ]
            st.rerun()

    st.divider()

    # Resolution status summary
    render_resolution_summary()

    st.divider()

    # Resolution table and manual controls
    render_resolution_table()


def render_resolution_summary():
    """Show summary of resolution status"""
    data = st.session_state.resolved_data

    if not data:
        return

    # Count by status
    status_counts = {}
    for person_data in data:
        status = person_data.get("resolution_status", "pending")
        status_counts[status] = status_counts.get(status, 0) + 1

    # Display metrics
    col1, col2, col3, col4, col5 = st.columns(5)

    with col1:
        st.metric("Total", len(data))
    with col2:
        st.metric("Resolved", status_counts.get("resolved", 0))
    with col3:
        st.metric("Auto-Resolved", status_counts.get("auto_resolved", 0))
    with col4:
        st.metric("Pending", status_counts.get("pending", 0))
    with col5:
        st.metric("Failed", status_counts.get("failed", 0))


def run_auto_resolution():
    """Auto-resolve all pending entries using Scopus ID or ORCID"""
    api = st.session_state.openalex_api
    cache = st.session_state.cache_manager
    identity_store = st.session_state.identity_store

    data = st.session_state.resolved_data
    progress_bar = st.progress(0)
    status_text = st.empty()

    resolved_count = 0

    for i, person_data in enumerate(data):
        if person_data.get("resolution_status") not in ["pending"]:
            progress_bar.progress((i + 1) / len(data))
            continue

        person = person_data.get("person", {})
        name = person.get("name", "Unknown")
        status_text.text(f"Resolving: {name}...")

        # Check identity store first
        stored_mapping = identity_store.get_mapping(name)
        if stored_mapping:
            openalex_id = stored_mapping.get("openalex_id")
            author = api.get_author(openalex_id)
            if author:
                person_data["openalex_author"] = author
                person_data["resolution_status"] = "resolved"
                person_data["person"]["openalex_id"] = openalex_id
                resolved_count += 1
                progress_bar.progress((i + 1) / len(data))
                continue

        # Try Scopus ID
        scopus_id = person.get("scopus_id")
        if scopus_id:
            author = api.get_author_by_scopus(scopus_id)
            if author:
                person_data["openalex_author"] = author
                person_data["resolution_status"] = "auto_resolved"
                person_data["person"]["openalex_id"] = author.id
                identity_store.set_mapping(name, author.id, author.display_name, "scopus")
                resolved_count += 1
                progress_bar.progress((i + 1) / len(data))
                continue

        # Try ORCID
        orcid = person.get("orcid")
        if orcid:
            author = api.get_author_by_orcid(orcid)
            if author:
                person_data["openalex_author"] = author
                person_data["resolution_status"] = "auto_resolved"
                person_data["person"]["openalex_id"] = author.id
                identity_store.set_mapping(name, author.id, author.display_name, "orcid")
                resolved_count += 1
                progress_bar.progress((i + 1) / len(data))
                continue

        progress_bar.progress((i + 1) / len(data))

    status_text.empty()
    progress_bar.empty()

    st.success(f"Auto-resolved {resolved_count} faculty members")
    st.rerun()


def fetch_all_data():
    """Fetch full data for all resolved entries"""
    api = st.session_state.openalex_api
    cache = st.session_state.cache_manager

    data = st.session_state.resolved_data
    progress_bar = st.progress(0)
    status_text = st.empty()

    fetched_count = 0

    for i, person_data in enumerate(data):
        if person_data.get("resolution_status") not in ["resolved", "auto_resolved"]:
            progress_bar.progress((i + 1) / len(data))
            continue

        person = person_data.get("person", {})
        name = person.get("name", "Unknown")
        status_text.text(f"Fetching data for: {name}...")

        # Fetch updated data
        result = fetch_person_data(
            person_data["person"],
            api,
            cache=cache,
            include_works=True,
        )

        # Update with fetched data
        if result.get("openalex_author"):
            person_data["openalex_author"] = result["openalex_author"]
        if result.get("works"):
            person_data["works"] = result["works"]
        person_data["data_sources"] = result.get("data_sources", [])
        fetched_count += 1

        progress_bar.progress((i + 1) / len(data))

    status_text.empty()
    progress_bar.empty()

    st.success(f"Fetched data for {fetched_count} faculty members")
    st.rerun()


def render_resolution_table():
    """Render the resolution table with manual controls"""
    data = st.session_state.resolved_data

    if not data:
        return

    st.subheader("Faculty Resolution Status")

    # Create table data
    table_data = []
    for i, person_data in enumerate(data):
        person = person_data.get("person", {})
        author = person_data.get("openalex_author")
        works = person_data.get("works", [])
        status = person_data.get("resolution_status", "pending")

        row = {
            "idx": i,
            "Name": person.get("name", "Unknown"),
            "Rank": person.get("rank", ""),
            "Status": status.replace("_", " ").title(),
            "OpenAlex ID": author.id if author else "",
            "Works": author.works_count if author else len(works),
            "Citations": author.cited_by_count if author else 0,
            "h-index": author.h_index if author else 0,
        }
        table_data.append(row)

    df = pd.DataFrame(table_data)

    # Status color coding
    def color_status(val):
        colors = {
            "Resolved": "background-color: #90EE90",
            "Auto Resolved": "background-color: #98FB98",
            "Pending": "background-color: #FFE4B5",
            "Failed": "background-color: #FFB6C1",
            "Skipped": "background-color: #D3D3D3",
        }
        return colors.get(val, "")

    styled_df = df.style.applymap(color_status, subset=["Status"])

    st.dataframe(
        styled_df,
        use_container_width=True,
        hide_index=True,
        column_config={
            "idx": None,  # Hide index column
            "Works": st.column_config.NumberColumn("Works", format="%d"),
            "Citations": st.column_config.NumberColumn("Citations", format="%d"),
            "h-index": st.column_config.NumberColumn("h-index", format="%d"),
        },
    )

    st.divider()

    # Manual resolution section
    st.subheader("Manual Resolution")

    pending_data = [d for d in data if d.get("resolution_status") in ["pending", "failed"]]

    if not pending_data:
        st.success("All faculty members have been resolved!")
        return

    # Select person to resolve
    pending_names = [d.get("person", {}).get("name", "Unknown") for d in pending_data]
    selected_name = st.selectbox("Select faculty member to resolve:", pending_names)

    if selected_name:
        selected_data = next(
            d for d in pending_data
            if d.get("person", {}).get("name") == selected_name
        )
        render_manual_resolution_panel(selected_data)


def render_manual_resolution_panel(person_data: Dict):
    """Render manual resolution controls for a single person"""
    person = person_data.get("person", {})
    name = person.get("name", "Unknown")

    st.write(f"**Resolving:** {name}")

    col1, col2 = st.columns(2)

    with col1:
        # Search controls
        search_query = st.text_input(
            "Search OpenAlex",
            value=name,
            key=f"search_{name}",
        )

        institution_filter = st.text_input(
            "Institution filter (optional)",
            key=f"inst_{name}",
        )

        if st.button("🔍 Search", key=f"search_btn_{name}"):
            api = st.session_state.openalex_api
            results = api.search_authors(
                search_query,
                institution=institution_filter if institution_filter else None,
                per_page=10,
            )

            st.session_state[f"search_results_{name}"] = results

    with col2:
        # Display search results
        results = st.session_state.get(f"search_results_{name}", [])

        if results:
            st.write("**Search Results:**")

            for i, author in enumerate(results):
                institutions = author.last_known_institutions
                inst_name = institutions[0].get("display_name", "Unknown") if institutions else "Unknown"

                with st.container():
                    st.write(f"**{author.display_name}**")
                    st.write(f"Institution: {inst_name}")
                    st.write(f"Works: {author.works_count} | Citations: {author.cited_by_count} | h-index: {author.h_index}")

                    if st.button(f"Select", key=f"select_{name}_{i}"):
                        # Apply resolution
                        apply_manual_resolution(person_data, author)
                        st.rerun()

                    st.divider()
        else:
            st.info("Enter a search query and click Search")

    # Skip option
    st.divider()
    if st.button("⏭️ Skip this person", key=f"skip_{name}"):
        person_data["resolution_status"] = "skipped"
        st.rerun()


def apply_manual_resolution(person_data: Dict, author):
    """Apply a manual resolution selection"""
    identity_store = st.session_state.identity_store

    person = person_data.get("person", {})
    name = person.get("name", "Unknown")

    person_data["openalex_author"] = author
    person_data["resolution_status"] = "resolved"
    person_data["person"]["openalex_id"] = author.id

    # Save to identity store
    identity_store.set_mapping(name, author.id, author.display_name, "manual")

    st.success(f"Resolved {name} to {author.display_name}")


def render_manual_roster_builder():
    """Render controls for building a roster from scratch"""
    st.subheader("🔧 Build Roster Manually")

    st.markdown("""
    Build a roster by searching OpenAlex and adding faculty one at a time.
    """)

    # Search controls
    search_query = st.text_input("Search for faculty member by name")
    institution_filter = st.text_input("Institution filter (optional)")

    if st.button("🔍 Search OpenAlex"):
        if search_query:
            api = st.session_state.openalex_api
            results = api.search_authors(
                search_query,
                institution=institution_filter if institution_filter else None,
                per_page=10,
            )
            st.session_state["manual_build_results"] = results

    # Display results
    results = st.session_state.get("manual_build_results", [])

    if results:
        st.write("**Search Results:**")

        for i, author in enumerate(results):
            institutions = author.last_known_institutions
            inst_name = institutions[0].get("display_name", "Unknown") if institutions else "Unknown"

            col1, col2 = st.columns([4, 1])

            with col1:
                st.write(f"**{author.display_name}**")
                st.caption(f"{inst_name} | Works: {author.works_count} | h-index: {author.h_index}")

            with col2:
                if st.button("➕ Add", key=f"add_manual_{i}"):
                    add_to_manual_roster(author)

            st.divider()

    # Show current manual roster
    manual_roster = st.session_state.get("manual_roster", [])

    if manual_roster:
        st.subheader(f"Current Roster ({len(manual_roster)} members)")

        df = pd.DataFrame(manual_roster)
        st.dataframe(df, use_container_width=True)

        if st.button("✅ Use This Roster", type="primary"):
            # Convert to standard roster format
            roster_df = pd.DataFrame(manual_roster)
            st.session_state.roster = roster_df
            st.session_state.roster_validated = True
            st.session_state.roster_cleaned = True

            # Initialize resolved data
            st.session_state.resolved_data = [
                {
                    "person": row,
                    "openalex_author": None,
                    "works": [],
                    "resolution_status": "resolved",  # Already resolved
                    "data_sources": [],
                }
                for row in manual_roster
            ]

            st.success("Roster created! Go to Identity Resolution to fetch data.")
            st.rerun()


def add_to_manual_roster(author):
    """Add an author to the manual roster"""
    if "manual_roster" not in st.session_state:
        st.session_state["manual_roster"] = []

    # Check for duplicates
    existing_ids = [r.get("openalex_id") for r in st.session_state["manual_roster"]]
    if author.id in existing_ids:
        st.warning(f"{author.display_name} is already in the roster")
        return

    institutions = author.last_known_institutions
    inst_name = institutions[0].get("display_name", "") if institutions else ""

    st.session_state["manual_roster"].append({
        "name": author.display_name,
        "openalex_id": author.id,
        "institution": inst_name,
        "rank": "",  # To be filled in
    })

    st.success(f"Added {author.display_name} to roster")
    st.rerun()
