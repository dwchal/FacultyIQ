"""
FacultyIQ - Academic Research Analytics Dashboard
Python/Streamlit version

Main application entry point with page navigation.
"""

import os
import sys
from pathlib import Path

import streamlit as st
import yaml

# Add the app directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

# Page configuration must be first Streamlit command
st.set_page_config(
    page_title="FacultyIQ - Academic Research Analytics",
    page_icon="📊",
    layout="wide",
    initial_sidebar_state="expanded",
)


def load_config():
    """Load configuration from YAML file"""
    config_path = Path(__file__).parent / "config.yaml"
    if config_path.exists():
        with open(config_path, "r") as f:
            return yaml.safe_load(f).get("default", {})
    return {}


def init_session_state():
    """Initialize session state variables"""
    defaults = {
        "roster": None,  # DataFrame of uploaded roster
        "roster_validated": False,
        "roster_cleaned": False,
        "resolved_data": None,  # List of dicts with person data and API results
        "metrics_computed": False,
        "current_page": "home",
        "config": load_config(),
        "cache_manager": None,
        "openalex_api": None,
        "identity_store": None,
    }

    for key, value in defaults.items():
        if key not in st.session_state:
            st.session_state[key] = value


def init_services():
    """Initialize API clients and cache"""
    from utils.cache import CacheManager, IdentityMappingStore
    from utils.api import OpenAlexAPI

    config = st.session_state.config

    if st.session_state.cache_manager is None:
        cache_config = config.get("cache", {})
        st.session_state.cache_manager = CacheManager(
            cache_dir=cache_config.get("directory", ".cache"),
            expiration_days=cache_config.get("expiration_days", 7),
            enabled=cache_config.get("enabled", True),
        )

    if st.session_state.openalex_api is None:
        openalex_config = config.get("openalex", {})
        email = openalex_config.get("email") or os.getenv("OPENALEX_EMAIL")
        st.session_state.openalex_api = OpenAlexAPI(
            email=email,
            per_page=openalex_config.get("per_page", 200),
        )

    if st.session_state.identity_store is None:
        st.session_state.identity_store = IdentityMappingStore()


def render_sidebar():
    """Render the sidebar navigation"""
    with st.sidebar:
        st.title("📊 FacultyIQ")
        st.markdown("*Academic Research Analytics*")
        st.divider()

        # Navigation
        st.subheader("Navigation")

        pages = {
            "home": ("🏠 Home", "Welcome & Upload"),
            "resolution": ("🔍 Identity Resolution", "Match faculty to profiles"),
            "dashboard": ("📈 Division Dashboard", "Aggregate analytics"),
            "profiles": ("👤 Faculty Profiles", "Individual details"),
            "prediction": ("🎯 Rank Prediction", "Career analysis"),
            "comparison": ("⚖️ Comparison", "Side-by-side analysis"),
            "export": ("📁 Export", "Download data"),
        }

        for page_key, (label, description) in pages.items():
            col1, col2 = st.columns([4, 1])
            with col1:
                if st.button(label, key=f"nav_{page_key}", use_container_width=True):
                    st.session_state.current_page = page_key
                    st.rerun()
            with col2:
                # Show status indicators
                if page_key == "home" and st.session_state.roster is not None:
                    st.write("✓")
                elif page_key == "resolution" and st.session_state.resolved_data:
                    st.write("✓")

        st.divider()

        # Status summary
        st.subheader("Status")

        if st.session_state.roster is not None:
            roster_len = len(st.session_state.roster)
            st.success(f"Roster: {roster_len} faculty")

            if st.session_state.resolved_data:
                resolved_count = sum(
                    1 for d in st.session_state.resolved_data
                    if d.get("resolution_status") == "resolved"
                )
                st.info(f"Resolved: {resolved_count}/{roster_len}")
        else:
            st.warning("No roster uploaded")

        st.divider()

        # Quick actions
        st.subheader("Actions")

        if st.button("🗑️ Clear Cache", use_container_width=True):
            if st.session_state.cache_manager:
                count = st.session_state.cache_manager.clear()
                st.success(f"Cleared {count} cache entries")

        if st.button("🔄 Reset Session", use_container_width=True):
            for key in list(st.session_state.keys()):
                del st.session_state[key]
            st.rerun()

        st.divider()

        # Version info
        config = st.session_state.config
        app_config = config.get("app", {})
        st.caption(f"Version {app_config.get('version', '2.0.0')}")


def render_home_page():
    """Render the home/upload page"""
    st.title("Welcome to FacultyIQ")
    st.markdown("""
    **FacultyIQ** is an academic research analytics dashboard that helps you track
    and analyze faculty research productivity and impact metrics.

    ### Getting Started

    1. **Upload your roster** - CSV or Excel file with faculty information
    2. **Resolve identities** - Match faculty to their OpenAlex profiles
    3. **Explore analytics** - View dashboards, profiles, and insights
    4. **Export data** - Download metrics and visualizations
    """)

    st.divider()

    # File upload
    st.subheader("📂 Upload Faculty Roster")

    uploaded_file = st.file_uploader(
        "Choose a CSV or Excel file",
        type=["csv", "xlsx", "xls"],
        help="Upload a file containing faculty names and optional identifiers (Scopus ID, ORCID, etc.)",
    )

    if uploaded_file is not None:
        import pandas as pd
        from utils.validation import normalize_column_names, validate_roster, clean_roster

        try:
            # Read file
            if uploaded_file.name.endswith(".csv"):
                df = pd.read_csv(uploaded_file)
            else:
                df = pd.read_excel(uploaded_file)

            st.success(f"Loaded {len(df)} rows from {uploaded_file.name}")

            # Normalize columns
            df, column_mapping = normalize_column_names(df)

            # Show column mapping
            with st.expander("Column Mapping"):
                st.write("Detected columns:")
                for standard, original in column_mapping.items():
                    st.write(f"  - **{standard}** ← {original}")

            # Validate
            validation = validate_roster(df)

            if not validation["is_valid"]:
                st.error("Validation failed:")
                for issue in validation["issues"]:
                    st.error(f"  - {issue}")
            else:
                # Show warnings if any
                for warning in validation.get("warnings", []):
                    st.warning(warning)

                # Show completeness
                with st.expander("Data Completeness"):
                    completeness = validation["completeness"]
                    for col, stats in completeness.items():
                        pct = stats["percentage"]
                        st.progress(pct / 100, text=f"{col}: {pct}%")

                # Clean data
                df_cleaned = clean_roster(df)

                # Preview
                st.subheader("Preview")
                st.dataframe(df_cleaned.head(10), use_container_width=True)

                # Confirm button
                if st.button("✅ Use This Roster", type="primary"):
                    st.session_state.roster = df_cleaned
                    st.session_state.roster_validated = True
                    st.session_state.roster_cleaned = True
                    st.session_state.resolved_data = None  # Reset resolution
                    st.success("Roster loaded! Go to Identity Resolution to continue.")

        except Exception as e:
            st.error(f"Error loading file: {e}")

    # Alternative: Manual entry
    st.divider()
    st.subheader("Or Build Roster Manually")
    st.info("You can also build a roster from scratch by searching OpenAlex directly on the Identity Resolution page.")

    # Sample data option
    st.divider()
    sample_path = Path(__file__).parent / "data" / "sample_roster.csv"
    if sample_path.exists():
        if st.button("📋 Load Sample Data"):
            import pandas as pd
            from utils.validation import normalize_column_names, clean_roster

            df = pd.read_csv(sample_path)
            df, _ = normalize_column_names(df)
            df = clean_roster(df)
            st.session_state.roster = df
            st.session_state.roster_validated = True
            st.session_state.roster_cleaned = True
            st.session_state.resolved_data = None
            st.success("Sample roster loaded!")
            st.rerun()


def render_page():
    """Render the current page based on session state"""
    page = st.session_state.current_page

    if page == "home":
        render_home_page()
    elif page == "resolution":
        from pages.resolution import render_resolution_page
        render_resolution_page()
    elif page == "dashboard":
        from pages.dashboard import render_dashboard_page
        render_dashboard_page()
    elif page == "profiles":
        from pages.profiles import render_profiles_page
        render_profiles_page()
    elif page == "prediction":
        from pages.prediction import render_prediction_page
        render_prediction_page()
    elif page == "comparison":
        from pages.comparison import render_comparison_page
        render_comparison_page()
    elif page == "export":
        from pages.export import render_export_page
        render_export_page()
    else:
        render_home_page()


def main():
    """Main application entry point"""
    init_session_state()
    init_services()
    render_sidebar()
    render_page()


if __name__ == "__main__":
    main()
