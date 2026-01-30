"""API clients for OpenAlex, Scopus, and Google Scholar"""

import os
import time
import logging
from typing import Optional, List, Dict, Any
from dataclasses import dataclass, field
from datetime import datetime

import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

logger = logging.getLogger(__name__)


@dataclass
class Author:
    """Represents an author from OpenAlex"""
    id: str
    display_name: str
    orcid: Optional[str] = None
    works_count: int = 0
    cited_by_count: int = 0
    h_index: int = 0
    i10_index: int = 0
    works_api_url: Optional[str] = None
    last_known_institutions: List[Dict] = field(default_factory=list)
    counts_by_year: List[Dict] = field(default_factory=list)
    summary_stats: Dict = field(default_factory=dict)

    @classmethod
    def from_openalex(cls, data: dict) -> "Author":
        """Create Author from OpenAlex API response"""
        summary = data.get("summary_stats", {})
        return cls(
            id=data.get("id", "").replace("https://openalex.org/", ""),
            display_name=data.get("display_name", "Unknown"),
            orcid=data.get("orcid"),
            works_count=data.get("works_count", 0),
            cited_by_count=data.get("cited_by_count", 0),
            h_index=summary.get("h_index", 0),
            i10_index=summary.get("i10_index", 0),
            works_api_url=data.get("works_api_url"),
            last_known_institutions=data.get("last_known_institutions", []),
            counts_by_year=data.get("counts_by_year", []),
            summary_stats=summary,
        )


@dataclass
class Work:
    """Represents a publication/work from OpenAlex"""
    id: str
    title: str
    publication_year: Optional[int] = None
    publication_date: Optional[str] = None
    doi: Optional[str] = None
    cited_by_count: int = 0
    is_oa: bool = False
    oa_status: Optional[str] = None
    type: Optional[str] = None
    venue: Optional[str] = None
    authorships: List[Dict] = field(default_factory=list)

    @classmethod
    def from_openalex(cls, data: dict) -> "Work":
        """Create Work from OpenAlex API response"""
        primary_location = data.get("primary_location", {}) or {}
        source = primary_location.get("source", {}) or {}
        oa = data.get("open_access", {}) or {}

        return cls(
            id=data.get("id", "").replace("https://openalex.org/", ""),
            title=data.get("title", "Untitled"),
            publication_year=data.get("publication_year"),
            publication_date=data.get("publication_date"),
            doi=data.get("doi"),
            cited_by_count=data.get("cited_by_count", 0),
            is_oa=oa.get("is_oa", False),
            oa_status=oa.get("oa_status"),
            type=data.get("type"),
            venue=source.get("display_name"),
            authorships=data.get("authorships", []),
        )


class OpenAlexAPI:
    """Client for OpenAlex API"""

    BASE_URL = "https://api.openalex.org"

    def __init__(self, email: Optional[str] = None, per_page: int = 200):
        self.email = email or os.getenv("OPENALEX_EMAIL")
        self.per_page = per_page

        # Set up session with retries
        self.session = requests.Session()
        retry_strategy = Retry(
            total=3,
            backoff_factor=1,
            status_forcelist=[429, 500, 502, 503, 504],
        )
        adapter = HTTPAdapter(max_retries=retry_strategy)
        self.session.mount("https://", adapter)

    def _build_params(self, **kwargs) -> dict:
        """Build request parameters with optional polite pool email"""
        params = {k: v for k, v in kwargs.items() if v is not None}
        if self.email:
            params["mailto"] = self.email
        return params

    def search_authors(
        self,
        query: str,
        institution: Optional[str] = None,
        per_page: int = 10,
    ) -> List[Author]:
        """Search for authors by name"""
        params = self._build_params(
            search=query,
            per_page=per_page,
        )

        if institution:
            params["filter"] = f"last_known_institutions.display_name.search:{institution}"

        try:
            response = self.session.get(
                f"{self.BASE_URL}/authors",
                params=params,
                timeout=30,
            )
            response.raise_for_status()
            data = response.json()
            return [Author.from_openalex(a) for a in data.get("results", [])]

        except requests.RequestException as e:
            logger.error(f"OpenAlex author search error: {e}")
            return []

    def get_author(self, author_id: str) -> Optional[Author]:
        """Get author by OpenAlex ID"""
        # Normalize ID format
        if not author_id.startswith("A"):
            author_id = f"A{author_id}"
        if author_id.startswith("https://"):
            author_id = author_id.split("/")[-1]

        params = self._build_params()

        try:
            response = self.session.get(
                f"{self.BASE_URL}/authors/{author_id}",
                params=params,
                timeout=30,
            )
            response.raise_for_status()
            return Author.from_openalex(response.json())

        except requests.RequestException as e:
            logger.error(f"OpenAlex get author error: {e}")
            return None

    def get_author_by_scopus(self, scopus_id: str) -> Optional[Author]:
        """Find OpenAlex author by Scopus ID"""
        # Clean up Scopus ID
        scopus_id = str(scopus_id).strip()

        params = self._build_params(
            filter=f"ids.scopus:{scopus_id}",
            per_page=1,
        )

        try:
            response = self.session.get(
                f"{self.BASE_URL}/authors",
                params=params,
                timeout=30,
            )
            response.raise_for_status()
            data = response.json()
            results = data.get("results", [])
            if results:
                return Author.from_openalex(results[0])
            return None

        except requests.RequestException as e:
            logger.error(f"OpenAlex Scopus lookup error: {e}")
            return None

    def get_author_by_orcid(self, orcid: str) -> Optional[Author]:
        """Find OpenAlex author by ORCID"""
        # Normalize ORCID format
        orcid = orcid.strip()
        if not orcid.startswith("https://"):
            orcid = f"https://orcid.org/{orcid}"

        params = self._build_params(
            filter=f"orcid:{orcid}",
            per_page=1,
        )

        try:
            response = self.session.get(
                f"{self.BASE_URL}/authors",
                params=params,
                timeout=30,
            )
            response.raise_for_status()
            data = response.json()
            results = data.get("results", [])
            if results:
                return Author.from_openalex(results[0])
            return None

        except requests.RequestException as e:
            logger.error(f"OpenAlex ORCID lookup error: {e}")
            return None

    def get_author_works(
        self,
        author_id: str,
        per_page: Optional[int] = None,
        max_works: int = 1000,
        from_year: Optional[int] = None,
        to_year: Optional[int] = None,
    ) -> List[Work]:
        """Get all works for an author with pagination"""
        # Normalize ID format
        if not author_id.startswith("A"):
            author_id = f"A{author_id}"

        per_page = per_page or self.per_page
        works = []
        cursor = "*"

        # Build filter
        filters = [f"author.id:{author_id}"]
        if from_year:
            filters.append(f"publication_year:>={from_year}")
        if to_year:
            filters.append(f"publication_year:<={to_year}")

        filter_str = ",".join(filters)

        while len(works) < max_works:
            params = self._build_params(
                filter=filter_str,
                per_page=min(per_page, max_works - len(works)),
                cursor=cursor,
                sort="publication_year:desc",
            )

            try:
                response = self.session.get(
                    f"{self.BASE_URL}/works",
                    params=params,
                    timeout=60,
                )
                response.raise_for_status()
                data = response.json()

                results = data.get("results", [])
                if not results:
                    break

                works.extend([Work.from_openalex(w) for w in results])

                # Check for next page
                cursor = data.get("meta", {}).get("next_cursor")
                if not cursor:
                    break

            except requests.RequestException as e:
                logger.error(f"OpenAlex get works error: {e}")
                break

        return works


class ScholarAPI:
    """Client for Google Scholar (via scholarly library)"""

    def __init__(self, rate_limit_seconds: float = 3.0):
        self.rate_limit = rate_limit_seconds
        self._last_request = 0

    def _wait_for_rate_limit(self):
        """Enforce rate limiting"""
        elapsed = time.time() - self._last_request
        if elapsed < self.rate_limit:
            time.sleep(self.rate_limit - elapsed)
        self._last_request = time.time()

    def get_author_by_id(self, scholar_id: str) -> Optional[Dict]:
        """Get Google Scholar author profile by ID"""
        try:
            from scholarly import scholarly

            self._wait_for_rate_limit()

            author = scholarly.search_author_id(scholar_id)
            if author:
                self._wait_for_rate_limit()
                author = scholarly.fill(author, sections=["basics", "indices", "counts"])
                return {
                    "scholar_id": author.get("scholar_id"),
                    "name": author.get("name"),
                    "affiliation": author.get("affiliation"),
                    "h_index": author.get("hindex", 0),
                    "i10_index": author.get("i10index", 0),
                    "cited_by": author.get("citedby", 0),
                    "interests": author.get("interests", []),
                    "cites_per_year": author.get("cites_per_year", {}),
                }
            return None

        except ImportError:
            logger.warning("scholarly library not installed")
            return None
        except Exception as e:
            logger.error(f"Google Scholar error: {e}")
            return None

    def search_author(self, name: str, affiliation: Optional[str] = None) -> List[Dict]:
        """Search for authors by name"""
        try:
            from scholarly import scholarly

            self._wait_for_rate_limit()

            query = f"{name} {affiliation}" if affiliation else name
            results = []

            for author in scholarly.search_author(query):
                results.append({
                    "scholar_id": author.get("scholar_id"),
                    "name": author.get("name"),
                    "affiliation": author.get("affiliation"),
                    "cited_by": author.get("citedby", 0),
                })
                if len(results) >= 5:
                    break

            return results

        except ImportError:
            logger.warning("scholarly library not installed")
            return []
        except Exception as e:
            logger.error(f"Google Scholar search error: {e}")
            return []


def fetch_person_data(
    person: Dict,
    openalex_api: OpenAlexAPI,
    cache: Optional["CacheManager"] = None,
    include_works: bool = True,
) -> Dict:
    """
    Fetch all available data for a single person using layered approach.
    Attempts OpenAlex first, then falls back to other sources.
    """
    from .cache import CacheManager

    result = {
        "person": person,
        "openalex_author": None,
        "works": [],
        "scholar_data": None,
        "resolution_status": "pending",
        "data_sources": [],
    }

    # Check for existing OpenAlex ID
    openalex_id = person.get("openalex_id")

    # Try to resolve via Scopus ID if no OpenAlex ID
    if not openalex_id:
        scopus_id = person.get("scopus_id")
        if scopus_id:
            cache_key = f"scopus_to_openalex_{scopus_id}"
            if cache:
                cached = cache.get(cache_key, prefix="resolution")
                if cached:
                    openalex_id = cached

            if not openalex_id:
                author = openalex_api.get_author_by_scopus(scopus_id)
                if author:
                    openalex_id = author.id
                    if cache:
                        cache.set(cache_key, openalex_id, prefix="resolution")
                    result["resolution_status"] = "auto_resolved"

    # Try to resolve via ORCID if still no OpenAlex ID
    if not openalex_id:
        orcid = person.get("orcid")
        if orcid:
            author = openalex_api.get_author_by_orcid(orcid)
            if author:
                openalex_id = author.id
                result["resolution_status"] = "auto_resolved"

    # Fetch OpenAlex data if we have an ID
    if openalex_id:
        cache_key = f"author_{openalex_id}"

        # Try cache first
        if cache:
            cached_author = cache.get(cache_key, prefix="openalex")
            if cached_author:
                result["openalex_author"] = cached_author
                result["data_sources"].append("openalex_cache")

        # Fetch from API if not cached
        if not result["openalex_author"]:
            author = openalex_api.get_author(openalex_id)
            if author:
                result["openalex_author"] = author
                result["data_sources"].append("openalex")
                if cache:
                    cache.set(cache_key, author, prefix="openalex")

        # Fetch works if requested
        if include_works and result["openalex_author"]:
            works_cache_key = f"works_{openalex_id}"

            if cache:
                cached_works = cache.get(works_cache_key, prefix="openalex")
                if cached_works:
                    result["works"] = cached_works
                    result["data_sources"].append("works_cache")

            if not result["works"]:
                works = openalex_api.get_author_works(openalex_id)
                result["works"] = works
                if cache:
                    cache.set(works_cache_key, works, prefix="openalex")

        result["resolution_status"] = "resolved"
    else:
        result["resolution_status"] = "pending"

    return result


def fetch_all_person_data(
    roster: List[Dict],
    openalex_api: OpenAlexAPI,
    cache: Optional["CacheManager"] = None,
    include_works: bool = True,
    progress_callback: Optional[callable] = None,
) -> List[Dict]:
    """
    Fetch data for all people in roster with progress tracking.
    """
    results = []
    total = len(roster)

    for i, person in enumerate(roster):
        try:
            result = fetch_person_data(
                person,
                openalex_api,
                cache=cache,
                include_works=include_works,
            )
            results.append(result)
        except Exception as e:
            logger.error(f"Error fetching data for {person.get('name', 'Unknown')}: {e}")
            results.append({
                "person": person,
                "openalex_author": None,
                "works": [],
                "resolution_status": "failed",
                "error": str(e),
            })

        if progress_callback:
            progress_callback(i + 1, total)

    return results
