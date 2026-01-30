"""Cache management utilities for FacultyIQ"""

import os
import json
import hashlib
import pickle
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Optional
import logging

logger = logging.getLogger(__name__)


class CacheManager:
    """File-based cache manager with configurable expiration"""

    def __init__(
        self,
        cache_dir: str = ".cache",
        expiration_days: int = 7,
        enabled: bool = True,
    ):
        self.cache_dir = Path(cache_dir)
        self.expiration_days = expiration_days
        self.enabled = enabled

        if self.enabled:
            self.cache_dir.mkdir(parents=True, exist_ok=True)

    def _generate_key(self, *args, **kwargs) -> str:
        """Generate a unique cache key from arguments"""
        key_data = json.dumps({"args": args, "kwargs": kwargs}, sort_keys=True, default=str)
        return hashlib.md5(key_data.encode()).hexdigest()

    def _get_cache_path(self, key: str, prefix: str = "") -> Path:
        """Get the path for a cache file"""
        filename = f"{prefix}_{key}.pkl" if prefix else f"{key}.pkl"
        return self.cache_dir / filename

    def get(self, key: str, prefix: str = "") -> Optional[Any]:
        """Retrieve a value from cache if it exists and is not expired"""
        if not self.enabled:
            return None

        cache_path = self._get_cache_path(key, prefix)

        if not cache_path.exists():
            return None

        try:
            with open(cache_path, "rb") as f:
                cached_data = pickle.load(f)

            # Check expiration
            cached_time = cached_data.get("timestamp")
            if cached_time:
                expiration = cached_time + timedelta(days=self.expiration_days)
                if datetime.now() > expiration:
                    logger.debug(f"Cache expired for key: {key}")
                    cache_path.unlink()
                    return None

            logger.debug(f"Cache hit for key: {key}")
            return cached_data.get("value")

        except (pickle.PickleError, EOFError, KeyError) as e:
            logger.warning(f"Cache read error for key {key}: {e}")
            return None

    def set(self, key: str, value: Any, prefix: str = "") -> bool:
        """Store a value in cache"""
        if not self.enabled:
            return False

        cache_path = self._get_cache_path(key, prefix)

        try:
            cached_data = {
                "timestamp": datetime.now(),
                "value": value,
            }
            with open(cache_path, "wb") as f:
                pickle.dump(cached_data, f)
            logger.debug(f"Cached value for key: {key}")
            return True

        except (pickle.PickleError, IOError) as e:
            logger.warning(f"Cache write error for key {key}: {e}")
            return False

    def delete(self, key: str, prefix: str = "") -> bool:
        """Delete a cache entry"""
        cache_path = self._get_cache_path(key, prefix)
        if cache_path.exists():
            cache_path.unlink()
            return True
        return False

    def clear(self, prefix: str = "") -> int:
        """Clear all cache entries, optionally filtered by prefix"""
        if not self.enabled:
            return 0

        count = 0
        pattern = f"{prefix}_*.pkl" if prefix else "*.pkl"

        for cache_file in self.cache_dir.glob(pattern):
            try:
                cache_file.unlink()
                count += 1
            except OSError:
                pass

        logger.info(f"Cleared {count} cache entries")
        return count

    def get_or_set(self, key: str, fetch_func: callable, prefix: str = "") -> Any:
        """Get from cache or fetch and cache if not present"""
        value = self.get(key, prefix)
        if value is not None:
            return value

        value = fetch_func()
        if value is not None:
            self.set(key, value, prefix)
        return value


class IdentityMappingStore:
    """Persistent storage for identity resolution mappings"""

    def __init__(self, storage_path: str = ".cache/identity_mappings.json"):
        self.storage_path = Path(storage_path)
        self.storage_path.parent.mkdir(parents=True, exist_ok=True)
        self._mappings = self._load()

    def _load(self) -> dict:
        """Load mappings from disk"""
        if self.storage_path.exists():
            try:
                with open(self.storage_path, "r") as f:
                    return json.load(f)
            except (json.JSONDecodeError, IOError):
                return {}
        return {}

    def _save(self):
        """Save mappings to disk"""
        with open(self.storage_path, "w") as f:
            json.dump(self._mappings, f, indent=2)

    def get_mapping(self, identifier: str) -> Optional[dict]:
        """Get a stored identity mapping"""
        return self._mappings.get(identifier)

    def set_mapping(
        self,
        identifier: str,
        openalex_id: str,
        display_name: str,
        resolution_type: str = "manual",
    ):
        """Store an identity mapping"""
        self._mappings[identifier] = {
            "openalex_id": openalex_id,
            "display_name": display_name,
            "resolution_type": resolution_type,
            "timestamp": datetime.now().isoformat(),
        }
        self._save()

    def delete_mapping(self, identifier: str) -> bool:
        """Delete an identity mapping"""
        if identifier in self._mappings:
            del self._mappings[identifier]
            self._save()
            return True
        return False

    def get_all_mappings(self) -> dict:
        """Get all stored mappings"""
        return self._mappings.copy()

    def clear_all(self):
        """Clear all mappings"""
        self._mappings = {}
        self._save()
