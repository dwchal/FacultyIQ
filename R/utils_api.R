# =============================================================================
# FacultyIQ - API Utilities
# =============================================================================
# Functions for interacting with OpenAlex, Scopus, and Google Scholar APIs
# All functions degrade gracefully and handle errors appropriately
# Note: Emails are sensitive - we only use IDs and names for API queries

# Load required packages (should be loaded in global.R)
# library(openalexR)
# library(rscopus)
# library(scholar)
# library(httr)
# library(jsonlite)

# =============================================================================
# OpenAlex Functions (Primary source, free)
# =============================================================================

#' Set up OpenAlex configuration
#'
#' @param email Email for polite pool (optional but recommended)
#' @return TRUE if successful
setup_openalex <- function(email = NULL) {
  if (!is.null(email) && email != "") {
    options(openalexR.mailto = email)
  }
  return(TRUE)
}

#' Search for authors in OpenAlex by name
#'
#' @param name Author name to search
#' @param institution Optional institution keyword for filtering
#' @param limit Maximum results to return
#' @return Data frame of matching authors or NULL on error
search_openalex_authors <- function(name, institution = NULL, limit = 10) {
  tryCatch({
    # Build search filter
    # Note: We only use name, not email (email is sensitive)
    search_str <- name

    if (!is.null(institution) && institution != "") {
      # Search with institution affiliation
      authors <- openalexR::oa_fetch(
        entity = "authors",
        search = search_str,
        options = list(per_page = limit)
      )
    } else {
      authors <- openalexR::oa_fetch(
        entity = "authors",
        search = search_str,
        options = list(per_page = limit)
      )
    }

    if (is.null(authors) || nrow(authors) == 0) {
      return(NULL)
    }

    # Extract affiliations safely - handle missing column
    n_authors <- nrow(authors)
    if ("last_known_institution" %in% names(authors) &&
        !is.null(authors$last_known_institution) &&
        length(authors$last_known_institution) == n_authors) {
      affiliations <- vapply(authors$last_known_institution, function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        if (is.data.frame(x)) return(as.character(x$display_name[1]))
        if (is.list(x)) return(as.character(x$display_name[1]))
        return(NA_character_)
      }, FUN.VALUE = character(1))
    } else {
      affiliations <- rep(NA_character_, n_authors)
    }

    # Extract h_index and i10_index from summary_stats
    h_indices <- vapply(seq_len(n_authors), function(i) {
      if ("summary_stats" %in% names(authors) &&
          !is.null(authors$summary_stats) &&
          length(authors$summary_stats) >= i) {
        stats <- authors$summary_stats[[i]]
        if (is.list(stats) && !is.null(stats$h_index)) {
          return(as.integer(stats$h_index))
        }
      }
      return(NA_integer_)
    }, FUN.VALUE = integer(1))

    i10_indices <- vapply(seq_len(n_authors), function(i) {
      if ("summary_stats" %in% names(authors) &&
          !is.null(authors$summary_stats) &&
          length(authors$summary_stats) >= i) {
        stats <- authors$summary_stats[[i]]
        if (is.list(stats) && !is.null(stats$i10_index)) {
          return(as.integer(stats$i10_index))
        }
      }
      return(NA_integer_)
    }, FUN.VALUE = integer(1))

    # Standardize output format
    result <- data.frame(
      openalex_id = authors$id,
      display_name = authors$display_name,
      works_count = authors$works_count,
      cited_by_count = authors$cited_by_count,
      h_index = h_indices,
      i10_index = i10_indices,
      affiliation = affiliations,
      stringsAsFactors = FALSE
    )

    return(result)

  }, error = function(e) {
    warning(sprintf("OpenAlex author search failed: %s", e$message))
    return(NULL)
  })
}

#' Get OpenAlex author by Scopus ID
#'
#' @param scopus_id Scopus author ID
#' @return Data frame with author info or NULL
get_openalex_by_scopus <- function(scopus_id) {
  if (is.null(scopus_id) || is.na(scopus_id) || scopus_id == "") {
    return(NULL)
  }

  # Clean scopus ID (remove any non-numeric characters)
  scopus_id <- gsub("[^0-9]", "", as.character(scopus_id))

  tryCatch({
    # OpenAlex stores Scopus IDs as external IDs
    # The external_id filter format is: scopus:AUTHOR_ID
    authors <- openalexR::oa_fetch(
      entity = "authors",
      identifier = paste0("scopus:", scopus_id)
    )

    if (is.null(authors) || nrow(authors) == 0) {
      return(NULL)
    }

    # Extract h_index and i10_index from summary_stats
    h_index <- NA_integer_
    i10_index <- NA_integer_
    if ("summary_stats" %in% names(authors) &&
        !is.null(authors$summary_stats) &&
        length(authors$summary_stats) >= 1) {
      stats <- authors$summary_stats[[1]]
      if (is.list(stats)) {
        if (!is.null(stats$h_index)) h_index <- as.integer(stats$h_index)
        if (!is.null(stats$i10_index)) i10_index <- as.integer(stats$i10_index)
      }
    }

    # Return first match
    result <- data.frame(
      openalex_id = authors$id[1],
      display_name = authors$display_name[1],
      works_count = authors$works_count[1],
      cited_by_count = authors$cited_by_count[1],
      h_index = h_index,
      i10_index = i10_index,
      affiliation = {
        inst <- authors$last_known_institution[[1]]
        if (is.null(inst) || length(inst) == 0) NA_character_
        else if (is.data.frame(inst)) inst$display_name[1]
        else if (is.list(inst)) inst$display_name[1]
        else NA_character_
      },
      stringsAsFactors = FALSE
    )

    return(result)

  }, error = function(e) {
    warning(sprintf("OpenAlex lookup by Scopus ID failed: %s", e$message))
    return(NULL)
  })
}

#' Get OpenAlex author details
#'
#' @param openalex_id OpenAlex author ID
#' @return List with author details or NULL
get_openalex_author <- function(openalex_id) {
  if (is.null(openalex_id) || is.na(openalex_id) || openalex_id == "") {
    return(NULL)
  }

  tryCatch({
    author <- openalexR::oa_fetch(
      entity = "authors",
      identifier = openalex_id
    )

    if (is.null(author) || nrow(author) == 0) {
      return(NULL)
    }

    # Extract counts by year
    counts_by_year <- author$counts_by_year[[1]]

    list(
      openalex_id = author$id[1],
      display_name = author$display_name[1],
      works_count = author$works_count[1],
      cited_by_count = author$cited_by_count[1],
      h_index = author$summary_stats$h_index[1],
      i10_index = author$summary_stats$i10_index[1],
      last_known_institution = {
        inst <- author$last_known_institution[[1]]
        if (is.null(inst)) NA_character_
        else if (is.data.frame(inst)) inst$display_name[1]
        else if (is.list(inst)) inst$display_name[1]
        else NA_character_
      },
      counts_by_year = counts_by_year
    )

  }, error = function(e) {
    warning(sprintf("OpenAlex author fetch failed: %s", e$message))
    return(NULL)
  })
}

#' Get works for an OpenAlex author
#'
#' @param openalex_id OpenAlex author ID
#' @param from_year Start year (optional)
#' @param to_year End year (optional)
#' @param limit Maximum works to fetch
#' @return Data frame of works or NULL
get_openalex_works <- function(openalex_id, from_year = NULL, to_year = NULL, limit = 500) {
  if (is.null(openalex_id) || is.na(openalex_id) || openalex_id == "") {
    return(NULL)
  }

  tryCatch({
    works <- openalexR::oa_fetch(
      entity = "works",
      author.id = openalex_id,
      from_publication_date = if (!is.null(from_year)) paste0(from_year, "-01-01") else NULL,
      to_publication_date = if (!is.null(to_year)) paste0(to_year, "-12-31") else NULL,
      options = list(per_page = min(limit, 200))
    )

    if (is.null(works) || nrow(works) == 0) {
      return(NULL)
    }

    # Standardize output
    result <- data.frame(
      work_id = works$id,
      title = works$display_name,
      publication_year = works$publication_year,
      publication_date = works$publication_date,
      type = works$type,
      cited_by_count = works$cited_by_count,
      doi = works$doi,
      is_oa = works$is_oa,
      oa_status = vapply(works$open_access, function(x) {
        if (is.null(x)) return(NA_character_)
        if (is.list(x) && !is.null(x$oa_status)) return(as.character(x$oa_status[1]))
        return(NA_character_)
      }, FUN.VALUE = character(1)),
      journal = vapply(works$primary_location, function(x) {
        if (is.null(x)) return(NA_character_)
        if (is.list(x) && !is.null(x$source) && !is.null(x$source$display_name)) {
          return(as.character(x$source$display_name[1]))
        }
        return(NA_character_)
      }, FUN.VALUE = character(1)),
      stringsAsFactors = FALSE
    )

    # Extract concepts/topics if available
    if ("concepts" %in% names(works)) {
      result$concepts <- vapply(works$concepts, function(x) {
        if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(NA_character_)
        paste(head(x$display_name, 3), collapse = "; ")
      }, FUN.VALUE = character(1))
    }

    return(result)

  }, error = function(e) {
    warning(sprintf("OpenAlex works fetch failed: %s", e$message))
    return(NULL)
  })
}

# =============================================================================
# Scopus Functions (Optional, requires API key)
# =============================================================================

#' Check if Scopus API is configured
#'
#' @return TRUE if API key is available
scopus_is_configured <- function() {
  api_key <- Sys.getenv("SCOPUS_API_KEY", "")
  return(api_key != "")
}

#' Set up Scopus API configuration
#'
#' @param api_key Scopus API key (optional, uses env var if not provided)
#' @return TRUE if configured, FALSE otherwise
setup_scopus <- function(api_key = NULL) {
  if (is.null(api_key) || api_key == "") {
    api_key <- Sys.getenv("SCOPUS_API_KEY", "")
  }

  if (api_key == "") {
    return(FALSE)
  }

  # rscopus uses environment variable
  Sys.setenv(Elsevier_API = api_key)
  return(TRUE)
}

#' Get author metrics from Scopus
#'
#' @param scopus_id Scopus author ID
#' @return List with author metrics or NULL
get_scopus_author_metrics <- function(scopus_id) {
  if (!scopus_is_configured()) {
    return(NULL)
  }

  if (is.null(scopus_id) || is.na(scopus_id) || scopus_id == "") {
    return(NULL)
  }

  # Clean scopus ID
  scopus_id <- gsub("[^0-9]", "", as.character(scopus_id))

  tryCatch({
    # Use rscopus to get author info
    # Note: This is a simplified version - full implementation would
    # use rscopus::author_retrieval
    author_info <- rscopus::author_retrieval(
      au_id = scopus_id,
      verbose = FALSE
    )

    if (is.null(author_info) || length(author_info) == 0) {
      return(NULL)
    }

    # Parse response
    content <- httr::content(author_info, as = "parsed")

    if (is.null(content)) {
      return(NULL)
    }

    # Extract metrics from response
    # Structure varies by rscopus version
    list(
      scopus_id = scopus_id,
      document_count = content$`author-retrieval-response`[[1]]$coredata$`document-count`,
      cited_by_count = content$`author-retrieval-response`[[1]]$coredata$`cited-by-count`,
      h_index = content$`author-retrieval-response`[[1]]$`h-index`,
      source = "scopus"
    )

  }, error = function(e) {
    warning(sprintf("Scopus author retrieval failed: %s", e$message))
    return(NULL)
  })
}

# =============================================================================
# Google Scholar Functions (Optional, fragile due to scraping)
# =============================================================================

#' Get author profile from Google Scholar
#'
#' @param scholar_id Google Scholar user ID
#' @return List with profile data or NULL
get_scholar_profile <- function(scholar_id) {
  if (is.null(scholar_id) || is.na(scholar_id) || scholar_id == "") {
    return(NULL)
  }

  # Clean scholar ID (remove URL parts if present)
  scholar_id <- gsub(".*user=", "", scholar_id)
  scholar_id <- gsub("&.*", "", scholar_id)
  scholar_id <- trimws(scholar_id)

  if (scholar_id == "" || nchar(scholar_id) < 5) {
    return(NULL)
  }

  tryCatch({
    # Get profile using scholar package
    profile <- scholar::get_profile(scholar_id)

    if (is.null(profile)) {
      return(NULL)
    }

    list(
      scholar_id = scholar_id,
      name = profile$name,
      affiliation = profile$affiliation,
      total_cites = profile$total_cites,
      h_index = profile$h_index,
      i10_index = profile$i10_index,
      source = "scholar"
    )

  }, error = function(e) {
    # Scholar scraping is fragile - fail silently with warning
    warning(sprintf("Google Scholar profile fetch failed (may be rate limited): %s", e$message))
    return(NULL)
  })
}

#' Get citation history from Google Scholar
#'
#' @param scholar_id Google Scholar user ID
#' @return Data frame with yearly citations or NULL
get_scholar_citation_history <- function(scholar_id) {
  if (is.null(scholar_id) || is.na(scholar_id) || scholar_id == "") {
    return(NULL)
  }

  # Clean scholar ID
  scholar_id <- gsub(".*user=", "", scholar_id)
  scholar_id <- gsub("&.*", "", scholar_id)
  scholar_id <- trimws(scholar_id)

  if (scholar_id == "" || nchar(scholar_id) < 5) {
    return(NULL)
  }

  tryCatch({
    history <- scholar::get_citation_history(scholar_id)

    if (is.null(history) || nrow(history) == 0) {
      return(NULL)
    }

    # Standardize column names
    names(history) <- c("year", "citations")
    history$source <- "scholar"

    return(history)

  }, error = function(e) {
    warning(sprintf("Google Scholar citation history failed: %s", e$message))
    return(NULL)
  })
}

#' Get publications from Google Scholar
#'
#' @param scholar_id Google Scholar user ID
#' @param limit Maximum publications to fetch
#' @return Data frame of publications or NULL
get_scholar_publications <- function(scholar_id, limit = 100) {
  if (is.null(scholar_id) || is.na(scholar_id) || scholar_id == "") {
    return(NULL)
  }

  # Clean scholar ID
  scholar_id <- gsub(".*user=", "", scholar_id)
  scholar_id <- gsub("&.*", "", scholar_id)
  scholar_id <- trimws(scholar_id)

  if (scholar_id == "" || nchar(scholar_id) < 5) {
    return(NULL)
  }

  tryCatch({
    pubs <- scholar::get_publications(scholar_id)

    if (is.null(pubs) || nrow(pubs) == 0) {
      return(NULL)
    }

    # Limit results
    if (nrow(pubs) > limit) {
      pubs <- head(pubs, limit)
    }

    # Standardize output
    result <- data.frame(
      title = pubs$title,
      author = pubs$author,
      journal = pubs$journal,
      year = pubs$year,
      cites = pubs$cites,
      source = "scholar",
      stringsAsFactors = FALSE
    )

    return(result)

  }, error = function(e) {
    warning(sprintf("Google Scholar publications fetch failed: %s", e$message))
    return(NULL)
  })
}

# =============================================================================
# ORCID Functions (Free, no API key required)
# =============================================================================

#' Clean and normalize an ORCID iD
#'
#' @param orcid_id Raw ORCID iD (may include URL prefix)
#' @return Cleaned ORCID iD in xxxx-xxxx-xxxx-xxxx format, or NULL
clean_orcid_id <- function(orcid_id) {
  if (is.null(orcid_id) || is.na(orcid_id) || orcid_id == "") {
    return(NULL)
  }
  # Strip URL prefix if present
  orcid_id <- gsub("^https?://orcid\\.org/", "", trimws(as.character(orcid_id)))
  # Validate format
  if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]$", orcid_id)) {
    return(NULL)
  }
  return(orcid_id)
}

#' Get OpenAlex author record by ORCID iD
#'
#' OpenAlex stores ORCID as an external ID, enabling direct lookup.
#'
#' @param orcid_id ORCID iD (xxxx-xxxx-xxxx-xxxx or URL form)
#' @return Data frame with author info or NULL
get_openalex_by_orcid <- function(orcid_id) {
  orcid_id <- clean_orcid_id(orcid_id)
  if (is.null(orcid_id)) {
    return(NULL)
  }

  tryCatch({
    authors <- openalexR::oa_fetch(
      entity = "authors",
      identifier = paste0("orcid:", orcid_id)
    )

    if (is.null(authors) || nrow(authors) == 0) {
      return(NULL)
    }

    h_index <- NA_integer_
    i10_index <- NA_integer_
    if ("summary_stats" %in% names(authors) &&
        !is.null(authors$summary_stats) &&
        length(authors$summary_stats) >= 1) {
      stats <- authors$summary_stats[[1]]
      if (is.list(stats)) {
        if (!is.null(stats$h_index)) h_index <- as.integer(stats$h_index)
        if (!is.null(stats$i10_index)) i10_index <- as.integer(stats$i10_index)
      }
    }

    data.frame(
      openalex_id = authors$id[1],
      display_name = authors$display_name[1],
      works_count = authors$works_count[1],
      cited_by_count = authors$cited_by_count[1],
      h_index = h_index,
      i10_index = i10_index,
      affiliation = {
        inst <- authors$last_known_institution[[1]]
        if (is.null(inst) || length(inst) == 0) NA_character_
        else if (is.data.frame(inst)) inst$display_name[1]
        else if (is.list(inst)) inst$display_name[1]
        else NA_character_
      },
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(sprintf("OpenAlex lookup by ORCID failed: %s", e$message))
    return(NULL)
  })
}

#' Get author profile from ORCID public API
#'
#' @param orcid_id ORCID iD (xxxx-xxxx-xxxx-xxxx or URL form)
#' @return List with profile data or NULL
get_orcid_profile <- function(orcid_id) {
  orcid_id <- clean_orcid_id(orcid_id)
  if (is.null(orcid_id)) {
    return(NULL)
  }

  base_url <- "https://pub.orcid.org/v3.0"

  tryCatch({
    url <- paste0(base_url, "/", orcid_id, "/record")
    resp <- httr::GET(url, httr::accept("application/json"),
                      httr::add_headers("User-Agent" = "FacultyIQ/1.0"))

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")

    if (is.null(content)) {
      return(NULL)
    }

    # Extract name
    name_data <- content$person$name
    given <- name_data$`given-names`$value
    family <- name_data$`family-name`$value
    display_name <- paste(given, family)

    # Extract current affiliation
    employments <- content$`activities-summary`$employments$`affiliation-group`
    current_org <- NA_character_
    if (!is.null(employments) && length(employments) > 0) {
      first_emp <- employments[[1]]$summaries[[1]]$`employment-summary`
      if (!is.null(first_emp$organization$name)) {
        current_org <- first_emp$organization$name
      }
    }

    list(
      orcid_id = orcid_id,
      display_name = display_name,
      current_affiliation = current_org,
      source = "orcid"
    )

  }, error = function(e) {
    warning(sprintf("ORCID profile fetch failed: %s", e$message))
    return(NULL)
  })
}

#' Get verified works list from ORCID public API
#'
#' @param orcid_id ORCID iD (xxxx-xxxx-xxxx-xxxx or URL form)
#' @param limit Maximum works to return
#' @return Data frame with DOIs of researcher-verified works, or NULL
get_orcid_works <- function(orcid_id, limit = 100) {
  orcid_id <- clean_orcid_id(orcid_id)
  if (is.null(orcid_id)) {
    return(NULL)
  }

  base_url <- "https://pub.orcid.org/v3.0"

  tryCatch({
    url <- paste0(base_url, "/", orcid_id, "/works")
    resp <- httr::GET(url, httr::accept("application/json"),
                      httr::add_headers("User-Agent" = "FacultyIQ/1.0"))

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")

    if (is.null(content) || is.null(content$group) || length(content$group) == 0) {
      return(NULL)
    }

    groups <- head(content$group, limit)

    dois <- vapply(groups, function(g) {
      summaries <- g$`work-summary`
      if (is.null(summaries) || length(summaries) == 0) return(NA_character_)
      ext_ids <- summaries[[1]]$`external-ids`$`external-id`
      if (is.null(ext_ids)) return(NA_character_)
      doi_entry <- Filter(function(x) x$`external-id-type` == "doi", ext_ids)
      if (length(doi_entry) == 0) return(NA_character_)
      as.character(doi_entry[[1]]$`external-id-value`)
    }, FUN.VALUE = character(1))

    titles <- vapply(groups, function(g) {
      summaries <- g$`work-summary`
      if (is.null(summaries) || length(summaries) == 0) return(NA_character_)
      title <- summaries[[1]]$title$title$value
      if (is.null(title)) return(NA_character_)
      as.character(title)
    }, FUN.VALUE = character(1))

    years <- vapply(groups, function(g) {
      summaries <- g$`work-summary`
      if (is.null(summaries) || length(summaries) == 0) return(NA_integer_)
      yr <- summaries[[1]]$`publication-date`$year$value
      if (is.null(yr)) return(NA_integer_)
      as.integer(yr)
    }, FUN.VALUE = integer(1))

    data.frame(
      title = titles,
      doi = dois,
      year = years,
      source = "orcid",
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(sprintf("ORCID works fetch failed: %s", e$message))
    return(NULL)
  })
}

# =============================================================================
# Grant Funding Sources (NIH Reporter + NSF Awards, free, no API key required)
# =============================================================================

#' Search NIH Reporter for grants by PI name
#'
#' @param name PI name to search
#' @param from_year Start fiscal year (NULL for all years)
#' @param to_year End fiscal year (NULL for current)
#' @param limit Maximum results
#' @return Data frame of grants or NULL
search_nih_grants_by_name <- function(name, from_year = NULL, to_year = NULL, limit = 50) {
  if (is.null(name) || is.na(name) || name == "") {
    return(NULL)
  }

  base_url <- "https://api.reporter.nih.gov/v2/projects/search"

  # Build request body
  body <- list(
    criteria = list(
      pi_names = list(list(any_name = name))
    ),
    offset = 0,
    limit = min(limit, 500),
    sort_field = "fiscal_year",
    sort_order = "desc"
  )

  if (!is.null(from_year)) {
    body$criteria$fiscal_years <- as.integer(seq(
      from_year,
      ifelse(is.null(to_year), as.integer(format(Sys.Date(), "%Y")), to_year)
    ))
  }

  tryCatch({
    resp <- httr::POST(
      base_url,
      httr::content_type_json(),
      httr::add_headers("User-Agent" = "FacultyIQ/1.0"),
      body = jsonlite::toJSON(body, auto_unbox = TRUE)
    )

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")

    if (is.null(content$results) || length(content$results) == 0) {
      return(NULL)
    }

    results <- content$results

    data.frame(
      grant_id = vapply(results, function(r) as.character(r$project_num %||% NA_character_), character(1)),
      title = vapply(results, function(r) as.character(r$project_title %||% NA_character_), character(1)),
      agency = vapply(results, function(r) as.character(r$agency_ic_admin %||% "NIH"), character(1)),
      mechanism = vapply(results, function(r) as.character(r$activity_code %||% NA_character_), character(1)),
      fiscal_year = vapply(results, function(r) as.integer(r$fiscal_year %||% NA_integer_), integer(1)),
      funding_amount = vapply(results, function(r) as.numeric(r$award_amount %||% NA_real_), numeric(1)),
      is_active = vapply(results, function(r) {
        fy <- r$fiscal_year
        if (is.null(fy)) return(FALSE)
        as.integer(fy) >= as.integer(format(Sys.Date(), "%Y")) - 1L
      }, logical(1)),
      source = "nih_reporter",
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(sprintf("NIH Reporter search failed: %s", e$message))
    return(NULL)
  })
}

#' Get NIH grants for a PI (wrapper with deduplication)
#'
#' @param name PI full name
#' @param from_year Start fiscal year (defaults to 2000)
#' @return Standardized data frame of grants or NULL
get_nih_grants_by_pi <- function(name, from_year = 2000) {
  grants <- search_nih_grants_by_name(name, from_year = from_year)

  if (is.null(grants) || nrow(grants) == 0) {
    return(NULL)
  }

  # Deduplicate by grant_id (same grant can appear in multiple fiscal years)
  grants <- grants[!duplicated(grants$grant_id) | is.na(grants$grant_id), ]

  return(grants)
}

#' Search NSF Award database by PI name
#'
#' @param name PI name to search
#' @param from_year Start year (NULL for all years)
#' @param to_year End year (NULL for current)
#' @param limit Maximum results
#' @return Data frame of awards or NULL
search_nsf_awards_by_name <- function(name, from_year = NULL, to_year = NULL, limit = 50) {
  if (is.null(name) || is.na(name) || name == "") {
    return(NULL)
  }

  base_url <- "https://www.research.gov/awardapi-service/v1/awards.json"

  params <- list(
    pdPIName = name,
    printFields = "id,title,agency,date,startDate,expDate,fundsObligatedAmt,abstractText",
    offset = "1"
  )

  if (!is.null(from_year)) {
    params$dateStart <- paste0("01/01/", from_year)
  }
  if (!is.null(to_year)) {
    params$dateEnd <- paste0("12/31/", to_year)
  }

  tryCatch({
    resp <- httr::GET(
      base_url,
      query = params,
      httr::add_headers("User-Agent" = "FacultyIQ/1.0")
    )

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")

    awards <- content$response$award
    if (is.null(awards) || length(awards) == 0) {
      return(NULL)
    }

    awards <- head(awards, limit)

    parse_nsf_date <- function(d) {
      if (is.null(d) || d == "") return(NA_character_)
      as.character(d)
    }

    is_active_nsf <- function(exp_date_str) {
      if (is.null(exp_date_str) || exp_date_str == "") return(FALSE)
      tryCatch({
        exp <- as.Date(exp_date_str, "%m/%d/%Y")
        !is.na(exp) && exp >= Sys.Date()
      }, error = function(e) FALSE)
    }

    data.frame(
      grant_id = vapply(awards, function(a) as.character(a$id %||% NA_character_), character(1)),
      title = vapply(awards, function(a) as.character(a$title %||% NA_character_), character(1)),
      agency = rep("NSF", length(awards)),
      start_date = vapply(awards, function(a) parse_nsf_date(a$startDate), character(1)),
      end_date = vapply(awards, function(a) parse_nsf_date(a$expDate), character(1)),
      funding_amount = vapply(awards, function(a) {
        suppressWarnings(as.numeric(a$fundsObligatedAmt %||% NA_real_))
      }, numeric(1)),
      is_active = vapply(awards, function(a) is_active_nsf(a$expDate), logical(1)),
      source = "nsf_awards",
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(sprintf("NSF Award search failed: %s", e$message))
    return(NULL)
  })
}

#' Get NSF awards for a PI (wrapper)
#'
#' @param name PI full name
#' @param from_year Start year (defaults to 2000)
#' @return Standardized data frame of awards or NULL
get_nsf_awards_by_pi <- function(name, from_year = 2000) {
  get_nsf_awards_by_pi_raw <- search_nsf_awards_by_name(name, from_year = from_year)

  if (is.null(get_nsf_awards_by_pi_raw) || nrow(get_nsf_awards_by_pi_raw) == 0) {
    return(NULL)
  }

  # Deduplicate by grant_id
  get_nsf_awards_by_pi_raw[!duplicated(get_nsf_awards_by_pi_raw$grant_id) |
                              is.na(get_nsf_awards_by_pi_raw$grant_id), ]
}

# Null-coalescing helper (used in grant parsing)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =============================================================================
# CrossRef Functions (Free, polite pool with email)
# =============================================================================

#' Set up CrossRef polite pool configuration
#'
#' @param email Email for polite pool (optional)
#' @return TRUE if email was set, FALSE otherwise
setup_crossref <- function(email = NULL) {
  if (is.null(email) || email == "") {
    email <- Sys.getenv("CROSSREF_EMAIL", "")
  }
  if (email != "") {
    Sys.setenv(CROSSREF_EMAIL = email)
    return(TRUE)
  }
  return(FALSE)
}

#' Get metadata for a single work from CrossRef by DOI
#'
#' @param doi DOI string (with or without https://doi.org/ prefix)
#' @return List with metadata or NULL
get_crossref_work_metadata <- function(doi) {
  if (is.null(doi) || is.na(doi) || doi == "") {
    return(NULL)
  }

  # Clean DOI: strip URL prefix
  doi <- gsub("^https?://doi\\.org/", "", trimws(doi))

  base_url <- "https://api.crossref.org/works"
  email <- Sys.getenv("CROSSREF_EMAIL", "")
  mailto_param <- if (email != "") paste0("?mailto=", email) else ""

  tryCatch({
    url <- paste0(base_url, "/", utils::URLencode(doi, reserved = TRUE), mailto_param)
    resp <- httr::GET(url, httr::add_headers("User-Agent" = "FacultyIQ/1.0"))

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")
    item <- content$message

    if (is.null(item)) {
      return(NULL)
    }

    # Extract funders
    funders <- item$funder
    funder_names <- if (!is.null(funders) && length(funders) > 0) {
      paste(vapply(funders, function(f) as.character(f$name %||% ""), character(1)), collapse = "; ")
    } else {
      NA_character_
    }

    # Extract license URL
    licenses <- item$license
    license_url <- if (!is.null(licenses) && length(licenses) > 0) {
      as.character(licenses[[1]]$URL %||% NA_character_)
    } else {
      NA_character_
    }

    list(
      doi = doi,
      reference_count = as.integer(item$`reference-count` %||% NA_integer_),
      is_referenced_by_count = as.integer(item$`is-referenced-by-count` %||% NA_integer_),
      funder_names = funder_names,
      has_funding = !is.null(funders) && length(funders) > 0,
      license_url = license_url,
      source = "crossref"
    )

  }, error = function(e) {
    warning(sprintf("CrossRef DOI lookup failed for %s: %s", doi, e$message))
    return(NULL)
  })
}

#' Enrich a works data frame with CrossRef metadata
#'
#' Looks up the top-cited works by DOI to add reference counts and funding info.
#'
#' @param works_df Data frame of works (must have `doi` and `cited_by_count` columns)
#' @param limit Maximum works to enrich (to control API usage)
#' @return works_df with added columns: crossref_reference_count, crossref_funder_names,
#'   crossref_has_funding, crossref_license_url
enrich_works_with_crossref <- function(works_df, limit = 50) {
  if (is.null(works_df) || nrow(works_df) == 0) {
    return(works_df)
  }

  # Add empty enrichment columns
  works_df$crossref_reference_count <- NA_integer_
  works_df$crossref_funder_names <- NA_character_
  works_df$crossref_has_funding <- NA
  works_df$crossref_license_url <- NA_character_

  # Only enrich works with DOIs, sorted by citation count descending
  has_doi <- !is.na(works_df$doi) & works_df$doi != ""
  if (!any(has_doi)) {
    return(works_df)
  }

  enrichable <- which(has_doi)
  if ("cited_by_count" %in% names(works_df)) {
    enrichable <- enrichable[order(works_df$cited_by_count[enrichable], decreasing = TRUE)]
  }
  enrichable <- head(enrichable, limit)

  for (idx in enrichable) {
    meta <- get_crossref_work_metadata(works_df$doi[idx])
    if (!is.null(meta)) {
      works_df$crossref_reference_count[idx] <- meta$reference_count
      works_df$crossref_funder_names[idx] <- meta$funder_names
      works_df$crossref_has_funding[idx] <- meta$has_funding
      works_df$crossref_license_url[idx] <- meta$license_url
    }
    Sys.sleep(0.02)  # Polite pool: ~50 req/sec
  }

  return(works_df)
}

# =============================================================================
# Semantic Scholar Functions (Free, no API key required for basic use)
# =============================================================================

#' Search for authors in Semantic Scholar
#'
#' @param name Author name to search
#' @param limit Maximum results to return
#' @return Data frame of matching authors or NULL
search_semantic_scholar_authors <- function(name, limit = 10) {
  if (is.null(name) || is.na(name) || name == "") {
    return(NULL)
  }

  base_url <- "https://api.semanticscholar.org/graph/v1/author/search"
  fields <- "authorId,name,affiliations,paperCount,citationCount,hIndex"

  tryCatch({
    resp <- httr::GET(
      base_url,
      query = list(query = name, fields = fields, limit = min(limit, 100)),
      httr::add_headers("User-Agent" = "FacultyIQ/1.0")
    )

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")

    if (is.null(content$data) || length(content$data) == 0) {
      return(NULL)
    }

    authors <- content$data

    data.frame(
      s2_author_id = vapply(authors, function(a) as.character(a$authorId %||% NA_character_), character(1)),
      display_name = vapply(authors, function(a) as.character(a$name %||% NA_character_), character(1)),
      affiliation = vapply(authors, function(a) {
        affs <- a$affiliations
        if (is.null(affs) || length(affs) == 0) return(NA_character_)
        as.character(affs[[1]] %||% NA_character_)
      }, character(1)),
      paper_count = vapply(authors, function(a) as.integer(a$paperCount %||% NA_integer_), integer(1)),
      citation_count = vapply(authors, function(a) as.integer(a$citationCount %||% NA_integer_), integer(1)),
      h_index = vapply(authors, function(a) as.integer(a$hIndex %||% NA_integer_), integer(1)),
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(sprintf("Semantic Scholar author search failed: %s", e$message))
    return(NULL)
  })
}

#' Get author profile from Semantic Scholar
#'
#' @param s2_author_id Semantic Scholar author ID
#' @return List with author profile or NULL
get_semantic_scholar_author <- function(s2_author_id) {
  if (is.null(s2_author_id) || is.na(s2_author_id) || s2_author_id == "") {
    return(NULL)
  }

  s2_author_id <- trimws(as.character(s2_author_id))
  base_url <- "https://api.semanticscholar.org/graph/v1/author"
  fields <- "authorId,name,affiliations,paperCount,citationCount,hIndex"

  tryCatch({
    url <- paste0(base_url, "/", s2_author_id)
    resp <- httr::GET(
      url,
      query = list(fields = fields),
      httr::add_headers("User-Agent" = "FacultyIQ/1.0")
    )

    if (httr::http_error(resp)) {
      return(NULL)
    }

    a <- httr::content(resp, as = "parsed", type = "application/json")

    if (is.null(a$authorId)) {
      return(NULL)
    }

    list(
      s2_author_id = as.character(a$authorId),
      display_name = as.character(a$name %||% NA_character_),
      paper_count = as.integer(a$paperCount %||% NA_integer_),
      citation_count = as.integer(a$citationCount %||% NA_integer_),
      h_index = as.integer(a$hIndex %||% NA_integer_),
      source = "semantic_scholar"
    )

  }, error = function(e) {
    warning(sprintf("Semantic Scholar author fetch failed: %s", e$message))
    return(NULL)
  })
}

#' Get papers with influential citation counts from Semantic Scholar
#'
#' @param s2_author_id Semantic Scholar author ID
#' @param limit Maximum papers to fetch
#' @return Data frame of papers with influentialCitationCount or NULL
get_semantic_scholar_papers <- function(s2_author_id, limit = 100) {
  if (is.null(s2_author_id) || is.na(s2_author_id) || s2_author_id == "") {
    return(NULL)
  }

  s2_author_id <- trimws(as.character(s2_author_id))
  base_url <- "https://api.semanticscholar.org/graph/v1/author"
  fields <- "title,year,citationCount,influentialCitationCount,externalIds"

  tryCatch({
    url <- paste0(base_url, "/", s2_author_id, "/papers")
    resp <- httr::GET(
      url,
      query = list(fields = fields, limit = min(limit, 1000)),
      httr::add_headers("User-Agent" = "FacultyIQ/1.0")
    )

    if (httr::http_error(resp)) {
      return(NULL)
    }

    content <- httr::content(resp, as = "parsed", type = "application/json")

    if (is.null(content$data) || length(content$data) == 0) {
      return(NULL)
    }

    papers <- content$data

    data.frame(
      title = vapply(papers, function(p) as.character(p$title %||% NA_character_), character(1)),
      year = vapply(papers, function(p) as.integer(p$year %||% NA_integer_), integer(1)),
      citation_count = vapply(papers, function(p) as.integer(p$citationCount %||% 0L), integer(1)),
      influential_citation_count = vapply(papers, function(p) {
        as.integer(p$influentialCitationCount %||% 0L)
      }, integer(1)),
      doi = vapply(papers, function(p) {
        ext <- p$externalIds
        if (is.null(ext) || is.null(ext$DOI)) return(NA_character_)
        as.character(ext$DOI)
      }, character(1)),
      source = "semantic_scholar",
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(sprintf("Semantic Scholar papers fetch failed: %s", e$message))
    return(NULL)
  })
}

# =============================================================================
# Bibliometrix Integration (For file imports)
# =============================================================================

#' Parse bibliographic file using bibliometrix
#'
#' @param file_path Path to bibliographic export file
#' @param format Format hint ("scopus", "wos", "pubmed", "auto")
#' @return bibliometrix data frame or NULL
parse_bibliographic_file <- function(file_path, format = "auto") {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  tryCatch({
    # bibliometrix::convert2df handles multiple formats
    M <- bibliometrix::convert2df(
      file = file_path,
      dbsource = if (format == "auto") NULL else format,
      format = "plaintext"  # or "bibtex" depending on file
    )

    if (is.null(M) || nrow(M) == 0) {
      return(NULL)
    }

    return(M)

  }, error = function(e) {
    warning(sprintf("Bibliometric file parsing failed: %s", e$message))
    return(NULL)
  })
}

#' Compute bibliometric analysis from imported data
#'
#' @param M bibliometrix data frame
#' @return List with analysis results
analyze_bibliographic_data <- function(M) {
  if (is.null(M) || nrow(M) == 0) {
    return(NULL)
  }

  tryCatch({
    # Basic bibliometric analysis
    results <- bibliometrix::biblioAnalysis(M, sep = ";")

    list(
      n_documents = results$nDocs,
      n_authors = results$nAuthors,
      n_sources = results$nSources,
      timespan = paste(min(results$Years, na.rm = TRUE),
                       max(results$Years, na.rm = TRUE), sep = " - "),
      documents_per_year = results$AnnualProduction,
      most_productive_authors = head(results$Authors, 10),
      most_cited_papers = head(results$MostCitedPapers, 10),
      source = "bibliometrix"
    )

  }, error = function(e) {
    warning(sprintf("Bibliometric analysis failed: %s", e$message))
    return(NULL)
  })
}

# =============================================================================
# Combined/Layered Data Fetching
# =============================================================================

#' Fetch all available data for a person using layered approach
#'
#' @param person List/row with person info (name, scopus_id, scholar_id, openalex_id,
#'   orcid_id, semantic_scholar_id)
#' @param cache_dir Cache directory
#' @param cache_expiry_days Cache expiration
#' @return List with combined data from all sources
fetch_person_data <- function(person, cache_dir = "cache", cache_expiry_days = 7) {
  result <- list(
    name = person$name,
    openalex = NULL,
    scopus = NULL,
    scholar = NULL,
    orcid = NULL,
    nih_grants = NULL,
    nsf_grants = NULL,
    semantic_scholar = NULL,
    works = NULL,
    data_sources = character(),
    errors = character()
  )

  # Generate cache key based on available IDs
  cache_key <- make_cache_key(
    "person",
    person$openalex_id,
    person$scopus_id,
    person$scholar_id,
    person$orcid_id,
    person$semantic_scholar_id,
    person$name
  )

  # Check cache first
  cached <- cache_get(cache_key, cache_expiry_days, cache_dir)
  if (!is.null(cached)) {
    return(cached)
  }

  # Layer 1a: OpenAlex via direct ID (primary, free)
  if (!is.null(person$openalex_id) && !is.na(person$openalex_id) && person$openalex_id != "") {
    tryCatch({
      result$openalex <- get_openalex_author(person$openalex_id)
      result$works <- get_openalex_works(person$openalex_id)
      if (!is.null(result$openalex)) {
        result$data_sources <- c(result$data_sources, "OpenAlex")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("OpenAlex:", e$message))
    })
  }

  # Layer 1b: OpenAlex via ORCID (if no OpenAlex ID yet)
  if (is.null(result$openalex) &&
      !is.null(person$orcid_id) && !is.na(person$orcid_id) && person$orcid_id != "") {
    tryCatch({
      oa_from_orcid <- get_openalex_by_orcid(person$orcid_id)
      if (!is.null(oa_from_orcid)) {
        result$openalex <- get_openalex_author(oa_from_orcid$openalex_id)
        result$works <- get_openalex_works(oa_from_orcid$openalex_id)
        if (!is.null(result$openalex)) {
          result$data_sources <- c(result$data_sources, "OpenAlex (via ORCID)")
        }
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("ORCID→OpenAlex:", e$message))
    })
  }

  # Layer 1c: ORCID profile (verified affiliation and works list)
  if (!is.null(person$orcid_id) && !is.na(person$orcid_id) && person$orcid_id != "") {
    tryCatch({
      result$orcid <- get_orcid_profile(person$orcid_id)
      if (!is.null(result$orcid)) {
        result$data_sources <- c(result$data_sources, "ORCID")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("ORCID:", e$message))
    })
  }

  # Layer 2: Scopus (optional, requires key)
  if (scopus_is_configured() &&
      !is.null(person$scopus_id) && !is.na(person$scopus_id) && person$scopus_id != "") {
    tryCatch({
      result$scopus <- get_scopus_author_metrics(person$scopus_id)
      if (!is.null(result$scopus)) {
        result$data_sources <- c(result$data_sources, "Scopus")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("Scopus:", e$message))
    })
  }

  # Layer 3: Google Scholar (optional, fragile)
  if (!is.null(person$scholar_id) && !is.na(person$scholar_id) && person$scholar_id != "") {
    tryCatch({
      Sys.sleep(2)  # Rate limiting for Scholar
      result$scholar <- get_scholar_profile(person$scholar_id)
      if (!is.null(result$scholar)) {
        result$data_sources <- c(result$data_sources, "Google Scholar")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("Scholar:", e$message))
    })
  }

  # Layer 4: NIH Reporter grants (free, search by name)
  if (!is.null(person$name) && !is.na(person$name) && person$name != "") {
    tryCatch({
      Sys.sleep(1 / 7)  # Rate limiting: 7 req/sec
      result$nih_grants <- get_nih_grants_by_pi(person$name)
      if (!is.null(result$nih_grants) && nrow(result$nih_grants) > 0) {
        result$data_sources <- c(result$data_sources, "NIH Reporter")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("NIH Reporter:", e$message))
    })
  }

  # Layer 5: NSF Awards (free, search by name)
  if (!is.null(person$name) && !is.na(person$name) && person$name != "") {
    tryCatch({
      Sys.sleep(1 / 5)  # Rate limiting: 5 req/sec
      result$nsf_grants <- get_nsf_awards_by_pi(person$name)
      if (!is.null(result$nsf_grants) && nrow(result$nsf_grants) > 0) {
        result$data_sources <- c(result$data_sources, "NSF Awards")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("NSF Awards:", e$message))
    })
  }

  # Layer 6: CrossRef enrichment of existing works (uses DOIs from OpenAlex)
  if (!is.null(result$works) && nrow(result$works) > 0) {
    tryCatch({
      result$works <- enrich_works_with_crossref(result$works, limit = 50)
      if (any(!is.na(result$works$crossref_has_funding))) {
        result$data_sources <- c(result$data_sources, "CrossRef")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("CrossRef:", e$message))
    })
  }

  # Layer 7: Semantic Scholar (optional, requires ID due to name ambiguity)
  if (!is.null(person$semantic_scholar_id) && !is.na(person$semantic_scholar_id) &&
      person$semantic_scholar_id != "") {
    tryCatch({
      Sys.sleep(1)  # Rate limiting: 1 req/sec unauthenticated
      result$semantic_scholar <- get_semantic_scholar_author(person$semantic_scholar_id)
      if (!is.null(result$semantic_scholar)) {
        s2_papers <- get_semantic_scholar_papers(person$semantic_scholar_id)
        result$semantic_scholar$papers <- s2_papers
        result$data_sources <- c(result$data_sources, "Semantic Scholar")
      }
    }, error = function(e) {
      result$errors <- c(result$errors, paste("Semantic Scholar:", e$message))
    })
  }

  # Cache result
  cache_set(cache_key, result, cache_dir)

  return(result)
}

#' Fetch data for multiple people with progress
#'
#' @param roster Data frame with roster
#' @param progress_callback Function to call with progress updates
#' @param cache_dir Cache directory
#' @return List of person data
fetch_all_person_data <- function(roster, progress_callback = NULL, cache_dir = "cache") {
  results <- list()

  for (i in seq_len(nrow(roster))) {
    person <- as.list(roster[i, ])

    if (!is.null(progress_callback)) {
      progress_callback(i, nrow(roster), person$name)
    }

    results[[i]] <- fetch_person_data(person, cache_dir)
    results[[i]]$roster_id <- person$id

    # Small delay to be polite to APIs
    Sys.sleep(0.5)
  }

  return(results)
}
