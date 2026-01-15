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

    # Standardize output format
    result <- data.frame(
      openalex_id = authors$id,
      display_name = authors$display_name,
      works_count = authors$works_count,
      cited_by_count = authors$cited_by_count,
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

    # Return first match
    result <- data.frame(
      openalex_id = authors$id[1],
      display_name = authors$display_name[1],
      works_count = authors$works_count[1],
      cited_by_count = authors$cited_by_count[1],
      affiliation = {
        inst <- authors$last_known_institution[[1]]
        if (is.null(inst) || length(inst) == 0) NA_character_
        else if (is.data.frame(inst)) inst$display_name[1]
        else if (is.list(inst)) inst$display_name
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
        else if (is.list(inst)) inst$display_name
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
    # Build filter
    filter_list <- list(author.id = openalex_id)

    if (!is.null(from_year)) {
      filter_list$from_publication_date <- paste0(from_year, "-01-01")
    }
    if (!is.null(to_year)) {
      filter_list$to_publication_date <- paste0(to_year, "-12-31")
    }

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
#' @param person List/row with person info (name, scopus_id, scholar_id, openalex_id)
#' @param cache_dir Cache directory
#' @param cache_expiry_days Cache expiration
#' @return List with combined data from all sources
fetch_person_data <- function(person, cache_dir = "cache", cache_expiry_days = 7) {
  result <- list(
    name = person$name,
    openalex = NULL,
    scopus = NULL,
    scholar = NULL,
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
    person$name
  )

  # Check cache first
  cached <- cache_get(cache_key, cache_expiry_days, cache_dir)
  if (!is.null(cached)) {
    return(cached)
  }

  # Layer 1: OpenAlex (primary, free)
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
