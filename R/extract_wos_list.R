#' extract the metadata from the new references from Web of Science
#' based on the search strings found in search_list.txt
#' @param search_list_path path to search_list
#' @param directory Choose the directory in which the references
#'  identification history will be saved.
#' @param run_id Identifier shared by all the rows of one retrieval in
#'  "run_log.csv" (created if \code{NULL}).
#' @param commit_history If \code{TRUE} (default), the references
#'  identification history and the run log are written before returning.
#'  If \code{FALSE}, nothing is written: the pending history update and the
#'  run log rows are attached to the returned data frame (attributes
#'  \code{pending_history} and \code{run_log}) so that the caller can write
#'  them once the output file exists (used by [manual_fetch()]).
#' @return A data.frame with one row per retrieved Web of Science record and columns:
#' \describe{
#'  \item{author}{Character. Publication authors.}
#'  \item{year}{Character. Publication year.}
#'  \item{title}{Character. Publication title.}
#'  \item{journal}{Character. Publication journal name.}
#'  \item{volume}{Character. Publication journal volume.}
#'  \item{number}{Character. Publication journal issue number.}
#'  \item{abstract}{Character. Publication abstract.}
#'  \item{doi}{Character. Publication DOI, or article URL when DOI is unavailable.}
#'  \item{pages}{Character. Publication page range (e.g. "179-192").}
#'  \item{isbn}{Character. ISBN for book chapters (NA for journal articles).}
#'  \item{source}{Character. Data source.}
#'  \item{platform_id}{Character. Publication unique identifier in data source.}
#' }
#' If \code{search_list_path} does not exist, returns \code{NULL}.
#' If any search fails (or the API key is missing), an error of class
#' \code{litfetchr_platform_error} is raised and nothing is added to the history.
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_wos_list <- function(search_list_path, directory,
                             run_id = NULL, commit_history = TRUE) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {
    # Read and validate the file "search_list.txt".
    search_list <- read_search_list(search_list_path)

    # Records extracted by the search step, filtered later to the new IDs.
    wos_store <- new.env(parent = emptyenv())
    wos_store$records <- data.frame()

    run_extraction(
      platform = "wos",
      search_list = search_list,
      directory = directory,
      run_id = run_id,
      commit_history = commit_history,
      search_fn = function(search_list, state) {
        wos_store$records <- search_wos(search_list, state)
      },
      fetch_fn = function(new_ids, state) {
        # Filter pre-extracted records to new IDs only (no additional API calls needed).
        wos_results <- wos_store$records[wos_store$records$platform_id %in%
                                           as.character(new_ids), , drop = FALSE]
        rownames(wos_results) <- NULL
        wos_results
      }
    )
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }

}

# Runs the searches and extracts the full metadata of every record found.
# Returns a data frame with all the records (new or not).
search_wos <- function(search_list, state) {

  # Stores extracted metadata for every record across all batches and queries.
  all_wos_records <- list()
  # Extracts the saved API key for Web of Science.
  wos_api_key <- Sys.getenv("wos_api_key")
  if (!nzchar(wos_api_key)) {
    stop("Web of Science API key not found.
           Set env var `wos_api_key` using the function `save_api_key`.",
         call. = FALSE)
  }

  # Loop to fetch data for each search term
  for (qi in seq_along(search_list)) {
    state$current <- qi
    query_wos <- search_list[[qi]]
    # UIDs found for the current search.
    ids_q <- character(0)
    # Defines at which reference to start the extraction of a batch.
    next_start_wos <- 1
    term_wos <- encode_query(build_platform_query(query_wos, "wos"))

    # Call WoS API to get total results
    base_url_wos <- "https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery="
    search_url_wos <- paste0(base_url_wos, term_wos)
    response_wos <- jsonlite::fromJSON(get_text_retry(search_url_wos,
                                                      headers = c("X-ApiKey" = wos_api_key)
                                                      )
                                       )

    # Gives the number of results from the API call.
    max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)
    if (length(max_result_wos) != 1 || is.na(max_result_wos)) {
      stop("Web of Science returned no result count for: ", query_wos,
           call. = FALSE)
    }
    message(max_result_wos, " total results found on Web of Science for: ", query_wos)

    if (max_result_wos == 0) {
      message("No results found on WoS for the saved seach string.")
      state$queries[[qi]] <- list(total = 0, ids = character(0))
      next
    }

    # Creates the indicator for the number of batches required to get all the references.
    # "100" represents the max number of references per batch.
    imax_wos <- ceiling(max_result_wos / 100)

    # Fetch all records in batches and extract full metadata from each batch response.
    # The search endpoint returns the same complete record structure as the /id/ endpoint,
    # so no separate per-record calls are needed.
    # simplifyDataFrame = FALSE keeps each record as a named list (not a data.frame row),
    # so nested fields like identifiers and abstracts are traversable with purrr::pluck.
    for (i in seq_len(imax_wos)) {
      batch_count <- min(100L, max_result_wos - next_start_wos + 1L)
      search_url_wos <- paste0(
        "https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=",
        term_wos,
        "&count=", batch_count,
        "&firstRecord=", next_start_wos)

      response_wos <- jsonlite::fromJSON(
        get_text_retry(search_url_wos, headers = c("X-ApiKey" = wos_api_key)),
        simplifyDataFrame = FALSE
      )
      next_start_wos <- next_start_wos + 100

      recs_raw <- response_wos$Data$Records$records$REC
      if (is.null(recs_raw) || length(recs_raw) == 0) {
        message("No more results found for ", query_wos)
        break
      }

      # With simplifyDataFrame = FALSE, REC is a list of named lists.
      # When only one record exists the API may return a single object instead of
      # an array; in that case recs_raw has a UID field directly.
      if (!is.null(recs_raw$UID)) {
        recs_list <- list(recs_raw)
      } else {
        recs_list <- recs_raw
      }

      # Collect UIDs for history tracking.
      rec_uids <- vapply(recs_list, function(r) as.character(r$UID)[1],
                         FUN.VALUE = character(1))
      ids_q <- c(ids_q, rec_uids)

      # Extract full metadata for each record in this batch.
      for (j in seq_along(recs_list)) {
        x <- rec_uids[j]
        wos_article <- list(Data = list(Records = list(records = list(REC = recs_list[[j]]))))

        rec_data <- tryCatch({

          # Extracts authors (set to NA if missing).
          names_raw <- purrr::pluck(wos_article,
                         "Data", "Records", "records", "REC",
                         "static_data", "summary", "names", "name",
                         .default = NULL)
          wos_authors <- NA_character_
          if (!is.null(names_raw) && is.list(names_raw)) {
            # Single name element comes back as a named list; wrap it.
            if (!is.null(names_raw$last_name) || !is.null(names_raw$full_name) ||
                !is.null(names_raw$display_name)) {
              names_raw <- list(names_raw)
            }
            last_names <- vapply(names_raw, function(n) {
              if (!is.list(n)) return(NA_character_)
              # Skip patent assignees; keep inventors and all journal-article roles.
              if (identical(n$role, "assignee")) return(NA_character_)
              if (!is.null(n$last_name)) as.character(n$last_name[[1]])
              else if (!is.null(n$full_name)) as.character(n$full_name[[1]])
              else if (!is.null(n$display_name)) as.character(n$display_name[[1]])
              else NA_character_
            }, FUN.VALUE = character(1))
            last_names <- last_names[!is.na(last_names)]
            if (length(last_names) > 0) wos_authors <- paste(last_names, collapse = ", ")
          }

          # Extracts year; falls back to sortdate for patents (no pubyear field).
          wos_year <- as.character(purrr::pluck(wos_article,
                            "Data", "Records", "records", "REC",
                            "static_data", "summary", "pub_info", "pubyear",
                            .default = NA))
          if (is.na(wos_year)) {
            sortdate <- purrr::pluck(wos_article,
                          "Data", "Records", "records", "REC",
                          "static_data", "summary", "pub_info", "sortdate",
                          .default = NULL)
            if (!is.null(sortdate)) wos_year <- substr(as.character(sortdate[[1]]), 1, 4)
          }

          # Extracts title and journal (set to NA if missing).
          titles_raw <- purrr::pluck(wos_article,
                          "Data", "Records", "records", "REC",
                          "static_data", "summary", "titles", "title",
                          .default = NULL)
          wos_title <- NA_character_
          wos_journal <- NA_character_

          if (!is.null(titles_raw) && is.list(titles_raw)) {
            # Single title element comes back as a named list; wrap it.
            if (!is.null(titles_raw$type)) titles_raw <- list(titles_raw)
            item_els <- Filter(function(t) is.list(t) && identical(t$type, "item"),   titles_raw)
            src_els  <- Filter(function(t) is.list(t) && identical(t$type, "source"), titles_raw)
            if (length(item_els) > 0) {
              # content is a vector per element; i is a scalar or short vector.
              # Iterate directly to avoid bind_rows recycling scalars across content rows.
              content_vec <- unlist(lapply(item_els, function(t) t$content))
              italic_vec  <- unlist(lapply(item_els, function(t) if (!is.null(t$i)) t$i else character(0)))
              italic_vec  <- italic_vec[!is.na(italic_vec)]
              if (length(italic_vec) > 0) {
                wos_title <- interleave_italic_content(content_vec, italic_vec)
              } else {
                wos_title <- paste(as.character(content_vec), collapse = " ")
              }
            }
            if (length(src_els) > 0) {
              wos_journal <- as.character(src_els[[1]]$content[[1]])
            } else {
              # Book Citation Index / Zoological Record use type "book" instead of "source".
              book_els <- Filter(function(t) is.list(t) && identical(t$type, "book"), titles_raw)
              if (length(book_els) > 0) {
                wos_journal <- as.character(book_els[[1]]$content[[1]])
              }
            }
          }

          # Extracts volume (set to NA if missing).
          wos_volume <- as.character(purrr::pluck(wos_article,
                                     "Data", "Records", "records", "REC",
                                     "static_data", "summary", "pub_info", "vol",
                                     .default = NA)
          )

          # Extracts abstract (set to NA if missing).
          abstract_raw <- purrr::pluck(wos_article,
                            "Data", "Records", "records", "REC",
                            "static_data", "fullrecord_metadata",
                            "abstracts", "abstract", "abstract_text", "p",
                            .default = NULL)

          wos_abstract <- NA_character_
          if (!is.null(abstract_raw)) {
            if (is.character(abstract_raw)) {
              # Plain character: no markup, just collapse.
              wos_abstract <- paste(abstract_raw, collapse = " ")
            } else if (is.list(abstract_raw)) {
              if (!is.null(abstract_raw$content)) {
                # Single <p> element — content/italic/sub are direct fields.
                content_vec <- unlist(abstract_raw$content)
                italic_vec  <- c(
                  if (!is.null(abstract_raw$italic)) unlist(abstract_raw$italic) else character(0),
                  if (!is.null(abstract_raw$sub))    unlist(abstract_raw$sub)    else character(0)
                )
                italic_vec <- italic_vec[!is.na(italic_vec)]
                if (length(italic_vec) > 0) {
                  wos_abstract <- interleave_italic_content(content_vec, italic_vec)
                } else {
                  wos_abstract <- paste(as.character(content_vec), collapse = " ")
                }
              } else {
                # Array of <p> elements — iterate directly.
                # Some p elements may be plain character strings (no markup); guard all $ accesses.
                content_vec <- unlist(lapply(abstract_raw, function(p) {
                  if (is.character(p)) p
                  else if (is.list(p) && !is.null(p$content)) p$content
                  else character(0)
                }))
                italic_vec  <- c(
                  unlist(lapply(abstract_raw, function(p) if (is.list(p) && !is.null(p$italic)) p$italic else character(0))),
                  unlist(lapply(abstract_raw, function(p) if (is.list(p) && !is.null(p$sub))    p$sub    else character(0)))
                )
                italic_vec <- italic_vec[!is.na(italic_vec)]
                if (length(italic_vec) > 0) {
                  wos_abstract <- interleave_italic_content(content_vec, italic_vec)
                } else if (length(content_vec) > 0) {
                  wos_abstract <- paste(as.character(content_vec), collapse = " ")
                }
              }
            }
          }

          # Multi-language abstract fallback (CSCD, KJD, SCIELO, etc.).
          # These databases return abstracts as a list of {lang_id, abstract_text.p}
          # elements rather than a single element, so the primary pluck returns NULL.
          # Prefer English; fall back to first available language.
          if (is.na(wos_abstract)) {
            abstract_list <- purrr::pluck(wos_article,
                              "Data", "Records", "records", "REC",
                              "static_data", "fullrecord_metadata",
                              "abstracts", "abstract",
                              .default = NULL)
            if (is.list(abstract_list) && length(abstract_list) > 0) {
              # Single element comes back as a named list; wrap it.
              if (!is.null(abstract_list$abstract_text)) abstract_list <- list(abstract_list)
              eng    <- Filter(function(a) is.list(a) && identical(a$lang_id, "en"), abstract_list)
              chosen <- if (length(eng) > 0) eng[[1]] else abstract_list[[1]]
              p <- purrr::pluck(chosen, "abstract_text", "p", .default = NULL)
              if (!is.null(p) && nzchar(trimws(p[[1]]))) {
                wos_abstract <- paste(as.character(p), collapse = " ")
              }
            }
          }

          # Patent abstract fallback: reconstruct from Derwent PhoenixTyp1 sections.
          # PatentTyp1 may be a single named list (one publication) or a list of
          # named lists (multiple A/B versions of the same patent); find the element
          # that carries ContentCorePtTyp1$AbsCTyp1.
          if (is.na(wos_abstract)) {
            patent_item <- purrr::pluck(wos_article,
                             "Data", "Records", "records", "REC",
                             "static_data", "item", "PatentTyp1",
                             .default = NULL)
            content_core <- NULL
            if (!is.null(patent_item)) {
              if (!is.null(patent_item$ContentCorePtTyp1)) {
                content_core <- patent_item$ContentCorePtTyp1
              } else {
                for (el in patent_item) {
                  if (is.list(el) && !is.null(el$ContentCorePtTyp1$AbsCTyp1)) {
                    content_core <- el$ContentCorePtTyp1
                    break
                  }
                }
              }
            }
            phoenix <- purrr::pluck(content_core, "AbsCTyp1", "PhoenixTyp1", "POnline",
                                    .default = NULL)
            if (!is.null(phoenix)) {
              parts <- character(0)
              nov      <- purrr::pluck(phoenix, "PONov", "POP", .default = NULL)
              use_text <- purrr::pluck(phoenix, "POUse", "POP", .default = NULL)
              adv      <- purrr::pluck(phoenix, "POAdv", "POP", .default = NULL)
              if (!is.null(nov))      parts <- c(parts, paste0("NOVELTY - ",   paste(nov,      collapse = " ")))
              if (!is.null(use_text)) parts <- c(parts, paste0("USE - ",       paste(use_text, collapse = " ")))
              if (!is.null(adv))      parts <- c(parts, paste0("ADVANTAGE - ", paste(adv,      collapse = " ")))
              if (length(parts) > 0) wos_abstract <- paste(parts, collapse = " ")
            }
          }

          # Extracts DOI (set to NA if missing).
          identifiers <- purrr::pluck(wos_article,
                               "Data", "Records", "records", "REC",
                               "dynamic_data", "cluster_related",
                               "identifiers", "identifier",
                               .default = NULL)

          # Single identifier returns a named list, not a list-of-lists; wrap it.
          if (!is.null(identifiers) && !is.null(identifiers$type)) {
            identifiers <- list(identifiers)
          }
          identifiers_df <- tryCatch(
            dplyr::bind_rows(lapply(identifiers, function(id) {
              if (!is.list(id)) return(NULL)
              list(type = as.character(id$type), value = as.character(id$value))
            })),
            error = function(e) NULL
          )
          wos_doi <- NA_character_
          if (!is.null(identifiers_df) && nrow(identifiers_df) > 0 &&
              all(c("type", "value") %in% names(identifiers_df))) {
            doi_rows <- identifiers_df[identifiers_df$type %in% c("doi", "xref_doi"), , drop = FALSE]
            if (nrow(doi_rows) > 0) {
              doi_rows$type <- factor(doi_rows$type, levels = c("doi", "xref_doi"))
              doi_rows <- doi_rows[order(doi_rows$type), , drop = FALSE]
              vals <- unlist(doi_rows$value, use.names = FALSE)
              vals <- vals[!is.na(vals)]
              if (length(vals) > 0) wos_doi <- as.character(vals[[1]])
            }
          }

          # Fallback: use article URL from address_spec when no DOI is available (e.g. CABI records).
          if (is.na(wos_doi)) {
            address_name <- purrr::pluck(wos_article,
                                         "Data", "Records", "records", "REC",
                                         "static_data", "fullrecord_metadata",
                                         "addresses", "address_name",
                                         .default = NULL)
            if (!is.null(address_name)) {
              first_url <- tryCatch({
                url_vec <- address_name[[1]]$address_spec$url_spec$url
                if (!is.null(url_vec) && length(url_vec) > 0) as.character(url_vec[[1]]) else NULL
              }, error = function(e) NULL)
              if (!is.null(first_url) && !is.na(first_url)) {
                wos_doi <- first_url
              }
            }
          }

          # Extracts issue (set to NA if missing).
          wos_issue <- as.character(purrr::pluck(wos_article,
                                    "Data", "Records", "records",
                                    "REC", "static_data", "summary",
                                    "pub_info", "issue",
                                    .default = NA)
          )

          # Extracts page range (set to NA if missing).
          wos_pages <- as.character(purrr::pluck(wos_article,
                                    "Data", "Records", "records", "REC",
                                    "static_data", "summary", "pub_info",
                                    "page", "content",
                                    .default = NA))

          # Extracts ISBN (set to NA if missing; reuses identifiers_df from DOI extraction).
          # Looks for both "isbn" and "eisbn" types, preferring "isbn".
          wos_isbn <- NA_character_
          if (!is.null(identifiers_df) && nrow(identifiers_df) > 0 &&
              all(c("type", "value") %in% names(identifiers_df))) {
            isbn_rows <- identifiers_df[identifiers_df$type %in% c("isbn", "eisbn"), , drop = FALSE]
            if (nrow(isbn_rows) > 0) {
              isbn_rows$type <- factor(isbn_rows$type, levels = c("isbn", "eisbn"))
              isbn_rows <- isbn_rows[order(isbn_rows$type), , drop = FALSE]
              vals <- unlist(isbn_rows$value, use.names = FALSE)
              vals <- vals[!is.na(vals)]
              if (length(vals) > 0) wos_isbn <- as.character(vals[[1]])
            }
          }

          data.frame(
            author = wos_authors[1],
            year = wos_year[1],
            title = strip_markup(wos_title[1]),
            journal = wos_journal[1],
            volume = wos_volume[1],
            number = wos_issue[1],
            abstract = strip_markup(wos_abstract[1]),
            doi = wos_doi[1],
            pages = wos_pages[1],
            isbn = wos_isbn[1],
            source = "Web of Science",
            platform_id = x,
            stringsAsFactors = FALSE
          )

        }, error = function(e) {
          message("FAILED UID=", x, " : ", conditionMessage(e))
          # Reported in the run log. The UID stays in the history, since
          # requesting the record again would fail the same way.
          state$unparsed_ids <- c(state$unparsed_ids, x)
          data.frame(
            author = NA_character_, year = NA_character_, title = NA_character_,
            journal = NA_character_, volume = NA_character_, number = NA_character_,
            abstract = NA_character_, doi = NA_character_, pages = NA_character_,
            isbn = NA_character_, source = "Web of Science",
            platform_id = x, stringsAsFactors = FALSE
          )
        })

        all_wos_records[[length(all_wos_records) + 1]] <- rec_data
      }

      message("Finished batch ", i, " of ", imax_wos)
    }

    state$queries[[qi]] <- list(total = max_result_wos, ids = ids_q)

  }

  dplyr::bind_rows(all_wos_records)
}

# Interleaves italic terms back into plain-text content fragments.
# CABI records in the WoS API store abstracts and titles as two parallel arrays:
# `content` (text between italic spans) and `italic`/`i` (the italic terms).
# Which array leads depends on length: if content has more items it comes first,
# otherwise italic comes first (equal → italic first, as observed in the API data).
interleave_italic_content <- function(content_vec, italic_vec) {
  n_c <- length(content_vec)
  n_i <- length(italic_vec)
  result <- character(n_c + n_i)
  if (n_c > n_i) {
    result[seq(1L, by = 2L, length.out = n_c)] <- content_vec
    result[seq(2L, by = 2L, length.out = n_i)] <- italic_vec
  } else {
    result[seq(1L, by = 2L, length.out = n_i)] <- italic_vec
    result[seq(2L, by = 2L, length.out = n_c)] <- content_vec
  }
  trimws(paste(trimws(result), collapse = " "))
}
