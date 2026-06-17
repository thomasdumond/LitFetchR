#' extract the metadata from the new references from Web of Science
#' based on the search strings found in search_list.txt
#' @param search_list_path path to search_list
#' @param directory Choose the directory in which the references
#'  identification history will be saved.
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
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_wos_list <- function(search_list_path, directory) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {
    # Initializes an empty list to store results for all queries.
    dfs_wos_all <- list()
    # Stores extracted metadata for every record across all batches and queries.
    all_wos_records <- list()
    # Extracts the saved API key for Web of Science.
    wos_api_key <- Sys.getenv("wos_api_key")
    if (!nzchar(wos_api_key)) {
      stop("Web of Science API key not found.
           Set env var `wos_api_key` using the function `save_api_key`.",
           call. = FALSE)
    }
    # Read the file "search_list.txt".
    lines <- readLines(search_list_path, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    # Convert file contents into a list of search strings.
    search_list <- stats::setNames(sub("^[^=]+=", "", lines), sub("=.*", "", lines))

    total_results_wos <- 0
    # Loop to fetch data for each search term
    for (query_wos in search_list) {
      # Initializes an empty list for current search.
      dfs_wos <- list()
      # Defines at which reference to start the extraction of a batch.
      next_start_wos <- 1
      search_wos <- utils::URLencode(paste0("TS=(", query_wos, ")"))

      # Call WoS API to get total results
      base_url_wos <- "https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery="
      search_url_wos <- paste0(base_url_wos, search_wos)
      response_wos <- jsonlite::fromJSON(get_text_retry(search_url_wos,
                                                        headers = c("X-ApiKey" = wos_api_key)
                                                        )
                                         )

      # Gives the number of results from the API call.
      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)
      total_results_wos <- total_results_wos + max_result_wos
      message(max_result_wos, " total results found on Web of Science for: ", query_wos)

      if (max_result_wos == 0) {
        message("No results found on WoS for the saved seach string.")
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
          search_wos,
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
        rec_uids <- sapply(recs_list, function(r) as.character(r$UID))
        dfs_wos[[i]] <- data.frame(wos_id = rec_uids)

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

        message("Finished batch number ", i)
      }

      # Combines all unique IDs into one data frame per search string.
      if (length(dfs_wos) > 0) {
        dfs_wos_all[[query_wos]] <- dplyr::bind_rows(dfs_wos)
      }

    }

    # Creates the search history sheet and/or
    # document and saves the unique IDs.
    # If already exists, just extract the data from it.
    sh <- create_id_history(directory)
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    history_id_path <- file.path(directory, "history_id.xlsx")
    # Combines all results into a single dataframe.
    wos_df <- dplyr::bind_rows(dfs_wos_all)
    if (nrow(wos_df) == 0) {
      message("No new record from WoS retrieved.")
      return(data.frame(author = character(), year = character(),
                        title = character(), journal = character(),
                        volume = character(), number = character(),
                        abstract = character(), doi = character(),
                        pages = character(), isbn = character(),
                        source = character(), platform_id = character(),
                        stringsAsFactors = FALSE))
    }
    # Appends raw IDs to the id_log sheet with platform and timestamp.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    existing_log <- openxlsx::readWorkbook(history_id, sheet = "id_log")
    new_log_rows <- data.frame(id = wos_df$wos_id, platform = "Web of Science",
                               timestamp = date_suffix, stringsAsFactors = FALSE)
    openxlsx::writeData(history_id, "id_log", rbind(existing_log, new_log_rows))
    # Makes sure there is no duplicates in the extracted IDs and format as vector.
    wos_id_vec <- unique(wos_df$wos_id)
    # Extract the IDs retrieved previously from the in-memory workbook.
    last_list <- openxlsx::readWorkbook(history_id, sheet = "updated_id_list")
    # Converts the list to a vector.
    last_list_vec <- last_list$id
    # Gets only the new IDs.
    wos_new <- setdiff(wos_id_vec, last_list_vec)
    message(length(wos_new), " new records found among ", total_results_wos, " total results.")
    # Creates a dataframe with only the new IDs.
    wos_new_id <- data.frame(id = wos_new)
    # Adds the new IDs to the current list.
    updated_list <- rbind(last_list, wos_new_id)

    # Filter pre-extracted records to new IDs only (no additional API calls needed).
    wos_new_set <- as.character(wos_new)
    all_wos_records_df <- dplyr::bind_rows(all_wos_records)
    wos_results <- all_wos_records_df[all_wos_records_df$platform_id %in% wos_new_set, , drop = FALSE]
    rownames(wos_results) <- NULL

    # Writes the list updated with the new IDs only after extraction succeeds.
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = updated_list)
    openxlsx::saveWorkbook(history_id,
                           file = history_id_path,
                           overwrite = TRUE)

    wos_results # Returns the dataframe with all the references.
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }

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
