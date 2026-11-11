#' Extracts the metadata from the new references found on Scopus
#' based on the search string(s) saved in "search_list.txt".
#' @param search_list_path Path to "search_list.txt".
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
#' @return A data.frame with one row per retrieved Scopus record and columns:
#' \describe{
#'  \item{author}{Character. Publication authors.}
#'  \item{year}{Character. Publication year.}
#'  \item{title}{Character. Publication title.}
#'  \item{journal}{Character. Publication journal name.}
#'  \item{volume}{Character. Publication journal volume.}
#'  \item{number}{Character. Publication journal issue number.}
#'  \item{abstract}{Character. Publication abstract.}
#'  \item{doi}{Character. Publication Digital Object Identifier (DOI).}
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

extract_scp_list <- function(search_list_path, directory,
                             run_id = NULL, commit_history = TRUE) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {
    # Read and validate the file "search_list.txt".
    search_list <- read_search_list(search_list_path)

    # Records extracted by the search step, filtered later to the new IDs.
    scp_store <- new.env(parent = emptyenv())
    scp_store$records <- data.frame()

    run_extraction(
      platform = "scp",
      search_list = search_list,
      directory = directory,
      run_id = run_id,
      commit_history = commit_history,
      search_fn = function(search_list, state) {
        scp_store$records <- search_scp(search_list, state)
      },
      fetch_fn = function(new_ids, state) {
        # Filter pre-extracted records to new IDs only (no additional API calls needed).
        scopus_results <- scp_store$records[scp_store$records$platform_id %in% new_ids, ,
                                            drop = FALSE]
        rownames(scopus_results) <- NULL
        scopus_results
      }
    )
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }

}

# Safely extract a scalar field from a one-row entry data frame.
scp_field <- function(entry, col) {
  v <- entry[[col]]
  if (is.null(v)) return(NA_character_)
  v1 <- v[[1L]]
  if (is.null(v1) || length(v1) == 0L) return(NA_character_)
  v1 <- v1[[1L]]  # take first element; guards multi-value fields like prism:isbn
  if (is.na(v1)) NA_character_
  else as.character(v1)
}

# Runs the searches and extracts the full metadata of every record found.
# Returns a data frame with all the records (new or not).
search_scp <- function(search_list, state) {

  # Stores full metadata across all queries.
  all_scp_records <- list()
  # Extracts the saved API key for Scopus.
  scp_api_key <- Sys.getenv("scp_api_key")
  if (!nzchar(scp_api_key)) {
    stop("Scopus API key not found.
           Set env var `scp_api_key` using the function `save_api_key`.",
         call. = FALSE)
  }
  scp_insttoken <- Sys.getenv("scp_insttoken")
  scp_headers <- c("X-ELS-APIKey" = scp_api_key,
                   "Accept" = "application/json",
                   if (nzchar(scp_insttoken)) c("X-ELS-Insttoken" = scp_insttoken))

  # Loop to fetch data for each search term
  for (qi in seq_along(search_list)) {
    state$current <- qi
    query_scp <- search_list[[qi]]
    # IDs found for the current search.
    ids_q <- character(0)
    # Defines at which reference to start the extraction of a batch.
    next_start_scp <- 0
    term_scp <- encode_query(build_platform_query(query_scp, "scp"))

    # Call Scopus API to get total results
    # Creates the baseline API URL
    base_url_scp <- "https://api.elsevier.com/content/search/scopus?query="
    # Adds the search information.
    search_url_scp <- paste0(base_url_scp, term_scp)
    # Extracts the content of the corresponding API call.
    response_scp <- jsonlite::fromJSON(
      get_text_retry(search_url_scp, headers = scp_headers),
      flatten = TRUE)

    # Check if the search gives results.
    if (is.null(response_scp$`search-results`$`opensearch:totalResults`)) {
      stop("Scopus returned no result count for: ", query_scp, call. = FALSE)
    }

    # Gives the number of results from the API call.
    max_result_scp <- as.numeric(
      response_scp$`search-results`$`opensearch:totalResults`
      )
    message(max_result_scp, " total results found on Scopus for: ", query_scp)

    if (max_result_scp == 0) {
      message("No results found on Scopus for the saved search string.")
      state$queries[[qi]] <- list(total = 0, ids = character(0))
      next
    }

    # Creates the indicator for the number of batches
    # required to get all the references.
    # "25" represents the max number of references per batch.
    imax_scp <- ceiling(max_result_scp / 25)

    # Fetch all records in batches and extract full metadata from each batch response.
    # view=COMPLETE returns all fields including abstract and full author list,
    # so no separate per-record calls are needed.
    for (i in seq_len(imax_scp)) {
      search_url_scp <- paste0(base_url_scp, term_scp,
                               "&start=", next_start_scp,
                               "&count=25&view=COMPLETE")
      response_scp <- jsonlite::fromJSON(
        get_text_retry(search_url_scp, headers = scp_headers),
        flatten = TRUE)
      next_start_scp <- next_start_scp + 25

      entries <- response_scp$`search-results`$entry
      if (is.null(entries) || !is.data.frame(entries) || nrow(entries) == 0) {
        message("No more results found for ", query_scp)
        break
      }

      # Collect IDs for history tracking.
      ids_q <- c(ids_q, as.character(entries$`dc:identifier`))

      # Extract full metadata for each entry in this batch.
      for (j in seq_len(nrow(entries))) {
        e <- entries[j, , drop = FALSE]
        scp_id <- as.character(e[["dc:identifier"]])

        # Extracts authors (set to NA if missing).
        scp_authors <- tryCatch({
          auth_df <- e$author[[1L]]
          if (is.data.frame(auth_df) && "surname" %in% names(auth_df)) {
            surnames <- auth_df$surname[!is.na(auth_df$surname)]
            if (length(surnames) > 0L) paste(surnames, collapse = ", ")
            else NA_character_
          } else NA_character_
        }, error = function(err) NA_character_)

        # Extracts year from prism:coverDate (set to NA if missing).
        cover_date <- e[["prism:coverDate"]]
        scp_year <- if (!is.null(cover_date) && !is.na(cover_date) &&
                        nchar(as.character(cover_date)) >= 4L) {
          substr(as.character(cover_date), 1L, 4L)
        } else NA_character_

        all_scp_records[[length(all_scp_records) + 1L]] <- data.frame(
          author      = scp_authors,
          year        = scp_year,
          title       = strip_markup(scp_field(e, "dc:title")),
          journal     = scp_field(e, "prism:publicationName"),
          volume      = scp_field(e, "prism:volume"),
          number      = scp_field(e, "prism:issueIdentifier"),
          abstract    = strip_markup(scp_field(e, "dc:description")),
          doi         = scp_field(e, "prism:doi"),
          pages       = scp_field(e, "prism:pageRange"),
          isbn        = scp_field(e, "prism:isbn"),
          source      = "Scopus",
          platform_id = scp_id,
          stringsAsFactors = FALSE
        )
      }

      message("Finished batch ", i, " of ", imax_scp)
    }

    state$queries[[qi]] <- list(total = max_result_scp, ids = ids_q)
  }

  dplyr::bind_rows(all_scp_records)
}
