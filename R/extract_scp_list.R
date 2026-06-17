#' Extracts the metadata from the new references found on Scopus
#' based on the search string(s) saved in "search_list.txt".
#' @param search_list_path Path to "search_list.txt".
#' @param directory Choose the directory in which the references
#'  identification history will be saved.
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
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_scp_list <- function(search_list_path, directory) {

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

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {
    # Initializes empty lists to store IDs and full metadata across all queries.
    dfs_scp_all <- list()
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
    # Read the file "search_list.txt".
    lines <- readLines(search_list_path, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    # Convert file contents into a list of search strings.
    search_list <- stats::setNames(sub("^[^=]+=", "", lines),
                                   sub("=.*", "", lines))

    total_results_scp <- 0
    # Loop to fetch data for each search term
    for (query_scp in search_list) {
      # Initializes an empty list for current search.
      dfs_scp <- list()
      # Defines at which reference to start the extraction of a batch.
      next_start_scp <- 0
      search_scp <- utils::URLencode(paste0("TITLE-ABS-KEY(", query_scp, ")"))

      # Call Scopus API to get total results
      # Creates the baseline API URL
      base_url_scp <- "https://api.elsevier.com/content/search/scopus?query="
      # Adds the search information.
      search_url_scp <- paste0(base_url_scp, search_scp)
      # Extracts the content of the corresponding API call.
      response_scp <- jsonlite::fromJSON(
        get_text_retry(search_url_scp, headers = scp_headers),
        flatten = TRUE)

      # Check if the search gives results.
      if (is.null(response_scp$`search-results`$`opensearch:totalResults`)) {
        message("No results found for ", search_scp)
        next
      }

      # Gives the number of results from the API call.
      max_result_scp <- as.numeric(
        response_scp$`search-results`$`opensearch:totalResults`
        )
      total_results_scp <- total_results_scp + max_result_scp
      message(max_result_scp, " total results found on Scopus for: ", query_scp)

      if (max_result_scp == 0) {
        message("No results found on Scopus for the saved search string.")
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
        search_url_scp <- paste0(base_url_scp, search_scp,
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
        dfs_scp[[i]] <- data.frame(
          scopus_id = as.character(entries$`dc:identifier`))

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

      # Combines all unique IDs into one data frame per search string.
      if (length(dfs_scp) > 0) {
        dfs_scp_all[[query_scp]] <- dplyr::bind_rows(dfs_scp)
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
    scopus_df <- dplyr::bind_rows(dfs_scp_all)
    if (nrow(scopus_df) == 0) {
      message("No new record from Scopus retrieved.")
      return(data.frame(author = character(), year = character(),
                        title = character(), journal = character(),
                        volume = character(), number = character(),
                        abstract = character(), doi = character(),
                        pages = character(), isbn = character(),
                        source = character(),
                        platform_id = character(),
                        stringsAsFactors = FALSE))
    }
    # Appends raw IDs to the id_log sheet with platform and timestamp.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    existing_log <- openxlsx::readWorkbook(history_id, sheet = "id_log")
    new_log_rows <- data.frame(id = scopus_df$scopus_id, platform = "Scopus",
                               timestamp = date_suffix, stringsAsFactors = FALSE)
    openxlsx::writeData(history_id, "id_log", rbind(existing_log, new_log_rows))
    # Makes sure there is no duplicates in the extracted IDs.
    scopus_id_vec <- unique(scopus_df$scopus_id)
    # Extract the IDs retrieved previously from the in-memory workbook.
    last_list <- openxlsx::readWorkbook(history_id, sheet = "updated_id_list")
    # Converts the list to a vector.
    last_list_vec <- last_list$id
    # Gets only the new IDs.
    scp_new <- setdiff(scopus_id_vec, last_list_vec)
    message(length(scp_new), " new records found among ", total_results_scp, " total results.")
    # Creates a dataframe with only the new IDs.
    scopus_new_id <- data.frame(id = scp_new)
    # Adds the new IDs to the current list.
    updated_list <- rbind(last_list, scopus_new_id)

    # Filter pre-extracted records to new IDs only (no additional API calls needed).
    all_scp_records_df <- dplyr::bind_rows(all_scp_records)
    scopus_results <- all_scp_records_df[all_scp_records_df$platform_id %in% scp_new, , drop = FALSE]
    rownames(scopus_results) <- NULL

    # Writes the list updated with the new IDs only after extraction succeeds.
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = updated_list)
    openxlsx::saveWorkbook(history_id,
                           file = history_id_path,
                           overwrite = TRUE)

    scopus_results # Returns the dataframe with all the references.
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }

}
