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
#'  \item{issue}{Character. Publication journal issue.}
#'  \item{abstract}{Character. Publication abstract.}
#'  \item{doi}{Character. Publication Digital Object Identifier (DOI).}
#'  \item{source}{Character. Data source.}
#'  \item{platform_id}{Character. Publication unique identifier in data source.}
#' }
#' If \code{search_list_path} does not exist, returns \code{NULL}.
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_scp_list <- function(search_list_path, directory) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {
    # Initializes an empty list to store results for all queries.
    dfs_scp_all <- list()
    # Extracts the saved API key for Scopus.
    scp_api_key <- Sys.getenv("scp_api_key")
    if (!nzchar(scp_api_key)) {
      stop("Scopus API key not found.
           Set env var `scp_api_key` using the function `save_api_key`.",
           call. = FALSE)
    }
    # Read the file "search_list.txt".
    lines <- readLines(search_list_path, warn = FALSE)
    # Convert file contents into a list of search strings.
    search_list <- stats::setNames(sub(".*=", "", lines),
                                   sub("=.*", "", lines))

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
      search_url_scp <- paste0(base_url_scp,
                               search_scp,
                               "&apiKey=",
                               scp_api_key)
      # Extracts the content of the corresponding API call.
      response_scp <- jsonlite::fromJSON(get_text_retry(search_url_scp),
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
      message(max_result_scp)

      # Creates the indicator for the number of batches
      # required to get all the references.
      # "25" represents the max number of references per batch.
      imax_scp <- ceiling(max_result_scp / 25)

      # STEP 1: Collect all the unique platform IDs in batches
      for (i in 1:imax_scp) {
        # Construct API call
        # Make the request with pagination
        # Adds the search information.
        search_url_scp <- paste0(base_url_scp,
                                 search_scp,
                                 "&start=",
                                 next_start_scp,
                                 "&apiKey=",
                                 scp_api_key)
        response_scp <- jsonlite::fromJSON(get_text_retry(search_url_scp),
                                           flatten = TRUE)
        # Update the starting index for the next batch
        next_start_scp <- next_start_scp + 25
        # Check if ID list exists, and extracts the IDs.
        if (!is.null(response_scp$`search-results`$entry$`dc:identifier`)) {
          dfs_scp[[i]] <- data.frame(
            scopus_id = unlist(response_scp$`search-results`$entry$`dc:identifier`)
            )
        } else {
          # When no more IDs are in the call, the extraction stops.
          message("No more results found for ", query_scp)
          break
        }

        # Message to indicate the progress of unique ID batch extraction.
        message("Finished batch number ", i)

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
    # System date time.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    # Creates a unique name.
    sheet_name <- paste0("scopus_search", date_suffix)
    # Adds a sheet with a unique name.
    openxlsx::addWorksheet(history_id, sheet_name)
    # Writes data to the sheet.
    openxlsx::writeData(history_id, sheet_name, scopus_df)
    # Saves history excel.
    openxlsx::saveWorkbook(history_id,
                           file = history_id_path,
                           overwrite = TRUE)
    # Makes sure there is no duplicates in the extracted IDs.
    scopus_id_vec <- unique(scopus_df$scopus_id)
    # Extract the IDs retrieved previously.
    last_list <- readxl::read_excel(history_id_path,
                                    sheet = "updated_id_list")
    # Converts the list to a vector.
    last_list_vec <- last_list$id
    # Gets only the new IDs.
    scp_new <- setdiff(scopus_id_vec, last_list_vec)
    # Creates a dataframe with only the new IDs.
    scopus_new_id <- data.frame(id = scp_new)
    # Adds the new IDs to the current list.
    updated_list <- rbind(last_list, scopus_new_id)
    # Writes the list updated with the new IDs.
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = updated_list)
    # Saves the updated list.
    openxlsx::saveWorkbook(history_id,
                           file = history_id_path,
                           overwrite = TRUE)

    # STEP 2: Fetch article details using unique platform IDs
    # Creates an empty dataframe.
    scopus_results <- data.frame(author = character(),
                                 year = character(),
                                 title = character(),
                                 journal = character(),
                                 volume = character(),
                                 issue = character(),
                                 abstract = character(),
                                 doi = character(),
                                 source = character(),
                                 platform_id = character(),
                                 stringsAsFactors = FALSE
                                 )
    # Sets up the count to inform user of which reference is being retrieved.
    num_doi_scp <- 0

    for (x in scp_new){

      # Gets the JSON response.
      search_url_scp <- paste0(
        "https://api.elsevier.com/content/abstract/scopus_id/",
        x
        )
      scp_article <- tryCatch(jsonlite::fromJSON(get_text_retry(url = search_url_scp, headers = c("X-ELS-APIKey" = scp_api_key, "Accept" = "application/json")), flatten = TRUE),
                              error = function(e) {
                                message("FAILED Scopus ID=", x, " : ", conditionMessage(e))
                                data.frame(
                                  author = NA_character_,
                                  year = NA_character_,
                                  title = NA_character_,
                                  journal = NA_character_,
                                  volume = NA_character_,
                                  issue = NA_character_,
                                  abstract = NA_character_,
                                  doi = NA_character_,
                                  source = "Scopus",
                                  platform_id = x,
                                  stringsAsFactors = FALSE
                                )
                              }
      )

      if (is.data.frame(scp_article)) {
        scopus_results <- rbind(scopus_results, scp_article)
        next
      }

      # Extracts article entries from API response.
      scp_data <- scp_article$`abstracts-retrieval-response`$coredata
      scp_data2 <- scp_article$`abstracts-retrieval-response`$item$bibrecord$head
      scp_data3 <- scp_article$`abstracts-retrieval-response`

      # Extracts authors (set to NA if missing).
      names_list <- purrr::pluck(scp_data3,
                                  "authors", "author", "ce:surname",
                                  .default = character(0)
                                  )
      scp_authors <- if (length(names_list) > 0) {
        paste(names_list, collapse = ", ")
      } else {
        NA_character_
      }

      # Extracts year (set to NA if missing).
      scp_year <- purrr::pluck(scp_data2,
                               "source", "publicationdate", "year",
                               .default = NA_character_
                               )

      # Extracts title (set to NA if missing).
      scp_titles <- if ("dc:title" %in% names(scp_data)) {
        scp_data$`dc:title`
      } else {
        NA_character_
      }

      # Extracts journal.
      scp_journal <- if ("prism:publicationName" %in% names(scp_data)) {
        scp_data$`prism:publicationName`
      } else {
        NA_character_
      }

      # Extracts volume (set to NA if missing).
      scp_volume <- as.character(
        purrr::pluck(
          scp_data,
          "prism:volume",
          .default = NA_character_
        )
      )

      # Extracts abstract (set to NA if missing).
      scp_abstracts <- if ("dc:description" %in% names(scp_data)) {
        scp_data$`dc:description`
      } else {
        NA_character_
      }

      # Extracts DOI (set to NA if missing).
      scp_doi <- if ("prism:doi" %in% names(scp_data)) {
        scp_data$`prism:doi`
      } else {
        NA_character_
      }

      # Extracts issue (set to NA if missing).
      scp_issue <- as.character(
        purrr::pluck(
          scp_data,
          "prism:issueIdentifier",
          .default = NA_character_
        )
      )

      # Indicates the source platform of the reference.
      scp_source <- "Scopus"

      # Store results in a dataframe,
      # "[1]" is to make sure that each extracted data has the same size.
      scopus_results_x <- data.frame(
        author = scp_authors[1],
        year = scp_year[1],
        title = scp_titles[1],
        journal = scp_journal[1],
        volume = scp_volume[1],
        issue = scp_issue[1],
        abstract = scp_abstracts[1],
        doi = scp_doi[1],
        source = scp_source[1],
        platform_id = x,
        stringsAsFactors = FALSE
      )

      # Increase the counter by 1.
      num_doi_scp <- num_doi_scp + 1
      # Informs the user of the advancement.
      message(paste(scp_doi, num_doi_scp, "/", length(scp_new)))

      # Merges the dataframes.
      scopus_results <- rbind(scopus_results, scopus_results_x)

    }

    scopus_results # Returns the dataframe with all the references.
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }

}
