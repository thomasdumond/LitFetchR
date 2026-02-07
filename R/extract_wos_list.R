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

extract_wos_list <- function(search_list_path, directory) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {
    # Initializes an empty list to store results for all queries.
    dfs_wos_all <- list()
    # Extracts the saved API key for Scopus.
    wos_api_key <- Sys.getenv("wos_api_key")
    if (!nzchar(wos_api_key)) {
      stop("Web of Science API key not found.
           Set env var `wos_api_key` using the function `save_api_key`.",
           call. = FALSE)
    }
    # Read the file "search_list.txt".
    lines <- readLines(search_list_path, warn = FALSE)
    # Convert file contents into a list of search strings.
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines))

    # Loop to fetch data for each search term
    for (query_wos in search_list) {
      # Initializes an empty list for current search.
      dfs_wos <- list()
      # Defines at which reference to start the extraction of a batch.
      next_start_wos <- 1
      search_wos <- utils::URLencode(paste0("TS=(", query_wos, ")"))

      # Call WoS API to get total results
      # Creates the baseline API URL
      base_url_wos <- "https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery="
      # Adds the search information.
      search_url_wos <- paste0(base_url_wos, search_wos)
      response_wos <- jsonlite::fromJSON(get_text_retry(search_url_wos,
                                                        headers = c("X-ApiKey" = wos_api_key)
                                                        )
                                         )

      # Gives the number of results from the API call.
      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)
      message(max_result_wos)

      # Creates the indicator for the number of batches required to get all the references.
      # "100" represents the max number of references per batch.
      imax_wos <- ceiling(max_result_wos / 100)

      # STEP 1: Collect all the unique platform IDs in batches
      for (i in 1:imax_wos) {
        # Construct API call
        # Make the request with pagination
        search_url_wos <- paste0(
          "https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=",
          search_wos,
          "&count=100",
          "&firstRecord=",
          next_start_wos)

        response_wos <- jsonlite::fromJSON(get_text_retry(search_url_wos,
                                                          headers = c("X-ApiKey" = wos_api_key)
                                                          )
                                           )
        # Update the starting index for the next batch
        next_start_wos <- next_start_wos + 100
        # Check if ID list exists, and extracts the IDs.
        if (!is.null(response_wos$Data$Records$records$REC$UID)) {
          dfs_wos[[i]] <- data.frame(
            wos_id = unlist(response_wos$Data$Records$records$REC$UID)
            )
        } else {
          # When no more IDs are in the call, the extraction stops.
          message("No more results found for ", query_wos)
          break
        }

        # Message to indicate the progress of unique ID batch extraction.
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
    # System date time.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    # Creates a unique name.
    sheet_name <- paste0("wos_search", date_suffix)
    # Adds a sheet with a unique name.
    openxlsx::addWorksheet(history_id, sheet_name)
    # Writes data to the sheet.
    openxlsx::writeData(history_id, sheet_name, wos_df)
    # Saves history excel.
    openxlsx::saveWorkbook(history_id,
                           file = history_id_path,
                           overwrite = TRUE)
    # Makes sure there is no duplicates in the extracted IDs and format as vector.
    wos_id_vec <- unique(wos_df$wos_id)
    # Extract the IDs retrieved previously.
    last_list <- readxl::read_excel(history_id_path,
                                    sheet = "updated_id_list")
    # Converts the list to a vector.
    last_list_vec <- last_list$id
    # Gets only the new IDs.
    wos_new <- setdiff(wos_id_vec, last_list_vec)
    # Creates a dataframe with only the new IDs.
    wos_new_id <- data.frame(id = wos_new)
    # Adds the new IDs to the current list.
    updated_list <- rbind(last_list, wos_new_id)
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
    wos_results <- data.frame(author = character(),
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
    num_doi_wos <- 0

    for (x in wos_new){

      # Gets the JSON response.
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/id/",
                               x,
                               "?databaseId=WOK")
      wos_article <- tryCatch(
        jsonlite::fromJSON(get_text_retry(search_url_wos,
                                          headers = c("X-ApiKey" = wos_api_key))),
        error = function(e) {
          message("FAILED UID=", x, " : ", conditionMessage(e))
          data.frame(
            author = NA_character_,
            year = NA_character_,
            title = NA_character_,
            journal = NA_character_,
            volume = NA_character_,
            issue = NA_character_,
            abstract = NA_character_,
            doi = NA_character_,
            source = "Web of Science",
            platform_id = x,
            stringsAsFactors = FALSE
          )

        }
      )

      if (is.data.frame(wos_article)) {
        wos_results <- rbind(wos_results, wos_article)
        next
      }

      # Extracts authors (set to NA if missing).
      names <- purrr::pluck(wos_article,
                     "Data", "Records", "records", "REC",
                     "static_data", "summary", "names", "name",
                     .default = NULL)
      names_tbl <- tryCatch(as.data.frame(names), error = function(e) NULL)
      wos_authors <- NA_character_
      if (!is.null(names_tbl) && nrow(names_tbl) > 0) {

        if ("last_name" %in% names(names_tbl)) {
          vals <- stats::na.omit(names_tbl$last_name)
        } else if ("full_name" %in% names(names_tbl)) {
          vals <- stats::na.omit(names_tbl$full_name)
        } else {
          vals <- character(0)
        }

        if (length(vals) > 0) {
          wos_authors <- paste(as.character(vals), collapse = ", ")
        }
      }

      # Extracts year (set to NA if missing).
      wos_year <- as.character(purrr::pluck(wos_article,
                        "Data", "Records", "records", "REC",
                        "static_data", "summary", "pub_info", "pubyear",
                        .default = NA)
      )

      # Extracts title (set to NA if missing).
      titles <- purrr::pluck(wos_article,
                      "Data", "Records", "records", "REC",
                      "static_data", "summary", "titles", "title",
                      .default = NA)
      titles_df <- tryCatch(as.data.frame(titles), error = function(e) NULL)
      wos_title <- NA_character_
      wos_journal <- NA_character_

      if (!is.null(titles_df) && all(c("type", "content") %in% names(titles_df))) {
        item <- titles_df[titles_df$type == "item", "content", drop = TRUE]
        src  <- titles_df[titles_df$type == "source", "content", drop = TRUE]
        if (length(item) > 0) wos_title <- as.character(item[[1]])
        if (length(src)  > 0) wos_journal <- as.character(src[[1]])
      }

      # Extracts volume (set to NA if missing).
      wos_volume <- as.character(purrr::pluck(wos_article,
                                 "Data",
                                 "Records",
                                 "records",
                                 "REC",
                                 "static_data",
                                 "summary",
                                 "pub_info",
                                 "vol",
                                 .default = NA
                                 )
      )

      # Extracts abstract (set to NA if missing).
      abstract <- purrr::pluck(wos_article,
                        "Data", "Records", "records", "REC",
                        "static_data", "fullrecord_metadata",
                        "abstracts", "abstract", "abstract_text", "p",
                        .default = NULL)

      wos_abstract <- NA_character_
      if (is.data.frame(abstract) && "content" %in% names(abstract)) {
        wos_abstract <- paste(abstract$content, collapse = " ")
      } else if (is.list(abstract) && length(abstract) > 0) {
        wos_abstract <- as.character(abstract[[1]][1])
      } else if (is.atomic(abstract) && length(abstract) > 0) {
        wos_abstract <- as.character(abstract[1])
      }

      # Extracts DOI (set to NA if missing).
      identifiers <- purrr::pluck(wos_article,
                           "Data", "Records", "records", "REC",
                           "dynamic_data", "cluster_related",
                           "identifiers", "identifier",
                           .default = NULL)

      identifiers_df <- tryCatch(as.data.frame(identifiers),
                                 error = function(e) NULL)
      wos_doi <- NA_character_
      if (!is.null(identifiers_df) && nrow(identifiers_df) > 0 && all(c("type", "value") %in% names(identifiers_df))) {

        # prefer "doi", then fallback to "xref_doi"
        doi_rows <- identifiers_df[identifiers_df$type %in% c("doi", "xref_doi"), , drop = FALSE]

        if (nrow(doi_rows) > 0) {
          # ensure preference order (doi first)
          doi_rows$type <- factor(doi_rows$type, levels = c("doi", "xref_doi"))
          doi_rows <- doi_rows[order(doi_rows$type), , drop = FALSE]

          # take first non-NA value
          vals <- unlist(doi_rows$value, use.names = FALSE)
          vals <- vals[!is.na(vals)]
          if (length(vals) > 0) wos_doi <- as.character(vals[[1]])
        }
      }

      # Extracts issue (set to NA if missing).
      wos_issue <- as.character(purrr::pluck(wos_article,
                                "Data", "Records", "records",
                                "REC", "static_data", "summary",
                                "pub_info", "issue",
                                .default = NA
        )
      )

      # Indicates the source platform of the reference.
      wos_source <- "Web of Science"


      # Store results in a dataframe,
      # "[1]" is to make sure that each extracted data has the same size.
      wos_results_x <- data.frame(
        author = wos_authors[1],
        year = wos_year[1],
        title = wos_title[1],
        journal = wos_journal[1],
        volume = wos_volume[1],
        issue = wos_issue[1],
        abstract = wos_abstract[1],
        doi = wos_doi[1],
        source = wos_source[1],
        platform_id = x,
        stringsAsFactors = FALSE
      )

      # Increase the counter by 1.
      num_doi_wos <- num_doi_wos + 1
      # Informs the user of the advancement.
      message(paste(wos_doi, num_doi_wos, "/", length(wos_new)))

      # Merges the dataframes.
      wos_results <- rbind(wos_results, wos_results_x)

    }

    wos_results # Returns the dataframe with all the references.
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }

}
