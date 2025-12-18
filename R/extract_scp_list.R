#' Extracts the metadata from the new references found on Scopus
#' based on the search string(s) saved in "search_list.txt".
#' @param search_list_path Path to "search_list.txt".
#' @return A dataframe containing the metadata from the new references found on Scopus.
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_scp_list <- function(search_list_path){

  # Only runs if search string(s) are saved, else asks the user to do so.
  if(file.exists(search_list_path)){

    dfs_scp_all <- list() # Initializes an empty list to store results for all queries.
    scp_api_key <- Sys.getenv("SCP_API_KEY") # Extracts the saved API key for Scopus.
    lines <- readLines(search_list_path, warn = FALSE) # Read the file "search_list.txt".
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines))  # Convert file contents into a list of search strings.

    # Loop to fetch data for each search term
    for (query_scp in search_list) {
      dfs_scp <- list() # Initializes an empty list for current search.
      next_start_scp <- 0 # Defines at which reference to start the extraction of a batch.
      search_scp <- utils::URLencode(paste0('TITLE-ABS-KEY(', query_scp, ')'))

      # Call Scopus API to get total results
      base_url_scp <- 'https://api.elsevier.com/content/search/scopus?query=' # Creates the baseline API URL
      search_url_scp <- paste0(base_url_scp, search_scp, '&apiKey=', scp_api_key) # Adds the search information.
      response_scp <- jsonlite::fromJSON(httr::content(httr::GET(search_url_scp), "text", encoding = "UTF-8")) # Extracts the content of the corresponding API call.

      # Check if the search gives results.
      if (is.null(response_scp$`search-results`$`opensearch:totalResults`)) {
        message("No results found for ", search_scp)
        next
      }

      # Gives the number of results from the API call.
      max_result_scp <- as.numeric(response_scp$`search-results`$`opensearch:totalResults`)
      print(max_result_scp)

      # Creates the indicator for the number of batches required to get all the references.
      imax_scp <- ceiling(max_result_scp / 25)  # "25" represents the max number of references per batch.

      # STEP 1: Collect all the unique platform IDs in batches
      for (i in 1:imax_scp) {
        # Construct API call
        # Make the request with pagination
        response_scp <- jsonlite::fromJSON(httr::content(httr::GET(paste0(base_url_scp, search_scp, '&start=', next_start_scp, '&apiKey=', scp_api_key)), as = "text"), flatten = TRUE)
        # Update the starting index for the next batch
        next_start_scp <- next_start_scp + 25
        # Check if ID list exists, and extracts the IDs.
        if (!is.null(response_scp$`search-results`$entry$`dc:identifier`)) {
          dfs_scp[[i]] <- data.frame(scp_id = unlist(response_scp$`search-results`$entry$`dc:identifier`))
        } else {
          message("No more results found for ", query_scp) # When no more IDs are in the call, the extraction stops.
          break
        }

        # Message to indicate the progress of unique ID batch extraction.
        message("Finished batch number ", i)

        # Break to avoid hitting rate limits.
        Sys.sleep(0.2)
      }

      # Combines all unique IDs into one data frame per search string.
      if (length(dfs_scp) > 0) {
        dfs_scp_all[[query_scp]] <- dplyr::bind_rows(dfs_scp)
      }

    }

    # Creates the search history sheet and/or document and saves the unique IDs.
    # If already exists, just extract the data from it.
    sh <- create_id_history()
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    scopus_df <- dplyr::bind_rows(dfs_scp_all)  # Combines all results into a single dataframe.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S") # System date time.
    sheet_name <- paste0("scopus_search", date_suffix) # Creates a unique name.
    openxlsx::addWorksheet(history_id, sheet_name) # Adds a sheet with a unique name.
    openxlsx::writeData(history_id, sheet_name, scopus_df) # Writes data to the sheet.
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE) # Saves history excel.
    scopus_id <- scopus_df[!duplicated(scopus_df),] # Makes sure there is no duplicates in the extracted IDs.
    last_list <- readxl::read_excel("history_id.xlsx", sheet = "updated_id_list") # Extract the IDs retrieved previously.
    last_list_vec <- last_list$id # Converts the list to a vector.
    scp_new <- setdiff(scopus_id, last_list_vec) # Gets only the new IDs.
    scopus_new_id <- data.frame(id = scp_new) # Creates a dataframe with only the new IDs.
    updated_list <- rbind(last_list, scopus_new_id) # Adds the new IDs to the current list.
    openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list) # Writes the list updated with the new IDs.
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE) # Saves the updated list.

    # STEP 2: Fetch article details using unique platform IDs
    scopus_results <- data.frame() # Creates an empty dataframe.
    num_doi_scp <- 0 # Sets up the count to inform user of which reference is being retrieved.

    for (x in scp_new){

      # Gets the JSON response.
      scp_article <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0("https://api.elsevier.com/content/abstract/scopus_id/",x), httr::add_headers("X-ELS-APIKey" = scp_api_key,"Accept" = "application/json")), "text", encoding = "UTF-8"))

      # Extracts article entries from API response.
      scp_data <- scp_article$`abstracts-retrieval-response`$coredata
      scp_data2 <- scp_article$`abstracts-retrieval-response`$item$bibrecord$head
      scp_data3 <- scp_article$`abstracts-retrieval-response`

      # Extracts authors (set to NA if missing).
      names_list <- purrr::pluck(scp_data3,
                                  "authors", "author", "ce:surname",
                                  .default = NA
                                  )
      scp_authors <- paste(names_list, collapse = ", ")

      # Extracts year (set to NA if missing).
      scp_year <- purrr::pluck(scp_data2,
                               "source", "publicationdate", "year",
                               .default = NA_character_
                               )

      # Extracts title (set to NA if missing).
      scp_titles <- ifelse("dc:title" %in% names(scp_data), scp_data$`dc:title`, NA)

      # Extracts journal.
      scp_journal <- ifelse("prism:publicationName" %in% names(scp_data), scp_data$`prism:publicationName`, NA)

      # Extracts volume (set to NA if missing).
      scp_volume <- as.character(
        purrr::pluck(
          scp_data,
          "prism:volume",
          .default = NA_character_
        )
      )

      # Extracts abstract (set to NA if missing).
      scp_abstracts <- ifelse("dc:description" %in% names(scp_data), scp_data$`dc:description`, NA)

      # Extracts DOI (set to NA if missing).
      scp_doi <- ifelse("prism:doi" %in% names(scp_data), scp_data$`prism:doi`, NA)

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

      # Store results in a dataframe, "[1]" is to make sure that each extracted data has the same size.
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
        stringsAsFactors = FALSE
      )

      # Increase the counter by 1.
      num_doi_scp <- num_doi_scp+1
      # Informs the user of the advancement.
      print(paste(scp_doi,num_doi_scp,"/",length(scp_new)))

      # Merges the dataframes.
      scopus_results <- rbind(scopus_results, scopus_results_x)

      # Break to prevent API rate limits. Does not work when too long though...
      Sys.sleep(0.2)
    }

    return(scopus_results) # Returns the dataframe with all the references.
  }
  else{
    # Asks the user to save a search string with the adequate function.
    print("No search string saved. Please save a search string using the function create_save_search().")
  }

}
