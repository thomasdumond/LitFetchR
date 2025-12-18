#' extract the metadata from the new references from Web of Science
#' based on the search strings found in search_list.txt
#' @param search_list_path path to search_list
#' @return A dataframe containing the metadata from the new references on Web of Science
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_wos_list <- function(search_list_path){

    # Only runs if search string(s) are saved, else asks the user to do so.
    if(file.exists(search_list_path)){

    dfs_wos_all <- list() # Initializes an empty list to store results for all queries.
    wos_api_key <- Sys.getenv("WOS_API_KEY") # Extracts the saved API key for Scopus.
    lines <- readLines(search_list_path, warn = FALSE) # Read the file "search_list.txt".
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines))  # Convert file contents into a list of search strings.

    # Loop to fetch data for each search term
    for (query_wos in search_list) {
      dfs_wos <- list() # Initializes an empty list for current search.
      next_start_wos <- 1  # Defines at which reference to start the extraction of a batch.
      search_wos <- utils::URLencode(paste0('TS=(', query_wos, ')'))

      # Call WoS API to get total results
      base_url_wos <- 'https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=' # Creates the baseline API URL
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=", search_wos) # Adds the search information.
      response_wos <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8")) # Extracts the content of the corresponding API call.

      # Check if the search gives results.
      if (is.null(response_wos$QueryResult$RecordsFound)) {
        message("No results found for ", search_wos)
        next
      }

      # Gives the number of results from the API call.
      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)
      print(max_result_wos)

      # Creates the indicator for the number of batches required to get all the references.
      imax_wos <- ceiling(max_result_wos / 100)  # "100" represents the max number of references per batch.

      # STEP 1: Collect all the unique platform IDs in batches
      for (i in 1:imax_wos) {
        # Construct API call
        # Make the request with pagination
        search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=", search_wos,'&count=100', '&firstRecord=', next_start_wos)
        response_wos <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8"))
        # Update the starting index for the next batch
        next_start_wos <- next_start_wos + 100
        # Check if ID list exists, and extracts the IDs.
        if (!is.null(response_wos$Data$Records$records$REC$UID)) {
          dfs_wos[[i]] <- data.frame(wos_id = unlist(response_wos$Data$Records$records$REC$UID))
        } else {
          message("No more results found for ", query_wos) # When no more IDs are in the call, the extraction stops.
          break
        }

        # Message to indicate the progress of unique ID batch extraction.
        message("Finished batch number ", i)

        # Break to avoid hitting rate limits.
        Sys.sleep(0.2)
      }

      # Combines all unique IDs into one data frame per search string.
      if (length(dfs_wos) > 0) {
        dfs_wos_all[[query_wos]] <- dplyr::bind_rows(dfs_wos)
      }

    }

    # Creates the search history sheet and/or document and saves the unique IDs.
    # If already exists, just extract the data from it.
    sh <- create_id_history()
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    wos_df <- dplyr::bind_rows(dfs_wos_all)  # Combines all results into a single dataframe.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S") # System date time.
    sheet_name <- paste0("wos_search", date_suffix) # Creates a unique name.
    openxlsx::addWorksheet(history_id, sheet_name) # Adds a sheet with a unique name.
    openxlsx::writeData(history_id, sheet_name, wos_df) # Writes data to the sheet.
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE) # Saves history excel.
    wos_id <- wos_df[!duplicated(wos_df),] # Makes sure there is no duplicates in the extracted IDs.
    last_list <- readxl::read_excel("history_id.xlsx", sheet = "updated_id_list") # Extract the IDs retrieved previously.
    last_list_vec <- last_list$id # Converts the list to a vector.
    wos_new <- setdiff(wos_id, last_list_vec) # Gets only the new IDs.
    wos_new_id <- data.frame(id = wos_new) # Creates a dataframe with only the new IDs.
    updated_list <- rbind(last_list, wos_new_id) # Adds the new IDs to the current list.
    openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list) # Writes the list updated with the new IDs.
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE) # Saves the updated list.

    # STEP 2: Fetch article details using unique platform IDs
    wos_results <- data.frame() # Creates an empty dataframe.
    num_doi_wos <- 0 # Sets up the count to inform user of which reference is being retrieved.

    for (x in wos_new){

      # Gets the JSON response.
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/id/",x,"?databaseId=WOK")
      wos_article <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8"))

      # Extracts authors (set to NA if missing).
      names <- purrr::pluck(wos_article,
                     "Data", "Records", "records", "REC",
                     "static_data", "summary", "names", "name",
                     .default = NA)
      names_df <- data.frame(names[1])
      names_list <- unlist(names_df$last_name)
      wos_authors <- paste(names_list, collapse = ", ")

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
      titles_df <- data.frame(titles)
      if(is.vector(titles_df[titles_df$type == "item", "content"])){
        wos_title <- ifelse(
          any(titles_df$type == "item", na.rm = TRUE),
          titles_df[titles_df$type == "item", "content"][[1]],
          NA
        )
      } else{
        wos_title <- ifelse(
          any(titles_df$type == "item", na.rm = TRUE),
          titles_df[titles_df$type == "item", "content"],
          NA
        )
      }


      # Extracts journal.
      if(is.vector(titles_df[titles_df$type == "source", "content"])){
        wos_journal <- ifelse(
          any(titles_df$type == "source", na.rm = TRUE),
          titles_df[titles_df$type == "source", "content"][[1]],
          NA
        )
      } else{
        wos_journal <- ifelse(
          any(titles_df$type == "source", na.rm = TRUE),
          titles_df[titles_df$type == "source", "content"],
          NA
        )
      }

      # Extracts volume (set to NA if missing).
      wos_volume <- as.character(purrr::pluck(wos_article,
                                 "Data", "Records", "records", "REC", "static_data", "summary", "pub_info", "vol",
                                 .default = NA
                                 )
      )

      # Extracts abstract (set to NA if missing).
      abstract <- purrr::pluck(wos_article,
                        "Data", "Records", "records", "REC",
                        "static_data", "fullrecord_metadata", "abstracts", "abstract", "abstract_text", "p",
                        .default = NA)
      if(is.data.frame(abstract)){
        wos_abstract <- paste(abstract$content, collapse = " ")
      } else{
        wos_abstract <- abstract[[1]][1]
      }

      # Extracts DOI (set to NA if missing).
      identifiers <- purrr::pluck(wos_article,
                           "Data", "Records", "records", "REC",
                           "dynamic_data", "cluster_related", "identifiers", "identifier",
                           .default = NA)
      if(is.data.frame(identifiers)) {
        wos_doi <- ifelse(
          any(identifiers_df$type == "doi", na.rm = TRUE),
          identifiers_df[identifiers_df$type == "doi", "value"],
          NA)
      } else {
        identifiers_df <- data.frame(identifiers[1])
        wos_doi <- ifelse(
          any(identifiers_df$type == "doi", na.rm = TRUE),
          identifiers_df[identifiers_df$type == "doi", "value"],
          NA)
      }

      # Extracts issue (set to NA if missing).
      wos_issue <- as.character(purrr::pluck(wos_article,
                                "Data", "Records", "records", "REC", "static_data", "summary", "pub_info", "issue",
                                .default = NA
        )
      )

      # Indicates the source platform of the reference.
      wos_source <- "Web of Science"

      # Store results in a dataframe, "[1]" is to make sure that each extracted data has the same size.
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
        stringsAsFactors = FALSE
      )

      # Increase the counter by 1.
      num_doi_wos <- num_doi_wos+1
      # Informs the user of the advancement.
      print(paste(wos_doi,num_doi_wos,"/",length(wos_new)))

      # Merges the dataframes.
      wos_results <- rbind(wos_results, wos_results_x)

      # Break to prevent API rate limits. Does not work when too long though...
      Sys.sleep(1.2)
    }

    return(wos_results) # Returns the dataframe with all the references.
  }
  else{
    # Asks the user to save a search string with the adequate function.
    print("No search string saved. Please save a search string using the function create_save_search().")
  }

}
