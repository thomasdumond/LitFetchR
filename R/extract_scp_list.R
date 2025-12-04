#' extract the metadata from the new references from Scopus
#' based on the search strings found in search_list.txt
#' @param search_list_path path to search_list
#' @return A dataframe containing the metadata from the new references on Scopus
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_scp_list <- function(search_list_path){
  if(file.exists(search_list_path)){

    # Initialize an empty list to store results for all queries
    dfs_scp_all <- list()
    scp_api_key <- Sys.getenv("SCP_API_KEY")
    # Read the file
    lines <- readLines(search_list_path, warn = FALSE)
    # Convert file contents into a named list
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines))

    # Loop to fetch data for each search term
    for (query_scp in search_list) {
      # Initialize an empty list to store the results
      dfs_scp <- list()
      next_start_scp <- 0
      search_scp <- utils::URLencode(paste0('TITLE-ABS-KEY(', query_scp, ')'))
      # Call Scopus API
      base_url_scp <- 'https://api.elsevier.com/content/search/scopus?query='
      search_url_scp <- paste0(base_url_scp, search_scp, '&apiKey=', scp_api_key)
      response_scp <- jsonlite::fromJSON(httr::content(httr::GET(search_url_scp), "text", encoding = "UTF-8"))
      # Check if count is available
      if (is.null(response_scp$`search-results`$`opensearch:totalResults`)) {
        message("No results found for ", search_scp)
        next
      }

      max_result_scp <- as.numeric(response_scp$`search-results`$`opensearch:totalResults`)
      print(max_result_scp)
      imax_scp <- ceiling(max_result_scp / 25)  # Pagination

      # Loop to fetch data in batches
      for (i in 1:imax_scp) {
        # Construct API call
        # Make the request with pagination
        response_scp <- jsonlite::fromJSON(httr::content(httr::GET(paste0(base_url_scp, search_scp, '&start=', next_start_scp, '&apiKey=', scp_api_key)), as = "text"), flatten = TRUE)
        # Update the starting index for the next batch
        next_start_scp <- next_start_scp + 25
        # Check if ID list exists
        if (!is.null(response_scp$`search-results`$entry$`dc:identifier`)) {
          dfs_scp[[i]] <- data.frame(scp_id = unlist(response_scp$`search-results`$entry$`dc:identifier`))
        } else {
          message("No more results found for ", query_scp)
          break
        }

        # Message to indicate progress
        message("Finished batch number ", i)

        # Avoid hitting rate limits (optional: you can adjust the sleep time)
        Sys.sleep(0.2)
      }

      # Combine results into one data frame per query
      if (length(dfs_scp) > 0) {
        dfs_scp_all[[query_scp]] <- dplyr::bind_rows(dfs_scp)
      }

    }

    #create the search history sheet and/or document and save the corresponding variables
    sh <- create_id_history()
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    # Combine all the data frames into one
    scopus_df <- dplyr::bind_rows(dfs_scp_all)
    # Add a sheet with date included in name
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("scopus_search", date_suffix)
    openxlsx::addWorksheet(history_id, sheet_name)
    # Write data to the sheet
    openxlsx::writeData(history_id, sheet_name, scopus_df)
    #save history excel
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE)

    #create a list of scopus ID
    scopus_id <- scopus_df[!duplicated(scopus_df),]
    last_list <- readxl::read_excel("history_id.xlsx", sheet = "updated_id_list")
    last_list_vec <- last_list$id
    scp_new <- setdiff(scopus_id, last_list_vec)
    scopus_new_id <- data.frame(id = scp_new)
    updated_list <- rbind(last_list, scopus_new_id)
    openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list)
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE)


    scopus_results <- data.frame()
    num_doi_scp <- 0
    for (x in scp_new){
      # Parse JSON response
      scp_article <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0("https://api.elsevier.com/content/abstract/scopus_id/",x), httr::add_headers("X-ELS-APIKey" = scp_api_key,"Accept" = "application/json")), "text", encoding = "UTF-8"))

      # Extract article entries from API response
      scp_data <- scp_article$`abstracts-retrieval-response`$coredata

      # Extract authors (set to NA if missing)
      scp_authors <- ifelse("dc:creator" %in% names(scp_data), scp_data$`dc:creator`$author$`ce:surname`, NA)
      # Extract year (set to NA if missing)
      scp_year <- ifelse("prism:coverDate" %in% names(scp_data), scp_data$`prism:coverDate`, NA)
      # Extract title (set to NA if missing)
      scp_titles <- ifelse("dc:title" %in% names(scp_data), scp_data$`dc:title`, NA)
      # Extract journal
      scp_journal <- ifelse("prism:publicationName" %in% names(scp_data), scp_data$`prism:publicationName`, NA)
      # Extract volume (set to NA if missing)
      scp_volume <- ifelse("prism:volume" %in% names(scp_data), scp_data$`prism:volume`, NA)
      # Extract abstract (set to NA if missing)
      scp_abstracts <- ifelse("dc:description" %in% names(scp_data), scp_data$`dc:description`, NA)
      # Extract DOI (set to NA if missing)
      scp_doi <- ifelse("prism:doi" %in% names(scp_data), scp_data$`prism:doi`, NA)
      # Extract issue (set to NA if missing)
      scp_issue <- ifelse("prism:issueIdentifier" %in% names(scp_data), scp_data$`prism:issueIdentifier`, NA)
      # Extract source
      scp_source <- "Scopus"

      # Combine into a data frame
      scopus_results_x <- data.frame(
        author = scp_authors,
        year = scp_year,
        title = scp_titles,
        journal = scp_journal,
        volume = scp_volume,
        issue = scp_issue,
        abstract = scp_abstracts,
        doi = scp_doi,
        source = scp_source,
        stringsAsFactors = FALSE
      )
      num_doi_scp <- num_doi_scp+1
      print(paste(scp_doi,num_doi_scp,"/",length(scp_new)))
      scopus_results <- rbind(scopus_results, scopus_results_x)
      Sys.sleep(0.2)
    }

    return(scopus_results)
  }
  else{
    print("No search string saved. Please save a search string using the function search_sensitivity().")
  }

}
