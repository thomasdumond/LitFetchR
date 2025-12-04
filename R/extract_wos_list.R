#' extract the metadata from the new references from Web of Science
#' based on the search strings found in search_list.txt
#' @param search_list_path path to search_list
#' @return A dataframe containing the metadata from the new references on Web of Science
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_wos_list <- function(search_list_path){
  if(file.exists(search_list_path)){

    # Initialize an empty list to store results for all queries
    dfs_wos_all <- list()
    wos_api_key <- Sys.getenv("WOS_API_KEY")
    # Read the file
    lines <- readLines(search_list_path, warn = FALSE)
    # Convert file contents into a named list
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines))

    # Loop to fetch data for each search term
    for (query_wos in search_list) {
      # Initialize an empty list to store the results
      dfs_wos <- list()
      next_start_wos <- 1
      search_wos <- utils::URLencode(paste0('TS=(', query_wos, ')'))
      # Call WOS API
      base_url_wos <- 'https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery='
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=", search_wos)
      response_wos <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8"))
      # Check if count is available
      if (is.null(response_wos$QueryResult$RecordsFound)) {
        message("No results found for ", search_wos)
        next
      }

      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)
      print(max_result_wos)
      imax_wos <- ceiling(max_result_wos / 100)  # Pagination

      # Loop to fetch data in batches
      for (i in 1:imax_wos) {
        # Construct API call
        # Make the request with pagination
        search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=", search_wos,'&count=100', '&firstRecord=', next_start_wos)
        response_wos <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8"))
        # Update the starting index for the next batch
        next_start_wos <- next_start_wos + 100
        # Check if ID list exists
        if (!is.null(response_wos$Data$Records$records$REC$UID)) {
          dfs_wos[[i]] <- data.frame(wos_id = unlist(response_wos$Data$Records$records$REC$UID))
        } else {
          message("No more results found for ", query_wos)
          break
        }

        # Message to indicate progress
        message("Finished batch number ", i)

        # Avoid hitting rate limits (optional: you can adjust the sleep time)
        Sys.sleep(0.2)
      }

      # Combine results into one data frame per query
      if (length(dfs_wos) > 0) {
        dfs_wos_all[[query_wos]] <- dplyr::bind_rows(dfs_wos)
      }

    }

    #create the search history sheet and/or document and save the corresponding variables
    sh <- create_id_history()
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    # Combine all the data frames into one
    wos_df <- dplyr::bind_rows(dfs_wos_all)
    # Add a sheet with date included in name
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("wos_search", date_suffix)
    openxlsx::addWorksheet(history_id, sheet_name)
    # Write data to the sheet
    openxlsx::writeData(history_id, sheet_name, wos_df)
    #save history excel
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE)

    #create a list of WOS ID
    wos_id <- wos_df[!duplicated(wos_df),]
    last_list <- readxl::read_excel("history_id.xlsx", sheet = "updated_id_list")
    last_list_vec <- last_list$id
    wos_new <- setdiff(wos_id, last_list_vec)
    wos_new_id <- data.frame(id = wos_new)
    updated_list <- rbind(last_list, wos_new_id)
    openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list)
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE)


    wos_results <- data.frame()
    num_doi_wos <- 0
    for (x in wos_new){
      # Parse JSON response
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/id/",x,"?databaseId=WOK")
      wos_article <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8"))

      # Extract authors (set to NA if missing)
      names <- purrr::pluck(wos_article,
                     "Data", "Records", "records", "REC",
                     "static_data", "summary", "names", "name",
                     .default = NA)
      names_df <- data.frame(names[1])
      names_list <- unlist(names_df$last_name)
      wos_authors <- paste(names_list, collapse = ", ")

      # Extract year (set to NA if missing)
      wos_year <- purrr::pluck(wos_article,
                        "Data", "Records", "records", "REC",
                        "static_data", "summary", "pub_info", "pubyear",
                        .default = NA)

      # Extract title (set to NA if missing)
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


      # Extract journal
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

      # Extract volume (set to NA if missing)
      wos_volume <- purrr::pluck(wos_article,
                          "Data", "Records", "records", "REC",
                          "static_data", "summary", "pub_info", "vol",
                          .default = NA)

      # Extract abstract (set to NA if missing)
      abstract <- purrr::pluck(wos_article,
                        "Data", "Records", "records", "REC",
                        "static_data", "fullrecord_metadata", "abstracts", "abstract", "abstract_text", "p",
                        .default = NA)
      if(is.data.frame(abstract)){
        wos_abstract <- paste(abstract$content, collapse = " ")
      } else{
        wos_abstract <- abstract[[1]][1]
      }

      # Extract DOI (set to NA if missing)
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

      # Extract issue (set to NA if missing)
      wos_issue <- purrr::pluck(wos_article,
                         "Data", "Records", "records", "REC",
                         "static_data", "summary", "pub_info", "issue",
                         .default = NA)

      # Extract source
      wos_source <- "Web of Science"

      # Combine into a data frame
      wos_results_x <- data.frame(
        author = wos_authors,
        year = wos_year,
        title = wos_title,
        journal = wos_journal,
        volume = wos_volume,
        issue = wos_issue,
        abstract = wos_abstract,
        doi = wos_doi,
        source = wos_source,
        stringsAsFactors = FALSE
      )
      num_doi_wos <- num_doi_wos+1
      print(paste(wos_doi,num_doi_wos,"/",length(wos_new)))
      wos_results <- rbind(wos_results, wos_results_x)
      Sys.sleep(0.2)
    }

    return(wos_results)
  }
  else{
    print("No search string saved. Please save a search string using the function search_sensitivity().")
  }

}
