#' extract the metadata from the new references from PubMed
#' based on the search strings found in search_list.txt
#' @param search_list_path path to search_list
#' @return A dataframe containing the metadata from the new references on PubMed
#' @importFrom utils URLencode
#' @importFrom stats setNames

extract_pmd_list <- function(search_list_path){
  if(file.exists(search_list_path)){
    # Initialize an empty list to store results for all queries
    dfs_pmd_all <- list()
    # Read the file
    lines <- readLines(search_list_path, warn = FALSE)
    # Convert file contents into a named list
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines))

    # Loop to fetch data for each search term
    for (search_query in search_list) {
      # Initialize an empty list for current search
      dfs_pmd <- list()
      next_start_pmd <- 0
      # Call PubMed API to get total results
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
      search_url_pmd <- paste0(base_url_pmd, "esearch.fcgi?db=pubmed&term=", utils::URLencode(search_query), "&retmax=1&retmode=json")
      response_pmd <- jsonlite::fromJSON(httr::content(httr::GET(search_url_pmd), "text", encoding = "UTF-8"))
      # Check if count is available
      if (is.null(response_pmd$esearchresult$count)) {
        message("No results found for ", search_query)
        next
      }

      max_result_pmd <- as.numeric(response_pmd$esearchresult$count)
      print(max_result_pmd)
      imax_pmd <- ceiling(max_result_pmd / 200)  # Pagination

      # Loop to fetch data in batches
      for (i in 1:imax_pmd) {
        # Construct API call
        batch_url_pmd <- paste0(
          base_url_pmd, "esearch.fcgi?db=pubmed&term=", utils::URLencode(search_query),
          "&retstart=", next_start_pmd, "&retmax=200&retmode=json"
        )
        response_pmd <- jsonlite::fromJSON(httr::content(httr::GET(batch_url_pmd), "text", encoding = "UTF-8"))
        # Check if ID list exists
        if (!is.null(response_pmd$esearchresult$idlist)) {
          dfs_pmd[[i]] <- data.frame(PMID = unlist(response_pmd$esearchresult$idlist))
        } else {
          message("No more results found for ", search_query)
          break
        }

        # Update starting index for the next batch
        next_start_pmd <- next_start_pmd + 200

        # Message to indicate progress
        message("Finished batch ", i, " for ", search_query)

        # Avoid hitting rate limits
        Sys.sleep(0.2)
      }

      # Combine results into one data frame per query
      if (length(dfs_pmd) > 0) {
        dfs_pmd_all[[search_query]] <- dplyr::bind_rows(dfs_pmd)
      }
    }

    #create the search history sheet and/or document and save the corresponding variables
    sh <- create_id_history()
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    # Combine all results into a single data frame
    pmd_df <- dplyr::bind_rows(dfs_pmd_all)
    # Add a sheet with date included in name
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("pmd_search", date_suffix)
    openxlsx::addWorksheet(history_id, sheet_name)
    # Write data to the sheet
    openxlsx::writeData(history_id, sheet_name, pmd_df)
    #save history excel
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE)

    pmid_list <- pmd_df[!duplicated(pmd_df),]
    last_list <- readxl::read_excel("history_id.xlsx", sheet = "updated_id_list")
    last_list_vec <- last_list$id
    pmd_new <- setdiff(pmid_list, last_list_vec)
    pmd_new_id <- data.frame(id = pmd_new)
    updated_list <- rbind(last_list, pmd_new_id)
    openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list)
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE)


    # Step 2: Fetch article details using PMIDs
    pubmed_results <- data.frame()
    num_doi_pmd <- 0
    for (pmid in pmd_new) {
      fetch_response <- xml2::read_xml(httr::content(httr::GET(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", "efetch.fcgi?db=pubmed&id=", pmid, "&retmode=xml")), "text", encoding = "UTF-8"))
      # Extract authors
      author_nodes <- xml2::xml_find_all(fetch_response, "//AuthorList/Author")
      pmd_authors <- if (length(author_nodes) > 0) {
        paste(xml2::xml_text(xml2::xml_find_all(author_nodes, "LastName")), collapse = ", ")
      } else {
        NA
      }
      # Extract year
      year_node <- xml2::xml_find_first(fetch_response, "//PubDate/Year")
      pmd_year <- if (!is.na(year_node)) xml2::xml_text(year_node) else NA
      # Extract article title
      title_node <- xml2::xml_find_first(fetch_response, "//ArticleTitle")
      pmd_title <- if (!is.na(title_node)) xml2::xml_text(title_node) else NA
      # Extract journal name
      journal_node <- xml2::xml_find_first(fetch_response, "//Journal/Title")
      pmd_journal <- if (!is.na(journal_node)) xml2::xml_text(journal_node) else NA
      # Extract volume
      volume_node <- xml2::xml_find_first(fetch_response, "//JournalIssue/Volume")
      pmd_volume <- if (!is.na(volume_node)) xml2::xml_text(volume_node) else NA
      # Extract issue
      issue_node <- xml2::xml_find_first(fetch_response, "//JournalIssue/Issue")
      pmd_issue <- if (!is.na(issue_node)) xml2::xml_text(issue_node) else NA
      # Extract abstract
      abstract_node <- xml2::xml_find_first(fetch_response, "//AbstractText")
      pmd_abstract <- if (!is.na(abstract_node)) xml2::xml_text(abstract_node) else NA
      # Extract DOI (if available)
      doi_node <- xml2::xml_find_first(fetch_response, "//ELocationID[@EIdType='doi']")
      pmd_doi <- if (!is.na(doi_node)) xml2::xml_text(doi_node) else NA
      pmd_source <- "PubMed"

      # Store results
      pubmed_results <- rbind(pubmed_results, data.frame(
        author = pmd_authors,
        year = pmd_year,
        title = pmd_title,
        journal = pmd_journal,
        volume = pmd_volume,
        issue = pmd_issue,
        abstract = pmd_abstract,
        doi = pmd_doi,
        source = pmd_source,
        stringsAsFactors = FALSE
      ))

      num_doi_pmd <- num_doi_pmd+1
      print(paste(pmd_doi,num_doi_pmd,"/",length(pmd_new)))

      # Optional: Add delay to prevent API rate limits
      Sys.sleep(0.2)
    }

    return(pubmed_results)
  }
  else{
    print("No search string saved. Please save a search string using the function search_sensitivity().")
  }
}
