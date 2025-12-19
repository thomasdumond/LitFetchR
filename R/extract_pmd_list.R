#' Extracts the metadata from the new references found on PubMed
#' based on the search string(s) saved in "search_list.txt".
#' @param search_list_path Path to "search_list.txt".
#' @return A dataframe containing the metadata from the new references found on PubMed.
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_pmd_list <- function(search_list_path){

  # Only runs if search string(s) are saved, else asks the user to do so.
  if(file.exists(search_list_path)){

    dfs_pmd_all <- list() # Initializes an empty list to store results for all queries.
    lines <- readLines(search_list_path, warn = FALSE) # Read the file "search_list.txt".
    search_list <- stats::setNames(sub(".*=", "", lines), sub("=.*", "", lines)) # Convert file contents into a list of search strings.

    # Loop to fetch data for each search string.
    for (search_query in search_list) {

      dfs_pmd <- list() # Initializes an empty list for current search.
      next_start_pmd <- 0 # Defines at which reference to start the extraction of a batch.

      # Call PubMed API to get total results
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/" # Creates the baseline API URL
      search_url_pmd <- paste0(base_url_pmd, "esearch.fcgi?db=pubmed&term=", utils::URLencode(search_query), "&retmax=1&retmode=json") # Adds the search information.
      response_pmd <- jsonlite::fromJSON(get_text_retry(search_url_pmd)) # Extracts the content of the corresponding API call.

      # Check if the search gives results.
      if (is.null(response_pmd$esearchresult$count)) {
        message("No results found for ", search_query)
        next
      }

      # Gives the number of results from the API call.
      max_result_pmd <- as.numeric(response_pmd$esearchresult$count)
      print(max_result_pmd)

      # Creates the indicator for the number of batches required to get all the references.
      imax_pmd <- ceiling(max_result_pmd / 200)  # "200" represents the max number of references per batch.

      # STEP 1: Collect all the unique platform IDs in batches
      for (i in 1:imax_pmd) {
        # Construction of the API call.
        batch_url_pmd <- paste0(
          base_url_pmd, "esearch.fcgi?db=pubmed&term=", utils::URLencode(search_query),
          "&retstart=", next_start_pmd, "&retmax=200&retmode=json"
        )
        response_pmd <- jsonlite::fromJSON(get_text_retry(batch_url_pmd))
        # Check if ID list exists, and extracts the IDs.
        if (!is.null(response_pmd$esearchresult$idlist)) {
          dfs_pmd[[i]] <- data.frame(PMID = unlist(response_pmd$esearchresult$idlist))
        } else {
          message("No more results found for ", search_query) # When no more IDs are in the call, the extraction stops.
          break
        }

        # Updates starting index for the next batch.
        next_start_pmd <- next_start_pmd + 200

        # Message to indicate the progress of unique ID batch extraction.
        message("Finished batch ", i, " for ", search_query)

        # Break to avoid hitting rate limits.
        #Sys.sleep(0.2)
      }

      # Combines all unique IDs into one data frame per search string.
      if (length(dfs_pmd) > 0) {
        dfs_pmd_all[[search_query]] <- dplyr::bind_rows(dfs_pmd)
      }
    }

    # Creates the search history sheet and/or document and saves the unique IDs.
    # If already exists, just extract the data from it.
    sh <- create_id_history()
    history_id <- sh$history_id

    #SAVE HISTORY IDs
    pmd_df <- dplyr::bind_rows(dfs_pmd_all) # Combines all results into a single dataframe.
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S") # System date time.
    sheet_name <- paste0("pmd_search", date_suffix) # Creates a unique name.
    openxlsx::addWorksheet(history_id, sheet_name) # Adds a sheet with a unique name.
    openxlsx::writeData(history_id, sheet_name, pmd_df) # Writes data to the sheet.
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE) # Saves history excel.
    pmid_vec <- unique(pmd_df$PMID) # Makes sure there is no duplicates in the extracted IDs.
    last_list <- readxl::read_excel("history_id.xlsx", sheet = "updated_id_list") # Extract the IDs retrieved previously.
    last_list_vec <- last_list$id # Converts the list to a vector.
    pmd_new <- setdiff(pmid_vec, last_list_vec) # Gets only the new IDs.
    pmd_new_id <- data.frame(id = pmd_new) # Creates a dataframe with only the new IDs.
    updated_list <- rbind(last_list, pmd_new_id) # Adds the new IDs to the current list.
    openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list) # Writes the list updated with the new IDs.
    openxlsx::saveWorkbook(history_id, "history_id.xlsx", overwrite = TRUE) # Saves the updated list.

    # STEP 2: Fetch article details using unique platform IDs
    pubmed_results <- data.frame() # Creates an empty dataframe.
    num_doi_pmd <- 0 # Sets up the count to inform user of which reference is being retrieved.

    for (pmid in pmd_new) {

      # API call for one unique ID.
      #fetch_response <- xml2::read_xml(httr::content(httr::GET(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", "efetch.fcgi?db=pubmed&id=", pmid, "&retmode=xml")), "text", encoding = "UTF-8"))
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
      fetch_url <- paste0(base_url_pmd,"efetch.fcgi?db=pubmed&id=", pmid, "&retmode=xml")
      fetch_response <- tryCatch(
        xml2::read_xml(get_text_retry(fetch_url)),
        error = function(e) {
          message("FAILED PMID=", pmid, " : ", conditionMessage(e))
          return(NULL)
        }
      )

      if (is.null(fetch_response)) {
        pubmed_results <- rbind(pubmed_results, data.frame(
          author = NA_character_,
          year = NA_character_,
          title = NA_character_,
          journal = NA_character_,
          volume = NA_character_,
          issue = NA_character_,
          abstract = NA_character_,
          doi = NA_character_,
          source = "PubMed",
          platform_id = pmid,
          stringsAsFactors = FALSE
        ))
        next
      }

      # Extracts authors.
      author_nodes <- xml2::xml_find_all(fetch_response, "//AuthorList/Author")
      pmd_authors <- if (length(author_nodes) > 0) {
        paste(xml2::xml_text(xml2::xml_find_all(author_nodes, "LastName")), collapse = ", ")
      } else {
        NA_character_
      }

      # Extracts year.
      year_node <- xml2::xml_find_first(fetch_response, "//PubDate/Year")
      pmd_year <- xml2::xml_text(year_node, trim = TRUE)
      pmd_year[pmd_year == ""] <- NA_character_

      # Extracts article title.
      title_node <- xml2::xml_find_first(fetch_response, "//ArticleTitle")
      pmd_title <- if (!is.null(title_node) && length(title_node) > 0) xml2::xml_text(title_node) else NA_character_

      # Extracts journal name.
      journal_node <- xml2::xml_find_first(fetch_response, "//Journal/Title")
      pmd_journal <- if (!is.null(journal_node) && length(journal_node) > 0) xml2::xml_text(journal_node) else NA_character_

      # Extracts volume.
      volume_node <- xml2::xml_find_first(fetch_response, "//JournalIssue/Volume")
      pmd_volume <- xml2::xml_text(volume_node, trim = TRUE)
      if (length(pmd_volume) == 0 || identical(pmd_volume, "")) pmd_volume <- NA_character_

      # Extracts issue.
      issue_node <- xml2::xml_find_first(fetch_response, "//JournalIssue/Issue")
      pmd_issue <- xml2::xml_text(issue_node, trim = TRUE)
      if (length(pmd_issue) == 0 || identical(pmd_issue, "")) pmd_issue <- NA_character_

      # Extracts abstract.
      abstract_node <- xml2::xml_find_first(fetch_response, "//AbstractText")
      pmd_abstract <- if (!is.null(abstract_node) && length(abstract_node) > 0) xml2::xml_text(abstract_node) else NA_character_

      # Extracts DOI.
      pmd_doi <- xml2::xml_text(
        xml2::xml_find_first(fetch_response, ".//ArticleId[@IdType='doi']"),
        trim = TRUE
      )
      if (length(pmd_doi) == 0 || identical(pmd_doi, "")) pmd_doi <- NA_character_

      # Indicates the source platform of the reference.
      pmd_source <- "PubMed"

      # Store results in a dataframe, "[1]" is to make sure that each extracted data has the same size.
      pubmed_results <- rbind(pubmed_results, data.frame(
        author = pmd_authors[1],
        year = pmd_year[1],
        title = pmd_title[1],
        journal = pmd_journal[1],
        volume = pmd_volume[1],
        issue = pmd_issue[1],
        abstract = pmd_abstract[1],
        doi = pmd_doi[1],
        source = pmd_source[1],
        platform_id = pmid,
        stringsAsFactors = FALSE
      ))

      # Increase the counter by 1.
      num_doi_pmd <- num_doi_pmd+1
      # Informs the user of the advancement.
      print(paste(pmd_doi,num_doi_pmd,"/",length(pmd_new)))

      # Break to prevent API rate limits. Does not work when too long though...
      Sys.sleep(0.1)
    }

    return(pubmed_results) # Returns the dataframe with all the references.
  }
  else{
    # Asks the user to save a search string with the adequate function.
    print("No search string saved. Please save a search string using the function create_save_search().")
  }
}
