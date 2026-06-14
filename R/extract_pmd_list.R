#' Extracts the metadata from the new references found on PubMed
#' based on the search string(s) saved in "search_list.txt".
#' @param search_list_path Path to "search_list.txt".
#' @param directory Choose the directory in which
#'  the references identification history will be saved.
#' @return A data.frame with one row per retrieved PubMed record and columns:
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
#'
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_pmd_list <- function(search_list_path, directory) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {

    # Initializes an empty list to store results for all queries.
    dfs_pmd_all <- list()
    # Read the file "search_list.txt".
    lines <- readLines(search_list_path, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    # Convert file contents into a list of search strings.
    search_list <- stats::setNames(sub("^[^=]+=", "", lines),
                                   sub("=.*", "", lines))

    ncbi_api_key <- Sys.getenv("NCBI_API_KEY")
    ncbi_key_param <- if (nzchar(ncbi_api_key)) paste0("&api_key=", ncbi_api_key) else ""
    pmd_sleep <- if (nzchar(ncbi_api_key)) 0.1 else 0.34

    total_results_pmd <- 0
    # Loop to fetch data for each search string.
    for (search_query in search_list) {
      # Initializes an empty list for current search.
      dfs_pmd <- list()
      # Defines at which reference to start the extraction of a batch.
      next_start_pmd <- 0

      # Call PubMed API to get total results
      # Creates the baseline API URL
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
      # Adds the search information.
      search_url_pmd <- paste0(base_url_pmd,
                               "esearch.fcgi?db=pubmed&term=",
                               utils::URLencode(search_query),
                               "&retmax=1&retmode=json",
                               ncbi_key_param)
      # Extracts the content of the corresponding API call.
      response_pmd <- jsonlite::fromJSON(get_text_retry(search_url_pmd))

      # Check if the search gives results.
      if (is.null(response_pmd$esearchresult$count)) {
        message("No results found for ", search_query)
        next
      }

      # Gives the number of results from the API call.
      max_result_pmd <- as.numeric(response_pmd$esearchresult$count)
      total_results_pmd <- total_results_pmd + max_result_pmd
      message(max_result_pmd, " total results found on PubMed for: ", search_query)

      if (max_result_pmd == 0) {
        message("No results found on PubMed for the saved seach string.")
        next
      }

      # Creates the indicator for the number of batches
      # required to get all the references.
      # "200" represents the max number of references per batch.
      imax_pmd <- ceiling(max_result_pmd / 200)

      # STEP 1: Collect all the unique platform IDs in batches
      for (i in seq_len(imax_pmd)) {
        # Construction of the API call.
        batch_url_pmd <- paste0(
          base_url_pmd,
          "esearch.fcgi?db=pubmed&term=",
          utils::URLencode(search_query),
          "&retstart=",
          next_start_pmd,
          "&retmax=200&retmode=json",
          ncbi_key_param
        )
        response_pmd <- jsonlite::fromJSON(get_text_retry(batch_url_pmd))
        # Check if ID list exists, and extracts the IDs.
        if (!is.null(response_pmd$esearchresult$idlist)) {
          dfs_pmd[[i]] <- data.frame(
            PMID = unlist(response_pmd$esearchresult$idlist)
            )
        } else {
          # When no more IDs are in the call, the extraction stops.
          message("No more results found for ", search_query)
          break
        }

        # Updates starting index for the next batch.
        next_start_pmd <- next_start_pmd + 200

        # Message to indicate the progress of unique ID batch extraction.
        message("Finished batch ", i, " for ", search_query)

      }

      # Combines all unique IDs into one data frame per search string.
      if (length(dfs_pmd) > 0) {
        dfs_pmd_all[[search_query]] <- dplyr::bind_rows(dfs_pmd)
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
    pmd_df <- dplyr::bind_rows(dfs_pmd_all)
    if (nrow(pmd_df) == 0) {
      message("No new record from PubMed retrieved.")
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
    new_log_rows <- data.frame(id = pmd_df$PMID, platform = "PubMed",
                               timestamp = date_suffix, stringsAsFactors = FALSE)
    openxlsx::writeData(history_id, "id_log", rbind(existing_log, new_log_rows))
    # Makes sure there is no duplicates in the extracted IDs.
    pmid_vec <- unique(pmd_df$PMID)
    # Extract the IDs retrieved previously from the in-memory workbook.
    last_list <- openxlsx::readWorkbook(history_id, sheet = "updated_id_list")
    # Converts the list to a vector.
    last_list_vec <- last_list$id
    # Gets only the new IDs.
    pmd_new <- setdiff(pmid_vec, last_list_vec)
    message(length(pmd_new), " new records found among ", total_results_pmd, " total results.")
    # Creates a dataframe with only the new IDs.
    pmd_new_id <- data.frame(id = pmd_new)
    # Adds the new IDs to the current list.
    updated_list <- rbind(last_list, pmd_new_id)
    # STEP 2: Fetch article details in batches of 200 PMIDs per call
    pubmed_results <- list()
    num_doi_pmd <- 0
    base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
    batches_pmd <- split(pmd_new, ceiling(seq_along(pmd_new) / 200))

    for (batch in batches_pmd) {

      fetch_url <- paste0(base_url_pmd,
                          "efetch.fcgi?db=pubmed&id=",
                          paste(batch, collapse = ","),
                          "&retmode=xml",
                          ncbi_key_param)
      fetch_response <- tryCatch(
        xml2::read_xml(get_text_retry(fetch_url)),
        error = function(e) {
          message("FAILED batch starting at PMID ", batch[1], ": ", conditionMessage(e))
          return(NULL)
        }
      )

      if (is.null(fetch_response)) {
        for (pmid in batch) {
          pubmed_results[[length(pubmed_results) + 1]] <- data.frame(
            author = NA_character_, year = NA_character_, title = NA_character_,
            journal = NA_character_, volume = NA_character_, number = NA_character_,
            abstract = NA_character_, doi = NA_character_, pages = NA_character_,
            isbn = NA_character_, source = "PubMed",
            platform_id = pmid, stringsAsFactors = FALSE
          )
        }
        next
      }

      articles <- xml2::xml_find_all(fetch_response, "//PubmedArticle")

      for (article in articles) {

        pmid <- xml2::xml_text(
          xml2::xml_find_first(article, ".//MedlineCitation/PMID"), trim = TRUE
        )

        # Extracts authors.
        author_nodes <- xml2::xml_find_all(article, ".//AuthorList/Author")
        pmd_authors <- if (length(author_nodes) > 0) {
          paste(xml2::xml_text(xml2::xml_find_all(author_nodes, "LastName")),
                collapse = ", ")
        } else {
          NA_character_
        }

        # Extracts year.
        year_node <- xml2::xml_find_first(article, ".//PubDate/Year")
        pmd_year <- xml2::xml_text(year_node, trim = TRUE)
        pmd_year[pmd_year == ""] <- NA_character_

        # Extracts article title.
        title_node <- xml2::xml_find_first(article, ".//ArticleTitle")
        pmd_title <- if (!is.null(title_node) && length(title_node) > 0) {
          xml2::xml_text(title_node)
        } else {
          NA_character_
        }

        # Extracts journal name.
        journal_node <- xml2::xml_find_first(article, ".//Journal/Title")
        pmd_journal <- if (!is.null(journal_node) && length(journal_node) > 0) {
          xml2::xml_text(journal_node)
        } else {
          NA_character_
        }

        # Extracts volume.
        volume_node <- xml2::xml_find_first(article, ".//JournalIssue/Volume")
        pmd_volume <- xml2::xml_text(volume_node, trim = TRUE)
        if (length(pmd_volume) == 0 || identical(pmd_volume, "")) pmd_volume <- NA_character_

        # Extracts issue.
        issue_node <- xml2::xml_find_first(article, ".//JournalIssue/Issue")
        pmd_issue <- xml2::xml_text(issue_node, trim = TRUE)
        if (length(pmd_issue) == 0 || identical(pmd_issue, "")) pmd_issue <- NA_character_

        # Extracts abstract.
        abstract_node <- xml2::xml_find_first(article, ".//AbstractText")
        pmd_abstract <- if (!is.null(abstract_node) && length(abstract_node) > 0) {
          xml2::xml_text(abstract_node)
        } else {
          NA_character_
        }

        # Extracts DOI.
        pmd_doi <- xml2::xml_text(
          xml2::xml_find_first(article, ".//ArticleId[@IdType='doi']"),
          trim = TRUE
        )
        if (length(pmd_doi) == 0 || identical(pmd_doi, "")) pmd_doi <- NA_character_

        # Extracts page range (set to NA if missing).
        pmd_pages <- xml2::xml_text(
          xml2::xml_find_first(article, ".//MedlinePgn"), trim = TRUE
        )
        if (length(pmd_pages) == 0 || identical(pmd_pages, "")) pmd_pages <- NA_character_

        # Extracts ISBN (set to NA if missing; rare in PubMed, applies to book chapters).
        pmd_isbn <- xml2::xml_text(
          xml2::xml_find_first(article, ".//ArticleId[@IdType='isbn']"), trim = TRUE
        )
        if (length(pmd_isbn) == 0 || identical(pmd_isbn, "")) pmd_isbn <- NA_character_

        pubmed_results[[length(pubmed_results) + 1]] <- data.frame(
          author = pmd_authors[1], year = pmd_year[1], title = pmd_title[1],
          journal = pmd_journal[1], volume = pmd_volume[1], number = pmd_issue[1],
          abstract = pmd_abstract[1], doi = pmd_doi[1], pages = pmd_pages[1],
          isbn = pmd_isbn[1], source = "PubMed",
          platform_id = pmid, stringsAsFactors = FALSE
        )

        num_doi_pmd <- num_doi_pmd + 1
        message(paste(pmd_doi, num_doi_pmd, "/", length(pmd_new)))
      }

      Sys.sleep(pmd_sleep)
    }

    pubmed_results <- dplyr::bind_rows(pubmed_results)

    # Writes the list updated with the new IDs only after Step 2 succeeds.
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = updated_list)
    openxlsx::saveWorkbook(history_id,
                           file = history_id_path,
                           overwrite = TRUE)

    return(pubmed_results) # Returns the dataframe with all the references.
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }
}
