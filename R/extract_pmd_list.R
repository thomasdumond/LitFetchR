#' Extracts the metadata from the new references found on PubMed
#' based on the search string(s) saved in "search_list.txt".
#' @param search_list_path Path to "search_list.txt".
#' @param directory Choose the directory in which
#'  the references identification history will be saved.
#' @param run_id Identifier shared by all the rows of one retrieval in
#'  "run_log.csv" (created if \code{NULL}).
#' @param commit_history If \code{TRUE} (default), the references
#'  identification history and the run log are written before returning.
#'  If \code{FALSE}, nothing is written: the pending history update and the
#'  run log rows are attached to the returned data frame (attributes
#'  \code{pending_history} and \code{run_log}) so that the caller can write
#'  them once the output file exists (used by [manual_fetch()]).
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
#' If any search fails, an error of class \code{litfetchr_platform_error}
#' is raised and nothing is added to the history.
#'
#' @importFrom utils URLencode
#' @importFrom stats setNames
#' @keywords internal

extract_pmd_list <- function(search_list_path, directory,
                             run_id = NULL, commit_history = TRUE) {

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (file.exists(search_list_path)) {

    # Read and validate the file "search_list.txt".
    search_list <- read_search_list(search_list_path)

    ncbi_api_key <- Sys.getenv("ncbi_api_key")
    ncbi_key_param <- if (nzchar(ncbi_api_key)) paste0("&api_key=", ncbi_api_key) else ""
    pmd_sleep <- if (nzchar(ncbi_api_key)) 0.1 else 0.34

    run_extraction(
      platform = "pmd",
      search_list = search_list,
      directory = directory,
      run_id = run_id,
      commit_history = commit_history,
      search_fn = function(search_list, state) {
        search_pmd(search_list, state, ncbi_key_param)
      },
      fetch_fn = function(new_ids, state) {
        fetch_pmd(new_ids, state, ncbi_key_param, pmd_sleep)
      }
    )
  } else {
    # Asks the user to save a search string with the adequate function.
    message("No search string saved.
            Please save a search string using the function create_save_search().")
    invisible(NULL)
  }
}

# Summarises the errors and warnings that PubMed reports for a search
# (e.g. phrases not found) in one line.
pubmed_notes <- function(esearchresult) {
  x <- c(esearchresult$errorlist, esearchresult$warninglist)
  if (length(x) == 0) return("")
  nm <- names(x)
  if (is.null(nm)) nm <- rep("note", length(x))
  vals <- vapply(x, function(v) paste(unlist(v), collapse = ", "), character(1))
  paste0("PubMed ", nm, ": ", vals, collapse = " | ")
}

# STEP 1: Collect all the unique platform IDs in batches, for each search.
search_pmd <- function(search_list, state, ncbi_key_param) {

  # Creates the baseline API URL
  base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"

  # Loop to fetch data for each search string.
  for (qi in seq_along(search_list)) {
    state$current <- qi
    search_query <- search_list[[qi]]
    # IDs found for the current search.
    ids_q <- character(0)
    # Defines at which reference to start the extraction of a batch.
    next_start_pmd <- 0
    term_pmd <- encode_query(build_platform_query(search_query, "pmd"))

    # Call PubMed API to get total results
    # Adds the search information.
    search_url_pmd <- paste0(base_url_pmd,
                             "esearch.fcgi?db=pubmed&term=",
                             term_pmd,
                             "&retmax=1&retmode=json",
                             ncbi_key_param)
    # Extracts the content of the corresponding API call.
    response_pmd <- jsonlite::fromJSON(get_text_retry(search_url_pmd))

    # Check if the search gives results.
    if (is.null(response_pmd$esearchresult$count)) {
      api_error <- c(response_pmd$error, response_pmd$esearchresult$ERROR)
      stop("PubMed returned no result count for: ", search_query,
           if (length(api_error) > 0) paste0(" (", paste(api_error, collapse = " "), ")"),
           call. = FALSE)
    }

    query_translation <- response_pmd$esearchresult$querytranslation
    if (is.null(query_translation)) query_translation <- ""
    query_note <- pubmed_notes(response_pmd$esearchresult)

    # Gives the number of results from the API call.
    max_result_pmd <- as.numeric(response_pmd$esearchresult$count)
    message(max_result_pmd, " total results found on PubMed for: ", search_query)

    if (max_result_pmd == 0) {
      message("No results found on PubMed for the saved seach string.")
      state$queries[[qi]] <- list(total = 0, ids = character(0),
                                  translation = query_translation,
                                  note = query_note)
      next
    }

    # Creates the indicator for the number of batches
    # required to get all the references.
    # "200" represents the max number of references per batch.
    imax_pmd <- ceiling(max_result_pmd / 200)

    for (i in seq_len(imax_pmd)) {
      # Construction of the API call.
      batch_url_pmd <- paste0(
        base_url_pmd,
        "esearch.fcgi?db=pubmed&term=",
        term_pmd,
        "&retstart=",
        next_start_pmd,
        "&retmax=200&retmode=json",
        ncbi_key_param
      )
      response_pmd <- jsonlite::fromJSON(get_text_retry(batch_url_pmd))
      # Check if ID list exists, and extracts the IDs.
      if (!is.null(response_pmd$esearchresult$idlist)) {
        ids_q <- c(ids_q, as.character(unlist(response_pmd$esearchresult$idlist)))
      } else {
        # When no more IDs are in the call, the extraction stops.
        message("No more results found for ", search_query)
        break
      }

      # Updates starting index for the next batch.
      next_start_pmd <- next_start_pmd + 200

      # Message to indicate the progress of unique ID batch extraction.
      message("Finished batch ", i, " of ", imax_pmd)

    }

    state$queries[[qi]] <- list(total = max_result_pmd, ids = ids_q,
                                translation = query_translation,
                                note = query_note)
  }
  invisible(NULL)
}

# STEP 2: Fetch article details in batches of 200 PMIDs per call.
# Only the new IDs are fetched.
fetch_pmd <- function(pmd_new, state, ncbi_key_param, pmd_sleep) {
  pubmed_results <- list()
  num_doi_pmd <- 0
  base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  batches_pmd <- split(pmd_new, ceiling(seq_along(pmd_new) / 200))
  n_batches_pmd <- length(batches_pmd)

  for (b in seq_along(batches_pmd)) {
    batch <- batches_pmd[[b]]

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
      # These PMIDs are kept out of the history so the next run fetches them again.
      state$failed_ids <- c(state$failed_ids, batch)
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
    }

    message("Finished batch ", b, " of ", n_batches_pmd,
            " (", num_doi_pmd, " / ", length(pmd_new), " records retrieved)")
    Sys.sleep(pmd_sleep)
  }

  if (length(pubmed_results) == 0) return(empty_records())
  dplyr::bind_rows(pubmed_results)
}
