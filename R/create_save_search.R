#' Creates and saves search string(s).
#'
#' An interactive function that
#' ask the user to enter a search string and provide
#' the number of results from 3 platforms:
#' Web of Science, Scopus and PubMed. You can then save one
#' or more search strings to retrieve the references later.
#'
#' Type `summary` to display the searches run so far and save one of them
#' (the saved search strings are stored in "search_list.txt", which is
#' required by the retrieval functions). Type `exit` to leave the tool. If
#' searches were run but none was saved during the session, `exit` first
#' offers to save one.
#'
#' @param wos Runs the search on Web of Science (TRUE or FALSE).
#' @param scp Runs the search on Scopus (TRUE or FALSE).
#' @param pmd Runs the search on PubMed (TRUE or FALSE).
#' @param dry_run Simulation run option.
#' @param directory Choose the directory in which the search string
#'  and the search history will be saved.
#' @return \code{NULL} (invisibly). Called for its side effects:
#'  interactive querying and writing search history files.
#'
#' @importFrom utils URLencode
#'
#' @examples
#' # This is a "dry run" example.
#' # No search will be created and no database will be accessed.
#' # It only shows how the function should react.
#' create_save_search(wos = TRUE,
#'                    scp = TRUE,
#'                    pmd = TRUE,
#'                    directory,
#'                    dry_run = TRUE)
#'
#' @export


create_save_search <- function(wos = FALSE,
                               scp = FALSE,
                               pmd = FALSE,
                               directory,
                               dry_run = FALSE) {

  if (dry_run) {
    message(
  'This is the message from the dry run showing
  what you should be seeing when the function will be used:
  History had been created.
  Enter your search string (or "summary" or "exit"): fish
  [1] "fish"
  [1] "Web of Science: 1793296 results"
  [1] "Scopus: 718644 results"
  [1] "PubMed: 384742 results"
  Enter your search string (or "summary" or "exit"):
  fish AND "vibrio harveyi"
  [1] "fish AND \"vibrio harveyi\""
  [1] "Web of Science: 2084 results"
  [1] "Scopus: 1080 results"
  [1] "PubMed: 727 results"
  Enter your search string (or "summary" or "exit"):
  fish AND "vibrio harveyi" AND diagnostic
  [1] "fish AND \"vibrio harveyi\" AND diagnostic"
  [1] "Web of Science: 126 results"
  [1] "Scopus: 22 results"
  [1] "PubMed: 106 results"
  Enter your search string (or "summary" or "exit"): summary
                                 Search_Term Results_WOS Results_SCP Results_PMD
  1                                     fish     1793296      718644      384742
  2                fish AND "vibrio harveyi"        2084        1080         727
  3 fish AND "vibrio harveyi" AND diagnostic         126          22         106
  Do you want to save a search string for automated retrieval? (yes/no): yes
  Select the index number of the search string to save (Enter to cancel): 3

  Selected search string: fish AND "vibrio harveyi" AND diagnostic

  Enter a name for the search identification: fish_vibrio
  Search string saved successfully.
  Enter your search string (or "summary" or "exit"): exit
  Exiting search tool.

  If you type "exit" before any search string was saved, the summary table
  is displayed again and you are offered to save a search string:
  Enter your search string (or "summary" or "exit"): exit
  No search string has been saved yet. Retrieval with manual_fetch() requires a saved search.
  Do you want to save a search string for automated retrieval? (yes/no):'
            )
    return(invisible(NULL))
  }

  if (missing(directory) || is.null(directory) || !nzchar(directory)) {
    stop("`directory` must be provided (path to your project folder).")
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory)

  # Require at least one platform
  if (!isTRUE(wos) && !isTRUE(scp) && !isTRUE(pmd)) {
    stop("Select at least one platform: wos, scp, or pmd.", call. = FALSE)
  }

  # Read only the keys you need
  wos_api_key <- NULL
  scp_api_key <- NULL

  #imports personal API keys stored in your Renvironment
  if (isTRUE(wos)) {
    wos_api_key <- Sys.getenv("wos_api_key")
    if (!nzchar(wos_api_key)) {
      stop("Web of Science API key not found. Set env var `wos_api_key` using `save_api_key`.",
           call. = FALSE)
    }
  }

  scp_headers <- NULL
  if (isTRUE(scp)) {
    scp_api_key <- Sys.getenv("scp_api_key")
    if (!nzchar(scp_api_key)) {
      stop("Scopus API key not found. Set env var `scp_api_key` using `save_api_key`.",
           call. = FALSE)
    }
    # Auth via headers, matching extract_scp_list (X-ELS-APIKey, optional insttoken).
    scp_insttoken <- Sys.getenv("scp_insttoken")
    scp_headers <- c("X-ELS-APIKey" = scp_api_key,
                     "Accept" = "application/json",
                     if (nzchar(scp_insttoken)) c("X-ELS-Insttoken" = scp_insttoken))
  }

  #create lists to store data
  search_history <- list() #the search strings from the user
  results_pmd <- list() #number of results from PubMed
  results_scp <- list() #number of results from Scopus
  results_wos <- list() #number of results from Web of Science

  #create the search history sheet and/or document and
  #save the corresponding variables
  #if the file already exist, it will only create a new sheet
  sh <- create_search_history(directory)
  history_search <- sh$history_search #new sheet R object created
  sheet_name <- sh$sheet_name #name of the new sheet R object

  # Location of the saved search strings
  search_file <- normalizePath(file.path(directory, "search_list.txt"),
                               mustWork = FALSE)
  # Stop early if an existing file is malformed, so that the user can fix it
  # before running searches.
  if (file.exists(search_file)) read_search_list(search_file, allow_empty = TRUE)

  # TRUE once a search string was saved during this session
  saved_any <- FALSE

  # Table of the searches run so far and their number of results
  make_search_table <- function() {
    data.frame(Search_Term = unlist(search_history),
               Results_WOS = unlist(results_wos),
               Results_SCP = unlist(results_scp),
               Results_PMD = unlist(results_pmd))
  }

  repeat {
    # Ask the user for a search string
    search <- trimws(readline(
      prompt = "Enter your search string (or 'summary' or 'exit'): "))

    # Exit condition (users can also press 'escape')
    if (tolower(search) == "exit") {
      # Searches were run but none was saved: offer to save one, because
      # "search_list.txt" is required by the retrieval functions.
      if (!saved_any && length(search_history) > 0) {
        message("\nNo search string has been saved yet. ",
                "Retrieval with manual_fetch() requires a saved search.\n")
        outcome <- offer_save_search(make_search_table(), search_history,
                                     search_file, context = "exit")
        if (outcome == "saved") saved_any <- TRUE
        # Back to the search prompt so that the user can retry.
        if (outcome == "cancelled") next
        if (outcome == "declined") {
          message("No search string saved. Run create_save_search() again ",
                  "to save one before retrieving references.\n")
        }
      }
      message("Exiting search tool.\n")
      break   # stops the repeat loop and ends the function
    }

    # Ignore empty input
    if (!nzchar(search)) {
      message("Empty search string ignored.\n")
      next
    }

    # Summarise the search(es)
    ##########################

    # If the answer was 'summary' then it provides the summary
    #of the results of the search(es)
    if (tolower(search) == "summary") {
      if (length(search_history) == 0) {
        message("No search has been run yet.\n")
      } else {
        outcome <- offer_save_search(make_search_table(), search_history,
                                     search_file, context = "summary")
        if (outcome == "saved") saved_any <- TRUE
      }

    } else {

      # Run the search on the databases
      #################################

      # Save search string to history
      search_history <- append(search_history, search)

      # Default values when a platform is not selected
      max_result_wos <- NA_real_
      max_result_scp <- NA_real_
      max_result_pmd <- NA_real_

      # Call PubMed API
      if (isTRUE(pmd)) {
      query_pmd <- build_platform_query(search, "pmd")
      max_result_pmd <- platform_count("PubMed", query_pmd, function() {
        base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
        #URL accessing PubMed API
        search_url_pmd <- paste0(base_url_pmd,
                                 "esearch.fcgi?db=pubmed&term=",
                                 utils::URLencode(query_pmd),
                                 "&retmax=200&retmode=json")
        response_pmd <- jsonlite::fromJSON(get_text_retry(search_url_pmd))
        # Extract the total number of results
        as.numeric(response_pmd$`esearchresult`$`count`)
      })
      }

      # Call Scopus API
      # Scopus uses "AND NOT", handled in build_platform_query()
      if (isTRUE(scp)) {
      query_scp <- build_platform_query(search, "scp")
      max_result_scp <- platform_count("Scopus", query_scp, function() {
        #URL accessing scopus API (auth via X-ELS-APIKey header, see scp_headers)
        search_url_scp <- paste0("https://api.elsevier.com/content/search/scopus?query=",
                                 utils::URLencode(query_scp)
                                 )
        #stores the metadata accessed
        response_scp <- jsonlite::fromJSON(get_text_retry(search_url_scp, headers = scp_headers))
        # Extract the total number of results
        as.numeric(response_scp$`search-results`$`opensearch:totalResults`)
      })
      }

      # Call Web of Science Extended API
      if (isTRUE(wos)) {
      query_wos <- build_platform_query(search, "wos")
      max_result_wos <- platform_count("Web of Science", query_wos, function() {
        search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=",
                                 utils::URLencode(query_wos)
                                 ) #URL accessing Web of Science API
        #stores the metadata accessed
        response_wos <- jsonlite::fromJSON(get_text_retry(search_url_wos,
                                                          headers = c("X-ApiKey" = wos_api_key)))
        # Extract the total number of results
        as.numeric(response_wos$QueryResult$RecordsFound)
      })
      }

      # Stores the results for each database
      results_pmd <- append(results_pmd, max_result_pmd)
      results_scp <- append(results_scp, max_result_scp)
      results_wos <- append(results_wos, max_result_wos)

      # Save this search to history immediately so it is recorded even if the
      # user exits without typing 'summary'.
      history_search_path <- file.path(directory, "history_search.xlsx")
      new_row <- data.frame(
        Search_Term = search,
        Results_WOS = max_result_wos,
        Results_SCP = max_result_scp,
        Results_PMD = max_result_pmd,
        timestamp   = format(Sys.time(), "%Y-%m-%d-%H%M%S"),
        stringsAsFactors = FALSE
      )
      existing_searches <- openxlsx::readWorkbook(history_search,
                                                  sheet = "search_history")
      openxlsx::writeData(history_search, "search_history",
                          rbind(existing_searches, new_row))
      openxlsx::saveWorkbook(history_search,
                             file = history_search_path,
                             overwrite = TRUE)

      # Print the search string following the results from
      # each database.
      message(search)
      if (isTRUE(wos)) message(count_label("Web of Science", max_result_wos))
      if (isTRUE(scp)) message(count_label("Scopus", max_result_scp))
      if (isTRUE(pmd)) message(count_label("PubMed", max_result_pmd))

    }
  }
  invisible(NULL)
}


# Internal helpers

# Runs the request of one platform and returns the number of results.
# If the platform rejects the search (for example a syntax error), the problem
# is reported with the query that was sent and NA is returned, so that the
# other platforms and the session carry on.
platform_count <- function(label, query, request) {
  tryCatch(request(), error = function(e) {
    message(label, " could not run this search: ", conditionMessage(e),
            "\n  Query sent: ", query,
            "\n  Check the syntax (AND, OR, NOT, parentheses, quoted phrases).")
    NA_real_
  })
}

# Text shown for the number of results of a platform.
count_label <- function(label, n) {
  if (is.na(n)) {
    paste0(label, ": no result (the search failed)")
  } else {
    paste0(label, ": ", n, " results")
  }
}
##################

# Checks a name for a saved search. Returns NULL when the name is valid,
# otherwise a message describing the problem.
check_search_name <- function(name) {
  if (!nzchar(name)) {
    return("The name cannot be empty.")
  }
  if (grepl("=", name, fixed = TRUE)) {
    return("The name cannot contain '=' (it separates the name from the search string).")
  }
  NULL
}

# Shows the summary table of the searches and offers to save one of them to
# "search_list.txt". Used by both 'summary' and 'exit' in create_save_search().
#
# search_table   Data frame of the searches and their number of results.
# search_history List of the search strings (same order as the table).
# search_file    Path to "search_list.txt".
# context        "summary" or "exit" (only changes the message when declined).
#
# Returns "saved", "declined" (the user answered no) or "cancelled" (the user
# left the index question empty, nothing was saved).
offer_save_search <- function(search_table, search_history, search_file,
                              context = c("summary", "exit")) {
  context <- match.arg(context)

  # Print search history and results as a table
  print(search_table)

  # Ask whether the user wants to save a search string
  save_search <- trimws(readline(
    prompt = "Do you want to save a search string for automated retrieval? (yes/no): "))

  if (!tolower(save_search) %in% c("yes", "y")) {
    if (context == "summary") message("Returning to search.\n")
    return("declined")
  }

  # Ask which search string to save. An invalid answer asks the question
  # again, and an empty answer (Enter) cancels.
  repeat {
    answer <- trimws(readline(
      prompt = "Select the index number of the search string to save (Enter to cancel): "))
    if (!nzchar(answer)) {
      message("Nothing saved.\n")
      return("cancelled")
    }
    choice <- suppressWarnings(as.integer(answer))
    if (!is.na(choice) && choice >= 1 && choice <= length(search_history)) break
    message("Invalid choice. Enter a number from 1 to ", length(search_history),
            ".\n")
  }

  search <- search_history[[choice]]
  message("\nSelected search string: ", search, "\n")

  # Load existing searches if the file exists. A malformed file stops here
  # with an explanation instead of being silently rewritten.
  if (file.exists(search_file)) {
    read_search_list(search_file, allow_empty = TRUE)
    search_content <- readLines(search_file, warn = FALSE)
    search_content <- search_content[nzchar(trimws(search_content))]
  } else {
    search_content <- character()
  }

  ask_name <- function() {
    trimws(readline(prompt = "Enter a name for the search identification: "))
  }

  save_name <- ask_name()
  repeat {
    problem <- check_search_name(save_name)
    if (!is.null(problem)) {
      message(problem, "\n")
      save_name <- ask_name()
      next
    }

    # Check if the name already exists in "search_list.txt"
    # (fixed matching, so that the name is never read as a regular expression)
    is_existing <- startsWith(search_content, paste0(save_name, "="))
    if (!any(is_existing)) break

    message("Variable name already exists:\n",
            paste(search_content[is_existing], collapse = "\n"), "\n")
    overwrite_choice <- trimws(readline(
      prompt = "Enter a different name or type 'overwrite' to replace: "))

    if (tolower(overwrite_choice) == "overwrite") {
      search_content <- search_content[!is_existing]
      break
    }
    # Anything else is taken as the different name and checked again.
    save_name <- overwrite_choice
  }

  # Add the new search string to "search_list.txt" content
  save_entry <- paste0(save_name, "=", search)
  search_content <- c(search_content, save_entry)

  # Save "search_list.txt" with the new content
  writeLines(search_content, search_file)

  message("Search string saved successfully.\n")
  "saved"
}
