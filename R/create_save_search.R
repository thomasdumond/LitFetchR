#' Creates and saves search string(s).
#'
#' An interactive function that
#' ask the user to enter a search string and provide
#' the number of results from 3 platforms:
#' Web of Science, Scopus and PubMed. You can then save one
#' or more search strings to retrieve the references later.
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
  Select the index number for the search string to use in automated retrieval: 3

  Selected search string: fish AND "vibrio harveyi" AND diagnostic
  Do you want to save the search string for future use? (yes/no): yes
  Enter a name for the search identification: fish_vibrio
  Search string saved successfully.
  Enter your search string (or "summary" or "exit"): exit
  Exiting search tool.'
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

  repeat {
    # Ask the user for a search string
    search <- readline(
      prompt = "Enter your search string (or 'summary' or 'exit'): ")

    # Exit condition (users can also press 'escape')
    if (tolower(search) == "exit") {
      message("Exiting search tool.\n")
      break   # stops the repeat loop and ends the function
    }

    # Summarise the search(es)
    ##########################

    # If the answer was 'summary' then it provides the summary
    #of the results of the search(es)
    if (tolower(search) == "summary") {
      # Print search history and results as a table
      search_table <- data.frame(Search_Term = unlist(search_history),
                                 Results_WOS = unlist(results_wos),
                                 Results_SCP = unlist(results_scp),
                                 Results_PMD = unlist(results_pmd))
      print(search_table)

      # Ask whether the user wants to save a search string
      save_search <- readline(
        prompt = "Do you want to save a search string for automated retrieval? (yes/no): ")

      if (tolower(save_search) == "yes") {
        # Ask which search string to save
        choice <- as.integer(
          readline(
            prompt = "Select the index number of the search string to save: "))

        if (!is.na(choice) && choice > 0 && choice <= length(search_history)) {
          search <- search_history[[choice]]
          message("\nSelected search string: ", search, "\n")

          # Select where to store saved search strings
          search_file <- file.path(directory, "search_list.txt")
          search_file <- normalizePath(search_file, mustWork = FALSE)

          # Load existing searches if file exists
          if (file.exists(search_file)) {
            search_content <- readLines(search_file, warn = FALSE)
          } else {
            search_content <- character()
          }

          repeat {
            # Ask a name for the saved search
            save_name <- readline(prompt = "Enter a name for the search identification: ")
            # Check if the name already exists in "search_list.txt"
            existing_entry <- grep(paste0("^", save_name, "="), search_content, value = TRUE)

            if (length(existing_entry) > 0) {
              message("Variable name already exists:\n", existing_entry, "\n")
              overwrite_choice <- readline(
                prompt = "Enter a different name or type 'overwrite' to replace: ")

              if (tolower(overwrite_choice) == "overwrite") {
                search_content <- search_content[!grepl(paste0("^", save_name, "="),
                                                        search_content)]
                break
              }
            } else {
              break
            }
          }

          # Add the new search string to "search_list.txt" content
          save_entry <- paste0(save_name, "=", search)
          search_content <- c(search_content, save_entry)

          # Save "search_list.txt" with the new content
          writeLines(search_content, search_file)

          message("Search string saved successfully.\n")

        } else {
          message("Invalid choice. Nothing saved.\n")
        }

      } else {
        message("Returning to search.\n")
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
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
      #URL accessing PubMed API
      search_url_pmd <- paste0(base_url_pmd,
                               "esearch.fcgi?db=pubmed&term=",
                               utils::URLencode(search),
                               "&retmax=200&retmode=json")
      response_pmd <- jsonlite::fromJSON(get_text_retry(search_url_pmd))
      # Extract the total number of results
      max_result_pmd <- as.numeric(response_pmd$`esearchresult`$`count`)
      }

      # Call Scopus API
      #scopus use "AND NOT", so transforms "NOT" to "AND NOT"
      if (isTRUE(scp)) {
      search_scp <- gsub("\\bNOT\\b",
                         "AND NOT",
                         search,
                         ignore.case = TRUE
                         )
      #URL accessing scopus API (auth via X-ELS-APIKey header, see scp_headers)
      search_url_scp <- paste0("https://api.elsevier.com/content/search/scopus?query=",
                               utils::URLencode(paste0("TITLE-ABS-KEY(", search_scp, ")"))
                               )
      #stores the metadata accessed
      response_scp <- jsonlite::fromJSON(get_text_retry(search_url_scp, headers = scp_headers))
      # Extract the total number of results
      max_result_scp <- as.numeric(response_scp$`search-results`$`opensearch:totalResults`)
      }

      # Call Web of Science Extended API
      if (isTRUE(wos)) {
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=",
                               utils::URLencode(paste0("TS=(", search, ")"))
                               ) #URL accessing Web of Science API
      #stores the metadata accessed
      response_wos <- jsonlite::fromJSON(get_text_retry(search_url_wos,
                                                        headers = c("X-ApiKey" = wos_api_key)))
      # Extract the total number of results
      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)
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
      if (isTRUE(wos)) message(paste0("Web of Science: ", max_result_wos, " results"))
      if (isTRUE(scp)) message(paste0("Scopus: ", max_result_scp, " results"))
      if (isTRUE(pmd)) message(paste0("PubMed: ", max_result_pmd, " results"))

    }
  }
  invisible(NULL)
}
