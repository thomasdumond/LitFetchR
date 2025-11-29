#' create and save search string by display the number of results from 3 platforms:
#' Web of Science, Scopus and PubMed
#' @return one or more search string(s) in the file "search_list.txt"
#' @export
#' @importFrom utils URLencode


create_save_search <- function(){
  #imports personal API keys stored in your Renvironment
  wos_api_key <- Sys.getenv("WOS_API_KEY")
  scp_api_key <- Sys.getenv("SCP_API_KEY")
  #create list to store accessed data
  search_history <- list()
  results_pmd <- list()
  results_scp <- list()
  results_wos <- list()

  #create the search history sheet and/or document and save the corresponding variables
  sh <- create_search_history()
  history_search <- sh$history_search
  sheet_name <- sh$sheet_name

  repeat {
    # Ask for a search string
    search <- readline(prompt = "Enter your search string (or 'summary' or 'exit'): ")

    # --- NEW EXIT CONDITION ---
    if (tolower(search) == "exit") {
      cat("Exiting search tool.\n")
      break   # stops the repeat loop and ends the function
    }

    #if the answer was 'summary' then it provides the summary of the results of the search
    #it saves the searches in the history file
    #asks to choose which search string to save in search_list.txt
    if (tolower(search) == "summary") {
      # Print search history and results as a table
      search_table <- data.frame(Search_Term = unlist(search_history),
                                 Results_WOS = unlist(results_wos),
                                 Results_SCP = unlist(results_scp),
                                 Results_PMD = unlist(results_pmd))
      print(search_table)
      # Write data to the sheet
      openxlsx::writeData(history_search, sheet_name, search_table)
      #save history excel
      openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE)
      # Ask user to select a previous search
      choice <- as.integer(readline(prompt = "Select the index number for the search string to use in automated retrieval: "))

      if (!is.na(choice) && choice > 0 && choice <= length(search_history)) {
        search <- search_history[[choice]]
        cat("\nSelected search string:", search, "\n")
        # where to store saved search strings
        search_file <- 'search_list.txt'
        search_file <- paste0(getwd(), "/", search_file)

        # Load existing searches if file exists
        if (file.exists(search_file)) {
          search_content <- readLines(search_file, warn = FALSE)
        } else {
          search_content <- character()
        }

        # repeat {
        #   # Ask for a variable name
        #   save_name <- readline(prompt = "Enter a name for the search identification: ")
        #   # Check if the variable already exists
        #   existing_entry <- grep(paste0("^", save_name, "="), search_content, value = TRUE)
        #
        #   if (length(existing_entry) > 0) {
        #     cat("Variable name already exists:\n", existing_entry, "\n")
        #     choice <- readline(prompt = "Enter a different name or type 'overwrite' to replace: ")
        #
        #     if (tolower(choice) == "overwrite") {
        #       # Remove existing entry
        #       search_content <- search_content[!grepl(paste0("^", save_name, "="), search_content)]
        #       break
        #     }
        #   } else {
        #     break
        #   }
        # }


        # Ask if the user wants to save the search string
        save_search <- readline(prompt = "Do you want to save the search string for future use? (yes/no): ")

        if (tolower(save_search) == "yes") {
          repeat {
            # Ask for a variable name
            save_name <- readline(prompt = "Enter a name for the search identification: ")
            # Check if the variable already exists
            existing_entry <- grep(paste0("^", save_name, "="), search_content, value = TRUE)

            if (length(existing_entry) > 0) {
              cat("Variable name already exists:\n", existing_entry, "\n")
              choice <- readline(prompt = "Enter a different name or type 'overwrite' to replace: ")

              if (tolower(choice) == "overwrite") {
                # Remove existing entry
                search_content <- search_content[!grepl(paste0("^", save_name, "="), search_content)]
                break
              }
            } else {
              break
            }
          }


          # Append new search
          save_entry <- paste0(save_name, "=", search)
          search_content <- c(search_content, save_entry)

          # Save back to file
          writeLines(search_content, search_file)

          cat("Search string saved successfully.\n")
        } else {
          cat("Exiting without saving.\n")
        }


      } else {
        cat("Invalid choice. Exiting.\n")
        break
      }

    } else {
      # Save search string to history
      search_history <- append(search_history, search)

      # Call PubMed API
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
      search_url_pmd <- paste0(base_url_pmd, "esearch.fcgi?db=pubmed&term=", utils::URLencode(search), "&retmax=200&retmode=json")
      Sys.sleep(1)  # 1 second delay before the request
      response_pmd <- jsonlite::fromJSON(httr::content(httr::GET(search_url_pmd), "text", encoding = "UTF-8"))
      max_result_pmd <- as.numeric(response_pmd$`esearchresult`$`count`)

      # Call Scopus API
      # Create a Scopus-compatible search string
      search_scp <- gsub("\\bNOT\\b", "AND NOT", search, ignore.case = TRUE)
      search_url_scp <- paste0('https://api.elsevier.com/content/search/scopus?query=', utils::URLencode(paste0('TITLE-ABS-KEY(', search_scp, ')')), '&apiKey=', scp_api_key)
      response_scp <- jsonlite::fromJSON(httr::content(httr::GET(search_url_scp), "text", encoding = "UTF-8"))
      max_result_scp <- as.numeric(response_scp$`search-results`$`opensearch:totalResults`)

      # Call Web of Science Extended API
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=", utils::URLencode(paste0("TS=(", search, ")")))
      response_wos <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)), "text", encoding = "UTF-8"))
      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)

      # Save results
      results_pmd <- append(results_pmd, max_result_pmd)
      results_scp <- append(results_scp, max_result_scp)
      results_wos <- append(results_wos, max_result_wos)

      print(search)
      print(paste0("Web of Science: ", max_result_wos, " results"))
      print(paste0("Scopus: ", max_result_scp, " results"))
      print(paste0("PubMed: ", max_result_pmd, " results"))

    }
  } #closes function
}
