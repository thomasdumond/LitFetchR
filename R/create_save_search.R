#' create and save search string
#'
#'`create_save_search`  is an interactive function that
#' ask the user to enter a search string and provide
#' the number of results from 3 platforms:
#' Web of Science, Scopus and PubMed. You can then save one
#' or more search strings to retrieve the references later.
#'
#' @return one or more search string(s) in the file "search_list.txt"
#' @importFrom utils URLencode
#'
#' @export


create_save_search <- function(){

  #imports personal API keys stored in your Renvironment
  wos_api_key <- Sys.getenv("WOS_API_KEY")
  scp_api_key <- Sys.getenv("SCP_API_KEY")

  #create lists to store data
  search_history <- list() #the search strings from the user
  results_pmd <- list() #number of results from PubMed
  results_scp <- list() #number of results from Scopus
  results_wos <- list() #number of results from Web of Science

  #create the search history sheet and/or document and save the corresponding variables
  sh <- create_search_history() #if the file already exist, it will only create a new sheet
  history_search <- sh$history_search #new sheet R object created
  sheet_name <- sh$sheet_name #name of the new sheet R object

  repeat {
    # Ask the user for a search string
    search <- readline(prompt = "Enter your search string (or 'summary' or 'exit'): ")

    # Exit condition (users can also press 'escape')
    if (tolower(search) == "exit") {
      cat("Exiting search tool.\n")
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

      # The search(es) results are saved in the "search_history.xlsx"
      openxlsx::writeData(history_search,
                          sheet_name,
                          search_table
                          ) #write data to the sheet
      openxlsx::saveWorkbook(history_search,
                             "history_search.xlsx",
                             overwrite = TRUE
                             ) #save history excel

      # The user chooses which search string(s) to save in "search_list.txt"
      # Ask user to select a previous search
      choice <- as.integer(readline(prompt = "Select the index number for the search string to use in automated retrieval: "))

      if (!is.na(choice) && choice > 0 && choice <= length(search_history)) {
        search <- search_history[[choice]]
        cat("\nSelected search string:", search, "\n")

        # Select where to store saved search strings
        search_file <- 'search_list.txt'
        search_file <- paste0(getwd(), "/", search_file)

        # Load existing searches if file exists
        if (file.exists(search_file)) {
          search_content <- readLines(search_file, warn = FALSE)
        } else {
          search_content <- character() #If the file does not exist we start by creating a character vector
        }

        # Ask if the user wants to save the search string
        save_search <- readline(prompt = "Do you want to save the search string for future use? (yes/no): ")

        if (tolower(save_search) == "yes") {
          repeat {
            # Ask a name for the saved search
            save_name <- readline(prompt = "Enter a name for the search identification: ")
            # Check if the name already exists in "search_list.txt"
            existing_entry <- grep(paste0("^", save_name, "="), search_content, value = TRUE)

            #Ask the user to chose another name or overwrite the search string if it already exists
            if (length(existing_entry) > 0) {
              cat("Variable name already exists:\n", existing_entry, "\n")
              choice <- readline(prompt = "Enter a different name or type 'overwrite' to replace: ")

              # Remove existing entry
              if (tolower(choice) == "overwrite") {
                search_content <- search_content[!grepl(paste0("^", save_name, "="), search_content)]
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

          cat("Search string saved successfully.\n")

        } else {
          cat("Exiting without saving.\n")
        }


      } else {
        cat("Invalid choice. Exiting.\n")
        break
      }

    } else {

      # Run the search on the databases
      #################################

      # Save search string to history
      search_history <- append(search_history, search)

      # Call PubMed API
      base_url_pmd <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
      search_url_pmd <- paste0(base_url_pmd,
                               "esearch.fcgi?db=pubmed&term=",
                               utils::URLencode(search),
                               "&retmax=200&retmode=json") #URL accessing PubMed API
      Sys.sleep(1)  # 1 second delay before the request to make sure the right URL is called
      response_pmd <- jsonlite::fromJSON(httr::content(httr::GET(search_url_pmd),
                                                       "text",
                                                       encoding = "UTF-8"
                                                       )
                                         ) #stores the metadata accessed
      # Extract the total number of results
      max_result_pmd <- as.numeric(response_pmd$`esearchresult`$`count`)

      # Call Scopus API
      search_scp <- gsub("\\bNOT\\b",
                         "AND NOT",
                         search,
                         ignore.case = TRUE
                         ) #scopus use "AND NOT", so transforms "NOT" to "AND NOT"
      search_url_scp <- paste0('https://api.elsevier.com/content/search/scopus?query=',
                               utils::URLencode(paste0('TITLE-ABS-KEY(', search_scp, ')')),
                               '&apiKey=',
                               scp_api_key
                               ) #URL accessing scopus API
      response_scp <- jsonlite::fromJSON(httr::content(httr::GET(search_url_scp),
                                                       "text",
                                                       encoding = "UTF-8"
                                                       )
                                         ) #stores the metadata accessed
      # Extract the total number of results
      max_result_scp <- as.numeric(response_scp$`search-results`$`opensearch:totalResults`)

      # Call Web of Science Extended API
      search_url_wos <- paste0("https://wos-api.clarivate.com/api/wos/?databaseId=WOK&usrQuery=",
                               utils::URLencode(paste0("TS=(", search, ")"))
                               ) #URL accessing Web of Science API
      response_wos <- jsonlite::fromJSON(httr::content(httr::GET(search_url_wos, httr::add_headers("X-ApiKey" = wos_api_key)),
                                                       "text",
                                                       encoding = "UTF-8"
                                                       )
                                         ) #stores the metadata accessed
      # Extract the total number of results
      max_result_wos <- as.numeric(response_wos$QueryResult$RecordsFound)

      # Stores the results for each database
      results_pmd <- append(results_pmd, max_result_pmd)
      results_scp <- append(results_scp, max_result_scp)
      results_wos <- append(results_wos, max_result_wos)

      # Print the search string following the results from
      # each database.
      print(search)
      print(paste0("Web of Science: ", max_result_wos, " results"))
      print(paste0("Scopus: ", max_result_scp, " results"))
      print(paste0("PubMed: ", max_result_pmd, " results"))

    }
  }
}
