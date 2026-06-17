#' Manual literature retrieval.
#'
#' Retrieves references corresponding to the saved search string(s)
#'  on up to three platforms
#'  (e.g. [Web of Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
#'  [Scopus](https://www.elsevier.com/en-au/products/scopus) and
#'  [PubMed](https://pubmed.ncbi.nlm.nih.gov/)).
#'
#' @param wos Runs the search on Web of Science (TRUE or FALSE).
#' @param scp Runs the search on Scopus (TRUE or FALSE).
#' @param pmd Runs the search on PubMed (TRUE or FALSE).
#' @param directory Choose the directory in which the search string is saved
#'  (Project's directory). That is also where the references metadata will be saved.
#' @param dedup Deduplicates the retrieved references (TRUE or FALSE).
#' @param open_file Automatically opens the CSV file after reference retrieval.
#' @param dry_run Simulation run option.
#'
#' @return \code{NULL} (invisibly). Called for its side effects: Create a CSV file with the references metadata, a history file of the references retrieved and a history file of the deduplication (if the option is selected).
#'
#' @examples
#' # This is a "dry run" example.
#' # No references will actually be scheduled, it only shows how the function should react.
#' manual_fetch(wos = TRUE,
#'              scp = TRUE,
#'              pmd = TRUE,
#'              directory,
#'              dedup = TRUE,
#'              open_file = FALSE,
#'              dry_run = TRUE
#'              )
#'
#'
#' @export

manual_fetch <- function(wos = FALSE,
                         scp = FALSE,
                         pmd = FALSE,
                         directory,
                         dedup = FALSE,
                         open_file = FALSE,
                         dry_run = FALSE
                         ) {

  if (dry_run) {
    message('This is the message from the dry run showing what you should be
            seeing when the function will be used:
              154 total results found on Web of Science for: fish AND "vibrio harveyi" AND diagnostic
              Finished batch 1 of 2
              Finished batch 2 of 2
              File already exists
              149 new records found among 154 total results.
              24 total results found on Scopus for: fish AND "vibrio harveyi" AND diagnostic
              Finished batch 1 of 1
              File already exists
              22 new records found among 24 total results.
              110 total results found on PubMed for: fish AND "vibrio harveyi" AND diagnostic
              Finished batch 1 of 1
              File already exists
              104 new records found among 110 total results.
              Finished batch 1 of 1 (104 / 104 records retrieved)
              Warning: The following columns are missing: record_id
              formatting data...
              identifying potential duplicates...
              identified duplicates!
              275 citations loaded...
              27 duplicate citations removed...
              248 unique citations remaining!
              Deduplication script has been executed,
                        concatenated deduplicated references had been exported.

              Warning message:
              In add_missing_cols(raw_citations) :
                Search contains missing values for the record_id column. A record_id will be created using row numbers'
            )
    return(invisible(NULL))
  }

  if (missing(directory) || is.null(directory) || !nzchar(directory)) {
    stop("`directory` must be provided (path to your project folder).")
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory)

  search_list_path <- file.path(directory, "search_list.txt")
  search_list_path <- normalizePath(search_list_path, mustWork = FALSE)

  # Only runs if search string(s) are saved, else asks the user to do so.
  if (!file.exists(search_list_path)) {
    stop("No search string saved.
         Please save a search string using the function create_save_search().")
  }

  # Builds a list of databases selected by user.
  selected <- c(wos = wos, scp = scp, pmd = pmd)
  selected <- names(selected)[selected]  # keep only TRUE ones

  # Informs the user if no databases were selected.
  if (length(selected) == 0) {
    stop("At least one database must be set to TRUE (wos, scp, pmd).")
  }

  # Create NULL databases to allow `dedup_refs()`
  # to work in case some platforms are not selected.
  df1 <- df2 <- df3 <- NULL

  # Extract the metadata of the references retrieved
  # on the platforms selected by user.
  if ("wos" %in% selected) {
    df1 <- extract_wos_list(search_list_path, directory)
  }
  if ("scp" %in% selected) {
    df2 <- extract_scp_list(search_list_path, directory)
  }
  if ("pmd" %in% selected) {
    df3 <- extract_pmd_list(search_list_path, directory)
  }

  # Deduplicates the results if the option was selected by user.
  if (isTRUE(dedup)) {
    dedup_refs(df1, df2, df3, directory = directory, open_file = open_file)

  # If not, the function returns the CSV file with all the references.
  } else {
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    csv_name <- paste0("citationsCSV_", date_suffix, ".csv")
    csv_path <- file.path(directory, csv_name)

    dfs <- Filter(Negate(is.null), list(df1, df2, df3))
    citations <- dplyr::bind_rows(dfs)

    utils::write.csv(citations, file = csv_path, row.names = FALSE)

    # Opens the CSV file if the option was selected by user
    if (isTRUE(open_file)) {

      if (.Platform$OS.type == "windows") {
        # Windows: opens in default app (e.g. Excel, Calc)
        shell.exec(csv_path)

      } else if (Sys.info()[["sysname"]] == "Darwin") {
        # macOS: opens in default app (e.g. Excel, Calc)
        system2("open", csv_path)

      } else {
        # Linux: opens in default app (e.g. Excel, Calc)
        system2("xdg-open", csv_path)
      }
    }
  }

  invisible(NULL)
}
