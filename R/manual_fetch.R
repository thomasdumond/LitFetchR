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
#' @details
#' Every retrieval is recorded in the file `run_log.csv` of `directory`
#' (one row per saved search and platform). It stores the exact query
#' submitted to each platform, the date, the package and R versions, the
#' number of results and new records, the status of the request and any
#' error message. For PubMed, it also stores the query translation reported
#' by PubMed. For Web of Science, it also stores the number of records
#' retrieved from each source database.
#'
#' The history of the references already retrieved (`history_id.xlsx`) is
#' only updated once the output file has been written. If a platform fails
#' (for example an expired API key), the other platforms are still saved,
#' a warning names the failed platform(s), and the failed platform is not
#' added to the history, so its references are retrieved again by the next
#' run. If all the selected platforms fail, an error is raised.
#'
#' @return \code{NULL} (invisibly). Called for its side effects: Create a CSV file with the references metadata, a history file of the references retrieved, a history file of the deduplication (if the option is selected) and a run log (`run_log.csv`).
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

  # One identifier shared by all the rows of this run in "run_log.csv".
  run_id <- make_run_id()
  # Rows of the run log, written at the end of the run (or on any exit).
  log_rows <- list()
  flush_log <- function() {
    if (length(log_rows) > 0) {
      flush_run_log(directory, dplyr::bind_rows(log_rows))
      log_rows <<- list()
    }
  }
  on.exit(flush_log(), add = TRUE)

  extractors <- list(wos = extract_wos_list,
                     scp = extract_scp_list,
                     pmd = extract_pmd_list)
  platform_names <- c(wos = "Web of Science", scp = "Scopus", pmd = "PubMed")

  # Extract the metadata of the references retrieved
  # on the platforms selected by user.
  # A platform that fails does not stop the others. Nothing is added
  # to the history at this stage: the pending updates are kept and
  # written only once the output file exists.
  dfs <- list()
  pendings <- list()
  failed <- character(0)
  for (p in selected) {
    res <- tryCatch(
      extractors[[p]](search_list_path, directory,
                      run_id = run_id, commit_history = FALSE),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      failed[[p]] <- conditionMessage(res)
      message("FAILED ", platform_names[[p]], ": ", conditionMessage(res))
      if (inherits(res, "litfetchr_platform_error")) {
        log_rows[[length(log_rows) + 1]] <- res$run_log
      } else {
        log_rows[[length(log_rows) + 1]] <- make_log_rows(
          run_id = run_id, platform = p, search_name = "", search_string = "",
          status = "error", message = conditionMessage(res))
      }
      next
    }
    if (is.null(res)) {
      failed[[p]] <- "No search string saved."
      next
    }
    pendings[p] <- list(attr(res, "pending_history"))
    log_rows[[length(log_rows) + 1]] <- attr(res, "run_log")
    attr(res, "pending_history") <- NULL
    attr(res, "run_log") <- NULL
    dfs[[p]] <- res
  }

  # Stops when no platform could be retrieved.
  if (length(failed) == length(selected)) {
    flush_log()
    stop("All the selected platforms failed. Nothing was saved and the ",
         "history was not updated.\n",
         paste0("  ", platform_names[names(failed)], ": ", shorten(failed),
                collapse = "\n"),
         "\nSee ", file.path(directory, "run_log.csv"), " for details.",
         call. = FALSE)
  }

  df1 <- dfs$wos
  df2 <- dfs$scp
  df3 <- dfs$pmd

  # Writes the output: deduplicated references if the option was selected
  # by user, else a CSV file with all the references.
  # If this fails, the history is left untouched.
  csv_path <- NULL
  tryCatch({
    if (isTRUE(dedup)) {
      dedup_refs(df1, df2, df3, directory = directory, open_file = open_file)

    } else {
      date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
      csv_name <- paste0("citationsCSV_", date_suffix, ".csv")
      csv_path <- file.path(directory, csv_name)

      dfs_out <- Filter(Negate(is.null), list(df1, df2, df3))
      citations <- dplyr::bind_rows(dfs_out)

      utils::write.csv(citations, file = csv_path, row.names = FALSE)
    }
  }, error = function(e) {
    log_rows <<- amend_run_log(
      log_rows, names(dfs),
      paste0("The output file was not written (", conditionMessage(e), "). ",
             "The history was not updated, so these records will be returned ",
             "again on the next run."))
    flush_log()
    stop("The output file could not be written: ", conditionMessage(e),
         "\nThe history was not updated, so the records will be retrieved ",
         "again on the next run. See ", file.path(directory, "run_log.csv"),
         call. = FALSE)
  })

  # The output exists: marks the retrieved references as seen.
  commit_failed <- character(0)
  for (p in names(pendings)) {
    if (is.null(pendings[[p]])) next
    tryCatch(
      commit_id_history(directory, pendings[[p]]),
      error = function(e) {
        commit_failed[[p]] <<- conditionMessage(e)
        log_rows <<- amend_run_log(
          log_rows, p,
          paste0("The output file was written but the history update failed (",
                 conditionMessage(e), "). These records will be returned ",
                 "again on the next run."))
      }
    )
  }

  # Opens the CSV file if the option was selected by user
  if (isTRUE(open_file) && !is.null(csv_path)) {
    tryCatch({
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
    }, error = function(e) {
      warning("The CSV file could not be opened: ", conditionMessage(e),
              call. = FALSE)
    })
  }

  # Reports what did not go as expected.
  partial <- unique(unlist(lapply(log_rows, function(r) {
    r$platform[r$status == "partial"]
  })))
  flush_log()
  message("Run log saved in ", file.path(directory, "run_log.csv"))
  problems <- character(0)
  if (length(failed) > 0) {
    problems <- c(problems, paste0(
      "FAILED, no references retrieved and history not updated (they will ",
      "be fetched again on the next run): ",
      paste0(platform_names[names(failed)], " (", shorten(failed), ")",
             collapse = ", ")))
  }
  if (length(commit_failed) > 0) {
    problems <- c(problems, paste0(
      "History update FAILED for: ",
      paste0(platform_names[names(commit_failed)], " (",
             shorten(commit_failed), ")", collapse = ", "),
      ". The references will be returned again on the next run."))
  }
  if (length(partial) > 0) {
    problems <- c(problems, paste0(
      "Incomplete retrieval for: ",
      paste(platform_names[partial], collapse = ", ")))
  }
  if (length(problems) > 0) {
    warning("LitFetchR retrieval finished with problems.\n",
            paste0("  - ", problems, collapse = "\n"),
            "\nThe other platforms were saved. See ",
            file.path(directory, "run_log.csv"), " for details.",
            call. = FALSE, immediate. = TRUE)
  }

  invisible(NULL)
}

# Shortens and cleans error messages for the console.
shorten <- function(x, n = 200) {
  x <- gsub("\\s+", " ", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

# Marks the successful rows of the given platforms as errors in the run log
# rows (a list of data frames), when the output or the history failed.
amend_run_log <- function(log_rows, platforms, text) {
  lapply(log_rows, function(r) {
    hit <- r$platform %in% platforms & r$status %in% c("success", "partial")
    r$message[hit] <- trimws(paste(text, r$message[hit]))
    r$status[hit] <- "error"
    r
  })
}
