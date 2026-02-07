#' Deduplicates the references from up to three dataframes.
#'
#' @param df1 Dataframe 1 (can be NULL)
#' @param df2 Dataframe 2 (can be NULL)
#' @param df3 Dataframe 3 (can be NULL)
#' @param directory Choose the directory in which
#'  the references deduplication history will be saved.
#' @param open_file Automatically opens the CSV file after reference retrieval.
#' @param dry_run Simulation run option.
#'
#' @return \code{NULL} (invisibly). Called for its side effects:
#'  writes a CSV of deduplicated citations and
#'  an Excel workbook recording the deduplication history.
#'
#' @examples
#' # This is a "dry run" example.
#' # No deduplication will happen.
#' # It only shows how the function should react.
#' dedup_refs(df1 = df_vibrio_wos,
#'            df2 = df_vibrio_scp,
#'            df3 = df_vibrio_pmd,
#'            directory = tempdir(),
#'            open_file = FALSE,
#'            dry_run = TRUE
#'            )
#'
#' @export

dedup_refs <- function(df1 = NULL,
                       df2 = NULL,
                       df3 = NULL,
                       directory,
                       open_file = FALSE,
                       dry_run = FALSE
                       ) {

  if (dry_run) {
    message("This is the message from the dry run showing what you should
    be seeing when the function will be used:
    Warning: The following columns are missing: pages, number, record_id, isbn
    formatting data...
    identifying potential duplicates...
    identified duplicates!
    flagging potential pairs for manual dedup...
    Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
    254 citations loaded...
    14 duplicate citations removed...
    240 unique citations remaining!
    Deduplication script has been executed,
    concatenated deduplicated references had been exported.
    Warning message:
    In add_missing_cols(raw_citations) :
    Search contains missing values for the record_id column.
    A record_id will be created using row numbers"
            )
    return(invisible(NULL))
  }

  #Guard code to inform that the package `ASySD`
  #is necessary to use this function
  if (!requireNamespace("ASySD", quietly = TRUE)) {
    stop(
      "ASySD is required for deduplication but is not installed.\n",
      "Install it with: remotes::install_github('camaradesuk/ASySD')",
      call. = FALSE
    )
  }

  if (missing(directory) || is.null(directory) || !nzchar(directory)) {
    stop("`directory` must be provided (path to your project folder).")
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory)

  # Filters the NULL dataframes.
  dfs <- Filter(function(x) !is.null(x), list(df1, df2, df3))
  # Inform the user when no dataframe is given.
  if (length(dfs) == 0) stop("No dataframes provided to deduplicate.")
  citations <- dplyr::bind_rows(dfs) # Merges the dataframes.

  # System date time.
  date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  # Creates a unique name for the CSV file.
  csv_name <- paste0("citationsCSV_", date_suffix, ".csv")
  csv_path <- file.path(directory, csv_name)

  # Creates a new history for the deduplication.
  hd <- create_dedup_history(directory = directory)
  # Gets the sheet R object.
  history_dedup <- hd$history_dedup
  # Gets the name of the sheets.
  hist_dedup_path <- hd$hist_dedup_path

  # Writes the entire citation list in first sheet.
  openxlsx::writeData(history_dedup, "citations", citations)
  # Saves the sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_path, overwrite = TRUE)

  # Deduplication using `ASySD`, only automatic option.
  results_dedup <- ASySD::dedup_citations(citations,
                                          manual_dedup = FALSE,
                                          merge_citations = TRUE,
                                          user_input = "1")

  # Writes the citation list automatically deduplicated in second sheet.
  openxlsx::writeData(history_dedup, "auto_dedup_unique", results_dedup$unique)
  # Saves the sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_path, overwrite = TRUE)

  # Extract the unique references
  unique_citations <- results_dedup$unique

  # Writes the unique citation list in forth sheet.
  openxlsx::writeData(history_dedup, "unique_citations", unique_citations)
  # Saves the sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_path, overwrite = TRUE)
  # Creates the CSV file only containing the unique citations.
  ASySD::write_citations(unique_citations, type = "csv", filename = csv_path)

  # Remove 'issue' column if present
  if ("issue" %in% names(unique_citations)) unique_citations$issue <- NULL

  # Informs the user that the deduplication was done and exported.
  message("Deduplication script has been executed,
          concatenated deduplicated references had been exported.\n")

  # Automatically opens the CSV if required by the user.
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
  invisible(NULL)
}
