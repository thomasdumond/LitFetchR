#' Deduplicates the references from up to three dataframes.
#'
#' @param df1 Dataframe 1 (can be NULL)
#' @param df2 Dataframe 2 (can be NULL)
#' @param df3 Dataframe 3 (can be NULL)
#' @param open_file Automatically opens the CSV file after reference retrieval.
#' @param dry_run Simulation run option.
#'
#' @return A CSV file containing all the new references deduplicated
#' and the history of the deduplication in an excel file.
#'
#' @examples
#' # This is a "dry run" example.
#' # No deduplication will happen.
#' # It only shows how the function should react.
#' dedup_refs(df_vibrio_wos,
#'            df_vibrio_scp,
#'            df_vibrio_pmd,
#'            dry_run = TRUE
#'            )
#'
#' @export

dedup_refs <- function(df1 = NULL,
                       df2 = NULL,
                       df3 = NULL,
                       open_file = FALSE,
                       dry_run = FALSE
                       ){

  if (dry_run) {
    message("Warning: The following columns are missing: pages, number, record_id, isbn
    formatting data...
    identifying potential duplicates...
    identified duplicates!
    flagging potential pairs for manual dedup...
    Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
    254 citations loaded...
    14 duplicate citations removed...
    240 unique citations remaining!
    Deduplication script has been executed, concatenated deduplicated references had been exported.
    Warning message:
    In add_missing_cols(raw_citations) :
    Search contains missing values for the record_id column.
    A record_id will be created using row numbers"
            )
    return(invisible(NULL))
  }

  # Guard code to inform that the package `ASySD` in necessary to use this function
  if (!requireNamespace("ASySD", quietly = TRUE)) {
    stop(
      "ASySD is required for deduplication but is not installed.\n",
      "Install it with: remotes::install_github('camaradesuk/ASySD')",
      call. = FALSE
    )
  }

  dfs <- Filter(function(x) !is.null(x), list(df1, df2, df3)) # Filters the NULL dataframes.
  if (length(dfs) == 0) stop("No dataframes provided to deduplicate.") # Inform the user when no dataframe is given.
  citations <- dplyr::bind_rows(dfs) # Merges the dataframes.

  date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S") # System date time.
  csv_name <- paste0("citationsCSV_", date_suffix,".csv") # Creates a unique name for the CSV file.

  hd <- create_dedup_history() # Creates a new history for the deduplication.
  history_dedup <- hd$history_dedup # Gets the sheet R object.
  hist_dedup_name <- hd$hist_dedup_name # Gets the name of the sheets.

  openxlsx::writeData(history_dedup, "citations", citations) # Writes the entire citation list in first sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE) # Saves the sheet.

  # Deduplication using `ASySD`, only automatic option.
  results_dedup <- ASySD::dedup_citations(citations,
                                          manual_dedup = TRUE,
                                          merge_citations = TRUE,
                                          user_input = "1")

  openxlsx::writeData(history_dedup, "auto_dedup_unique", results_dedup$unique) # Writes the citation list automatically deduplicated in second sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE) # Saves the sheet.
  openxlsx::writeData(history_dedup, "manual_dedup", results_dedup$manual_dedup) # Writes the citation list for manual deduplication in third sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE) # Saves the sheet.

  unique_citations <- results_dedup$unique # Extract the unique references

  # The code below does not work for now, but will allow users to use ASySD shiny app to check the manual deduplication list.
  #starts the manual deduplication with ASySD shiny if required
  # if (nrow(results_dedup$manual_dedup)!=0){
  #   manual_review <- ASySD::manual_dedup_shiny(results_dedup$manual_dedup, cols=names(results_dedup$manual_dedup))
  #   unique_citations <- ASySD::dedup_citations_add_manual(results_dedup$unique, additional_pairs = manual_review)
  # } else{
  #   unique_citations <- results_dedup$unique
  # }

  openxlsx::writeData(history_dedup, "unique_citations", unique_citations) # Writes the unique citation list in forth sheet.
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE) # Saves the sheet.
  ASySD::write_citations(unique_citations, type="csv", filename=csv_name) # Creates the CSV file only containing the unique citations.

  # Remove 'issue' column if present
  if ("issue" %in% names(unique_citations)) unique_citations$issue <- NULL

  # Informs the user that the deduplication was done and exported.
  cat("Deduplication script has been executed, concatenated deduplicated references had been exported.\n")

  # Automatically opens the CSV if required by the user.
  if (isTRUE(open_file)){

    if (.Platform$OS.type == "windows") {
      # Windows: opens in default app (e.g. Excel, Calc)
      shell.exec(csv_name)

    } else if (Sys.info()[["sysname"]] == "Darwin") {
      # macOS: opens in default app (e.g. Excel, Calc)
      system2("open", csv_name)

    } else {
      # Linux: opens in default app (e.g. Excel, Calc)
      system2("xdg-open", csv_name)
    }
  }

}
