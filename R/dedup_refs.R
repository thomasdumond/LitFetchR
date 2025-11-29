#' deduplicate the references from three dataframes
#' @param df1 dataframe 1
#' @param df2 dataframe 2
#' @param df3 dataframe 3
#' @return A CSV file containing all the new references deduplicated
#' @export

dedup_refs <- function(df1, df2, df3){

  date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  csv_name <- paste0("citationsCSV_", date_suffix,".csv")
  hd <- create_dedup_history()
  history_dedup <- hd$history_dedup
  hist_dedup_name <- hd$hist_dedup_name
  citations <- rbind(df1, df2, df3)
  openxlsx::writeData(history_dedup, "citations", citations)
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE)

  results_dedup <- ASySD::dedup_citations(citations,
                                          manual_dedup = TRUE,
                                          merge_citations = TRUE,
                                          user_input = "1")

  openxlsx::writeData(history_dedup, "auto_dedup_unique", results_dedup$unique)
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE)
  openxlsx::writeData(history_dedup, "manual_dedup", results_dedup$manual_dedup)
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE)

  unique_citations <- results_dedup$unique

  #starts the manual deduplication with ASySD shiny if required
  # if (nrow(results_dedup$manual_dedup)!=0){
  #   manual_review <- ASySD::manual_dedup_shiny(results_dedup$manual_dedup, cols=names(results_dedup$manual_dedup))
  #   unique_citations <- ASySD::dedup_citations_add_manual(results_dedup$unique, additional_pairs = manual_review)
  # } else{
  #   unique_citations <- results_dedup$unique
  # }

  openxlsx::writeData(history_dedup, "unique_citations", unique_citations)
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name, overwrite = TRUE)
  ASySD::write_citations(unique_citations, type="csv", filename=csv_name)

  # Remove 'issue' column if present
  if ("issue" %in% names(unique_citations)) unique_citations$issue <- NULL

  cat("Deduplication script has been executed, concatenated deduplicated references had been exported.\n")

  if (.Platform$OS.type == "windows") {
    # Windows: opens in default app
    shell.exec(csv_name)

  } else if (Sys.info()[["sysname"]] == "Darwin") {
    # macOS: opens in default app (e.g., Excel)
    system2("open", csv_name)

  } else {
    # Linux: opens in default app
    system2("xdg-open", csv_name)
  }

}
