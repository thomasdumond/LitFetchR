#' creates an excel file to store the deduplication history
#' @return create an excel file

create_dedup_history <- function(){
  date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  history_dedup <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(history_dedup, "citations")
  openxlsx::addWorksheet(history_dedup, "auto_dedup_unique")
  openxlsx::addWorksheet(history_dedup, "manual_dedup")
  openxlsx::addWorksheet(history_dedup, "unique_citations")
  hist_dedup_name <- paste0("history_dedup_", date_suffix,".xlsx")
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name)

  # return history_search and sheet_name in a list
  return(list(
    history_dedup = history_dedup,
    hist_dedup_name = hist_dedup_name
    )
  )

}
