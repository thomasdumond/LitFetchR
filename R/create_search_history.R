#' Creates an excel file to store the history of searches
#' made using `create_save_search()`
#'
#' @return "history_search.xlsx" and or a new sheet for the new search(es)
#' @keywords internal

create_search_history <- function(){

  # Create history if does not exist
  if (!file.exists("history_search.xlsx")){
    history_search <- openxlsx::createWorkbook()
    # Add a sheet with a unique name
    sheet_name <- build_sheet_name()
    openxlsx::addWorksheet(history_search, sheet_name)
    openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE)
  } else{
    message("File already exists")
    history_search <- openxlsx::loadWorkbook("history_search.xlsx")
    # Add a sheet with a unique name
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("search", date_suffix)
    openxlsx::addWorksheet(history_search, sheet_name)
    openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE)
  }
  message("History had been created.")

  # return history_search R object workbook and sheet_name in a list
  return(list(
    history_search = history_search,
    sheet_name = sheet_name
  ))

}
