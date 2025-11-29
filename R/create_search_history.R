#' creates an excel file to store the search history
#' @return create an excel file

# create history if does not exist
create_search_history <- function(){
  if (!file.exists("history_search.xlsx")){
    history_search <- openxlsx::createWorkbook()
    # Add a sheet with date included in name
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("search", date_suffix)
    openxlsx::addWorksheet(history_search, sheet_name)
    openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE)
  } else{
    message("File already exists")
    history_search <- openxlsx::loadWorkbook("history_search.xlsx")
    # Add a sheet with date included in name
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("search", date_suffix)
    openxlsx::addWorksheet(history_search, sheet_name)
    openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE)
  }
  message("History had been created.")

  # return history_search and sheet_name in a list
  return(list(
    history_search = history_search,
    sheet_name = sheet_name
  ))

}
