#' Creates an excel file to store the history of searches
#' made using `create_save_search()`.
#'
#' @return An history_search R object workbook and sheet_name in a list.
#' @keywords internal

create_search_history <- function(){

  # Create history if does not exist
  if (!file.exists("history_search.xlsx")){
    history_search <- openxlsx::createWorkbook() # creates the excel workbook in R
    sheet_name <- build_sheet_name() # creates a unique name for the new sheet
    openxlsx::addWorksheet(history_search, sheet_name) # Adds a sheet with a unique name
    openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE) # saves the R workbook to an excel file
  } else{
    message("File already exists")
    history_search <- openxlsx::loadWorkbook("history_search.xlsx") # loads the excel workbook in R
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("search", date_suffix) # creates a unique name for the new sheet
    openxlsx::addWorksheet(history_search, sheet_name) # Adds a sheet with a unique name
    openxlsx::saveWorkbook(history_search, "history_search.xlsx", overwrite = TRUE) # saves the R workbook to an excel file
  }
  message("History had been created.")

  # return history_search R object workbook and sheet_name in a list
  return(list(
    history_search = history_search,
    sheet_name = sheet_name
  ))

}
