#' Creates an excel file to store the history of searches
#' made using `create_save_search()`.
#'
#' @param directory Choose the directory in which
#'  the search history will be saved.
#'
#' @return A list with elements:
#' \describe{
#'  \item{history_search}{Workbook object.}
#'  \item{sheet_name}{Character scalar.}
#' }
#'
#' @keywords internal

create_search_history <- function(directory) {

  if (missing(directory) || is.null(directory) || !nzchar(directory)) {
    stop("`directory` must be provided (path to your project folder).")
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory)

  history_search_path <- file.path(directory, "history_search.xlsx")

  # Create history if does not exist
  if (!file.exists(history_search_path)) {
    # creates the excel workbook in R
    history_search <- openxlsx::createWorkbook()
    # creates a unique name for the new sheet
    sheet_name <- build_sheet_name()
    # Adds a sheet with a unique name
    openxlsx::addWorksheet(history_search, sheet_name)
    # saves the R workbook to an excel file
    openxlsx::saveWorkbook(history_search,
                           file = history_search_path,
                           overwrite = TRUE)
  } else {
    message("File already exists")
    # loads the excel workbook in R
    history_search <- openxlsx::loadWorkbook(history_search_path)
    # creates a unique name for the new sheet
    date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    sheet_name <- paste0("search", date_suffix)
    # Adds a sheet with a unique name
    openxlsx::addWorksheet(history_search, sheet_name)
    # saves the R workbook to an excel file
    openxlsx::saveWorkbook(history_search,
                           file = history_search_path,
                           overwrite = TRUE)
  }
  message("History had been created.")

  # return history_search R object workbook and sheet_name in a list
  list(
    history_search = history_search,
    sheet_name = sheet_name
  )

}
