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

  if (!file.exists(history_search_path)) {
    history_search <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(history_search, "search_history")
    openxlsx::writeData(history_search, "search_history",
                        data.frame(Search_Term = character(), Results_WOS = numeric(),
                                   Results_SCP = numeric(), Results_PMD = numeric(),
                                   timestamp = character(), stringsAsFactors = FALSE))
    openxlsx::saveWorkbook(history_search,
                           file = history_search_path,
                           overwrite = TRUE)
  } else {
    message("File already exists")
    history_search <- openxlsx::loadWorkbook(history_search_path)
    # Add search_history sheet if opening a file created before this version.
    if (!"search_history" %in% names(history_search)) {
      openxlsx::addWorksheet(history_search, "search_history")
      openxlsx::writeData(history_search, "search_history",
                          data.frame(Search_Term = character(), Results_WOS = numeric(),
                                     Results_SCP = numeric(), Results_PMD = numeric(),
                                     timestamp = character(), stringsAsFactors = FALSE))
    }
  }
  message("History had been created.")

  list(
    history_search = history_search,
    sheet_name = "search_history"
  )

}
