#' Creates an excel file to store the references identification
#'  retrieved at each search.
#'
#' @param directory Choose the directory in which
#'  the references identification history will be saved.
#'
#' @return A list with element:
#' \describe{
#'   \item{history_id}{A Workbook object (from \pkg{openxlsx}).}
#' }
#'
#' @keywords internal

# create history if does not exist
create_id_history <- function(directory) {

  if (missing(directory) || is.null(directory) || !nzchar(directory)) {
    stop("`directory` must be provided (path to your project folder).")
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory)

  history_id_path <- file.path(directory, "history_id.xlsx")

  if (!file.exists(history_id_path)) {
    history_id <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(history_id, "updated_id_list")
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = "id",
                        startCol = 1,
                        startRow = 1)
    openxlsx::addWorksheet(history_id, "id_log")
    openxlsx::writeData(history_id, "id_log",
                        data.frame(id = character(), platform = character(),
                                   timestamp = character(),
                                   stringsAsFactors = FALSE))
    openxlsx::saveWorkbook(history_id, history_id_path, overwrite = TRUE)
  } else {
    message("File already exists")
    history_id <- openxlsx::loadWorkbook(history_id_path)
    # Add id_log sheet if opening a file created before this version.
    if (!"id_log" %in% names(history_id)) {
      openxlsx::addWorksheet(history_id, "id_log")
      openxlsx::writeData(history_id, "id_log",
                          data.frame(id = character(), platform = character(),
                                     timestamp = character(),
                                     stringsAsFactors = FALSE))
    }
  }

  # Returns the R workbook object named history_id
  list(
    history_id = history_id
  )

}
