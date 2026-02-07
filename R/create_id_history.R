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
    history_id <- openxlsx::createWorkbook() #create the excel workbook in R
    openxlsx::addWorksheet(history_id, "updated_id_list") #add a sheet
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = "id",
                        startCol = 1,
                        startRow = 1) #column name is "id"
    #save the R workbook to an excel file
    openxlsx::saveWorkbook(history_id, history_id_path, overwrite = TRUE)
  } else {
    # If the ID history already exist, just load it to an R workbook object
    message("File already exists")
    history_id <- openxlsx::loadWorkbook(history_id_path)
  }

  # Returns the R workbook object named history_id
  list(
    history_id = history_id
  )

}
