#' Creates an excel file to store the deduplication history.
#'
#' @param directory Choose the directory in which
#'  the references deduplication history will be saved.
#'
#' @return A list with elements:
#' \describe{
#'   \item{history_dedup}{A Workbook object (from \pkg{openxlsx}).}
#'   \item{hist_dedup_path}{Character. Path to the created .xlsx file.}
#' }
#'
#' @keywords internal

create_dedup_history <- function(directory) {

  directory <- normalizePath(directory, mustWork = FALSE)

  #setup the R object representing the excel file
  date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S") #setup a unique name
  history_dedup <- openxlsx::createWorkbook() #create the excel workbook in R
  openxlsx::addWorksheet(history_dedup, "citations") #add a sheet
  openxlsx::addWorksheet(history_dedup, "auto_dedup_unique") #add a sheet
  openxlsx::addWorksheet(history_dedup, "manual_dedup") #add a sheet
  openxlsx::addWorksheet(history_dedup, "unique_citations") #add a sheet

  #create the history deduplication excel file
  #full name of the excel file
  hist_dedup_name <- paste0("history_dedup_", date_suffix, ".xlsx")
  hist_dedup_path <- file.path(directory, hist_dedup_name)
  openxlsx::saveWorkbook(wb = history_dedup,
                         file = hist_dedup_path,
                         overwrite = TRUE) #save the R workbook to an excel file

  # returns the R workbook object and the name of the excel file in a list
  list(history_dedup = history_dedup,
              hist_dedup_path = hist_dedup_path
              )
}
