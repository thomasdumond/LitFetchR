#' Creates an excel file to store the deduplication history
#'
#' @return history_dedup.xlsx

create_dedup_history <- function(){

  #setup the R object representing the excel file
  date_suffix <- format(Sys.time(), "%Y-%m-%d-%H%M%S") #setup a unique name
  history_dedup <- openxlsx::createWorkbook() #create the excel workbook in R
  openxlsx::addWorksheet(history_dedup, "citations") #add a sheet
  openxlsx::addWorksheet(history_dedup, "auto_dedup_unique") #add a sheet
  openxlsx::addWorksheet(history_dedup, "manual_dedup") #add a sheet
  openxlsx::addWorksheet(history_dedup, "unique_citations") #add a sheet

  #create the history deduplication excel file
  hist_dedup_name <- paste0("history_dedup_", date_suffix,".xlsx") #full name of the excel file
  openxlsx::saveWorkbook(history_dedup, hist_dedup_name) #save the R workbook to an excel file

  # returns the R workbook object and the name of the excel file in a list
  return(list(history_dedup = history_dedup,
              hist_dedup_name = hist_dedup_name
              )
         )

}
