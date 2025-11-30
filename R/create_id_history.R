#' Creates an excel file to store the references identification retrieved at each search
#'
#' @return history_id.xlsx

# create history if does not exist
create_id_history <- function(){

  if (!file.exists("history_id.xlsx")){
    history_id <- openxlsx::createWorkbook() #create the excel workbook in R
    openxlsx::addWorksheet(history_id, "updated_id_list") #add a sheet
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = "id",
                        startCol = 1,
                        startRow = 1) #column name is "id"
    openxlsx::saveWorkbook(history_id, "history_id.xlsx") #save the R workbook to an excel file
  } else{
    # If the ID history already exist, just load it to an R workbook object
    message("File already exists")
    history_id <- openxlsx::loadWorkbook("history_id.xlsx")
  }

  # Returns the R workbook object named history_id
  return(list(
    history_id = history_id
  ))

}
