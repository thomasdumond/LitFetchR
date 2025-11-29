#' creates an excel file to store the references' identification retrieved at each search
#' @return create an excel file

# create history if does not exist
create_id_history <- function(){

  if (!file.exists("history_id.xlsx")){
    history_id <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(history_id, "updated_id_list")
    openxlsx::writeData(history_id,
                        sheet = "updated_id_list",
                        x = "id",
                        startCol = 1,
                        startRow = 1)
    openxlsx::saveWorkbook(history_id, "history_id.xlsx")
  } else{
    message("File already exists")
    history_id <- openxlsx::loadWorkbook("history_id.xlsx")
  }

  # return history_search and sheet_name in a list
  return(list(
    history_id = history_id
  ))

}
