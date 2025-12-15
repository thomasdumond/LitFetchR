#' manual literature fetch
#' @param WOS choose to search on Web of Science (TRUE or FALSE)
#' @param SCP choose to search on Scopus (TRUE or FALSE)
#' @param PMD choose to search on PubMed (TRUE or FALSE)
#'
#' @return create a CSV file with the literature metadata, a history file of the references retreived and a history file of the deduplication
#'
#' @examples
#' \dontrun{
#'
#' #Example of what you should see:
#' > manual_fetch(WOS = TRUE, SCP = TRUE, PMD = TRUE)
#' [1] 126
#' Finished batch number 1
#' Finished batch number 2
#' [1] "10.1016/j.aaf.2023.11.002 1 / 126"
#' [1] "10.3390/fishes10090439 2 / 126"
#' [FOR THE PURPOSE OF THIS EXAMPLE WE ARE NOT SHOWING EACH LINE OF THE CONSOLE]
#' [1] "NA 125 / 126"
#' [1] "NA 126 / 126"
#' [1] 22
#' Finished batch number 1
#' File already exists
#' [1] "10.1007/s12602-023-10207-x 1 / 22"
#' [1] "10.1016/j.fsi.2025.110189 2 / 22"
#' [FOR THE PURPOSE OF THIS EXAMPLE WE ARE NOT SHOWING EACH LINE OF THE CONSOLE]
#' [1] "10.1111/j.1472-765X.2010.02894.x 21 / 22"
#' [1] "NA 22 / 22"
#' [1] 106
#' Finished batch 1 for fish AND "vibrio harveyi" AND diagnostic
#' File already exists
#' [1] "10.1016/j.fsi.2025.110503 1 / 106"
#' [1] "10.1016/j.fsi.2025.110501 2 / 106"
#' [FOR THE PURPOSE OF THIS EXAMPLE WE ARE NOT SHOWING EACH LINE OF THE CONSOLE][1] "NA 105 / 106"
#' [1] "NA 106 / 106"
#' Warning: The following columns are missing: pages, number, record_id, isbn
#' formatting data...
#' identifying potential duplicates...
#' identified duplicates!
#' flagging potential pairs for manual dedup...
#' Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
#' 254 citations loaded...
#' 14 duplicate citations removed...
#' 240 unique citations remaining!
#' Deduplication script has been executed, concatenated deduplicated references had been exported.
#' Warning message:
#' In add_missing_cols(raw_citations) :
#'   Search contains missing values for the record_id column.
#'   A record_id will be created using row numbers
#' }
#'
#' @export

manual_fetch <- function(WOS = TRUE, SCP = TRUE, PMD = TRUE){

  wd <- getwd()
  setwd(wd)

  if(file.exists("search_list.txt")){
    search_list_path <- paste0(wd,"/search_list.txt")
  } else{
    stop("The search list does not exist. Please create one using the function 'search_sensitivity'")
  }

  # Build list of selected databases
  selected <- c(WOS = WOS, SCP = SCP, PMD = PMD)
  selected <- names(selected)[selected]  # keep only TRUE ones

  if (length(selected) == 0) {
    stop("At least one database must be set to TRUE (WOS, SCP, PMD).")
  }

  if ("WOS" %in% selected) {
    df1 <- extract_wos_list(search_list_path)
  }
  if ("SCP" %in% selected) {
    df2 <- extract_scp_list(search_list_path)
  }
  if ("PMD" %in% selected) {
    df3 <- extract_pmd_list(search_list_path)
  }

  dedup_refs(df1, df2, df3)

}

