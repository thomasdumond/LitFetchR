#' manual literature fetch
#' @param WOS choose to search on Web of Science
#' @param SCP choose to search on Scopus
#' @param PMD choose to search on PubMed
#' @return create a CSV file with the literature metadata
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

