#' Automating the retrieval of references based on a saved search string
#'
#' `auto_LitFetchR_setup` creates a read-only Rscript and a task to run the code automatically
#' at a specified frequency and time, on up to three platforms (e.g. [Web of Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
#' [Scopus](https://www.elsevier.com/en-au/products/scopus) and [PubMed](https://pubmed.ncbi.nlm.nih.gov/))
#'
#' @param task_ID Name of the automated task (e.g. one keyword describing your review)
#' @param when Frequency of the fetching, i.e. DAILY, WEEKLY, MONTHLY
#' @param time Time of the fetching, i.e. HH:MM 24-hour clock format
#' @param WOS Choose to search on Web of Science (TRUE or FALSE)
#' @param SCP Choose to search on Scopus (TRUE or FALSE)
#' @param PMD Choose to search on PubMed (TRUE or FALSE)
#' @param dedup Choose to deduplicate or not the references (TRUE or FALSE)
#' @param open_file choose to automatically open the CSV file after reference retrieval
#'
#' @returns Create a Rscript file (READ ONLY) and a task in Task Scheduler (Windows), or in Cron (Mac/Linux)
#'
#' @examples
#' \dontrun{
#'
#' #Example of what you should see:
#' auto_LitFetchR_setup(task_ID = "fish_vibrio",
#'                        when = "WEEKLY",
#'                        time = "14:00",
#'                        WOS = TRUE,
#'                        SCP = TRUE,
#'                        PMD = TRUE)
#' Task scheduled!
#' }
#'
#' @export

auto_LitFetchR_setup <- function(task_ID = "task_ID", when = "DAILY", time = "08:00", WOS = TRUE, SCP = TRUE, PMD = TRUE, dedup = FALSE, open_file = FALSE) {

  # CREATE AUTOMATION CODE
  ########################

  # Build the list of selected databases
  selected <- c(WOS = WOS, SCP = SCP, PMD = PMD, dedup = dedup, open_file = open_file)
  # If no database was selected, then the code stops and mentions
  # that at least one database must be selected
  if (!any(c(WOS, SCP, PMD))) {
    stop("At least one database must be set to TRUE (WOS, SCP, PMD).")
  }

  # Creates the path to the read-only R script containing the code
  # that will be run automatically. Different approach windows vs mac
  if (.Platform$OS.type == "windows") {
    script_path <- file.path(getwd(), "auto_LitFetchR_code(READ_ONLY).R")
    #long paths can be problematic later when using `taskscheduler()`
    script_path <- get_short_path(script_path)
  } else {
    script_path <- file.path(getwd(), "auto_LitFetchR_code(READ_ONLY).R")
  }
  # If the task needs to be modified or add a new task, then the read-only
  # file must be readable again, deleted and recreated. Windows only.
  if (file.exists(script_path)) {
    if (.Platform$OS.type == "windows") {
      system(paste("attrib -R", shQuote(script_path)))
    }
    unlink(script_path)
  }

  # Creates the Rscript from the path
  file.create(script_path)

  # Vector of code lines to be added in the Rscript
  lines <- character() #create the character vector
  lines <- c(lines, paste0('setwd(', '"', getwd(), '"', ')')) #make sure to use the right directory
  lines <- c(lines,paste0('search_list_path <- ',
                          shQuote(file.path(getwd(),"search_list.txt"))
                          )
             ) #variable path to the search string(s)

  # Build the `manual_fetch()` call based on selected databases
  arg_strings <- sprintf("%s = %s",
                         names(selected),
                         ifelse(selected, "TRUE", "FALSE")
                         ) #Creates a string vector showing with argument from `auto_LitFetchR_setup` parameters

  lines <- c(lines, sprintf("LitFetchR::manual_fetch(%s)",
                            paste(arg_strings, collapse = ", ")
                            )
             ) #Creates a string vector with the `manual_fetch` function containing the same arguments as `auto_LitFetchR_setup`

  # Create the R script
  writeLines(lines, script_path)

  # Make the new R script READ ONLY.
  if (.Platform$OS.type == "windows") {
    system(paste("attrib +R", shQuote(script_path)))
  } else {
    system(paste("chmod 444", shQuote(script_path)))
  }


  #SCHEDULE TASK
  ##############

  taskdirectory <- getwd() #make sure to use the right directory
  if (.Platform$OS.type == "windows") {
    # Windows: use taskscheduleR
    # create the scheduled task
    taskscheduleR::taskscheduler_create(
      taskname  = task_ID,
      rscript   = script_path,
      schedule  = when,
      starttime = time
    )
  } else {
    # macOS/Linux: use cronR if available
    if (requireNamespace("cronR", quietly = TRUE)) {
      cron_cmd <- cronR::cron_rscript(script_path)
      cronR::cron_add(
        command   = cron_cmd,
        frequency = when,
        at        = time,
        id        = task_ID
      )
    } else {
      stop("The 'cronR' package is required for scheduling on this OS, but is not installed.")
    }
  }
  message("Task scheduled!")
}
