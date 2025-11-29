#' creates a read-only code file and a task to run the code automatically.
#' @param reviewID name of the review
#' @param when frequency of the fetching, i.e. DAILY, WEEKLY, MONTHLY
#' @param time time of the fetching, i.e. HH:MM 24-hour clock format
#' @param WOS choose to search on Web of Science
#' @param SCP choose to search on Scopus
#' @param PMD choose to search on PubMed
#' @return create a Rcode file and a task in Task Scheduler (Windows), or in Cron (Mac/Linux)
#' @export


auto_LitFetchR_setup <- function(reviewID = "reviewID", when = "DAILY", time = "08:00", WOS = TRUE, SCP = TRUE, PMD = TRUE) {

  ##############
  # CREATE AUTOMATION CODE

  # Build list of selected databases
  selected <- c(WOS = WOS, SCP = SCP, PMD = PMD)

  if (!any(selected)) {
    stop("At least one database must be set to TRUE (WOS, SCP, PMD).")
  }

  # Path to the new R script
  if (.Platform$OS.type == "windows") {
    # Windows: use attrib
    script_path <- file.path(getwd(), "auto_LitFetchR_code(READ_ONLY).R")
    script_path <- get_short_path(script_path)
  } else {
    # macOS/Linux: use chmod
    script_path <- file.path(getwd(), "auto_LitFetchR_code(READ_ONLY).R")
  }

  if (file.exists(script_path)) {
    if (.Platform$OS.type == "windows") {
      system(paste("attrib -R", shQuote(script_path)))
    }
    unlink(script_path)
  }
  file.create(script_path)

  # Collect code lines
  lines <- character()
  lines <- c(lines, paste0('setwd(', '"', getwd(), '"', ')'))
  # lines <- c(lines, 'devtools::load_all("...")')
  lines <- c(
    lines,
    paste0(
      'search_list_path <- ',
      shQuote(file.path(getwd(), "search_list.txt"))
    )
  )

  # Build the manual_fetch() call based on selected databases
  # This will create something like:
  # manual_fetch(WOS = TRUE, SCP = FALSE, PMD = TRUE)
  arg_strings <- sprintf(
    "%s = %s",
    names(selected),
    ifelse(selected, "TRUE", "FALSE")
  )

  lines <- c(lines, sprintf("LitFetchR::manual_fetch(%s)", paste(arg_strings, collapse = ", ")))

  # Write to file
  writeLines(lines, script_path)

  # Make the automation code READ ONLY
  if (.Platform$OS.type == "windows") {
    # Windows: use attrib
    system(paste("attrib +R", shQuote(script_path)))
  } else {
    # macOS/Linux: use chmod
    system(paste("chmod 444", shQuote(script_path)))
  }


  #SCHEDULE TASK
  ##############

  taskdirectory <- getwd()
  if (.Platform$OS.type == "windows") {
    # Windows: use taskscheduleR
    # create the scheduled task
    taskscheduleR::taskscheduler_create(
      taskname  = reviewID,
      rscript   = script_path,
      schedule  = when,
      starttime = time,
    )
  } else {
    # macOS/Linux: use cronR if available
    if (requireNamespace("cronR", quietly = TRUE)) {
      cron_cmd <- cronR::cron_rscript(script_path)
      cronR::cron_add(
        command   = cron_cmd,
        frequency = when,
        at        = time,
        id        = reviewID
      )
    } else {
      stop("The 'cronR' package is required for scheduling on this OS, but is not installed.")
    }
  }
  message("Task scheduled!")
}
