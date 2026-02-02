#' Automating the retrieval of references based on a saved search string(s).
#'
#' Creates a read-only Rscript and a task to run the code automatically
#' at a specified frequency and time, to retrieve references corresponding to the saved search string(s) on up to three platforms (e.g. [Web of Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
#' [Scopus](https://www.elsevier.com/en-au/products/scopus) and [PubMed](https://pubmed.ncbi.nlm.nih.gov/)).
#'
#' @param task_ID Name of the automated reference retrieval task (e.g. one keyword describing your review).
#' @param when Frequency of the automated reference retrieval task (DAILY, WEEKLY or MONTHLY).
#' @param time Time of the automated reference retrieval task (must be HH:MM 24-hour clock format).
#' @param WOS Runs the search on Web of Science (TRUE or FALSE).
#' @param SCP Runs the search on Scopus (TRUE or FALSE).
#' @param PMD Runs the search on PubMed (TRUE or FALSE).
#' @param directory Choose the directory in which the search string is saved (Project's directory). That is also where the references metadata will be saved.
#' @param dedup Deduplicates the retrieved references (TRUE or FALSE).
#' @param open_file Automatically opens the CSV file after reference retrieval.
#' @param dry_run Simulation run option.
#'
#' @return \code{NULL} (invisibly). Called for its side effects: writes an R script and schedules a task (Windows Task Scheduler or cron).
#'
#' @examples
#' # This is a "dry run" example.
#' # No task will actually be scheduled, it only shows how the function should react.
#' auto_LitFetchR_setup(task_ID = "fish_vibrio",
#'                        when = "WEEKLY",
#'                        time = "14:00",
#'                        WOS = TRUE,
#'                        SCP = TRUE,
#'                        PMD = TRUE,
#'                        directory = tempdir(),
#'                        dedup = FALSE,
#'                        open_file = FALSE,
#'                        dry_run = TRUE
#'                        )
#'
#' @export

auto_LitFetchR_setup <- function(task_ID = "task_ID",
                                 when = "DAILY",
                                 time = "08:00",
                                 WOS = TRUE,
                                 SCP = TRUE,
                                 PMD = TRUE,
                                 directory,
                                 dedup = FALSE,
                                 open_file = FALSE,
                                 dry_run = FALSE
                                 ) {
  if (dry_run) {
    message('Dry run: no task scheduled, the message "Task scheduled!" will appear when the function will run successfully.')
    return(invisible(NULL))
  }

  # CREATE AUTOMATION CODE
  ########################

  if (missing(directory) || is.null(directory) || !nzchar(directory)) {
    stop("`directory` must be provided (path to your project folder).")
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory)

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
    script_path <- file.path(directory, "auto_LitFetchR_code_READ_ONLY.R")
    #long paths can be problematic later when using `taskscheduler()`
  } else {
    script_path <- file.path(directory, "auto_LitFetchR_code_READ_ONLY.R")
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
  check <- file.create(script_path)
  if (!check) stop("Could not create script file: ", script_path)
  script_path_scheduler <- if (.Platform$OS.type == "windows") get_short_path(script_path) else script_path

  # Vector of code lines to be added in the Rscript
  lines <- character() #create the character vector
  # Build the `manual_fetch()` call based on selected databases
  arg_strings <- c(
    sprintf("%s = %s", names(selected), ifelse(selected, "TRUE", "FALSE")), #Creates a string vector showing with argument from `auto_LitFetchR_setup` parameters
    sprintf("directory = %s", shQuote(directory))
  )

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

  if (.Platform$OS.type == "windows") {
    # Windows: use taskscheduleR
    # create the scheduled task
    taskscheduleR::taskscheduler_create(
      taskname  = task_ID,
      rscript   = script_path_scheduler,
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
  invisible(NULL)
}
