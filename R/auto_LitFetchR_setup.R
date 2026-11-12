#' Automating the retrieval of references based on a saved search string(s).
#'
#' Creates a read-only Rscript and a task to run the code automatically
#' at a specified frequency and time, to retrieve references corresponding to
#' the saved search string(s) on up to three platforms
#' (e.g. [Web of Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
#' [Scopus](https://www.elsevier.com/en-au/products/scopus)
#' and [PubMed](https://pubmed.ncbi.nlm.nih.gov/)).
#'
#' The function reports "Task scheduled!" only after checking that the
#' scheduler accepted the task and that the task exists. On Windows the
#' registered task is read back from Task Scheduler and compared with the
#' requested schedule. A short summary (recurrence, next run time and status)
#' is then printed. If the scheduler returns an error, the function stops and
#' shows the scheduler's own message. If the task cannot be confirmed, a
#' warning is given instead of the success message. Check the summary: it is
#' the only evidence that automatic retrieval will run.
#'
#' The scheduled task runs only while the computer is on and you are logged in.
#' Each run is recorded in `run_log.csv` (see [manual_fetch()]) with the name
#' of the task in the column `run_mode`. When a run fails, or finishes with
#' problems on one platform, it ends with a non-zero exit status, which the
#' Windows Task Scheduler shows as the Last Run Result of the task.
#'
#' @param task_id Name of the automated reference retrieval task
#'  (e.g. one keyword describing your review). Spaces are replaced by "-".
#'  A task with the same name must not exist already: use
#'  [remove_scheduled_task()] first to replace it.
#' @param when Frequency of the automated reference retrieval task
#'  (DAILY, WEEKLY or MONTHLY).
#' @param time Time of the automated reference retrieval task
#'  (must be HH:MM 24-hour clock format).
#' @param day Day on which a WEEKLY or MONTHLY task runs. For WEEKLY, one of
#'  MON, TUE, WED, THU, FRI, SAT or SUN. For MONTHLY, a day of the month
#'  between 1 and 28. If \code{NULL} (default), the weekday (WEEKLY) or day of
#'  the month (MONTHLY) of today's date is used. Ignored for DAILY.
#' @param wos Runs the search on Web of Science (TRUE or FALSE).
#' @param scp Runs the search on Scopus (TRUE or FALSE).
#' @param pmd Runs the search on PubMed (TRUE or FALSE).
#' @param directory Choose the directory in which the search string is saved
#'  (Project's directory). That is also where the references metadata
#'  will be saved.
#' @param dedup Deduplicates the retrieved references (TRUE or FALSE).
#' @param open_file Automatically opens the output file after reference retrieval.
#' @param dry_run Simulation run option.
#' @param output_format Format of the file of references written at each run:
#'  `"csv"` (default) or `"ris"` (see [manual_fetch()]). The choice is fixed in
#'  the script of the task. To change it, remove the task with
#'  [remove_scheduled_task()] and create it again.
#'
#' @return \code{NULL} (invisibly). Called for its side effects:
#'  writes an R script and schedules a task (Windows Task Scheduler or cron)
#'  to run the script automatically. The function stops with the scheduler's
#'  error message if the task cannot be created, and gives a warning if the
#'  new task cannot be confirmed.
#'
#' @examples
#' # This is a "dry run" example.
#' # No task will actually be scheduled,
#' # it only shows how the function should react.
#' auto_LitFetchR_setup(task_id = "fish_vibrio",
#'                        when = "WEEKLY",
#'                        time = "14:00",
#'                        day = "MON",
#'                        wos = TRUE,
#'                        scp = TRUE,
#'                        pmd = TRUE,
#'                        directory,
#'                        dedup = FALSE,
#'                        open_file = FALSE,
#'                        dry_run = TRUE
#'                        )
#'
#' @export

auto_LitFetchR_setup <- function(task_id = "task_id",
                                 when = "DAILY",
                                 time = "08:00",
                                 wos = FALSE,
                                 scp = FALSE,
                                 pmd = FALSE,
                                 directory,
                                 dedup = FALSE,
                                 open_file = FALSE,
                                 dry_run = FALSE,
                                 day = NULL,
                                 output_format = c("csv", "ris")
                                 ) {
  output_format <- check_output_format(output_format)
  # Checks the schedule first, before anything is created
  sched <- check_schedule_args(when, time, day)
  when <- sched$when
  time <- sched$time
  day <- sched$day

  if (dry_run) {
    message('Dry run: no task scheduled,
            the message "Task scheduled!" will appear when the function will
            run successfully.')
    message("The task would run ", describe_schedule(when, day, time), ".")
    return(invisible(NULL))
  }

  # CREATE AUTOMATION CODE
  ########################

  if (missing(directory)) {
    stop("`directory` must be provided (path to your project folder).",
         call. = FALSE)
  }
  directory <- check_directory(directory)
  task_id <- normalize_task_id(task_id)
  if (sched$day_defaulted) {
    message("`day` not supplied: using ", day, " (from today's date).")
  }

    # Build the list of selected databases
  selected <- c(wos = wos,
                scp = scp,
                pmd = pmd,
                dedup = dedup,
                open_file = open_file)
  # If no database was selected, then the code stops and mentions
  # that at least one database must be selected
  if (!any(c(wos, scp, pmd))) {
    stop("At least one database must be set to TRUE (wos, scp, pmd).")
  }

  # Stops before anything is written if the task cannot be created.
  # A task with the same name is never overwritten silently.
  is_win <- is_windows()
  if (is_win) {
    if (win_task_query(task_id)$exists) {
      stop("A scheduled task named '", task_id, "' already exists. ",
           "Use remove_scheduled_task(\"", task_id, "\") first, ",
           "or choose another `task_id`.")
    }
  } else {
    if (!cron_available()) {
      stop("The 'cronR' package is required for scheduling on this OS,
           but is not installed.")
    }
    if (!is.null(cron_find(task_id))) {
      stop("A cron job with the id '", task_id, "' already exists. ",
           "Use remove_scheduled_task(\"", task_id, "\") first, ",
           "or choose another `task_id`.")
    }
  }

  # Creates the path to the read-only R script containing the code
  # that will be run automatically. Different approach windows vs mac
  if (is_windows()) {
    script_path <- file.path(directory, "auto_LitFetchR_code_READ_ONLY.R")
    #long paths can be problematic later when using `taskscheduler()`
  } else {
    script_path <- file.path(directory, "auto_LitFetchR_code_READ_ONLY.R")
  }
  # If the task needs to be modified or add a new task, then the read-only
  # file must be readable again, deleted and recreated. Windows only.
  if (file.exists(script_path)) {
    if (is_windows()) {
      system(paste("attrib -R", shQuote(script_path)))
    }
    unlink(script_path)
  }

  # Creates the Rscript from the path
  check <- file.create(script_path)
  if (!check) stop("Could not create script file: ", script_path)
  script_path_scheduler <- if (is_windows()) {
    get_short_path(script_path)
  } else {
    script_path
  }
  # The Windows Task Scheduler stores at most 261 characters for the command,
  # which contains the path of the script twice (script and log).
  if (is_win) {
    cmd_length <- nchar(win_task_command(script_path_scheduler))
    if (cmd_length > 261) {
      unlink(script_path)
      stop("The path of the project folder is too long for the Windows Task ",
           "Scheduler (its command is ", cmd_length, " characters long, the ",
           "limit is 261). Use a project folder with a shorter path, for ",
           "example C:/reviews/my_review. No task was scheduled.",
           call. = FALSE)
    }
  }

  # Vector of code lines to be added in the Rscript
  lines <- character() #create the character vector
  # Build the `manual_fetch()` call based on selected databases
  arg_strings <- c(
    sprintf("%s = %s",
            names(selected),
            ifelse(selected, "TRUE", "FALSE")
            ), #Creates a vector with argument from `auto_LitFetchR_setup`
    # Backslashes of Windows paths must be escaped to give valid R code.
    sprintf("output_format = %s", encodeString(output_format, quote = "\"")),
    sprintf("directory = %s", encodeString(directory, quote = "\""))
  )
  #Creates a string vector with the `manual_fetch`
  #function containing the same arguments as `auto_LitFetchR_setup`
  fetch_call <- sprintf("LitFetchR::manual_fetch(%s)",
                        paste(arg_strings, collapse = ", "))

  # The script gives the name of the task to the run log, and ends with a
  # non-zero exit status when the run failed or finished with problems, so
  # that the Task Scheduler shows it (Last Run Result).
  lines <- c(
    lines,
    "# Script written by LitFetchR::auto_LitFetchR_setup(), do not edit.",
    sprintf("Sys.setenv(LITFETCHR_TASK_ID = %s)",
            encodeString(task_id, quote = "\"")),
    "run_ok <- TRUE",
    "withCallingHandlers(",
    "  tryCatch(",
    paste0("    ", fetch_call, ","),
    "    error = function(e) {",
    "      message(\"LitFetchR run failed: \", conditionMessage(e))",
    "      run_ok <<- FALSE",
    "    }",
    "  ),",
    "  litfetchr_run_problem = function(w) run_ok <<- FALSE",
    ")",
    "if (!run_ok) quit(save = \"no\", status = 1)"
  )

  # Create the R script
  writeLines(lines, script_path)

  # The script must be valid R code, otherwise the scheduled task can never run.
  script_check <- tryCatch({
    parse(script_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(script_check)) {
    unlink(script_path)
    stop("The generated script is not valid R code and no task was created: ",
         script_check, call. = FALSE)
  }

  # Make the new R script READ ONLY.
  if (is_windows()) {
    system(paste("attrib +R", shQuote(script_path)))
  } else {
    system(paste("chmod 444", shQuote(script_path)))
  }


  #SCHEDULE TASK
  ##############

  # Success is reported only after the scheduler's answer is checked and
  # the task is read back.
  if (is_win) {
    # Windows: Task Scheduler through `schtasks`
    created <- win_task_create(task_id, script_path_scheduler, when, time, day)
    if (created$status != 0L) {
      stop("Windows Task Scheduler could not create the task '", task_id,
           "'. No task was scheduled.\n", format_sched_failure(created),
           call. = FALSE)
    }

    found <- win_task_query(task_id)
    if (!found$exists) {
      warning("Task Scheduler reported success but the task '", task_id,
              "' could not be found afterwards. The task is NOT confirmed, ",
              "so automatic retrieval may not run. Check Task Scheduler.",
              call. = FALSE)
      return(invisible(NULL))
    }

    info <- found$info
    diffs <- compare_win_task(info, when, day, time)
    next_run <- if (is.null(info$next_run) || is.na(info$next_run) ||
                    !nzchar(info$next_run)) "not available" else info$next_run
    summary_lines <- c(
      sprintf("  Task name:  %s", task_id),
      sprintf("  Recurrence: %s", describe_schedule(when, day, time)),
      if (!is.null(info$start_date) && !is.na(info$start_date))
        sprintf("  Start date: %s", info$start_date),
      sprintf("  Next run:   %s", next_run),
      sprintf("  Status:     %s", if (is.na(info$status)) "unknown" else info$status)
    )
    if (length(diffs) || next_run %in% c("not available", "N/A")) {
      warning("The task '", task_id, "' exists but is NOT confirmed as ",
              "requested: ",
              paste(c(diffs, if (next_run %in% c("not available", "N/A"))
                "there is no next run time"), collapse = ", "),
              ". Check Task Scheduler.\n",
              paste(summary_lines, collapse = "\n"), call. = FALSE)
      return(invisible(NULL))
    }
    message("Task scheduled!\n", paste(summary_lines, collapse = "\n"))
  } else {
    # macOS/Linux: cronR
    err <- cron_create(task_id, script_path, when, time, day)
    if (!is.null(err)) {
      stop("cronR could not create the job '", task_id,
           "'. No task was scheduled.\n", err, call. = FALSE)
    }
    entry <- cron_find(task_id)
    if (is.null(entry)) {
      warning("cronR reported success but the job '", task_id,
              "' was not found in the crontab afterwards. The task is NOT ",
              "confirmed, so automatic retrieval may not run. ",
              "Check with cronR::cron_ls().", call. = FALSE)
      return(invisible(NULL))
    }
    message("Task scheduled!\n",
            sprintf("  Task name:  %s\n", task_id),
            sprintf("  Recurrence: %s\n", describe_schedule(when, day, time)),
            if (!is.na(entry$schedule_line))
              sprintf("  Cron entry: %s", entry$schedule_line))
  }
  invisible(NULL)
}
